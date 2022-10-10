defmodule Tria do

  @moduledoc """
  Optimizing transpiler for Elixir language.
  """

  alias Tria.Translator.Elixir, as: ElixirTranslator
  alias Tria.Compiler.ContextServer
  import ContextServer, only: [fname: 2]
  import Tria.Common

  @typedoc """
  Tria is (mostly) a subset of Elixir language which differs in some ways from Elixir
  Generally, it preserves the structure of Elixir's AST.

  The only incompatible difference here is that Tria supports having integer instead of context
  in the ast for variables. This is very handy for SSA transformations and stuff.
  """
  @type t :: expression
  | variable
  | {t, t}
  | [t]
  | atom
  | number
  | binary

  @type expression :: {expression | atom, meta, [t]}

  @type variable :: {atom, meta, context}

  @type context :: atom | integer

  @type meta :: Keyword.t()

  # Public interface

  @doc """
  This macro applies optimizer to the passed body.
  Use it with caution, because it can fuck up the context
  """
  defmacro tria(do: body), do: run(body, translate: true, env: __CALLER__)
  defmacro tria(body), do: run(body, translate: true, env: __CALLER__)

  @doc """
  This same as `tria/1` macro, but as a function
  """
  @spec run(Macro.t(), Keyword.t()) :: Macro.t()
  def run(quoted, opts \\ [])
  def run(quoted, %Macro.Env{} = env) do
    run(quoted, env: env)
  end
  def run(quoted, opts) do
    translate = Keyword.get(opts, :translate, true)
    quoted =
      if translate do
        env = Keyword.fetch!(opts, :env)
        ElixirTranslator.to_tria!(quoted, env)
      else
        quoted
      end

    result =
      quoted
      |> inspect_ast(label: :after_translation)
      |> Tria.Translator.SSA.from_tria()
      |> run_while()
      |> inspect_ast(label: :result, with_contexts: true)

    if translate do
      ElixirTranslator.from_tria(result)
    else
      result
    end
  end

  @doc """
  `use Tria, context: Context`
  It aggregates all definitions and makes every function call the Context module internally

  How it should work

  1. Lazily start server for the contexts if it is not started yet
  2. Send every def to server
  3. Replace every public def with delegation to context
  4. Send ready signal to the server and wait finish from server

  5. When server receives all finishes (this is a bad algorithm), it start emitting context module
  6. Context module should replace each local call with global call
  """
  defmacro __using__(opts) do
    context = unalias Keyword.get(opts, :context, TriaGlobalContext)
    ContextServer.start(context)

    Module.register_attribute(__CALLER__.module, :defs, accumulate: true)
    quote do
      @on_definition unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
      @context unquote(context)
    end
  end

  # Using callbacks

  def __on_definition__(_env, _kind, :__struct__, _args, _guards, _body), do: :ok
  def __on_definition__(%Macro.Env{module: module} = env, kind, name, args, guards, body) do
    args   = ElixirTranslator.to_tria!(args, %Macro.Env{env | context: :match})
    guards = ElixirTranslator.to_tria!(guards, %Macro.Env{env | context: :guard})
    body   = ElixirTranslator.to_tria!(body, env)
    Module.put_attribute(module, :defs, {{kind, name}, {args, guards, body}})
  end

  defmacro __before_compile__(%Macro.Env{module: module}) do
    IO.inspect module, label: :module
    context = Module.get_attribute(module, :context)

    defs =
      module
      |> Module.get_attribute(:defs, [])
      |> join_clauses()
      |> :lists.reverse()
      |> Enum.map(fn {{kind, name}, [{args, _guards, _body} | _] = clauses} ->
        # IO.inspect name, label: :name
        # IO.inspect args, label: :args
        Module.delete_definition(module, {name, length(args)})

        ContextServer.emit_definition(context, kind, module, name, clauses)
        delegate(context, module, kind, name, args)
      end)
      |> inspect_ast(label: :defs)

    ContextServer.mark_ready(context, module)

    {:__block__, [], defs}
  end

  # Helpers

  defp delegate(_, _, private_kind, _, _) when private_kind in ~w[defp defmacrop]a, do: nil
  defp delegate(context, module, kind, name, args) do
    args = Macro.generate_unique_arguments(length(args), nil)
    fname = fname(module, name)
    quote do
      unquote(kind)(unquote(name)(unquote_splicing args), do: unquote(context).unquote(fname)(unquote_splicing args))
    end
  end

  # FIXME it is not handling lists
  defp join_clauses([{name, {_, _, _} = clause} | tail]) do
    join_clauses [{name, [clause]} | tail]
  end
  defp join_clauses([{name, [{args1, _, _} | _] = clauses}, {name, {args2, _, _} = clause} | tail]) do
    if length(args1) == length(args2) do
      join_clauses [{name, [clause | clauses]} | tail]
    else
      [{name, clauses} | join_clauses [{name, [clause]} | tail]]
    end
  end
  defp join_clauses([{name, clauses} | tail]) do
    [{name, clauses} | join_clauses tail]
  end
  defp join_clauses([]), do: []

  defp run_while(ast) do
    case Tria.Pass.Evaluation.run_once(ast, hooks: %{after: &Tria.Pass.Peephole.after_hook/2}) do
      {:ok, ast} ->
        run_while(ast)

      {:error, :nothing_to_evaluate} ->
        ast
    end
  end

end
