defmodule Tria do

  @moduledoc """
  Optimizing transpiler for Elixir language.
  """

  import Tria.Common, only: [inspect_ast: 2], warn: false
  alias Tria.Translator.Elixir, as: ElixirTranslator

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

  defmacro __using__(opts) do
    quote do: use Tria.Compiler.Callbacks, unquote(opts)
  end

  # Public interface

  @doc """
  This macro applies optimizer to the passed body.
  Use it with caution, because it can fuck up the context
  """
  defmacro tria(opts \\ [], do_body)
  defmacro tria(opts, do: body), do: run(body, opts ++ [translate: true, env: __CALLER__])
  defmacro tria(opts, body), do: run(body, opts ++ [translate: true, env: __CALLER__])

  @doc """
  This same as `tria/1` macro, but as a function.
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
      # |> inspect_ast(label: :after_translation, with_contexts: true)
      |> Tria.Translator.SSA.from_tria()
      |> run_while()
      # |> inspect_ast(label: :result, with_contexts: true)

    if translate do
      ElixirTranslator.from_tria(result)
    else
      result
    end
  end

  defp run_while(ast) do
    case Tria.Pass.Evaluation.run_once(ast, hooks: %{after: &Tria.Pass.Peephole.after_hook/2}) do
      {:ok, ast} ->
        run_while(ast)

      {:error, :nothing_to_evaluate} ->
        ast
    end
  end

end
