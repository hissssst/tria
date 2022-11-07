defmodule Tria.Compiler.Callbacks do

  @moduledoc """
  Callbacks to be inserted in module when compiling a whole module using Tria
  """

  alias Tria.Translator.Elixir, as: ElixirTranslator
  alias Tria.Compiler.ContextServer
  import Tria.Compiler, only: [fname: 2]
  import Tria.Common

  @typedoc """
  Kind of definition
  """
  @type kind :: :def | :defp | :defmacro | :defmacrop

  @typedoc """
  Name of function
  """
  @type name :: atom()

  @typedoc """
  Universal signature of a function
  """
  @type signature :: {module(), kind(), name(), arity()}

  @typedoc """
  One clause of a function in a `def` style
  """
  @type clause :: {args :: [Tria.t()], guards :: [Tria.t()], body :: Tria.t()}

  @typedoc """
  One definition
  """
  @type definition :: {signature(), clauses :: [clause()]}

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
    opts = Keyword.put(opts, :context, context)
    ContextServer.start(context)

    Module.register_attribute(__CALLER__.module, :defs, accumulate: true)
    quote do
      @on_definition unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
      @tria_opts unquote(opts)
    end
  end

  # Using callbacks

  # __struct__ is ignored because it is a compile-time magic. I am sorry
  def __on_definition__(_env, _kind, :__struct__, _args, _guards, _body), do: :ok
  def __on_definition__(%Macro.Env{module: module} = env, kind, name, args, guards, body) do
    arity = length(args)
    signature = {module, kind, name, arity}
    clauses = [{args, guards, body}]

    definition = {signature, clauses}
    definition = translate_def(definition, env)

    overridable? = Module.overridable?(module, {name, arity})
    Module.put_attribute(module, :defs, {overridable?, definition})

    :ok
  end

  defmacro __before_compile__(%Macro.Env{module: module}) do
    opts = Module.get_attribute(module, :tria_opts, [])
    context = Keyword.fetch!(opts, :context)

    delegations =
      module
      |> Module.get_attribute(:defs, [])
      |> :lists.reverse() # Because we get definitions in different order
      |> handle_overrides()
      |> expand_defaults()
      |> join_clauses()
      |> Enum.map(fn {{module, _kind, name, arity} = signature, _clauses} = definition ->
        Module.delete_definition(module, {name, arity})
        # Send definition for each clause without defaults
        ContextServer.emit_definition(context, definition)
        delegate(context, signature)
      end)

    ContextServer.mark_ready(context, module)

    Module.delete_attribute(module, :spec)
    Module.delete_attribute(module, :doc)

    {:__block__, [], delegations}
    # |> inspect_ast(label: module, with_contexts: true)
    # |> IO.inspect(label: module)
  rescue
    e ->
      IO.puts "Failed during compilation of #{inspect module}"
      reraise e, __STACKTRACE__
  end

  # Helpers

  defp handle_overrides(definitions) do
    Enum.reduce(definitions, [], fn
      # Is not marked overridable yet
      {false, definition}, acc -> [{false, definition} | acc]

      # It is overridable, therefore it will be overrided
      {true, {{module, kind, name, arity} = signature, clauses}}, acc ->
        acc =
          Enum.map(acc, fn
            {false, {^signature, clauses}} ->
              name = :"#{name} (overridable)"
              {}
          end)

    end)
  end

  # defp delegate(_, _, :defp, _, _), do: []
  defp delegate(tria_context, {module, kind, name, arity}) when kind in ~w[defmacro defmacrop]a do
    args = Macro.generate_unique_arguments(arity, nil)
    callargs = [{:__CALLER__, [], nil} | args]
    fname = fname(module, name)

    quote do
      unquote(kind)(unquote(name)(unquote_splicing args), do: unquote(tria_context).unquote(fname)(unquote_splicing callargs))
    end
  end
  defp delegate(tria_context, {module, kind, name, arity}) do
    args = Macro.generate_unique_arguments(arity, nil)
    fname = fname(module, name)

    quote do
      unquote(kind)(unquote(name)(unquote_splicing args), do: unquote(tria_context).unquote(fname)(unquote_splicing args))
    end
  end

  # We explicitly translate clauses before joining, because different clauses can have different envs
  defp translate_def({signature, [{args, guards, body}]}, env) do
    args   = ElixirTranslator.to_tria!(args,   %Macro.Env{env | context: :match})
    guards = ElixirTranslator.to_tria!(guards, %Macro.Env{env | context: :guard})
    body =
      case body do
        nil -> nil
        [do: body] -> [do: ElixirTranslator.to_tria!(body, env)]
        body ->
          {:try, _, [body]} = ElixirTranslator.to_tria!({:try, [], [body]}, env)
          body
      end

    {signature, [{args, guards, body}]}
  rescue
    e ->
      IO.puts "Failed during translation of #{inspect signature}"
      reraise e, __STACKTRACE__
  end

  # FIXME it is not handling lists
  defp join_clauses([{signature, left_clauses}, {signature, right_clauses} | tail]) do
    join_clauses [{signature, left_clauses ++ right_clauses} | tail]
  end
  defp join_clauses([{signature, clauses} | tail]) do
    clauses = Enum.reject(clauses, & match?({_, _, nil}, &1))
    [{signature, clauses} | join_clauses tail]
  end
  defp join_clauses([]), do: []

  def expand_defaults(definitions) do
    Enum.flat_map(definitions, fn {{module, kind, name, _arity} = signature, clauses} ->
      Enum.flat_map(clauses, fn {args, guards, body} ->
        defaulted =
          args
          |> do_expand_defaults()
          |> List.delete_at(-1) # Because first expanded default is always a full-arity call
          |> Enum.map(fn {sigargs, callargs} ->
            arity = length(sigargs)
            signature = {module, kind, name, arity}
            body = [do: dot_call(module, name, callargs)]
            clauses = [{sigargs, [], body}]
            {signature, clauses}
          end)

        args =
          # Here we drop default definitions from args
          Enum.map(args, fn
            {:"\\\\", _, [arg, _default]} -> arg
            arg -> arg
          end)

        defaulted ++ [{signature, [{args, guards, body}]}]
      end)
    end)
  end

  #TODO
  # This can be optimised not to compute
  # remove_defaults and leave_defaults on every iteration
  # But I don't mind O(n^2) complexity here

  # Returns list like [{arguments_in_signature, arguments_in_call}]
  defp do_expand_defaults([]), do: [{[], []}]
  defp do_expand_defaults([{:"\\\\", _, [arg, default]} | tail]) do
    # List where the argument is not passed
    where_arg_is_not_default =
      tail
      |> do_expand_defaults()
      |> Enum.map(fn {in_signature, in_call} ->
        {[arg | in_signature], [arg | in_call]}
      end)

    # List where the argument is passed
    head = {remove_defaults(tail), [default | leave_defaults(tail)]}
    [head | where_arg_is_not_default]
  end
  defp do_expand_defaults([arg | tail]) do
    Enum.map(do_expand_defaults(tail), fn {sig, call} -> {[arg | sig], [arg | call]} end)
  end

  defp remove_defaults([]), do: []
  defp remove_defaults([{:"\\\\", _, _} | tail]) do
    remove_defaults(tail)
  end
  defp remove_defaults([arg | tail]) do
    [arg | remove_defaults(tail)]
  end

  defp leave_defaults([]), do: []
  defp leave_defaults([{:"\\\\", _, [_arg, default]} | tail]) do
    [default | leave_defaults(tail)]
  end
  defp leave_defaults([arg | tail]) do
    [arg | leave_defaults(tail)]
  end

end
