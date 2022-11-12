defmodule Tria.Tracer do

  @moduledoc """
  Dead simple module which leverages `:persistent_term` to
  trace changes of functions during compilation.

  Debugging purposes only
  """

  import Tria.Common, only: [inspect_ast: 2]

  @doc """
  Adds one function or multiple functions to the tracing.
  Functions are defined in the mfarity form
  """
  def trace(mfarities) when is_list(mfarities) do
    tracing = :persistent_term.get(__MODULE__, [])
    :persistent_term.put(__MODULE__, mfarities ++ tracing)
  end
  def trace(mfarity), do: trace [mfarity]

  @doc """
  Emits a trace of the function in the stdout if the function is traced.
  `term` is inspected with given opts and printed to stdout.
  """
  def tag(term, {module, name, arity} = mfarity, opts) do
    if mfarity in :persistent_term.get(__MODULE__, []) do
      IO.puts "#{inspect module}.#{name}/#{arity}"
      IO.inspect(term, opts)
    end

    term
  end

  @doc """
  Emits a trace of the function in the stdout if the function is traced.
  `ast` is formatted with given opts and written to stdout.
  """
  def tag_ast(ast, {module, name, arity} = mfarity, opts) do
    if mfarity in :persistent_term.get(__MODULE__, []) do
      IO.puts "#{inspect module}.#{name}/#{arity}"
      inspect_ast(ast, opts)
    end

    ast
  end

end
