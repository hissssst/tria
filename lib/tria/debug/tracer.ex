defmodule Tria.Debug.Tracer do

  @moduledoc """
  Dead simple module which leverages `:persistent_term` to
  trace changes of functions during compilation.

  Debugging purposes only
  """

  import Tria.Language, only: [ast_to_string: 2], warn: false
  # import Tria.Compiler, only: [fname: 1]

  @doc """
  Adds one function or multiple functions to the tracing.
  Functions are defined in the mfarity form
  """
  def trace(key, opts \\ [only: :all]) do
    :persistent_term.put({__MODULE__, key}, Map.new(opts))
  end

  @doc """
  Emits a trace of the function in the stdout if the function is traced.
  `term` is inspected with given opts and printed to stdout.
  """
  def tag(term, opts) do
    if key = opts[:key] || Process.get(:local_trace) do
      {label, opts} = Keyword.pop!(opts, :label)
      do_trace(key, term, &inspect(&1, opts), label)
    end

    term
  end

  @doc """
  Emits a trace of the function in the stdout if the function is traced.
  `ast` is formatted with given opts and written to stdout.
  """
  def tag_ast(ast, opts) do
    if key = opts[:key] || Process.get(:local_trace) do
      {label, opts} = Keyword.pop!(opts, :label)
      opts = Keyword.put_new(opts, :with_contexts, true)
      do_trace(key, ast, &ast_to_string(&1, opts), label)
    end

    ast
  end

  @doc """
  Puts an info about currently chaged function into pdict for further tracing
  """
  def with_local_trace(key, func) do
    old = Process.put(:local_trace, key)
    try do
      func.()
    after
      old && Process.put(:local_trace, old) || Process.delete(:local_trace)
    end
  end


  defp do_trace({module, _kind, function, arity}, data, inspector, label) do
    do_trace({module, function, arity}, data, inspector, label)
  end
  defp do_trace({module, function, arity} = mfarity, data, inspector, label) do
    with %{only: labels} <- :persistent_term.get({__MODULE__, mfarity}, nil) do
      if right_label?(label, labels) do
        print(label, "#{module}.#{function}/#{arity}", inspector, data)
      end
    end
  end
  defp do_trace(fname, data, inspector, label) when is_atom(fname) do
    case :persistent_term.get({__MODULE__, fname}, nil) do
      %{mfarity: {module, function, arity}, only: labels} ->
        if right_label?(label, labels) do
          print(label, "#{module}.#{function}/#{arity}", inspector, data)
        end

      %{only: labels} ->
        if right_label?(label, labels) do
          print(label, fname, inspector, data)
        end

      _ ->
        nil
    end
  end

  defp right_label?(_, :all), do: true
  defp right_label?(label, labels), do: label in labels

  defp print(label, keystr, inspector, data) do
    IO.puts """

    #{label}: #{keystr}
    #{inspector.(data)}
    """
  end
end
