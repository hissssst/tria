defmodule Tria.Debug.Tracer do

  @moduledoc """
  Dead simple module which leverages `:persistent_term` to
  trace changes of functions during compilation.

  Debugging purposes only
  """

  import Tria.Language, only: [ast_to_string: 2], warn: false
  alias Tria.Language.MFArity
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
    pid =
      spawn fn ->
        Process.sleep :timer.seconds 5
        MFArity.inspect(key, label: :takes_too_long)
      end

    try do
      func.()
    after
      Process.exit(pid, :kill)
      old && Process.put(:local_trace, old) || Process.delete(:local_trace)
    end
  end

  @doc """
  Parses string passed to tria tracing env

  ## Example:

      iex> parse_trace_env "Foo.bar/2,Boo.far/0"
      [{Foo, :bar, 2}, {Boo, :far, 0}]
  """
  @spec parse_trace_env(String.t()) :: [MFArity.mfarity()]
  def parse_trace_env(string) do
    "[#{string}]"
    |> Code.string_to_quoted!()
    |> Enum.map(fn {:/, _, [{{:".", _, [{:__aliases__, _, aliases}, function]}, _, _}, arity]} when is_integer(arity) and is_atom(function) ->
      {Module.concat(aliases), function, arity}
    end)
  rescue
    exception ->
      IO.warn "Tracing string syntax is incorrect"
      reraise exception, __STACKTRACE__
  end

  defp do_trace({module, _kind, function, arity}, data, inspector, label) do
    do_trace({module, function, arity}, data, inspector, label)
  end
  defp do_trace(fname, data, inspector, label) when is_atom(fname) do
    case :persistent_term.get({__MODULE__, fname}, nil) do
      %{mfarity: mfarity, only: labels} ->
        if right_label?(label, labels) do
          print(label, MFArity.to_string(mfarity), inspector, data)
        end

      %{only: labels} ->
        if right_label?(label, labels) do
          print(label, fname, inspector, data)
        end

      _ ->
        nil
    end
  end
  defp do_trace(mfarity, data, inspector, label) do
    with %{only: labels} <- :persistent_term.get({__MODULE__, mfarity}, nil) do
      if right_label?(label, labels) do
        print(label, MFArity.to_string(mfarity), inspector, data)
      end
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
