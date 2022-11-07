defmodule Tria.Compiler do

  @moduledoc """
  Helpers for compiling Elixir modules
  """

  import Tria.Common
  alias Tria.Compiler.ContextServer

  # Project compilation pipeline

  def to_quoted(file) do
    quoted =
      file
      |> File.read!()
      |> Code.string_to_quoted!(file: file)

    {quoted, %{file: file}}
  end

  def start_compilation({quoted, meta}), do: start_compilation(quoted, meta)
  def start_compilation(quoted, meta) do
    prepared =
      quoted
      |> add_use()
      |> add_meta(meta)

    {
      Task.async(fn -> compile_quoted(prepared, meta.file) end),
      Map.put_new(meta, :context, TriaGlobalContext)
    }
  end

  def finish_compilation({task, meta}), do: finish_compilation(task, meta)
  def finish_compilation(task, meta) do
    {Task.await(task, :infinity), meta}
  end

  def generate_context(context) do
    ContextServer.generate(context)
  end

  # Helpers

  defp add_meta(ast, %{file: file}) do
    Macro.prewalk(ast, fn ast ->
      Macro.update_meta(ast, fn meta ->
        case meta[:file] do
          nil -> Keyword.put(meta, :file, file)
          _ -> meta
        end
      end)
    end)
  end

  defp add_use(ast) do
    Macro.prewalk(ast, fn
      {kind, meta, opts} when kind in ~w[defmodule defimpl]a ->
        opts = List.update_at(opts, -1, &do_add_use(&1, kind))
        {kind, meta, opts}

      other ->
        other
    end)
  end

  def save(build_path, module, binary) do
    filename = Path.join(build_path, "#{module}.beam")
    File.write!(filename, binary)
  end

  @doc """
  Like `Code.compile_quoted/2` but with error descriptions
  """
  def compile_quoted(quoted, file \\ "nofile") do
    try do
      Code.compile_quoted(quoted, file)
    rescue
      _ ->
        lined =
          postwalk(quoted, fn ast ->
            Macro.update_meta(ast, &Keyword.put(&1, :line, :erlang.unique_integer([:positive])))
          end)

        try do
          Code.compile_quoted(lined, file)
        rescue
          e in CompileError ->
            inspect_ast(lined, label: :failed_to_compile, with_contexts: true, highlight_line: e.line)
            reraise e, __STACKTRACE__
        end
    end
  end

  @doc """
  Converts module, name to the name in the context module
  """
  def fname(module, name) do
    module
    |> Module.split()
    |> Enum.map(&snake/1)
    |> Kernel.++([to_string name])
    |> Enum.join("_")
    |> String.to_atom()
  end

  @doc """
  Converts `def`'s `{args, guards, body}` clause or clauses to fn
  """
  def def_to_fn(clauses) when is_list(clauses) do
    fn_clauses =
      Enum.flat_map(clauses, fn {args, guards, body} ->
        pattern = add_guards(args, guards)
        body =
          case body do
            [do: body] -> body
            body -> {:try, [], [body]}
          end

        quote do
          unquote_splicing pattern -> unquote body
        end
      end)

    {:fn, [], fn_clauses}
  end
  def def_to_fn(args, guards, body) do
    def_to_fn [{args, guards, body}]
  end

  @doc """
  Converts fn to `def`'s `{args, guards, body}` clause or clauses
  """
  def fn_to_def({:fn, _, fn_clauses}) do
    fn_to_def(fn_clauses)
  end

  def fn_to_def(fn_clauses) when is_list(fn_clauses) do
    Enum.map(fn_clauses, fn
      {:"->", _, [pattern, {:try, _, [body]}]} ->
        {args, guards} = pop_guards(pattern)
        {args, guards, body}

      {:"->", _, [pattern, body]} ->
        {args, guards} = pop_guards(pattern)
        {args, guards, [do: body]}
    end)
  end

  # Helpers

  #FIXME fails when `quote do: defmodule`
  defp do_add_use(body, _kind) do
    Keyword.update!(body, :do, fn
      {:__block__, meta, block} ->
        {:__block__, meta, [{:use, [], [Tria]} | block]}

      line ->
        {:__block__, [], [{:use, [], [Tria]}, line]}
    end)
  end

  # Converts camel case strings to snake case
  defp snake(""), do: ""
  defp snake(<<first, second, rest :: binary>>) when first not in ?A..?Z and second in ?A..?Z do
    <<first, ?_, second - ?A + ?a>> <> snake(rest)
  end
  defp snake(<<upper, rest :: binary>>) when upper in ?A..?Z do
    <<upper - ?A + ?a>> <> snake(rest)
  end
  defp snake(<<other, rest :: binary>>) do
    <<other>> <> snake(rest)
  end

  defp add_guards(args, []), do: args
  defp add_guards(args, guards) do
    List.update_at(args, -1, fn last_arg ->
      quote do
        unquote(last_arg) when unquote(join_guards guards)
      end
    end)
  end

  defp join_guards([guard]), do: guard
  defp join_guards([guard | guards]) do
    {:when, [], [guard, join_guards(guards)]}
  end

  defp unjoin_guards({:when, _, [guard, guards]}), do: [guard | unjoin_guards guards]
  defp unjoin_guards(guard), do: [guard]

  defp pop_guards(pattern) do
    case :lists.reverse(pattern) do
      [{:when, _, [arg, guards]} | tail] ->
        {:lists.reverse([arg | tail]), unjoin_guards(guards)}

      _ ->
        {pattern, []}
    end
  end

end
