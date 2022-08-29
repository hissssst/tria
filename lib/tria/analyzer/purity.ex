defmodule Tria.Analyzer.Purity do

  import Tria.Common
  import Tria.Tri

  alias Tria.{Analyzer, Fundb}
  alias Tria.Translator.Elixir, as: ElixirTranslator

  @doc """
  Runs valid code and checks if any of effect functions is called
  """
  def run_analyze(ast) do
    me = {:me, [], nil}
    body =
      Macro.postwalk(ast, fn
        dot_call(m, f, a) = mfa ->
          if lookup {m, f, a} do
            mfa
          else
            quote do: send(unquote(me), {:effect, unquote Macro.escape mfa})
          end

        other ->
          other
      end)

    body =
      quote do
        unquote(me) = self()
        unquote(body)
      end

    {type, result} =
      try do
        {:ok, Code.eval_quoted(body, [])}
      rescue
        error -> {:error, error}
      catch
        :exit, exit -> {:exit, exit}
        thrown -> {:thrown, thrown}
      end

    effects = fetch_effects()
    case type do
      :ok when effects == [] ->
        {:pure, result, effects}

      :ok ->
        {:effect, result, effects}

      other ->
        {other, result, effects}
    end
  end

  @doc """
  Checks if effect calls are present in ast
  """
  def check_analyze(ast) do
    # IO.puts "Analyzing ===="
    # IO.puts Macro.to_string ast
    # IO.puts "=============="
    Macro.prewalk(ast, [], fn
      dot_call(m, f, a), acc ->
        mfa = {unalias(m), f, a}
        # IO.inspect mfa, label: :mfa
        if lookup mfa do
          {mfa, acc}
        else
          {mfa, [mfa | acc]}
        end

      tri(_.(tri_splicing _)) = func, acc ->
        {func, [func | acc]}

      other, acc ->
        {other, acc}
    end)
    |> case do
      {_, []} -> true
      _ -> false
    end
  end

  defp lookup({m, f, a} = mfa) when is_atom(m) do
    if in? arityfy mfa do
      true
    else
      with nil <- Fundb.lookup mfa, :pure do
        dotted = {{:".", [], [m, f]}, [], a}
        case Analyzer.fetch_tria(mfa) do
          nil ->
            ask(mfa)

          {:fn, _, [{:"->", _, [_, dot_call(:erlang, :nif_error, _)]}]} ->
            ask(mfa)

          {:fn, _, [{:"->", _, [_, ^dotted]}]} ->
            ask(mfa)

          ast ->
            mfa = push arityfy mfa
            # IO.inspect mfa
            res = check_analyze(ast)
            Fundb.insert(mfa, :pure, res)
            pop()
            res
        end
      end
    end
  end
  defp lookup({_, _f, _a}), do: false

  defp fetch_effects() do
    receive do
      {:effect, effect} -> [effect | fetch_effects()]
      after 0 -> []
    end
  end

  defp ask({m, f, a} = mfa) do
    Fundb.at(fn ->
      with nil <- Fundb.lookup(mfa, :pure) do
        res = ask("===\n#{Macro.to_string {{:".", [], [m, f]}, [], a}}\n===\nIs pure [yn] ")
        Fundb.insert(mfa, :pure, res)
        res
      end
    end)
  end

  defp ask(string) do
    string
    |> IO.gets()
    |> String.downcase()
    |> String.codepoints()
    |> List.first()
    |> case do
      "y" -> true
      "n" -> false
      _ -> ask(string)
    end
  end

  defp push(mfa) do
    stack = Process.get(:stack, [])
    Process.put(:stack, [mfa | stack])
    mfa
  end

  defp pop do
    {first, stack} =
      :stack
      |> Process.get([])
      |> List.pop_at(0)

    Process.put(:stack, stack)
    first
  end

  defp in?(mfa) do
    mfa in Process.get(:stack, [])
  end
  
end
