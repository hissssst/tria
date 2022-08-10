defmodule Tria.Analyzer.Purity do

  import Tria.Common
  alias Tria.Fundb

  alias Tria.Translator.Elixir, as: ElixirTranslator

  def test do
    [
      {Map, :get, 2},
      {Map, :get, 3},
      {Kernel, :"+", 2},
      {Kernel, :"-", 2}
    ]
    |> Enum.each(&Fundb.insert(&1, :pure, true))

    [
      {Kernel, :send, 2},
      {Kernel, :self, 0}
    ]
    |> Enum.each(&Fundb.insert(&1, :pure, false))

    quote do
      x = Map.get(%{x: 1}, :x)
      y = Map.get(%{y: 2}, :y)
      z = x + y

      send(self(), x + y)
    end
    |> ElixirTranslator.to_tria!(__ENV__)
    |> run_analyze()
  end

  @doc """
  Runs valid code and checks if any of impure functions is called
  """
  def run_analyze(ast) do
    me = {:me, [], nil}
    body =
      Macro.postwalk(ast, fn
        dot_call(_, _, _) = mfa ->
          if lookup mfa do
            mfa
          else
            quote do: send(unquote(me), {:impure, unquote Macro.escape mfa})
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

    impures = fetch_impures()
    case type do
      :ok when impures == [] ->
        {:pure, result, impures}

      :ok ->
        {:impure, result, impures}

      other ->
        {other, result, impures}
    end
  end

  @doc """
  Checks if impure calls are present in ast
  """
  def check_analyze(ast) do
    Macro.prewalk(ast, [], fn
      dot_call(_, _, _) = mfa, acc ->
        if lookup mfa do
          {mfa, acc}
        else
          {mfa, [mfa | acc]}
        end

      other, acc ->
        {other, acc}
    end)
  end

  defp lookup(mfa) do
    with nil <- Fundb.lookup mfa, :pure do
      raise "Not implemented #{inspect mfa}"
    end
  end

  defp fetch_impures() do
    receive do
      {:impure, impure} ->
        [impure | fetch_impures()]
    after 0 ->
      []
    end
  end
  
end
