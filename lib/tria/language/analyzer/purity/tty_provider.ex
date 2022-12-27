defmodule Tria.Language.Analyzer.Purity.TTYProvider do

  @moduledoc """
  Purity Provider which reads the purity information from stdin.
  It's a singleton server to make sure that only one question is
  asked at a time
  """

  import Tria.Language
  alias Tria.Language.Analyzer.Purity.Provider
  alias Tria.Language.Codebase

  @behaviour Provider

  defguard is_empty(s) when s in [[], nil]

  use GenServer

  @impl Provider
  def is_pure(mfa, opts \\ []) do
    GenServer.call(start(), {:is_pure, mfa, opts}, :infinity)
  end

  defp start(opts \\ []) do
    case GenServer.start(__MODULE__, opts, name: __MODULE__) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  @impl GenServer
  def init(_opts) do
    state = %{}
    {:ok, state}
  end

  @impl GenServer
  def handle_call({:is_pure, mfa, opts}, _, state) do
    result = ask(mfa, Enum.into(opts, %{stack: [], show: nil}))
    {:reply, result, state}
  end

  defp ask(mfa, %{stack: stack} = opts) do
    mfa
    |> prompt(opts)
    |> IO.gets()
    |> String.codepoints()
    |> List.first()
    |> case do
      "y" ->
        true

      "n" ->
        false

      "s" when not is_empty(stack) ->
        Enum.each(stack, fn {m, f, a} -> IO.puts "#{m}.#{f}/#{a}" end)
        ask(mfa, opts)

      "S" ->
        inspect_ast Codebase.fetch_tria(mfa), label: :show
        ask(mfa, opts)

      _ ->
        ask(mfa, opts)
    end
  end

  defp prompt({m, f, a}, %{stack: stack}) when not is_empty(stack) do
    "\n#{ast_to_string {{:".", [], [m, f]}, [], a}}\n\nIs pure [y(yes); n(no); S(show); s(stack)] "
  end

  defp prompt({m, f, a}, _) do
    "\n#{ast_to_string {{:".", [], [m, f]}, [], a}}\n\nIs pure [y(yes); n(no); S(show)] "
  end

end
