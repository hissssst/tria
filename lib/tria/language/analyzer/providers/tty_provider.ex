defmodule Tria.Language.Analyzer.TTYProvider do

  @moduledoc """
  Purity Provider which reads the purity information from stdin.
  It's a singleton server to make sure that only one question is
  asked at a time
  """

  import Tria.Language
  alias Tria.Language.Analyzer.Provider
  alias Tria.Language.Beam

  @behaviour Provider

  defguard is_empty(s) when s in [[], nil]

  use GenServer

  @impl Provider
  def decide(trait, mfa, opts \\ []) do
    GenServer.call(start(), {:decide, trait, mfa, opts}, :infinity)
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
  def handle_call({:decide, trait, mfa, opts}, _, state) do
    result = ask(trait, mfa, Enum.into(opts, %{stack: [], show: nil}))
    {:reply, result, state}
  end

  defp ask(trait, {module, _, _} = mfa, %{stack: stack} = opts) do
    trait
    |> prompt(mfa, opts)
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
        ask(trait, mfa, opts)

      "S" ->
        module
        |> Beam.object_code!()
        |> Beam.abstract_code!()
        |> Beam.tria(mfa)
        |> inspect_ast(label: :show)
        ask(trait, mfa, opts)

      _ ->
        ask(trait, mfa, opts)
    end
  end

  defp prompt(trait, {m, f, a}, %{stack: stack}) when not is_empty(stack) do
    "\n#{ast_to_string {{:".", [], [m, f]}, [], a}}\n\nIs #{trait} [y(yes); n(no); S(show); s(stack)] "
  end

  defp prompt(trait, {m, f, a}, _) do
    "\n#{ast_to_string {{:".", [], [m, f]}, [], a}}\n\nIs #{trait} [y(yes); n(no); S(show)] "
  end

end
