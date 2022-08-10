defmodule Tria.Analyzer do

  use Tria.Ternary

  @moduledoc """
  Purity analyzer for Elixir in-compilation modules and already compiled erlang modules

  #TODO
  """

  def is_pure(module, func, args, opts \\ []) do
    if Keyword.get(opts, :quick, true) do
      quick_pure(module, func, args)
    else
      raise "Not implemented"
    end
  end

  defp quick_pure(module, func, args) do
    calls =
      try do
        module
        |> Module.get_definition({func, length(args)})
        |> get_elixir_ast_calls()
      rescue
        _ ->
          # Already compiled
          purity_map =
            module
            |> get_compiled_calls()
            |> traverse_calls()

      end
  end

  def get_calls(module) do
    
  end

  def traverse_calls(%{public: public, private: private}) do
    all = Map.merge(public, private)

  end

  def get_elixir_ast_calls(ast) do
    [{:erlang, :send, 2}] #FIXME
  end

  def fetch_chunks(module) do
    with(
      {^module, bin, _filename} <- :code.get_object_code(module),
      {:ok, {^module, chunks}} <- :beam_lib.chunks(bin, ~w[abstract_code locals exports]a)
    ) do
      {:ok, chunks}
    else
      error ->
        {:error, error}
    end
  end

  def fetch_chunk(module, chunk_name) do
    with {:ok, chunks} <- fetch_chunks(module), do: Keyword.fetch(chunks, chunk_name)
  end

  def fetch_abstract_code!(module) do
    {:ok, {:raw_abstract_v1, ac}} = fetch_chunk(module, :abstract_code)
    ac
  end

  def get_compiled_calls(module) do
    with(
      {:ok, chunks} <- fetch_chunks(module),
      {:ok, {:raw_abstract_v1, code}} <- Keyword.fetch(chunks, :abstract_code),
      {:ok, exports} <- Keyword.fetch(chunks, :exports),
      {:ok, locals} <- Keyword.fetch(chunks, :locals)
    ) do
      {public, private} =
        for {:function, _wtf, name, arity, body} <- code, into: %{} do
          calls = find_calls_in_abstract(body)
          {{name, arity}, calls}
        end
        |> enrich_fa(module)
        |> Enum.reduce({[], []}, fn {name_arity, _} = func, {public, private} ->
          if name_arity in exports do
            {[func | public], private}
          else
            {public, [func | private]}
          end
        end)

      {:ok, %{public: Map.new(public), private: Map.new(private)}}
    else
      other ->
        IO.inspect other
        :error
    end
  end

  defp find_calls_in_abstract(abstract_code, acc \\ [])
  # Apply
  defp find_calls_in_abstract(
     {:call, _, {:remote, _, {:atom, _, :erlang}, {:atom, _, :apply}}, [m, f, a]}, acc
  ) do
    find_calls_in_abstract({:call, 0, {:remote, 0, m, f}, uncons(a)}, acc)
  end
  # Local calls
  defp find_calls_in_abstract({:call, _, {:atom, _, func}, args}, acc) do
    [{:fa, func, length(args)} | find_calls_in_abstract(args, acc)]
  end
  # Remote calls
  defp find_calls_in_abstract({:call, _, {:remote, _, {:atom, _, module}, {:atom, _, func}}, args}, acc) do
    [{:mfa, module, func, length(args)} | find_calls_in_abstract(args, acc)]
  end
  # Calls with non-atom module or function (or both)
  defp find_calls_in_abstract({:call, _, what, args}, acc) do
    [{:varied, what, length(args)} | find_calls_in_abstract(args, acc)]
  end
  # Operations
  defp find_calls_in_abstract({:op, _, op, left, right}, acc) do
    args = [left, right]
    [{:op, op, length(args)} | find_calls_in_abstract(args, acc)]
  end
  defp find_calls_in_abstract(t, acc) when is_tuple(t) do
    t
    |> Tuple.to_list()
    |> find_calls_in_abstract(acc)
  end
  defp find_calls_in_abstract([head | tail], acc) do
    acc = find_calls_in_abstract(head, acc)
    find_calls_in_abstract(tail, acc)
  end
  defp find_calls_in_abstract(_, acc), do: acc

  defp enrich_fa(funs, module) do
    Enum.reduce(funs, funs, fn {key, calls}, funs ->
      calls =
        Enum.map(calls, fn
          {:fa, name, arity} ->
            name_arity = {name, arity}
            case funs do
              %{^name_arity => _} ->
                {:mfa, module, name, arity}

              _ ->
                {:mfa, :erlang, name, arity}
            end

          other ->
            other
        end)

      Map.put(funs, key, calls)
    end)
  end

  defp uncons({:cons, _, item, tail}) do
    [item | uncons(tail)]
  end
  defp uncons({nil, _}), do: []

  def is_pure() do

  end

end
