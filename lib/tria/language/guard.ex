defmodule Tria.Language.Guard do

  @moduledoc """
  Helper for working with guards
  """

  import Tria.Language
  import Tria.Language.Tri
  import Tria.Language.Binary, only: [traverse_specifier: 3]

  @checks [
    # Checks
    {:erlang, :is_atom, 1},
    {:erlang, :is_binary, 1},
    {:erlang, :is_bitstring, 1},
    {:erlang, :is_boolean, 1},
    {:erlang, :is_float, 1},
    {:erlang, :is_function, 1},
    {:erlang, :is_function, 2},
    {:erlang, :is_integer, 1},
    {:erlang, :is_list, 1},
    {:erlang, :is_map, 1},
    {:erlang, :is_number, 1},
    {:erlang, :is_pid, 1},
    {:erlang, :is_port, 1},
    # {:erlang, :is_record, 2},
    # {:erlang, :is_record, 3},
    {:erlang, :is_reference, 1},
    {:erlang, :is_tuple, 1},
  ]

  @available @checks ++ [
    # Allowed functions
    {:erlang, :abs, 1},
    {:erlang, :bit_size, 1},
    {:erlang, :byte_size, 1},
    {:erlang, :element, 2},
    {:erlang, :float, 1},
    {:erlang, :hd, 1},
    {:erlang, :is_map_key, 2},
    {:erlang, :length, 1},
    {:erlang, :map_get, 2},
    {:erlang, :map_size, 1},
    {:erlang, :node, 0},
    {:erlang, :node, 1},
    {:erlang, :round, 1},
    {:erlang, :self, 0},
    {:erlang, :size, 1},
    {:erlang, :tl, 1},
    {:erlang, :trunc, 1},
    {:erlang, :tuple_size, 1},

    # Operators
    {:erlang, :and, 2},
    {:erlang, :or, 2},
    {:erlang, :xor, 2},
    {:erlang, :orelse, 2},
    {:erlang, :andalso, 2},
    {Kernel,  :==, 2},
    {Kernel,  :!=, 2},
    {Kernel,  :<=, 2},
    {Kernel,  :>=, 2},
    {Kernel,  :<, 2},
    {Kernel,  :>, 2},
    {Kernel,  :===, 2},
    {Kernel,  :!==, 2},
    {Kernel,  :+, 2},
    {Kernel,  :-, 2},
    {Kernel,  :*, 2},
    {Kernel,  :/, 2},
    {Kernel,  :div, 2},
    {Kernel,  :rem, 2},
    {Kernel,  :not, 1},
    {Kernel,  :or, 2},
    {Kernel,  :and, 2},

    # Elixir
    {Kernel, :*, 2},
    {Kernel, :+, 1},
    {Kernel, :+, 2},
    {Kernel, :-, 1},
    {Kernel, :-, 2},
    {Kernel, :/, 2},
    {Kernel, :!=, 2},
    {Kernel, :!==, 2},
    {Kernel, :<, 2},
    {Kernel, :<=, 2},
    {Kernel, :==, 2},
    {Kernel, :===, 2},
    {Kernel, :>, 2},
    {Kernel, :>=, 2},
    {Kernel, :abs, 1},
    {Kernel, :and, 2},
    {Kernel, :binary_part, 3},
    {Kernel, :bit_size, 1},
    {Kernel, :byte_size, 1},
    {Kernel, :ceil, 1},
    {Kernel, :div, 2},
    {Kernel, :elem, 2},
    {Kernel, :floor, 1},
    {Kernel, :hd, 1},
    {Kernel, :in, 2},
    {Kernel, :is_atom, 1},
    {Kernel, :is_binary, 1},
    {Kernel, :is_bitstring, 1},
    {Kernel, :is_boolean, 1},
    {Kernel, :is_exception, 1},
    {Kernel, :is_exception, 2},
    {Kernel, :is_float, 1},
    {Kernel, :is_function, 1},
    {Kernel, :is_function, 2},
    {Kernel, :is_integer, 1},
    {Kernel, :is_list, 1},
    {Kernel, :is_map, 1},
    {Kernel, :is_map_key, 2},
    {Kernel, :is_nil, 1},
    {Kernel, :is_number, 1},
    {Kernel, :is_pid, 1},
    {Kernel, :is_port, 1},
    {Kernel, :is_reference, 1},
    {Kernel, :is_struct, 1},
    {Kernel, :is_struct, 2},
    {Kernel, :is_tuple, 1},
    {Kernel, :length, 1},
    {Kernel, :map_size, 1},
    {Kernel, :node, 0},
    {Kernel, :node, 1},
    {Kernel, :not, 1},
    {Kernel, :or, 2},
    {Kernel, :rem, 2},
    {Kernel, :round, 1},
    {Kernel, :self, 0},
    {Kernel, :tl, 1},
    {Kernel, :trunc, 1},
    {Kernel, :tuple_size, 1},
  ]
  |> Enum.uniq()

  def available, do: @available

  defguardp is_guard_mfa(m, f, a) when
            (is_integer(a) and {m, f, a} in @available)
            or (is_list(a) and {m, f, length(a)} in @available)

  @doc """
  Checks if given AST can be a guard expression
  """
  @spec is_guard(Tria.t()) :: boolean()
  def is_guard(ast) do
    case ast do
      dot_call(module, function, args) when is_guard_mfa(module, function, args) ->
        is_guard(args)

      variable when is_variable(variable) ->
        true

      [] ->
        true

      tri [head | tail] ->
        is_guard(head) and is_guard(tail)

      [head | tail] ->
        is_guard(head) and is_guard(tail)

      {left, right} ->
        is_guard(left) and is_guard(right)

      tri %{_ | tri_splicing _} ->
        # Map update syntax is not available in Elixir
        # but it is available in Erlang
        false

      {map_or_tuple, _, pairs} when map_or_tuple in ~w[%{} {}]a ->
        is_guard(pairs)

      {:<<>>, _, binary} ->
        {_, result} =
          traverse_specifier(binary, true, fn spec, acc ->
            {spec, is_guard(spec) and acc}
          end)

        result

      other ->
        Macro.quoted_literal?(other)
    end
    # |> tap(fn r ->
    #   IO.inspect(ast, label: :ast)
    #   IO.inspect(r, label: :result)
    # end)
  end

  @spec append_guard([Tria.t()], Tria.t() | nil) :: [Tria.t()]
  def append_guard(args, nil), do: args
  def append_guard(args, guard) do
    [{:when, [], args ++ [guard]}]
  end

  @spec pop_guard([Tria.t()]) :: {guard :: Tria.t(), args :: Tria.t()}
  def pop_guard([{:when, _meta, args_and_guard}]) do
    List.pop_at(args_and_guard, -1)
  end
  def pop_guard(args), do: {nil, args}

  def unjoin_when({:when, _, [left, right]}) do
    [left | unjoin_when(right)]
  end
  def unjoin_when(other), do: other

  def join_when(guards, meta \\ [])
  def join_when([other], _), do: other
  def join_when([head | tail], meta), do: {:when, meta, [head, join_when(tail, meta)]}

end
