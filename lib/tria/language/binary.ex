defmodule Tria.Language.Binary do
  @moduledoc """
  Helper module for working with binary type-specifiers
  """

  # Helper for zero-based tuple indexing
  defmacrop element(tuple, index) when is_integer(index) do
    quote do: :erlang.element(unquote(index + 1), unquote(tuple))
  end

  @reserved ~w[
    integer
    float
    bits
    bitstring
    binary
    bytes
    utf8
    utf16
    utf32
    signed
    unsigned
    big
    little
    native
  ]a

  @type state :: any()

  @type traverse_func :: (Tria.t(), state() -> {Tria.t(), state()})

  @doc "Checks if the variable ast is a reserved type specifier"
  defguard is_reserved(t) when is_tuple(t) and tuple_size(t) == 3
     and element(t, 0) in @reserved
     and is_list(element(t, 1))
     and (is_atom(element(t, 2)) or is_integer(element(t, 2)))

  @parametrised ~w[size unit]a

  @doc "Checks if the variable ast is a parametrised type specifier"
  defguard is_parametrised(t) when is_tuple(t) and tuple_size(t) == 3
     and element(t, 0) in @parametrised
     and is_list(element(t, 1))
     and is_list(element(t, 2))
     and length(element(t, 2)) == 1

  @doc """
  Traverses type specifications of binary
  """
  @spec traverse_binary_specifiers(Tria.t(), state(), traverse_func()) :: {Tria.t(), state()}
  def traverse_binary_specifiers({:<<>>, meta, parts}, state, func) do
    {parts, state} =
      Enum.map_reduce(parts, state, fn
        {:"::", meta, [value, specifier]}, state ->
          {specifier, state} = traverse_specifier(specifier, state, func)
          { {:"::", meta, [value, specifier]}, state }

        value, state ->
          { value, state }
      end)

    { {:<<>>, meta, parts}, state }
  end

  @doc """
  Traverses one type specification
  """
  @spec traverse_specifier(Tria.t(), state(), traverse_func()) :: {Tria.t(), state()}
  def traverse_specifier(ast, state, func) do
    case ast do
      {op, meta, [left, right]} when op in ~w[* -]a ->
        {left, state} = traverse_specifier(left, state, func)
        {right, state} = traverse_specifier(right, state, func)
        { {op, meta, [left, right]}, state }

      {name, meta, [arg]} = spec when is_parametrised(spec) ->
        {arg, state} = func.(arg, state)
        { {name, meta, arg}, state }

      spec when is_reserved(spec) ->
        { spec, state }

      other ->
        func.(other, state)
    end
  end

end
