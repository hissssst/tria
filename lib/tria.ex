defmodule Tria do

  @moduledoc """
  Macro building helpers
  """

  @special_forms ~w[
    % %{} & . :: <<>> = ^ {}
    __CALLER__ __DIR__ __ENV__ __MODULE__ __STACKTRACE__
    __aliases__ __block__
    alias case cond fn for import quote receive require
    super try unquote unquote_splicing with
  ]a

  defmacrop element(tuple, index) do
    index = index + 1
    quote(do: :erlang.element(unquote(index), unquote(tuple)))
  end

  defguard is_var(t) when is_tuple(t) and tuple_size(t) == 3
    and is_atom(element(t, 0))
    and is_list(element(t, 1))
    and (is_atom(element(t, 2))
    or is_nil(element(t, 2)))

  defguard is_dot(t) when is_tuple(t) and tuple_size(t) == 3
    and (element(t, 0) == :.)
    and is_list(element(t, 1))
    and is_list(element(t, 2))

  defguard is_node(t) when is_tuple(t) and tuple_size(t) == 3
    and (is_atom(element(t, 0)) or is_dot(element(t, 0)))
    and is_list(element(t, 1))
    and is_list(element(t, 2))

  defguard is_atomic(term) when is_atom(term)
    or is_number(term)
    or is_binary(term)
    or is_function(term)

  defguard is_call(t) when is_node(t) and (
    (element(t, 0) not in @special_forms) or is_dot(element(t, 0))
  )

end
