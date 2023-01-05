defmodule Tria.Language.MFArity do

  @moduledoc """
  Helper for working with MFArity structure
  """

  @type mfargs :: {module(), atom(), [Tria.t()]}

  @type mfarity :: {module(), atom(), non_neg_integer()}

  @type dotcall :: {{:".", list(), [module() | atom()]}, list(), [Tria.t()]}

  import Tria.Language
  import Kernel, except: [inspect: 1, inspect: 2, to_string: 1]

  defmacrop element(tuple, index) do
    quote do: :erlang.element(unquote(index + 1), unquote(tuple))
  end

  ## Guards

  defguard is_mfarity(module, function, arity)
    when is_atom(module) and is_atom(function) and is_integer(arity) and arity >= 0

  defguard is_mfarity(mfarity)
    when is_triple(mfarity) and is_mfarity(element(mfarity, 0), element(mfarity, 1), element(mfarity, 2))

  defguard is_mfargs(module, function, args)
    when is_atom(module) and is_atom(function) and is_list(args)

  defguard is_mfargs(mfargs)
    when is_triple(mfargs) and is_mfargs(element(mfargs, 0), element(mfargs, 1), element(mfargs, 2))

  @doc """
  Checks if passed AST is a function call with dot (like Module.function)
  """
  defguard is_dotcall(t)
    when is_triple(t)
    and is_list(element(t, 1))
    and is_list(element(t, 2))
    and (
          is_triple(element(t, 0))
          and element(element(t, 0), 0) == :.
          and is_list(element(element(t, 0), 1))
          and is_list(element(element(t, 0), 2))
          and length(element(element(t, 0), 2)) == 2
        )

  ## Converters

  @spec to_mfarity(mfargs() | dotcall() | mfarity()) :: mfarity()
  def to_mfarity({module, function, args}) when is_mfargs(module, function, args) do
    {module, function, length(args)}
  end

  def to_mfarity(dot_call(module, function, args)) do
    {module, function, length(args)}
  end

  def to_mfarity(mfarity) when is_mfarity(mfarity) do
    mfarity
  end

  ### To mfargs

  @spec to_mfargs(mfargs() | dotcall() | mfarity()) :: mfargs()
  def to_mfargs({module, function, arity}) when is_mfarity(module, function, arity) do
    {module, function, List.duplicate(nil, arity)}
  end

  def to_mfargs(dot_call(module, function, args)) when is_mfargs(module, function, args) do
    {module, function, args}
  end

  def to_mfargs(mfargs) when is_mfargs(mfargs), do: mfargs

  ### To dotcall

  @spec to_dotcall(mfargs() | dotcall() | mfarity()) :: dotcall()
  def to_dotcall({module, function, args}) when is_mfargs(module, function, args) do
    dot_call(module, function, args)
  end

  def to_dotcall({module, function, arity}) when is_mfarity(module, function, arity) do
    dot_call(module, function, List.duplicate(nil, arity))
  end

  def to_dotcall(dotcall) when is_dotcall(dotcall), do: dotcall

  ### To AST

  @spec to_ast(mfargs() | dotcall() | mfarity()) :: Tria.t()
  def to_ast({module, function, arity}) when is_mfarity(module, function, arity) do
    {:"&", [], [{:"/", [], [{{:".", [], [module, function]}, [no_parens: true], []}, arity]}]}
  end

  def to_ast(other) do
    to_dotcall other
  end

  ### Inspect

  @spec inspect(input, Keyword.t()) :: input
        when input: mfargs() | dotcall() | mfarity()
  def inspect(something, opts \\ []) do
    IO.inspect(to_string(something, opts), opts)
  end

  ### To string

  @spec to_string(mfargs() | dotcall() | mfarity(), Keyword.t()) :: binary()
  def to_string(something, opts \\ []) do
    something
    |> to_ast()
    |> ast_to_string(opts)
    |> IO.iodata_to_binary()
  end

end
