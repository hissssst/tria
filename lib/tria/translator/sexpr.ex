defmodule Tria.Translator.Sexpr do
  def translate_from(ast) do
    ast
    |> do_translate_from()
    |> IO.iodata_to_binary()
  end

  def do_translate_from({call, _meta, args}) do
    args =
      args
      |> Enum.map(&do_translate_from(&1))
      |> Enum.intersperse(" ")

    ["(", to_string(call), " ", args, ")"]
  end

  def do_translate_from(list) when is_list(list) do
    inner =
      list
      |> Enum.map(&do_translate_from(&1))
      |> Enum.intersperse(" ")

    ["[", inner, "]"]
  end

  def do_translate_from(other), do: inspect(other)
end
