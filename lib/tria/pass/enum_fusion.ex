defmodule Tria.Pass.EnumFusion do
  import Tria.Common

  def run(ast, _opts \\ []) do
    Macro.postwalk(ast, &go/1)
  end

  defp go(dot_call(Enum, :map, [dot_call(Enum, :map, [enum, mapper1]), mapper2])) when not is_dot_call(enum) do
    arg = Macro.var(:arg, nil)

    new_mapper = quote do
      fn unquote(arg) ->
        (unquote(mapper2)).((unquote(mapper1)).(unquote(arg)))
      end
    end

    dot_call(Enum, :map, [enum, new_mapper])
  end

  defp go(dot_call(Enum, :map, [dot_call(Enum, :map, _) = enum, mapper])) do
    dot_call(Enum, :map, [go(enum), mapper])
    |> go()
  end

  defp go(ast), do: ast
end
