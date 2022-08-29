defmodule Tria.Pass.EnumFusion do
  import Tria.Common

  def run_once(ast, _opts \\ []) do
    do_run(ast)
  end

  defp do_run(dot_call(Enum, :map, [dot_call(Enum, :map, [enum, mapper1]), mapper2])) do
    arg = Macro.var(:arg, nil)

    new_mapper = quote do
      fn unquote(arg) ->
        (unquote(mapper2)).((unquote(mapper1)).(unquote(arg)))
      end
    end

    dot_call(Enum, :map, [enum, new_mapper])
  end

  defp do_run(ast), do: ast
end
