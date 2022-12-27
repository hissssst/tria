defmodule Tria.Debug.IEx do

  import Tria.Language
  alias Tria.Compiler.ElixirTranslator
  alias Tria.Compiler.AbstractTranslator

  defmacro compiled(do: body) do
    tria_body = ElixirTranslator.to_tria!(body, __ENV__)
    undefined = []

    name = :"Tria.Temp#{:erlang.unique_integer [:positive]}"
    quoted =
      quote do
        defmodule unquote(name) do
          def f(unquote_splicing undefined) do
            unquote(body)
          end
        end
      end
      |> Macro.prewalk(fn ast ->
        Macro.update_meta(ast, &Keyword.put(&1, :generated, true))
      end)

    [{_, binary}] = Code.compile_quoted(quoted, __CALLER__.file)
    {:ok, abstract_code} = Tria.Language.Codebase.fetch_abstract_code(binary)

    abstract_code
    |> Enum.find_value(fn
      {:function, _, :f, _, [{:clause, _, _, _, body}]} -> body
      _ -> false
    end)
    |> AbstractTranslator.to_tria!(as_block: true, env: __ENV__)
    |> inspect_ast()
    |> Macro.escape()
  end

end
