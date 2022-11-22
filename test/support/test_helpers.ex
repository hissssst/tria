defmodule Tria.TestHelpers do
  require Tria.Tri

  defmacro assert_tri(arg, opts \\ [], do: code) do
    quote do
      assert Tria.Tri.tri(unquote(opts), unquote(code)) = unquote(arg)
    end
  end

  defmacro abstract(do: body) do
    name = :"Tria.Temp#{:erlang.unique_integer [:positive]}"
    quoted =
      quote do
        defmodule unquote(name) do
          def f do
            unquote(body)
          end
        end
      end

    [{_, binary}] = Code.compile_quoted(quoted, __CALLER__.file)
    {:ok, abstract_code} = Tria.Codebase.fetch_abstract_code(binary)

    Enum.find_value(abstract_code, fn
      {:function, _, :f, _, [{:clause, _, _, _, body}]} -> Macro.escape body
      _ -> false
    end)
  end
end
