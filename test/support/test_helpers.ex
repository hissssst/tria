defmodule Tria.TestHelpers do

  @moduledoc """
  A bunch of helpers for testing
  """

  import Tria.Language, warn: false
  require Tria.Language.Tri, as: Tri

  def last_line({:__block__, _, lines}) do
    last_line List.last lines
  end
  def last_line(other), do: other

  defmacro assert_unique(list) do
    quote bind_quoted: [list: list] do
      len = length list
      assert len == length Enum.uniq list
    end
  end

  defmacro assert_tri(arg, opts \\ [], do: code) do
    pattern =
      code
      |> Tri.do_tri(opts, %{__CALLER__ | context: :match})
      |> prewalk(fn
        #FIXME https://github.com/elixir-lang/elixir/issues/12296
        {:{}, _, [:__block__, _, [line]]} -> line
        {:{}, _, [:__aliases__, _, modules]} -> Module.concat(modules)
        other -> other
      end)

    quote do
      assert unquote(pattern) = unquote(arg)
    end
    |> Macro.prewalk(fn x -> Macro.update_meta(x, & [{:generated, true} | &1]) end)
  end

  defmacro abstract(vars \\ [], do: body) do
    vars = Enum.map(vars, fn {name, meta, _} -> {name, meta, nil} end)
    name = :"Tria.Temp#{:erlang.unique_integer [:positive]}"
    quoted =
      quote do
        defmodule unquote(name) do
          @compile :debug_info
          @compile :nowarn_unused_vars
          def f(unquote_splicing(vars)) do
            unquote(body)
          end
        end
      end
      |> Macro.prewalk(fn ast ->
        Macro.update_meta(ast, &Keyword.put(&1, :generated, true))
      end)

    [{_, binary}] = Code.compile_quoted(quoted, __CALLER__.file)
    {:ok, abstract_code} = Tria.Language.Beam.abstract_code(binary)

    Enum.find_value(abstract_code, fn
      {:function, _, :f, _, [{:clause, _, _, _, body}]} -> Macro.escape body
      _ -> false
    end)
  end
end
