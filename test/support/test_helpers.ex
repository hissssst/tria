defmodule Tria.TestHelpers do
  require Tria.Tri

  defmacro assert_tri(arg, opts \\ [], do: code) do
    quote do
      assert Tria.Tri.tri(unquote(opts), unquote(code)) = unquote(arg)
    end
  end

end
