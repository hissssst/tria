defmodule Tria.CodeCase do

  import Tria

  defp to_pattern(quoted) do
    quoted
    |> Macro.escape()
    |> Macro.prewalk(fn
      {:{}, _, [n, m, c]} when is_var({n, m, c}) ->
        {n, m, c}
      other ->
        other
    end)
  end

  defp drop_meta_in_escaped(escaped) when is_list(escaped) do
    Enum.map(escaped, &drop_meta_in_escaped/1)
  end
  defp drop_meta_in_escaped({:{}, _, [n, _, a]}) do
    {:{}, [], [drop_meta_in_escaped(n), {:_, [], Elixir}, drop_meta_in_escaped(a)]}
  end
  defp drop_meta_in_escaped(other), do: other

  defp drop_meta(quoted) do
    Macro.prewalk(quoted, fn x ->
      Macro.update_meta(x, fn _ -> [] end)
    end)
  end

  defmacro code_case(input, do: cases) do
    cases = Enum.map(cases, fn {:"->", meta, [l, r]} ->
      l =
        l
        |> to_pattern()
        |> drop_meta_in_escaped()

      r =
        r
        |> drop_meta()
        |> to_pattern()

      {:"->", meta, [l, r]}
    end)
    quote do
      case(unquote(input), do: [unquote_splicing(cases)])
    end
  end

end
