defmodule Tria.Comparator do

  import Tria.Common

  defmacrop left <~ right do
    quote do
      with(
        {true, leftctx} <- unquote(left),
        {true, rightctx} <- unquote(right)
      ) do
        Enum.reduce_while(leftctx, {true, rightctx}, fn {first, second}, {true, acc} ->
          case put(acc, first, second) do
            {:ok, acc} ->
              {:cont, {true, acc}}

            :error ->
              {:halt, false}
          end
        end)
      end
    end
  end

  def compare(left, right) do
    with {true, _} <- compare(left, right, %{}) do
      true
    end
  end
  def compare([lh | lt], [rh | rt], ctxmap) do
    compare(lh, rh, ctxmap) <~ compare(lt, rt, ctxmap)
  end
  def compare({l1, l2}, {r1, r2}, ctxmap) do
    compare(l1, r1, ctxmap) <~ compare(l2, r2, ctxmap)
  end
  def compare({name, _, leftctx} = vl, {name, _, rightctx} = vr, ctxmap) when is_variable(vl) and is_variable(vr) do
    case put(ctxmap, leftctx, rightctx) do
      {:ok, ctxmap} ->
        {true, ctxmap}

      _ ->
        false
    end
  end
  def compare({lop, _, largs}, {rop, _, rargs}, ctxmap) do
    compare(lop, rop, ctxmap) <~ compare(largs, rargs, ctxmap)
  end
  def compare(same, same, ctxmap), do: {true, ctxmap}
  def compare(_, _, _), do: false

  defp put(ctxmap, context, context), do: {:ok, ctxmap}
  defp put(ctxmap, context1, context2) when context1 < context2 do
    case ctxmap do
      %{^context1 => ^context2} ->
        {:ok, ctxmap}

      %{^context1 => _} ->
        :error

      %{^context2 => _} ->
        :error

      ctxmap ->
        {:ok, Map.put(ctxmap, context1, context2)}
    end
  end
  defp put(ctxmap, context1, context2), do: put(ctxmap, context2, context1)
  
end
