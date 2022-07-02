defmodule Tria.Comparator do

  def size({op, _meta, args}), do: size(op) + size(args)
  def size([lh | lt]), do: size(lh) + size(lt)
  def size({left, right}), do: size(left) + size(right)
  def size(_other), do: 1
  
  def [lh | lt] <~> [rh | rt] do
    lh <~> rh and lt <~> rt
  end

  def {l1, r1} <~> {l2, r2} do
    l1 <~> l2 and r1 <~> r2
  end

  def {op1, _, args1} <~> {op2, _, args2} do
    op1 <~> op2 and args1 <~> args2
  end

  def left <~> right do
    left === right
  end
end
