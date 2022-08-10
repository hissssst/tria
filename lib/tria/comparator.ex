defmodule Tria.Comparator do
  
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
