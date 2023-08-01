# TODO

> This is an unordered dump with features to be implemented

[?] `no_optimize` macro which ensures that optimizations are not called
    Kinda done because it can be specified in `@tria` annotations

[ ] Incremental compilation
    [x] Tracing deps during optimizations
    [x] Manifest with hashes of functions
    [ ] Testing

[ ] Fix cte bug when function is not found in runtime

[ ] Proper `Compiler.fname`

[ ] Delegated functions rewrite stacktrace

[ ] Multiple contexts

[ ] Parallel compilation

[ ] A-Normal form (or any other normal form suitable for more useful evaluation)

[ ] `warmup` options like `--only`

[ ] Cache invalidation (haha, must be a __simple task__)

[ ] Purity depending on argument purity
For example `Enum.map` is pure
if first argument is a list, and a second is a pure function

[ ] Peephole
  [ ] `Map.get(something, something, default)`
  [ ] `try(do: pure)`
  [ ] `if left === right do`

[ ] Inlining
  [ ] Size measurement
  [ ] Call graph analyzing algorithm

[ ] Bubble
Calls reordering for more predictive optimizations

[ ] Case lifting
Joins nested cases into one big case

[ ] Determine order of optimizers

[ ] Create an ability to plan optimizers, so the compilation won't take too much time

[ ] EnumFusion to :lists

[ ] EnumFusion loop unrolling

[ ] check

```elixir
Enum.each(list, fn item ->
  f(item, %{key: value})
end)
# vs
map = %{key: value}
Enum.each(list, fn item ->
  f(item, map)
end)
```

[ ] `optional_callbacks`

[x] Module attributes are stored correctly

[x] Tria annotations. Like
```elixir
@tria pure: true, safe: true
def pure_inspect(x) do
  IO.inspect(x)
end
```

[x] map.field
Optimize this to unpack map in one place.
Actually can be done with checking if there is a zero-arity function with such name in any module
