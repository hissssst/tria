# Tria

Tria is a subset of `Elixir` language designed specially for easy AST manipulations

## Goals

The things `Tria` project tries to acheive:
1. Optimizations of `Elixir`'s `AST`
2. Compatible with new and current `Elixir` features, no new features and no limitations
3. Optimized code size is comparable to original code size

### Roadmap

1. Conversion of elixir to tria and back
   Just `AST` conversion

2. Multiple optimization passes
	 and pluggable to compilation pipeline

> All passes before step 3 are very simple and exist just
> to make sure that changes are applied

3. Transducers

4. Constant folding

## Iteration 1

1. ex->tria
2. tria code pattern matching
3. some very simple optimization
4. tria->ex

Ready!

## Specs

### Tria

Tria anguage differs from Elixir:
* No aliases, no imports
* Every call is a `Mod.function`
* No `&` captures, only `fn`-s
* Only mfa
* No macros, (even `|>`)
* With `with`

---

### TODO

[ ] `Enum` optimizers

[x] Get over FUD with `Tria.Matcher`

[x] ex->tria mfa (it's fine to handle this by hand)

[x] ex->tria macro_expansion

[ ] ex->tria `&` capture
    is tricky because of `&function/2` and `&Module.function/2`

[ ] Investigate NimbleParsec, perhaps this can be optimized with Tria

---

### Optimization passes ideas

[x] Enum.map joining

[ ] Enum.map unrolling

[ ] Moving Enum sequence to generated `defp`

[ ] Fn inlining (almost complete, requires matcher to be finished)

[ ] Map.get and family
