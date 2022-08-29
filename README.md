# Tria

Tria is a subset of `Elixir` language designed specially for easy AST manipulations

## Specs

### Tria

Tria anguage differs from Elixir:
* No aliases, no imports
* Every call is a `Mod.function`
* No `&` captures, only `fn`-s
* Only mfa
* No macros, (even `|>`)
* With `with`
* No `if_undefined: :apply`
* No counters for variables context

---

### TODO

[ ] Infrastructure to enable inter-module inlinings

[x] Generate one module from multiple modules

[T] `Enum` optimizers
    Needs testing and purity checking

[ ] Purity checking
    Kinda
    Needs more proper checking

[T] Erlang abstract format to Tria translator
    It works but needs testing

[T] Fetch local calls in abstract->elixir->tria translator
    Needs testing

[x] Get over FUD with `Tria.Matcher`

[x] ex->tria mfa (it's fine to handle this by hand)

[x] ex->tria macro_expansion

[ ] ex->tria `&` capture
    is tricky because of `&function/2` and `&Module.function/2`

---

### Optimization passes ideas

[x] Enum.map joining

[x] Enum.map unrolling

[ ] Moving Enum sequence to generated `defp`

[ ] Fn inlining (almost complete, requires matcher to be finished)

[ ] Map.get and family

[ ] zero-arity pure functions pre-evaluation

### Other ideas

[ ] Investigate NimbleParsec, perhaps this can be optimized with Tria
