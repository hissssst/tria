# Intermediate representation

Intermediate representation of Tria compiler is actually called Tria.
It differs in a few places and it is designed to reflect Elixir's AST as close as possible,
while removing syntax sugar and being friendly for code generation

## Purpose

1. Be friendly for SSA form.
2. Have a left side of `->`, `<-` and `=` to be a pattern, always.
3. Have no macro or local functions, expand any syntax sugar like `with`, `cond`, etc.
4. No special forms

## Specification

There is no formal specification of Tria AST but it is best described in how it differs from Elixir's AST

* Variables are `{name :: atom(), meta :: meta(), context :: atom() | non_neg_integer()}`. The context can be an integer or an atom.
* No structures like `cond`
* `after` clause in `receive` is `{timeout, body}` instead of `{:"->", meta, [timeout, body]}`
* No macros and `quote`-s
* No `&` captures
* No local calls
* No `rescue` in `try`. Only `catch` with `Exception.normalize`
* No `cond`, no `if`, no `|>`, no `with`
* No `for` (except binary for comprehension)

## Implementation

The `ElixirTranslator` module contains the logic which translates Elixir's AST to Tria AST.
Since things like `__MODULE__`, aliases, imports and local functions are expanded during translation, it requires `Macro.Env.t()` to be known.

AST manipulation functions are implemented as a part of `Tria.Language` family of modules

## Single static assignment form

Tria and Tria SSA have no structural differencies. SSA form of Tria guarantees only that each variable is defined not more than once
