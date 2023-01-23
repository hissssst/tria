# Tria

> WARNING This is a development version
> Compiler is unstable and may break some code

An optimizing compiler for elixir.

## How to use

1. Add a dependency
```elixir
def deps do
  [
    {:tria, github: "hissssst/tria"}
  ]
end
```

2. Add a compiler and remove protocol consolidation
```elixir
def project do
  [
    ...
    consolidate_protocols: false,
    compilers: [:erlang, :tria, :app]
  ]
end
```

3. `mix compile`
There will be a ton of warnings. Just ignore them

## Features

1. Constant evaluation. Plain Elixir and Erlang are unable to evaluate so-called remote call. Tria evalutes what can be evaluated in runtime

2. Enum fusion. Tria joins multiple consequent Enum or Stream calls to most optimal form.

## How it works

Compiler calls elixir's compiler which generates beam,
with all compile-time stuff resolved. Compiler then
exctracts abstract code (debug info) from modules and
translates it to Tria. Tria is then optimized and then
translated back to Elixir which is then compiled to beam
for the last time

## How to debug

```sh
export TRIA_DEBUG=1
export TRIA_TRACE="Module.function/1,Foo.bar/2"
```

This will print all steps of compilation of the `Module.function/arity` function . Each step will be named and will be possible to grep in the exact same format

## Tria language

Tria is a language, it differs from Elixir, but it is mostly Elixir
and it is represented similary to `Macro.t()`.

Tria has no imports, requires, macros, aliases,
aliased modules. Variables can have integer contexts (useful for SSA form).
Tria has no `for` (almost), `cond`, `if`, `.` (in a regular sense).
