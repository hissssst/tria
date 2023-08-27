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

3. `mix compile && mix tria.warmup --available`
This compiles the deps, and warms up the cache of used functions

4. Project is ready to use now

## Features

1. Constant evaluation. Plain Elixir and Erlang are unable to evaluate so-called remote call. Tria evalutes what can be evaluated in runtime

2. Enum fusion. Tria joins multiple consequent Enum or Stream calls to most optimal form.

3. `map.field` optimization. This construction is now 3 times faster

## Reporting bugs

Just use `mix tria.report "INSERT BUG TITLE HERE"` and it will automatically open tracker with information about system and env prefilled

## How to debug

```sh
export TRIA_DEBUG=1
export TRIA_TRACE="Module.function/1,Foo.bar/2"
```

This will print all steps of compilation of the `Module.function/arity` function . Each step will be named and will be possible to grep in the exact same format.
Plus, `TRIA_DEBUG` increases verbosity of compiler and generates `tria_global_context.ex` file which contains the final version of the code after all optimizations.

## Tria language

Tria is a language, it differs from Elixir, but it is mostly Elixir
and it is represented similary to `Macro.t()`. Tria is designed specifically
for optimizing transformations, like any IR language and has a specific
Single Static Assignment form translator.
