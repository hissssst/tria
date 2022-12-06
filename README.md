# Tria

!!! Think about replacing `defmodule` and `def` to my own macro
It is easy to replace them
Hard part is to be compatible with compilation hooks, attributes and stuff

An optimizing compiler for elixir

## Known problems
Phoenix router compilation fails because of improper list somewhere

## Overview
TODO

## Milestones

[x] Compiles my project

[x] Passes tests on some external project correctly

[x] Passes tests on some big external project correctly

[x] Passes tests on phoenix

# Roadmap

Project consists of several parts

### Tria language

A special language for manipulating Elixir's AST

No imports, requires, macro
And varibles can have integer in context field

[x] Tria -> Elixir
[x] Elixir -> Tria
[x] ATF -> Tria. Kinda, needs testing
[ ] Tria -> ATF (actually don't need this, but it can be useful for erlangers)

[x] Tria->SSA, SSA->Tria

### Compiler

`Mix.Task.Compiler`-compatible compiler.
And ability to inline optimizers

Basic idea is to compile modules into context modules
This means that modules are divided into groups
and each group is compiled into separate module
while original modules delegate the this separate module

[x] `Tria.tria` and `Tria.run` functions and macro

[x] Task interface
Just prepends `use Tria` to every module

[ ] `use Tria, context: context`
Needs testing on big projects

### Infrastructure

[x] DB

[x] Purity check providers

[ ] Tria annotations

[ ] Module generation Infrastructure
I want to create a module and pass functions into it
when I need it

[ ] Callgraph

### Purity checking

[x] Purity check by xref

[ ] Separate the tables:
`pure_cache` local cache during compilation
`pure_roots` nifs and stuff from internal libraries

[ ] Purity for `pure_cache` invalidation

[ ] Purity depending on argument purity
For example `Enum.map` is pure
if first argument is a list, and a second is a pure function


### Optimization pipeline

Transformations which take an AST and produce an AST.
Can be called one after another.

[ ] Determine order of optimizers

[ ] Create an ability to plan optimizers,
so the compilation won't take too much time

---

[x] Evaluation
of everything what can be evaluated in compile time
  [x] evaluation hit
  [x] pure functions pre-evaluation

[x] Peephole
Kinda. Needs testing
  [x] `Map.get(something, something, default)`
  [x] `try(do: pure)`

[ ] Inlining
  [ ] Size measurement
  [ ] Call graph analyzing algorithm

[ ] Bubble
Moves the call up in the stacktrace

[ ] Case lifting
Joins nested cases into one big case

[ ] map.field
Optimize this to unpack map in one place

### Enum optimizers

[ ] Join maps
If they're pure inside

[ ] Moving Enum sequence to generated `defp`
