# Tria

An optimizing compiler for elixir

---

## Alpha version roadmap

The purpose of this iteration is to find the silliest design flaws and problems
before publishing this project to open source

### Known problems

[ ] raising lines removed. Needs to be reworked

[FIXED] Protocols are broken, I don't know why

[ ] Guards checking

### Pre public alpha tasks

[x] EnumFusion testing

[x] Tria.Optimizer pipeline testing

[x] Purity check tests

[ ] Deduplication of mfarity

[ ] Raising check implementation

[ ] Raising check tests

[ ] Raising check integration to Evaluation pass

[ ] Tria.Optimizer assurance tests
Tests which just check that original code
returns the same thing as the optimized code

### Alpha milestones

[x] Compiles any project

[x] Passes tests on some external project correctly

[ ] Passes tests on some big external project correctly

[ ] Passes tests on phoenix

---

## How it works

Compiler calls elixir's compiler which generates beam,
with all compile-time stuff resolved. Compiler then
exctracts abstract code (debug info) from modules and
translates it to Tria. Tria is then optimized and then
translated back to Elixir which is then compiled to beam
for the last time

## Tria language

This is a specification for internal IR

No imports, requires, macro, alias and anything like this
And varibles can have integer in context field

[x] Tria -> Elixir
[x] Elixir -> Tria
[x] ATF -> Tria
[ ] Tria -> ATF
[x] Tria->SSA, SSA->Tria

### Compiler

`Mix.Task.Compiler`-compatible compiler.

Basic idea is to compile modules into context modules
This means that modules are divided into groups
and each group is compiled into separate module
while original modules delegate the this separate module

[x] `Tria.tria` and `Tria.run` functions and macro

[x] Task interface

[ ] `use Tria, context: context`
Needs testing on big projects

### Infrastructure

[x] DB

[x] Purity check providers

[x] Raise checking

[ ] Move `Tria.Language.Tri` to separate project

[ ] Tria annotations

[ ] Module generation Infrastructure
I want to create a module and pass functions into it
when I need it

### Purity checking

[x] Purity check by xref

[x] Separate the tables:
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

[x] Joinings

[x] Map.new

[ ] to `:lists`
