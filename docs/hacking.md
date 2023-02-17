# Hacking

So you want to change someting in Tria...

## Architecture

To find where to implement the change you want, you need to know what parts of system are there and what can be changed

### Workflow overview

1. Compiler calls elixir's compiler which generates beam with all compile-time macro and dependencies resolved.
   Elixir's compiler first compiles all modules, and only then the Tria Compiler proceeds to the next step.
   Modules for compilation are defined in a compiler manifest which is a special file for state which is
   shared among several compilation steps

2. Tria compiler then exctracts abstract code from debug_info beam chunk

3. Translates abstract code to Tria.

4. Emits definition to ContextServer which is a GenServer for a context. And a context is bunch of modules
   which are allowed to have compile-time dependencies on each other.

5. When the whole module is translated, the delegating modules is generated.
   Delegating module is a module, which redirects each function to the context module of a context this module belongs to.

5. When all abstract code is translated and all functions are emitted to the context server,
   compiler calls generation of a context module.

6. During generation of a context module, each function is optimized with an optimizing pass.
   Some passes may loop, so they can be interrupted with timeout mechanism

7. When all functions are optimized, ContextServer starts generation of a context module which is a module
   of all optimized functions of all modules in the context

8. This context module is persisted to disk

9. When all context modules are generated, manifest file with dependencies is generated and persisted to disk

That's it.

### Architecture overview

TODO
