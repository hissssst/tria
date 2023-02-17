# Compilation

Tria can be used as a drop-in replacement for Elixir's compiler. Originally, this was the main purpose of Tria project.

## How to use

Add it to compilers section of your mix project in `mix.exs` like this:
```elixir
def project do
  [
    ...
    consolidate_protocols: false,
    compilers: [:erlang, :tria, :app]
  ]
end
```

And it should work out of the box. You may want to warm up the caches to increase compilation speed. See the caches doc for this.

## How it works

> This section is about internals and you may want to skip it, unless you're interested in changing the compiler's behaviour

TODO
