defmodule Tria do

  @moduledoc """
  Optimizing transpiler for Elixir language.
  """

  @typedoc """
  Tria is (mostly) a subset of Elixir language which differs in some ways from Elixir
  Generally, it preserves the structure of Elixir's AST.

  The only incompatible difference here is that Tria supports having integer instead of context
  in the ast for variables. This is very handy for SSA transformations and stuff.
  """
  @type t :: expression
  | variable
  | {t, t}
  | [t]
  | atom
  | number
  | binary

  @type expression :: {expression | atom, meta, [t]}

  @type variable :: {atom, meta, context}

  @type context :: atom | integer

  @type meta :: Keyword.t()

  alias Tria.Compiler.ElixirTranslator

  defmacro __using__(opts) do
    quote do: use Tria.Compiler.Callbacks, unquote(opts)
  end

  # Public interface

  @doc """
  This macro applies optimizer to the passed body.
  Use it with caution, because it can mess up the context
  """
  defmacro tria(opts \\ [], do_body)
  defmacro tria(opts, do: body), do: run(body, opts ++ [env: __CALLER__])
  defmacro tria(opts, body), do: run(body, opts ++ [env: __CALLER__])

  def run(quoted, opts) do
    env = Keyword.fetch!(opts, :env)

    quoted
    |> ElixirTranslator.to_tria!(env)
    |> Tria.Optimizer.run()
    |> ElixirTranslator.from_tria()
  end

end
