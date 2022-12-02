defmodule Tria.Compiler.Callbacks do

  @moduledoc """
  Callbacks to be inserted in module when compiling a whole module using Tria
  """

  alias Tria.Compiler.ContextServer
  import Tria.Compiler.ElixirTranslator, only: [unalias: 1]

  defmacro __using__(opts) do
    context = unalias Keyword.get(opts, :context, TriaGlobalContext)
    opts = Keyword.put(opts, :context, context)
    ContextServer.start(context)

    Module.register_attribute(__CALLER__.module, :tria, accumulate: true, persist: true)
    Module.register_attribute(__CALLER__.module, :tria_opts, persist: true)

    quote do
      @after_compile unquote(__MODULE__)
      @tria_opts unquote(opts)
    end
  end

  def __after_compile__(_env, _bytecode) do
    #TODO
    :ok
  end

end
