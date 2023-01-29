defmodule Tria.Compiler.Callbacks do

  @moduledoc """
  Callbacks to be inserted in module when compiling a whole module using Tria
  """

  alias Tria.Compiler.ContextServer
  alias Tria.Compiler.Annotations
  import Tria.Compiler.ElixirTranslator, only: [unalias: 1]
  import Module, only: [register_attribute: 3, delete_attribute: 2, put_attribute: 3]

  defmacro __using__(opts) do
    %Macro.Env{module: module} = __CALLER__
    context = unalias Keyword.get(opts, :context, TriaGlobalContext)
    opts = Keyword.put(opts, :context, context)
    ContextServer.start(context)

    register_attribute(module, :tria, accumulate: true)
    register_attribute(module, :tria_acc, accumulate: true, persist: true)
    register_attribute(module, :tria_opts, persist: true)

    quote do
      @on_definition unquote(__MODULE__)
      @tria_opts unquote(opts)
    end
  end

  def __on_definition__(%Macro.Env{module: module, file: file, line: line}, kind, name, args, _, _) do
    with [[_ | _] = opts] <- delete_attribute(module, :tria) do
      unless Annotations.valid_annotations?(opts) do
        raise CompileError,
          description: "Tria annotations #{inspect opts} are invalid",
          file: file,
          line: line
      end

      put_attribute(module, :tria_acc, [{kind, name, length(args), opts}])
    end
  end

end
