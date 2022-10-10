defmodule Tria.Compiler do

  import Tria.Common

  @doc """
  Compiles arbitary Elixir AST into a bunch of modules
  """
  def compile(ast, metadata) do
    Macro.prewalk(ast, fn
      {kind, meta, opts} when kind in ~w[defmodule defimpl]a ->
        opts = List.update_at(opts, -1, &add_use/1)
        {kind, meta, opts}
        
      other ->
        other
    end)
    |> inspect_ast(label: :well)
    |> Code.compile_quoted(metadata.file)
  end

  #FIXME fails when `quote do: defmodule`
  defp add_use(body) do
    Keyword.update!(body, :do, fn 
      {:__block__, meta, block} ->
        {:__block__, meta, [{:use, [], [Tria]} | block]}

      line ->
        {:__block__, [], [{:use, [], [Tria]}, line]}
    end)
  end

end
