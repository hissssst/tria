defmodule Tria do
  alias Tria.Translator.Elixir, as: ElixirTranslator
  import Tria.Common

  defmacro look(do: code) do
    __CALLER__
    |> Map.from_struct()
    |> IO.inspect(label: :caller)

    translated =
      code
      |> Macro.prewalk(&purge_meta/1)
      |> ElixirTranslator.to_tria(__CALLER__)

    translated
    |> IO.inspect(label: :ast, pretty: true)

    translated
    |> inspect_ast(label: :code)

    code
  end

  @type variable :: {atom(), list(), atom()}

  @type special_form :: {}

  @type call :: {atom(), list(), atom()}
end
