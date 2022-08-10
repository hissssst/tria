defmodule Tria.Translator do

  @callback to_tria(any(), any()) :: Tria.t()

  @callback from_tria(Tria.t()) :: any()

end
