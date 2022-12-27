defmodule Tria.Language.Analyzer.Purity.Provider do

  @moduledoc """
  Sometimes Purity analyzer is unable to find the definition for the function.
  Therefore some external interaction is required to provide information
  about the function. This is the behaviour to do this
  """

  alias Tria.Language.Analyzer.Purity.TTYProvider
  alias Tria.Language.MFArity

  @type opts :: Keyword.t()

  @doc "Returns if the Module.function is pure. Args and opts are passed just for info"
  @callback is_pure(MFArity.mfargs(), opts) :: true | false

  @spec is_pure(MFArity.mfargs(), opts) :: true | false
  def is_pure(mfargs, opts), do: default_analyzer().is_pure(mfargs, opts)

  defp default_analyzer() do
    with nil <- :persistent_term.get(__MODULE__, nil), do: TTYProvider
  end

end
