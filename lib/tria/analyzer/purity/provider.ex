defmodule Tria.Analyzer.Purity.Provider do

  @moduledoc """
  Sometimes Purity analyzer is unable to find the definition for the function.
  Therefore some external interaction is required to provide information
  about the function. This is the behaviour to do this
  """

  alias Tria.Analyzer.Purity.TTYProvider

  @type opts :: Keyword.t()

  @doc "Returns if the Module.function is pure. Args and opts are passed just for info"
  @callback is_pure({atom(), atom(), list()}, opts) :: true | false

  @spec is_pure({atom(), atom(), list()}, opts) :: true | false
  def is_pure(mfacall, opts), do: default_analyzer().is_pure(mfacall, opts)

  defp default_analyzer() do
    with nil <- :persistent_term.get(__MODULE__, nil), do: TTYProvider
  end

end
