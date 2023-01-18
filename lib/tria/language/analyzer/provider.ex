defmodule Tria.Language.Analyzer.Provider do

  @moduledoc """
  Sometimes Purity analyzer is unable to find the definition for the function.
  Therefore some external interaction is required to provide information
  about the function. This is the behaviour to do this
  """

  alias Tria.Language.Analyzer.TTYProvider
  alias Tria.Language.MFArity

  @type opts :: Keyword.t()

  @doc """
  Returns the trait value for Module.function/arity when Analyzer can't make the correct decidion.
  Args and opts are passed just for info
  """
  @callback decide(atom(), MFArity.mfargs(), opts) :: any()

  @spec decide(atom(), MFArity.mfargs(), opts) :: any()
  def decide(trait, mfargs, opts), do: provider_for(trait).(trait, mfargs, opts)

  defp provider_for(_trait) do
    case :persistent_term.get(__MODULE__, nil) do
      nil ->
        fn trait, mfargs, opts -> TTYProvider.decide(trait, mfargs, opts) end

      module when is_atom(module) ->
        fn trait, mfargs, opts -> module.decide(trait, mfargs, opts) end

      function when is_function(function, 3) ->
        function
    end
  end

end
