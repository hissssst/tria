defmodule Tria.Translator do

  @moduledoc """
  Behaviour for translators
  """

  @typedoc """
  Basically any language in any form
  """
  @type language :: any()

  @typedoc """
  Options for the translator
  """
  @type options :: Keyword.t() | Macro.Env.t()

  @type result(ok) :: {:ok, ok, _state :: any()} | {:ok, ok} | {:error, any()}

  @doc """
  Translates a language and returns a Tria.t() language
  """
  @callback to_tria(language(), options()) :: result(Tria.t())

  @doc """
  Translates a language and returns a Tria.t() language
  """
  @callback to_tria(language()) :: result(Tria.t())

  @doc """
  Translates a language and returns a Tria.t() language
  """
  @callback to_tria!(language(), options()) :: Tria.t()

  @doc """
  Translates a language and returns a Tria.t() language
  """
  @callback to_tria!(language()) :: Tria.t()

  @doc """
  Translates a language and returns a Tria.t() language
  """
  @callback from_tria(Tria.t(), options()) :: result(language())

  @doc """
  Translates a language and returns a Tria.t() language
  """
  @callback from_tria(Tria.t()) :: result(language())

  @doc """
  Translates a language and returns a Tria.t() language
  """
  @callback from_tria!(Tria.t(), options()) :: language()

  @doc """
  Translates a language and returns a Tria.t() language
  """
  @callback from_tria!(Tria.t()) :: language()

  @optional_callbacks [
    to_tria: 1,
    from_tria: 1,
    to_tria!: 1,
    to_tria!: 2,
    from_tria!: 1,
    from_tria!: 2,
  ]

end
