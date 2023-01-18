defmodule Tria.Debug.Breakpoint do

  @moduledoc """
  Black magic of debugging
  """

  import Tria.Language

  defmacro breakpoint(opts) do
    quote do: {:tria_breakpoint, unquote(opts)}
  end

  defmacro handle_breakpoint(point) do
    vars = vars __CALLER__
    quote do
      stacktrace = try(do: raise("error"), rescue: (_ -> __STACKTRACE__))
      unquote(__MODULE__).do_handle_breakpoint(unquote(point), __ENV__, stacktrace, unquote(vars))
      nil #AS A NOOP
    end
  end

  def do_handle_breakpoint(_opts, _env, stacktrace, variables) do
    formatted_vars =
      variables
      |> Enum.map(fn {name, value} ->
        "#{name} = #{do_inspect(value)}"
      end)
      |> Enum.join("\n")
      |> format()

    message = """

    Breakpoint
      Stacktrace:
        #{format Exception.format_stacktrace(stacktrace)}
      Context:
        #{formatted_vars}

    """

    IO.puts message
  end

  # Helpers

  defp do_inspect(something, replace_with \\ "  ") do
    something
    |> inspect(limit: :infinity, pretty: true, printable_limit: :infinity, structs: false)
    |> format(replace_with)
  end

  defp format(s, replace_with \\ "  ") do
    replace_with = "\n" <> replace_with
    String.replace(s, "\n", replace_with)
  end

  defp vars(%Macro.Env{versioned_vars: versioned_vars}) do
    versioned_vars
    |> Enum.map(fn
      {{name, {context, counter}}, _} ->
        {name, [counter: counter], context}

      {{name, counter}, _} when is_integer(counter) ->
        {name, [counter: counter], nil}

      {{name, context}, _} when is_atom(context) ->
        {name, [], context}
    end)
    |> Enum.map(fn variable ->
      {ast_to_string(variable, with_contexts: true), variable}
    end)
  end

end
