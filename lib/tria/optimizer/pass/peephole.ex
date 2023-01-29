defmodule Tria.Optimizer.Pass.Peephole do

  @moduledoc """
  Simple peephole optimization which hits one-by-one
  """

  use Tria.Optimizer.Pass
  import Tria.Language
  import Tria.Language.Tri
  alias Tria.Language.FunctionRepo
  alias Tria.Debug.Tracer
  alias Tria.Optimizer.Pass

  ## Public

  def run_once(ssa_ast, opts \\ []) do
    ast =
      case ssa_ast do
        # Optimizes `map.field` structure to dispatch to map where possible
        tri([to_tria: :force],
          case input do
           %{key => value} ->
             value

           module when :erlang.andalso(
             :erlang.is_atom(module),
             :erlang.andalso(
               Kernel.!==(module, nil),
               :erlang.andalso(
                 Kernel.!==(module, true),
                 Kernel.!==(module, false)
               )
             )
           ) ->
             tri dot_call(input_or_module, key, [])

           other ->
             :erlang.error({:badkey, key, input_or_other})
          end
        ) = ast when input_or_module in [input, module] and input_or_other in [input, other] ->
          case Keyword.get(opts, :undot, :safe) do
            :force ->
              Tracer.tag_ast(ast, label: :hits_peephole_dot)
              tri to_ssa: true do
                %{key => value} = input
                value
              end

            :safe ->
              case FunctionRepo.exists_any?([:defined, :pure_cache, :pure], function: key, arity: 0) do
                false ->
                  Tracer.tag_ast(ast, label: :hits_peephole_dot)
                  tri to_ssa: true do
                    %{key => value} = input
                    value
                  end

                _ ->
                  ast
              end


            _ ->
              ast
          end

        other ->
          other
      end

    {:ok, ast}
  end

  def run_while(ast, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, Pass.default_timeout())
    timer = Process.send_after(self(), :timeout, timeout)

    try do
      {ast, _} =
        postwalk(ast, :run, fn
          ast, :run ->
            maybe_run_once(ast)

          ast, :stop ->
            { ast, :stop }
        end)

      ast
    after
      Process.cancel_timer(timer)
      receive do
        :timeout -> :ok
        after 0 -> :ok
      end
    end
  end

  defp maybe_run_once(ast) do
    result =
      case run_once(ast) do
        {:ok, new_ast} -> new_ast
        _ -> ast
      end

    receive do
      :timeout -> {result, :stop}
      after 0 -> {result, :run}
    end
  end

end
