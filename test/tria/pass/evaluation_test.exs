defmodule Tria.Pass.EvaluationTest do
  use ExUnit.Case

  import Tria.Tri
  import Tria.Common
  alias Tria.Pass.Evaluation

  @compile :nowarn_unused_vars
  @compile {:nowarn_unused_vars, true}

  describe "simple arithmetics" do
    test "constant" do
      evaluated =
        tri do
          x = 1 + 2
          y = x + x
          y + 5
        end
        |> Evaluation.run_once!()

      assert 11 == evaluated
    end

    test "x + y" do
      evaluated =
        tri do
          a = 1 + 2
          b = 2 + 2
          a + b + x + y
        end
        |> Evaluation.run_once!()
        |> inspect_ast()

      assert tri(Kernel.+(Kernel.+(7, x), y)) = evaluated
    end
  end

  describe "matchers" do
    test "yes case by value" do
      evaluated =
        tri do
          x = 1
          case x do
            3 -> 3 + x
            2 -> 2 + x
            1 -> 1 + x
            other -> other
          end
        end
        |> Evaluation.run_once!()

      assert 2 == evaluated
    end

    test "yes case by pattern" do
      evaluated =
        tri do
          case {x, y} do
            {one, two, three} -> one + two + three
            {left, right} -> left + right
            {one} -> one
            other -> 0
          end
        end
        |> Evaluation.run_once!()

      assert tri(Kernel.+(_x, _y)) = evaluated
    end

    test "maybe case" do
      evaluated =
        tri do
          case [1, 2 | tail] do
            [] -> 0
            [_] -> 1
            [2, 2] -> :twos
            [_, _] -> 2
            other -> length(other)
          end
        end
        |> Evaluation.run_once!()

      assert(tri do
        case [1, 2 | _tail] do
          [_, _] -> 2
          other -> Kernel.length(other)
        end
      end = evaluated)
    end

    test "pathex-style force_update" do
      evaluated =
        tri do
          case something do
            %{1 => x} -> x
            [_, y | _] -> y
            %{} -> :map
            l when is_list(l) -> :list
            _ -> :error
          end
        end
        |> Evaluation.run_once!()

      assert(tri do
        case something do
          %{1 => x} -> x
          [_, y | _] -> y
          %{} -> :map
          l when is_list(l) -> :list
          _ -> :error
        end
      end = evaluated)
    end

    test "same variable twice" do
      evaluated =
        tri do
          case x do
            [head, head] when head > 10 -> head
          end
        end
        |> Evaluation.run_once!()

      assert(tri do
        case x do
          [head, head] when head > 10 -> head
        end
      end = evaluated)
    end

    test "outer context in guard" do
      evaluated =
        tri do
          head = something
          case x do
            1 when head > 10 -> :ok
          end
        end
        |> Evaluation.run_once!()

      assert(tri do
        case x do
          1 when something > 10 -> :ok
        end
      end = evaluated)
    end

    test "outer context in deeper in guard" do
      evaluated =
        tri do
          case arg do
            x when true and y > 10 and x > 10 -> :ok
          end
        end
        |> inspect_ast(with_contexts: true, label: :was)
        |> Evaluation.run_once!()
        |> inspect_ast(with_contexts: true, label: :became)

      # Why no tri, because `assert` is fucking shit
      assert {
        :case, _, [arg, [do: [
          {:"->", _, [
            [{:when, _, [x,
              {andd, _, [
                {andd, _, [
                  true,
                  {more, _, [y, 10]}
                ]},
                {more, _, [x, 10]}
              ]}
            ]}],
            :ok
          ]}
        ]]]
      } = evaluated
    end

    test "either case" do
      evaluated =
        tri do
          case {:ok, 4} do
            {:ok, value} -> value
            :error -> raise "error"
          end
        end
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()

      assert 4 == evaluated
    end
  end

  describe "Complex" do
    test "GenServer" do
      evaluated =
        tri do
          opts = [name: __MODULE__, restart: :always]
          opts = Keyword.put(opts, :restart, :temporary)
          opts = Keyword.put(opts, :option, :value)
          GenServer.start_link(__MODULE__, [], opts)
        end
        |> Evaluation.run_once!()

      assert(tri GenServer.start_link(__MODULE__, [], option: :value, name: __MODULE__, restart: :always) = evaluated)
    end

    test "Pathex-style fn inlining" do
      evaluated =
        tri do
          (fn x, function ->
            try do
              {:ok,
               case x do
                 %{0 => value} = map ->
                   case function.(value) do
                     {:ok, new_value} -> %{map | 0 => new_value}
                     :delete_me -> Map.delete(map, 0)
                     :error -> Kernel.throw(:path_not_found)
                   end

                 [x | _] = list ->
                   case function.(:lists.nth(1, list)) do
                     {:ok, new_value} -> List.replace_at(list, 0, new_value)
                     :delete_me -> List.delete_at(list, 0)
                     :error -> Kernel.throw(:path_not_found)
                   end

                 tuple when :erlang.andalso(is_tuple(tuple), tuple_size(tuple) > 0) ->
                   case function.(:erlang.element(1, tuple)) do
                     {:ok, new_value} -> :erlang.setelement(1, tuple, new_value)
                     :delete_me -> :erlang.delete_element(1, tuple)
                     :error -> Kernel.throw(:path_not_found)
                   end

                 _ ->
                   Kernel.throw(:path_not_found)
               end}
            catch
              :path_not_found -> :error
            end
          end).({1, 2}, fn _ -> :delete_me end)
        end
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()

      assert(tri do
        {:ok, {2}}
      end = evaluated)
    end

    test "Pathex-style fn inlining 2" do
      evaluated =
        tri do
          case [1] do
           %{0 => value} = map ->
             case function.(value) do
               {:ok, new_value} -> %{map | 0 => new_value}
               :delete_me -> Map.delete(map, 0)
               :error -> Kernel.throw(:path_not_found)
             end

           [x | _] = list ->
             case function.(:lists.nth(1, list)) do
               {:ok, new_value} -> List.replace_at(list, 0, new_value)
               :delete_me -> List.delete_at(list, 0)
               :error -> Kernel.throw(:path_not_found)
             end

           tuple when :erlang.andalso(is_tuple(tuple), tuple_size(tuple) > 0) ->
             case function.(:erlang.element(1, tuple)) do
               {:ok, new_value} -> :erlang.setelement(1, tuple, new_value)
               :delete_me -> :erlang.delete_element(1, tuple)
               :error -> Kernel.throw(:path_not_found)
             end

           _ ->
             Kernel.throw(:path_not_found)
          end
        end
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()

      assert(tri do
        case function.(1) do
          {:ok, new_value} -> List.replace_at([1], 0, new_value)
          :delete_me -> []
          :error -> Kernel.throw(:path_not_found)
        end
      end = evaluated)
    end

    test "Pathex-style fn inlining 3" do
      evaluated =
        tri do
          case {} do
            %{0 => value} = map ->
              case function.(value) do
                {:ok, new_value} -> %{map | 0 => new_value}
                :delete_me -> Map.delete(map, 0)
                :error -> Kernel.throw(:path_not_found)
              end

            [x | _] = list ->
              case function.(:lists.nth(1, list)) do
                {:ok, new_value} -> List.replace_at(list, 0, new_value)
                :delete_me -> List.delete_at(list, 0)
                :error -> Kernel.throw(:path_not_found)
              end

            _ ->
               Kernel.throw(:path_not_found)
          end
        end
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()

      assert tri(Kernel.throw(:path_not_found)) = evaluated
    end

    test "fn in variable" do
      evaluated =
        tri do
          func = fn x -> x + 1 end
          func.(func.(10))
        end
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()

      assert 12 = evaluated
    end

    test "Pathex-style fn in fn" do
      evaluated =
        tri do
          variable_0 = fn
           :update, {x, function} ->
             case x do
               %{"x" => x} = x_1410 ->
                 with {:ok, y} <- function.(x) do
                   {:ok, %{x_1410 | "x" => y}}
                 else
                   []
                 end

               _ ->
                 :error
             end

           :view, {x, function} ->
             case x do
               %{"x" => x} -> function.(x)
               _ -> :error
             end
           end

           variable_1 = fn
             :update, {x, function} ->
               case x do
                 %{"x" => x} = x_1474 ->
                   with {:ok, y} <- function.(x) do
                     {:ok, %{x_1474 | "x" => y}}
                   else
                     []
                   end

                 _ ->
                   :error
               end

             :view, {x, function} ->
               case x do
                 %{"x" => x} -> function.(x)
                 _ -> :error
               end
           end

           fn
             :view, {x, func} ->
               variable_0.(:view, {x, fn x -> variable_1.(:view, {x, func}) end})

             :update, {x, func} ->
               variable_0.(:update, {x, fn x -> variable_1.(:update, {x, func}) end})
           end
        end
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()
        |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        # |> Evaluation.run_once!()
        |> inspect_ast(label: :here)
    end
  end

end
