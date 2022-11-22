defmodule Tria.Pass.EvaluationTest do
  use ExUnit.Case

  import Tria.Tri
  import Tria.Common, only: [inspect_ast: 2], warn: false
  alias Tria.Pass.Evaluation
  alias Tria.Translator.SSA

  import Tria.TestHelpers

  @tri_opts meta: false

  # Does this even work in elixir?
  @compile :nowarn_unused_vars
  @compile {:nowarn_unused_vars, true}

  defp run_while(ast) do
    ast
    |> SSA.from_tria()
    |> do_run_while()
  end

  defp do_run_while(ast) do
    case Evaluation.run_once(ast) do
      {:ok, new_ast} ->
        run_while(new_ast)

      {:error, :nothing_to_evaluate} ->
        ast
    end
  end

  describe "simple arithmetics" do
    test "constant" do
      evaluated =
        tri do
          x = 1 + 2
          y = x + x
          y + 5
        end
        |> run_while()

      assert 11 == evaluated
    end

    test "x + y" do
      evaluated =
        tri do
          a = 1 + 2
          b = 2 + 2
          a + b + x + y
        end
        |> run_while()

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
        |> run_while

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
        |> run_while()

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
        |> run_while()

      assert_tri evaluated do
        case [1, 2 | _tail] do
          [_, _] -> 2
          other -> Kernel.length(other)
        end
      end
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
        |> run_while()

      assert_tri evaluated do
        case something do
          %{1 => x} -> x
          [_, y | _] -> y
          %{} -> :map
          l when is_list(l) -> :list
          _ -> :error
        end
      end
    end

    test "same variable twice" do
      evaluated =
        tri do
          case x do
            [head, head] when head > 10 -> head
          end
        end
        |> run_while()

      assert_tri evaluated do
        case x do
          [head, head] when head > 10 -> head
        end
      end
    end

    test "outer context in guard" do
      evaluated =
        tri do
          head = something
          case x do
            1 when head > 10 -> :ok
          end
        end
        |> run_while()

      assert_tri evaluated do
        case x do
          1 when something > 10 -> :ok
        end
      end
    end

    test "outer context in deeper in guard" do
      evaluated =
        tri do
          case arg do
            x when true and y > 10 and x > 10 -> :ok
          end
        end
        |> run_while()

      # Why no tri? Well, because `assert` is fucking shit
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
        |> run_while()

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
          # Note double `r` on the end to avoid evaluating this code
          GenServerr.start_link(__MODULE__, [], opts)
        end
        |> run_while()

      assert_tri evaluated do
        GenServerr.start_link(tri(__MODULE__), [], option: :value, restart: :temporary, name: tri(__MODULE__))
      end
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
        |> run_while()

      assert_tri evaluated do
        try do
          {:ok, {2}}
        catch
          :path_not_found -> :error
        end
      end
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
        |> run_while()

      assert_tri evaluated do
        case function.(1) do
          {:ok, new_value} -> List.replace_at([1], 0, new_value)
          :delete_me -> []
          :error -> Kernel.throw(:path_not_found)
        end
      end
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
        |> run_while()

      assert_tri evaluated do
        Kernel.throw :path_not_found
      end
    end

    test "fn in variable" do
      evaluated =
        tri do
          func = fn x -> x + 1 end
          func.(func.(10))
        end
        |> run_while()

      assert_tri evaluated do
        func = fn x -> Kernel.+(x, 1) end
        12
      end
    end

    test "Pathex-style fn in fn" do
      evaluated =
        tri do
          variable_0 = fn
           :update, {x, function} ->
             case x do
               %{"x" => x} = map ->
                 with {:ok, y} <- function.(x) do
                   {:ok, %{map | "x" => y}}
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
                 %{"y" => x} = map ->
                   with {:ok, y} <- function.(x) do
                     {:ok, %{map | "y" => y}}
                   else
                     []
                   end

                 _ ->
                   :error
               end

             :view, {x, function} ->
               case x do
                 %{"y" => x} -> function.(x)
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
        |> run_while()

      assert_tri evaluated do
        _
        _
        fn
          :view, {x1, func1} ->
            case x1 do
              %{"x" => x2} ->
                case x2 do
                  %{"y" => x3} -> func1.(x3)
                  _ -> :error
                end

              _ ->
                :error
            end

          :update, {x4, func2} ->
            case x4 do
              %{"x" => x6} = x5 ->
                with {:ok, y1} <-
                       (case x6 do
                          %{"y" => x8} = x7 ->
                            with {:ok, y2} <- func2.(x8) do
                              {:ok, %{x6 | "y" => y2}}
                            end

                          _ ->
                            :error
                        end) do
                  {:ok, %{x4 | "x" => y1}}
                end

              _ ->
                :error
            end
        end
      end

      assert 9 == length Enum.uniq [x1, x2, x3, x4, x5, x6, x7, y1, y2]
    end
  end

  describe "Shadowing" do
    test "Simple" do
      evaluated =
        tri do
          y = x + x
          x = 2
          x + x + y
        end
        |> run_while()

      assert_tri evaluated do
        y = Kernel.+(x, x)
        Kernel.+(4, y)
      end
    end
  end

  describe "List concatenation" do
    test "Prepend" do
      evaluated =
        tri do
          x = [2, 3]
          [1 | x]
        end
        |> run_while()

      assert [1, 2, 3] == evaluated
    end

    test "Concat" do
      evaluated =
        tri do
          x = [2, 3]
          [1] ++ x
        end
        |> run_while()

      assert [1, 2, 3] == evaluated
    end

    test "Concat and prepend" do
      evaluated =
        tri do
          x = [2]
          [1 | x] ++ [3]
        end
        |> run_while()

      assert [1, 2, 3] == evaluated
    end
  end

  # describe "Interpolation" do
  #   test "string interpolation" do
  #     evaluated =
  #       tri do
  #         "#{x}_y"
  #       end
  #       |> Evaluation.run_once!()
  #       |> Evaluation.run_once!()
  #   end

  #   test "atom interpolation" do
  #     evaluated =
  #       tri do
  #         :"#{x}_y"
  #       end
  #       |> Evaluation.run_once!()
  #       |> Evaluation.run_once!()
  #   end
  # end

  @tag :cubdb
  describe "CubDB" do
    test "binary" do
      evaluated =
        tri do
          data_size = block_size - 1
          <<block::binary-size(data_size), rest::binary>> = bin
        end
        |> run_while()

      assert_tri evaluated do
        data_size = Kernel.-(block_size, 1)
        <<block::binary-size(data_size), rest::binary>> = bin
      end
    end

    test "one" do
      evaluated =
        tri do
          data_size = Kernel.-(block_size, 1)

          case byte_size(bin) <= block_size - 1 do
            x when :erlang.orelse(:erlang."=:="(x, false), :erlang."=:="(x, nil)) ->
              <<block::binary-size(data_size), rest::binary>> = bin

              CubDB.Store.File.Blocks.add(
                rest,
                [block, <<0>> | acc],
                block_size
              )

            _ ->
              Enum.reverse([bin, <<0>> | acc])
          end
        end
        |> run_while()

      assert_tri evaluated do
        data_size = Kernel.-(block_size, 1)

        case Kernel.<=(Kernel.byte_size(bin), Kernel.-(block_size, 1)) do
          x when :erlang.orelse(:erlang."=:="(x, false), :erlang."=:="(x, nil)) ->
            <<block::binary-size(data_size), rest::binary>> = bin

            CubDB.Store.File.Blocks.add(
              rest,
              [block, <<0>> | acc],
              block_size
            )

          _ ->
            Enum.reverse([bin, <<0>> | acc])
        end
      end
    end
  end

  describe "Pathex-style nesting" do
    test "case" do
      evaluated =
        tri do
          case x do
            %{"x" => x} ->
              case x do
                %{"x" => x} -> 10 + 10
                _ -> :error
              end

            _ ->
              :error
          end
        end
        |> run_while()

      assert_tri evaluated do
        case tri(x, _, x1) do
          %{"x" => tri(x, _, x2)} ->
            case tri(x, _, x2) do
              %{"x" => tri(x, _, x3)} -> 20
              _ -> :error
            end

          _ ->
            :error
        end
      end

      assert 3 == length Enum.uniq [x1, x2, x3]
    end

    test "fn" do
      evaluated =
        tri do
          case x do
            %{"x" => x} ->
              (fn
                %{"x" => x} -> 10 + 10
                _ -> :error
              end).(x)

            _ ->
              :error
          end
        end
        |> run_while()

      assert_tri evaluated do
        case tri(x, _, x1) do
          %{"x" => tri(x, _, x2)} ->
            case tri(x, _, x2) do
              %{"x" => tri(x, _, x3)} -> 20
              _ -> :error
            end

          _ ->
            :error
        end
      end

      assert 3 == length Enum.uniq [x1, x2, x3]
    end

    test "real" do
      evaluated =
        tri do
          variable_0 = fn
            :view, {x, function} ->
              case x do
                %{"x" => x} -> function.(x)
                _ -> :error
              end
          end

          variable_1 = fn
            :view, {x, function} ->
              case x do
                %{"y" => x} -> function.(x)
                _ -> :error
              end
          end

          fn
           :view, {x, func} ->
             variable_0.(:view, {x, fn x -> variable_1.(:view, {x, func}) end})
          end
        end
        |> run_while()

      assert_tri evaluated do
        _
        _
        fn
          :view, {x1, function} ->
            case x1 do
              %{"x" => x2} ->
                case x2 do
                  %{"y" => x3} -> function.(x3)
                  _ -> :error
                end

              _ -> :error
            end
        end
      end

      assert 3 == length Enum.uniq [x1, x2, x3]
    end
  end

  test "Inlining isolation" do
    evaluated =
      tri do
        (fn x, function ->
           case x do
             %{"x" => x} -> function.(x)
             _ -> :error
           end
         end).(x, fn x ->
           case x do
             %{"y" => x} -> x
             _ -> :error
           end
         end)
      end
      |> run_while()

    assert_tri evaluated do
      case x1 do
        %{"x" => x2} ->
          case x2 do
            %{"y" => x3} ->
              x3

            _ ->
              :error
          end

        _ ->
          :error
      end
    end

    assert 3 == length Enum.uniq [x1, x2, x3]
  end

  test "Inlining isolation 2" do
    evaluated =
      tri do
        (fn x, function ->
          case x do
            %{3 => x} ->
              function.(x)

            [_, _, _, x | _] ->
              function.(x)

            tuple when :erlang.andalso(is_tuple(tuple), tuple_size(tuple) > 3) ->
              function.(:erlang.element(4, tuple))

            _ ->
              :error
          end
        end).(input, fn x -> {:ok, x} end)
      end
      |> run_while()

    assert_tri evaluated do
      case input do
        %{3 => x1} ->
          {:ok, x1}

        [_, _, _, x2 | _] ->
          {:ok, x2}

        tuple when :erlang.andalso(is_tuple(tuple), tuple_size(tuple) > 3) ->
          {:ok, :erlang.element(4, input)}

        _ ->
          :error
      end
    end
  end

  describe "Structure" do
    defmodule S do
      defstruct [x: 1]
    end

    test "Defined variable remains present" do
      tri do
        underscore_5063 = tri(S).__struct__()

        {underscore_5095, underscore_5127} =
          Enum.split_with(underscore_5031, fn {underscore_5159, _} ->
            :maps.is_key(underscore_5159, underscore_5063)
          end)
      end
      |> run_while()
      |> inspect_ast(label: :result)
      |> assert_tri do
        _
        _
      end
    end
  end
end
