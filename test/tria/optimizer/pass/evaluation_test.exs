defmodule Tria.Optimizer.Pass.EvaluationTest do
  use ExUnit.Case

  import Tria.Language, only: [inspect_ast: 2], warn: false
  import Tria.Language.Tri
  import Tria.TestHelpers

  alias Tria.Optimizer.Pass.Evaluation, as: Evaluation
  alias Tria.Compiler.SSATranslator

  @tri_opts meta: false, to_ssa: true

  # Does this even work in elixir?
  @compile :nowarn_unused_vars

  defp run_while(ast, opts \\ []) do
    Evaluation.run_while(ast, opts)
  end

  describe "Structural evaluation" do
    test "Block joining" do
      tri do
        (M.x1(); M.x2()); (M.x3(); M.x4()); (M.x5(); M.x6(); M.x7())
      end
      |> run_while()
      |> assert_tri do
        M.x1(); M.x2(); M.x3(); M.x4(); M.x5(); M.x6(); M.x7()
      end
    end

    test "Block literal removal" do
      tri do
        1; 2; M.f(); 3; 4; 5
      end
      |> run_while()
      |> assert_tri do
        M.f(); 5
      end
    end

    test "List cons" do
      tri do
        [0, 1 | [2 | [3]]]
      end
      |> run_while()
      |> assert_tri do
        [0, 1, 2, 3]
      end
    end

    test "Map cons joined" do
      tri do
        %{%{a: 1} | a: 2, b: 3, c: 4}
      end
      |> run_while()
      |> assert_tri do
        %{a: 1, a: 2, b: 3, c: 4}
      end
    end

    test "Map cons unjoined" do
      tri do
        %{x | a: 2, b: 3, c: 4}
      end
      |> run_while()
      |> assert_tri do
        %{_x | a: 2, b: 3, c: 4}
      end
    end

    test "Map cons cons joined" do
      tri do
        %{%{x | x: 2} | x: 3}
      end
      |> run_while()
      |> assert_tri do
        %{_x | x: 2, x: 3}
      end
    end
  end

  describe "Variable propagation" do
    test "Constant" do
      tri do
        x = 1
        y = 2
        f(x, y)
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x = 1
        y = 2
        f(1, 2)
      end

      assert x != y
    end

    test "The fn" do
      tri do
        function = fn x -> f(x, 1) end
        f(function)
      end
      |> run_while(remove_unused: false)
      |> last_line()
      |> assert_tri do
        f(fn x -> f(x, 1) end)
      end
    end

    test "Variable chained" do
      tri do
        x = 1
        y = x
        z = y
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x = 1
        y = 1
        z = 1
      end

      assert_unique [x, y, z]
    end

    test "Quoted literal composed" do
      tri do
        x = 1
        y = 2
        z = [x, y]
        list = [z, z]
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x = 1
        y = 2
        z = [1, 2]
        list = [[1, 2], [1, 2]]
      end

      assert_unique [x, y, z, list]
    end

    test "Guard" do
      tri do
        x = 1
        y = 2
        case x do
          _ when y > z -> 1
          _ -> 2
        end
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x = 1
        y = 2
        case 1 do
          _ when Kernel.>(2, z) -> 1
          _ -> 2
        end
      end

      assert_unique [x, y, z]
    end

    test "Guard propagation stops" do
      tri do
        x = function(x)
        y = 1
        case something do
          _ when x > y -> 1
          _ -> 2
        end
      end
      |> run_while()
      |> assert_tri do
        x2 = function(x1)
        case something do
          _ when Kernel.>(x2, 1) -> 1
          _ -> 2
        end
      end

      assert_unique [x1, x2, something]
    end

    test "Guard propagation recursive stops" do
      tri do
        x = function()
        y = 1
        z = is_atom(x)
        case something do
          _ when z or :erlang.is_map_key(y, m) -> 1
          _ -> 2
        end
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x = function()
        y = 1
        z = Kernel.is_atom(x)
        case something do
          _ when :erlang.orelse(Kernel.is_atom(x), :erlang.is_map_key(1, m)) -> 1
          _ -> 2
        end
      end

      assert_unique [x, y, z, something, m]
    end

    @tag skip: true
    test "Definitions in strange places" do
      tri do
        x = (x = 1; y = x + 1)
        z = [y = x + 1, x = y - 1]
        case z = z ++ z do
          ^z -> z
          z -> z
        end
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x2 = (x1 = 1; y1 = 2)
        z1 = [y2 = 3, x3 = 1]
        z2 = [3, 1, 3, 1]
      end
    end

    test "Propagated and removed" do
      tri do
        field = func(variable)
        [38, encode_pair(field)]
      end
      |> run_while()
      |> assert_tri do
        [38, encode_pair func variable]
      end
    end

    test "Propagated and removed take 2" do
      tri do
        mapper = fn
          {_, 1} ->
            []

          {field, value} ->
            field =
              case parent_field do
                "" -> field
                _ -> parent_field
              end

            [?&, encode_pair(field, value, encoder)]
        end

        kv
        |> Enum.flat_map(mapper)
      end
      |> run_while()
      |> assert_tri do
        Enum.flat_map(kv, fn
          {_, 1} -> []
          {field, value} ->
            [
              38,
              encode_pair(
                case parent_field do
                  "" -> field
                  _ -> parent_field
                end,
                value,
                encoder
              )
            ]
        end)
      end
    end
  end

  describe "Context inheritance" do
    test "Block shadowing" do
      tri do
        x = 1
        y = x
        x = 2
        y = x
        z = [x, y]
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x1 = 1
        y1 = 1
        x2 = 2
        y2 = 2
        z = [2, 2]
      end

      assert_unique [x1, x2, y1, y2, z]
    end

    test "List, map, tuple and function arguments" do
      tri do
        [x = 1, x = 2 | x = 3]
        M.f(x) # Here we use `f` to make sure that block optimization does not kick in

        {y = 1, y = 2, y = 3}
        M.f(y)

        %{z = %{} | x: (z = 2), x: (z = 3)}
        M.f(z)

        f(a = 1, a = 2, a = 3)
        M.f(a)

        (b = 1).(b = 2, b = 3)
        M.f(b)
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x1 = 1
        x2 = 2
        x3 = 3
        M.f(3)
        y1 = 1
        y2 = 2
        y3 = 3
        M.f(3)
        z1 = %{}
        z2 = 2
        z3 = 3
        M.f(3)
        a1 = 1
        a2 = 2
        a3 = 3
        M.f(3)
        (b1 = 1).(b2 = 2, b3 = 3)
        M.f(3)
      end

      assert_unique [
        x1, x2, x3,
        y1, y2, y3,
        z1, z2, z3,
        a1, a2, a3,
        b1, b2, b3,
      ]
    end

    test "Case clause context" do
      tri do
        x = 1
        case something do
          {:x, ^x} -> x
          {:x, x} -> x
          {:y, y} -> f(x, y)
          {:z, y} when maybe -> y
        end
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x1 = 1
        case something do
          {:x, 1} -> 1
          {:x, x2} -> x2
          {:y, y1} -> f(1, y1)
          {:z, y2} when maybe -> y2
        end
      end

      assert_unique [x1, x2, y1, y2, maybe, something]
    end

    test "Fn clauses" do
      tri do
        x = 1
        fn
          1, ^x -> x
          1, x -> x
          1, y -> f(x, y)
          1, z when maybe -> f(x, z)
        end
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        x1 = 1
        fn
          1, 1 -> 1
          1, x2 -> x2
          1, y -> f(1, y)
          1, z when maybe -> f(1, z)
        end
      end

      assert_unique [x1, x2, y, z, maybe]
    end
  end

  describe "Simple evaluation" do
    test "Constant" do
      evaluated =
        tri do
          x = 1 + 2
          y = x + x
          y + 5
        end
        |> run_while()

      assert 11 == last_line evaluated
    end

    test "x + y" do
      tri do
        a = 1 + 2
        b = 2 + 2
        a + b + x + y
      end
      |> run_while(remove_unused: false)
      |> assert_tri do
        a = 3
        b = 4
        Kernel.+(Kernel.+(7, x), y)
      end

      assert_unique [a, b, x, y]
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
        |> run_while()

      assert 2 == last_line evaluated
    end

    test "yes case by pattern" do
      x = {:x, [], 1}
      y = {:y, [], 1}
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

      assert tri(Kernel.+(tri(^x), tri(^y))) = last_line evaluated
    end

    test "maybe case" do
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
      |> assert_tri do
        case [1, 2 | tail] do
          [_, _] -> 2
          other -> Kernel.length([1, 2 | tail])
        end
      end
    end

    test "pathex-style force_update" do
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
      |> assert_tri do
        case something do
          %{1 => x} -> x
          [_, y | _] -> y
          %{} -> :map
          l when Kernel.is_list(l) -> :list
          _ -> :error
        end
      end

      assert_unique [x, y, l, something]
    end

    test "same variable twice" do
      tri do
        case x do
          [head, head] when head > 10 -> head
        end
      end
      |> run_while()
      |> assert_tri do
        case x do
          [head, head] when Kernel.>(head, 10) -> head
        end
      end

      assert head != x
    end

    test "outer context in guard" do
      tri do
        head = something
        case x do
          1 when head > 10 -> :ok
        end
      end
      |> run_while()
      |> last_line()
      |> assert_tri do
        case x do
          1 when Kernel.>(something, 10) -> :ok
        end
      end

      assert x != something
    end

    test "outer context in deeper in guard" do
      evaluated =
        tri do
          case arg do
            x when true and y > 10 and x > 10 -> :ok
          end
        end
        |> run_while()

      # Why no tri? Well, because `assert` is buggy
      assert {
        :case, _, [_arg, [do: [
          {:"->", _, [
            [{:when, _, [x,
              {andd, _, [
                {andd, _, [
                  true,
                  {more, _, [_y, 10]}
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

  describe "Fn inlining" do
    test "constant" do
      tri do
        (fn x -> send(pid, x) end).(123)
      end
      |> run_while()
      |> assert_tri do
        Kernel.send(_pid, 123)
      end
    end

    test "variable" do
      tri do
        send(pid, y)
        (fn x -> send(pid, x) end).(y)
      end
      |> run_while()
      |> assert_tri do
        Kernel.send(pid, y)
        Kernel.send(pid, y)
      end
    end
  end

  describe "Complex" do
    test "GenServer" do
      tri do
        opts = [name: __MODULE__, restart: :always]
        opts = Keyword.put(opts, :restart, :temporary)
        opts = Keyword.put(opts, :option, :value)
        # Note double `r` on the end to avoid evaluating this code
        GenServerr.start_link(__MODULE__, [], opts)
      end
      |> run_while()
      |> last_line()
      |> assert_tri do
        GenServerr.start_link(tri(__MODULE__), [], option: :value, restart: :temporary, name: tri(__MODULE__))
      end
    end

    test "Pathex-style fn inlining" do
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
      |> assert_tri do
        {:ok, {2}}
      end
    end

    test "Pathex-style fn inlining 2" do
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
      |> assert_tri do
        case function.(1) do
          {:ok, new_value} -> List.replace_at([1], 0, new_value)
          :delete_me -> []
          :error -> Kernel.throw(:path_not_found)
        end
      end

      assert_unique [function, new_value]
    end

    test "Pathex-style fn inlining 3" do
      tri do
        case {} do
          %{0 => _} = _map -> 1
          [x | _] = _list -> 2
          _ -> Kernel.throw(:path_not_found)
        end
      end
      |> run_while()
      |> assert_tri do
        Kernel.throw :path_not_found
      end
    end

    test "fn in variable" do
      tri do
        func = fn x -> x + 1 end
        func.(func.(10))
      end
      |> run_while()
      |> assert_tri do
        12
      end
    end

    test "Pathex-style fn in fn" do
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
      |> last_line()
      |> assert_tri do
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

      assert_unique [x1, x2, x3, x4, x5, x6, x7, y1, y2]
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

      assert [1, 2, 3] == last_line evaluated
    end

    test "Concat" do
      evaluated =
        tri do
          x = [2, 3]
          [1] ++ x
        end
        |> run_while()

      assert [1, 2, 3] == last_line evaluated
    end

    test "Concat and prepend" do
      evaluated =
        tri do
          x = [2]
          [1 | x] ++ [3]
        end
        |> run_while()

      assert [1, 2, 3] == last_line evaluated
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
      tri do
        data_size = block_size - 1
        <<block::binary-size(data_size), rest::binary>> = bin
      end
      |> run_while()
      |> assert_tri do
        data_size = Kernel.-(block_size, 1)
        <<block::binary-size(data_size), rest::binary>> = bin
      end

      assert_unique [data_size, block_size, rest, bin, block]
    end

    test "one" do
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
      |> assert_tri do
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
      |> assert_tri do
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

      assert_unique [x1, x2, x3]
    end

    test "fn" do
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
      |> assert_tri do
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

      assert_unique [x1, x2, x3]
    end

    test "real" do
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
      |> last_line()
      |> assert_tri do
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

      assert_unique [x1, x2, x3]
    end
  end

  test "Inlining isolation" do
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
    |> assert_tri do
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

    assert_unique [x1, x2, x3]
  end

  test "Inlining isolation 2" do
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
    |> last_line()
    |> assert_tri do
      case input do
        %{3 => x1} ->
          {:ok, x1}

        [_, _, _, x2 | _] ->
          {:ok, x2}

        tuple when :erlang.andalso(Kernel.is_tuple(tuple), Kernel.>(Kernel.tuple_size(tuple), 3)) ->
          x3 = :erlang.element(4, input)
          {:ok, x3}

        _ ->
          :error
      end
    end

    assert_unique [x1, x2, x3, tuple, input]
  end

  describe "Structure" do
    defmodule S do
      defstruct [x: 1]
    end

    test "Defined variable remains present" do
      tri do
        structure = tri(S).__struct__()

        {with_key, without_key} =
          Enum.split_with(list, fn {key, _} ->
            :maps.is_key(key, structure)
          end)
      end
      |> run_while()
      |> last_line()
      |> assert_tri do
        {with_key, without_key} =
          Enum.split_with(list, fn {key, _} ->
            :maps.is_key(key, %{__struct__: tri(S), x: 1})
          end)
      end

      assert_unique [with_key, without_key, key, list]
    end
  end

  describe "Usage detection" do
    @tag skip: true
    test "Block" do
      tri do
        x = function()
        y = x + x
        z = f(y, y)
      end
      |> run_while(remove_unused: true)
      |> assert_tri do
        x = function()
        y = Kernel.+(x, x)
        f(y, y)
      end

      assert_unique [x, y]
    end

    @tag skip: true
    test "Used once in pin removed" do
      tri do
        x = [y, z]
        {^x, ^x, bar} = foo
        bar
      end
      |> run_while(remove_unused: true)
      |> assert_tri do
        {[^y, ^z], [^y, ^z], bar} = foo
        bar
      end

      assert_unique [y, z, bar, foo]
    end

    @tag skip: true
    test "Pure, vared, quoted used once" do
      tri do
        x = a + b
        y = [c, c, c]
        z = [1, 2, 3]
        impure = :persistent_term.get(:key)
        {x, y, z, impure}
      end
      |> run_while(remove_unused: true)
      |> assert_tri do
        impure = :persistent_term.get(:key)
        {Kernel.+(a, b), [c, c, c], [1, 2, 3], impure}
      end

      assert_unique [a, b, c, impure]
    end

    @tag skip: true
    test "Block unused removal with fn" do
      tri do
        x = [1, fn x -> y = M.f(); x + y end]
        {x, x}
      end
      |> run_while()
      |> assert_tri do
        x1 = [1, fn x2 -> y = M.f(); Kernel.+(x2, y) end]
        {x1, x1}
      end

      assert_unique [x1, x2, y]
    end

    @tag skip: true
    test "Simplest in block used once" do
      tri do
        y = x + 1
        y * 2
      end
      |> run_while()
      |> assert_tri do
        Kernel.*(Kernel.+(x, 1), 2)
      end
    end

    @tag skip: true
    test "Used once inside fn" do
      tri do
        fn x ->
          y = x + 1
          y * 2
        end
      end
      |> run_while()
      |> assert_tri do
        fn x -> Kernel.*(Kernel.+(x, 1), 2) end
      end
    end
  end

  describe "Regression" do
    test "Plug" do
      tri do
        try do
          URI.decode_www_form(value)
        catch
          :error, :badarg ->
            raise invalid_exception, "invalid urlencoded params, got #{value}"
        else
          binary ->
            case validate_utf8 do
              true ->
                throw :oops

              _ ->
                nil
            end

            binary
        end
      end
      |> run_while()
      |> assert_tri do
        try do
          URI.decode_www_form(value)
        catch
          :error, :badarg ->
            :erlang.error(_)
        else
          binary ->
            case validate_utf8 do
              true ->
                Kernel.throw :oops

              _ ->
                nil
            end

            binary
        end
      end
    end
  end
end
