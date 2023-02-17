defmodule Tria.Compiler.ElixirCompilerTest do
  # Since the elixir compiler is a singleton
  use ExUnit.Case, async: false

  alias Tria.Compiler.ElixirCompiler

  defp quoted_to_file(quoted) do
    string =
      quoted
      |> Macro.to_string()
      |> Code.format_string!()

    filename = Path.join System.tmp_dir!(), "file_#{:erlang.unique_integer([:positive])}"
    File.write!(filename, string)
    on_exit(fn -> File.rm(filename) end)
    filename
  end

  defp purge_modules(modules) do
    on_exit(fn ->
      for module <- modules do
        :code.soft_purge module
        :code.delete module
        :code.purge module
      end
    end)
  end

  describe "dependency tracing" do
    test "compile_quoted" do
      {_compiled, graphs} =
        quote do
          defmodule X do
            Regex
            defmodule Y do
              def f(x) do
                Map.get(x, :x)
              end
            end
          end

          defmodule Z do
            def g(x) do
              Module
              unless x == 2 do
                IO.puts(x)
              end
            end
          end
        end
        |> ElixirCompiler.compile_quoted(trace_deps: true, file: "the_file.ex")

      assert { {X, nil, 0},   {Regex, nil, 0}      } in graphs.depends
      assert { {X.Y, nil, 0}, {Regex, nil, 0}      } in graphs.depends
      assert { {Z, nil, 0},   {Regex, nil, 0}      } in graphs.depends

      refute { {Z, nil, 0},   {Module, nil, 0}     } in graphs.depends
      refute { {Z, :g, 1},    {Module, nil, 0}     } in graphs.depends

      assert { {X.Y, :f, 1},  {Map, :get, 2}       } in graphs.calls
      refute { {X.Y, :f, 1},  {Map, :get, 2}       } in graphs.depends
      refute { {X.Y, nil, 0}, {Map, :get, 2}       } in graphs.depends

      assert { {Z, :g, 1},    {Kernel, :unless, 2} } in graphs.depends
      assert { {Z, :g, 1},    {IO, :puts, 1}       } in graphs.calls
      refute { {Z, :g, 1},    {IO, :puts, 1}       } in graphs.depends
      refute { {Z, nil, 1},   {IO, :puts, 1}       } in graphs.calls

      purge_modules [X, X.Y, Z]
    end

    test "parallel_compile" do
      a =
        quote do
          Map
          defmodule A do
            def f x do
              Regex
              x + 1
            end
          end
        end
        |> quoted_to_file()

      b =
        quote do
          defmodule B do
            Regex
            def f y do
              y + 2
            end
          end
        end
        |> quoted_to_file()

      {_, graphs} = ElixirCompiler.parallel_compile([a, b], trace_deps: true)

      assert { {A, nil, 0}, {Map, nil, 0}   } in graphs.depends
      refute { {B, nil, 0}, {Map, nil, 0}   } in graphs.depends

      assert { {B, nil, 0}, {Regex, nil, 0} } in graphs.depends
      refute { {A, nil, 0}, {Regex, nil, 0} } in graphs.depends

      assert { {A, :f, 1},  {Kernel, :+, 2} } in graphs.calls
      assert { {B, :f, 1},  {Kernel, :+, 2} } in graphs.calls

      purge_modules [A, B]
    end
  end
end
