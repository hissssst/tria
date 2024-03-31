defmodule Tria.Compiler.IncrementalCompilationTest do
  use ExUnit.Case, async: true

  import MixTester, only: [write_ast: 3]

  setup do
    project =
      MixTester.setup(name: :subject, project: [
        consolidate_protocols: false,
        compilers: [:tria, :app],
        deps: [tria: [path: File.cwd!()]]
      ])

    on_exit(fn -> MixTester.cleanup(project) end)
    {:ok, project: project}
  end

  test "basic", %{project: project} do
    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def f(x), do: x + 1
      end
    end)

    write_ast(project, "test/subject/x_test.exs", quote do
      defmodule XGTest do
        use ExUnit.Case, async: true

        test "X.f(1) == 2" do
          assert X.f(1) == 2
        end
      end
    end)

    assert MixTester.mix_test(project)

    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def f(x), do: x + 2
      end
    end)

    write_ast(project, "test/subject/x_test.exs", quote do
      defmodule XGTest do
        use ExUnit.Case, async: true

        test "X.f(1) == 3" do
          assert X.f(1) == 3
        end
      end
    end)

    assert MixTester.mix_cmd(project, "test")
  end

  test "Compiles and recompiles macros", %{project: project} do
    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def g(x), do: x + 100
        def f(x), do: Y.g(x)
      end
    end)

    write_ast(project, "lib/y.ex", quote do
      defmodule Y do
        require Z
        def g(x), do: Z.macro(x)
      end
    end)

    write_ast(project, "lib/z.ex", quote do
      defmodule Z do
        defmacro macro(something) do
          quote do: unquote(something) + 1
        end
      end
    end)

    write_ast(project, "test/subject/x_g_test.exs", quote do
      defmodule XGTest do
        use ExUnit.Case, async: true

        test "X.g(1)" do
          assert X.g(1) == 101
        end
      end
    end)

    write_ast(project, "test/subject/x_f_test.exs", quote do
      defmodule XFTest do
        use ExUnit.Case, async: true

        test "X.f(1) returns 2" do
          assert X.f(1) == 2
        end
      end
    end)

    assert MixTester.mix_test(project)

    write_ast(project, "lib/z.ex", quote do
      defmodule Z do
        defmacro macro(something) do
          quote do: unquote(something) + 2
        end
      end
    end)

    ExUnit.CaptureIO.capture_io(fn ->
      refute MixTester.mix_test(project)
    end)

    write_ast(project, "test/subject/x_f_test.exs", quote do
      defmodule XFTest do
        use ExUnit.Case, async: true

        test "X.f(1) returns 3" do
          assert X.f(1) == 3
        end
      end
    end)

    assert MixTester.mix_test(project)
  end

  test "constant evaluation", %{project: project} do
    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def f(x), do: x + 100
      end
    end)

    write_ast(project, "lib/y.ex", quote do
      defmodule Y do
        def g, do: X.f(123)
      end
    end)

    write_ast(project, "test/subject/y_test.exs", quote do
      defmodule YTest do
        use ExUnit.Case, async: true
        test "Y.g()" do
          assert Y.g() == 223
        end
      end
    end)

    assert MixTester.mix_test(project)

    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def f(x), do: x + 10
      end
    end)

    write_ast(project, "test/subject/y_test.exs", quote do
      defmodule YTest do
        use ExUnit.Case, async: true
        test "Y.g()" do
          assert Y.g() == 133
        end
      end
    end)

    assert MixTester.mix_test(project)
  end

  test "function to macro", %{project: project} do
    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def f(x), do: x + 100
      end
    end)

    write_ast(project, "lib/y.ex", quote do
      defmodule Y do
        require X
        def g(x), do: X.f(123) + X.f(x)
      end
    end)

    write_ast(project, "test/subject/y_test.exs", quote do
      defmodule YTest do
        use ExUnit.Case, async: true
        test "Y.g(1)" do
          assert Y.g(1) == 123 + 100 + 1 + 100
        end
      end
    end)

    assert MixTester.mix_test(project)

    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        defmacro f(x), do: quote do: unquote(x) + 101
      end
    end)

    write_ast(project, "test/subject/y_test.exs", quote do
      defmodule YTest do
        use ExUnit.Case, async: true
        test "Y.g(1)" do
          assert Y.g(1) == 123 + 101 + 1 + 101
        end
      end
    end)

    assert MixTester.mix_test(project)

    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def f(x), do: x + 100
      end
    end)

    write_ast(project, "test/subject/y_test.exs", quote do
      defmodule YTest do
        use ExUnit.Case, async: true
        test "Y.g(1)" do
          assert Y.g(1) == 123 + 100 + 1 + 100
        end
      end
    end)

    assert MixTester.mix_test(project)
  end

  test "private calls handled correctly", %{project: project} do
    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def f(x), do: 100 + g(x)

        defp g(x), do: x
      end
    end)

    write_ast(project, "test/subject/x_test.exs", quote do
      defmodule XTest do
        use ExUnit.Case, async: true
        test "X.f(1)" do
          assert X.f(1) == 100 + 1
        end
      end
    end)

    assert MixTester.mix_test(project)

    write_ast(project, "lib/y.ex", quote do
      defmodule Y do
        def g(x), do: x
      end
    end)

    assert MixTester.mix_test(project)
  end

  test "string interpolation works correctly", %{project: project} do
    write_ast(project, "lib/x.ex", quote do
      defmodule X do
        def f(x), do: "one #{x} three"
        def g(x), do: "one #{x}"
      end
    end)

    write_ast(project, "test/subject/x_test.exs", quote do
      defmodule XTest do
        use ExUnit.Case, async: true
        test "X.f(2)" do
          assert X.f(2) == "one 2 three"
        end

        test "X.g(2)" do
          assert X.g(2) == "one 2"
        end
      end
    end)

    assert MixTester.mix_test(project)

    write_ast(project, "lib/y.ex", quote do
      defmodule Y do
        defstruct [x: 1]
        defimpl String.Chars do
          def to_string(%{x: x}), do: Kernel.to_string(x)
        end
      end
    end)

    write_ast(project, "test/subject/y_test.exs", quote do
      defmodule YTest do
        use ExUnit.Case, async: true
        test "X.f(%Y{x: 2})" do
          assert X.f(%Y{x: 2}) == "one 2 three"
        end

        test "X.g(%Y{x: 2})" do
          assert X.g(%Y{x: 2}) == "one 2"
        end
      end
    end)

    assert MixTester.mix_test(project)
  end
end
