defmodule Tria.Compiler.ElixirCompiler do

  @moduledoc """
  Wrapper for `Kernel.ParallelCompiler` with inter-module tracing, ensuring that only
  one compiler runs at the time
  """

  alias Tria.Debug
  alias Tria.Compiler.DependencyTracer

  @parallel_options ~w[each_file each_long_compilation each_module each_cycle long_compilation_threshold profile dest beam_timestamp]a

  @typedoc """
  Available options are options which can be specified in `Code.put_compiler_option`
  or in wrapped-function options plus extra options added

  — `:trace_deps` (boolean) - to trace inter-module compile-time dependencies
  """
  @type option :: {:trace_deps, boolean()}
  | {:ignore_module_conflict, boolean()}
  | {:no_warn_undefined, boolean() | :all}

  @doc """
  Like `Kernel.ParallelCompiler.compile`, but with ability to pass `Code` options
  """
  @spec parallel_compile([Path.t()], Keyword.t()) :: {:ok, [atom()], list()} | {:error, list(), list()}
  def parallel_compile(files, opts) do
    {parallel_options, compiler_options} = Keyword.split(opts, @parallel_options)
    with_compiler_options(compiler_options, fn ->
      parallel_options = maybe_update_hooks(compiler_options, parallel_options)
      Kernel.ParallelCompiler.compile(files, parallel_options)
    end)
  end

  @spec compile_quoted(Macro.t(), Keyword.t()) :: [{module(), binary()}]
  def compile_quoted(quoted, opts) do
    {file, opts} = Keyword.pop(opts, :file, "nofile")
    {debug?, opts} = Keyword.pop(opts, :debug, Debug.debugging?())

    with_compiler_options(opts, fn ->
      try do
        result = Code.compile_quoted(quoted, file)
        DependencyTracer.complete_file(file)
        result
      rescue
        original_error in CompileError ->
          if debug? do
            stacktrace = __STACKTRACE__

            lined =
              Macro.postwalk(quoted, fn ast ->
                Macro.update_meta(ast, &Keyword.put(&1, :line, :erlang.unique_integer([:positive])))
              end)

            try do
              Code.compile_quoted(lined, file)
            rescue
              e in CompileError ->
                Debug.inspect_ast(lined, label: :failed_to_compile, with_contexts: true, highlight_line: e.line)
                reraise e, stacktrace
            end
          else
            reraise original_error, __STACKTRACE__
          end
      end
    end)
  end

  @spec ensure_compiled(module()) :: {:module, module}
  | {:error, :embedded | :badfile | :nofile | :on_load_failure | :unavailable}
  def ensure_compiled(module) do
    at_lock(fn -> Code.ensure_compiled(module) end)
  end

  @spec ensure_compiled!(module :: module()) :: module :: module()
  def ensure_compiled!(module) do
    at_lock(fn -> Code.ensure_compiled!(module) end)
  end

  @spec ensure_loaded!(module :: module()) :: module :: module()
  def ensure_loaded!(module) do
    at_lock(fn -> Code.ensure_loaded!(module) end)
  end

  @spec with_compiler_options(Keyword.t(), (() -> any())) :: any()
  def with_compiler_options([], func), do: at_lock(func)
  def with_compiler_options(options, func) do
    at_lock(fn ->
      {trace_deps?, options} = Keyword.pop(options, :trace_deps, false)
      tracers = if trace_deps?, do: [DependencyTracer], else: []
      options = Keyword.update(options, :tracers, tracers, & &1 ++ tracers)

      was =
        Enum.map(options, fn {key, value} ->
          was = Code.get_compiler_option(key)
          Code.put_compiler_option(key, value)
          {key, was}
        end)

      try do
        maybe_with_tracer(options, func)
      after
        Enum.each(was, fn {key, value} ->
          Code.put_compiler_option(key, value)
        end)
      end
    end)
  end

  ### Local locking mechanics

  @doc """
  This function acquires lock for the compiler.
  And executes the `func` in transactional manner
  in the caller process.

  Lock is written in selective receive style.
  Nested calls to `at_lock` with one locker are
  considered transparent, therefore no error is
  raised when acquiring already acquired lock
  """
  @spec at_lock((() -> any())) :: any()
  def at_lock(func) do
    case call(:lock) do
      :ok ->
        try do
          func.()
        after
          :ok = call(:unlock)
        end

      {:ok, :already_locked} ->
        func.()
    end
  end

  defp call(msg) do
    pid = start()
    ref = Process.monitor(pid)
    send(pid, {msg, self(), ref})
    receive do
      {^ref, answer} ->
        answer

      {:DOWN, ^ref, :process, ^pid, reason} ->
        raise ArgumentError, "Compiler lock died with #{inspect reason}"

      after :timer.hours(2) ->
        :erlang.exit({:timeout, {__MODULE__, :call, [msg]}})
    end
  end

  defp start do
    with nil <- Process.whereis(__MODULE__) do
      pid = spawn(fn -> loop(false) end)
      try do
        Process.register(pid, __MODULE__)
        pid
      rescue
        ArgumentError ->
          Process.exit(pid, :normal)
          start()
      end
    end
  end

  defp loop(false) do
    receive do
      {:lock, owner, ref} ->
        Process.monitor(owner)
        send(owner, {ref, :ok})
        loop(owner)

      {:unlock, owner, ref} ->
        send(owner, {ref, {:error, :already_unlocked}})
        loop(false)

      _ ->
        loop(false)
    end
  end

  defp loop(owner) do
    receive do
      {:unlock, ^owner, ref} ->
        send(owner, {ref, :ok})
        loop(false)

      {:unlock, wrong_owner, ref} ->
        send(wrong_owner, {ref, {:error, :incorrect_owner}})
        loop(owner)

      {:lock, ^owner, ref} ->
        send(owner, {ref, {:ok, :already_locked}})
        loop(owner)

      {:DOWN, _, :process, ^owner, _} ->
        loop(false)
    end
  end

  defp maybe_with_tracer(opts, func) do
    if DependencyTracer in Keyword.get(opts, :tracers, []) do
      DependencyTracer.with_tracer func
    else
      func.()
    end
  end

  defp maybe_update_hooks(compiler_options, parallel_options) do
    if compiler_options[:trace_deps] or DependencyTracer in compiler_options[:tracers] do
      each_file =
        case Keyword.fetch(parallel_options, :each_file) do
          {:ok, func} ->
            fn file -> DependencyTracer.complete_file(file); func.(file) end

          :error ->
            fn file -> DependencyTracer.complete_file(file) end
        end

      parallel_options =
        case Keyword.fetch(parallel_options, :each_cycle) do
          {:ok, func} when is_function(func, 1) ->
            func =
              fn ->
                graphs = DependencyTracer.get_graphs()
                func.(graphs)
              end
            Keyword.put(parallel_options, :each_cycle, func)

          _ ->
            parallel_options
        end

      Keyword.put(parallel_options, :each_file, each_file)
    else
      parallel_options
    end
  end

end
