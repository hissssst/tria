defmodule Tria do
  # alias Tria.Translator.Elixir, as: ElixirTranslator
  import Tria.Common

  @type t :: Macro.t()
  @type variable :: {atom(), list(), atom()}
  @type special_form :: {}
  @type call :: {atom(), list(), atom()}

  defmacro __using__(context: context) do
    context = unalias(context)
    module = __CALLER__.module
    Module.register_attribute(module, :defs, accumulate: true)
    # Module.register_attribute(module, :context, persist: true)
    # Module.put_attribute(module, :context, context)
    start(context)
    quote do
      @on_definition unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
      @context unquote(context)
    end
  end

  def __on_definition__(%Macro.Env{module: module}, kind, name, args, guards, body) do
    Module.put_attribute(module, :defs, {{kind, name}, {args, guards, body}})
  end

  defmacro __before_compile__(%Macro.Env{module: module}) do
    context = Module.get_attribute(module, :context)
    defs = join_clauses Module.get_attribute(module, :defs, []) |> IO.inspect(label: :defs)
    defs =
      Enum.map(defs, fn {{kind, name}, [{args, _guards, _body} | _] = clauses} ->
        Module.delete_definition(module, {name, length(args)})
        GenServer.call(context, {:new_def, module, kind, name, clauses})
        args = for i <- 1..length(args), do: {:"var_#{i}", [], nil}
        fname = fname(module, name)
        quote do
          def unquote(name)(unquote_splicing args) do
            unquote(context).unquote(fname)(unquote_splicing args)
          end
        end
      end)

    GenServer.call(context, {:ready, module}, :infinity)

    {:__block__, [], defs}
  end

  defp join_clauses([{name, {args1, _, _} = clause1}, {name, {args2, _, _} = clause2} | tail]) do
    if length(args1) == length(args2) do
      join_clauses [{name, [clause1, clause2]} | tail]
    else
      [{name, List.flatten List.wrap clause1}, {name, List.flatten List.wrap clause2} | join_clauses tail]
    end
  end
  defp join_clauses([{name, clause} | tail]) do
    [{name, List.flatten List.wrap clause} | join_clauses tail]
  end
  defp join_clauses([]), do: []

  # GenServer

  def start(name) do
    # Trick to inherit compiler state
    # `Module.create` is not an option, because it doesn't generate the ebin file
    pdict = Process.get() |> Keyword.put(:elixir_compiler_modules, [])
    case GenServer.start(__MODULE__, %{name: name, pdict: pdict}, name: name) |> IO.inspect do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  def init(%{name: name, pdict: pdict}) do
    for {k, v} <- pdict, do: Process.put(k, v)
    {:ok, %{definitions: %{}, modules: MapSet.new(), name: name}}
  end

  def handle_call({:new_def, module, kind, name, clauses}, _from, %{definitions: defs, modules: mods} = state) do
    IO.puts "#{inspect module} new_def"
    defs = Map.put(defs, {module, name}, {kind, clauses})
    {:reply, :ok, %{state | definitions: defs, modules: MapSet.put(mods, module)}}
  end

  def handle_call({:ready, module}, _from, %{definitions: defs, modules: mods, name: name} = state) do
    mods = MapSet.delete(mods, module)
    if MapSet.new() == mods do
      create_module(name, defs)
    end
    {:reply, :ok, %{state | modules: mods}}
  end

  defp create_module(name, defs) do
    funcs = defs_to_funcs(defs)

    [{module, binary}] =
      Code.compile_quoted(
        quote do
          defmodule unquote(name) do
            unquote_splicing(funcs)
          end
        end,
        "ttt.ex"
      )

    :code.load_binary(module, '#{Process.get :elixir_compiler_dest}/#{module}.beam', binary)
  end

  defp defs_to_funcs(defs) do
    Enum.flat_map(defs, fn {{module, name}, {_kind, clauses}} ->
      fname = fname(module, name)
      for {args, guards, body} <- clauses do
        case guards do
          [] ->
            quote do
              def(unquote(fname)(unquote_splicing args), unquote(body))
            end

          guards ->
            quote do
              def(unquote(fname)(unquote_splicing args) when unquote(guards), unquote(body))
            end
        end
      end
    end)
  end

  defp fname(_module, name), do: name

end
