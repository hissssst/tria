defmodule Tria.Compiler.AbstractTranslator do

  @moduledoc """
  Erlang ATF to Tria translator
  It is designed to translate ATF provided in `abstract_code` chunk
  therefore it is unable to translate records, macros and parsetransforms

  #TODO preserve `anno`
  """

  @behaviour Tria.Compiler.Translator

  import Tria.Language, except: [pin: 1, pin: 2, traverse: 4]
  import Tria.Language.Tri
  alias Tria.Debug.Tracer
  alias Tria.Language.Codebase
  alias Tria.Compiler.SSATranslator

  def to_tria!(abstract, opts \\ []) do
    {:ok, tria, _} = to_tria(abstract, opts)
    tria
  end

  def to_tria(abstract, opts \\ [])
  def to_tria(abstract, %Macro.Env{} = env) do
    to_tria(abstract, env: env)
  end
  def to_tria(abstract, opts) when is_list(opts) do
    to_tria(abstract, Map.new opts)
  end
  def to_tria(abstract, opts) do
    traverse_function =
      if is_list(abstract) and opts[:as_block] do
        &traverse_block/1
      else
        &traverse/1
      end

    opts
    |> Map.take([ :locals, :env ])
    |> with_pdict(fn ->
      tria =
        abstract
        # |> IO.inspect(label: :abstract)
        |> traverse_function.()
        |> Tracer.tag_ast(label: :abstract_pre_ssa)
        |> Tracer.tag(label: :abstract_pre_ssa_ast, pretty: true)
        |> to_ssa(pin_known: true)
        |> Tracer.tag_ast(label: :abstract_after_ssa)
        # |> inspect_ast(label: :abstracted)

      {:ok, tria, []}
    end)
  end

  defp to_ssa([{:"->", _, _} | _] = clauses, opts) do
    with {:fn, _, clauses} <- to_ssa({:fn, [], clauses}, opts) do
      clauses
    end
  end
  defp to_ssa(ast, opts), do: SSATranslator.from_tria!(ast, opts)

  def traverse(abstract) do
    case abstract do
      # Specials
      list when is_list(list) ->
        Enum.map(list, &traverse/1)

      {nil, _anno} ->
        []

      # Operators
      {:op, anno, op, left, right} ->
        args = traverse [left, right]
        traverse_op(op, anno, args)

      {:op, anno, op, arg} ->
        args = traverse [arg]
        traverse_op(op, anno, args)

      # Case
      {:case, anno, arg, clauses} ->
        clauses = Enum.map(clauses, &traverse/1)
        arg = traverse(arg)
        {:case, meta(anno), [arg, [do: clauses]]}

      # If
      {:if, anno, clauses} ->
        #TODO needs testing
        clauses =
          clauses
          |> traverse()
          |> Enum.map(fn {:"->", meta, [[{:when, _, [condition]}], body]} ->
            {:"->", meta, [[condition], body]}
          end)

        {:cond, meta(anno), [[do: clauses]]}

      # List comprehension
      {:lc, anno, body, loops} ->
        {:for, meta(anno), traverse(loops) ++ [[do: traverse_block body]]}

      {:block, anno, block} ->
        {:__block__, meta(anno), Enum.map(block, &traverse/1)}

      # Equals
      {:match, anno, left, right} ->
        {:=, meta(anno), [traverse(left), traverse_block(right)]}

      # Maybe
      {:maybe_match, anno, left, right} ->
        {:"<-", meta(anno), [traverse(left), traverse_block(right)]}

      {:maybe, anno, body, {:else, _anno2, elses}} ->
        {res, clauses} = add_res_to_clauses traverse body
        elses = traverse elses
        {:with, meta(anno), clauses ++ [[do: res, else: elses]]}

      {:maybe, anno, body} ->
        clauses = traverse body
        {res, clauses} = add_res_to_clauses(clauses)
        {:with, meta(anno), clauses ++ [[do: res]]}

      # Receive
      {:receive, anno, clauses} ->
        {:receive, meta(anno), [[do: traverse clauses]]}

      {:receive, anno, [], after_what, do_what} ->
        {:receive, meta(anno), [[after: {traverse(after_what), traverse_block(do_what)}]]}

      {:receive, anno, clauses, after_what, do_what} ->
        {:receive, meta(anno), [[do: traverse(clauses), after: {traverse(after_what), traverse_block(do_what)}]]}

      # Try
      {:try, anno, body, else_clauses, catch_clauses, after_body} ->
        clauses =
          [do: traverse_block body]
          ++ [catch: traverse_catch_clauses(catch_clauses)]
          ++ [else: traverse else_clauses]
          ++ [after: traverse_block after_body]

        cleanup_try {:try, meta(anno), [clauses]}

      {:catch, anno, body} ->
        #TODO maybe this is wrong
        body = traverse_block body
        tri do
          try do
            body
          catch
            :error, any -> any
            :throw, any -> any
          end
        end
        |> with_meta(meta anno)
        |> cleanup_try()

      {:generate, anno, left, right} ->
        {:"<-", meta(anno), [traverse(left), traverse(right)]}

      {:b_generate, anno, left, right} ->
        {:"<-", meta(anno), [traverse(left), traverse(right)]}

      # Binary
      {:bin, anno, elements} ->
        # Why flatten?
        {:"<<>>", meta(anno), List.flatten traverse(elements)}

      {:bin_element, anno, expr, :default, :default} ->
        expr
        |> traverse()
        |> with_meta(meta anno)

      {:bin_element, anno, expr, :default, tsl} ->
        {:"::", meta(anno), [traverse(expr), traverse_tsl(tsl)]}

      {:bin_element, anno, expr, size, tsl} ->
        {:"::", meta(anno), [traverse(expr), traverse_tsl([tsl, {:size, size}])]}

      # Binary comprehension
      {:bc, anno, element, qualifiers} ->
        {:for, meta(anno), [{:<<>>, meta(anno), traverse(qualifiers)}, [do: traverse(element)]]}

      # Charlist
      {:string, _anno, list} ->
        Enum.map(list, &traverse/1)

      # List
      {:cons, anno, head, tail} ->
        tail = traverse(tail)
        head = traverse(head)
        case tail do
          list when is_list(list) ->
            [head | tail]

          other ->
            [{:"|", meta(anno), [head, other]}]
        end

      # Map
      {:map, anno, items} ->
        items =
          Enum.map(items, fn
            {:map_field_assoc, _anno, key, value} ->
              {traverse(key), traverse(value)}

            {:map_field_exact,  anno, key, value} ->
              key = pin(traverse(key), anno)
              {key, traverse(value)}
          end)

        {:"%{}", meta(anno), items}

      {:map, anno, original, items} ->
        #TODO optimize map generation for all map_field_exact
        items
        |> Enum.reduce(traverse(original), fn
          {:map_field_exact, _anno, key, value}, {:"%{}", meta, [{:"|", consmeta, [acc, pairs]}]} ->
            {:"%{}", meta, [{:"|", consmeta, [acc, pairs ++ [{traverse(key), traverse(value)}]]}]}

          {:map_field_exact, anno, key, value}, acc ->
            {:"%{}", [], [{:"|", meta(anno), [acc, [{traverse(key), traverse(value)}]]}]}

          {:map_field_assoc, anno, key, value}, acc ->
            dot_call(Map, :put, [acc, traverse(key), traverse(value)], meta(anno), meta(anno))
        end)
        |> with_meta(meta anno)

      # Tuple
      {:tuple, _anno, [left, right]} ->
        {traverse(left), traverse(right)}

      {:tuple, anno, items} ->
        {:"{}", meta(anno), Enum.map(items, &traverse/1)}

      # Functions, funs and calls
      {:fun, anno, {:function, name, arity}} when is_atom(name) and is_integer(arity) ->
        meta = meta anno
        args = gen_uniq_vars(arity)

        body =
          case module_for_fa(name, arity) do
            nil -> {name, meta, args}
            module -> dot_call(module, name, args, meta, meta)
          end

        {:fn, meta, [{:"->", meta, [args, body]}]}

      {:fun, anno, {:function, module, function, arity}} ->
        meta = meta anno
        case traverse [module, function, arity] do
          [module, function, arity] when is_atom(function) and is_integer(arity) ->
            args = gen_uniq_vars(arity)
            call = dot_call(module, function, args, meta, meta)
            {:fn, meta, [{:"->", meta, [args, call]}]}

          [module, function, arity] ->
            dot_call(Function, :capture, [module, function, arity], meta, meta)
        end

      {:fun, anno, {:clauses, clauses}} ->
        # We call translation this translation, because actually
        # Erlang is not statically assigned. `fun()` arguments shadow
        # existing variables. Therefore, we change names of variables
        # defined in arguments of functions
        {:fn, meta(anno), traverse clauses}
        |> SSATranslator.from_tria!()

      {:named_fun, anno, name, clauses} ->
        #FIXME this one is absolutely incorrect
        var = traverse name
        fun = traverse {:fun, 0, {:clauses, clauses}}
        {:=, meta(anno), [var, fun]}

      {:call, anno, {:remote, dotanno, module, func}, args} ->
        m = meta anno
        dotm = meta dotanno
        dot_call(traverse(module), traverse(func), traverse(args), dotm, m)

      {:call, anno, func, args} ->
        meta = meta anno
        args = traverse(args)
        case traverse(func) do
          func when is_atom(func) ->
            case module_for_fa(func, length args) do
              nil -> {func, meta, args}
              module -> dot_call(module, func, args, meta, meta)
            end

          other ->
            dot_call(other, args)
        end

      # Variable
      {:var, anno, name} ->
        traverse_variable(name, anno)

      # Clauses
      {:clause, anno, left, [], right} ->
        {:"->", meta(anno), [traverse(left), traverse_block(right)]}

      {:clause, anno, left, guards, right} ->
        guard = traverse_guards(guards)
        left = traverse(left)

        {:"->", meta(anno), [
          [{:when, [], left ++ [guard]}],
          traverse_block(right)
        ]}

      # Literals and atoms
      {k, _anno, atom} when k in ~w[float integer atom char]a ->
        atom

      literal when is_literal(literal) ->
        literal
    end
  rescue
    e ->
      IO.inspect(abstract, label: :failed_abstract, pretty: true, limit: :infinity)
      reraise(e, __STACKTRACE__)
  end

  ## Context-specific traversal

  ### Try

  # try/rescue actually generates `catch` clauses.
  # But these clauses can't be transpiled back to Elixir straight away
  # Because there is some magic goin on inside `try`
  #
  # So, this functions translates these `catch` clauses back to rescue
  # There are 3 different cases which are translated differently into Elixir errors
  defp traverse_catch_clauses(clauses) when is_list(clauses) do
    clauses
    |> traverse()
    |> Enum.map(fn {:"->", meta, [[pattern], body]} ->
      case pattern do
        tri {type, exception, stacktrace} when guards ->
          {:"->", meta, [[{:when, [], [type, exception, guards]}], replace_stacktrace(body, stacktrace)]}

        tri {type, exception, stacktrace} ->
          {:"->", meta, [[type, exception], replace_stacktrace(body, stacktrace)]}
      end
    end)
  end

  ### Variables

  defp traverse_variable(name, anno) do
    name
    |> Atom.to_string()
    |> String.downcase()
    |> String.split("@")
    |> case do
      ["_"] ->
        {:_, meta(anno), nil}

      [strname] ->
        {atomify_varname(strname), [], nil}

      ["_", context] ->
        do_traverse_variable(:_, anno, context)

      ["_" <> strname, context] ->
        do_traverse_variable(String.to_atom(strname), anno, context)

      [strname, context | _] ->
        do_traverse_variable(String.to_atom(strname), anno, context)
    end
  end

  defp do_traverse_variable(name, anno, context) do
    case {name, Integer.parse(context)} do
      {:_, {integer, ""}} ->
        {:underscore, meta(anno), integer}

      {:_, :error} ->
        {atomify_varname(context), meta(anno), :underscore}

      {_, {integer, ""}} ->
        {atomify_varname(name), meta(anno), integer}

      _ ->
        {atomify_varname(name), meta(anno), String.to_atom(context)}
    end
  end

  defp atomify_varname(name) when is_binary(name) do
    name |> String.trim_leading("_") |> String.to_atom()
  end
  defp atomify_varname(name) when is_atom(name), do: name

  defp traverse_guards(guards) do
    join(guards, :when, fn guard_tests ->
      join(guard_tests, :and, &traverse_guard/1)
    end)
  end

  ### Block
  defp traverse_block([line]), do: traverse(line)
  defp traverse_block(lines) when is_list(lines), do: {:__block__, [], traverse(lines)}
  defp traverse_block(line), do: traverse(line)

  defp traverse_tsl(tsl) when is_list(tsl) do
    # IO.inspect tsl, label: :tsl
    join(tsl, :"-", &traverse_tsl/1)
  end
  defp traverse_tsl({type, size}), do: {type, [], [traverse(size)]}
  defp traverse_tsl(type) when is_atom(type), do: {type, [], nil}
  defp traverse_tsl(size) when is_integer(size), do: size
  defp traverse_tsl(other), do: traverse(other)

  defp add_res_to_clauses(clauses) do
    [last | rev_tail] = Enum.reverse(clauses)
    res = {:res, [], gen_uniq_context()}
    last = {:=, [], [res, last]}
    {res, Enum.reverse([last | rev_tail])}
  end

  ### Operators

  @op_map %{
    ==:      {Kernel,  :==},
    "/=":    {Kernel,  :!=},
    "=<":    {Kernel,  :<=},
    >=:      {Kernel,  :>=},
    <:       {Kernel,  :<},
    >:       {Kernel,  :>},
    "=:=":   {Kernel,  :===},
    "=/=":   {Kernel,  :!==},
    +:       {Kernel,  :+},
    -:       {Kernel,  :-},
    *:       {Kernel,  :*},
    /:       {Kernel,  :/},
    div:     {Kernel,  :div},
    rem:     {Kernel,  :rem},
    not:     {Kernel,  :not},
    # orelse:  {Kernel,  :or},
    # andalso: {Kernel,  :and},
    orelse:  {:erlang, :orelse},
    andalso: {:erlang, :andalso},
    and:     {:erlang, :and},
    or:      {:erlang, :or},
    xor:     {:erlang, :xor},
    ++:      {Kernel,  :++},
    --:      {Kernel,  :--},
    !:       {Kernel,  :send},
    band:    {Bitwise, :&&&},
    bor:     {Bitwise, :|||},
    bxor:    {Bitwise, :^^^},
    bsl:     {Bitwise, :<<<},
    bsr:     {Bitwise, :>>>},
    bnot:    {Bitwise, :~~~},
  }
  @kernel_ops for {_, {Kernel, op}} <- @op_map, do: op

  defp traverse_op(op, anno, args) do
    %{^op => {m, f}} = @op_map
    dot_call(m, f, args, meta(anno), meta(anno))
  end

  ### Guards

  defp traverse_guard(guard) do
    traverse guard
  end

  ### Exceptions

  defp cleanup_try({:try, meta, [parts]}) do
    parts =
      Enum.reject(parts, fn
        {_, {:__block__, [], []}} -> true
        {:after, none} when is_atom(none) or none == [] -> true
        {part, []} when part in ~w[catch else]a -> true
        _ -> false
      end)

    {:try, meta, [parts]}
  end

  defp replace_stacktrace(body, {:_, _, _context}), do: body
  defp replace_stacktrace(body, {name, _, context}) do
    prewalk(body, fn
      {^name, _, ^context} -> quote do: __STACKTRACE__
      other -> other
    end)
  end

  ### Metadata

  def meta(line) when is_integer(line), do: [line: line]
  def meta({line, column}) do
    [line: line, column: column]
  end
  def meta(keyword) when is_list(keyword) do
    keyword
    |> Keyword.drop(~w[record text]a)
    |> meta_location()
    |> keyword_update_new(:file, &to_string/1)
  end

  defp meta_location(keyword) do
    case Keyword.pop(keyword, :location) do
      {nil, keyword} -> keyword
      {location, keyword} -> meta(location) ++ keyword
    end
  end

  defp with_meta({one, [], two}, meta) do
    {one, meta, two}
  end
  defp with_meta(other, _), do: other

  ### General helpers

  defp list_unwrap([item]), do: item
  defp list_unwrap(items), do: items

  defp keyword_update_new(keyword, key, func) do
    case Keyword.has_key?(keyword, key) do
      true -> Keyword.update!(keyword, key, func)
      false -> keyword
    end
  end

  defp join([head], _op, f), do: f.(head)
  defp join([head | tail], op, f) do
    {op, [], [f.(head), join(tail, op, f)]}
  end

  defp pin(var, anno) when is_variable(var), do: {:"^", meta(anno), [var]}
  defp pin(other, _anno), do: other

  defp erlang_funcs do
    case Process.get(:erlang_funcs, nil) do
      nil ->
        erlang_funcs = MapSet.new Codebase.fetch_functions :erlang
        Process.put(:erlang_funcs, erlang_funcs)
        erlang_funcs

      erlang_funcs ->
        erlang_funcs
    end
  end

  defp module_for_fa(function, arity) do
    module_for_fa {function, arity}
  end
  defp module_for_fa({function, arity} = farity) when is_atom(function) and is_integer(arity) do
    cond do
      farity in Process.get(:locals, []) ->
        nil # Process.get(:env, [])[:module]

      farity in erlang_funcs() ->
        :erlang

      true ->
        Process.get(:env, %{module: nil}).module
    end
  end
end
