defmodule Tria.Compiler.AbstractTranslator do

  @moduledoc """
  Erlang ATF to Tria translator
  It is designed to translate ATF provided in `abstract_code` chunk
  therefore it is unable to translate records, macros and parsetransforms

  #TODO preserve `anno`
  """

  @behaviour Tria.Compiler.Translator

  import Tria.Language, except: [pin: 1, pin: 2]
  import Tria.Language.Tri
  alias Tria.Debug.Tracer
  alias Tria.Language.Codebase
  alias Tria.Compiler.ElixirTranslator
  alias Tria.Compiler.SSATranslator

  def to_tria!(abstract, env \\ __ENV__) do
    {:ok, tria, _} = to_tria(abstract, env)
    tria
  end

  def to_tria(abstract, env \\ __ENV__)
  def to_tria(abstract, %Macro.Env{} = env) do
    to_tria(abstract, env: env)
  end
  def to_tria(abstract, opts) when is_list(opts) do
    to_tria(abstract, Map.new opts)
  end
  def to_tria(abstract, %{env: env} = opts) do
    traverse_function =
      if is_list(abstract) and opts[:as_block] do
        &traverse_block/1
      else
        &traverse/1
      end

    with_locals(opts, fn ->
      abstract
      |> traverse_function.()
      |> elixir_to_tria(env)
    end)
  end

  defp with_locals(%{locals: locals}, func) do
    old = Process.put(:locals, locals)
    try do
      func.()
    after
      old && Process.put(:locals, old) || Process.delete(:locals)
    end
  end
  defp with_locals(_, func), do: func.()

  defp elixir_to_tria([{:"->", _, _} | _] = clauses, env) do
    with {:ok, {:fn, _, clauses}, env} <- elixir_to_tria({:fn, [], clauses}, env) do
      {:ok, clauses, env}
    end
  end
  defp elixir_to_tria(other, env) do
    with {:ok, tria, env} <- ElixirTranslator.to_tria(other, env) do
      Tracer.tag_ast(tria, label: :abstract_pre_ssa)
      # We call translation to SSA, because actually
      # Erlang is not statically assigned. `fun()` arguments
      # actually shadow existing variables
      {:ok, SSATranslator.from_tria(tria, pin_known: true), env}
    end
  end

  def traverse(abstract) do
    case abstract do
      # Specials
      l when is_list(l) ->
        Enum.map(l, &traverse/1)

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

        quote do
          cond(do: unquote(clauses))
        end
        |> with_meta(meta anno)

      # List comprehension
      {:lc, _anno, body, loops} ->
        quote do: for(unquote_splicing(traverse loops), do: unquote(traverse_block body))

      {:block, anno, block} ->
        {:__block__, meta(anno), Enum.map(block, &traverse/1)}

      # Equals
      {:match, anno, left, right} ->
        {:=, meta(anno), [traverse(left), traverse_block(right)]}

      # Maybe
      {:maybe_match, anno, left, right} ->
        {:"<-", meta(anno), [traverse(left), traverse_block(right)]}

      {:maybe, _anno, body, {:else, _anno2, elses}} ->
        {res, clauses} = add_res_to_clauses traverse body
        elses = traverse elses
        quote do: with(unquote_splicing(clauses), do: unquote(res), else: unquote(elses))

      {:maybe, _anno, body} ->
        clauses = traverse body
        {res, clauses} = add_res_to_clauses(clauses)
        quote do: with(unquote_splicing(clauses), do: unquote(res))

      # Receive
      {:receive, anno, clauses} ->
        quote do
          receive do
            unquote traverse clauses
          end
        end
        |> with_meta(meta anno)

      {:receive, anno, [], after_what, do_what} ->
        quote do
          receive after: (unquote(traverse after_what) -> unquote(traverse_block do_what))
        end
        |> with_meta(meta anno)

      {:receive, anno, clauses, after_what, do_what} ->
        quote do
          receive do
            unquote traverse clauses
          after
            unquote(traverse after_what) -> unquote(traverse_block do_what)
          end
        end
        |> with_meta(meta anno)

      # Try
      {:try, anno, body, else_clauses, catch_clauses, after_body} ->
        clauses =
          [do: traverse_block body]
          ++ traverse_try_clauses(catch_clauses)
          ++ [else: traverse else_clauses]
          ++ [after: traverse_block after_body]

        cleanup_try {:try, meta(anno), [clauses]}

      {:catch, _anno, _body} ->
        # TODO maybe this is wrong
        # quote do
        #   try do
        #     unquote traverse_block body
        #   rescue
        #     any -> any
        #   catch
        #     any -> any
        #   end
        # end
        # |> cleanup_try()

        raise "Not implemented"

      {:generate, _anno, left, right} ->
        quote do: unquote(traverse left) <- unquote(traverse right)

      {:b_generate, _anno, left, right} ->
        IO.inspect left
        IO.inspect right
        raise "Not implemented"

      # Binary
      {:bin, anno, elements} ->
        # Why flatten?
        {:"<<>>", meta(anno), List.flatten traverse(elements)}

      {:bin_element, anno, expr, :default, :default} ->
        expr
        |> traverse()
        |> with_meta(meta anno)

      {:bin_element, anno, expr, :default, tsl} ->
        quote do
          unquote(traverse expr) :: unquote(traverse_tsl tsl)
        end
        |> with_meta(meta anno)

      {:bin_element, anno, expr, size, tsl} ->
        quote do
          unquote(traverse expr) :: unquote(traverse_tsl [tsl, {:size, size}])
        end
        |> with_meta(meta anno)

      # Charlist
      {:string, _anno, list} ->
        Enum.map(list, &traverse/1)

      # List
      {:cons, anno, head, tail} ->
        tail = traverse(tail)
        head = traverse(head)
        case tail do
          # This clause is handled by the next clause
          # [{:"|", _, [left, right]}] ->
          #   quote do: [head, left | right]

          list when is_list(list) ->
            [head | tail]

          other ->
            # [unquote(head) | unquote(other)]
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

        quote do
          %{unquote_splicing items}
        end
        |> with_meta(meta anno)

      {:map, anno, original, items} ->
        #TODO optimize map generation for all map_field_exact
        items
        |> Enum.reduce(traverse(original), fn
          {:map_field_exact, _anno, key, value}, {:"%{}", meta, [{:"|", consmeta, [left, right]}]} ->
            {:"%{}", meta, [{:"|", consmeta, [left, right ++ [{traverse(key), traverse(value)}]]}]}

          {:map_field_exact, _anno, key, value}, acc ->
            quote do: %{unquote(acc) | unquote(traverse key) => unquote(traverse value)}

          {:map_field_assoc, _anno, key, value}, acc ->
            quote do: Map.put(unquote(acc), unquote(traverse key), unquote(traverse value))
        end)
        |> with_meta(meta anno)

      # Tuple
      {:tuple, _anno, [left, right]} ->
        {traverse(left), traverse(right)}

      {:tuple, anno, items} ->
        {:"{}", meta(anno), Enum.map(items, &traverse/1)}

      # Functions, funs and calls
      {:fun, anno, {:function, name, arity}} ->
        quote do: &unquote({name, meta(anno), nil})/unquote(arity)

      {:fun, anno, {:function, m, f, a}} ->
        quote do
          &unquote(traverse m).unquote(traverse f)/unquote(traverse a)
        end
        |> with_meta(meta anno)

      {:fun, anno, {:clauses, clauses}} ->
        {:fn, meta(anno), traverse(clauses)}
        # We call translation this translation chain, because actually
        # Erlang is not statically assigned. `fun()` arguments shadow
        # existing variables
        |> ElixirTranslator.to_tria!(__ENV__)
        |> SSATranslator.from_tria(pin_known: true)
        |> ElixirTranslator.from_tria()

      {:named_fun, _anno, _name, _clauses} ->
        # var = traverse name
        # fun = traverse {:fun, 0, {:clauses, clauses}}
        # quote do: unquote(var) = unquote(fun)

        raise "Not implemented"

      {:call, anno, {:remote, dotanno, module, func}, args} ->
        m = meta anno
        dotm = meta dotanno
        dot_call(traverse(module), traverse(func), traverse(args), dotm, m)

      {:call, anno, func, args} ->
        meta = meta(anno)
        case traverse(func) do
          func when is_atom(func) ->
            farity = {func, length(args)}
            cond do
              farity in Process.get(:locals, []) ->
                {func, meta, traverse(args)}

              farity in erlang_funcs() ->
                dot_call(:erlang, traverse(func), traverse(args), meta, meta)

              true ->
                {func, meta, traverse(args)}
            end

          other ->
            quote do
              (unquote(other)).(unquote_splicing traverse args)
            end
            |> with_meta(meta)
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
  defp traverse_try_clauses(clauses) when is_list(clauses) do
    {catch_clauses, rescue_clauses} =
      clauses
      |> traverse()
      |> Enum.map(fn {:"->", meta, [[pattern], body]} ->
        # inspect_ast(pattern, label: :pattern)
        case pattern do
          # Case where exception is an erlang exception wrapped into elixir's exception structure
          # Just like erlang's `undef` means the same as Elixir's `UndefinedFunctionError`
          tri {:error, exception, stacktrace} when guards ->
            exceptions = list_unwrap extract_exceptions(guards)
            rescue_pattern = [quote do: unquote(exception) in unquote(exceptions)]
            body =
              body
              |> denormalize_catch_body(exception)
              |> replace_stacktrace(stacktrace)
            {nil, {:"->", meta, [rescue_pattern, body]}}

          # Third clause for `any -> body` style exceptions
          tri {:error, exception, stacktrace} ->
            body =
              body
              |> denormalize_catch_body(exception)
              |> replace_stacktrace(stacktrace)
            {nil, {:"->", meta, [[exception], body]}}

          tri {:throw, pattern, stacktrace} ->
            body = replace_stacktrace(body, stacktrace)
            {{:"->", meta, [[pattern], body]}, nil}

          tri {kind, pattern, stacktrace} ->
            body = replace_stacktrace(body, stacktrace)
            {{:"->", meta, [[kind, pattern], body]}, nil}

          _ ->
            {{:"->", meta, [pattern, body]}, nil}
        end

      end)
      |> Enum.unzip()

    catch_clauses = Enum.reject(catch_clauses, &is_nil/1)
    rescue_clauses = Enum.reject(rescue_clauses, &is_nil/1)

    Enum.reject([rescue: rescue_clauses, catch: catch_clauses], &match?({_, []}, &1))
  end

  ### Variables

  defp traverse_variable(name, anno) do
    name
    |> Atom.to_string()
    |> String.downcase()
    |> String.split("@")
    |> case do
      ["_", context] ->
        do_traverse_variable(:_, anno, context)

      ["_" <> strname, context] ->
        do_traverse_variable(String.to_atom(strname), anno, context)

      [strname, context]
        do_traverse_variable(String.to_atom(strname), anno, context)

      ["_"] ->
        {:_, meta(anno), nil}

      [strname] ->
        {atomify_varname(strname), [], nil}
    end
  end

  defp do_traverse_variable(name, anno, context) do
    case {name, Integer.parse(context)} do
      {:_, {integer, ""}} ->
        {:underscore, [counter: integer] ++ meta(anno), nil}

      {:_, :error} ->
        {atomify_varname(context), meta(anno), :underscore}

      {_, {integer, ""}} ->
        {atomify_varname(name), [counter: integer] ++ meta(anno), nil}

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
    last = quote do: unquote(res) = unquote(last)
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
    orelse:  {Kernel,  :or},
    andalso: {Kernel,  :and},
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

  defp traverse_op(op, _anno, args) do
    %{^op => {m, f}} = @op_map
    dot_call(m, f, args)
  end

  ### Guards

  defp traverse_guard(guard) do
    guard
    |> traverse()
    |> prewalk(fn
      dot_call(Kernel, op, args) when op in @kernel_ops ->
        {op, [], args}

      other ->
        other
    end)
  end

  ### Exceptions

  defp cleanup_try({:try, meta, [parts]}) do
    parts =
      Enum.reject(parts, fn
        {_, {:__block__, [], []}} -> true
        {:after, none} when is_atom(none) or none == [] -> true
        {part, []} when part in ~w[rescue catch else]a -> true
        _ -> false
      end)

    {:try, meta, [parts]}
  end

  defp denormalize_catch_body(body, exception) do
    case body do
      # First clause, where erlang's exception is normalized
      (tri do
        x = tri dot_call(Exception, :normalize, _)
        tri_splicing(block)
      end) ->
        {:__block__, [], [tri(x = exception) | block]}

      # Second clause where we don't use exception in `rescue`, therefore
      # there is no normalisation
      other ->
        other
    end
  end

  defp replace_stacktrace(body, {name, _, context}) do
    prewalk(body, fn
      {^name, _, ^context} -> quote do: __STACKTRACE__
      other -> other
    end)
  end

  defp extract_exceptions(ast) do
    case ast do
      tri(
        :erlang.map_get(:__struct__, _) == exception
        and :erlang.map_get(:__exception__, _)
      ) ->
        [exception]

      tri(
        :erlang.map_get(:__struct__, _) == exception
        and :erlang.map_get(:__exception__, _)
        when tail
      ) ->
        [exception | extract_exceptions(tail)]

      tri(_ when tail) ->
        extract_exceptions(tail)

      _ ->
        []
    end
  end

  ### Metadata

  defp meta(line) when is_integer(line), do: [line: line]
  defp meta({line, column}) do
    [line: line, column: column]
  end
  defp meta(keyword) when is_list(keyword) do
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

  defp pin(var, _anno) when is_variable(var), do: {:"^", [], [var]}
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
end
