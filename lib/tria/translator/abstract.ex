defmodule Tria.Translator.Abstract do

  @moduledoc """
  Erlang ATF to Tria translator
  It is designed to translate ATF provided in `abstract_code` chunk
  therefore it is unable to translate records, macros and parsetransforms

  #TODO preserve `anno`
  """

  @behaviour Tria.Translator

  import Tria.Common
  import Tria.Tri
  alias Tria.Codebase
  alias Tria.Translator.Elixir, as: ElixirTranslator

  def from_tria(_ast) do
    raise "Not implemented"
  end

  def to_tria!(abstract, env \\ __ENV__) do
    {:ok, tria, _} = to_tria(abstract, env)
    tria
  end

  def to_tria(abstract, env \\ __ENV__) do
    abstract
    |> traverse()
    |> elixir_to_tria(env)
  end

  defp elixir_to_tria([{:"->", _, _} | _] = clauses, env) do
    with {:ok, {:fn, _, clauses}, env} <- ElixirTranslator.to_tria({:fn, [], clauses}, env) do
      {:ok, clauses, env}
    end
  end
  defp elixir_to_tria(other, env) do
    ElixirTranslator.to_tria(other, env)
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
      {:if, _anno, clauses} ->
        #TODO needs testing
        clauses =
          clauses
          |> traverse()
          |> Enum.map(fn {:"->", _, [[{:when, _, [condition]}], body]} ->
            {:"->", [], [[condition], body]}
          end)

        quote do: cond(do: unquote(clauses))

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
      {:receive, _anno, clauses} ->
        quote do
          receive do
            unquote traverse clauses
          end
        end

      {:receive, _anno, [], after_what, do_what} ->
        quote do
          receive after: (unquote(traverse after_what) -> unquote(traverse_block do_what))
        end

      {:receive, _anno, clauses, after_what, do_what} ->
        quote do
          receive do
            unquote traverse clauses
          after
            unquote(traverse after_what) -> unquote(traverse_block do_what)
          end
        end

      # Try
      {:try, _anno, body, catch_clauses} ->
        quote do
          try do
            unquote traverse_block body
          catch
            unquote traverse catch_clauses
          end
        end
        |> cleanup_try()

      {:try, _anno, body, else_clauses, catch_clauses} ->
        quote do
          try do
            unquote traverse_block body
          else
            unquote traverse else_clauses
          catch
            unquote traverse catch_clauses
          end
        end
        |> cleanup_try()

      {:try, _anno, body, else_clauses, catch_clauses, after_body} ->
        quote do
          try do
            unquote traverse_block body
          else
            unquote traverse else_clauses
          catch
            unquote traverse catch_clauses
          after
            unquote traverse_block after_body
          end
        end
        |> cleanup_try()

      {:catch, _anno, body} ->
        # TODO maybe this is wrong
        quote do
          try do
            unquote traverse_block body
          rescue
            any -> any
          catch
            any -> any
          end
        end
        |> cleanup_try()

      {:generate, _anno, left, right} ->
        quote do: unquote(traverse left) <- unquote(traverse right)

      {:b_generate, _anno, left, right} ->
        IO.inspect left
        IO.inspect right
        raise "Not implemented"

      # Binary
      {:bin, _anno, elements} ->
        quote do: <<unquote_splicing List.flatten traverse elements>>

      {:bin_element, _anno, expr, :default, :default} ->
        traverse(expr)

      {:bin_element, _anno, expr, :default, tsl} ->
        quote do: unquote(traverse expr) :: unquote(traverse_tsl tsl)

      {:bin_element, _anno, expr, size, tsl} ->
        quote do: unquote(traverse expr) :: unquote(traverse_tsl [tsl, {:size, size}])

      # Charlist
      {:string, _anno, list} ->
        Enum.map(list, &traverse/1)

      # List
      {:cons, _anno, head, tail} ->
        tail = traverse(tail)
        head = traverse(head)
        case tail do
          # This clause is handled by the next clause
          # [{:"|", _, [left, right]}] ->
          #   quote do: [head, left | right]
          list when is_list(list) ->
            [head | tail]

          other ->
            quote do: [unquote(head) | unquote(other)]
        end

      # Map
      {:map, _anno, items} ->
        items =
          Enum.map(items, fn
            {:map_field_assoc, _anno, key, value} ->
              {traverse(key), traverse(value)}

            {:map_field_exact,  anno, key, value} ->
              key = pin(traverse(key), anno)
              {key, traverse(value)}
          end)

        quote do: %{unquote_splicing items}

      {:map, _anno, original, items} ->
        Enum.reduce(items, traverse(original), fn
          {:map_field_exact, _anno, key, value}, acc ->
            quote do: %{unquote(acc) | unquote(traverse key) => unquote(traverse value)}

          {:map_field_assoc, _anno, key, value}, acc ->
            quote do: Map.put(unquote(acc), unquote(traverse key), unquote(traverse value))
        end)

      # Tuple
      {:tuple, _anno, items} ->
        quote do: {unquote_splicing Enum.map(items, &traverse/1)}

      # Functions, funs and calls
      {:fun, _anno, {:function, name, arity}} ->
        quote do: &unquote({name, [], nil})/unquote(arity)

      {:fun, _anno, {:function, m, f, a}} ->
        quote do: &unquote(traverse m).unquote(traverse f)/unquote(traverse a)

      {:fun, _anno, {:clauses, clauses}} ->
        {:fn, [], traverse clauses}

      {:named_fun, _anno, name, clauses} ->
        var = traverse name
        fun = traverse {:fun, 0, {:clauses, clauses}}
        quote do: unquote(var) = unquote(fun)

      {:call, _anno, {:remote, _anno2, module, func}, args} ->
        dot_call(traverse(module), traverse(func), traverse(args))

      {:call, _anno, func, args} ->
        case traverse(func) do
          func when is_atom(func) ->
            if {func, length(args)} in erlang_funcs() do
              dot_call(:erlang, traverse(func), traverse(args))
            else
              {func, [], traverse(args)}
            end

          other ->
            quote do: (unquote(other)).(unquote_splicing traverse args)
        end

      # Variable
      {:var, anno, name} ->
        traverse_variable(name, anno)

      # Clauses
      {:clause, _anno, left, [], right} ->
        {:"->", [], [traverse(left), traverse_block(right)]}

      {:clause, _anno, left, guards, right} ->
        guard = traverse_guards(guards)
        left = traverse(left)

        {:"->", [], [
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
    res = {:res, [], Tria.Common.gen_uniq_context()}
    last = quote do: unquote(res) = unquote(last)
    {res, Enum.reverse([last | rev_tail])}
  end

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

  defp traverse_op(op, _anno, args) do
    %{^op => {m, f}} = @op_map
    dot_call(m, f, args)
  end

  @kernel_ops for {_, {Kernel, op}} <- @op_map, do: op

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

  # Helpers

  defp join([head], _op, f), do: f.(head)
  defp join([head | tail], op, f) do
    {op, [], [f.(head), join(tail, op, f)]}
  end

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

  defp pin(var, _anno) when is_variable(var), do: {:"^", [], [var]}
  defp pin(other, _anno), do: other

  defp cleanup_try({:try, meta, [parts]}) do
    parts =
      parts
      |> catch_to_rescue()
      |> Enum.reject(fn
        {_, {:__block__, [], []}} -> true
        {:after, none} when is_atom(none) or none == [] -> true
        {part, []} when part in ~w[rescue catch else]a -> true
        _ -> false
      end)

    {:try, meta, [parts]}
  end

  # try/rescue actually generates `catch` clauses.
  # But these clauses can't be transpiled back to Elixir straight away
  # Because there is some magic goin on inside `try`
  #
  # So, this functions translates these `catch` clauses back to rescue
  # There are 3 different cases which are translated differently into Elixir errors
  defp catch_to_rescue(try_parts) do
    case Keyword.pop(try_parts, :catch, []) do
      {[], parts} -> parts
      {catch_clauses, parts} ->
        {catch_clauses, rescue_clauses} =
          catch_clauses
          |> Enum.map(fn {:"->", meta, [pattern, body]} ->
            case pattern do
              # Case where exception is an erlang exception wrapped into elixir's exception structure
              # Just like erlang's `undef` means the same as Elixir's `UndefinedFunctionError`
              [tri do
                {:error, exception, stacktrace}
                  when :erlang.map_get(:__struct__, exception) == error
                  and :erlang.map_get(:__exception__, exception)
                  when exception == _erlang_error
              end] ->
                rescue_pattern = [quote do: unquote(exception) in unquote(error)]
                body =
                  body
                  |> denormalize_catch_body(exception)
                  |> replace_stacktrace(stacktrace)
                {nil, {:"->", meta, [rescue_pattern, body]}}

              # Second clause which hanldes code generated from Elixir-native exceptions
              [tri do
                {:error, exception, stacktrace}
                  when :erlang.map_get(:__struct__, exception) == error
                  and :erlang.map_get(:__exception__, exception)
              end] ->
                # Body normalisation is not required for Elixir-only exceptions
                rescue_pattern = [quote do: unquote(exception) in unquote(error)]
                body = replace_stacktrace(body, stacktrace)
                {nil, {:"->", meta, [rescue_pattern, body]}}

              # Third clause for `any -> body` style exceptions
              [tri do
                {:error, exception, stacktrace}
              end] ->
                body =
                  body
                  |> denormalize_catch_body(exception)
                  |> replace_stacktrace(stacktrace)
                {nil, {:"->", meta, [[exception], body]}}

              _ ->
                {{:"->", meta, [pattern, body]}, nil}
            end

          end)
          |> Enum.unzip()

        catch_clauses = Enum.reject(catch_clauses, &is_nil/1)
        rescue_clauses = Enum.reject(rescue_clauses, &is_nil/1)

        # We always want `do` clause to be the first one
        {dobody, parts} = Keyword.pop!(parts, :do)

        parts
        |> Keyword.put(:catch, catch_clauses)
        |> Keyword.put(:rescue, rescue_clauses)
        |> Keyword.put(:do, dobody)
    end
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

  defp keyword_update_new(keyword, key, func) do
    case Keyword.has_key?(keyword, key) do
      true -> Keyword.update!(keyword, key, func)
      false -> keyword
    end
  end

end
