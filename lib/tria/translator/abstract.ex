defmodule Tria.Translator.Abstract do

  @behaviour Tria.Translator

  import Tria.Common
  alias Tria.Translator.Elixir, as: ElixirTranslator

  def test(module \\ Example) do
    module
    |> fetch_abstract_code()
    |> Enum.each(fn {name, arity, clauses} ->
      # IO.inspect clauses
      clauses =
        Enum.map(clauses, fn clause ->
          traverse(clause)
        end)

      IO.inspect {name, arity}
      if name == :size, do: IO.inspect(clauses)
      {:fn, [], clauses} |> Macro.to_string |> IO.puts
      IO.puts ""
    end)

  end

  def from_tria(_ast) do
    raise "Not implemented"
  end

  def to_tria(abstract, env \\ __ENV__) do
    abstract
    |> traverse()
    |> ElixirTranslator.to_tria(env)
  end

  def fetch_abstract_code(module) do
    with(
      {^module, bin, _filename} <- :code.get_object_code(module),
      {:ok, {^module, chunks}} <- :beam_lib.chunks(bin, ~w[abstract_code locals exports]a),
      {:ok, {:raw_abstract_v1, code}} <- Keyword.fetch(chunks, :abstract_code)
    ) do
      for {:function, _, name, arity, clauses} <- code, do: {name, arity, clauses}
    end
  end

  def traverse(abstract) do
    case abstract do
      # Specials
      l when is_list(l) ->
        Enum.map(l, &traverse/1)
        
      {nil, _anno} ->
        []

      # Expressions
      {:op, _anno, op, left, right} ->
        traverse_op(op, [left, right])

      {:op, _anno, op, arg} ->
        traverse_op(op, [arg])

      {:case, _anno, arg, clauses} ->
        clauses = Enum.map(clauses, &traverse/1)
        arg = traverse(arg)
        quote do: case(unquote(arg), do: unquote(clauses))

      {:if, _anno, clauses} ->
        clauses =
          clauses
          |> traverse
          |> Enum.map(fn {:"->", _, [[{:when, _, [condition]}], body]} ->
            {:"->", [], [[condition], body]}
          end)

        quote do: cond(do: unquote(clauses))

      {:lc, _anno, body, loops} ->
        quote do: for(unquote_splicing(traverse loops), do: unquote(traverse_block body))

      {:catch, _anno, body} ->
        quote do
          try do
            unquote(traverse_block body)
          rescue
            any -> any
          catch
            any -> any
          end
        end
        
      {:block, _anno, block} ->
        {:__block__, [], Enum.map(block, &traverse/1)}

      {:match, _anno, left, right} ->
        quote do: unquote(traverse(left)) = unquote(traverse_block right)

      {:maybe_match, _anno, left, right} ->
        quote do: unquote(traverse left) <- unquote(traverse_block right)

      {:maybe, _anno, body, {:else, _anno2, elses}} ->
        {res, clauses} = add_res_to_clauses traverse body
        elses = traverse elses
        quote do: with(unquote_splicing(clauses), do: unquote(res), else: unquote(elses))

      {:maybe, _anno, body} ->
        clauses = traverse body
        {res, clauses} = add_res_to_clauses(clauses)
        quote do: with(unquote_splicing(clauses), do: unquote(res))

      {:receive, _anno, patterns} ->
        quote do
          receive do
            unquote_splicing(traverse patterns)
          end
        end

      {:receive, _anno, patterns, after_what, do_what} ->
        quote do
          receive do
            unquote_splicing(traverse patterns)
          after
            unquote(traverse after_what) -> unquote(traverse_block do_what)
          end
        end

      {:try, _anno, body, catch_clauses} ->
        quote do
          try do
            unquote(traverse body)
          catch
            unquote_splicing(traverse catch_clauses)
          end
        end

      {:try, _anno, body, clauses, catch_clauses} ->
        quote do
          try do
            case unquote(traverse body), do: unquote traverse clauses
          catch
            unquote_splicing(traverse catch_clauses)
          end
        end

      {:try, _anno, body, clauses, catch_clauses, after_body} ->
        quote do
          try(
            do: (case unquote(traverse body), do: unquote traverse clauses),
            catch: [unquote_splicing(traverse catch_clauses)],
            after: [unquote_splicing(traverse after_body)]
          )
        end

      {:generate, _anno, left, right} ->
        quote do: unquote(traverse left) <- unquote(traverse right)

      {:b_generate, _anno, left, right} ->
        IO.inspect left
        IO.inspect right
        raise "Not implemented"

      # Collections
      {:bin, _anno, elements} ->
        quote do: <<unquote_splicing List.flatten traverse elements>>

      {:bin_element, _anno, expr, :default, :default} ->
        traverse(expr)

      {:bin_element, _anno, expr, :default, tsl} ->
        quote do: unquote(traverse expr) :: unquote(traverse_tsl tsl)

      {:bin_element, _anno, expr, size, tsl} ->
        quote do: unquote(traverse expr) :: unquote(traverse_tsl [size, tsl])

      {:string, _anno, list} ->
        Enum.map(list, &traverse/1)

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

      {:map, _anno, items} ->
        items =
          Enum.map(items, fn
            {:map_field_assoc, _anno, key, value} -> {traverse(key), traverse(value)}
            {:map_field_exact, _anno, key, value} -> {quote(do: ^unquote(traverse key)), traverse(value)}
          end)

        quote do: %{unquote_splicing items}

      {:map, _anno, original, items} ->
        Enum.reduce(items, traverse(original), fn
          {:map_field_exact, _anno, key, value}, acc ->
            quote do: %{unquote(acc) | unquote(traverse key) => unquote(traverse value)}

          {:map_field_assoc, _anno, key, value}, acc ->
            quote do: Map.put(unquote(acc), unquote(traverse key), unquote(traverse value))
        end)

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
          var when is_variable(var) ->
            quote do: (unquote(var)).(unquote_splicing traverse args)

          func ->
            {traverse(func), [], traverse(args)}
        end

      # Records
      {:record, _anno, name, fields} ->
        IO.inspect name
        IO.inspect fields
        raise "Not implemented"

      {:record_field, _anno, type, name, field} ->
        IO.inspect name
        IO.inspect type
        IO.inspect field
        raise "Not implemented"

      {:record_index, _anno, name, field} ->
        IO.inspect name
        IO.inspect field
        raise "Not implemented"

      # Variable
      {:var, _anno, name} ->
        name =
          name
          |> Atom.to_string()
          |> String.downcase()
          |> String.to_atom()

        {name, [], nil}

      # Clauses
      {:clause, _anno, left, [], right} ->
        {:"->", [], [traverse(left), traverse_block(right)]}

      {:clause, _anno, left, guards, right} ->
        guard =
          join(guards, :or, fn guard_tests ->
            join(guard_tests, :and, &traverse/1)
          end)

        {:"->", [], [
          [{:when, [], traverse(left) ++ [guard]}],
          traverse_block(right)
        ]}

      # Literals and atoms
      {k, _anno, atom} when k in ~w[float integer atom char]a ->
        atom

      literal when is_literal(literal) ->
        literal

      other ->
        IO.inspect other
        raise "Not implemented"
    end
  end

  defp traverse_block([line]), do: traverse(line)
  defp traverse_block(lines) when is_list(lines), do: {:__block__, [], traverse(lines)}
  defp traverse_block(line), do: traverse(line)

  defp traverse_tsl(tsl) when is_list(tsl) do
    join(tsl, :"-", &traverse_tsl/1)
  end
  defp traverse_tsl({type, size}), do: {type, [], [size]}
  defp traverse_tsl(type) when is_atom(type), do: {type, [], nil}
  defp traverse_tsl(size) when is_integer(size), do: size

  defp add_res_to_clauses(clauses) do
    [last | rev_tail] = Enum.reverse(clauses)
    res = {:res, [], Tria.Common.gen_uniq_context()}
    last = quote do: unquote(res) = unquote(last)
    {res, Enum.reverse([last | rev_tail])}
  end

  @op_map %{
    ==: {Kernel, :==},
    "/=": {Kernel, :!=},
    "=<": {Kernel, :<=},
    >=: {Kernel, :>=},
    <: {Kernel, :<},
    >: {Kernel, :>},
    "=:=": {Kernel, :===},
    "=/=": {Kernel, :!==},
    +: {Kernel, :+},
    -: {Kernel, :-},
    *: {Kernel, :*},
    /: {Kernel, :/},
    div: {Kernel, :div},
    rem: {Kernel, :rem},
    not: {Kernel, :not},
    orelse: {Kernel, :or},
    andalso: {Kernel, :and},
    and: {:erlang, :and},
    or: {:erlang, :or},
    xor: {:erlang, :xor},
    ++: {Kernel, :++},
    --: {Kernel, :--},
    !: {Kernel, :send},
    band: {Bitwise, :&&&},
    bor: {Bitwise, :|||},
    bxor: {Bitwise, :^^^},
    bsl: {Bitwise, :<<<},
    bsr: {Bitwise, :>>>},
    bnot: {Bitwise, :~~~},
  }

  defp traverse_op(op, args) do
    %{^op => {m, f}} = @op_map
    dot_call(m, f, traverse args)
  end

  defp join([head], _op, f), do: f.(head)
  defp join([head | tail], op, f) do
    {op, [], [f.(head), join(tail, op, f)]}
  end

end
