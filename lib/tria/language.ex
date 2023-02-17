defmodule Tria.Language do

  @moduledoc """
  Like `Macro`, but for Tria. Contains useful guards, macro and functions
  """

  @doc """
  Function which creates clean macro env
  """
  @spec empty_env(module() | nil, Path.t()) :: Macro.Env.t()
  def empty_env(module, file \\ "nofile") do
    %Macro.Env{__ENV__ |
      context: nil,
      versioned_vars: %{},
      module: module,
      file: file,
      function: nil,
      line: 0
    }
  end

  alias Tria.Debug
  import Tria.Language.Binary

  # These special forms are variables, but they can be assigned to, only used
  @special_vars ~w[__CALLER__ __DIR__ __ENV__ __MODULE__ __STACKTRACE__]a

  # These are special forms which can be valid variable names
  # @special_forms_valid ~w[
  #   alias case cond for import quote receive
  #   super try unquote unquote_splicing with
  # ]a

  # These are special forms and things that are not allowed to be a variable name
  @special_forms_invalid ~w[% %{} & .  :: <<>> = ^ __aliases__ __block__ {} fn]a

  # Plus reserved words which are not special forms
  @reserved_words ~w[rescue after catch do when end]a

  # Guards

  # Helper for zero-based tuple indexing
  defmacrop element(tuple, index) when is_integer(index) do
    quote do: :erlang.element(unquote(index + 1), unquote(tuple))
  end

  @doc """
  Checks if the given term can be escaped and represented in AST
  """
  defguard is_escapable(x) when not (is_port(x) or is_pid(x) or is_function(x))

  @doc """
  Checks if it is tuple with size 3
  """
  defguard is_triple(tuple) when is_tuple(tuple) and tuple_size(tuple) == 3

  @doc """
  Checks if atom is reserved and can't be used as a variable or function name
  """
  defguard is_reserved(name)
           when name in @reserved_words or
                name in @special_forms_invalid or
                name in @special_vars

  @doc """
  Checks if given term is Tria context
  """
  defguard is_context(context) when is_integer(context) or is_atom(context)

  @doc """
  Checks if given name, meta, context make a Tria variable
  """
  defguard is_variable(name, meta, context)
           when is_atom(name) and
                  # name not in @special_forms_invalid and
                  # name not in @reserved_words and
                  name not in @special_vars and
                  is_list(meta) and
                  is_context(context)

  @doc """
  Checks if given AST is a Tria variable (with integer or atom in context field)
  """
  defguard is_variable(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  is_variable(element(t, 0), element(t, 1), element(t, 2))

  defguard same_variable?(left, right)
           when is_variable(left) and is_variable(right)
           and element(left, 0) == element(right, 0)
           and element(left, 2) == element(right, 2)

  @doc """
  Checks if given AST is a Tria variable (with integer or atom in context field)
  With the specific `name`
  """
  defguard is_variable(t, name)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  element(t, 0) == name and
                  is_variable(element(t, 0), element(t, 1), element(t, 2))

  @doc """
  Checks if given AST is a special Elixir variable
  """
  defguard is_special_variable(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  is_atom(element(t, 0)) and
                  element(t, 0) in @special_vars and
                  is_list(element(t, 1)) and
                  is_atom(element(t, 2))

  @doc """
  Checks is AST is `__CALLER__`
  """
  defguard is_CALLER(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  element(t, 0) == :__CALLER__ and
                  is_list(element(t, 1)) and
                  is_atom(element(t, 2))

  @doc """
  Checks if given AST is an Elixir variable (with integer in context field)
  """
  defguard is_elixir_variable(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  is_atom(element(t, 0)) and
                  # element(t, 0) not in @special_forms_invalid and
                  element(t, 0) not in @special_vars and
                  is_list(element(t, 1)) and
                  is_atom(element(t, 2))

  @doc """
  Checks if given AST is a pin
  """
  defguard is_pinned(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  element(t, 0) == :^ and
                  is_list(element(t, 1)) and
                  is_list(element(t, 2))

  @doc """
  Checks if given AST is an :__aliases__
  """
  defguard is_aliases(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  element(t, 0) == :__aliases__ and
                  is_list(element(t, 1)) and
                  is_list(element(t, 2))

  @doc """
  Checks if passed AST is a local/imported function call or a special form
  """
  defguard is_call(t)
           # and (element(t, 0) not in @special_forms)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  is_atom(element(t, 0)) and
                  is_list(element(t, 1)) and
                  is_list(element(t, 2))

  @doc """
  Checks if passed AST is a literal (this means a value which represents itself in the AST)
  """
  defguard is_literal(l) when is_atom(l) or is_number(l) or is_binary(l)

  @doc """
  Checks if passed AST is a `fn` closure
  """
  defguard is_fn(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  element(t, 0) == :fn and
                  is_list(element(t, 1)) and
                  is_list(element(t, 2))

  @doc """
  Checks is passed AST is a collection or something
  """
  defguard is_collection(c)
           when (is_tuple(c) and (tuple_size(c) == 2 or
             (tuple_size(c) == 3 and element(c, 0) in ~w[{} %{}]a)))
           or is_list(c)

  ## Debug

  @doc """
  Converts Tria AST to string
  """
  @spec ast_to_string(Tria.t(), Keyword.t()) :: String.t()
  def ast_to_string(ast, opts \\ []) do
    highlight_line = Keyword.get(opts, :highlight_line, :no_highlight)

    unformatted =
      if Keyword.get(opts, :with_contexts, true) do
        prewalk(ast, fn
          {name, meta, {context, counter}} when is_atom(context) and is_integer(counter) ->
            {:"#{name}_#{context}_#{counter}", meta, nil}

          {:_, meta, _} = v when is_variable(v) ->
            {:_, meta, nil}

          {name, meta, ctx} = v when is_variable(v) ->
            ctx_str = ctx && to_string(ctx) || "nil"
            case meta[:counter] do
              {ctx, counter} ->
                {:"#{name}_#{ctx_str}_#{ctx}_#{counter}", meta, nil}

              nil ->
                {:"#{name}_#{ctx_str}", meta, nil}

              counter ->
                {:"#{name}_#{ctx_str}_#{counter}", meta, nil}
            end

          other ->
            other
        end)
      else
        prewalk(ast, fn
          {name, _, context} when is_integer(context) ->
            {name, [], nil}

          other ->
            other
        end)
      end
      # https://github.com/elixir-lang/elixir/issues/12162
      # https://github.com/elixir-lang/elixir/issues/12248
      |> prewalk(fn
        {{:".", _, [:erlang, :binary_to_atom]}, _, [{:"<<>>", _, items}, :utf8]} ->
          {{:".", [], [:erlang, :binary_to_atom]}, [], [{:"<<>>", [], items}, :utf1488]}

        {{:".", _, [List, :to_charlist]}, _, args} ->
          {{:".", [], [{:__aliases__, [], [:List]}, :to_charlist]}, [], args}

        other ->
          other
      end)
      |> Macro.postwalk(fn
        {_op, meta, _ctx} = x ->
          if meta[:line] == highlight_line do
            {:"HERE_HITS!", [], [x]}
          else
            x
          end

        other ->
          other
      end)
      |> Macro.to_string()

    try do
      Code.format_string!(unformatted)
    rescue
      _ -> unformatted
    end
  end

  @doc """
  Like IO.inspect/1 but for Elixir's AST
  Supports `label` option

  ## Options

  - `:with_contexts` - display contexts next to variables
  - `:label` - inspect with label
  - `:highlight_line` - wraps given line into `HERE_HITS()` pseudo call
  """
  @spec inspect_ast(ast :: Tria.t(), Keyword.t()) :: Macro.t()
  def inspect_ast(ast, opts \\ []) do
    {label, opts} = Keyword.pop(opts, :label)
    string = ast_to_string(ast, opts)

    if label do
      label = "#{label}: "
      label_length = length String.graphemes label
      tab = for _ <- 1..label_length, do: " ", into: ""

      "#{label}#{string}"
      |> String.replace("\n", "\n" <> tab)
      |> IO.puts()
    else
      IO.puts string
    end

    ast
  rescue
    e ->
      Debug.puts "\n=== Failed to inspect AST ==="
      Debug.inspect opts, pretty: true, limit: :infinity, label: :failed_to_inspect_opts
      Debug.inspect ast, pretty: true, limit: :infinity, label: :failed_to_inspect
      Debug.puts "\n"
      reraise e, __STACKTRACE__
  end

  ## MFA related stuff

  @doc """
  Public macro for MFA call AST like `Module.function(arg1, arg2, arg3)`
  """
  defmacro dot_call(module, function, args, dotmeta \\ nil, callmeta \\ nil) do
    module =
      case module do
        {:__aliases__, _, _} = aliased ->
          Macro.expand(aliased, __ENV__)

        other ->
          other
      end

    dotmeta  = cmeta(__CALLER__, dotmeta)
    callmeta = cmeta(__CALLER__, callmeta)

    quote do: {{:., unquote(dotmeta), [unquote(module), unquote(function)]}, unquote(callmeta), unquote(args)}
  end

  @doc """
  Public macro for MFA-call AST like `Module.function(arg1, arg2, arg3)`
  """
  defmacro dot_call(function, args) do
    quote do
      {{:., unquote(cmeta __CALLER__), [unquote(function)]}, unquote(cmeta __CALLER__), unquote(args)}
    end
  end

  @doc """
  Macro for pinning variables
  """
  defmacro pin(value, meta \\ nil) do
    quote do: {:^, unquote(cmeta(__CALLER__, meta)), [unquote value]}
  end

  # C-Meta -- context-aware meta
  defp cmeta(env, meta \\ nil)
  defp cmeta(%{context: :match}, nil), do: {:_, [], Elixir}
  defp cmeta(%{context: _}, nil), do: []
  defp cmeta(_, value), do: value

  ## Variables

  @doc """
  Generates N unique Tria variables
  """
  @spec gen_uniq_vars(non_neg_integer()) :: [Tria.variable()]
  def gen_uniq_vars(0), do: []
  def gen_uniq_vars(n) do
    [gen_uniq_var() | gen_uniq_vars(n - 1)]
  end

  @doc """
  Generates one unique Tria variable
  """
  @spec gen_uniq_var() :: Tria.variable()
  def gen_uniq_var do
    {:tria_unique, [], gen_uniq_context()}
  end

  @doc """
  Generates unique Tria context
  """
  @spec gen_uniq_context() :: Tria.context()
  def gen_uniq_context do
    :erlang.unique_integer [:positive]
  end

  @spec unify_contexts(Tria.t()) :: Tria.t()
  def unify_contexts(ast) do
    {ast, _} = unify_contexts(ast, %{})
    ast
  end

  @spec unify_contexts(Tria.t(), %{Tria.context() => Tria.context()}) :: {Tria.t(), %{Tria.context() => Tria.context()}}
  def unify_contexts(ast, context_map) do
    prewalk(ast, context_map, fn
      {name, meta, context} = a, context_map when is_variable(a) ->
        case context_map do
          %{^context => new_context} ->
            {{name, meta, new_context}, context_map}

          _ ->
            new_context = gen_uniq_context()
            {{name, meta, new_context}, Map.put(context_map, context, new_context)}
        end

      ast, context_map ->
        {ast, context_map}
    end)
  end

  ## Checks

  @doc """
  Checks if given AST is a special form
  """
  @spec is_special_form(Tria.t()) :: boolean()
  def is_special_form({:when, _meta, _args}), do: true
  def is_special_form({name, _, _}) when name in @special_forms_invalid, do: true
  def is_special_form({name, _, _}) when name in @reserved_words, do: true
  def is_special_form({name, _meta, args}) when is_atom(name) do
    Macro.special_form?(name, length(args))
  end
  def is_special_form(_), do: false

  @doc """
  Counts **approximate** size of an AST
  """
  @spec size_ast(Tria.t()) :: pos_integer()
  def size_ast({op, _meta, args}), do: size_ast(op) + size_ast(args)
  def size_ast([lh | lt]), do: size_ast(lh) + size_ast(lt)
  def size_ast({left, right}), do: size_ast(left) + size_ast(right)
  def size_ast(other), do: :erts_debug.size(other)

  @spec quoted_literal?(Tria.t()) :: boolean()
  def quoted_literal?({:%{}, _, args}), do: quoted_literal?(args)
  def quoted_literal?({:{}, _, args}), do: quoted_literal?(args)
  def quoted_literal?({:<<>>, _, _} = binary) do
    binary
    |> traverse_binary_inputs(true, fn ast, acc -> {ast, acc and quoted_literal? ast} end)
    |> elem(1)
  end
  def quoted_literal?({left, right}), do: quoted_literal?(left) and quoted_literal?(right)
  def quoted_literal?(list) when is_list(list), do: :lists.all(&quoted_literal?/1, list)
  def quoted_literal?(term), do: is_atom(term) or is_number(term) or is_binary(term)

  @doc """
  Like `Macro.quoted_literal?` but also accounts variables in the structure
  """
  @spec vared_literal?(Tria.t()) :: boolean()
  def vared_literal?(ast) do
    case ast do
      [head | tail] ->
        vared_literal?(head) and vared_literal?(tail)

      {left, right} ->
        vared_literal?(left) and vared_literal?(right)

      vl when is_variable(vl) or is_literal(vl) ->
        true

      {:%{}, _, [{:|, _, [_, _]}]} ->
        # Because this construction actually raises
        false

      {s, _, children} when s in ~w[%{} {} |]a ->
        vared_literal?(children)

      {:<<>>, _, _} = binary ->
        binary
        |> traverse_binary_inputs(true, fn ast, acc -> {ast, acc and vared_literal? ast} end)
        |> elem(1)

      [] ->
        true

      _ ->
        false
    end
  end

  ## AST Traversal

  @type traverse_func :: (Tria.t(), any() -> {Tria.t(), any})

  @doc """
  Like `Macro.traverse/4` but for Tria
  """
  @spec traverse(Tria.t(), any(), traverse_func(), traverse_func()) :: {Tria.t(), any()}
  def traverse(ast, acc, pre, post) when is_function(pre, 2) and is_function(post, 2) do
    {ast, acc} = pre.(ast, acc)
    do_traverse(ast, acc, pre, post)
  end

  defp do_traverse({form, meta, args}, acc, pre, post) when is_atom(form) do
    {args, acc} = do_traverse_children(args, acc, pre, post)
    post.({form, meta, args}, acc)
  end

  defp do_traverse({form, meta, args}, acc, pre, post) do
    {form, acc} = pre.(form, acc)
    {form, acc} = do_traverse(form, acc, pre, post)
    {args, acc} = do_traverse_children(args, acc, pre, post)
    post.({form, meta, args}, acc)
  end

  defp do_traverse({left, right}, acc, pre, post) do
    {left, acc} = pre.(left, acc)
    {left, acc} = do_traverse(left, acc, pre, post)
    {right, acc} = pre.(right, acc)
    {right, acc} = do_traverse(right, acc, pre, post)
    post.({left, right}, acc)
  end

  defp do_traverse(list, acc, pre, post) when is_list(list) do
    {list, acc} = do_traverse_children(list, acc, pre, post)
    post.(list, acc)
  end

  defp do_traverse(x, acc, _pre, post) do
    post.(x, acc)
  end

  defp do_traverse_children(ctx, acc, _pre, _post) when is_context(ctx), do: {ctx, acc}
  defp do_traverse_children(args, acc, pre, post) when is_list(args) do
    :lists.mapfoldl(
      fn x, acc ->
        {x, acc} = pre.(x, acc)
        do_traverse(x, acc, pre, post)
      end,
      acc,
      args
    )
  end

  @doc """
  Like `Macro.prewalk/2` but for Tria
  """
  @spec prewalk(Tria.t(), (Tria.t() -> Tria.t())) :: Tria.t()
  def prewalk(ast, func) do
    element(prewalk(ast, [], fn x, _ -> {func.(x), []} end), 0)
  end

  @doc """
  Like `Macro.prewalk/3` but for Tria
  """
  @spec prewalk(Tria.t(), any(), traverse_func()) :: {Tria.t(), any()}
  def prewalk(ast, acc, func) do
    traverse(ast, acc, func, fn x, a -> {x, a} end)
  end

  @doc """
  Like `Macro.postwalk/2` but for Tria
  """
  @spec postwalk(Tria.t(), (Tria.t() -> Tria.t())) :: Tria.t()
  def postwalk(ast, func) do
    element(postwalk(ast, [], fn x, _ -> {func.(x), []} end), 0)
  end

  @doc """
  Like `Macro.postwalk/3` but for Tria
  """
  @spec postwalk(Tria.t(), any(), traverse_func()) :: {Tria.t(), any()}
  def postwalk(ast, acc, func) do
    traverse(ast, acc, fn x, a -> {x, a} end, func)
  end

  @type context :: nil | :guard | :match

  @doc """
  Prewalks the context
  """
  @spec context_prewalk(Tria.t(), (Tria.t(), context() -> Tria.t()), context()) :: Tria.t()
  def context_prewalk(ast, func, context) do
    {ast, _} = context_prewalk(ast, [], fn ast, _acc, ctx -> {func.(ast, ctx), []} end, context)
    ast
  end

  @spec context_prewalk(Tria.t(), acc, (Tria.t(), acc, context() -> {Tria.t(), acc}), context()) :: {Tria.t(), acc}
        when acc: any()
  def context_prewalk(ast, acc, func, context)
  def context_prewalk(ast, acc, func, nil) do
    {ast, acc} = func.(ast, acc, nil)
    case ast do
      {:"->", m, [[{:when, mw, pattern_and_guard}], body]} ->
        {guard, pattern} = List.pop_at(pattern_and_guard, -1)
        {pattern, acc} = context_prewalk(pattern, acc, func, :match)
        {guard, acc} = context_prewalk(guard, acc, func, :guard)
        {body, acc} = context_prewalk(body, acc, func, nil)
        { {:"->", m, [[{:when, mw, pattern ++ [guard]}], body]}, acc }

      {atom, m, [pattern, body]} when atom in ~w[-> = <-]a ->
        {pattern, acc} = context_prewalk(pattern, acc, func, :match)
        {body, acc} = context_prewalk(body, acc, func, nil)
        { {atom, m, [pattern, body]}, acc }

      {left, right} ->
        {left, acc} = context_prewalk(left, acc, func, nil)
        {right, acc} = context_prewalk(right, acc, func, nil)
        { {left, right}, acc }

      [head | tail] ->
        {head, acc} = context_prewalk(head, acc, func, nil)
        {tail, acc} = context_prewalk(tail, acc, func, nil)
        { [head | tail], acc }

      {node, m, children} ->
        {node, acc} = context_prewalk(node, acc, func, nil)
        {children, acc} = context_prewalk(children, acc, func, nil)
        { {node, m, children}, acc }

      other ->
        { other, acc }
    end
  end
  def context_prewalk(ast, acc, func, :match) do
    prewalk(ast, acc, & func.(&1, &2, :match))
  end
  def context_prewalk(ast, acc, func, :guard) do
    prewalk(ast, acc, & func.(&1, &2, :guard))
  end

  @spec findwalk(Tria.t(), (Tria.t() -> boolean())) :: boolean()
  def findwalk(ast, predicate) do
    postwalk(ast, fn ast ->
      if predicate.(ast), do: throw(:found), else: ast
    end)
    false
  catch
    :found -> true
  end

  @spec filterwalk(Tria.t(), (Tria.t() -> boolean())) :: [Tria.t()]
  def filterwalk(ast, predicate) do
    {_, filtered} =
      postwalk(ast, [], fn ast, acc ->
        if predicate.(ast), do: {ast, [ast | acc]}, else: {ast, acc}
      end)

    filtered
  end

  ## Other non-Tria related helpers
  #TODO think of a better place for them

  @spec with_pdict(map | [{any(), any()}], (() -> result :: any())) :: result :: any()
  def with_pdict(opts, func) do
    olds = for {key, value} <- opts, do: {key, Process.put(key, value)}

    try do
      func.()
    after
      Enum.each(olds, fn
        {key, nil} -> Process.delete(key)
        {key, old} -> Process.put(key, old)
      end)
    end
  end

end
