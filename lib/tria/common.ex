defmodule Tria.Common do

  @moduledoc """
  Like `Macro`, but for Tria. Contains useful guards, macro and functions
  """

  @special_forms ~w[
    % %{} & .  :: <<>> = ^ __CALLER__ __DIR__ __ENV__ __MODULE__
    __STACKTRACE__ __aliases__ __block__ {} alias case cond fn
    for import quote receive require super try unquote unquote_splicing with
  ]a

  # Guards

  defmacrop element(tuple, index) when is_integer(index) do
    quote do: :erlang.element(unquote(index + 1), unquote(tuple))
  end

  @doc """
  Checks if given term is Tria context
  """
  defguard is_context(context) when is_integer(context) or is_atom(context)

  @doc """
  Checks if given name, meta, context make a Tria variable
  """
  defguard is_variable(name, meta, context)
           when is_atom(name) and
                  name not in @special_forms and
                  is_list(meta) and
                  is_context(context)

  @doc """
  Checks if given AST is a Tria variable (with integer or atom in context field)
  """
  defguard is_variable(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  is_variable(element(t, 0), element(t, 1), element(t, 2))

  @doc """
  Checks if given AST is an Elixir variable (with integer in context field)
  """
  defguard is_elixir_variable(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  is_atom(element(t, 0)) and
                  element(t, 0) not in @special_forms and
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
  Checks if passed AST is a function call with dot (like Module.function)
  """
  defguard is_dot_call(t)
           # and (element(t, 0) not in @special_forms)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  (is_tuple(element(t, 0)) and
                     element(element(t, 0), 0) == :. and
                     is_list(element(element(t, 0), 1)) and
                     is_list(element(element(t, 0), 2))) and
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
    unformatted =
      if Keyword.get(opts, :with_contexts, false) do
        Macro.prewalk(ast, fn
          {name, meta, {context, counter}} when is_atom(context) and is_integer(counter) ->
            {:"#{name}_#{context}_#{counter}", meta, nil}

          {:_, meta, _} = v when is_variable(v) ->
            {:_, meta, nil}

          {name, meta, nil} ->
            {:"#{name}_nil", meta, nil}

          {name, meta, ctx} = v when is_variable(v) ->
            {:"#{name}_#{ctx}", meta, nil}

          other ->
            other
        end)
      else
        Macro.prewalk(ast, fn
          {name, meta, context} when is_integer(context) ->
            {name, meta, nil}

          other ->
            other
        end)
      end
      # Fixes the https://github.com/elixir-lang/elixir/issues/12162
      |> Macro.prewalk(fn
        {{:".", _, [:erlang, :binary_to_atom]}, _, [{:"<<>>", _, items}, :utf8]} ->
          {{:".", [], [:erlang, :binary_to_atom]}, [], [{:"<<>>", [], items}, :utf1488]}

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
  """
  @spec inspect_ast(ast :: Tria.t(), Keyword.t()) :: Macro.t()
  def inspect_ast(ast, opts \\ []) do
    string = ast_to_string(ast, opts)

    if opts[:label] do
      label = "#{opts[:label]}: "
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
      IO.puts "\n=== Failed to inspect AST ==="
      IO.inspect opts, pretty: true, limit: :infinity, label: :failed_to_inspect_opts
      IO.inspect ast, pretty: true, limit: :infinity, label: :failed_to_inspect
      IO.puts "\n"
      reraise e, __STACKTRACE__
  end

  ## MFA related stuff

  @doc """
  Public macro for MFA call AST like `Module.function(arg1, arg2, arg3)`
  """
  defmacro dot_call(module, function, args) do
    module =
      case module do
        {:__aliases__, _, _} = aliased ->
          Macro.expand(aliased, __ENV__)

        other ->
          other
      end

    if Macro.Env.in_match?(__CALLER__) do
      quote do: {{:., _, [unquote(module), unquote(function)]}, _, unquote(args)}
    else
      quote do: {{:., [], [unquote(module), unquote(function)]}, [], unquote(args)}
    end
  end

  @doc """
  Public macro for MFA-call AST like `Module.function(arg1, arg2, arg3)`
  """
  defmacro dot_call(function, args) do
    if Macro.Env.in_match?(__CALLER__) do
      quote do: {{:., _, [unquote(function)]}, _, unquote(args)}
    else
      quote do: {{:., [], [unquote(function)]}, [], unquote(args)}
    end
  end

  @doc """
  Returns full module atom from an alias
  """
  @spec unalias(Macro.t()) :: module()
  def unalias({:__aliases__, meta, names} = ast) when is_aliases(ast) do
    the_alias = meta[:alias]

    if the_alias do
      the_alias
    else
      Module.concat(names)
    end
  end
  def unalias(other), do: other

  @doc """
  Converts Module.function(args) to MFA
  """
  def arityfy({{:".", _, [module, function]}, _, arguments}) do
    arityfy {unalias(module), function, arguments}
  end

  def arityfy({module, function, arguments}) when is_list(arguments) do
    {module, function, length(arguments)}
  end

  def arityfy({module, function, arguments}) when is_integer(arguments) do
    {module, function, arguments}
  end

  ## Variables

  @doc """
  Generates N unique Tria variables
  """
  @spec gen_uniq_vars(non_neg_integer()) :: [Tria.variable()]
  def gen_uniq_vars(0), do: []
  def gen_uniq_vars(n) do
    for _i <- 1..n, do: gen_uniq_var()
  end

  @doc """
  Generates one unique Tria variable
  """
  @spec gen_uniq_var() :: Tria.variable()
  def gen_uniq_var do
    # `triau` stands for Tria Unique
    {:triau, [], gen_uniq_context()}
  end

  @doc """
  Generates unique Tria context
  """
  @spec gen_uniq_context() :: Tria.context()
  def gen_uniq_context do
    :erlang.unique_integer [:positive]
  end

  @spec unify_contexts(Macro.t(), %{atom() => atom()}) :: {Macro.t(), %{atom() => atom()}}
  def unify_contexts(ast, context_map \\ %{}) do
    Macro.prewalk(ast, context_map, fn
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

  ## Misc

  @doc """
  Checks if given AST is a special form
  """
  def is_special_form({:when, _meta, _args}), do: true
  def is_special_form({name, _meta, args}) when name in @special_forms do
    Macro.special_form?(name, length(args))
  end
  def is_special_form(_), do: false

  @doc """
  Counts size of an AST
  """
  def size_ast({op, _meta, args}), do: size_ast(op) + size_ast(args)
  def size_ast([lh | lt]), do: size_ast(lh) + size_ast(lt)
  def size_ast({left, right}), do: size_ast(left) + size_ast(right)
  def size_ast(_other), do: 1

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
  def prewalk(ast, fun) do
    element(prewalk(ast, [], fn x, _ -> {fun.(x), []} end), 0)
  end

  @doc """
  Like `Macro.prewalk/3` but for Tria
  """
  @spec prewalk(Tria.t(), any(), traverse_func()) :: {Tria.t(), any()}
  def prewalk(ast, acc, fun) do
    traverse(ast, acc, fun, fn x, a -> {x, a} end)
  end

  @doc """
  Like `Macro.postwalk/2` but for Tria
  """
  @spec postwalk(Tria.t(), (Tria.t() -> Tria.t())) :: Tria.t()
  def postwalk(ast, fun) do
    element(postwalk(ast, [], fn x, _ -> {fun.(x), []} end), 0)
  end

  @doc """
  Like `Macro.postwalk/3` but for Tria
  """
  @spec postwalk(Tria.t(), any(), traverse_func()) :: {Tria.t(), any()}
  def postwalk(ast, acc, fun) do
    traverse(ast, acc, fn x, a -> {x, a} end, fun)
  end

end
