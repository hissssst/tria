defmodule Tria.Common do

  @special_forms ~w[
    % %{} & .  :: <<>> = ^ __CALLER__ __DIR__ __ENV__ __MODULE__
    __STACKTRACE__ __aliases__ __block__ {} alias case cond fn
    for import quote receive require super try unquote unquote_splicing with
  ]a

  # Guards

  defmacrop element(tuple, index) when is_integer(index) do
    index = index + 1

    quote do
      :erlang.element(unquote(index), unquote(tuple))
    end
  end

  defguard is_macro_ast(t)
           when is_list(t) or
                  (is_tuple(t) and
                     tuple_size(t) == 3 and
                     (is_atom(element(t, 0)) or
                        (is_tuple(element(t, 0)) and
                           is_atom(element(element(t, 0), 0)) and
                           is_list(element(element(t, 0), 1)) and
                           is_list(element(element(t, 0), 2)))) and
                     is_list(element(t, 1)))

  @doc "Checks if given AST is a variable"
  defguard is_variable(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  is_atom(element(t, 0)) and
                  element(t, 0) not in @special_forms and
                  is_list(element(t, 1)) and
                  is_atom(element(t, 2))

  @doc "Checks if given AST is a pinned variable"
  defguard is_pinned(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  element(t, 0) == :^ and
                  is_list(element(t, 1)) and
                  is_variable(element(t, 2))

  @doc "Checks if given AST is an :__aliases__"
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
  defguard is_literal(l) when is_atom(l) or is_number(l) or is_pid(l) or is_reference(l) or
    is_port(l)

  @doc """
  Checks if passed AST is a `fn` closure
  """
  defguard is_fn(t)
           when is_tuple(t) and
                  tuple_size(t) == 3 and
                  element(t, 0) == :fn and
                  is_list(element(t, 1)) and
                  is_list(element(t, 2))

  # Debug

  @doc """
  Like IO.inspect/1 but for Elixir's AST
  Supports `label` option
  """
  @spec inspect_ast(ast :: Macro.t(), Keyword.t()) :: Macro.t()
  def inspect_ast(ast, opts \\ []) do
    string =
      try do
        ast
        |> then(fn ast ->
          if Keyword.get(opts, :with_contexts, false) do
            Macro.prewalk(ast, fn
              {name, meta, ctx} = v when is_variable(v) and (name == :_ or ctx == nil) ->
                {name, meta, ctx}

              {name, meta, ctx} = v when is_variable(v) ->
                {:"#{name}_#{ctx}", meta, ctx}

              other ->
                other
            end)
          else
            ast
          end
        end)
        |> Macro.to_string()
        |> Code.format_string!()
      rescue
        _ -> Macro.to_string(ast)
      end

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


  @doc "Checks if given AST is a Module.function(args, ...) call"
  @spec is_mfa(ast :: Macro.t()) :: boolean()
  def is_mfa({{:., _, [{:__aliases__, _, m}, _]}, _, _}) when is_list(m), do: true
  def is_mfa(_), do: false

  @doc "Converts mfa-call to mfa"
  @spec call_to_mfa(ast :: Macro.t()) :: {module(), atom(), list()}
  def call_to_mfa({:., _, [m, f], _, a} = ast) do
    true = is_mfa(ast)
    {unalias(m), f, a}
  end

  def call_to_mfa({f, _, a} = ast) do
    if not is_special_form(ast) do
      {f, a}
    end
  end

  @doc "Returns full module atom from alias"
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

  @doc "Generates N unique variables"
  @spec gen_uniq_vars(non_neg_integer()) :: [Tria.variable()]
  def gen_uniq_vars(n) do
    gen_uniq_vars(n, gen_uniq_context())
  end

  @doc "Generates unique context"
  @spec gen_uniq_context() :: atom()
  def gen_uniq_context do
    # Just to make debugging prettier
    if function_exported?(Mix, :env, 0) and Mix.env() in [:dev, :test] do
      :"c#{:erlang.unique_integer([:positive])}"
    else
      :"#{:erlang.unique_integer([:positive])}_tria_generated_context"
    end
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

  @spec purge_meta(Macro.t()) :: Macro.t()
  def purge_meta({call, _meta, args}), do: {call, [], args}
  def purge_meta(other), do: other

  # Helpers

  @spec gen_uniq_vars(non_neg_integer(), atom()) :: [Macro.t()]
  defp gen_uniq_vars(0, _context), do: []

  defp gen_uniq_vars(n, context) when n > 0 do
    for i <- 1..n do
      {:"tria_var_#{i}", [], context}
    end
  end

  @doc "Checks if given AST is a special form"
  def is_special_form({name, _meta, args}) when name in @special_forms do
    Macro.special_form?(name, length(args))
  end
  def is_special_form(_), do: false

  @doc "Counts size of an AST"
  def size_ast({op, _meta, args}), do: size_ast(op) + size_ast(args)
  def size_ast([lh | lt]), do: size_ast(lh) + size_ast(lt)
  def size_ast({left, right}), do: size_ast(left) + size_ast(right)
  def size_ast(_other), do: 1

  def arityfy({{:".", _, [module, function]}, _, arguments}) do
    arityfy {unalias(module), function, arguments}
  end

  def arityfy({module, function, arguments}) when is_list(arguments) do
    {module, function, length(arguments)}
  end

  def arityfy({module, function, arguments}) when is_integer(arguments) do
    {module, function, arguments}
  end

  def varctx({name, meta, context} = v) when is_variable(v) do
    case Keyword.fetch(meta, :counter) do
      {:ok, counter} ->
        {name, {context, counter}}

      :error ->
        {name, context}
    end
  end

end
