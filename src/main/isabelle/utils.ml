
val unvarify_typ =
  let
    fun aux (TVar ((name, idx), sort)) = TFree (name ^ Markup.print_int idx, sort)
    | aux t = t
  in
    map_atyps aux
  end

val unvarify =
  let
    fun aux (Var ((name, idx), typ)) = Free (name ^ Markup.print_int idx, typ)
    | aux t = t
  in
    map_aterms aux o map_types unvarify_typ
  end

fun print_term ctxt =
  let
    val ctxt' = Config.put show_markup false ctxt
  in
    YXML.content_of o Syntax.string_of_term ctxt' o unvarify
  end

fun print_typ ctxt =
  let
    val ctxt' = Config.put show_markup false ctxt
  in
    YXML.content_of o Syntax.string_of_typ ctxt' o unvarify_typ
  end

fun print_thm ctxt thm =
  let
    val t = Thm.prop_of thm
    val {oracle, ...} = Thm.peek_status thm
    val suffix = if oracle then " [!]" else ""
  in
    print_term ctxt t ^ suffix
  end
