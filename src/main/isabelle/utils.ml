
exception Impossible of string
exception Invalid_State of string
exception Timeout of string

fun try_timeout secs f x =
  let
   val time = Time.fromSeconds secs
  in
   Exn.capture (TimeLimit.timeLimit time f) x
  end

fun print_position pos =
  let
    val print_file = Path.implode o Path.base o Path.explode
  in
    case (Position.file_of pos, Position.line_of pos) of
      (SOME file, SOME line) => print_file file ^ ":" ^ Markup.print_int line
    | (SOME file, NONE) => print_file file
    | _ => "<unknown>"
  end

local
  fun format_message msg pos = msg ^ " (at " ^ print_position pos ^ ")"
in

fun impossible msg pos =
  raise Impossible (format_message msg pos)

fun invalid_state msg pos =
  raise Invalid_State (format_message msg pos)

end

fun all_frees (Free (name, typ)) = [(name, typ)]
  | all_frees (t $ u) = all_frees t @ all_frees u
  | all_frees (Abs (_, _, t)) = all_frees t
  | all_frees _ = []

fun all_undeclared_frees ctxt =
  all_frees
  #> distinct op =
  #> filter (not o Variable.is_declared ctxt o fst)

fun register_terms ts lthy =
  let
    val ts' = Syntax.check_terms lthy ts |> map HOLogic.mk_Trueprop
    val frees = distinct op = (maps (all_undeclared_frees lthy) ts')
    val (free_names', lthy) = Variable.add_fixes (map fst frees) lthy
    val frees' = map Free (free_names' ~~ map snd frees)
    val frees = map Free frees
    val ts'' = map (subst_free (frees ~~ frees')) ts'
  in
    (ts'', fold Variable.declare_term ts'' lthy)
  end

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

fun prove_tac ctxt =
  HEADGOAL
    (SELECT_GOAL (TRY (auto_tac ctxt)) THEN_ALL_NEW
      (FIRST'
        [Cooper.tac false [] [] ctxt,
         Sledgehammer_Tactics.sledgehammer_with_metis_tac ctxt [] Sledgehammer_Fact.no_fact_override []]))

fun method_tac pos src = Subgoal.FOCUS (fn {context = ctxt, concl, ...} =>
  let
    val scan = Scan.finite Token.stopper Method.parse;
    val ((m, _), []) =
      src
      |> Token.explode (Thy_Header.get_keywords (Proof_Context.theory_of ctxt)) pos
      |> filter_out Token.is_space
      |> Scan.catch scan
    val state =
      Proof.theorem NONE (K I) [[(Thm.term_of concl, [])]] ctxt
      |> Proof.refine_singleton m
    val {goal, ...} = Proof.simple_goal state
  in
    HEADGOAL (resolve_tac ctxt [Goal.conclude goal])
  end)
