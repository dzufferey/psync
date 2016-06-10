signature OPS = sig
  (* argument is pair:
   *  term to prove
   *  method: Some("(induct n, auto)")
   *)
  val prove: term * string option -> string option
end

structure Ops: OPS = struct

fun try_timeout' f = try_timeout 5 f (* global timeout for auxiliary operations *)

fun err_timeout msg f x =
  case try_timeout 5 f x of
    Exn.Res r => r
  | Exn.Exn TimeLimit.TimeOut => raise Timeout msg
  | Exn.Exn exn => reraise exn

type state = local_theory option
val empty_state = NONE: state

val state = Synchronized.var "libisabelle.psync_state" empty_state

fun unset_parallel_proofs () =
  Goal.parallel_proofs := 0

local
  fun not_loaded pos =
    invalid_state "no theory loaded" pos
  fun already_loaded lthy =
    invalid_state ("theory " ^ Context.theory_name (Proof_Context.theory_of lthy) ^ "already loaded")
in

fun access_loaded f =
  case Synchronized.value state of
    SOME thy => f thy
  | NONE => not_loaded @{here}

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

fun prove (t, method) =
  access_loaded (fn lthy =>
    let
      (* XXX
      val (ts', lthy) = register_terms [t] lthy
      val prop = Balanced_Tree.make HOLogic.mk_disj ts'
      *)
      val (prop, lthy) = register_term t lthy
      fun tac ctxt =
        case method of
          NONE => prove_tac ctxt
        | SOME src => HEADGOAL (method_tac @{here} src ctxt)
    in
      (* Assumption: all proofs are sequential *)
      try (Goal.prove lthy [] [] prop) (fn {context, ...} => tac context)
      |> Option.map (print_thm lthy)
    end)

end
