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

end

fun prove (t, method) =
  access_loaded (fn lthy =>
    let
      val (ts', lthy) = register_terms [t] lthy
      val prop = Balanced_Tree.make HOLogic.mk_disj ts'
      (* XXX
      val (prop, lthy) = register_term t lthy
      *)
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
