signature OPS = sig
  (* arguments are:
   * root_name: the name of the root theory, e.g., PSync
   * thy_name: the name of the thoery to start
   *)
  val start_theory: string * string -> unit
  (* arguments are:
   *  term to prove
   *  method: Some("(induct n, auto)")
   * TODO more structured output than string
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

fun start_theory (root_name, thy_name) =
  let
    val _ = unset_parallel_proofs ()
    val _ =
      if Execution.is_running_exec Document_ID.none then ()
      else invalid_state "Must not be running in interactive mode" @{here}
    fun upd (SOME lthy) = already_loaded lthy @{here}
      | upd NONE =
        let
          val root = Thy_Info.get_theory root_name
          val thy = Theory.begin_theory (thy_name, Position.none) [root]
          val res = SOME (Named_Target.theory_init thy)
        in
          res
        end
  in
    Synchronized.change state upd
  end

end

fun prove (t, method) =
  access_loaded (fn lthy =>
    let
      val ([prop], lthy) = register_terms [t] lthy
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
