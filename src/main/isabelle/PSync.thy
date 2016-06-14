theory PSync
imports
  Protocol
  Codec_Class
  Main
  Venn2
  EisbachVenn2
begin

ML_file "~~/src/HOL/TPTP/sledgehammer_tactics.ML"

sledgehammer_params [timeout = 5, provers = cvc4 z3 spass e]

ML_file "utils.ml"
ML_file "operations.ml"

operation_setup (auto) start_theory = \<open>Ops.start_theory\<close>
operation_setup (auto) prove = \<open>Ops.prove\<close>

end
