theory EisbachVenn2
imports Main "~~/src/HOL/Eisbach/Eisbach_Tools" "Venn2"
begin

(* checks if two terms are different *)
method not_same for P Q = ( match (P) in Q \<Rightarrow> \<open> fail \<close> \<bar> _ \<Rightarrow> \<open> - \<close> )
(* checks that something is not already in the premises *)
method not_in_premises for P :: bool = ( match premises in P \<Rightarrow> \<open> fail \<close> \<bar> _ \<Rightarrow> \<open> - \<close> )
(* neither UNIV nor {}, or any set operation *)
method relevant_set for A :: "'a set" =
  ( match (A) in
      "{}" \<Rightarrow> \<open> fail \<close>
    \<bar> "-{}" \<Rightarrow> \<open> fail \<close>
    \<bar> "UNIV" \<Rightarrow> \<open> fail \<close>
    \<bar> "-UNIV" \<Rightarrow>  \<open> fail \<close>
    \<bar> "_ \<inter> _" \<Rightarrow>  \<open> fail \<close>
    \<bar> "_ \<union> _" \<Rightarrow>  \<open> fail \<close>
    \<bar> _ \<Rightarrow>  \<open> - \<close> )

(* lemma to introduce the Venn regions and check if empty *)
lemma singleVennIntro: "\<lbrakk> (A \<inter>  B) = {}; (A \<inter>  B) = {} \<Longrightarrow> P\<rbrakk> \<Longrightarrow> P" by auto
method singleVennIntroNoForce for A B :: "'a set" =
  ( match premises in "(A \<inter>  B) = {}" \<Rightarrow> \<open> fail \<close>
    \<bar>"(B \<inter> A) = {}" \<Rightarrow> \<open> fail \<close>
    \<bar> _ \<Rightarrow> \<open> rule singleVennIntro[of A B] \<close> )
method singleVennIntro for A B :: "'a set" = (
  singleVennIntroNoForce A B, force )

lemma cardIntro: "\<lbrakk> finite (UNIV::'a set);
                    card ((A:: 'a set) \<inter> B) + card (-A \<inter> B) + card (A \<inter> -B) + card (-A \<inter> -B) = card (UNIV:: 'a set) \<Longrightarrow>
                    card (A \<inter> B) + card (A \<inter> -B) = card A \<Longrightarrow>
                    card (A \<inter> B) + card (-A \<inter> B) = card B \<Longrightarrow>
                    P\<rbrakk> \<Longrightarrow> P"
  using cardComp1 cardComp2 vennCard by blast
method vennCard for A B :: "'a set" = (
    match premises in
      "card (A \<inter> B) + card (A \<inter> -B) = card A" (cut) \<Rightarrow> \<open> fail \<close>
    \<bar> "card (B \<inter> A) + card (B \<inter> -A) = card B" (cut) \<Rightarrow> \<open> fail \<close>
    \<bar> L:"finite (UNIV::'a set)" (cut) \<Rightarrow> \<open> rule cardIntro[of A B], simp add:L \<close>
    \<bar> _ (cut) \<Rightarrow> \<open> fail \<close>
  )

method vennIntro for A B :: "'a set" = (
  print_term A, print_term B,
  ( singleVennIntro A B, (singleVennIntro A "-B")?, (singleVennIntro "-A" B)?, (singleVennIntro "-A" "-B")?, (vennCard A B)? ) |
  ( singleVennIntro A "-B", (singleVennIntro "-A" B)?, (singleVennIntro "-A" "-B")?, (vennCard A B)? ) |
  ( singleVennIntro "-A" B, (singleVennIntro "-A" "-B")?, (vennCard A B)? ) |
  ( singleVennIntro "-A" "-B", (vennCard A B)? ) |
  ( vennCard A B )
  )

(* all sets pairwise ... *)

method allVennIntro1 =
  ( match premises in "?P A" (multi) for A :: "'a set" \<Rightarrow>
      \<open> relevant_set A,
        ( match premises in "?Q B" for B :: "'a set" \<Rightarrow>
          \<open> relevant_set B, not_same A B, vennIntro A B \<close>
        | match conclusion in"?Q B" for B :: "'a set" \<Rightarrow>
          \<open> relevant_set B, not_same A B, vennIntro A B \<close> ) \<close>
  |  match conclusion in "?P A" for A :: "'a set" \<Rightarrow>
      \<open> relevant_set A, print_term A,
        match conclusion in"?Q B" for B :: "'a set" \<Rightarrow>
        \<open> relevant_set B, not_same A B, vennIntro A B \<close> \<close> )
(* TODO how to match all the sets once ?!?
 * calling this multiple times (or with '+') is _really slow_  *)
method allVennIntro = allVennIntro1

lemma emptyCardIntro: "\<lbrakk> A = {}; card A = 0 \<Longrightarrow> P \<rbrakk> \<Longrightarrow> P" by simp
method emptyCardIntro =
  ( match premises in "finite (UNIV::'a set)" \<Rightarrow>
    \<open> match premises in L:"A = {}" for A::"'a set" \<Rightarrow>
      \<open> not_in_premises "card A = 0", rule emptyCardIntro[of A], (simp add:L) \<close> \<close> )
method allEmptyCardIntro = emptyCardIntro+


method tryAddVennFacts = ( allVennIntro?, allEmptyCardIntro? )

end
