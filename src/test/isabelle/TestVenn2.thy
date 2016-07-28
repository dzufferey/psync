theory TestVenn2
imports Main "../../main/isabelle/EisbachVenn2"
begin

lemma basicTest:
  assumes "finite (UNIV::'a set)"
  assumes "card (UNIV::'a set) - y < card {(q::'a). P q}"
  assumes "y \<le> card (UNIV::'a set)"
  shows "card {q. ~P q} \<le> y"
  proof -
  show "card {q. ~P q} \<le> y" using assms
  apply -
  apply tryAddVennFacts
  by linarith
qed

lemma majorityTest1:
  assumes "finite (UNIV::'a set)"
  assumes "2 * card {(p::'a). (x p) = v1 } > card (UNIV::'a set)"
  assumes "2 * card {(p::'a). (x p) = v2 } > card (UNIV::'a set)"
  shows "v1 = v2"
  proof -
  show "v1 = v2" using assms
  apply -
  apply (rule ccontr)
  apply (singleVennIntroNoForce "{(p::'a). (x p) = v1 }" "{(p::'a). (x p) = v2 }")
  apply force
  apply tryAddVennFacts
  by simp
qed

lemma majorityTest2: "finite (UNIV::'a set) \<and> n = card (UNIV::'a set) \<and> 2 * card {(x::'a). (p x) = v1 } > n \<and> 2 * card {(x::'a). (p x) = v2 } > n \<longrightarrow> v1 = v2"
  by (auto, rule ccontr, singleVennIntroNoForce "{(x::'a). (p x) = v1}" "{(x::'a). (p x) = v2}", force, tryAddVennFacts, simp)

lemma simpleTest1: "finite (UNIV::'a set) \<and> n = card (UNIV::'a set) \<and> card {(x::'a). P x } = n \<longrightarrow> card {(x::'a). ~P x } = 0"
  by (auto, tryAddVennFacts, linarith)

end
