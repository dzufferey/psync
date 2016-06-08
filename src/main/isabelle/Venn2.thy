theory Venn2
imports Main
begin

lemma vennDecomp: "(A \<inter> B) \<union> (-A \<inter> B) \<union> (A \<inter> -B) \<union> (-A \<inter> -B) = UNIV"
  by blast

lemma disj1: "(A \<inter> B) \<inter> (-A \<inter> B) = {}" by simp
lemma disj2: "(A \<inter> B) \<inter> (A \<inter> -B) = {}" by simp
lemma comp1: "(A \<inter> B) \<union> (A \<inter> -B) = A" by auto
lemma comp2: "(A \<inter> B) \<union> (-A \<inter> B) = B" by auto

lemma vennCard:
  assumes "finite (UNIV:: 'a set)"
  shows "card ((A:: 'a set) \<inter> B) + card (-A \<inter> B) + card (A \<inter> -B) + card (-A \<inter> -B) = card (UNIV:: 'a set)"
  by (metis Compl_partition Int_absorb2 add.assoc assms card_Un_disjoint comp1 comp2 disj1 disjoint_eq_subset_Compl finite_Int inf_compl_bot)

lemma vennMember:
  assumes "finite (UNIV:: 'a set)"
  shows "card ((A:: 'a set) \<inter> B) > 0 \<longleftrightarrow> (\<exists> x. x \<in> A \<and> x \<in> B)"
  by (metis Compl_partition assms card_gt_0_iff disjoint_iff_not_equal finite_Un)

lemma cardDisjoint:
  assumes "finite (UNIV:: 'a set)"
  shows "(A::'a set) \<inter> B = {} \<Longrightarrow> card (A \<union> B) = card A + card B"
  by (meson assms card_Un_disjoint finite_subset top_greatest)

lemma cardComp1:
  assumes "finite (UNIV:: 'a set)"
  shows "card (A \<inter> B) + card (A \<inter> -B) = card (A::'a set)"
  by (metis assms cardDisjoint comp1 disj2)

lemma cardComp2:
  assumes "finite (UNIV:: 'a set)"
  shows "card (A \<inter> B) + card (-A \<inter> B) = card (B::'a set)"
  by (metis assms cardDisjoint comp2 disj1)

end
