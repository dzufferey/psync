package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ReduceMaps {

  val termGen = {
    val kType = TypeVariable("K")
    val vType = TypeVariable("V")
    val v = Variable("m").setType(FMap(kType, vType))
    new TermGenerator(List(v), KeySet(v))
  }

  //TODO more operations, e.g.,
  // containsEntry(m, k, v) ⇔ k ∈ keySet(m) ∧ lookUp(m, k) = v
  // add(m,k,v) ⇔ keySet(add(m,k,v)) = {k} ∪ keySet(m) ∧ ∀ l. ite(l = v, lookUp(add(m,k,v), l) = v, lookUp(add(m,k,v), l) = lookUp(m, l))
  // rem(m,k) ⇔ keySet(rem(m,k)) = diff(keySet(m), k) ∧ ∀ l. l ≠ v ⇒ lookUp(rem(m,k), l) = lookUp(m, l)

  def addMapGroundTerms(/*f: Formula,*/ cc: CongruenceClosure) {
    //Simplify.normalize should already have taken care of IsDefinedAt and Size
    //we only need to add new terms to cc
    val terms = termGen(cc.groundTerms)
    terms.foreach(cc.repr) //lookup also add in cc
  }

}
