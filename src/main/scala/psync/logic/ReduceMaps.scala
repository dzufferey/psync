package psync.logic

import psync.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ReduceMaps {

  def termGen = {
    val kType = TypeVariable("K")
    val vType = TypeVariable("V")
    val v = Variable("m").setType(FMap(kType, vType))
    val t = KeySet(v).setType(FSet(kType)) //currently we need to put the type manually when there are type variables
    new TermGenerator(List(v), t)
  }

  def newTerms(cc: CongruenceClosure): Set[Formula] = {
    val terms = termGen(cc.groundTerms).filter(!cc.contains(_))
    Logger("ReduceMaps", Debug, terms.mkString("new sets: \n    ","\n    ",""))
    terms
  }

  def addMapGroundTerms(/*f: Formula,*/ cc: CongruenceClosure): Unit = {
    //Simplify.normalize should already have taken care of IsDefinedAt and Size
    //we only need to add new terms to cc
    val terms = newTerms(cc)
    terms.foreach(cc.repr) //lookup also add in cc
  }

}
