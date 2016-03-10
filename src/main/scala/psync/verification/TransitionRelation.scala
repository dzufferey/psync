package psync.verification

import Utils._

import psync.formula._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

//a wrapper around a formula, old/primed variables, ...

class RoundTransitionRelation(val send: Formula,
                              val mailboxSend: Variable,
                              val update: Formula,
                              val mailboxUpdt: Variable,
                              val old: List[Variable],
                              val local: List[Variable],
                              val primed: List[Variable]) {

  //TODO clean that method!
  def retype(env: Set[Variable]): RoundTransitionRelation = {
    assert(mailboxSend.tpe != Wildcard, "mailboxSend has Wildcard type")
    assert(mailboxUpdt.tpe != Wildcard, "mailboxUpdt has Wildcard type")
    assert(local.forall(_.tpe != Wildcard), "some local variables has Wildcard type")
    Logger("TransitionRelation", Debug, "retype env:\n  " + env.map(a => a+":"+a.tpe).mkString("\n  "))
    Logger("TransitionRelation", Debug, "retype old:\n  " + old.map(a => a+":"+a.tpe).mkString("\n  "))
    Logger("TransitionRelation", Debug, "retype primed:\n  " + primed.map(a => a+":"+a.tpe).mkString("\n  "))
    //match old and primed with env
    val substOld = old.map( v => v -> env.find(_ == v).get ).toMap
    def rmSuffix(prefix: String) = {
      val idx = prefix.lastIndexOf("$")
      if (idx == -1) prefix
      else prefix.substring(0, idx)
    }
    val substPrimed = primed.map( v => v -> v.setType(env.find(v2 => rmSuffix(v.name) == (v2.name)).get.tpe) ).toMap
    val subst = substOld ++ substPrimed
    assert(subst.forall(_._2.tpe != Wildcard), "some env variables has Wildcard type")
    Logger("TransitionRelation", Debug, "retype subst:\n  " + subst.map{ case (a,b) => a+":"+a.tpe+" → "+b+":"+b.tpe }.mkString("\n  "))
    def fixMB(f: Formula) = {
      for (v <- f.freeVariables) {
        if (v == mailboxSend) v.setType(mailboxSend.tpe)
        if (v == mailboxUpdt) v.setType(mailboxUpdt.tpe)
      }
      f
    }
    def fixType(f: Formula) = {
      val traverser = new Traverser{
        override def traverse(f: Formula) = {
          super.traverse(f)
          f match {
            case v @ Variable(_) if subst contains v=>
              v.setType(subst(v).tpe)
            case _ =>
          }
        }
      }
      traverser.traverse(f)
      f
    }
    new RoundTransitionRelation(
      fixType(fixMB(FormulaUtils.alpha(subst, send))),
      mailboxSend,
      fixType(fixMB(FormulaUtils.alpha(subst, update))),
      mailboxUpdt,
      old.map(substOld),
      local,
      primed.map(substPrimed)
    )
  }

  //link mailboxes with HO:
  //  ∀ i j v. (i, v) ∈ mailboxUpdt(j) ⇔ (i ∈ HO(j) ∧ (j, v) ∈ mailboxSend(i))
  //  ∀ j. |mailboxUpdt(j)| ≤ |HO(j)|
  lazy val mailboxLink = {
    val i = procI
    val j = procJ
    val vTpe = mailboxSend.tpe match {
      case FSet(Product(List(t, p))) => assert(p == procType); t
      case other => sys.error("mailbox type is " + other)
    }
    val v = Variable("v").setType(vTpe)
    val iv = Application(Tuple, List(v, i))
    val jv = Application(Tuple, List(v, j))
    val mi = skolemify(mailboxSend, i)
    val mj = skolemify(mailboxUpdt, j)
    val ho = skolemify(Variable("HO").setType(FSet(procType)), j)
    And(
      ForAll(List(i, j, v), Eq(In(iv, mj), And(In(i, ho), In(jv, mi)))),
      ForAll(List(j), Leq(Cardinality(mj), Cardinality(ho)))
    )
  }
  
  class InlinePost(aux: Map[String, AuxiliaryMethod], vars: Set[Variable], i: Variable) extends Transformer {
    override def transform(f: Formula): Formula = {
      f match {
        case Eq(retVal, Application(UnInterpretedFct(fct, _, tParams), args)) if aux contains fct =>
          Logger("TransitionRelation", Debug, "inline post in " + f)
          val auxDef = aux(fct).applyType(tParams)
          auxDef.makePostAssume(args, retVal) match {
            case Some(post) =>
              val loc = localize(vars, i, post)
              Logger("TransitionRelation", Debug, "resulting in " + loc)
              super.transform(loc)
            case None =>
              super.transform(f)
          }
        case other =>
          super.transform(other)
      }
    }
  }

  protected def captureId(currentScope: Variable, f: Formula) = {
    val id = Variable("id").setType(procType)
    FormulaUtils.alpha(Map(id -> currentScope), f)
  }

  def makeFullTr(vars: Set[Variable], aux: Map[String, AuxiliaryMethod]): Formula = {
    assert(old forall ((vars + mailboxSend + mailboxUpdt) contains _))
    val localVars = vars ++ local ++ old ++ primed + mailboxSend + mailboxUpdt
    Logger("TransitionRelation", Debug, "makeFullTr, localize with" + localVars.mkString(", "))
    val i = procI
    //check it is not captured/ing
    assert(!(send.freeVariables contains i), "capture in send")
    assert(!(update.freeVariables contains i), "capture in update")
    val inliner = new InlinePost(aux, localVars, i)
    val sendLocal = inliner.transform(localize(localVars, i, send))
    val updateLocal = inliner.transform(localize(localVars, i, update))
    val sendFinal = ForAll(List(i), captureId(i, sendLocal))
    val updateFinal = ForAll(List(i), captureId(i, updateLocal))
    And(sendFinal, And(mailboxLink, updateFinal))
  }
  
  lazy val primedSubst: Map[UnInterpretedFct, UnInterpretedFct] = {
    val map = (old zip primed).flatMap{ case (o,p) =>
      val o1 = skolemify(o)
      val o2 = o1.stripType
      val p1 = skolemify(p)
      List((o1, p1), (o2, p1)) 
    }.toMap
    //println("primedSubst: " + map)
    map
  }
  
  def primeFormula(f: Formula) = {
    removeOldPrefix(FormulaUtils.mapSymbol({
      case f @ UnInterpretedFct(_,_,_) =>
        //println("potential subst for " + f.raw + " in " + primedSubst.keys.map(_.raw).mkString(", "))
        primedSubst.getOrElse(f,f)
      case f => f
    }, f))
  }

}


//this is the single process version
class TransitionRelation(_tr: Formula,
                         val old: List[Variable],
                         val local: List[Variable],
                         val primed: List[Variable]) {

  val tr = FormulaUtils.purify(_tr)

  /* change the var so the formula refer to the primed vars */
  def primeFormula(f: Formula) = {
    val subst = old.zip(primed).foldLeft(Map.empty[Variable,Variable])(_ + _)
    FormulaUtils.alpha(subst, f)
  }

  class InlinePost(aux: Map[String, AuxiliaryMethod]) extends Transformer {
    override def transform(f: Formula): Formula = {
      f match {
        case Eq(retVal, Application(UnInterpretedFct(fct, _, tParams), args)) if aux contains fct =>
          val auxDef = aux(fct).applyType(tParams)
          super.transform(auxDef.makePostAssume(args, retVal).getOrElse(f))
        case other => other
      }
    }
  }

  /* the formula with the postconditions */
  def addAuxiliary(aux: Map[String, AuxiliaryMethod]): Formula = {
    val inliner = new InlinePost(aux)
    inliner.transform(tr)
  }

  /* returns a list of precondition to test */
  def auxPrecondition(aux: Map[String, AuxiliaryMethod]): List[(Formula, Formula)] = {
    ???
  }
  
  def report = ???

}
