package psync.macros

import psync.formula._
import psync.verification._

trait RoundRewrite {
  self: Impl =>
  import c.universe._


  protected def traverseBody(body: List[Tree]) = {
    val acc: (Option[DefDef], Option[DefDef], List[AuxiliaryMethod]) = (None,None,Nil)
    val (snd, upd, aux) = body.foldLeft(acc)( (acc, stmt) => stmt match { 
      case ValDef(_, name, _, _) =>
          c.abort(c.enclosingPosition, "'"+name+"', Round should not contain variable/value definition. Please declare them as LocalVariable in the Algorithm.")
      case d @ DefDef(_, TermName("send"), _, _, _, _) =>
        assert(acc._1.isEmpty, "found multiple 'send' declarations")
        (Some(d), acc._2, acc._3)
      case d @ DefDef(_, TermName("update"), _, _, _, _) =>
        assert(acc._2.isEmpty, "found multiple 'update' declarations")
        (acc._1, Some(d), acc._3)
      case d @ DefDef(_, name, _, _, _, _) =>
        //ignore the ctor for the moment
        if (name == termNames.CONSTRUCTOR || name == TermName("expectedNbrMessages"))
          (acc._1, acc._2, acc._3)
        else
          (acc._1, acc._2, auxiliaryFunction(d) :: acc._3)
      case TypeDef(_, _, _, _) =>
        (acc._1, acc._2, acc._3)
    })
    (snd.get, upd.get, aux)
  }

  def extendsRound(t: Tree) = t match {
    case tq"psync.Round[$tpt]" => true
    case _ => false
  }
  
  protected def mkVerifAnnot(tree: Tree) = tree match {
    case q"new psync.Round[$tpt] { ..$body }" =>
      val (snd, upd, aux) = traverseBody(body)
      val tr = processSendUpdate(snd, upd)
      val sndS = snd.toString
      val updS = upd.toString
      val treeAuxMap = mkAuxMap(aux)
      q"""new psync.RoundSpec {
        override def sendStr: String = $sndS
        override def updtStr: String = $updS
        override def rawTR: psync.verification.RoundTransitionRelation = $tr
        override def auxSpec: Map[String, psync.verification.AuxiliaryMethod] = $treeAuxMap
      }"""
    case other =>
      c.warning(tree.pos, "expected new Round[_], did not match making trivial spec.")
      q"new psync.RoundSpec { }"
  }


  protected def processRound(t: Tree): (Tree, Tree) = {
      val spec = mkVerifAnnot(t)
      (t, spec)
  }

}
