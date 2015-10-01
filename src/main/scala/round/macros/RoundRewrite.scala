package round.macros

import round.formula._
import round.verification._

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
    case tq"round.Round[$tpt]" => true
    case _ => false
  }
  

//def findTypeParam(parents: List[Tree]) = {
//  parents.collectFirst{ case tq"round.Round[$tpt]" => tpt }.get
//  //body.collectFirst{ case td @ TypeDef(_, TypeName("A"), _, tpt) => tpt }.get
//}

//object insideRound extends Transformer {
//  override def transform(tree: Tree): Tree = {
//    super.transform(tree) match {
//      case cd @ ClassDef(mods, name, tparams, tmpl @ Template(parents, self, body)) if parents exists extendsRound =>
//        val (snd, upd, aux) = traverseBody(body)
//        val tr = processSendUpdate(snd, upd)
//        val sndS = snd.toString
//        val updS = upd.toString
//        val s = q"override def sendStr: String = $sndS"
//        val u = q"override def updtStr: String = $updS"
//        val valTR = q"override def rawTR: round.verification.RoundTransitionRelation = $tr"
//        val treeAuxMap = mkAuxMap(aux)
//        val valAuxMap = q"override def auxSpec: Map[String, round.verification.AuxiliaryMethod] = $treeAuxMap"
//        val tpt = findTypeParam(parents)
//        val serialization = serializationMethods(tpt)
//        serialization.foreach(c.typecheck(_))
//        val body2 = /*s :: u :: valTR :: valAuxMap ::*/ body /*::: serialization*/
//        val tmpl2 = treeCopy.Template(tmpl, parents, self, body2)
//        treeCopy.ClassDef(cd, mods, name, tparams, tmpl2)
//      case other => other
//    }
//  }
//}

  protected def wrapRound(tree: Tree) = tree match {
    case q"new round.Round[$tpt] { ..$body }" =>
      val (snd, upd, aux) = traverseBody(body)
      val tr = processSendUpdate(snd, upd)
      val sndS = snd.toString
      val updS = upd.toString
      val treeAuxMap = mkAuxMap(aux)
      val serialization = serializationMethods(tpt)
      q"""new round.RoundWrapper {
        type A = $tpt
        val r = $tree
        ..$serialization
        override def sendStr: String = $sndS
        override def updtStr: String = $updS
        override def rawTR: round.verification.RoundTransitionRelation = $tr
        override def auxSpec: Map[String, round.verification.AuxiliaryMethod] = $treeAuxMap
      }"""
    case other =>
      //TODO if it is not a "new Round" we can still wrap it, the only difference is that we cannot get the spec!
      c.abort(tree.pos, "expected new Round[A], found: " + tree)
  }


  protected def processRound(t: Tree) = {// t match {
      //val tree = insideRound.transform(t)
      val tree = wrapRound(t)
      //println("generated round: " + show(tree))
      //c.typecheck(tree)
      tree
  }

}
