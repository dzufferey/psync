package psync.macros

import psync.formula._
import psync.verification._
import dzufferey.utils.{Namer, Misc}

trait TrExtractor {
  self: Impl =>
  import c.universe._

  protected def findMethod(body: List[Tree], name: String): Option[DefDef] = {
    body.collectFirst{
      case d @ DefDef(_, TermName(nme), _, _, _, _) if nme == name => d
    }
  }

  protected def findMethod(clazz: Tree, name: String): Option[DefDef] = {
    clazz match {
      case ClassDef(_, _, _, Template(_, _, body)) => findMethod(body, name)
      case _ => None
    }
  }

  //TODO using type annotations
  private def getPostCondition(body: Tree): Option[(Tree, Variable, Formula)] = body match {
    case q"scala.this.Predef.Ensuring[$tpt]($expr).ensuring( $ret => $postCond )" =>
      val v = ret match {
        case ValDef(_, name, _, _) => Variable(name.toString)
        case _ => c.abort(ret.pos, "expected ValDef, found: " + showRaw(ret))
      }
      val f = tree2Formula(postCond)
      val t = extractType(tpt)
      Some((expr,v.setType(t),f))
    case _ =>
      None
  }

  //TODO using type annotations
  private def getPreCondition(body: Tree): Option[(Tree, Formula)] = body match {
    case q"{ scala.this.Predef.require(..$args); ..$exprs }" =>
      val f = tree2Formula(args.head)
      //there might be a 2nd Srting arg that describes what the precond is about
      Some(Block(exprs.init, exprs.last), f)
    case _ => None
  }

  //TODO rename local var to give them unique name ? (avoid shadowing)

  class CollectValDef extends Traverser {
    var vds: List[ValDef] = Nil
    override def traverse(t: Tree) = {
      super.traverse(t)
      t match {
        case vd @ ValDef(_, _, _, _) => vds = vd :: vds
        case _ => ()
      }
    }

  }
  def getValDefs(t: Tree): List[ValDef] = {
    val collector = new CollectValDef
    collector.traverse(t)
    collector.vds
  }

  protected def tryType(f: Formula, t: Tree, err: String): Formula = {
    Typer(f) match {
      case Typer.TypingSuccess(f) =>
        f
      case Typer.TypingFailure(r) =>
        c.warning(t.pos, err +", leaving it unconstrained:\n" + r + "\n" + f)
        True()
      case Typer.TypingError(r) =>
        c.abort(t.pos, err +":\n" + r + "\n" + f)
    }
  }

  protected def auxiliaryFunction(d: DefDef): AuxiliaryMethod = {
    if (d.vparamss.length > 1) {
      c.abort(c.enclosingPosition, "auxiliaryFunction, currying not yet supported: " + d.name)
    }
    c.echo(d.pos, "currently we do not verify auxiliary functions (" +d.name.toString +") and assume they are pure")
    val name = d.name.toString
    val params = d.vparamss.headOption.getOrElse(Nil).map(extractVarFromValDef)
    val tpe = psync.formula.Function(params.map(_.tpe), extractType(d.tpt.tpe))
    val tParams: List[TypeVariable] = d.tparams.map(extractTypeVar)

    val (body2, _pre) = getPreCondition(d.rhs).getOrElse((d.rhs, True()))
    val pre = tryType(_pre, d.rhs, "unable to type precondition of " + d.name)
    getPostCondition(body2) match {
      case Some((body3, vRet, _post)) =>
        val body = None //TODO Option[TransitionRelation],
        val post = tryType(_post, body2, "unable to type postcondition of " + d.name)
        new AuxiliaryMethod(name, params, tpe, tParams, pre, body, Some((vRet, post)))
      case None =>
        val body = None //TODO Option[TransitionRelation],
        new AuxiliaryMethod(name, params, tpe, tParams, pre, body, None)
    }
  }

  protected def processSendUpdate(send: DefDef, update: DefDef): RoundTransitionRelation = {
    val mailboxValDef = update.vparamss.head.head
    val mailbox = extractVarFromValDef(mailboxValDef)
    val mailboxSnd = Variable(mailboxValDef.name.toString + "Snd").setType(mailbox.tpe)
    val (ssaSend, subst) = ssa(send.rhs)

    //TODO should try to 'transpose' mailboxSnd and only keep one mailbox

    val _cstr1 =
      try {
        makeConstraints(ssaSend, mailboxSnd, mailboxSnd)
      } catch {
        case e: Exception =>
          c.warning(send.pos, "error while extracting the send TR, leaving it unconstrained.\n" + e)
          True()
      }

    val (ssaUpdt, subst2) = ssa(update.rhs, subst)
    val _cstr2 =
      try {
        makeConstraints(ssaUpdt)
      } catch {
        case e: Exception =>
          c.warning(update.pos, "error while extracting the update TR, leaving it unconstrained.\n" + e)
          True()
      }

    val cstr1 = tryType(_cstr1, send,   "unable to type the 'send' formula")
    val cstr2 = tryType(_cstr2, update, "unable to type the 'update' formula")

    def getVar(t: Tree): Variable = tree2Formula(t) match {
      case v @ Variable(_) => v
      case other =>
        c.abort(t.pos, "could not extract variable from: " + t + ", got " + other)
    }

    val keys = subst2.keys.toList
    val oldV = keys.map( k => getVar(subst2.getTree(k)))
    val newV = keys.map( k => getVar(subst2.getMostRecentTree(k)) )
    //println("typed update cstr: " + cstr2)

    val allVars = subst2.foldLeft(Nil: List[Variable])( (acc, kv) => {
      val tree = kv._2._1
      val first = SsaMap.version(tree)
      val last = kv._2._2
      val vs = for(i <- first to last) yield getVar(SsaMap.name(tree, i))
      acc ++ vs
    })
    val local1 = allVars.filter( x => !(oldV.contains(x) || newV.contains(x)))
    val local3 = getValDefs(send.rhs).map(extractVarFromValDef)
    val local4 = getValDefs(update.rhs).map(extractVarFromValDef)
    val localC = local1 ::: mailboxSnd :: mailbox :: local3 ::: local4
    val localB = And(cstr1, cstr2).boundVariables
    val localF = And(cstr1, cstr2).freeVariables
    val localV = localC.filter(x => !localB(x)) //otherwise we capture var bound in comprehensions with the getValDefs
    //val localV = localC.filter(x => localF(x)) //otherwise we capture var bound in comprehensions with the getValDefs

    new RoundTransitionRelation(cstr1, mailboxSnd,
                                cstr2, mailbox, oldV, localV, newV)
  }



  protected def mkAuxMap(aux: List[AuxiliaryMethod]): Tree = {
    aux.foldLeft(q"Map.empty[String,psync.verification.AuxiliaryMethod]")( (acc, a) => {
      val name = a.name
      q"$acc + ($name -> $a)"
    })
  }

}
