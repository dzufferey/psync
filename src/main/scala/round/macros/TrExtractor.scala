package round.macros

import round.formula._
import round.verification._
import round.utils.{Namer, Misc}

trait TrExtractor {
  self: Impl =>
  import c.universe._


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

  private def getPreCondition(body: Tree): Option[(Tree, Formula)] = body match {
    case q"{ scala.this.Predef.require(..$args); ..$exprs }" =>
      val f = tree2Formula(args.head)
      //there might be a 2nd Srting arg that describes what the precond is about
      Some(Block(exprs.init, exprs.last), f)
    case _ => None
  }


  private def assigned(t: Tree): List[Tree] = t match {
    case If(cond, thenp, elsep) =>
      val thena = assigned(thenp)
      val elsea = assigned(elsep)
      //multiset least upper bound:
      (thena intersect elsea) union ((thena diff elsea) union (elsea diff thena))
    case Block(stats, expr) => (stats ::: List(expr)) flatMap assigned
    case Typed(e, _) => assigned(e)
    case Apply(Select(lhs, TermName("$less$tilde")), List(rhs)) => List(lhs)
    case Assign(lhs, rhs) => List(lhs)
    //case ValDef(mods, name, tpe, rhs) => List(Ident(name))
    case other => Nil
  }

  private def joinSsaSubst(
      s1: Map[Tree, List[Tree]],
      s2: Map[Tree, List[Tree]]
    ): (Map[Tree, List[Tree]], List[List[Tree]], List[List[Tree]]) =
  {
    val ks = s1.keySet union s2.keySet
    val init: (Map[Tree, List[Tree]], List[List[Tree]], List[List[Tree]]) = (Map.empty, Nil, Nil)
    ks.foldLeft(init)( (acc, k) => {
      val v1 = s1.getOrElse(k, Nil)
      val v2 = s2.getOrElse(k, Nil)
      val only1 = v1 drop v2.length
      val only2 = v2 drop v1.length
      val common = k :: (v1 take (math.min(v1.length, v2.length)))
      val joined = if (v1.length > v2.length) v1 else v2
      //
      val acc1 = acc._1 + (k -> joined)
      val acc2 = common.last :: only2
      val acc3 = common.last :: only1
      (acc1, acc2 :: acc._2, acc3 :: acc._3)
    })
  }


  private def addSsaMatchingCode(t: Tree, eqs: List[List[Tree]]): Tree = {
    def mkEqs(vars: List[Tree]) = {
      if (vars.length > 1) {
        vars.sliding(2).map( vs => Assign(vs(1), vs(0)) ).toList
      } else {
        Nil
      }
    }
    val matchingCode: List[Tree] = eqs flatMap mkEqs
    //blockify
    t match {
      case Block(stmts, ret) =>
        treeCopy.Block(t, stmts ::: matchingCode, ret)
      case EmptyTree =>
        if (matchingCode.size > 1) {
          treeCopy.Block(t, matchingCode, EmptyTree)
        } else if (matchingCode.size == 1) {
          matchingCode.head
        } else {
          EmptyTree
        }
      case other =>
        if (!matchingCode.isEmpty)
          //TODO does that make sense? if the if returns something it should fail later when extracting constraints ?!
          treeCopy.Block(other, other :: matchingCode, EmptyTree)
        else
          other
    }
  }
  
  class SsaSubst(map: Map[Tree, List[Tree]]) extends Transformer {
    override def transform(tree: Tree): Tree = {
      val sup = super.transform(tree)
      map.getOrElse(sup, List(sup)).last
    }
  }

  private def applySsaSubst(t: Tree, subst: Map[Tree, List[Tree]]): Tree = {
    val sub = new SsaSubst(subst)
    sub.transform(t)
  }

  private def increment(t: Tree, subst: Map[Tree, List[Tree]]): (Tree, Map[Tree, List[Tree]]) = {
    val prev = subst.getOrElse(t, Nil)
    val last = if (prev.isEmpty) t else prev.last
    def newName(oldName: TermName) = TermName(Namer(oldName.toString))
    val next = last match {
      case Ident(name @ TermName(_)) =>
        treeCopy.Ident(last, newName(name))
      case Select(scope, name @ TermName(_)) => 
        treeCopy.Select(last, scope, newName(name))
      case other =>
        c.abort(t.pos, "expected identifer or field, found: " + showRaw(other))
    }
    (next, subst + (t -> (prev :+ next)))
  }


  private val emptyMods = Modifiers()

  //returns the body in SSA and a map of tree to the different version
  private def ssa(
      body: Tree,
      substMap: Map[Tree, List[Tree]] = Map.empty[Tree, List[Tree]]
    ): (Tree, Map[Tree, List[Tree]]) = body match {
    
    case If(cond, thenp, elsep) =>
      val cond2 = applySsaSubst(cond, substMap)
      val (then2, substMapT) = ssa(thenp, substMap)
      val (else2, substMapE) = ssa(elsep, substMap)
      val (substMap2, matchT, matchE) = joinSsaSubst(substMapT, substMapE)
      val then3 = addSsaMatchingCode(then2, matchT)
      val else3 = addSsaMatchingCode(else2, matchE)
      (treeCopy.If(body, cond2, then3, else3), substMap2)
    
    case Block(stats, expr) => 
      val (stats2, substMap2) = Misc.mapFold(stats, substMap, ssa)
      val (expr2, substMap3) = ssa(expr, substMap2)
      (treeCopy.Block(body, stats2, expr2), substMap3)
    
    case Typed(e, _) =>
      ssa(e, substMap)
    
    case Apply(Select(lhs, TermName("$less$tilde")), List(rhs)) => 
      ssa(treeCopy.Assign(body, lhs, rhs), substMap)

    case Assign(lhs, rhs) => 
      val (rhs2, substMap2) = ssa(rhs, substMap)
      val (lhs2, substMap3) = increment(lhs, substMap2)
      (treeCopy.Assign(body, lhs2, rhs2), substMap3)

    case ValDef(`emptyMods`, name, tpt, rhs) =>
      val (rhs2, substMap2) = ssa(rhs, substMap)
      (treeCopy.ValDef(body, emptyMods, name, tpt, rhs2), substMap2)

    case ValDef(mods, name, tpt, rhs) => //if mutable ...
      c.abort(body.pos, "Round should not contain variable, please use values only: non-empty modifier in ValDef " + mods)

    case other =>
      (applySsaSubst(other, substMap), substMap)
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

  
  protected def auxiliaryFunction(d: DefDef): AuxiliaryMethod = {
    if (d.vparamss.length > 1) {
      c.abort(c.enclosingPosition, "auxiliaryFunction, currying not yet supported: " + d.name)
    }
    c.echo(c.enclosingPosition, "currently we do not verify auxiliary functions (" +d.name.toString +") and assume they are side-effect free")
    val name = d.name.toString
    val params = d.vparamss.head.map(extractVarFromValDef)
    val tpe = round.formula.Function(params.map(_.tpe), extractType(d.tpt.tpe))
    val tParams: List[TypeVariable] = d.tparams.map(extractTypeVar)

    val (body2, pre) = getPreCondition(d.rhs).getOrElse((d.rhs, True()))
    val (body3, vRet, post) = getPostCondition(body2).getOrElse(body2, Variable(Namer("__return")).setType(tpe), True())
    val body = None //TODO Option[TransitionRelation],
    new AuxiliaryMethod(name, params, tpe, tParams, pre, body, (vRet, post))
  }

  protected def processSendUpdate(send: DefDef, update: DefDef): RoundTransitionRelation = {
    val mailboxValDef = update.vparamss.head.head
    val mailboxIdent = Ident(TermName(mailboxValDef.name + "Snd")) //TODO type ?
    val (ssaSend, subst) = ssa(send.rhs)
    val cstr1 = makeConstraints(ssaSend, Some(mailboxIdent), Some(mailboxIdent))

    val (ssaUpdt, subst2) = ssa(update.rhs, subst)
    val cstr2 = makeConstraints(ssaUpdt, None, None)

    def getVar(t: Tree): Variable = tree2Formula(t) match {
      case v @ Variable(_) => v
      case other => 
        c.abort(t.pos, "could not extract vaiable from: " + t + ", got " + other)
    }

    val keys = subst2.keys.toList
    val oldV = keys.map(getVar)
    val newV = keys.map( k => getVar(subst2(k).last))

    val allVars = subst2.foldLeft(Nil: List[Variable])( (acc, kv) => {
      (kv._1 :: kv._2).map(getVar) ::: acc
    }) 
    val local1 = allVars.filter( x => !(oldV.contains(x) || newV.contains(x)))
    val local2 = tree2Formula(mailboxIdent).asInstanceOf[Variable]
    val local3 = getValDefs(send.rhs).map(extractVarFromValDef)
    val local4 = getValDefs(update.rhs).map(extractVarFromValDef)
    val localV = local1 ::: local2 :: local3 ::: local4

    new RoundTransitionRelation(cstr1, getVar(mailboxIdent),
                                cstr2, extractVarFromValDef(mailboxValDef),
                                oldV, localV, newV)
  }

  

  protected def mkAuxMap(aux: List[AuxiliaryMethod]): Tree = {
    aux.foldLeft(q"Map.empty[String,round.verification.AuxiliaryMethod]")( (acc, a) => {
      val name = a.name
      q"$acc + ($name -> $a)"
    })
  }

}
