package round.macros

import round.formula._
import round.verification._
import round.utils.{Namer, Misc}

trait TrExtractor {
  self: Impl =>
  import c.universe._

  //TODO from the round extract constraints representing the transition relation (TR)
  
  //model for the mailbox: M(i,j) means i sends to j at this round.
  //need to connect what is send to what is received
  //the must be SSA (no loop the modifies variables) //please put that in methods outside the send/receive

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
      vars.sliding(2).map( vs => Assign(vs(1), vs(0)) ).toList
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
      ssa(Assign(lhs, rhs), substMap)

    case Assign(lhs, rhs) => 
      val (rhs2, substMap2) = ssa(rhs, substMap)
      sys.error("TODO")

    case ValDef(`emptyMods`, name, tpt, rhs) =>
      val (rhs2, substMap2) = ssa(rhs, substMap)
      (treeCopy.ValDef(body, emptyMods, name, tpt, rhs2), substMap2)

    case ValDef(mods, name, tpt, rhs) => //if mutable ...
      sys.error("non-empty modifier for ValDef ?!: " + mods)

    case other =>
      (applySsaSubst(other, substMap), substMap)
  }

  //TODO rename local var to give them unique name ? (avoid shadowing)

  
  private def auxiliaryFunction(d: DefDef): AuxiliaryMethod = {
    if (d.vparamss.length > 1) {
      c.abort(c.enclosingPosition, "auxiliaryFunction, currying not yet supported: " + d.name)
    }
    c.echo(c.enclosingPosition, "currently we do not verify auxiliary functions (" +d.name.toString +")")
    val name = d.name.toString
    val params = d.vparamss.head.map(extractVarFromValDef)
    val tpe = round.formula.Function(params.map(_.tpe), extractType(d.tpt.tpe))
    val tParams: List[TypeVariable] = d.tparams.map(extractTypeVar)

    val (body2, pre) = getPreCondition(d.rhs).getOrElse((d.rhs, True()))
    val (body3, vRet, post) = getPostCondition(body2).getOrElse(body2, Variable(Namer("__return")).setType(tpe), True())
    val body = None //TODO Option[TransitionRelation],
    new AuxiliaryMethod(name, params, tpe, tParams, pre, body, (vRet, post))
  }

  private def processSendUpdate(send: DefDef, update: DefDef): TransitionRelation = {
    //need to take care of the mailbox!!
    c.warning(send.pos, "TODO TransitionRelation for the round")
    new TransitionRelation(True(), Nil, Nil)
  }

  

  private def traverseBody(body: List[Tree]) = {
    val acc: (Option[DefDef], Option[DefDef], List[AuxiliaryMethod]) = (None,None,Nil)
    val (snd, upd, aux) = body.foldLeft(acc)( (acc, stmt) => stmt match { 
      case ValDef(_, name, _, _) =>
          c.abort(c.enclosingPosition, "'"+name+"', Round should not contain variable/value definition. Please declare them as LocalVariable in the Algorithm.")
      case d @ DefDef(_, TermName("send"), _, _, _, _) =>
        assert(acc._1.isEmpty)
        (Some(d), acc._2, acc._3)
      case d @ DefDef(_, TermName("update"), _, _, _, _) =>
        assert(acc._2.isEmpty)
        (acc._1, Some(d), acc._3)
      case d @ DefDef(_, name, _, _, _, _) =>
        //ignore the ctor for the moment
        if (name == termNames.CONSTRUCTOR)
          (acc._1, acc._2, acc._3)
        else
          (acc._1, acc._2, auxiliaryFunction(d) :: acc._3)
      case TypeDef(_, _, _, _) =>
        (acc._1, acc._2, acc._3)
    })
    (processSendUpdate(snd.get, upd.get), aux)
  }

  private def mkAuxMap(aux: List[AuxiliaryMethod]): Tree = {
    aux.foldLeft(q"Map.empty[String,round.verification.AuxiliaryMethod]")( (acc, a) => {
      val name = a.name
      q"$acc + ($name -> $a)"
    })
  }

  def extendsRound(t: Tree) = t match {
    case tq"round.Round" => true
    case _ => false
  }
  
  def methodToAdd(tpt: Tree) = List(
      q"""protected def serialize(payload: $tpt, out: ByteBuf, withLength: Boolean = true, offset: Int = 8): Int = {
        if (offset > 0) out.writerIndex(out.writerIndex() + offset)
        val bytes0 = payload.pickle.value
        val length = bytes0.length
        if (withLength) {
          out.writeInt(length.toShort)
          out.writeBytes(bytes0)
          length + 4
        } else {
          out.writeBytes(bytes0)
          length
        }
      }""",
      q"""protected def deserialize(in: ByteBuf, withLength: Boolean = true, offset: Int = 8): $tpt = {
        if (offset > 0) in.readerIndex(in.readerIndex() + offset)
        val length = if (withLength) in.readInt() else in.readableBytes()
        val bytes = Array.ofDim[Byte](length)
        in.readBytes(bytes)
        BinaryPickle(bytes).unpickle[$tpt]
      }"""
    )

  def findTypeParam(body: List[Tree]) = {
    body.collectFirst{ case td @ TypeDef(_, TypeName("A"), _, tpt) => tpt }.get
  }

  object insideRound extends Transformer {
    override def transform(tree: Tree): Tree = {
      super.transform(tree) match {
        case cd @ ClassDef(mods, name, tparams, tmpl @ Template(parents, self, body)) if parents exists extendsRound =>
          val (tr, aux) = traverseBody(body)
          val valTR = q"val rawTR: round.verification.TransitionRelation = $tr"
          val treeAuxMap = mkAuxMap(aux)
          val valAuxMap = q"val auxSpec: Map[String, round.verification.AuxiliaryMethod] = $treeAuxMap"
          val tpt = findTypeParam(body)
          val body2 =  valTR :: valAuxMap :: body ::: methodToAdd(tpt)
          val tmpl2 = treeCopy.Template(tmpl, parents, self, body2)
          treeCopy.ClassDef(cd, mods, name, tparams, tmpl2)
        case other => other
      }
    }
  }

  protected def processRound(t: Tree) = {// t match {
      val tree = insideRound.transform(t)
      tree
      //c.typecheck(tree)
  }

}
