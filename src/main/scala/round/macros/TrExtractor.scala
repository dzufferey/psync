package round.macros

import round.formula._
import round.verification._
import round.utils.Namer

trait TrExtractor {
  self: Impl =>
  import c.universe._

  //TODO from the round extract constraints representing the transition relation (TR)
  
  //model for the mailbox: M(i,j) means i sends to j at this round.
  //need to connect what is send to what is received
  //the must be SSA (no loop the modifies variables) //please put that in methods outside the send/receive


  private def getPostCondition(body: Tree): Option[(Tree, Variable, Formula)] = body match {
    case q"scala.Predef.Ensuring[$tpt]($expr).ensuring( $ret => $postCond )" =>
      //TODO about tpt
      val v = ret match {
        //TODO
        case smth => sys.error("TODO Variable: " + showRaw(ret))
      }
      val f = tree2Formula(postCond)
      Some((expr,v,f))
    case _ => None
  }

  private def getPreCondition(body: Tree): Option[(Tree, Formula)] = body match {
    case q"{ scala.Predef.require(..$args); ..$exprs }" =>
      val f = tree2Formula(args.head)
      //there might be a 2nd Srting arg that describes what the precond is about
      Some(Block(exprs.init, exprs.last), f)
    case _ => None
  }

  private def makeConstraints(
      body: Tree,
      currRet: Option[Tree] = None,
      globalRet: Option[Tree] = None
    ): Formula =
  {
    body match {
     
      case If(cond, thenp, elsep) =>
        val condCstr = makeConstraints(cond, None, None)
        val thenCstr = makeConstraints(thenp, currRet, globalRet)
        val elseCstr = makeConstraints(elsep, currRet, globalRet)
        Or(And(condCstr, thenCstr), And(Not(condCstr), elseCstr))
     
      case Block(stats, expr) =>
        val statsCstr = stats.map(makeConstraints(_, None, globalRet))
        val retCstr = makeConstraints(expr, currRet, globalRet)
        statsCstr.foldRight(retCstr)(And(_,_))
     
      case Return(expr) =>
        makeConstraints(Assign(globalRet.get, expr))
      
      case Typed(e, _) =>
        makeConstraints(e, currRet, globalRet)
      
      case Apply(Select(lhs, TermName("$less$tilde")), List(rhs)) =>
        Eq(tree2Formula(lhs), tree2Formula(rhs))
      case Assign(lhs, rhs) =>
        Eq(tree2Formula(lhs), tree2Formula(rhs))
     
      case ValDef(mods, name, tpe, rhs) =>
        makeConstraints(rhs, Some(Ident(name)), globalRet)
     
      case term: TermTree => 
        makeConstraints(Assign(currRet.get, term))
     
      case term: RefTree =>
        makeConstraints(Assign(currRet.get, term))
      
      case Match(selector, cases) =>
        c.abort(c.enclosingPosition, "pattern matching an algebraic datatypes not yet supported")
      //for loops
      case LabelDef(name, params, rhs) =>
        c.abort(c.enclosingPosition, "while loop not yet supported")
      //case Apply(id @ Ident(_), paramss) if id.symbol.isLabel =>
      //  c.abort(c.enclosingPosition, "loop not yet supported")
      case Try(block, catches, finalizer) =>
        c.abort(c.enclosingPosition, "try/catch yet supported")
      case Throw(expr) =>
        c.abort(c.enclosingPosition, "throwing exception not yet supported")
     
      case other =>
        c.abort(c.enclosingPosition, "makeConstraints, did not expect: " + other)
    }
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

  //returns the body in SSA and a map of tree to the different version
  private def ssa(body: List[Tree]): (List[Tree], Map[Tree, List[Tree]]) = {
    //XXX return a map of Tree to version #
    sys.error("TODO")
  }
  
  //DefDef(mods, name, tparams, vparamss, tpt, rhs)

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
    //sys.error("TODO")
    new TransitionRelation(True(), Nil, Nil)
  }

  //ClassDef(mods, name, tparams, Template(parents, self, body))
  
  val ioStuff = Set("ftt", "spkl", "upkl")

  private def traverseBody(body: List[Tree]) = {
    val acc: (Option[DefDef], Option[DefDef], List[AuxiliaryMethod]) = (None,None,Nil)
    val (snd, upd, aux) = body.foldLeft(acc)( (acc, stmt) => stmt match { 
      case ValDef(_, name, _, _) =>
        if (ioStuff contains name.toString.trim) {
          (acc._1, acc._2, acc._3)
        } else {
          c.abort(c.enclosingPosition, "'"+name+"', Round should not contain variable/value definition. Please declare them as LocalVariable in the Algorithm.")
        }
      case d @ DefDef(_, TermName("send"), _, _, _, _) =>
        assert(acc._1.isEmpty)
        (Some(d), acc._2, acc._3)
      case d @ DefDef(_, TermName("update"), _, _, _, _) =>
        assert(acc._2.isEmpty)
        (acc._1, Some(d), acc._3)
      case d @ DefDef(_, name, _, _, _, _) =>
        //ignore the ctor for the moment
        if (name == termNames.CONSTRUCTOR || ioStuff.contains(name.toString.trim))
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
          //val tmpl2 = treeCopy.Template(tmpl, parents, self, body2)
          //treeCopy.ClassDef(cd, mods, name, tparams, tmpl2)
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
    //case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
    //case q"new ..$parents { ..$body }" =>
      //val tree = q"new ..$parents { ..$body2 }"
    //case _ =>
    //  c.abort(c.enclosingPosition, "definition of round did not match expected pattern: 'new Round[$tpt] { ..$body }'")
  }

}
