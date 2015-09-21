package round.macros

import round.formula._

trait ProcessRewrite {
  self: Impl =>
  import c.universe._

  //TODO in the long run we want to make the process extends or contains a SimpleChannelInboundHandler

  //look into c.enclosingClass

  private case class MyVarDef(name: String, tpe: Tree, default: Tree, local: Boolean, ghost: Boolean)

  private def defaultVariables = List(
    MyVarDef("r", tq"Int", q"-1", false, false),
    MyVarDef("_r", tq"Int", q"-1", false, false),
    MyVarDef("n", tq"Int", q"0", false, false),
    MyVarDef("HO", tq"Set[ProcessID]", q"Set[ProcessID]()", true, true)
  )

  private def getVariables(t: Tree): List[MyVarDef] = t match {
    case q"val $tname = new LocalVariable[$tpt]($expr)" =>
      List(MyVarDef(tname.toString, tpt, expr, true, false))
    case q"val $tname = new GhostVariable[$tpt]($expr)" =>
      List(MyVarDef(tname.toString, tpt, expr, true, true))
    case q"val $tname = new GlobalVariable[$tpt]($expr)" =>
      List(MyVarDef(tname.toString, tpt, expr, false, false))
    case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
      stats.flatMap(getVariables)
    case _ =>
      Nil
  }

  private def mkLocalDecl(mvd: MyVarDef): (ValDef, Ident) = {
    val name = TermName(mvd.name)
    val tpe = mvd.tpe
    val init = mvd.default
    //val decl = q"@volatile var $name: $tpe = $init"
    val decl = q"var $name: $tpe = $init"
    val ident = q"$name"
    (decl, ident)
  }
  
  private val defaultMethods = List(
    q"protected def incrementRound: Unit = { r = (r + 1); _r = r % rounds.length }",
    q"protected def currentRound: Round = { rounds(_r) }",
    q"def setGroup(g: round.runtime.Group): Unit = { r = -1; _r = -1; id = g.self; rounds.foreach(_.setGroup(g)); n = g.size }"
  )


  //on decl check for name clash
  class InsideProcess(map: Map[String, Ident]) extends Transformer {
    override def transform(tree: Tree): Tree = {
      super.transform(tree) match {
        case Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("getter")), List(TypeTree())), List(expr)) =>
          expr

        case Apply(Select(lhs, TermName("$less$tilde")), List(rhs)) =>
          Assign(lhs, rhs)

        case Select(_, TermName("HO")) =>
          c.abort(c.enclosingPosition, "HO should be used only in the specification")

        case Select(_, TermName(name)) if map contains name.toString =>
          map(name.toString)
    
        case Ident(name) if map contains name.toString =>
          map(name.toString)
        
        case ValDef(_, name, _, _) if map contains name.toString =>
          c.abort(c.enclosingPosition, "compiler not yet hygienic, please do not reuse " + name)

        case DefDef(_, name, _, _, _, _) if map contains name.toString =>
          c.abort(c.enclosingPosition, "compiler not yet hygienic, please do not reuse " + name)

      //case q"$expr.value" => //might be a GlobalVariable, TODO need to inspect the type
      //  val expr2 = insideProcess(map, expr)
      //  if (expr != expr2) expr2 else t
      //
      //case q"$expr.get" => //might be a LocalVariable, TODO need to inspect the type
      //  val expr2 = insideProcess(map, expr)
      //  if (expr != expr2) expr2 else t

        case other => other
      }
    }
  }

  def getInitState(ts: List[Tree]): Formula = {
    findMethod(ts, "init") match {
      case Some(ddef) =>
        try {
          //TODO capturing args
          val f = makeConstraints(ddef.rhs)
          FormulaUtils.map({
            case v @ Variable(_) => removeProcTypeArg(v)
            case other => other
          }, f)
        } catch {
          case e: Exception =>
            c.warning(ddef.pos, "error while extracting the initial state, leaving it unconstrained.\n" + e)
            True()
        }
      case None =>
        c.warning(ts.head.pos, "did not find 'init' method, leav the initial state unconstrained.")
        True()
    }
  }

  def globalList(decls: List[MyVarDef]) = {
    decls.filter(!_.local).map( d => Variable(d.name).setType(extractType(d.tpe)))
  }
  def localList(decls: List[MyVarDef]) = {
    decls.filter(v => v.local && !v.ghost).map( d => Variable(d.name).setType(extractType(d.tpe)))
  }
  def ghostList(decls: List[MyVarDef]) = {
    decls.filter(v => v.local && v.ghost).map( d => Variable(d.name).setType(extractType(d.tpe)))
  }

  protected def emitNonOverflowingString(s: String): Tree = {
    if (s.length < 60000) {
      q"$s"
    } else {
      val chuncks = s.grouped(10000).toList.map(s => q"builder.append($s)")
      q"""{ val builder = new StringBuilder
            ..$chuncks
            builder.toString
          }"""
    }
  }

  def processRewrite(t: Tree): Tree = t match {
    //TODO make sure it is a Process
    //TODO can we add type parameters to allow generic consensus
    case q"new ..$parents { ..$body }" =>
      //TODO enclosingClass
      val vars = getVariables(c.enclosingClass) ::: defaultVariables
      val implVars = vars.filter( !_.ghost ) 
      val (newDefs, idMap) = implVars.foldLeft((Nil: List[ValDef],Map.empty[String,Ident]))( (acc, mvd) => {
        val (vdef, id) = mkLocalDecl(mvd)
        (acc._1 :+ vdef, acc._2 + (mvd.name -> id))
      })
      val transformer = new InsideProcess(idMap)
      //
      val _f = getInitState(body)
      val f = Typer(_f) match {
        case Typer.TypingSuccess(f) =>
          f
        case Typer.TypingFailure(r) =>
          c.abort(body.head.pos, "unable to type formula corresponding to initial state: " + _f.toStringFull)
        case Typer.TypingError(r) =>
          c.abort(body.head.pos, "formula typer failed on formula corresponding to initial state: " + r)
      }
      val init = q"val initState: round.formula.Formula = $f"
      val v1 = globalList(vars).map(_liftF)
      val v2 = localList(vars).map(_liftF)
      val v3 = ghostList(vars).map(_liftF)
      val _v1 = q"val globalVariables: List[round.formula.Variable] = $v1" 
      val _v2 = q"val localVariables: List[round.formula.Variable] = $v2" 
      val _v3 = q"val ghostVariables: List[round.formula.Variable] = $v3" 
      //
      val body2 = init :: _v1 :: _v2 :: _v3 :: newDefs ::: defaultMethods ::: transformer.transformTrees(body)
      val tree = q"new ..$parents { ..$body2 }"
      //
      val s1 = emitNonOverflowingString(t.toString)
      val s2 = emitNonOverflowingString(tree.toString)
      val _s1 = q"val beforeProcessing: String = $s1"
      val _s2 = q"val afterProcessing: String = $s2"
      //
      val body3 = _s1 :: _s2 :: body2
      val tree2 = q"new ..$parents { ..$body3 }"
      c.untypecheck(tree2)
    case _ =>
      c.abort(c.enclosingPosition, "'p' should be applied to class definition: p(new Process{ ... })")
  }

}
