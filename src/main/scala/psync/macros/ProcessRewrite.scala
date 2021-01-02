package psync.macros

import psync.formula._

//TODO clean that part now that we don't use it anymore
trait ProcessRewrite {
  self: Impl =>
  import c.universe._

  //look into c.enclosingClass

  private case class MyVarDef(name: String, tpe: Tree, default: Tree, local: Boolean, ghost: Boolean)

  private def defaultVariables = List(
    MyVarDef("r", tq"psync.Time", q"new psync.Time(-1)", false, false),
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
  
  /*
  private val defaultMethods = List(
    q"protected def incrementRound: Unit = { r = r.tick; _r = r.toInt % rounds.length }",
    q"protected def currentRound: Round = { rounds(_r) }",
    q"def setGroup(g: psync.runtime.Group): Unit = { r = new Time(-1); _r = -1; id = g.self; rounds.foreach(_.setGroup(g)); n = g.size }"
  )
  */


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

        case Select(_, TermName(name)) if map contains name =>
          map(name)
    
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
      val chuncks = s.grouped(50000).toList.map(s => q"builder.append($s)")
      q"""{ val builder = new StringBuilder
            ..$chuncks
            builder.toString
          }"""
    }
  }

  def extractInit(body: Tree): Tree = {
    if (isEnabled) {
      val _f = try {
          makeConstraints(body)
        } catch {
          case e: Exception =>
            c.warning(body.pos, "error while extracting the initial state, leaving it unconstrained.\n" + e)
            True()
        }
      val f = Typer(_f) match {
        case Typer.TypingSuccess(f) =>
          f
        case Typer.TypingFailure(r) =>
          c.warning(body.pos, "unable to type formula corresponding to initial state (leaving it unconstrained): " + _f.toStringFull)
          True()
        case Typer.TypingError(r) =>
          c.abort(body.pos, "formula typer failed on formula corresponding to initial state: " + r)
      }
      q"{ $body; if (psync.Process.fillInitState) initState = Some($f) }"
    } else {
      body
    }
  }

}
