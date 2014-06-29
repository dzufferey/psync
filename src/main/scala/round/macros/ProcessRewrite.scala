package round.macros

import round.formula._

trait ProcessRewrite {
  self: Impl =>
  import c.universe._

  //TODO in the long run we want to make the process extends or contains a SimpleChannelInboundHandler

  //look into c.enclosingClass

  private case class MyVarDef(name: String, tpe: Tree, default: Tree, local: Boolean)

  private def defaultVariables = List(
    MyVarDef("r", tq"Int", q"-1", false),
    MyVarDef("n", tq"Int", q"0", true),
    MyVarDef("HO", tq"Set[ProcessID]", q"Set[ProcessID]()", true)
  )

  private def getVariables(t: Tree): List[MyVarDef] = t match {
    case q"val $tname = new LocalVariable[$tpt]($expr)" =>
      List(MyVarDef(tname.toString, tpt, expr, true))
    case q"val $tname = new GlobalVariable[$tpt]($expr)" =>
      List(MyVarDef(tname.toString, tpt, expr, false))
    case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
      stats.flatMap(getVariables)
    case _ =>
      Nil
  }

  private def mkLocalDecl(mvd: MyVarDef): (ValDef, Ident) = {
    val name = TermName(mvd.name)
    val tpe = mvd.tpe
    val init = mvd.default
    val decl = q"var $name: $tpe = $init"
    val ident = q"$name"
    (decl, ident)
  }
  
  private val defaultMethods = List(
    q"protected def incrementRound: Unit = { r = (r + 1) % rounds.length }",
    q"protected def currentRound: Round[T] = { rounds(r) }",
    q"def setGroup(g: Group): Unit = { rounds.foreach(_.setGroup(g)); n = g.size }"
  )


  //on decl check for name clash
  class insideProcess(map: Map[String, Ident]) extends Transformer {
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


  def processRewrite(t: Tree): Tree = t match {
    //case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
    case q"new ..$parents { ..$body }" =>
      //TODO enclosingClass
      val vars = getVariables(c.enclosingClass) ::: defaultVariables
      //println("got variables: " + vars)
      val (newDefs, idMap) = vars.foldLeft((Nil: List[ValDef],Map.empty[String,Ident]))( (acc, mvd) => {
        val (vdef, id) = mkLocalDecl(mvd)
        (acc._1 :+ vdef, acc._2 + (mvd.name -> id))
      })
      val transformer = new insideProcess(idMap)
      val body2 = newDefs ::: defaultMethods ::: transformer.transformTrees(body)
      //remove attribute and let the typechecker run again
      val tree = q"new ..$parents { ..$body2 }"
      c.untypecheck(tree)
      //tree
    case _ =>
      c.abort(c.enclosingPosition, "'p' should be applied to class definition: p(new Process{ ... })")
  }

}
