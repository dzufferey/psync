package round.macros

import round.formula._
import round.verification._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

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
        And(And(condCstr, thenCstr), And(Not(condCstr), elseCstr))
     
      case Block(stats, expr) =>
        val statsCstr = stats.map(makeConstraints(_, None, globalRet))
        val retCstr = makeConstraints(expr, currRet, globalRet)
        statsCstr.foldLeft(retCstr)(And(_,_))
     
      case Return(expr) =>
        globalRet match {
          case Some(ret) =>
            makeConstraints(Assign(ret, expr))
          case None =>
            c.abort(c.enclosingPosition, "return with unknown return value")
        }
      
      case Match(selector, cases) =>
        sys.error("TODO")
     
      case Typed(e, _) =>
        makeConstraints(e, currRet, globalRet)
      
      case Assign(lhs, rhs) =>
        sys.error("TODO")
     
      case ValDef(mods, name, tpe, rhs) =>
        makeConstraints(rhs, Some(Ident(name)), globalRet)
     
      case term: TermTree => 
        sys.error("TODO")
     
      case term: RefTree =>
        sys.error("TODO")
      
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

  private def ssa(body: Tree): Tree = { //XXX return a map of ... to ...
    sys.error("TODO")
  }
  
  //DefDef(mods, name, tparams, vparamss, tpt, rhs)

  private def auxiliaryFunctions(d: DefDef): AuxiliaryMethod = {
    sys.error("TODO")
  }

  private def processSend(d: DefDef) = {
    sys.error("TODO")
  }

  private def processUpdate(d: DefDef) = {
    sys.error("TODO")
  }

}
