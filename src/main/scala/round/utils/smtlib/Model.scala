package round.utils.smtlib

import round.formula._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

sealed abstract class Def
sealed abstract class ValDef extends Def
case class ValI(i: Long) extends ValDef { override def toString = i.toString }
case class ValB(b: Boolean) extends ValDef { override def toString = b.toString }
case class ValExt(idx: Int, tpe: Type) extends ValDef { override def toString = tpe+"!"+idx }
case class FunDef(defs: List[(List[ValDef], ValDef)], default: ValDef) extends Def

object Def {

  def eval(f: FunDef,  args: List[ValDef]): ValDef = {
    def same(c: List[ValDef]): Boolean = {
      c.zip(args).forall{ case (a,b) => a == b }
    }
    f.defs.find{ case (args, _) => same(args) } match {
      case Some(s) => s._2
      case None => f.default
    }
  }
  
  def uneval(domains: Map[Type, Set[ValExt]], d: Def, ret: ValDef): List[List[ValDef]] = d match {
    case v: ValDef =>
      if (v == ret) List() else Nil
    case f: FunDef =>
      val mtch = f.defs.filter(_._2 == ret).map(_._1)
        if (!mtch.isEmpty)
          mtch
        else {
          if (ret == f.default) {
            val args = f.defs.head._1.map{
              case ValB(_) => List(ValB(true), ValB(false))
              case ValI(_) => sys.error("cannot complement integers")
              case ValExt(_, tpe) => domains(tpe).toList
            }
            val allArgs = Misc.cartesianProduct(args)
            allArgs.flatMap( seq => {
              val lst = seq.toList
              if (f.defs.exists( _._1 == lst )) None
              else Some(lst)
            }).toList
          } else {
            Nil
          }
        }
  }

}


class Model(domains: Map[Type, Set[ValExt]],
            interpretation: Map[Symbol, Def])
{

  def get(s: Symbol, args: ValDef*): Option[ValDef] = {
    val aLst = args.toList
    interpretation.get(s).map( _ match {
      case v: ValDef => v
      case f: FunDef => Def.eval(f, aLst)
    })
  }

  def apply(s: Symbol, args: ValDef*): ValDef = get(s, args:_*).get
  

  override def toString = {
    val buffer = new StringBuilder
    buffer.append("model\n")
    buffer.append("  domains:\n")
    for ( (t, vals) <- domains ) {
      buffer.append("    " + t + ": " + vals.mkString(", "))
      buffer.append("\n")
    }
    buffer.append("  interpretation:\n")
    for ( (f, df) <- interpretation) {
      df match {
        case v: ValDef =>
          buffer.append("    " + f + " = " + v + "\n")
        case FunDef(defs, default) =>
          for ((args, v) <- defs) {
          buffer.append("    " + f + args.mkString("(",", ",") = ") + v + "\n")
          }
          buffer.append("    " + f + "(_) = " + default + "\n")
      }
    }
    buffer.toString
  }

}

object Model {


  def apply(cmds: List[Command], declared: Iterable[(Symbol, List[Type])]) = {

    val values: Map[String, ValExt] = (cmds.collect{
      case DeclareFun(id, Function(Nil, tpe)) =>
        (id -> ValExt(id.split("!").last.toInt, tpe))
    }).toMap
    val domains = values.values.groupBy(_.tpe).map{ case (k, v) => (k, v.toSet) }

    val toSym = declared.foldLeft(Map[String, Symbol]())( (acc, decl) => {
      acc + (Names.overloadedSymbol(decl._1, decl._2) -> decl._1)
    })
    //println("toSym: " + toSym)

    def tryParseVal(f: Formula): Option[ValDef] = f match {
      case Literal(b: Boolean) => Some(ValB(b))
      case Literal(l: Long) => Some(ValI(l))
      case Variable(id) => values get id
      case _ => None
    }
 
    def parseCase(args: Formula, ret: Formula): (List[ValDef], ValDef) = {
      val args2 = args match {
        case And(cs) => cs
        case other => List(other)
      }
      val args3 = args2.map( _ match {
          case Eq(List(_, v)) => tryParseVal(v).get
          case other => sys.error("expected Eq, found: " + other)
      })
      (args3, tryParseVal(ret).get)
    }

    def getSym(id: String): Symbol = {
      if (toSym contains id) toSym(id) else UnInterpretedFct(id)
    }
 
    def tryParseFun(d: DefineFun): Option[(Symbol, Def)] = {
      val sym = getSym(d.id)
      val (cases, default) = collectCases(d.body)
      tryParseVal(default) match {
        case Some(v) =>
          val cases2 = cases map { case (args, v) => parseCase(args, v) }
          Some(sym, if (cases2.isEmpty) v else FunDef(cases2, v) )
        case None => None
      }
    }


    //first pass
    val fstPass: List[Either[(Symbol, Def), DefineFun]] = cmds collect {
      case d: DefineFun =>
        tryParseFun(d) match {
          case Some(s) => Left(s)
          case None => Right(d)
        }
    }

    val (_defs, _rest) = fstPass partition {
      case Left(_) => true
      case Right(_) => false
    }
    var defs = _defs.map{ case Left(l) => l; case _ => sys.error("??") }.toMap
    var rest = _rest map { case Right(l) => l; case _ => sys.error("??") }
    
    def tryFillDef(d: DefineFun): Option[(Symbol, Def)] = {
      def inline(symbol: Symbol, args: List[Option[Symbol]]): Def = {
        defs(symbol) match {
          case v: ValDef => v
          case FunDef(cases, default) =>
            def invert(vals: List[ValDef]): List[List[ValDef]] = {
              val vals2 = args.zip(vals).map{
                case (Some(s), v) => Def.uneval(domains, defs(s), v).flatten //assume single arg
                case (None, v) => List(v)
              }
              val cart = Misc.cartesianProduct(vals2)
              cart.toList.map(_.toList)
            }
            val cases2 = cases.flatMap( c => {
              val lst = invert(c._1)
              lst.map(_ -> c._2)
            })
            FunDef(cases2, default)
        }
      }
      try {
        d.body match {
          case Application(UnInterpretedFct(s, _, _), args) =>
            val args2 = args map {
              case Application(sym, List(_)) => Some(sym)
              case Variable(_) => None
              case _ => sys.error("??")
            }
            Some(getSym(d.id) -> inline(getSym(s), args2))
          case _ => sys.error("??")
        }
      } catch {
        case e: Exception => None
      }
    }


    //second pass for functions defined with other funs ...
    while(!rest.isEmpty) {
      var progress = false
      val r = rest
      rest = Nil
      for (d <- r) {
        tryFillDef(d) match {
          case Some(d) =>
            defs = defs + d
            progress = true
          case None =>
           rest = d :: rest
        }
      }
      if (!progress) {
        sys.error("cannot reconstruct model: " + rest)
      }
    }
 
    //remove the fct introduced by the solver
    val defined = toSym.values.toSet
    val there = defs filter { case (s, _) => defined(s) }

    new Model(domains, there)
  }

  private def collectCases(f: Formula): (List[(Formula, Formula)], Formula) = f match {
    case Application(UnInterpretedFct("ite",_,_), List(cnd, tr, fa)) =>
      val (acc, other) = collectCases(fa)
      ((cnd, tr) :: acc, other)
    case other => (Nil, other)
  }

}
