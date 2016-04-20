package psync.formula

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

//started from the automata tutor code

//TODO replace the Symbol by normalized Formula.

//overloading is not directly supported by the Typer. resolving overloading must be done a priori.

object Typer {

  val level1 = Debug
  val level2 = Info

  sealed abstract class TypingResult[+T <: Formula]{
    def get: T
    def success: Boolean
  }
  case class TypingSuccess[T <: Formula](e: T) extends TypingResult[T] {
    def get = e
    def success = true
  }
  case class TypingFailure(reason: String) extends TypingResult[Nothing] {
    //failed because of not well-formed expression
    def get = throw new java.util.NoSuchElementException("TypingFailure.get")
    def success = false 
  }
  case class TypingError(reason: String) extends TypingResult[Nothing] {
    //failed because of error in the typer
    def get = throw new java.util.NoSuchElementException("TypingError.get")
    def success = false 
  }



  sealed abstract class TypeConstraints {
    def apply(substitution: Map[TypeVariable, Type]): TypeConstraints
    def normalize: TypeConstraints
  }
  case object TrivialCstr extends TypeConstraints {
    def apply(substitution: Map[TypeVariable, Type]) = this
    def normalize = this
  }
  case class SingleCstr(t1: Type, t2: Type) extends TypeConstraints {
    def apply(substitution: Map[TypeVariable, Type]) = {
      SingleCstr(t1 alpha substitution, t2 alpha substitution)
    }
    def normalize = {
      if (t1 == t2) TrivialCstr else {
        (t1, t2) match {
          case (TypeVariable(v1), TypeVariable(v2)) if v2 < v1 => SingleCstr(t2, t1)
          case (other, tv @ TypeVariable(_)) => SingleCstr(tv, other)
          case (_, _) => this
        }
      }
    }
  }
  case class ConjCstr(lst: List[TypeConstraints]) extends TypeConstraints {
    def apply(substitution: Map[TypeVariable, Type]) = {
      ConjCstr(lst map (_(substitution)))
    }
    def normalize = {
      val nonTrivial1 = lst.map(_.normalize).filter(_ != TrivialCstr)
      val nonTrivial2 = nonTrivial1.flatMap{
        case ConjCstr(lst) => lst
        case other => List(other)
      }.toSet.toList
      nonTrivial2 match {
        case Nil => TrivialCstr
        case x :: Nil => x
        case other => ConjCstr(other)
      }
    }
  }
  //case class DisjCstr(lst: List[TypeConstraints]) extends TypeConstraints {
  //  def apply(substitution: Map[TypeVariable, Type]) = {
  //    DisjCstr(lst map (_(substitution)))
  //  }
  //  def normalize = {
  //    val normalized = lst.map(_.normalize)
  //    if (normalized contains TrivialCstr) TrivialCstr
  //    else if (normalized.length == 1) normalized.head
  //    else DisjCstr(normalized)
  //  }
  //}




  //returns a typed formula (with error, wildcards in case of failure)
  //The boolean indicates whether the formula was typed
  def apply(e: Formula): TypingResult[Formula] = {
    //(1) scope and symbols of the Formula
    Logger("Typer", level1, "starting to type " + e)
    val e2 = Simplify.boundVarUnique(e)
    //(2) extract type equations
    val (e3, eqs) = extractEquations(e2)
    if (e3.success) {
      //(3) unifies type equations
      val eqs2 = eqs.normalize
      //Console.println("equations extracted: " + eqs)
      val solution = solveConstraints(eqs2)
      //Console.println("able to solve equations: " + solution.isDefined)
      //(4) uses the type info to resolve the overloading and replace the types
      solution.headOption match {
        case Some(subst) => putTypes(e3.get, subst)
        case None => TypingFailure("cannot solve: " + eqs2)
      }
    } else {
      e3
    }
  }

  def typeLiteral[T](l: Literal[T]): TypingResult[Literal[T]] =  l.value match {
    case _: Boolean => TypingSuccess(l setType Bool)
    case _: scala.Int => TypingSuccess(l setType Int)
    case _: scala.Long => TypingSuccess(l setType Int)
    case _: scala.Short => TypingSuccess(l setType Int)
    case _: scala.Byte => TypingSuccess(l setType Int)
    case () => TypingSuccess(l setType UnitT())
    case other => TypingError("typeOfLiteral: error with " + other + " " + other.asInstanceOf[AnyRef].getClass())
  }

  def extractEquations(e: Formula): (TypingResult[Formula], TypeConstraints) = {

    val symbolToType = scala.collection.mutable.HashMap[String, Type]()
    val varToType = scala.collection.mutable.HashMap[String, Type]()

    def processVariables(v: Variable): (TypingResult[Variable], TypeConstraints) = {
      if (v.tpe == Wildcard) {
        val newTpe = varToType.getOrElse(v.name, Type.freshTypeVar)
        varToType += (v.name -> newTpe)
        Logger("Typer", level1, "variable " + v + " fresh type -> " + newTpe)
        (TypingSuccess(v setType newTpe), TrivialCstr)
      } else {
        val oldTpe = varToType.getOrElse(v.name, v.tpe)
        varToType += (v.name -> oldTpe)
        val cstr = SingleCstr(v.tpe, oldTpe).normalize
        Logger("Typer", level1, "variable " + v + " -> " + cstr)
        (TypingSuccess(v), cstr)
      }
    }

    def processSym(s: Symbol, argsT: List[Type]): (Type, TypeConstraints) = s match {
      case And | Or => //allows variable arity
        val cstr = variableArity(Bool, argsT)
        (Bool, cstr)
      case Plus | Times => //allows variable arity
        val cstr = variableArity(Int, argsT)
        (Int, cstr)
      //TODO
      case Tuple =>
        (Product(argsT), TrivialCstr)
      case Fst =>
        argsT match {
          case List(Product(t)) => (t(0), TrivialCstr)
          case other => (Type.freshTypeVar, TrivialCstr)
        }
      case Snd =>
        argsT match {
          case List(Product(t)) => (t(1), TrivialCstr)
          case other => (Type.freshTypeVar, TrivialCstr)
        }
      case Trd =>
        argsT match {
          case List(Product(t)) => (t(2), TrivialCstr)
          case other => (Type.freshTypeVar, TrivialCstr)
        }
      case _ =>
        s.tpe match {
          case Wildcard =>
            symbolToType.get(s.toString) match {
              case Some(Function(a, r)) =>
                (r, fixedArity(a, argsT))
              case Some(other) =>
                (other, fixedArity(Nil, argsT))
              case None =>
                val r = Type.freshTypeVar
                symbolToType += (s.toString -> Function(argsT,r))
                (r, TrivialCstr)
            }
          case Function(a, r) =>
            (r, fixedArity(a, argsT))
          case other =>
            (other, fixedArity(Nil, argsT))
        }
    }
    
    def fixedArity(tpe: List[Type], args: List[Type]) = {
      ConjCstr((tpe zip args).map({ case (a,b) => SingleCstr(a,b) })).normalize
    }

    def variableArity(tpe: Type, args: List[Type]) = {
      ConjCstr(args.map(t => SingleCstr(tpe, t))).normalize
    }

    def process(e: Formula): (TypingResult[Formula], TypeConstraints) = e match {
      case l @ Literal(_) => (typeLiteral(l), TrivialCstr)

      case v @ Variable(_) => processVariables(v)
      
      case a @ Application(fct, args) =>
        //no overloading, (just polymorphism)
        val (args2, argsCstr) = args.map(process).unzip
        //leave symbol pending until the overloading is resolved
        (args2) find (r => !r.success) match {
          case Some(err) =>
            Logger("Typer", level2, "failed to type: " + err)
            (err, TrivialCstr)
          case None =>
            val unwrappedArgs = args2.map(_.get)
            val argsTypes = unwrappedArgs.map(_.tpe)
            val (returnT, cstr) = processSym(fct, argsTypes)
            val cstrs = if (a.tpe == Wildcard) cstr :: argsCstr
                        else SingleCstr(returnT, a.tpe) :: cstr :: argsCstr
            //val a2 = Application(fct, unwrappedArgs) setType returnT
            val a2 = a setType returnT
            (TypingSuccess(a2), ConjCstr(cstrs).normalize)
        }
      
      case Binding(b, vars, expr) =>
        val (vars2, varsCstr) = vars.map(processVariables(_)).unzip
        val (tpe, exprCstr) = process(expr)
        (tpe :: vars2) find (!_.success) match {
          case Some(err) =>
            Logger("Typer", level2, "failed to type: " + err)
            (err, TrivialCstr)
          case None =>
            val vars3 = vars2.map(_.get)
            val bTpe = b match {
              case ForAll | Exists => Bool
              case Comprehension =>
                val varsT = vars3.map(_.tpe)
                val innerType =
                  if (varsT.length == 1) varsT.head
                  else Product(varsT)
                FSet(innerType)
            }
            val inCstr = SingleCstr(tpe.get.tpe, Bool)
            val tp = TypingSuccess(Binding(b, vars3, tpe.get) setType bTpe)
            val cstr = ConjCstr(inCstr :: exprCstr :: varsCstr).normalize
            Logger("Typer", level1, "binding: " + b + ", tpe: " +bTpe+ ", res:" + cstr)
            (tp, cstr)
        }
    }
    //
    process(e)
  }
  

  def mergeSubst(base: Map[TypeVariable, Type], addition: Map[TypeVariable, Type]): Map[TypeVariable, Type] = {
    //assert(base.keySet.intersect(addition.values.flatMap(_.freeParameters).toSet).isEmpty)
    //semantics:
    //  base.map{ case (t1, t2) => (t1, t2.alpha(addition))} ++ addition
    //more efficient implementation:
    val base2 = base.foldLeft(base)( (acc, kv) => {
      val (k, v) = kv
      val v2 = v.alpha(addition)
      if (v2 != v) acc + (k -> v2) else acc
    })
    base2 ++ addition
  }
  
  def mergeTypeMap(m1: Map[TypeVariable,Type], m2: Option[Map[TypeVariable,Type]]): Option[Map[TypeVariable,Type]] = {
    m1.foldLeft(m2)( (acc, kv) => {
      acc.flatMap( m =>
        if (m.contains(kv._1)) {
          if (m(kv._1) == kv._2) Some(m)
          else None
        } else Some(m + kv) )
    })
  }

  def solveConstraints(eqs: TypeConstraints): List[Map[TypeVariable, Type]] = eqs match {
    case TrivialCstr => List(Map.empty[TypeVariable, Type])
    case SingleCstr(t1, t2) => unify(t1, t2).toList
    case ConjCstr(lst) =>
      (List(Map.empty[TypeVariable, Type]) /: lst)( (acc, cstr) => acc.flatMap( subst => {
        val cstr2 = cstr(subst)
        solveConstraints(cstr2).map( subst2 => mergeSubst(subst, subst2) )
      }))
    //case DisjCstr(lst) => 
    //  lst.flatMap( solveConstraints(_).toList )
  }

  def unify(t1: Type, t2: Type): Option[Map[TypeVariable, Type]] = (t1,t2) match {
    case (Bool, Bool) | (Int, Int) | (Wildcard, _) | (_, Wildcard) =>
      Some(Map.empty[TypeVariable, Type])
    case (v1 @ TypeVariable(n1), v2 @ TypeVariable(n2)) =>
      if (n1 == n2) Some(Map.empty[TypeVariable, Type])
      else if (n1 < n2) Some(Map(v1 -> v2))
      else Some(Map(v2 -> v1))
    case (v1 @ TypeVariable(_), otherType) =>
      if (otherType.freeParameters contains v1) {
        Logger("Typer", level2, "failed to unify: " + t1 + " ⇔ " + t2)
        None
      } else Some(Map(v1 -> otherType))
    case (otherType, v1 @ TypeVariable(_)) =>
      if (otherType.freeParameters contains v1) {
        Logger("Typer", level2, "failed to unify: " + t1 + " ⇔ " + t2)
        None
      } else Some(Map(v1 -> otherType))
    case (UnInterpreted(i1), UnInterpreted(i2)) =>
      if (i1 == i2) Some(Map.empty[TypeVariable, Type])
      else {
        Logger("Typer", level2, "failed to unify: " + t1 + " ⇔ " + t2)
        None
      }
    case (Product(lst1), Product(lst2)) =>
      if (lst1.size == lst2.size) {
        solveConstraints(ConjCstr((lst1 zip lst2).map{ case (t1,t2) => SingleCstr(t1,t2)})).headOption
      } else {
        Logger("Typer", level2, "failed to unify: " + t1 + " ⇔ " + t2)
        None
      }
    case (FOption(s1), FOption(s2)) =>
      unify(s1, s2)
    case (FSet(s1), FSet(s2)) =>
      unify(s1, s2)
    case (FMap(k1,v1), FMap(k2,v2)) =>
      solveConstraints(ConjCstr(List(SingleCstr(k1,k2), SingleCstr(v1,v2)))).headOption
    case (Function(arg1, r1), Function(arg2, r2)) =>
      //Console.println("unifying: " + (FunctionT(arg1, r1), FunctionT(arg2, r2)))
      if (arg1.size == arg2.size)
        solveConstraints(ConjCstr(SingleCstr(r1,r2) :: (arg1 zip arg2).map{ case (t1,t2) => SingleCstr(t1,t2)})).headOption
      else {
        Logger("Typer", level2, "failed to unify: " + t1 + " ⇔ " + t2)
        None
      }
    case _ =>
      Logger("Typer", level2, "failed to unify: " + t1 + " ⇔ " + t2)
      None
  }
  
  def putTypes(e: Formula, subst: Map[TypeVariable, Type]): TypingResult[Formula] = {
    //in the current version, e contains the appropriate type, so no need to check for the smbols
    def addType(e: Formula): Unit = e match {
      case l @ Literal(_) =>
        l.tpe = l.tpe.alpha(subst)
        //assert(l.tpe != Wildcard, "wildcard: " + l)
      case v @ Variable(_) =>
        v.tpe = v.tpe.alpha(subst)
        //assert(v.tpe != Wildcard, "wildcard: " + v)
      case a @ Application(fct, args) =>
        args foreach addType
        a.tpe = a.tpe.alpha(subst)
        //assert(a.tpe != Wildcard, "wildcard: " + a)
      case b @ Binding(_, vars, expr) =>
        addType(expr)
        vars foreach addType
        b.tpe = b.tpe.alpha(subst)
        //assert(b.tpe != Wildcard, "wildcard: " + b)
    }
    try {
      addType(e)
      TypingSuccess(e)
    } catch {
      case err: java.lang.RuntimeException => TypingError(err.toString)
    }
  }

}
