package round.formula

//Ctrl-C, Ctrl-V from that automata tutor code

//TODO replace the Symbol by normalized Formula.

object Typer {

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
    def normalize = this
  }
  case class ConjCstr(lst: List[TypeConstraints]) extends TypeConstraints {
    def apply(substitution: Map[TypeVariable, Type]) = {
      ConjCstr(lst map (_(substitution)))
    }
    def normalize = {
      val nonTrivial = lst.map(_.normalize).filter(_ != TrivialCstr)
      nonTrivial match {
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
    //Console.println("starting to type " + e)
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

  def typeLiteral[T <: AnyVal](l: Literal[T]): TypingResult[Literal[T]] =  l.value match {
    case _: Boolean => TypingSuccess(l setType Bool)
    case _: scala.Int => TypingSuccess(l setType Int)
    case other => TypingError("typeOfLiteral: error with " + other + " " + other.asInstanceOf[AnyRef].getClass())
  }

  def extractEquations(e: Formula): (TypingResult[Formula], TypeConstraints) = {

    val symbolToType = scala.collection.mutable.HashMap[String, Type]()

    def processVariables(v: Variable): (TypingResult[Variable], TypeConstraints) = {
      if (v.tpe == Wildcard) {
        var newTpe = symbolToType.getOrElse(v.name, Type.freshTypeVar)
        symbolToType += (v.name -> newTpe)
        //Console.println("fresh type for " + v + " " + v.symbol + " -> " + newTpe)
        (TypingSuccess(v setType newTpe), TrivialCstr)
      } else {
        var oldTpe = symbolToType.getOrElse(v.name, v.tpe)
        symbolToType += (v.name -> oldTpe)
        //Console.println(v + " -> " + SingleCstr(v.tpe, oldTpe))
        (TypingSuccess(v), SingleCstr(v.tpe, oldTpe))
      }
    }

    def process(e: Formula): (TypingResult[Formula], TypeConstraints) = e match {
      case l @ Literal(_) => (typeLiteral(l), TrivialCstr)

      case v @ Variable(_) => processVariables(v)
      
      case a @ Application(fct, args) =>
        //TODO no overloading! (just polymorphism)
        val (args2, argsCstr) = args.map(process).unzip
        //leave symbol pending until the overloading is resolved
        (args2) find (r => !r.success) match {
          case Some(err) => (err, TrivialCstr)
          case None =>
            val unwrappedArgs = args2.map(_.get)
            val returnT = Type.freshTypeVar
            val argsTypes = unwrappedArgs.map(_.tpe)
            val a2 = Application(fct, unwrappedArgs) setType returnT
            val (argsType, returnType) = fct.tpe match {
              case Function(a, r) => (a,r)
              case other => (Nil, other)
            }
            val returnCstr = SingleCstr(returnT, returnType)
            val appCstr = argsType zip argsTypes map { case (a,b) => SingleCstr(a,b) }
            //Console.println(a + " -> " + argsCstrs + " -> " + returnCstr)
            val cstr = ConjCstr(returnCstr :: appCstr ::: argsCstr)
            (TypingSuccess(a2), cstr)
        }
      
      case Binding(b, vars, expr) =>
        val (vars2, varsCstr) = vars.map(processVariables(_)).unzip
        val (tpe, exprCstr) = process(expr)
        (tpe :: vars2) find (!_.success) match {
          case Some(err) => (err, TrivialCstr)
          case None =>
            val vars3 = vars2.map(_.get)
            val bTpe = b match {
              case ForAll | Exists => Bool
              case Comprehension =>
                val varsT = vars3.map(_.tpe)
                if (varsT.length == 1) varsT.head
                else Product(varsT)
            }
            val inCstr = SingleCstr(tpe.get.tpe, Bool)
            val tp = TypingSuccess(Binding(b, vars3, tpe.get) setType bTpe)
            val cstr = ConjCstr(inCstr :: exprCstr :: varsCstr)
            (tp, cstr)
        }
        
    }
    //
    process(e)
  }
  

  def mergeSubst(base: Map[TypeVariable, Type], addition: Map[TypeVariable, Type]): Map[TypeVariable, Type] = {
    assert(base.keySet.intersect(addition.values.flatMap(_.freeParameters).toSet).isEmpty)
    base.map{ case (t1, t2) => (t1, t2.alpha(addition))} ++ addition
  }

  def solveConstraints(eqs: TypeConstraints): List[Map[TypeVariable, Type]] = eqs match {
    case TrivialCstr => List(Map.empty[TypeVariable, Type])
    case SingleCstr(t1, t2) => unify(t1, t2).toList
    case ConjCstr(lst) =>
      //TODO adapt to List
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
      Some(if (n1 == n2) Map.empty[TypeVariable, Type] else Map(v1 -> v2))
    case (v1 @ TypeVariable(_), otherType) =>
      if (otherType.freeParameters contains v1) None
      else Some(Map(v1 -> otherType))
    case (otherType, v1 @ TypeVariable(_)) =>
      if (otherType.freeParameters contains v1) None else
      Some(Map(v1 -> otherType))
    case (FOption(s1), FOption(s2)) =>
      unify(s1, s2)
    case (FSet(s1), FSet(s2)) =>
      unify(s1, s2)
    case (Function(arg1, r1), Function(arg2, r2)) =>
      //Console.println("unifying: " + (FunctionT(arg1, r1), FunctionT(arg2, r2)))
      if (arg1.size == arg2.size)
        solveConstraints(ConjCstr(SingleCstr(r1,r2) :: (arg1 zip arg2).map{ case (t1,t2) => SingleCstr(t1,t2)})).headOption
      else None
    case _ => None
  }
  

  //TODO
  def putTypes(e: Formula, subst: Map[TypeVariable, Type]): TypingResult[Formula] = {
    //in the current version, e contains the appropriate type, so no need to check for the smbols
    def addType(e: Formula): Unit = e match {
      case l @ Literal(_) => l.tpe = l.tpe.alpha(subst)
      case v @ Variable(_) => v.tpe = v.tpe.alpha(subst)
      case a @ Application(fct, args) =>
        args foreach addType
        a.tpe = a.tpe.alpha(subst)
      case Binding(_, vars, expr) =>
        addType(expr)
        vars foreach addType
    }
    try {
      addType(e)
      TypingSuccess(e)
    } catch {
      case err: java.lang.RuntimeException => TypingError(err.toString)
    }
  }

}
