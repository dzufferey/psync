package round.formula

import dzufferey.utils.Namer

object Simplify {

  //removes functions that can be expressed in terms of other
  def normalize(f: Formula) = normalizer.transform(f)
  private val normalizer = new Mapper(normalizef)
  private def normalizef(f: Formula): Formula = f match {
    case Implies(a,b) => Copier.Application(f, Or, List(Copier.Application(a, Not, List(a)),b))
    case Neq(a,b) => Copier.Application(f, Not, List(Copier.Application(f, Eq, List(a,b))))
    case Geq(a,b) => Copier.Application(f, Not, List(Copier.Application(f, Lt, List(a,b))))
    case Leq(a,b) => Copier.Application(f, Not, List(Copier.Application(f, Lt, List(b,a))))
    case Gt(a,b) =>  Copier.Application(f, Lt, List(b,a))
    case SupersetEq(a,b) => Copier.Application(f, SubsetEq, List(b,a))
    case Contains(a,b) => Copier.Application(f, In, List(b,a))
    case IsEmpty(a) => Copier.Application(f, Not, List(Copier.Application(f, IsDefined, List(a))))
    case other => other
  }


  //negation normal form (assumes normalized formula)
  //TODO also recurse in Comprehension
  def nnf(f: Formula, neg: Boolean = false): Formula = f match {
    case Binding(ForAll, vs, f2) =>
      val bt = if (neg) Exists else ForAll
      Copier.Binding(f, bt, vs, nnf(f2, neg))
    case Binding(Exists, vs, f2) =>
      val bt = if (neg) ForAll else Exists
      Copier.Binding(f, bt, vs, nnf(f2, neg))
    case Application(Not, List(f2)) =>
      nnf(f2, !neg)
    case Application(And, args) => 
      val fct = if (neg) Or else And
      val args2 = args.map(nnf(_, neg))
      Copier.Application(f, fct, args2)
    case Application(Or, args) => 
      val fct = if (neg) And else Or
      val args2 = args.map(nnf(_, neg))
      Copier.Application(f, fct, args2)
    case other =>
      assert(other.tpe == Bool)
      if (neg) Not(other) else other
  }

  def cnf(f: Formula): Formula = {
    ???
  }
  
  def dnf(f: Formula): Formula = {
    ???
  }

  def deBruijnIndex(f0: Formula, renameFreeVars: Boolean = false): Formula = {
    def mkVar(tpe: Type, idx: Int) = {
      val prefix = round.utils.smtlib.Names.tpe(tpe)
      Variable(prefix + "_" + idx).setType(tpe)
    }

    //generic renaming of variables _XXX
    val f = boundVarUnique(f0)
    val allVars = if (renameFreeVars) f.freeVariables ++ f.boundVariables else f.boundVariables
    val dummyNames = allVars.foldLeft(Map[Variable,Variable]())( (acc, v) => acc + (v -> Variable(Namer("_")).setType(v.tpe)) )
    val cleanNames = FormulaUtils.alphaAll(dummyNames, f)

    def merge(m1: Map[Type, Int], m2: Map[Type, Int]): Map[Type, Int] = {
      m2.foldLeft(m1)( (acc, k) => acc + (k._1 -> math.max(k._2, acc.getOrElse(k._1, 0))) )
    }

    //renaming
    def assignNames(f: Formula): (Map[Type, Int], Formula) = f match {
      case Literal(_) | Variable(_) => (Map(), f)
      case Application(fct, args) =>
        val (maps, args2) = args.map(assignNames).unzip
        val map = maps.foldLeft(Map[Type,Int]())(merge(_, _))
        (map, Copier.Application(f, fct, args2).setType(f.tpe))
      case Binding(bt, vs, f2) =>
        val (map, f3) = assignNames(f2)
        val (vs2, map2) = dzufferey.utils.Misc.mapFold(vs, map, (v: Variable, acc: Map[Type, Int]) => {
          val idx = 1 + acc.getOrElse(v.tpe, 0)
          val acc2 = acc + (v.tpe -> idx)
          (mkVar(v.tpe, idx), acc2)
        })
        val subst = vs.zip(vs2).toMap
        val f4 = FormulaUtils.alpha(subst, f3)
        (map2, Copier.Binding(f, bt, vs2, f4).setType(f.tpe))
    }

    assignNames(cleanNames)._2
  }
  

  //prenex normal form
  //TODO also recurse in Comprehension ?
  def pnf(f: Formula) = {
    val f2 = boundVarUnique(nnf(f))
    def merge(lst: List[List[(BindingType, List[Variable])]], exists: Boolean): List[(BindingType, List[Variable])] = {
      val (vs, rest) = lst.foldLeft( (List[Variable](), List[List[(BindingType, List[Variable])]]()) )( (acc, f) => {
        f match {
          case (ForAll, vs) :: xs => if (!exists) (vs ::: acc._1, xs :: acc._2) else (acc._1, f :: acc._2)
          case (Exists, vs) :: xs => if ( exists) (vs ::: acc._1, xs :: acc._2) else (acc._1, f :: acc._2)
          case Nil => acc
          case _ => sys.error("should not happen")
        }
      })
      val suffix = if (!rest.isEmpty) merge(rest, !exists) else Nil
      if (vs.isEmpty) suffix
      else if (exists) (Exists, vs) :: suffix
      else (ForAll, vs) :: suffix
    }
    def pull(f: Formula, exists: Boolean): (List[(BindingType, List[Variable])], Formula) = f match {
      case Binding(ForAll, vs, f2) =>
        val (b1, f3) = pull(f2, false)
        val b2 = b1 match {
          case (ForAll, vs2) :: bs => (ForAll, vs ::: vs2) :: bs
          case _ => (ForAll, vs) :: b1 
        }
        (b2, f3)

      case Binding(Exists, vs, f2) =>
        val (b1, f3) = pull(f2, true)
        val b2 = b1 match {
          case (Exists, vs2) :: bs => (Exists, vs ::: vs2) :: bs
          case _ => (Exists, vs) :: b1 
        }
        (b2, f3)

      case Application(And, args) => 
        val (binders, args2) = args.map(pull(_, exists)).unzip
        val merged = merge(binders, exists)
        val f2 = Copier.Application(f, And, args2)
        (merged, f2)

      case Application(Or, args) => 
        val (binders, args2) = args.map(pull(_, exists)).unzip
        val merged = merge(binders, exists)
        val f2 = Copier.Application(f, Or, args2)
        (merged, f2)

      case other =>
        (Nil, other)
    }
    val (quantifiers, qf) = pull(f2, true)
    FormulaUtils.restoreQuantifierPrefix(quantifiers, qf)
  }

  def isPnf(f: Formula): Boolean = {
    def rmQ(f: Formula): Formula = f match {
      case Binding(ForAll | Exists, _, f) => rmQ(f)
      case f => f
    }
    def hasQ(f: Formula): Boolean = f match {
      case Binding(ForAll | Exists, _, _) => true
      case Application(_, args) => args.exists(hasQ)
      case _ => false
    }
    !hasQ(rmQ(f))
  }

  /** try to push the quantifiers as low as possible. */
  def reversePnf(f: Formula): Formula = {
    def partition(vars: Set[Variable],
                  fs: Seq[Formula],
                  mkApp: Symbol,
                  mkQuant: (List[Variable], Formula) => Formula): Seq[Formula] = {
      def span(v: Variable): Int = fs.foldLeft(0)( (acc,f) => if (f.freeVariables contains v) acc + 1 else acc )
      def nonOverlapingSpan(vs: Set[Variable]): (Set[Variable], Seq[Formula], Seq[Formula]) = {
        val (fIn,fOut) = fs.partition(f => !f.freeVariables.intersect(vs).isEmpty)
        val overlapping = vars.filter( v => {
          fIn.exists(_.freeVariables(v)) &&
          fOut.exists(_.freeVariables(v))
        })
        if (overlapping.size > vs.size) nonOverlapingSpan(overlapping)
        else {
          val same = vars.filter(v => fIn.forall(_.freeVariables(v)))
          assert(vs.subsetOf(same))
          (same, fIn, fOut)
        }
      }

      if (vars.isEmpty) {
        fs
      } else {
        val largest = vars.maxBy(span)
        val (currSpan, fIn, fOut) = nonOverlapingSpan(Set(largest))
        val remaining = vars -- currSpan
        val (strictlyIn, outside) = remaining.partition{ v =>
          val sIn = fIn.exists( _.freeVariables(v)) &&
                    fIn.exists(!_.freeVariables(v))
          if (sIn) assert(fOut.forall(!_.freeVariables(v)))
          else fOut.exists( _.freeVariables(v))
          sIn
        }
        val fIn2 = partition(strictlyIn, fIn, mkApp, mkQuant)
        val fOut2 = partition(outside, fOut, mkApp, mkQuant)
        mkQuant(currSpan.toList, mkApp(fIn2:_*)) +: fOut2
      }
    }
    def pushDown(f: Formula): Formula = f match {
      case ForAll(vars, And(conjs @ _*)) =>
        And(conjs.map( c => {
          val vars2 = vars.filter(c.freeVariables)
          if (vars2.isEmpty) c
          else ForAll(vars2, c)
        }):_*)
      case Exists(vars, Or(disjs @ _*)) =>
        Or(disjs.map( c => {
          val vars2 = vars.filter(c.freeVariables)
          if (vars2.isEmpty) c
          else Exists(vars2, c)
        }):_*)
      case Exists(vars, And(conjs @ _*)) =>
        val (c1,c2) = conjs.partition(c => c.freeVariables.exists(vars contains _))
        val c1p = partition(vars.toSet, c1, And, Exists.apply)
        And(c1p ++ c2 :_*)
      case ForAll(vars, Or(disjs @ _*)) =>
        val (c1,c2) = disjs.partition(c => c.freeVariables.exists(vars contains _))
        val c1p = partition(vars.toSet, c1, Or, ForAll.apply)
        Or(c1p ++ c2 :_*)
      case other => other
    }
    def fixedPoint(f: Formula): Formula = {
      val f0 = simplifyBool(simplifyQuantifiers(f))
      val f1 = FormulaUtils.map(pushDown, f0)
      val f2 = simplifyBool(simplifyQuantifiers(f1))
      if (f0 != f2) fixedPoint(f2) else f2
    }
    fixedPoint(f)
  }

  def splitForall(f: Formula): Formula = {
    def split(f: Formula): Formula = f match {
      case ForAll(vars, And(conjs @ _*)) =>
        And(conjs.map( c => {
          val vars2 = vars.filter(c.freeVariables)
          if (vars2.isEmpty) c
          else ForAll(vars2, c)
        }):_*)
      case And(_*) =>
        val conjs2 = FormulaUtils.getConjuncts(f)
        Copier.Application(f, And, conjs2)
      case other => other
    }
    FormulaUtils.map(split, f)
  }

  //warning: this will pull up the ∃ to the top ∨
  def mergeExists(f: Formula): Formula = {
    def merge(f: Formula): Formula = f match {
      case Or(disjs @ _*) =>
        val init = (List[Formula](), Set[Variable]())
        val (disj2, vars) = disjs.foldLeft( init )( (acc, f) => {
          val (fs, vs) = acc
          val (f2, v) = round.logic.Quantifiers.getExistentialPrefix(f)
          val existing = vs -- v
          val news = v.filterNot(vs)
          val (map,_) = news.foldLeft((Map[Variable,Variable](),existing))( (acc, v) => {
            //try to map the news to existing of the same type and alpha
            val (m,vs) = acc
            vs.find(_.tpe == v.tpe) match {
              case Some(v2) =>
                (m + (v -> v2), vs - v2)
              case None => acc
            }
          })
          (FormulaUtils.alpha(map, f2) ::fs, vs ++ v.map( v => map.getOrElse(v,v)))
        })
        if (!vars.isEmpty) Exists(vars.toList, Or(disj2:_*))
        else Copier.Application(f, Or, disjs.toList)
      case other => other
    }
    FormulaUtils.map(merge, f)
  }


  //makes all the bound variables different
  def boundVarUnique(f: Formula): Formula = {
    def s(subst: Map[String, String], v: Variable): Variable = {
      if (subst contains v.name) Copier.Variable(v, subst(v.name))
      else v
    }
    def process(f: Formula, used: Set[String], subst: Map[String, String]): (Formula, Set[String]) = f match {
      case Binding(bt, vs, f2) =>
        val masking = vs.filter(v => used(v.name))
        val newSubst = masking.map( v => (v.name, Namer(v.name)) )
        //println("boundVarUnique, renamings: " + newSubst)
        val subst2 = subst ++ newSubst
        val vs2 = vs.map(s(subst2,_))
        val used2 = vs2.foldLeft(used)(_ + _.name)
        val (f3, used3) = process(f2, used2, subst2)
        (Copier.Binding(f, bt, vs2, f3), used3)
      case Application(fct, args) => 
        val (args2, used2) = args.foldRight( (List[Formula](), used) )( (arg, acc) => {
          val (args, used) = acc
          val (arg2, used2) = process(arg, used, subst)
          (arg2 :: args, used2)
        }) 
        (Copier.Application(f, fct, args2), used2)
      case v @ Variable(_) => (s(subst, v), used)
      case other => (other, used)
    }
    process(f,
            f.freeVariables.map(_.name),
            Map.empty[String, String])._1
  }

  //TODO some more
  def simplifySetOp(f: Formula): Formula = {
    import FormulaUtils._
    def fct(f: Formula) = f match {
      case Union(lst @ _*) =>
        val lst2 = lst.toSet
        Application(Union, lst2.toList.sorted).setType(f.tpe)
      case Intersection(lst @ _*) =>
        val lst2 = lst.toSet
        Application(Intersection, lst2.toList.sorted).setType(f.tpe)
      case other =>
        other
    }
    FormulaUtils.map(fct, f)
  }
  
  def lcm(m: Long, n: Long) = {
    var a = m
    var b = n
    while (a != b) {
      if (a < b) a += m
      else       b += n
    }
    a
  }
  
  def simplifyQuantifiers(f: Formula): Formula = {
    import FormulaUtils.VariableOrdering
    def fct(formula: Formula): Formula = formula match {
      case ForAll(vs, ForAll(vs2, f)) =>
        val vs3 = (vs.toSet ++ vs2.toSet).toList
        fct(ForAll(vs3, f))
      case Exists(vs, Exists(vs2, f)) =>
        val vs3 = (vs.toSet ++ vs2.toSet).toList
        fct(Exists(vs3, f))
      case ForAll(vs, f) =>
        val free = f.freeVariables
        val vs2 = vs.filter(free contains _)
        if (vs2.isEmpty) f else ForAll(vs2.sorted, f)
      case Exists(vs, f) =>
        val free = f.freeVariables
        val vs2 = vs.filter(free contains _)
        if (vs2.isEmpty) f else Exists(vs2.sorted, f)
      case other => other 
    }
    FormulaUtils.map(fct, f)
  }
  
  //TODO better: like the polynomial in react
  def simplifyInt(f: Formula): Formula = {
    //division: from 'x > 2n/3' to '3x > 2n'
    def getDenom(f: Formula): Long = f match {
      case Plus(lst @ _*) => lst.foldLeft(1l)( (acc, f) => lcm(acc, getDenom(f)) )
      case Minus(lst @ _*) => lst.foldLeft(1l)( (acc, f) => lcm(acc, getDenom(f)) )
      case Times(lst @ _*) => lst.foldLeft(1l)( (acc, f) => acc * getDenom(f) )
      case Divides(a, IntLit(i)) => i * getDenom(a)
      case other => 1
    }
    def rmDenom(f: Formula, i: Long): Formula = f match {
      case Plus(lst @ _*) => Application(Plus, lst.map(rmDenom(_, i)).toList).setType(Int)
      case Minus(lst @ _*) => Application(Minus, lst.map(rmDenom(_, i)).toList).setType(Int)
      case Times(lst @ _*) => 
        val init = (Nil: List[Formula], i)
        val (lst2, csts) = lst.foldLeft(init)( (acc, f) => rmDenom(f, acc._2) match {
          case Times(IntLit(i), xs @ _*) => (xs.toList ::: acc._1, i)
          case other => (other :: acc._1, 1)
        })
        if (csts == 1) Application(Times, lst2.reverse).setType(Int)
        else Application(Times, IntLit(csts) :: lst2.reverse).setType(Int)
      case Divides(a, IntLit(i2)) =>
        assert(i % i2 == 0, "simplifyInt: not divisible by denominator")
        val m = i / i2
        if (m == 1) a else Times(IntLit(m), a)
      case other => Times(IntLit(i), other)
    }
    def removeDiv(lhs: Formula, rhs: Formula): (Formula, Formula) = {
      val d = lcm(getDenom(lhs), getDenom(rhs))
      if (d == 1 || d == 0) lhs -> rhs
      else {
        val l2 = rmDenom(lhs, d)
        val r2 = rmDenom(rhs, d)
        if (d >= 0) l2 -> r2
        else r2 -> l2
      }
    }
    def fct(f: Formula): Formula = f match {
      case Minus(x, IntLit(i)) =>
        fct(Plus(x , IntLit(-i)))
      case Plus(lst @ _*) =>
        val init = (Nil: List[Formula], 0l)
        val (lst2, csts) = lst.foldLeft( init )( (acc, f) => f match {
          case Plus(l2 @ _*) => (l2.toList ::: acc._1, acc._2)
          case IntLit(i) => (acc._1, acc._2 + i)
          case other => (other::acc._1, acc._2)
        })
        if (lst2.isEmpty) {
          IntLit(csts)
        } else if (csts == 0) {
          Application(Plus, lst2.reverse)
        } else {
          Application(Plus, (IntLit(csts) :: lst2).reverse)
        }
      case Times(lst @ _*) =>
        val init = (Nil: List[Formula], 1l)
        val (lst2, csts) = lst.foldLeft( init )( (acc, f) => f match {
          case Times(l2 @ _*) => (l2.toList ::: acc._1, acc._2)
          case IntLit(i) => (acc._1, acc._2 * i)
          case other => (other::acc._1, acc._2)
        })
        if (lst2.isEmpty) {
          IntLit(csts)
        } else if (csts == 0) {
          IntLit(0)
        } else if (csts == 1) {
          Application(Times, lst2.reverse)
        } else {
          Application(Times, (IntLit(csts) :: lst2).reverse)
        }
      case Divides(IntLit(i1), IntLit(i2)) if i1 % i2 == 0 => IntLit(i1/i2)
      case Divides(a, IntLit(1)) => a
      case Divides(a, b) if a == b => IntLit(1)
      case Eq(a, b) if a.tpe == Int =>
        removeDiv(a, b) match {
          case (IntLit(i1), IntLit(i2)) =>
            if (i1 == i2) True()
            else False()
          case (a2, b2) =>
            if (a2 == b2) True()
            else Eq(a2, b2)
        }
      case Lt(a, b) =>
        removeDiv(a, b) match {
          case (IntLit(i1), IntLit(i2)) =>
            if (i1 < i2) True()
            else False()
          case (a2, b2) =>
            if (a2 == b2) False()
            else Lt(a2, b2)
        }
      case other =>
        other
    }
    FormulaUtils.map(fct, f)
  }
  
  def simplifyBool(f: Formula): Formula = {
    import FormulaUtils._
    def fct(f: Formula) = f match {
      case Or(lst @ _*) =>
        val lst2 = lst.toSet.filterNot(_ == False())
        if (lst2.exists(_ == True())) True()
        else if (lst2.isEmpty) False()
        else if (lst2.size == 1) lst2.head
        else Copier.Application(f, Or, lst2.toList.sorted)
      case And(lst @ _*) =>
        val lst2 = lst.toSet.filterNot(_ == True())
        if (lst2.exists(_ == False())) False()
        else if (lst2.isEmpty) True()
        else if (lst2.size == 1) lst2.head
        else Copier.Application(f, And, lst2.toList.sorted)
      case Not(Literal(b: Boolean)) =>
        Literal(!b)
      case other =>
        other
    }
    FormulaUtils.map(fct, f)
  }

  def simplify(f: Formula): Formula = {
    val f0 = normalize(f)
    val f1 = nnf(f0)
    val f2 = FormulaUtils.flatten(f1)
    val f3 = simplifyInt(f2)
    val f4 = simplifyBool(f3)
    val f5 = simplifySetOp(f4)
    val f6 = simplifyQuantifiers(f5)
    f6
  }

  //TODO while simplifying rename the variables (de Bruijn indices)

}
