package psync.formula


object InlineOps {

  implicit def intToLit(i: Int): Formula = IntLit(i)

  implicit class Ops(lhs: Formula) {
  
    def unary_! = Not(lhs)
    def &&(rhs: Formula) = And(lhs, rhs)
    def ∧(rhs: Formula) = And(lhs, rhs)
    def ||(rhs: Formula) = Or(lhs, rhs)
    def ∨(rhs: Formula) = Or(lhs, rhs)

    def ==>(rhs: Formula) = Implies(lhs, rhs)
    def ===(rhs: Formula) = Eq(lhs, rhs)
    def !==(rhs: Formula) = Neq(lhs, rhs)
    def ≠(rhs: Formula)   = Neq(lhs, rhs)

    def +(rhs: Formula) = Plus(lhs, rhs)
    def -(rhs: Formula) = Minus(lhs, rhs)
    def *(rhs: Formula) = Times(lhs, rhs)
    def /(rhs: Formula) = Divides(lhs, rhs)
    
    def <=(rhs: Formula) = Leq(lhs, rhs)
    def >=(rhs: Formula) = Geq(lhs, rhs)
    def ≤(rhs: Formula) = Leq(lhs, rhs)
    def ≥(rhs: Formula) = Geq(lhs, rhs)
    def <(rhs: Formula) = Lt(lhs, rhs)
    def >(rhs: Formula) = Gt(lhs, rhs)

    def ∪(rhs: Formula) = Union(lhs, rhs)
    def ∩(rhs: Formula) = Intersection(lhs, rhs)
    def ⊆(rhs: Formula) = SubsetEq(lhs, rhs)
    def ⊇(rhs: Formula) = SupersetEq(lhs, rhs)
    def ∈(rhs: Formula) = In(lhs, rhs)
    def contains(rhs: Formula) = In(rhs, lhs)
    def card = Cardinality(lhs)

    def isDefined = IsDefined(lhs)
    def isEmpty = IsEmpty(lhs)
    def get = Get(lhs)

    def _1 = Fst(lhs)
    def _2 = Snd(lhs)
    def _3 = Trd(lhs)

    def keySet = KeySet(lhs)
    def lookUp(rhs: Formula) = LookUp(lhs, rhs)
    def size = Size(lhs)
    def isDefinedAt(rhs: Formula) = IsDefinedAt(lhs, rhs)

  }

}
