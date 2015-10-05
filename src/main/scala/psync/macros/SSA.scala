package psync.macros

import psync.formula._
import psync.verification._
import dzufferey.utils.{Namer, Misc}

trait SSA {
  self: Impl =>
  import c.universe._

  class SsaMap(map: Map[Symbol, (Tree, Int)]) {

    def this() = this(Map.empty)

    def keySet = map.keySet

    def keys = map.keys

    def foldLeft[A](acc: A)(fct: (A, (Symbol,(Tree,Int))) => A) = map.foldLeft(acc)(fct)

    def +(kv: (Symbol, (Tree,Int))) = new SsaMap(map + kv)

    def increment(t: Tree): SsaMap = {
      val sym = t.symbol
      assert(sym != null)
      if (map.contains(sym)) {
        val (t,i) = map(sym)
        new SsaMap(map + (sym -> (t,i+1)))
      } else {
        val i = SsaMap.version(t)
        new SsaMap(map + (sym -> (t,i+1)))
      }
    }
    
    def getFirstVersion(sym: Symbol): Int = {
      SsaMap.version(getTree(sym))
    }
    
    def getMostRecentVersion(sym: Symbol): Option[Int] = {
      map.get(sym).map(_._2)
    }

    def getMostRecentVersion(t: Tree): Int = {
      assert(t.symbol != null)
      getMostRecentVersion(t.symbol).getOrElse(SsaMap.version(t))
    }

    def getMostRecentTree(t: Tree) = {
      val i = getMostRecentVersion(t)
      SsaMap.name(t, i)
    }
    
    def getMostRecentTree(s: Symbol) = {
      val i = getMostRecentVersion(s).getOrElse(0)
      SsaMap.name(getTree(s), i)
    }

    def contains(t: Tree): Boolean = {
      t.symbol != null && map.contains(t.symbol)
    }

    def getTree(s: Symbol) = {
      map.apply(s)._1
    }

  }

  object SsaMap {

    def name(t: Tree, i: Int) = {
      def newName(n: TermName) = {
        val (prefix, _) = Namer.getPrefixAndVersion(n.toString)
        if (i == 0) TermName(prefix)
        else TermName(prefix + "$" + i)
      }
      val res = t match {
        case Ident(name @ TermName(_)) =>
          treeCopy.Ident(t, newName(name))
        case Select(scope, name @ TermName(_)) => 
          treeCopy.Select(t, scope, newName(name))
        case other =>
          c.abort(t.pos, "expected identifer or field, found: " + showRaw(other))
      }
      //println(showRaw(res))
      res
    }

    def version(t: Tree): Int = {
      val name = t match {
        case Ident(name @ TermName(_)) => name.toString
        case Select(scope, name @ TermName(_)) => name.toString
        case other =>
          c.abort(t.pos, "SsaMap.version: expected identifer or field, found: " + showRaw(other))
      }
      val (prefix, version) = Namer.getPrefixAndVersion(name)
      version
    }


  }

  private def joinSsaSubst(
      s1: SsaMap,
      s2: SsaMap 
    ): (SsaMap, List[(Tree,Int,Int)], List[(Tree,Int,Int)]) =
  {
    val ks = s1.keySet union s2.keySet
    val init: (SsaMap, List[(Tree,Int,Int)], List[(Tree,Int,Int)]) = (new SsaMap, Nil, Nil)
    ks.foldLeft(init)( (acc, k) => {
      val v1 = s1.getMostRecentVersion(k)
      val v2 = s2.getMostRecentVersion(k)
      (v1, v2) match {
        case (Some(v1), None) =>
          val first = s1.getFirstVersion(k)
          val t = s1.getTree(k)
          val acc1 = acc._1 + (k -> (t, v1))
          val acc3 = (t, v1, first) :: acc._3
          (acc1, acc._2, acc3)
        case (None, Some(v2)) =>
          val first = s2.getFirstVersion(k)
          val t = s2.getTree(k)
          val acc1 = acc._1 + (k -> (t, v2))
          val acc2 = (s2.getTree(k), v2, first) :: acc._2
          (acc1, acc2, acc._3)
        case (Some(v1), Some(v2)) =>
          val last = math.max(v1, v2)
          val acc1 = acc._1 + (k -> (s1.getTree(k), last))
          val acc2 = if (v1 < last) (s2.getTree(k), last, v1) :: acc._2 else acc._2
          val acc3 = if (v2 < last) (s1.getTree(k), last, v2) :: acc._3 else acc._3
          (acc1, acc2, acc3)
        case (None, None) => sys.error("???")
      }
    })
  }


  private def addSsaMatchingCode(t: Tree, eqs: List[(Tree,Int,Int)]): Tree = {
    val matchingCode: List[Tree] = eqs.map{
      case (v, last, prev) =>
        Assign(SsaMap.name(v, last), SsaMap.name(v, prev))
    }
    //blockify
    t match {
      case Block(stmts, ret) =>
        treeCopy.Block(t, stmts ::: matchingCode, ret)
      case EmptyTree =>
        if (matchingCode.size > 1) {
          treeCopy.Block(t, matchingCode, EmptyTree)
        } else if (matchingCode.size == 1) {
          matchingCode.head
        } else {
          EmptyTree
        }
      case other =>
        if (!matchingCode.isEmpty)
          //TODO does that make sense? if the if returns something it should fail later when extracting constraints ?!
          treeCopy.Block(other, other :: matchingCode, EmptyTree)
        else
          other
    }
  }
  
  class SsaSubst(map: SsaMap) extends Transformer {
    override def transform(tree: Tree): Tree = {
      val sup = super.transform(tree)
      if (map contains sup) {
        map.getMostRecentTree(sup)
      } else {
        sup
      }
    }
  }

  private def applySsaSubst(t: Tree, subst: SsaMap): Tree = {
    val sub = new SsaSubst(subst)
    sub.transform(t)
  }

  private def increment(t: Tree, subst: SsaMap): (Tree, SsaMap) = {
    val subst2 = subst.increment(t)
    val t2 = subst2.getMostRecentTree(t)
    //println("increment: " + t + " -> " + t2 + " with " + subst2.getMostRecentVersion(t) + ", " + subst2.getMostRecentVersion(t2) )
    (t2, subst2)
  }


  private val emptyMods = Modifiers()

  //returns the body in SSA and a map of tree to the different version
  protected def ssa(
      body: Tree,
      substMap: SsaMap = new SsaMap
    ): (Tree, SsaMap) = body match {
    
    case If(cond, thenp, elsep) =>
      val cond2 = applySsaSubst(cond, substMap)
      val (then2, substMapT) = ssa(thenp, substMap)
      val (else2, substMapE) = ssa(elsep, substMap)
      val (substMap2, matchT, matchE) = joinSsaSubst(substMapT, substMapE)
      val then3 = addSsaMatchingCode(then2, matchT)
      val else3 = addSsaMatchingCode(else2, matchE)
      (treeCopy.If(body, cond2, then3, else3), substMap2)
    
    case Block(stats, expr) => 
      val (stats2, substMap2) = Misc.mapFold(stats, substMap, ssa)
      val (expr2, substMap3) = ssa(expr, substMap2)
      (treeCopy.Block(body, stats2, expr2), substMap3)
    
    case Typed(e, _) =>
      ssa(e, substMap)
    
    case Apply(Select(lhs, TermName("$less$tilde")), List(rhs)) => 
      ssa(treeCopy.Assign(body, lhs, rhs), substMap)

    case Assign(lhs, rhs) => 
      val (rhs2, substMap2) = ssa(rhs, substMap)
      val (lhs2, substMap3) = increment(lhs, substMap2)
      (treeCopy.Assign(body, lhs2, rhs2), substMap3)

    case ValDef(`emptyMods`, name, tpt, rhs) =>
      val (rhs2, substMap2) = ssa(rhs, substMap)
      (treeCopy.ValDef(body, emptyMods, name, tpt, rhs2), substMap2)

    case ValDef(mods, name, tpt, rhs) => //if mutable ...
      c.abort(body.pos, "Round should not contain variable, please use values only: non-empty modifier in ValDef " + mods)

    case other =>
      (applySsaSubst(other, substMap), substMap)
  }

}
