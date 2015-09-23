package round.macros

import round.formula._
import round.verification._
import dzufferey.utils.Namer

trait TypeExtractor {
  self: Impl =>
  import c.universe._

  object IsTuple {
    def unapply(t: Type): Option[List[Type]] = t match {
      case TypeRef(_, tRef, args) if showRaw(tRef) startsWith "scala.Tuple" => Some(args)
      case _ => None
    }
  }

  object IsSet {
    def unapply(t: Type): Option[Type] = t match {
      case TypeRef(_, tRef, List(arg)) =>
        val sr = showRaw(tRef)
        if (sr.startsWith("scala.collection.immutable.Set") || 
            sr == "TypeName(\"Set\")") {
          Some(arg)
        } else {
          None
        }
      case _ => None
    }
  }

  object IsMap {
    def unapply(t: Type): Option[(Type,Type)] = t match {
      case TypeRef(_, tRef, List(key, value)) =>
        val sr = showRaw(tRef)
        if (sr.startsWith("scala.collection.immutable.Map") || 
            sr == "TypeName(\"Map\")") {
          Some(key -> value)
        } else {
          None
        }
      case _ => None
    }
  }

  object IsOption {
    def unapply(t: Type): Option[Type] = t match {
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.Option" => Some(arg)
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.Some" => Some(arg)
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.None" => Some(arg)
      case _ => None
    }
  }

  object IsUnit {
    def unapply(t: Type): Boolean = t match {
      case TypeRef(_, tRef, List()) if showRaw(tRef) == "scala.Unit" => true
      case _ => false
    }
  }

  //TODO clean version using mirror ....
  def extractType(t: Type): round.formula.Type = {
    import definitions._
    if (t == null) {
      Wildcard
    } else if (t weak_<:< LongTpe) {
      Int
    } else if (t weak_<:< BooleanTpe) {
      Bool
    } else if (t == NoType) {
      Wildcard
    } else {
      t match {
        case IsTuple(args) => Product(args map extractType)
        case IsSet(arg) => FSet(extractType(arg))
        case IsMap(k,v) => FMap(extractType(k),extractType(v))
        case IsOption(arg) =>  FOption(extractType(arg))
        case IsUnit() => UnitT()
        case MethodType(args, returnT) =>
          round.formula.Function(args.map(arg => extractType(arg.typeSignature)), extractType(returnT))
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"LocalVariable\")" =>
          round.formula.Function(List(round.verification.Utils.procType), (extractType(arg)))
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"GhostVariable\")" =>
          round.formula.Function(List(round.verification.Utils.procType), (extractType(arg)))
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"GlobalVariable\")" =>
          extractType(arg)
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"Domain\")" =>
          FSet(extractType(arg)) //Domain are Set
        case t @ TypeRef(_, _, List()) =>
          val str = t.toString
          if (str == "round.ProcessID") round.verification.Utils.procType
          else UnInterpreted(str)
        case SingleType(_, _) =>
          Wildcard
        case NullaryMethodType(tpe) =>
          extractType(tpe)
        case other =>
          //TODO
          println("TODO extractType:\n  " + other + "\n  " + showRaw(other))
          Wildcard
      }
    }
  }
  
  // what about type alias ?
  
  //TODO
  def extractType(t: Tree): round.formula.Type =
    extractType(t.tpe) match {
      case Wildcard =>
        t match {
          case TypeTree() => extractType(t.tpe)
          case Ident(TypeName("Int"))
             | Ident(TypeName("Long"))
             | Ident(TypeName("Short")) => Int
          case Ident(TypeName("Boolean")) => Bool
          case AppliedTypeTree(Ident(TypeName("Option")), List(tpe)) => FOption(extractType(tpe))
          case AppliedTypeTree(Ident(TypeName("Set")), List(tpe)) => FSet(extractType(tpe))
          case AppliedTypeTree(Ident(TypeName("Map")), List(k,v)) => FMap(extractType(k), extractType(v))
          case Ident(TypeName("ProcessID")) => round.verification.Utils.procType
          case Select(Ident(pkg), TypeName(tn)) => UnInterpreted(pkg.toString + "." + tn)
          case Ident(TypeName(tn)) => UnInterpreted(tn)
          case _ =>
            c.warning(t.pos, "TODO extractType from tree: " + showRaw(t) + " currently Wildcard")
            Wildcard
        }
      case other => other
    }
  
  def typeOfTree(e: Tree): round.formula.Type = {
    extractType(e.tpe) match {
      case Wildcard => extractType(e.symbol.typeSignature)
      case other => other
    }
  }

}
