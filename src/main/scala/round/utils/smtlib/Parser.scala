package round.utils.smtlib

import Names._
import round.utils._
import round.formula._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._

object Parser extends StandardTokenParsers {

  lexical.delimiters += (
    "(", ")", "!",
    "=", "<", ">", ">=", "<=", "=>",
    "+", "-", "*"
  )
  
  lexical.reserved += (
    "model", "assert",
    "declare-sort", "declare-fun", 
    "define-sort", "define-fun",
    ":named",
    "forall", "exists",
    "ite",
    "true", "false",
    "Int", "Bool"
  )
    
  def paren[T](parser: Parser[T]): Parser[T] = "(" ~> parser <~ ")"

  def model: Parser[List[Command]] = paren("model" ~> rep(cmd))

  def cmd: Parser[Command] = paren(
      "declare-sort" ~> ident ~ numericLit                          ^^ { case id ~ num => DeclareSort(id, num.toInt) }
    | "declare-fun" ~> ident ~ paren(rep(sort)) ~ sort              ^^ { case id ~ args ~ ret => DeclareFun(id, Function(args, ret))  }
    | "define-sort" ~> ident ~ paren(rep(ident)) ~ sort             ^^ { case id ~ args ~ ret => DefineSort(id, args, ret) }
    | "define-fun" ~> ident ~ paren(rep(typedVar)) ~ sort ~ term    ^^ { case id ~ vars ~ tpe ~ body => DefineFun(id, vars, tpe, body) }
    | "assert" ~> term                                              ^^ { f => Assert(f) }
    | term                                                          ^^ { f => Assert(f) }
  )

  def binder: Parser[(List[Variable], Formula) => Formula] = (
      "forall" ^^^ ( (vs: List[Variable], f: Formula) => ForAll(vs, f) )
    | "exists" ^^^ ( (vs: List[Variable], f: Formula) => Exists(vs, f) )
  )

  def term: Parser[Formula] = (
      "true"                        ^^^ True()
    | "false"                       ^^^ False()
    | numericLit                    ^^ { str => Literal(str.toLong) }
    | ident                         ^^ { id  => Variable(id) }
    | paren("ite" ~> rep(term))     ^^ { case args => Application(ite, args) }
    | paren(symbol ~ rep(term))     ^^ { case sym ~ args => Application(sym, args) }
    | paren(binder ~ paren(rep(typedVar)) ~ term) ^^ { case b ~ v ~ f => b(v, f) }
    | paren("!" ~> term ~ (":named" ~> ident)) ^^ { case t ~ id => t } //TODO ??
  )

  def sort: Parser[Type] = (
      "Int" ^^^ Int
    | "Bool" ^^^ Bool
    | ident ^^ { id => UnInterpreted(id) }
    | paren(ident ~ rep(sort)) ^^ { case id ~ args => sys.error("TODO FSet, FOption, Product") }
  )

  def symbol: Parser[Symbol] = (
      "="  ^^^ Eq
    | "<"  ^^^ Lt
    | ">"  ^^^ Gt
    | ">=" ^^^ Get
    | "<=" ^^^ Leq
    | "=>" ^^^ Implies
    | "+"  ^^^ Plus
    | "-"  ^^^ Minus
    | "*"  ^^^ Times
    | ident ^^ { id => InterpretedFct(id).getOrElse(UnInterpretedFct(id)) }
  )

  def typedVar: Parser[Variable] = "(" ~> ident ~ sort <~ ")" ^^ { case id ~ srt => Variable(id).setType(srt) }

}
