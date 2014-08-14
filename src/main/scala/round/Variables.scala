package round

trait Variables[IO] {
  self: Algorithm[IO] =>

  //placeholder for quantifying over some domain
  class Domain[A] {
    def forall(fct: A => Boolean): Boolean = sys.error("only for verification purpose, removed by macros")
    def exists(fct: A => Boolean): Boolean = sys.error("only for verification purpose, removed by macros")
    def filter(fct: A => Boolean): Set[A] =  sys.error("only for verification purpose, removed by macros")
  }

  abstract class Variable[A] {
    def get: A
  }

  class GlobalVariable[A](val init: A) extends Variable[A] {
    def get = init
  }


  class LocalVariable[A](val default: A) extends Variable[A] {

    private var value = default

    def <~(v: A) { value = v }

    def get: A = value

    override def equals(any: Any) = value == any

    def apply(p: ProcessID): A = get //TODO

  }

  class GhostVariable[A] extends Variable[A] {
    def <~(v: A) { sys.error("only for verification purpose, removed by macros") }
    def get: A = sys.error("only for verification purpose, removed by macros")
    override def equals(any: Any) = sys.error("only for verification purpose, removed by macros")
    def apply(p: ProcessID): A = sys.error("only for verification purpose, removed by macros")
  }


  object VarHelper {

    implicit def getter[A](v: Variable[A]): A = v.get
    def init[T <: Variable[_]](v: T): T = sys.error("only for verification purpose, removed by macros")
    def old[T <: Variable[_]](v: T): T = sys.error("only for verification purpose, removed by macros")

  }

}

