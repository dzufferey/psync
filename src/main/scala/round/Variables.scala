package round

import Algorithm._

trait Variables[IO] {
  self: Algorithm[IO] =>

  //placeholder for quantifying over some domain
  class Domain[A] {
    def forall(fct: A => Boolean): Boolean = sys.error("only for verification purpose, removed by macros")
    def exists(fct: A => Boolean): Boolean = sys.error("only for verification purpose, removed by macros")
    def filter(fct: A => Boolean): Set[A] =  sys.error("only for verification purpose, removed by macros")
  }

  class GlobalVariable[A](val init: A) {
    var value = init
  }


  class LocalVariable[A](val default: A) {
    private var value = default

    def <~(v: A) {
      value = v
    }

    def get: A = value

    override def equals(any: Any) = value == any

    def apply(p: ProcessID): A = get //TODO

  }

  object VarHelper {

    implicit def getter[A](v: LocalVariable[A]): A = v.get
    implicit def getter[A](v: GlobalVariable[A]): A = v.value
    
    def init[T <: LocalVariable[_]](v: T): T = sys.error("only for verification purpose, removed by macros")

    def old[T <: LocalVariable[_]](v: T): T = sys.error("only for verification purpose, removed by macros")

  }

}

