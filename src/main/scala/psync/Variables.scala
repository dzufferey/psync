package psync

trait Variables[IO, P <: Process[IO]] {
  self: Algorithm[IO, P] =>

  abstract class Variable[A] {
    def value: A
  }

  class GlobalVariable[A](val init: A) extends Variable[A] {
    def value = init
  }

  class LocalVariable[A](val default: A) extends Variable[A] {
  //private var value = default
  //def <~(v: A) { value = v }
  //def value: A = value
  //override def equals(any: Any) = value == any
  //def apply(p: ProcessID): A = value //TODO
    def <~(v: A) { sys.error("only for compilation purpose, removed by macros") }
    def value: A = sys.error("only for compilation purpose, removed by macros")
    override def equals(any: Any) = sys.error("only for compilation purpose, removed by macros")
    def apply(p: ProcessID): A = sys.error("only for compilation purpose, removed by macros")
  }

  class GhostVariable[A] extends Variable[A] {
    def <~(v: A) { sys.error("only for verification purpose, removed by macros") }
    def value: A = sys.error("only for verification purpose, removed by macros")
    override def equals(any: Any) = sys.error("only for verification purpose, removed by macros")
    def apply(p: ProcessID): A = sys.error("only for verification purpose, removed by macros")
  }

}

