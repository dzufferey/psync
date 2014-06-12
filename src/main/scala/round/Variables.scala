package round

trait Variables {
  self: Algorithm =>

  //placeholder for quantifying over some domain
  class Domain[A] {
    def forall(fct: A => Boolean): Boolean = sys.error("only for verification purpose")
    def exists(fct: A => Boolean): Boolean = sys.error("only for verification purpose")
    def filter(fct: A => Boolean): Set[A] = sys.error("only for verification purpose")
  }

  class GlobalVariable[A](default: A) {
    var value = default
  }


  class LocalVariable[A](default: A) {
    private var value = default

    def <~(v: A) {
      value = v
    }

    def get: A = value

    override def equals(any: Any) = value == any

    def apply(p: Process): A = get //TODO

    //TODO use implicit to index within a process def

  }

  object VarHelper {

    implicit def getter[A](v: LocalVariable[A]): A = v.get
    
    def init[T <: LocalVariable[_]](v: T): T = sys.error("only for verification purpose")

    def old[T <: LocalVariable[_]](v: T): T = sys.error("only for verification purpose")

  }

}

