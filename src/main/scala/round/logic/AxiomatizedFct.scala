package round.logic

import round.formula._

class AxiomatizedFct(
    val symbol: UnInterpretedFct,
    val axioms: List[Formula],
    val local: Boolean = true,
    val psi: Set[Formula] => Set[Formula] = ( (s: Set[Formula]) => Set.empty[Formula])
  )
{
  def name = symbol.toString

  //TODO ...
}

//list of axiomatized functions
object AxiomatizedFct {
  private var symbols: List[AxiomatizedFct] = Nil
  private var map: Map[String,AxiomatizedFct] = Map.empty
  def add(s: AxiomatizedFct) {
    symbols = s :: symbols
    assert(!(map contains s.name), "axiomatized function redefinition: " + s)
    map = map + (s.name -> s)
  }
  def apply(s: String): Option[AxiomatizedFct] = map.get(s)
  def symbol(s: String) = apply(s).map(_.symbol)
  def knows(s: String) = map contains s

  private val a = TypeVariable("A")
  private val al = List(a)
  private val as = FSet(a)
  
  private val x = Variable("x").setType(a)
  private val xl = List(x)
  
  private val s = Variable("s").setType(as)
  private val sl = List(s)

  add({
    val h = UnInterpretedFct("head", Some(as ~> a), al)
    val axs = List(
      ForAll(List(x, s), In(Application(h, sl), s))
    )
    new AxiomatizedFct(h, axs)
  })

}
