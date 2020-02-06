package psync.verification

import psync.formula._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

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
  def add(s: AxiomatizedFct): Unit = {
    if (map contains s.name) {
      Logger("AxiomatizedFct", Warning, "redefinition of " + s.name)
    } else {
      symbols = s :: symbols
      map = map + (s.name -> s)
    }
  }
  def clear: Unit = {
    symbols = Nil
    map = Map.empty
  }
  def apply(s: String): Option[AxiomatizedFct] = map.get(s)
  def symbol(s: String) = apply(s).map(_.symbol)
  def knows(s: String) = map contains s

}
