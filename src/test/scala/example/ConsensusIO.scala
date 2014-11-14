package example

abstract class ConsensusIO {
  val initialValue: Int
  def decide(value: Int): Unit
}

