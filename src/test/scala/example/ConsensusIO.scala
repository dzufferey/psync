package example

abstract class ConsensusIO[@specialized A] {
  val initialValue: A
  def decide(value: A): Unit
}

