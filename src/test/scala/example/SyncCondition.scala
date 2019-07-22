package example

// some utils to test for different progres condition
object SyncCondition extends Enumeration {
  type SyncCondition = Value
  val Quorum, All, OnTO = Value
}
