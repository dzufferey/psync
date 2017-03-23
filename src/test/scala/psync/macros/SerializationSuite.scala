package psync.macros

import psync._
import psync.runtime._
import io.netty.buffer.ByteBuf
import io.netty.buffer.PooledByteBufAllocator
import org.scalacheck._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

class SerializationSuite extends Properties("Serialization") {

  val group = new Group( new ProcessID(0), Array(Replica(new ProcessID(0), "127.0.0.1", 4444)))
  val allocator = PooledByteBufAllocator.DEFAULT

  def run(r: RtRound) {
    try {
      r.setGroup(group)
      val mailbox = scala.collection.mutable.Map[ProcessID, ByteBuf]()
      r.packSend(allocator, mailbox.update )
      mailbox.foreach{ case (sender, payload) => r.receiveMsg(sender, payload) }
      r.finishRound
    } catch { case t: Throwable =>
      Logger("SerializationSuite", Error, t.getMessage)
      Logger("SerializationSuite", Error, (s: java.io.BufferedWriter) => {
        val pw = new java.io.PrintWriter(s)
        t.printStackTrace(pw) 
        pw.flush
      })
      throw t
    }
  }

  property("Int") = Prop forAll { (value: Int) =>
    val rnd = Macros.rnd(new Round[Int]{
      def send(): Map[ProcessID,Int] = {
        broadcast( value )
      }

      def check(mailbox: Map[ProcessID,Int]) {
        assert(mailbox.values.forall(_ == value))
      }

      def update(mailbox: Map[ProcessID,Int]) {
        check(mailbox)
      }
    })
    run(rnd)
    true
  }

  property("String") = Prop forAll { (value: String) =>
    val rnd = Macros.rnd(new Round[String]{
      def send(): Map[ProcessID,String] = {
        broadcast( value )
      }

      def check(mailbox: Map[ProcessID,String]) {
        assert(mailbox.values.forall(_ == value))
      }

      def update(mailbox: Map[ProcessID,String]) {
        check(mailbox)
      }
    })
    run(rnd)
    true
  }

  property("Array[Byte]") = Prop forAll { (value: Array[Byte]) =>
    val rnd = Macros.rnd(new Round[Array[Byte]]{
      def send(): Map[ProcessID,Array[Byte]] = {
        broadcast( value )
      }

      def check(mailbox: Map[ProcessID,Array[Byte]]) {
        assert(mailbox.values.forall(_.deep == value.deep))
      }

      def update(mailbox: Map[ProcessID,Array[Byte]]) {
        check(mailbox)
      }
    })
    run(rnd)
    true
  }

  property("Seq[Byte]") = Prop forAll { (value: Seq[Byte]) =>
    val rnd = Macros.rnd(new Round[Seq[Byte]]{
      def send(): Map[ProcessID,Seq[Byte]] = {
        broadcast( value )
      }

      def check(mailbox: Map[ProcessID,Seq[Byte]]) {
        assert(mailbox.values.forall(_ == value))
      }

      def update(mailbox: Map[ProcessID,Seq[Byte]]) {
        check(mailbox)
      }
    })
    run(rnd)
    true
  }

  //from https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md
  //and http://stackoverflow.com/questions/42834516/understanding-scalachecks-generation-size/42855840#42855840
  sealed abstract class Tree
  case class Node(left: Tree, right: Tree, v: Int) extends Tree
  case object Leaf extends Tree

  val genLeaf = Gen.const(Leaf)
  val genNode = for {
    v <- Arbitrary.arbitrary[Int]
    left <- Gen.sized(h => Gen.resize(h/2, genTree))
    right <- Gen.sized(h => Gen.resize(h/2, genTree))
    } yield Node(left, right, v)

  def genTree: Gen[Tree] = Gen.sized { height =>
    if (height <= 0) {
      genLeaf
    } else {
      Gen.oneOf(genLeaf, genNode)
    }
  }

  implicit lazy val arbTree: Arbitrary[Tree] = Arbitrary(genTree)

  property("Tree") = Prop forAll { (value: Tree) =>
    val rnd = Macros.rnd(new Round[Tree]{
      def send(): Map[ProcessID,Tree] = {
        broadcast( value )
      }

      //TODO ?!?
      def strangeEq(v1: Tree, v2: Tree) = {
        if (v1 != v2) {
          if (v1.toString == v2.toString) {
            Logger("SerializationSuite", Debug, "same but !=:\n" + v1 + "\n" + v2)
            true
          } else {
            false
          }
        } else {
          true
        }
      }

      def check(mailbox: Map[ProcessID,Tree]) {
        mailbox.values.foreach( v => assert(strangeEq(v, value), "difference between:\n" + v + "\n" + value))
      }

      def update(mailbox: Map[ProcessID,Tree]) {
        check(mailbox)
      }
    })
    run(rnd)
    true
  }
}
