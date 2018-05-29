package psync.macros

import psync._
import psync.runtime._
import psync.utils.serialization._
import io.netty.buffer.ByteBuf
import io.netty.buffer.PooledByteBufAllocator
import org.scalacheck._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.language.existentials

class SerializationSuite extends Properties("Serialization") {

  val group = new Group( new ProcessID(0), Array(Replica(new ProcessID(0), "127.0.0.1", 4444)))
  val allocator = PooledByteBufAllocator.DEFAULT

  def run(r: RtRound) {
    try {
      r.setGroup(group)
      val mailbox = scala.collection.mutable.Map[ProcessID, ByteBuf]()
      val tag = Tag(0, 0)
      def getBuffer() = {
        val buffer = allocator.buffer()
        buffer.writeLong(tag.underlying)
        buffer
      }
      r.packSend(getBuffer, mailbox.update)
      mailbox.foreach{ case (sender, payload) => r.receiveMsg(sender, Message.moveAfterTag(payload)) }
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

  val serializer = KryoSerializer.serializer
  val kryoOut = new KryoByteBufOutput(null)
  val kryoIn = new KryoByteBufInput(null)
  serializer.register(classOf[Byte])
  serializer.register(classOf[Int])
  serializer.register(classOf[String])
  serializer.register(classOf[Array[Byte]])
  serializer.register(classOf[Seq[_]])
  serializer.register(classOf[Tuple1[_]])
  serializer.register(classOf[Tuple2[_,_]])
  serializer.register(classOf[Tuple3[_,_,_]])
  serializer.register(Nil.getClass)

  //Try custom serializers
  import com.esotericsoftware.kryo.io.{Input, Output}
  import com.esotericsoftware.kryo.{Kryo, Serializer}
  class TreeSerializer extends Serializer[Tree] {
    setImmutable(true)
    def write(kryo: Kryo, output: Output, t: Tree) = t match {
      case Leaf =>
        output.writeByte(-1)
      case Node(l, r, v) =>
        output.writeByte(0)
        output.writeInt(v)
        write(kryo, output, l)
        write(kryo, output, r)
    }
    def read(kryo: Kryo, input: Input, ct: Class[Tree]): Tree = {
      val b = input.readByte()
      if (b != 0) {
        Leaf
      } else {
        val v = input.readInt()
        val l = read(kryo, input, ct)
        val r = read(kryo, input, ct)
        Node(l, r, v)
      }
    }
  }

  implicit lazy val treeSerializer: Serializer[Tree] = new TreeSerializer 

  serializer.register(classOf[Tree], treeSerializer)
  serializer.register(Leaf.getClass, treeSerializer)
  serializer.register(classOf[Node], treeSerializer)

  import scala.reflect.ClassTag

  class OptionSerializer extends Serializer[Option[_]] {
    setImmutable(true)
    def write(kryo: Kryo, output: Output, opt: Option[_]) = opt match {
      case None =>
        output.writeByte(-1)
      case Some(v) =>
        output.writeByte(0)
        kryo.writeClassAndObject(output, v)
    }
    def read(kryo: Kryo, input: Input, ct: Class[Option[_]]): Option[_] = {
      val b = input.readByte()
      if (b != 0) {
        None
      } else {
        val v = kryo.readClassAndObject(input)
        Some(v)
      }
    }
  }

  serializer.register(classOf[Option[_]], new OptionSerializer)
  serializer.register(classOf[Some[_]], new OptionSerializer)
  serializer.register(None.getClass, new OptionSerializer)


  implicit lazy val arbSome: Arbitrary[Some[Int]] = Arbitrary(for {v <- Arbitrary.arbitrary[Int]} yield Some(v))

  def kryoTest[A: ClassTag](value: A, equals: (A, A) => Boolean = { (x: A, y: A) => x == y } ) = {
    val buffer = allocator.buffer()
    try {
      //serialize
      kryoOut.setBuffer(buffer)
      serializer.writeObject(kryoOut, value)
      kryoOut.setBuffer(null: ByteBuf)
      //deserialize
      kryoIn.setBuffer(buffer)
      val result = serializer.readObject(kryoIn, implicitly[ClassTag[A]].runtimeClass).asInstanceOf[A]
      kryoIn.setBuffer(null: ByteBuf)
      equals(result, value)
    } catch { case t: Throwable =>
      Logger("SerializationSuite", Error, t.getMessage)
      Logger("SerializationSuite", Error, (s: java.io.BufferedWriter) => {
        val pw = new java.io.PrintWriter(s)
        t.printStackTrace(pw)
        pw.flush
      })
      throw t
    } finally {
      buffer.release()
    }
  }

  assert( kryoTest(None) )
  assert( kryoTest(Some(1)) )
  assert( kryoTest(Nil) )

  def compareArray[A](x: Array[A], y: Array[A]) = {
    x.length == y.length && x.corresponds(y)(_ == _)
  }

  property("Int with Kryo") = Prop forAll { (value: Int) => kryoTest(value) }
  property("(Int,Int) with Kryo") = Prop forAll { (value: (Int,Int)) => kryoTest(value) }
  property("String with Kryo") = Prop forAll { (value: String) => kryoTest(value) }
  property("Some[Int] with Kryo") = Prop forAll { (value: Some[Int]) => kryoTest(value) }
  property("Array[Byte] with Kryo") = Prop forAll { (value: Array[Byte]) => kryoTest(value, compareArray[Byte]) }
  property("Option[Int] with Kryo") = Prop forAll { (value: Option[Int]) => kryoTest(value) }
  //property("Seq[Byte] with Kryo") = Prop forAll { (value: Seq[Byte]) => kryoTest(value) }
  property("Tree with Kryo") = Prop forAll { (value: Tree) => kryoTest(value) }

}
