package psync.utils.serialization

import psync.utils.serialization._
import io.netty.buffer.ByteBuf
import io.netty.buffer.PooledByteBufAllocator
import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.reflect.ClassTag
import org.scalacheck._
import scala.language.existentials

class SerializationTests extends Properties("Serialization") {

  val allocator = PooledByteBufAllocator.DEFAULT

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
  //Try custom serializers
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


  implicit lazy val regSomeInt = new KryoRegistration[Some[Int]] {
    override def registerClasses = Seq(classOf[Some[_]])
  }

  implicit lazy val regOptInt = new KryoRegistration[Option[Int]] {
    //val optionSerializer = new GenericOptionSerializer
    val optionSerializer = new OptionSerializer[Int]
    override def registerClassesWithSerializer = Seq(
      classOf[Option[Int]] -> optionSerializer,
      classOf[Some[Int]] -> optionSerializer,
      None.getClass -> optionSerializer
    )
  }

  implicit lazy val regTree = new KryoRegistration[Tree] {
    val treeSerializer = new TreeSerializer
    override def registerClassesWithSerializer = Seq(
      classOf[Tree] -> treeSerializer,
      Leaf.getClass -> treeSerializer,
      classOf[Node] -> treeSerializer
    )
  }

  implicit lazy val arbSome: Arbitrary[Some[Int]] = Arbitrary(for {v <- Arbitrary.arbitrary[Int]} yield Some(v))

  def kryoTest[A: ClassTag: KryoRegistration](value: A, equals: (A, A) => Boolean = { (x: A, y: A) => x == y } ) = {
    val serializer = KryoSerializer.serializer
    val kryoOut = new KryoByteBufOutput(null)
    val kryoIn = new KryoByteBufInput(null)
    val buffer = allocator.buffer()
    val reg = implicitly[KryoRegistration[A]]
    reg.registerClasses.foreach( serializer.register(_) )
    reg.registerClassesWithSerializer.foreach{ case (c, s) => serializer.register(c, s) }
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

  def compareArray[A](x: Array[A], y: Array[A]) = {
    x.length == y.length && x.corresponds(y)(_ == _)
  }

  property("Int with Kryo") = Prop forAll { (value: Int) => kryoTest(value) }
  property("(Int,Int) with Kryo") = Prop forAll { (value: (Int,Int)) => kryoTest(value) }
  property("String with Kryo") = Prop forAll { (value: String) => kryoTest(value) }
  property("Some[Int] with Kryo") = Prop forAll { (value: Some[Int]) => kryoTest(value) }
  property("Array[Byte] with Kryo") = Prop forAll { (value: Array[Byte]) => kryoTest(value, compareArray[Byte]) }
  property("Option[Int] with Kryo") = Prop forAll { (value: Option[Int]) => kryoTest(value) }
  property("Tree with Kryo") = Prop forAll { (value: Tree) => kryoTest(value) }
}
