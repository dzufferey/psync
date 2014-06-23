package round

//IO is a type parameter to communicate the initial value, parameter, and callbacks
//the use of mixing composition forces elements (like variables) to be used only with the algorithm
abstract class Algorithm[IO] extends Variables[IO]
    with Processes[IO]
    with Rounds[IO]
    with Specs[IO]
{

  //round number
  val r = new GlobalVariable[Int](0)
  
  //the univers of processes
  val P = new Domain[Process]
  //and sets of Process
  val S = new Domain[Set[Process]]

  //number of processes
  val n = new LocalVariable[Int](0)

  //the heard-of set
  val HO = new LocalVariable[Set[Process]](Set[Process]())

  //specification of the consensus
  val spec: Spec

  def process(io: IO): Process


  //////////////////
  // util methods //
  //////////////////
  import io.netty.buffer.{ByteBuf, PooledByteBufAllocator}

  //private val maxSize = 4096// 65536-1
  private val allocator = new PooledByteBufAllocator(true)

  protected final def getBuffer: ByteBuf = {
    allocator.buffer()
  }

}
