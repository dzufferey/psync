package example

import round._
import round.runtime._
import round.macros.Macros._

class CgolIO(val id: Int, val rows: Int, val cols: Int, val init: Boolean) { }

class CgolProcess extends Process[CgolIO] {
  
  import ConwayGameOfLife._

  var alive = false
  var row = 0
  var col = 0
  var neighbours = Set.empty[ProcessID]

  def init(io: CgolIO) = i{
    alive = io.init
    row = io.id / io.cols
    col = io.id % io.cols
    neighbours = getNeighbours(io.rows, io.cols, io.id)
  }

  val rounds = phase(
    new Round[Boolean]{
    
      def send: Map[ProcessID,Boolean] = {
        neighbours.map( _ -> (alive: Boolean) ).toMap
      }

      def update(mailbox: Map[ProcessID,Boolean]) {
        val aliveNeighbours = mailbox.filter(_._2).size
        if (alive) {
          if (aliveNeighbours != 2 && aliveNeighbours != 3) {
            alive = false
          }
        } else {
          if (aliveNeighbours == 3) {
            alive = true
          }
        }
        println("replica: "+id.id+","+r+" ("+(row:Int)+","+(col: Int)+") is " + (if(alive) "alive" else "dead"))
        Thread.sleep(1000)
      }

    }
  )

}

class ConwayGameOfLife extends Algorithm[CgolIO,CgolProcess] {
  
  val spec = TrivialSpec

  def process = new CgolProcess()

}

//utils about neighbours overlay
object ConwayGameOfLife {

  def toID(rows: Int, cols: Int, r0: Int, c0: Int): ProcessID = {
    val r1 = if (r0 < 0) r0 + rows
             else if (r0 >= rows) r0 - rows
             else r0 
    val c1 = if (c0 < 0) c0 + cols
             else if (c0 >= cols) c0 - cols
             else c0 
    val id = r1 * cols + c1
    new ProcessID(id.toShort)
  }

  def getNeighbours(rows: Int, cols: Int, id: Int): Set[ProcessID] = {
    val row = id / cols
    val col = id % cols
    assert(toID(rows, cols, row, col) == new ProcessID(id.toShort))
    Set[ProcessID](
      toID(rows, cols, row - 1, col - 1),
      toID(rows, cols, row - 1, col    ),
      toID(rows, cols, row - 1, col + 1),

      toID(rows, cols, row    , col - 1),
      //self is not a neighbour
      toID(rows, cols, row    , col + 1),

      toID(rows, cols, row + 1, col - 1),
      toID(rows, cols, row + 1, col    ),
      toID(rows, cols, row + 1, col + 1)
    )
  }

}

object CgolRunner extends RTOptions {
  
  var rows = 5
  newOption("-rows", dzufferey.arg.Int( i => rows = i), "(default = 5)")
  var cols = 5
  newOption("-cols", dzufferey.arg.Int( i => cols = i), "(default = 5)")

  var confFile = "src/test/resources/15replicas-conf.xml"
  
  val usage = "..."
  
  var rt: Runtime[CgolIO,CgolProcess] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val start = java.lang.System.currentTimeMillis()
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new ConwayGameOfLife
    rt = new Runtime(alg, this, defaultHandler(_))
    rt.startService

    val io = new CgolIO(id, rows, cols, scala.util.Random.nextBoolean)
    val cur = java.lang.System.currentTimeMillis()
    Thread.sleep(8000 + start - cur)
    Console.println("replica " + id + " starting with " + io.init)
    rt.startInstance(0, io)
    Thread.sleep(20000)
    System.exit(0)
  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
      }
    }
  )

}
