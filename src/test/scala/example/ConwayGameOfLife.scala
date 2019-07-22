package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._

//on a torus

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
    new EventRound[Boolean]{
      
      var received = 0
      var aliveNeighbours = 0

      def init = {
        received = 0
        aliveNeighbours = 0
        Progress.waitMessage
      }
    
      def send: Map[ProcessID,Boolean] = {
        neighbours.map( _ -> (alive: Boolean) ).toMap
      }

      def receive(sender: ProcessID, payload: Boolean) = {
        assert(neighbours contains sender)
        received += 1
        if (payload) {
          aliveNeighbours += 1
        }
        if (received == neighbours.size) Progress.goAhead
        else Progress.waitMessage
      }

      override def finishRound(didTimeout: Boolean) = {
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
        true
      }

    }
  )

}

class ConwayGameOfLife(rt: Runtime) extends Algorithm[CgolIO,CgolProcess](rt) {
  
  val spec = TrivialSpec

  def process = new CgolProcess

  def dummyIO = new CgolIO(0,0,0,false)
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

object CgolRunner extends Runner {
  
  var rows = 5
  newOption("-rows", dzufferey.arg.Int( i => rows = i), "(default = 5)")
  var cols = 5
  newOption("-cols", dzufferey.arg.Int( i => cols = i), "(default = 5)")

  override def defaultConfFile = "src/test/resources/25replicas-conf.xml"
  
  def onStart {
    val start = java.lang.System.currentTimeMillis()
    val alg = new ConwayGameOfLife(rt)
    val io = new CgolIO(id, rows, cols, scala.util.Random.nextBoolean)
    val cur = java.lang.System.currentTimeMillis()
    Thread.sleep(8000 + start - cur)
    Console.println("replica " + id + " starting with " + io.init)
    alg.startInstance(0, io)
    Thread.sleep(20000)
    System.exit(0)
  }

}
