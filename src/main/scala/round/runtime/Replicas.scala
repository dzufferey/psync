package round.runtime

import round._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import java.net.InetSocketAddress

case class Replica(val id: ProcessID, val address: String, val port: Int) {

  lazy val netAddress = new InetSocketAddress(address, port)
  def getNetAddress = netAddress

}

class Group(val self: ProcessID, val replicas: Array[Replica]) {

  def others = replicas.filter( r => r != null && r.id != self)

  def contains(pid: ProcessID) = {
    val i = pid.id
    i >= 0 && i < replicas.length && replicas(i) != null
  }

  def getSafe(address: InetSocketAddress): Option[Replica] = {
    try Some(get(address))
    catch { case _: Exception => None }
  }
  
  def get(pid: ProcessID): Replica = replicas(pid.id)

  def get(address: InetSocketAddress): Replica = {
    val ip = address.getAddress.getHostAddress
    val port = address.getPort
    try {
      replicas.find( r => r.address == ip && r.port == port).get
    } catch {
      case e: Exception =>
        Logger("Replica", Warning, "could not find replica " + ip + ":" + port)
        throw e
    }
  }

  def idToInet(pid: ProcessID): InetSocketAddress = {
    replicas(pid.id).getNetAddress
  }

  def inetToId(address: InetSocketAddress): ProcessID = get(address).id

  def size: Int = {
    var n = 0
    for(i <- replicas.indices if replicas(i) != null) {
      n += 1
    }
    //replicas.size
    n
  }

  def add(r: Replica) = {
    assert( r.id != self &&
            replicas.forall( r2 => r2.id != r.id && (r2.address != r.address || r2.port != r.port))
          )
    val maxIdx = math.max(r.id.id, replicas.filter(_ != null).map(_.id.id).max)
    val a2 = Array.ofDim[Replica](maxIdx+1)
    for(i <- replicas.indices) {
      a2(i) = replicas(i)
    }
    a2(r.id.id) = r
    new Group(self, a2)
  }

  def remove(id: ProcessID) = {
    assert( id != self && replicas.exists( r2 => r2.id == id ) )
    val a2 = replicas.clone
    a2(id.id) = null
    new Group(self, a2)
  }

  def compact = {
    val rs = replicas.toList.filter(_ != null)
    Group(self, rs)
  }

  def firstAvailID: ProcessID = {
    for(i <- replicas.indices if replicas(i) == null) {
      return new ProcessID(i.toShort)
    }
    new ProcessID(replicas.size.toShort)
  }
    
  def asList = replicas.toList.filter(_ != null)

  override def toString = {
    "group (self = " + self.id + ")\n  " + asList.mkString("\n  ")
  }

}

object Group {

  //rename the replica so that the Ids start at 0 and do not have gaps
  def renameReplica(lst: List[Replica]): (List[Replica], Map[ProcessID, ProcessID]) = {
    val sorted = lst.sortWith( (a, b) => a.id.id < b.id.id ).zipWithIndex
    val idMap = sorted.foldLeft(Map.empty[ProcessID,ProcessID])( (acc, p) => acc + (p._1.id -> new ProcessID(p._2.toShort)))
    val renamed = sorted.map{ case (r, id) =>
        if (r.id.id != id.toShort) Replica(idMap(r.id), r.address, r.port) else r }
    (renamed, idMap)
  }

  def apply(self: ProcessID, lst: List[Replica]): Group = {
    val (lst2, map) = renameReplica(lst)
    new Group(map.getOrElse(self, self), lst2.toArray)
  }

}

//a 'mutable' wrapper around a group
class Directory(private var g: Group) {

  private val lock = new java.util.concurrent.locks.ReentrantLock()

  @inline private def sync[A](fct: => A) = {
    lock.lock
    try {
      fct
    } finally {
      lock.unlock
    }
  }

  def self = sync( g.self )
  
  def others = sync( g.others )

  def group = g

  def group_=(grp: Group) = sync{
    g = grp
  }
  
  def contains(pid: ProcessID) = sync(g.contains(pid))

  def getSafe(address: InetSocketAddress) = sync(g.getSafe(address))

  def get(id: ProcessID) = sync(g.get(id))

  def get(address: InetSocketAddress) = sync(g.get(address))

  def idToInet(processId: ProcessID) = sync(g.idToInet(processId))

  def inetToId(address: InetSocketAddress) = sync(g.inetToId(address))

  def addReplica(r: Replica) = sync{
    g = g.add(r)
  }

  def removeReplica(id: ProcessID) = sync{
    g = g.remove(id)
  }

  def compact = sync{
    g = g.compact
  }
  
  def firstAvailID = sync( g.firstAvailID )
  
  def asList = sync( g.asList )

  override def toString = g.toString

}
