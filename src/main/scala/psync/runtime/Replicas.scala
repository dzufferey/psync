package psync.runtime

import psync._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import java.net.InetSocketAddress

case class Replica(id: ProcessID, address: String, ports: Set[Int]) {
  assert(!ports.isEmpty, "replica cannot have an empty list of ports")

  lazy val netAddresses = ports.toArray.map( p => new InetSocketAddress(address, p) )
  def netAddress: InetSocketAddress = netAddresses(0)
  def netAddress(i: Int): InetSocketAddress = netAddresses(i.abs % netAddresses.size)

}

class Group(val self: ProcessID, val replicas: Array[Replica]) {

  def others = replicas.filter( r => r != null && r.id != self)

  def contains(pid: ProcessID) = {
    val i = pid.id
    i >= 0 && i < replicas.length && replicas(i) != null
  }

  def get(address: InetSocketAddress): Replica = {
    try getSafe(address).get
    catch {
      case e: Exception =>
        Logger("Replica", Warning, "could not find replica " + address.getAddress.getHostAddress + ":" + address.getPort)
        throw e
    }
  }
  
  def get(pid: ProcessID): Replica = replicas(pid.id)

  //TODO a map ?
  def getSafe(address: InetSocketAddress): Option[Replica] = {
    val ip = address.getAddress.getHostAddress
    val port = address.getPort
    replicas.find( r => r != null && r.address == ip && r.ports(port) )
  }

  def idToInet(pid: ProcessID) = {
    replicas(pid.id).netAddress
  }
  
  def idToInet(pid: ProcessID, i: Int) = {
    replicas(pid.id).netAddress(i)
  }

  def idToInets(pid: ProcessID) = {
    replicas(pid.id).netAddresses
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
            replicas.forall( r2 => r2.id != r.id && (r2.address != r.address || r2.ports != r.ports))
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
        if (r.id.id != id.toShort) Replica(idMap(r.id), r.address, r.ports) else r }
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

  def self = g.self
  
  def others = g.others

  def group = g

  def group_=(grp: Group) = sync{ g = grp }
  
  def contains(pid: ProcessID) = g.contains(pid)

  def size = g.size

  def getSafe(address: InetSocketAddress) = g.getSafe(address)

  def get(id: ProcessID) = g.get(id)

  def get(address: InetSocketAddress) = g.get(address)

  def idToInet(processId: ProcessID) = g.idToInet(processId)
  
  def idToInet(processId: ProcessID, i: Int) = g.idToInet(processId, i)

  def idToInets(processId: ProcessID) = g.idToInets(processId)

  def inetToId(address: InetSocketAddress) = g.inetToId(address)

  def addReplica(r: Replica) = sync{ g = g.add(r) }

  def removeReplica(id: ProcessID) = sync{ g = g.remove(id) }

  def compact = sync{ g = g.compact }
  
  def firstAvailID = g.firstAvailID
  
  def asList = g.asList

  override def toString = g.toString

}
