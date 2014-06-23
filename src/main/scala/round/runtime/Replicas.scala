package round.runtime

import round.Algorithm._

import java.net.InetSocketAddress

case class Replica(val id: ProcessID, val address: String, val port: Int) {

  lazy val netAddress = new InetSocketAddress(address, port)
  def getNetAddress = netAddress

}

  //TODO communication group
  //something that could also be modified:
  //  crash -> removing
  //  joining -> adding
  
class Group(val self: ProcessID, val replicas: Array[Replica]) {

  //TODO some maps for faster access.

  def getSafe(address: InetSocketAddress): Option[Replica] = {
    try Some(get(address))
    catch { case _: Exception => None }
  }
  
  def get(id: ProcessID): Replica = replicas(id)

  def get(address: InetSocketAddress): Replica = {
    val ip = address.getAddress.getHostAddress
    val port = address.getPort
    replicas.find( r => r.address == ip && r.port == port).get
  }

  def idToInet(processId: ProcessID): InetSocketAddress = {
    replicas(processId).getNetAddress
  }

  def inetToId(address: InetSocketAddress): ProcessID = get(address).id

}

object Group {

  //rename the replica so that the Ids start at 0 and do not have gaps
  def renameReplica(lst: List[Replica]): (List[Replica], Map[ProcessID, ProcessID]) = {
    val sorted = lst.sortWith( (a, b) => a.id < b.id ).zipWithIndex
    val idMap = sorted.foldLeft(Map.empty[ProcessID,ProcessID])( (acc, p) => acc + (p._1.id -> p._2.toShort))
    val renamed = sorted.map{ case (r, id) =>
        if (r.id != id.toShort) Replica(idMap(r.id), r.address, r.port) else r }
    (renamed, idMap)
  }

  def apply(self: ProcessID, lst: List[Replica]): Group = {
    val (lst2, map) = renameReplica(lst)
    new Group(map(self), lst2.toArray)
  }

}

//a 'mutable' wrapper around a group
class Directory(private var g: Group) {

  //TODO options to modify the group

  def group = g

  def getSafe(address: InetSocketAddress) = g.getSafe(address)

  def get(id: ProcessID) = g.get(id)

  def get(address: InetSocketAddress) = g.get(address)

  def idToInet(processId: ProcessID) = g.idToInet(processId)

  def inetToId(address: InetSocketAddress) = g.inetToId(address)

}
