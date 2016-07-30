package psync.runtime

import scala.xml._
import psync._

object Config {

  private def parseReplica(e: Node) = {
    val id = new ProcessID((e \ "id").text.toShort)
    val address = (e \ "address").text
    val port = (e \ "port").text.toInt
    val raw = Replica(id, address, port)
    raw.normalize
  }

  private def parseOption(e: Node): (String, String) = {
    ((e \ "name").text, (e \ "value").text)
  }

  def parse(file: String) = {
    val tree = XML.loadFile(file)
    val params = (tree \ "parameters" \\ "param").map(parseOption)
    val peers = (tree \ "peers" \\ "replica").map(parseReplica)
    (peers.toList,
     params.foldLeft(Map[String,String]())( (acc, kv) => acc + kv ))
  }

}
