package round.runtime

import scala.xml._

case class Replica(id: Short, address: String, port: Int)

object Config {

  val defaultConfigFile = "config.xml"

  def parseReplica(e: Node) = {
    Replica((e \ "id").text.toShort,
            (e \ "address").text,
            (e \ "port").text.toInt)
  }

  def parseOption(e: Node): (String, String) = {
    ((e \ "name").text, (e \ "value").text)
  }

  def parse(file: String = defaultConfigFile) = {
    val tree = XML.loadFile(file)
    //there is two parts:
    //the parameters: Map[String,String]
    //a list of peers: List[Replica]
    val params = (tree \ "parameters")(0).child.map(parseOption)
    val peers = (tree \ "peers")(0).child.map(parseReplica)
    (peers.toList,
     params.foldLeft(Map[String,String]())( (acc, kv) => acc + kv ))
  }

}
