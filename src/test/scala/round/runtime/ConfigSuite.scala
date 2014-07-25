package round.runtime

import org.scalatest._

class ConfigSuite extends FunSuite {

  val resources = "src/test/resources/"

  test("sample-conf.xml") {
    val (peers, conf) = Config.parse(resources + "sample-conf.xml")
    assert(peers.size == 4)
    assert(peers(0) == Replica(0, "127.0.0.1", 4444))
    assert(peers(1) == Replica(1, "127.0.0.1", 4445))
    assert(peers(2) == Replica(2, "127.0.0.1", 4446))
    assert(peers(3) == Replica(3, "127.0.0.1", 4447))
    assert(conf.size == 3)
    assert(conf.get("transport layer") == Some("UDP"))
    assert(conf.get("timeout") == Some("200"))
    assert(conf.get("group") == Some("nio"))
  }

}
