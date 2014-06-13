package round.runtime

import org.scalatest._

class ConfigSuite extends FunSuite {

  val resources = "src/test/resources/"

  test("sample-conf.xml") {
    val (peers, conf) = Config.parse(resources + "sample-conf.xml")
    assert(peers.size == 3)
    assert(peers(0) == Replica(0, "localhost", 4444))
    assert(peers(1) == Replica(1, "localhost", 4445))
    assert(peers(2) == Replica(2, "localhost", 4446))
    assert(conf.size == 2)
    assert(conf.get("transport layer") == Some("UDP"))
    assert(conf.get("timeout") == Some("200"))
  }

}
