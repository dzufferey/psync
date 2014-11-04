package round.runtime

import org.scalatest._
import round._

class ConfigSuite extends FunSuite {

  val resources = "src/test/resources/"

  test("sample-conf.xml") {
    val (peers, conf) = Config.parse(resources + "sample-conf.xml")
    assert(peers.size == 4)
    assert(peers(0) == Replica(new ProcessID(0), "127.0.0.1", 4444))
    assert(peers(1) == Replica(new ProcessID(1), "127.0.0.1", 4445))
    assert(peers(2) == Replica(new ProcessID(2), "127.0.0.1", 4446))
    assert(peers(3) == Replica(new ProcessID(3), "127.0.0.1", 4447))
    assert(conf.size == 3)
    assert(conf.get("transport layer") == Some("UDP"))
    assert(conf.get("timeout") == Some("100"))
    assert(conf.get("group") == Some("nio"))
  }

}
