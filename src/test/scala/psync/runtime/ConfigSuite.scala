package psync.runtime

import org.scalatest._
import psync._

class ConfigSuite extends FunSuite {

  val resources = "src/test/resources/"

  test("sample-conf.xml") {
    val (peers, conf) = Config.parse(resources + "sample-conf.xml")
    assert(peers.size == 4)
    assert(peers(0) == Replica(new ProcessID(0), "127.0.0.1", Set(4444)))
    assert(peers(1) == Replica(new ProcessID(1), "127.0.0.1", Set(4445)))
    assert(peers(2) == Replica(new ProcessID(2), "127.0.0.1", Set(4446)))
    assert(peers(3) == Replica(new ProcessID(3), "127.0.0.1", Set(4447)))
    assert(conf.size == 4)
    assert(conf.get("protocol") == Some("UDP"))
    conf.get("timeout").map(_.toInt)
    assert(conf.get("group") == Some("NIO"))
    //assert(conf.get("workers") == Some("1x"))
  }

}
