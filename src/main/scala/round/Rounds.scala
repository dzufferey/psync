package round

trait Rounds[IO] {
  self: Algorithm[IO] =>

  abstract class Round[A] {

    final protected def broadcast(msg: A): Set[(A, Process)] = {
      sys.error("not yet implemented")
    }

    def send(): Set[(A, Process)]

    def update(mailbox: Set[(Int, Process)]): Unit

  }

}

