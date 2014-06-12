package round

trait Processes {
  self: Algorithm =>

  abstract class Process(val id: Short) {
  }

}

