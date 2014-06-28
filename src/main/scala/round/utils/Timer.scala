package round.utils

import io.netty.util.{HashedWheelTimer, Timeout, TimerTask}
import java.util.concurrent.TimeUnit

object Timer extends HashedWheelTimer() {

  final val unit = TimeUnit.MILLISECONDS

  def newTimeout(task: TimerTask, delay: Long): Timeout = {
    newTimeout(task, delay, unit)
  }

}
