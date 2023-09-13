package cats.effect.shell

import scala.concurrent.duration.FiniteDuration
import com.sun.tools.attach.VirtualMachineDescriptor

object Formats:

  def durationToDaysThroughSeconds(value: FiniteDuration): String =
    val totalSeconds = value.toSeconds
    val seconds = totalSeconds % 60
    val totalMinutes = totalSeconds / 60
    val minutes = totalMinutes % 60
    val totalHours = totalMinutes / 60
    val hours = totalHours % 24
    val days = totalHours / 24
    val daysStr = if days > 0 then Some(s"${days}d") else None
    val hoursStr = if hours > 0 then Some(s"${hours}h") else None
    val minutesStr = if minutes > 0 then Some(s"${minutes}m") else None
    val secondsStr = if seconds > 0 then Some(s"${seconds}s") else None
    List(daysStr, hoursStr, minutesStr, secondsStr).flatten.mkString(" ")


  def giga(value: Long): String = f"${value / 1_000_000_000.0}%.2f"

  def vmDescriptor(vmd: VirtualMachineDescriptor): String =
    s"${vmd.id()} ${vmd.displayName()}"
