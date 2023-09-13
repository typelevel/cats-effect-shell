/*
 * Copyright 2023 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
