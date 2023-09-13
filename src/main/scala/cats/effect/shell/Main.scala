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

import tui.*
import tui.crossterm.{CrosstermJni, Duration, Event, KeyCode}
import tui.widgets.*
import scala.concurrent.duration.*

@main def catsEffectShell(args: String*) =
  val vmid = args.headOption
  val jmx = vmid.map(Jmx.connectByVmId)
  withTerminal((jni, terminal) => run(terminal, jni, jmx))

def run(terminal: Terminal, jni: CrosstermJni, jmx: Option[Jmx]): Unit =
  jmx match
    case Some(jmx) =>
      val shouldExit = runMonitoringProcess(terminal, jni, jmx)
      if shouldExit then () else run(terminal, jni, None)
    case None =>
      runSelect(terminal, jni) match
        case jmx @ Some(_) => run(terminal, jni, jmx)
        case None => ()

case class ProcessSelectionState(
  list: StatefulList[com.sun.tools.attach.VirtualMachineDescriptor],
  var lastRefresh: Long
):
  def possiblyRefresh(): Unit =
    if (lastRefresh - System.currentTimeMillis()).abs > 1000L then refresh()

  def refresh(): Unit =
    list.setItems(Jmx.localProcesses.toArray)
    lastRefresh = System.currentTimeMillis()

def runSelect(terminal: Terminal, jni: CrosstermJni): Option[Jmx] =
  var done = false
  var result: Option[Jmx] = None
  val state = ProcessSelectionState(StatefulList.fromItems(Array.empty), 0L)
  while !done do
    state.possiblyRefresh()
    terminal.draw(f => uiSelect(f, state))
    val polled = jni.poll(Duration(1L, 0))
    if polled then
      jni.read() match
        case key: Event.Key =>
          key.keyEvent.code match
            case char: KeyCode.Char if char.c() == 'q' =>
              done = true
            case char: KeyCode.Down => state.list.next()
            case char: KeyCode.Up => state.list.previous()
            case char: KeyCode.Enter =>
              state.list.selectedItem match
                case Some(vmd) =>
                  done = true
                  result = Some(Jmx.connectByDescriptor(vmd))
                case None => ()
            case _ => ()
        case _ => ()
  result

def uiSelect(f: Frame, state: ProcessSelectionState): Unit =
  val chunks = Layout(
    direction = Direction.Vertical,
    constraints = Array(Constraint.Min(1), Constraint.Percentage(100))
  ).split(f.size)
  f.renderWidget(ParagraphWidget(Text.from(Span.nostyle("Select a process to monitor:"))), chunks(0))
  val items = state.list.items.map(vmd => ListWidget.Item(Text.from(Span.nostyle(Formats.vmDescriptor(vmd)))))
  val processes = ListWidget(
    block = Some(BlockWidget(title = Some(Spans.nostyle("Processes")), borders = Borders.ALL)),
    items = items,
    highlightSymbol = Some(">> "),
    highlightStyle = Style.DEFAULT.addModifier(Modifier.BOLD)
  )
  f.renderStatefulWidget(processes, chunks(1))(state.list.state)


def runMonitoringProcess(terminal: Terminal, jni: CrosstermJni, jmx: Jmx): Boolean =
  var done = false
  var shouldExit = false
  while !done do
    terminal.draw(f => uiConnected(f, jmx))
    val polled = jni.poll(Duration(1L, 0))
    if polled then
      jni.read() match
        case key: Event.Key =>
          key.keyEvent.code match
            case char: KeyCode.Char if char.c() == 'q' =>
              done = true; shouldExit = true
            case char: KeyCode.Char if char.c() == 'd' =>
              done = true
            case _ => ()
        case _ => ()
  shouldExit

def uiConnected(f: Frame, jmx: Jmx): Unit =
  val chunks = Layout(
    direction = Direction.Vertical,
    constraints = Array(Constraint.Min(3), Constraint.Percentage(100))
  ).split(f.size)

  val bold = Style.DEFAULT.addModifier(Modifier.BOLD)
  val heap = jmx.memory.getHeapMemoryUsage()
  val heapPercentage = (heap.getUsed() / heap.getMax().toDouble) * 100

  val summary = ListWidget(
    items = Array(
      ListWidget.Item(
        Text.from(Span.nostyle(jmx.connectionId), Span.styled(" (CONNECTED)", bold))
      ),
      ListWidget.Item(
        Text.from(Span.styled("Uptime: ", bold), Span.nostyle(Formats.durationToDaysThroughSeconds(jmx.runtime.getUptime().millis)))
      ),
      ListWidget.Item(
        Text.from(Span.styled("Heap: ", bold), Span.nostyle(s"${heapPercentage.toInt}% (${Formats.giga(heap.getUsed)}GB / ${Formats.giga(heap.getMax)}GB)"))
      )
    )
  )
  f.renderWidget(summary, chunks(0))

  val threads = ListWidget(
    block = Some(BlockWidget(title = Some(Spans.nostyle("Threads")), borders = Borders.ALL)),
    items = jmx.threadInfos.map(ti =>
      ListWidget.Item(Text.from(Span.nostyle(s"[${ti.getThreadId()}] ${ti.getThreadName} (${ti.getThreadState()})")))
    )
  )
  f.renderWidget(threads, chunks(1))
