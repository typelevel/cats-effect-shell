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

import scala.concurrent.Future
import cats.effect.{IO, IOApp, ExitCode}
import fs2.{Stream, Chunk}
import tui.*
import tui.crossterm.{CrosstermJni, Duration, Event, KeyCode}
import tui.widgets.*
import scala.concurrent.duration.*
import cats.effect.std.Dispatcher
import java.lang.management.ThreadInfo

object Main extends IOApp:

  def run(args: List[String]) =
    Dispatcher
      .parallel[IO]
      .use: dispatcher =>
        IO.blocking:
          val vmid = args.headOption
          vmid.map: connectionId =>
            val jmx = Jmx.connectByVmId(connectionId)
            ConnectionState.unsafeStartConnect(connectionId, jmx, dispatcher)
        .flatMap: cnxState =>
            IO.blocking(withTerminal((jni, terminal) => run(terminal, jni, cnxState, dispatcher)))
              .as(ExitCode.Success)

  def run(
      terminal: Terminal,
      jni: CrosstermJni,
      optCnxState: Option[ConnectionState],
      dispatcher: Dispatcher[IO]
  ): Unit =
    optCnxState match
      case Some(cnxState) =>
        val shouldExit = runMonitoringProcess(terminal, jni, cnxState)
        if shouldExit then () else run(terminal, jni, None, dispatcher)
      case None =>
        runSelect(terminal, jni, dispatcher) match
          case cnxState @ Some(_) => run(terminal, jni, cnxState, dispatcher)
          case None               => ()

  case class ProcessSelectionState(
      list: StatefulList[com.sun.tools.attach.VirtualMachineDescriptor],
      var lastRefresh: Long
  ):
    def possiblyRefresh(): Unit =
      if (lastRefresh - System.currentTimeMillis()).abs > 1000L then refresh()

    def refresh(): Unit =
      list.setItems(Jmx.localProcesses.toArray)
      lastRefresh = System.currentTimeMillis()

  def runSelect(
      terminal: Terminal,
      jni: CrosstermJni,
      dispatcher: Dispatcher[IO]
  ): Option[ConnectionState] =
    var done = false
    var result: Option[ConnectionState] = None
    val state = ProcessSelectionState(StatefulList.fromItems(Array.empty), 0L)
    while !done do
      state.possiblyRefresh()
      terminal.draw(f => uiSelect(f, state))
      val polled = jni.poll(Duration(1L, 0))
      if polled then
        jni.read() match
          case key: Event.Key =>
            key.keyEvent.code match
              case char: KeyCode.Char =>
                char.c() match
                  case 'q' => done = true
                  case 'j' => state.list.next()
                  case 'k' => state.list.previous()
                  case _   => ()
              case _: KeyCode.Down => state.list.next()
              case _: KeyCode.Up   => state.list.previous()
              case char: KeyCode.Enter =>
                state.list.selectedItem match
                  case Some(vmd) =>
                    done = true
                    val connection =
                      ConnectionState.unsafeStartConnect(
                        vmd.id(),
                        Jmx.connectByDescriptor(vmd),
                        dispatcher
                      )
                    result = Some(connection)
                  case None => ()
              case _ => ()
          case _ => ()
    result

  val Bold = Style.DEFAULT.addModifier(Modifier.BOLD)

  def controlsText(controls: (String, String)*): Text =
    val controlSpans = Stream
      .chunk(Chunk.from(controls))
      .map((k, v) => Stream(Span.styled(k, Bold), Span.nostyle(" = "), Span.nostyle(v)))
      .intersperse(Stream(Span.nostyle(", ")))
      .flatten
      .toList
    val spans = Span.nostyle("Controls: ") :: controlSpans
    Text.from(spans*)

  def uiSelect(f: Frame, state: ProcessSelectionState): Unit =
    val chunks = Layout(
      direction = Direction.Vertical,
      constraints = Array(Constraint.Min(2), Constraint.Percentage(100))
    ).split(f.size)
    f.renderWidget(
      ListWidget(items =
        Array(
          ListWidget.Item(controlsText("↑↓" -> "scroll", "↲" -> "connect", "q" -> "quit")),
          ListWidget.Item(Text.from(Span.nostyle("Select a process to monitor:")))
        )
      ),
      chunks(0)
    )
    val items =
      state.list.items.map(vmd =>
        ListWidget.Item(Text.from(Span.nostyle(Formats.vmDescriptor(vmd))))
      )
    val processes = ListWidget(
      block = Some(BlockWidget(title = Some(Spans.nostyle("Processes")), borders = Borders.ALL)),
      items = items,
      highlightSymbol = Some(">> "),
      highlightStyle = Bold
    )
    f.renderStatefulWidget(processes, chunks(1))(state.list.state)

  case class ProcessMonitoringState(
      jmx: Jmx,
      list: StatefulList[ThreadInfo],
      var lastRefresh: Long
  ):
    def possiblyRefresh(): Unit =
      if (lastRefresh - System.currentTimeMillis()).abs > 1000L then refresh()

    def refresh(): Unit =
      list.setItems(jmx.threadInfos)
      lastRefresh = System.currentTimeMillis()

  def runMonitoringProcess(
      terminal: Terminal,
      jni: CrosstermJni,
      cnxState0: ConnectionState
  ): Boolean =
    var done = false
    var shouldExit = false
    var cnxState = cnxState0
    var monitoringState: ProcessMonitoringState = null
    while !done do
      cnxState = cnxState match
        case ConnectionState.Connecting(_, Some(Right(jmx)), _) => ConnectionState.Connected(jmx)
        case ConnectionState.Connecting(id, Some(Left(err)), _) =>
          ConnectionState.Disconnected(id, Some(err))
        case _ => cnxState
      if monitoringState eq null then
        cnxState match
          case ConnectionState.Connected(jmx) =>
            monitoringState = ProcessMonitoringState(jmx, StatefulList.fromItems(Array.empty), 0L)
          case _ => ()
      else monitoringState.possiblyRefresh()

      terminal.draw(f =>
        cnxState match
          case cnx: ConnectionState.Connected => uiConnected(f, cnx.jmx, monitoringState)
          case _                              => uiDisconnected(f, cnxState)
      )
      val polled = jni.poll(Duration(0L, 100_000_000))
      if polled then
        jni.read() match
          case key: Event.Key =>
            key.keyEvent.code match
              case char: KeyCode.Char if char.c() == 'q' =>
                done = true; shouldExit = true
              case char: KeyCode.Char if char.c() == 'd' =>
                cnxState match
                  case ConnectionState.Connecting(_, _, cancel) => cancel()
                  case ConnectionState.Connected(jmx)           => jmx.disconnect()
                  case _                                        => ()
                done = true
              case _ => ()
          case _ => ()
    shouldExit

  def uiDisconnected(f: Frame, cnxState: ConnectionState): Unit =
    val cnxStateSpan = cnxState match
      case _: ConnectionState.Connecting => Span.styled(s" (CONNECTING)", Bold.fg(Color.Cyan))
      case _: ConnectionState.Connected  => Span.styled(s" (CONNECTED)", Bold.fg(Color.Green))
      case ConnectionState.Disconnected(_, err) =>
        Span.styled(
          s" (DISCONNECTED)",
          Bold.fg(if err.isDefined then Color.Red else Color.DarkGray)
        )
    val err = cnxState match
      case ConnectionState.Disconnected(_, Some(err)) => err.getMessage()
      case _                                          => ""
    val summary = ListWidget(
      items = Array(
        ListWidget.Item(
          Text.from(Span.nostyle("Connection: "), Span.nostyle(cnxState.connectionId), cnxStateSpan)
        ),
        ListWidget.Item(controlsText("d" -> "disconnect", "q" -> "quit")),
        ListWidget.Item(Text.from(Span.styled(err, Style.DEFAULT.fg(Color.Red))))
      )
    )
    f.renderWidget(summary, f.size)

  def uiConnected(f: Frame, jmx: Jmx, state: ProcessMonitoringState): Unit =
    val chunks = Layout(
      direction = Direction.Vertical,
      constraints = Array(Constraint.Min(4), Constraint.Percentage(100))
    ).split(f.size)

    val heap = jmx.memory.getHeapMemoryUsage()
    val heapPercentage = (heap.getUsed() / heap.getMax().toDouble) * 100

    val summary = ListWidget(
      items = Array(
        ListWidget.Item(
          Text
            .from(
              Span.nostyle("Connection: "),
              Span.nostyle(jmx.connectionId),
              Span.styled(" (CONNECTED)", Bold.fg(Color.Green))
            )
        ),
        ListWidget.Item(
          Text.from(
            Span.styled("Uptime: ", Bold),
            Span.nostyle(Formats.durationToDaysThroughSeconds(jmx.runtime.getUptime().millis))
          )
        ),
        ListWidget.Item(
          Text.from(
            Span.styled("Heap: ", Bold),
            Span.nostyle(
              s"${heapPercentage.toInt}% (${Formats.giga(heap.getUsed)}GB / ${Formats.giga(heap.getMax)}GB)"
            )
          )
        ),
        ListWidget.Item(controlsText("d" -> "disconnect", "q" -> "quit"))
      )
    )
    f.renderWidget(summary, chunks(0))

    val threads = ListWidget(
      block = Some(BlockWidget(title = Some(Spans.nostyle("Threads")), borders = Borders.ALL)),
      items = state.list.items.map(ti =>
        ListWidget.Item(
          Text.from(
            Span.nostyle(s"[${ti.getThreadId()}] ${ti.getThreadName} (${ti.getThreadState()})")
          )
        )
      )
    )
    f.renderStatefulWidget(threads, chunks(1))(state.list.state)
