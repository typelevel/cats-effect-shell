package cats.effect.shell

import scala.concurrent.Future
import cats.effect.IO
import cats.effect.std.Dispatcher

enum ConnectionState:
  case Connecting(
    override val connectionId: String,
    var jmx: Option[Jmx],
    var cancel: () => Future[Unit]
  )
  case Connected(jmx: Jmx)
  case Disconnected(override val connectionId: String)

  def connectionId: String = this match
    case Connecting(connectionId, _, _) => connectionId
    case Connected(jmx)                 => jmx.connectionId
    case Disconnected(connectionId)     => connectionId

object ConnectionState:
  def unsafeStartConnect(
      connectionId: String,
      j: IO[Jmx],
      dispatcher: Dispatcher[IO]
  ): ConnectionState =
    val state: ConnectionState.Connecting =
      ConnectionState.Connecting(connectionId, None, () => Future.unit)
    val cancel = dispatcher.unsafeRunCancelable(j.map(jmx => state.jmx = Some(jmx)))
    state.cancel = cancel
    state

