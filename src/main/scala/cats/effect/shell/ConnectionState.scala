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
import cats.effect.IO
import cats.effect.std.Dispatcher

enum ConnectionState:
  case Connecting(
      override val connectionId: String,
      var connection: Option[Either[Throwable, Jmx]],
      var cancel: () => Future[Unit]
  )
  case Connected(jmx: Jmx)
  case Disconnected(override val connectionId: String, error: Option[Throwable])

  def connectionId: String = this match
    case Connecting(connectionId, _, _) => connectionId
    case Connected(jmx)                 => jmx.connectionId
    case Disconnected(connectionId, _)  => connectionId

object ConnectionState:

  def apply(dispatcher: Dispatcher[IO], connectionId: String): IO[ConnectionState] =
    IO.blocking:
      val jmx = Jmx.connectByVmId(connectionId)
      ConnectionState.unsafeStartConnect(dispatcher, connectionId, jmx)

  def unsafeStartConnect(
      dispatcher: Dispatcher[IO],
      connectionId: String,
      j: IO[Jmx]
  ): ConnectionState =
    val state: ConnectionState.Connecting =
      ConnectionState.Connecting(connectionId, None, () => Future.unit)
    val cancel = dispatcher.unsafeRunCancelable(j.attempt.map: cnx =>
      state.connection = Some(cnx))
    state.cancel = cancel
    state
