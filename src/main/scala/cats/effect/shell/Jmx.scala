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

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.IO
import com.sun.tools.attach.*
import java.lang.management.{
  ManagementFactory,
  MemoryMXBean,
  RuntimeMXBean,
  PlatformManagedObject,
  ThreadMXBean
}
import javax.management.MBeanServerConnection
import javax.management.remote.{JMXConnector, JMXConnectorFactory, JMXServiceURL}
import java.io.IOException
import java.rmi.server.RMISocketFactory
import java.net.{Socket, ServerSocket, InetSocketAddress}
import javax.management.remote.rmi.RMIConnectorServer
import java.rmi.server.RMIClientSocketFactory

case class Jmx(connection: Option[JMXConnector], mbeanServer: MBeanServerConnection):
  def connectionId: String = connection.map(_.getConnectionId()).getOrElse("self")

  private def proxy[A <: PlatformManagedObject](using ct: reflect.ClassTag[A]): A =
    ManagementFactory.getPlatformMXBean(mbeanServer, ct.runtimeClass.asInstanceOf[Class[A]])

  lazy val memory: MemoryMXBean = proxy[MemoryMXBean]
  lazy val runtime: RuntimeMXBean = proxy[RuntimeMXBean]
  lazy val thread: ThreadMXBean = proxy[ThreadMXBean]

  def threadInfos = thread.getThreadInfo(thread.getAllThreadIds()).filterNot(_ == null)

  def disconnect(): Unit = connection.foreach(_.close())

object Jmx:
  def connectByDescriptor(vmd: VirtualMachineDescriptor): IO[Jmx] =
    connectByVmId(vmd.id()).recover:
      case t: IOException if t.getMessage().contains("Can not attach to current VM") => connectSelf

  def connectByVmId(vmid: String): IO[Jmx] = IO.interruptibleMany:
    val vm = VirtualMachine.attach(vmid)
    val jmxUrl = JMXServiceURL(vm.startLocalManagementAgent())
    vm.detach()
    val duration = 5.seconds
    val csf = new RMIClientSocketFactory {
      override def createSocket(host: String, port: Int): Socket =
        val socket = new Socket()
        socket.setSoTimeout(duration.toMillis.toInt)
        socket.connect(new InetSocketAddress(host, port), duration.toMillis.toInt)
        socket
    }
    val env = Map(RMIConnectorServer.RMI_CLIENT_SOCKET_FACTORY_ATTRIBUTE -> csf)
    val jmxCnx = JMXConnectorFactory.connect(jmxUrl, env.asJava)
    val mbeanServer = jmxCnx.getMBeanServerConnection()
    Jmx(Some(jmxCnx), mbeanServer)

  def connectSelf: Jmx =
    Jmx(None, ManagementFactory.getPlatformMBeanServer())

  def localProcesses: List[VirtualMachineDescriptor] = VirtualMachine.list().asScala.toList
