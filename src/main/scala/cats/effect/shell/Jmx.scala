package cats.effect.shell

import com.sun.tools.attach.*
import java.lang.management.{ManagementFactory, MemoryMXBean, RuntimeMXBean, PlatformManagedObject, ThreadMXBean}
import javax.management.MBeanServerConnection
import javax.management.remote.{JMXConnector, JMXConnectorFactory, JMXServiceURL}
import scala.jdk.CollectionConverters.*
import java.io.IOException

case class Jmx(connection: Option[JMXConnector], mbeanServer: MBeanServerConnection):
  def connectionId: String = connection.map(_.getConnectionId()).getOrElse("self")

  private def proxy[A <: PlatformManagedObject](using ct: reflect.ClassTag[A]): A =
    ManagementFactory.getPlatformMXBean(mbeanServer, ct.runtimeClass.asInstanceOf[Class[A]])

  lazy val memory: MemoryMXBean = proxy[MemoryMXBean]
  lazy val runtime: RuntimeMXBean = proxy[RuntimeMXBean]
  lazy val thread: ThreadMXBean = proxy[ThreadMXBean]

  def threadInfos = thread.getThreadInfo(thread.getAllThreadIds())

object Jmx:
  def connectByDescriptor(vmd: VirtualMachineDescriptor): Jmx =
    try connectByVmId(vmd.id())
    catch case t: IOException if t.getMessage().contains("Can not attach to current VM") => connectSelf

  def connectByVmId(vmid: String): Jmx =
    val vm = VirtualMachine.attach(vmid)
    val jmxUrl = JMXServiceURL(vm.startLocalManagementAgent())
    val jmxCnx = JMXConnectorFactory.connect(jmxUrl)
    val mbeanServer = jmxCnx.getMBeanServerConnection()
    Jmx(Some(jmxCnx), mbeanServer)

  def connectSelf: Jmx =
    Jmx(None, ManagementFactory.getPlatformMBeanServer())

  def localProcesses: List[VirtualMachineDescriptor] = VirtualMachine.list().asScala.toList