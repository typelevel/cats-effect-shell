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

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import javax.management.{MBeanServerConnection, ObjectInstance, ObjectName}

object CeJmx:

  case class ComputePoolStats(
      workerThreadCount: Int,
      activeThreadCount: Int,
      searchingThreadCount: Int,
      blockedWorkerThreadCount: Int,
      localQueueFiberCount: Int,
      suspendedFiberCount: Int
  )

  private def findMBean(mbeanServer: MBeanServerConnection, name: String): Option[ObjectInstance] =
    val namePattern = new ObjectName(name)
    mbeanServer.queryMBeans(namePattern, null).asScala.headOption

  def snapshotComputePoolStats(mbeanServer: MBeanServerConnection): Option[ComputePoolStats] =
    findMBean(mbeanServer, "cats.effect.unsafe.metrics:type=ComputePoolSampler-*").flatMap:
      computePoolMBean =>
        val attrNames = Array[String](
          "WorkerThreadCount",
          "ActiveThreadCount",
          "SearchingThreadCount",
          "BlockedWorkerThreadCount",
          "LocalQueueFiberCount",
          "SuspendedFiberCount"
        )
        val attrs =
          mbeanServer.getAttributes(computePoolMBean.getObjectName(), attrNames).asList.asScala
        if attrs.size == attrNames.length then
          Some(
            ComputePoolStats(
              attrs(0).getValue.asInstanceOf[java.lang.Integer].intValue,
              attrs(1).getValue.asInstanceOf[java.lang.Integer].intValue,
              attrs(2).getValue.asInstanceOf[java.lang.Integer].intValue,
              attrs(3).getValue.asInstanceOf[java.lang.Integer].intValue,
              attrs(4).getValue.asInstanceOf[java.lang.Long].intValue,
              attrs(5).getValue.asInstanceOf[java.lang.Long].intValue
            )
          )
        else None
