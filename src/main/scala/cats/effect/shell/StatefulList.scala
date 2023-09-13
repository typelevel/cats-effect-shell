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

import tui.widgets.ListWidget

case class StatefulList[A](
    state: ListWidget.State,
    var items: Array[A]
):
  def next(): Unit =
    val newIndex = state.selected match
      case Some(i) => (i + 1).min(items.length - 1)
      case None    => 0
    state.select(Some(newIndex))

  def previous(): Unit =
    val newIndex = state.selected match
      case Some(i) => (i - 1).max(0)
      case None    => 0
    state.select(Some(newIndex))

  def setItems(newItems: Array[A]): Unit =
    val oldSelectedItem = selectedItem
    items = newItems
    oldSelectedItem.foreach: otm =>
      val indexOfItemToSelect = items.indexOf(otm)
      if indexOfItemToSelect >= 0 then state.select(Some(indexOfItemToSelect))
      else state.select(None)

  def selectedItem: Option[A] =
    state.selected.map(items)

object StatefulList:
  def fromItems[A](items: Array[A]): StatefulList[A] =
    StatefulList(ListWidget.State(), items)
