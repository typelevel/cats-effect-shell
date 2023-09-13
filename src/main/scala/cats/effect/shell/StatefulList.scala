package cats.effect.shell

import tui.widgets.ListWidget

case class StatefulList[A](
  state: ListWidget.State,
  var items: Array[A]
):
  def next(): Unit =
    val newIndex = state.selected match
      case Some(i) => (i + 1).min(items.length - 1)
      case None => 0
    state.select(Some(newIndex))

  def previous(): Unit =
    val newIndex = state.selected match
      case Some(i) => (i - 1).max(0)
      case None => 0
    state.select(Some(newIndex))

  def setItems(newItems: Array[A]): Unit =
    val oldSelectedItem = selectedItem
    items = newItems
    oldSelectedItem.foreach: otm =>
      val indexOfItemToSelect = items.indexOf(otm)
      if indexOfItemToSelect >= 0 then state.select(Some(indexOfItemToSelect)) else state.select(None)

  def selectedItem: Option[A] =
    state.selected.map(items)
    
    
object StatefulList:
  def fromItems[A](items: Array[A]): StatefulList[A] =
    StatefulList(ListWidget.State(), items)