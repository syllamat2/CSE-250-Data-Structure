/**
 * cse250.pa2.SortedList.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: msylla2
 * Person#:50285613
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.list.{ImmutableLinkedList,EmptyList,ListNode}
import cse250.adaptors.{LectureQueue,LectureStack}
import cse250.objects.TaxParcel


class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"

  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------
  // You may add member variables as you wish.
  var numbElements = 0
  val undoLastModif = new LectureStack[(ImmutableLinkedList[A], Int)]
  var undoProcessBatch = 0

  /** Gets element at position idx within the list. */
  override def apply(idx: Int): A = {
    require(idx >= 0 && idx < numbElements)
    _storageList(idx)
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = numbElements

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = new Iterator[A] {
    var currentList: ImmutableLinkedList[A] = _storageList

    override def hasNext: Boolean = !currentList.isEmpty

    override def next(): A ={
      require(hasNext)
      val ret = currentList.head
      currentList = currentList.tail
      ret
    }
  }

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit = {
    if(undoProcessBatch == 0) undoLastModif.push(_storageList, numbElements)

    if(numbElements == 0) _storageList = _storageList.inserted(numbElements, elem)
    else {
      var curIndex = 0
      while (curIndex < numbElements){
        if(_comp.lteq(elem, _storageList(curIndex))){
          _storageList = _storageList.inserted(curIndex, elem)
          curIndex = numbElements
        }
        curIndex += 1
      }
      if(curIndex == numbElements) _storageList = _storageList.inserted(numbElements, elem)
    }
    numbElements += 1
  }

  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return the number of copies removed.
   */
  def remove(elem: A): Int = {
    if(undoProcessBatch == 0) undoLastModif.push(_storageList, numbElements)

    val it = iterator
    var count = 0
    var index = 0
    while(it.hasNext){
      if(_comp.equiv(elem, it.next())){
        _storageList = _storageList.removed(index)
        count += 1
        index -= 1
      }
      index += 1
    }
    numbElements -= count
    count
  }

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = {
    undoLastModif.push(_storageList, numbElements)
    undoProcessBatch = 1

    while (!operations.isEmpty) {
      val element = operations.dequeue
      if (element._1 == "insert") insert(element._2)
      else if (element._1 == "remove") remove(element._2)
    }
    undoProcessBatch = 0
  }

  /** Undo the last modification, if any change has been made.
   *  If no change to undo exists, raise an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    require(!undoLastModif.isEmpty)
    val undo = undoLastModif.pop
    _storageList = undo._1
    numbElements = undo._2
  }
}

object NewIntOrdering extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = {
    var out = 0
    if ((x % 2 == 0 && y % 2 == 0) || (x % 2 == 1 && y % 2 == 1)) {
      if(x > y) out = -1 else if (y > x ) out = 1
    }
    else{
      if(x % 2 == 0) out = -1 else if (y % 2 == 0) out = 1
    }

    out
  }
}

object TaxParcelStreetGroupingOrdering extends Ordering[TaxParcel] {
  def compare(x: TaxParcel, y: TaxParcel): Int = {
  var out = 0
    val _x: String = x.parcelInfo("STREET")
    val _y: String = y.parcelInfo("STREET")
    if(_x < _y) out = -1 else if(_y < _x) out = 1
    out
  }
}