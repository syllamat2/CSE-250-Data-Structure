/**
 * cse250.pa1.GroupByStore.scala
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
 * Person#: 50285613
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa1.objects

import cse250.objects.{DNode, TaxParcel}

import scala.collection.mutable.ArrayBuffer

class GroupByStore extends Seq[TaxParcel] {
  // Member/instance variables are defined public for ease of access for testing.
  var _groupings: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
  var _groupingAttribute: String = "STREET"
  var _numStored = 0


  def apply(i: Int): TaxParcel = {
    val iter = this.iterator
    for (_ <- 0 until i) iter.next()
    iter.next()
  }

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxParcel: TaxParcel): Unit = {

     val newNode = new DNode[TaxParcel](taxParcel, null, null)
     val stringAttribute = taxParcel.parcelInfo(_groupingAttribute)

    if (_groupings.isEmpty) {
      _groupings.append(newNode)

    }
    else {
      var found = false
      var i: Int = 0
      while (i < _groupings.length) {
        val oldNode = _groupings(i)._value
        val string = oldNode.parcelInfo(_groupingAttribute)


        if (stringAttribute == string) {
          newNode._next = _groupings(i)
          _groupings(i)._prev = newNode
          _groupings(i) = newNode
          found = true
          i = _groupings.length - 1
        } else{
          if(stringAttribute < string) {
            _groupings.insert(i, newNode)
            i += _groupings.length - 1
            found = true
          }
        }
        i += 1
      } // store at new index (last)

      if (!found) {
        _groupings.insert(_groupings.length, newNode)
      }
    }

    _numStored += 1
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {
   if(attribute != _groupingAttribute) {
     _groupingAttribute = attribute
     val temp: ArrayBuffer[TaxParcel] = new ArrayBuffer[TaxParcel]
     //val entry = new TaxParcel

     var index = 0
     val it = iterator
     while (it.hasNext) {
       temp.insert(index, it.next())
       index += 1
     }

     _groupings.clear()
     _numStored = 0

     for (i <- 0 until temp.length) {
       insert(temp(i))
     }
   }
  }


  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxParcel] = new Iterator[TaxParcel] {
    var count = 0
    for(i <- 0 until _groupings.length){
      var current = _groupings(i)
      while(current!=null){
        count += 1
        current = current._next
      }
    }



    var arrayIndex = 0
      var currInNode: DNode[TaxParcel] = null
    if(count > 0) currInNode = _groupings(arrayIndex)


    override def hasNext: Boolean = count > 0

    override def next(): TaxParcel = {
      require(hasNext)
        val ret = currInNode._value
        if (currInNode._next == null && count > 1) {
          arrayIndex += 1
          currInNode = _groupings(arrayIndex)
        } else {
          currInNode = currInNode._next
        }
        count -= 1
      ret
    }
  }

  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxParcel] = new Iterator[TaxParcel] {

    var index = 0
    var i = 0
    var current: DNode[TaxParcel] = null
    while(i < _groupings.length ){
      val node = _groupings(i)._value.parcelInfo(_groupingAttribute)
      if(node ==value) {
        current = _groupings(i)
        index = 1
        i = _groupings.length
      }
      i += 1
    }
    override def hasNext: Boolean = index == 1

    override def next(): TaxParcel = {
      require(hasNext)
      val ret = current._value
      if(current._next == null) index = 0 else current = current._next
      ret
    }
  }

  def length: Int = _numStored

  override def toString: String = this.iterator.mkString("GroupByStore(", "\n", ")")
}
