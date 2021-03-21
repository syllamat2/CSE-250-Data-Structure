/**
 * cse250.pa4.HashTableMap.scala
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
package cse250.pa4

import cse250.examples.types.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing

class HashTableMap[K, V](val alphaMax: Double = 0.6)(implicit hash: Hashing[K]) extends Map[K, V] {
  var _n = 0
  var _N = 10
  var _alpha: Double = 0.0
  var _bucketArray: Array[ListBuffer[(K, V)]] = Array.fill[ListBuffer[(K, V)]](_N)(ListBuffer[(K, V)]())

  def rehash(newSize: Int): Unit = {
    if (newSize > _N) {
      val oldBucketArray = _bucketArray
      _n = 0
      _N = newSize
      _alpha = 0.0
      _bucketArray = Array.fill(_N)(ListBuffer[(K, V)]())
      for (bucket <- oldBucketArray; elem <- bucket) this.addOne(elem)
    }
  }

  override def get(key: K): Option[V] = {
    val lookupIndex = hash.hash(key) % _N
    _bucketArray(lookupIndex).find(elem => elem._1 == key) match {
      case Some(elem) => Some(elem._2)
      case None       => None
    }
  }

  override def addOne(elem: (K, V)): Unit = {
    var indexIt = hash.hash(elem._1) % _N
    val value = get(elem._1)
    if(value.isDefined){
      val index = _bucketArray(indexIt).indexWhere(_._1 == elem._1)
      _bucketArray(indexIt).update(index, elem)
    }else{
      _n += 1
      _alpha = 1.0 * _n/_N
      if(_alpha > alphaMax){
        rehash(_N * 2)
        indexIt = hash.hash(elem._1) % _N
        _n += 1
      }
      _bucketArray(indexIt).prepend(elem)
    }
  }

  override def removeOne(key: K): Boolean = {
    val value = get(key)
    val boolean = value.isDefined
    if(boolean){
      val indexIt = hash.hash(key)
      val index = _bucketArray(indexIt).indexWhere(_._1 == key)
      _bucketArray(indexIt).remove(index)
      _n -= 1
    }
    boolean
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    var numbElems: Int = _n
    var bucketIndex: Int = _bucketArray.indexWhere(e=>e.nonEmpty, 0)
    var listIndex = 0

    override def hasNext: Boolean = numbElems > 0

    override def next(): (K, V) = {
      //require(hasNext)
      val list = _bucketArray(bucketIndex)
      val ret = list(listIndex)
      listIndex += 1
        if (listIndex == list.length && numbElems > 0) {
          listIndex = 0
          bucketIndex = _bucketArray.indexWhere(e=>e.nonEmpty, bucketIndex+1)
        }

      numbElems -=1
      ret
    }
  }
}
