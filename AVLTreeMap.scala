/**
 * cse250.pa4.AVLTreeMap.scala
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
import collection.mutable.Stack

class AVLTreeMap[K, V]()(implicit ord: Ordering[K]) extends Map[K, V]{
  val _storageTree = new AVLTree[K, V]

  override def addOne(elem: (K, V)): Unit = _storageTree.insert(elem)

  override def removeOne(key: K): Boolean = _storageTree.remove(key)

  override def get(key: K): Option[V] = _storageTree.find(key) match {
    case n: _storageTree.AVLNode[(K, V)] if n != null => Some(n._value._2)
    case null                                         => None
  }

  override def iterator: Iterator[(K, V)] = _storageTree.iterator
}

class AVLTree[K, V]()(implicit ord: Ordering[K]) {

  class AVLNode[A](var _value: A, var _left: AVLNode[A], var _right: AVLNode[A], var _parent: AVLNode[A],
                   var _leftH: Boolean, var _rightH: Boolean)

  var _avlRoot: AVLNode[(K, V)] = null

  def find(elem: K): AVLNode[(K, V)] = {
    var current = _avlRoot
    var found = false
    while (!found && current != null) {
      val currentKey = current._value._1
      if (ord.lt(elem, currentKey)) current = current._left
      else if (ord.lt(currentKey, elem)) current = current._right
      else found = true
    }
    current
  }

  def rotateLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val parentA = nodeA._parent
    val nodeB = nodeA._right
    if(parentA == null) {
      nodeB._parent = null
      _avlRoot = nodeB
    } else {
      if(parentA._left == nodeA){
        parentA._left = nodeB
      } else parentA._right = nodeB
    }
    nodeA._right = nodeB._left
    if(nodeA._right==null && nodeA._left!=null) nodeA._leftH = true
    if(nodeA._right != null) nodeA._right._parent = nodeA
    nodeA._parent = nodeB
    nodeA._rightH = false
    nodeB._left = nodeA
    nodeB._parent = parentA
    nodeB._rightH = false
    nodeB._leftH = false
    nodeB
  }

  def rotateRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val parentA = nodeA._parent
    val nodeB = nodeA._left
    if(parentA == null){
      nodeB._parent = null
      _avlRoot = nodeB
    } else {
      if(parentA._left == nodeA){
        parentA._left = nodeB
      } else parentA._right = nodeB
    }
    nodeA._left = nodeB._right
    if(nodeA._left==null && nodeA._right!=null) nodeA._rightH = true
    if(nodeA._left!= null) nodeA._left._parent = nodeA
    nodeA._parent = nodeB
    nodeA._leftH = false
    nodeB._right = nodeA
    nodeB._parent = parentA
    nodeB._leftH = false
    nodeB._rightH = false
    nodeB
  }

  def rotateLeftRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val nodeB = nodeA._left
    nodeA._left = rotateLeft(nodeB)
    val nodeC = rotateRight(nodeA)
    nodeC
  }

  def rotateRightLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val nodeB = nodeA._right
    nodeA._right = rotateRight(nodeB)
    val nodeC = rotateLeft(nodeA)
    nodeC
  }

  def insert(elem: (K, V)): AVLNode[(K, V)] = {
    var newNode = new AVLNode[(K, V)](elem, null, null, null, false,false)
    if(_avlRoot == null) _avlRoot = newNode
    else{
      var boolean = false
      var cur = _avlRoot
      while(!boolean){
        val curKey = cur._value._1
        // insert on the right
        if(ord.lt(curKey, elem._1)){
          if(cur._right ==null) {
            cur._right = newNode
            newNode._parent = cur
            if (cur._leftH) cur._leftH = false else cur._rightH = true
            if (cur!= _avlRoot && cur._parent._right == cur && cur._parent._rightH){
              rotateLeft(cur._parent)
            }
            else {
              if (cur!= _avlRoot && cur._parent._left == cur && cur._parent._leftH) {
                rotateLeftRight(cur._parent)
                boolean = true
              }
            }
          }else cur = cur._right

          // insert on the left
        }else if (ord.gt(curKey, elem._1)){
          if(cur._left == null){
            cur._left = newNode
            newNode._parent = cur
            if(cur._rightH) cur._rightH = false else cur._leftH = true
            if(cur._parent._left == cur && cur._parent._leftH) rotateRight(cur._parent)
            else if(cur._parent._right == cur && cur._parent._rightH) {
              rotateRightLeft(cur._parent)
              boolean = true
            }
          }else cur = cur._left
        }else {
          cur._value = newNode._value
          boolean = true
        }
      }
    }
    newNode
  }

  def remove(key: K): Boolean = {
    var node = find(key)
    if(node == _avlRoot){
      if(node._right !=null && node._left!=null) { // root has two children
        var curr = node._right
        while (curr._right != null) curr = curr._right
        val childValue = curr._value
        remove(curr._value._1)
        curr._value = childValue
        curr._parent = null
        curr._left = _avlRoot._left
        curr._right = _avlRoot._right
        curr._leftH = _avlRoot._leftH
        curr._rightH = _avlRoot._rightH
        if(_avlRoot._left!=null) _avlRoot._left._parent = curr
        _avlRoot = curr
      }else if (node._left!=null){  // only has one child
        val chileNodeV = node._left._value
        remove(_avlRoot._left._value._1)
        _avlRoot._value = chileNodeV
      }else if(node._right!=null){
        val chileNodeVa = node._right._value
        remove(_avlRoot._right._value._1)
        _avlRoot._value = chileNodeVa
      }else{ // root is only node
        _avlRoot = null
      }
      return true
    } else if (node != null) {
          val parentNode = node._parent
          //if(node == _avlRoot) _avlRoot = null
          //node is a leaf
          if (node._left == null && node._right == null && node._parent != null) {
            if (parentNode._left == node) {
              parentNode._left = null
              parentNode._leftH = false
              if (parentNode._rightH) {
                if (parentNode._right._leftH) rotateRightLeft(parentNode) else rotateLeft(parentNode)
              }else parentNode._rightH = true
              //parentNode.right == node
            } else if (parentNode._right == node) {
              parentNode._right = null
              parentNode._rightH = false
              if (parentNode._leftH) {
                if (parentNode._left._leftH) rotateRight(parentNode) else rotateLeftRight(parentNode)
              }else parentNode._leftH = true
            }
            return true
            // if node is not a leaf
          } else if (node._leftH || node._rightH) {
            // if node has one child
            if (node._leftH) {
              val LchildValue = node._left._value
              remove(node._left._value._1)
              node._value = LchildValue
            } else if (node._rightH) {
              val RchildValue = node._right._value
              remove(node._right._value._1)
              node._value = RchildValue
            }
            return true
            //node has two children
          } else {
            // (node._left != null && node._right != null)
            var curr = node._right
            while (curr._right != null) curr = curr._right
            val RchildValue = curr._value
            remove(node._right._value._1)
            curr._value = RchildValue
            curr._parent = node._parent
            curr._left = node._left
            curr._right = node._right
            curr._leftH = node._leftH
            curr._rightH = node._rightH
            if(node._left!=null) node._left._parent = curr
            node = curr
            return true
          }
    }
    false
  }

  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    val _parentStack = {
      val stack = new Stack[AVLNode[(K, V)]]
      var currentNode = _avlRoot
      while (currentNode != null) {
        stack.push(currentNode)
        currentNode = currentNode._left
      }
      stack
    }

    override def hasNext: Boolean = _parentStack.nonEmpty

    override def next(): (K, V) = {
      val originalTop = _parentStack.top
      if (originalTop._right != null) {
        var currentNode = originalTop._right
        while (currentNode != null) {
          _parentStack.push(currentNode)
          currentNode = currentNode._left
        }
      }
      else {
        var recentTop = _parentStack.pop
        while (_parentStack.nonEmpty && recentTop != _parentStack.top._left) {
          recentTop = _parentStack.pop
        }
      }
      originalTop._value
    }
  }
}
