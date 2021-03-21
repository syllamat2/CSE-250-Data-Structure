/**
 * cse250.pa3.MapUtilities.scala
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
package cse250.pa3

import cse250.objects.{StreetGraph, TaxParcel}

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.xml.{NodeSeq, XML}

object MapUtilities {

  def loadIntersectionIDs(filename: String): mutable.Set[String] = {
    val x = scala.xml.XML.loadFile(filename)
    val mySet = mutable.Set[String]()
    for (node <- x \ "node") {
      mySet.add(node \@ "id")
    }
    mySet
  }

  def loadMapInfo(filename: String): mutable.Map[String, mutable.Set[String]] ={

    val xmlFile = scala.xml.XML.loadFile(filename)

    val map = scala.collection.mutable.Map[String, mutable.Set[String]]()

    for (way <- xmlFile \ "way"){
      for( tag <- way \ "tag") {
        val key = tag \@ "k"

        if (key == "tiger:name_base") {
          for(nd <- way \ "nd"){
            val ref = nd \@ "ref"
            val street = (tag \@ "v").toUpperCase
            if(map.contains(ref)){
              map(ref).add(street)
            }else{
              map(ref) = mutable.Set(street)
            }
          }
        }
      }

    }
    map
  }

  def buildIntersectionGraph(intersectionIDs: mutable.Set[String],
                             nodeToStreetMapping: mutable.Map[String, mutable.Set[String]]): StreetGraph = {

    val streetGraph = new StreetGraph
    var verticesList = collection.mutable.ArrayBuffer[String]()
    for(e <- intersectionIDs){
      val streets = nodeToStreetMapping.get(e)
      if(streets.isDefined){
         val list = streets.get

        while(list.nonEmpty){
          verticesList.append(list.head.toUpperCase)
          list.remove(list.head)
        }
        if(verticesList.length >1){
          var i = 0
          var j = 1
          while(j < verticesList.length){
            streetGraph.insertEdge(verticesList(i), verticesList(j))
            streetGraph.insertEdge(verticesList(j), verticesList(i))
            i += 1
            j += 1
          }
        }
        verticesList = verticesList.empty
      }


    }
    streetGraph
  }

  def computeFewestTurns(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Int = {
    val startStreet = start.parcelInfo("STREET")
    val endStreet = end.parcelInfo("STREET")
    if(startStreet == endStreet) 0
    else if (streetGraph.edges.contains(startStreet, endStreet)){
      1
    } else {
      val edges: mutable.Set[(String, String)] = streetGraph.edges
      if (edges.contains(startStreet, endStreet)) return 1
      var numstreet = -1
      val edgeList = edges.toBuffer
      var v = edgeList.indexWhere(e => e._1 == startStreet)
      val visited = mutable.ArrayBuffer[String]()
      var i = v + 1
      var count = 0
      var found = false
      while (i < edgeList.length && !found) {
        if (edgeList(v)._2 == edgeList(i)._1 && !visited.contains(edgeList(i)._1)) {
          visited.append(edgeList(i)._1)
          count += 1
          v = i
          if (edgeList(i)._2 == endStreet) {
            count += 1
            found = true
          }
        }
        i += 1
      }
      if (found) numstreet = count

      numstreet
    }
  }

  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Seq[String] = {
    var list: scala.collection.immutable.List[String] = List()

    val startStreet = start.parcelInfo("STREET").toUpperCase
    val endStreet = end.parcelInfo("STREET").toUpperCase

    if (startStreet == endStreet) {
      list = list.appended(startStreet)
      list
    }
    else {
      if (streetGraph.edges.contains(startStreet, endStreet)) {
        list = list.appended(startStreet)
        list = list.appended(endStreet)
      }
      list
    }
  }
}
