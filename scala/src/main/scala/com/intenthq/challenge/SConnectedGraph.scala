package com.intenthq.challenge

import scala.collection.immutable.Queue

case class Node(value: Int, edges: List[Node] = List.empty)

object SConnectedGraph {

  // Find if two nodes in a directed graph are connected.
  // Based on http://www.codewars.com/kata/53897d3187c26d42ac00040d
  // For example:
  // a -+-> b -> c -> e
  //    |
  //    +-> d
  // run(a, a) == true
  // run(a, b) == true
  // run(a, c) == true
  // run(b, d) == false



  def run(source: Node, target: Node): Boolean = {
    var result = false
    var visited = Set.empty[Node]
    var q = Queue(source)
    while (q.nonEmpty && !result) {
      val (vertex, rest) = q.dequeue
      if (vertex.edges.contains(target) || vertex == target) {
        result = true
      }
      q = rest.enqueueAll(vertex.edges.filterNot(visited.contains))
      visited = (vertex.edges ++ visited).toSet
    }
    result
  }
}
