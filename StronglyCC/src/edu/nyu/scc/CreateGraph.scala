package edu.nyu.scc

import scala.util.Random

object CreateGraph {
  
  /**
   * 
   */
  def erdosRenyi(n : Int, p : Double): Graph = {
    if (p < 0.0 || p > 1.0) throw new Exception("out of bounds")
    val rand = new Random()
    var graph: Graph = new Graph(n)
    
    for (u <- graph.vertices) {
      for (v <- graph.vertices) {
        if ((u != v) && (rand.nextDouble() <= p)) {
            graph.update((u -> v))   
        }
      }    
    }
    return graph
  }
  
  
  
  /**
   * 
   */
  def Watts(n : Int, k: Int, p : Double): Graph = {
    if (p < 0.0 || p > 1.0) throw new IllegalArgumentException("out of bounds")
    val rand = new Random()
    var graph: Graph = new Graph(n)
    
    //build lattice, k/2 neighbors on each side
    for (u <- graph.vertices) {
      for (v <- graph.vertices) {
        val d = Math.abs(u - v) % (n-(k/2))
        if (d > 0 && d <= k/2) {
          graph.update((u -> v)) 
        }
      }
    }
    var c = 0;  
    for (u <- graph.vertices) {
      println("count: " + c)
      for (v <- graph.outNeighbours(u)) {
        if (u < v && (rand.nextDouble() <= p)) {
          graph-=(u -> v)
          var r = -1
          do {
            r = rand.nextInt((graph.vertices.size+1)) 
           // println(r + " " + u)
          } while (r != u &&  !graph.edges.contains(u->r))
          graph.update((u->r))
         // println("updated")
        }
      } 
      c+=1;
    }
    return graph
  }
}