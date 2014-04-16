package edu.nyu.ppc.scc

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
            graph.update((u,v),1)   
        }
      }    
    }
    return graph
  }
}