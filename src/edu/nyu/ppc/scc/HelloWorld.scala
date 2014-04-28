package edu.nyu.ppc.scc

/**
 * Created by asher on 3/28/14.
 */
import scala.actors._
import org.jgrapht.DirectedGraph
import org.jgrapht.graph.DefaultEdge

class HelloWorld {
  
}

object HelloWorld {
  def main(args: Array[String]) {
    
    /*
    println("Hello, dude! you rule the world!")
    val m = Map("normal" -> Map("home" -> Map("wins" -> 0, "scores" -> 0),
                                   "away" -> Map("wins" -> 0, "scores" -> 0)))
                                   println(m)
    val map:Map[String,Map[String,Int]] = 
      Map(
        "mykey" -> Map("myval" -> 3),
        "myotherkey" -> Map("otherval" -> 4)
        )

        val k = map + ("nextkey" -> Map("nextval" -> 5))
    println(k)
    */
    /*
    var capital = Map("US" -> "Washington DC", "France" -> "Paris")
    capital += ("Japan" -> "Tokyo")
    println(capital)

   // println(factorial(5))
    println(square(3+1))
    println(sumOfSquares(3,4))
    SillyActor.start()
    * 
    */
    
    //First implementation. Something doesn't seem right.
    //val algo: SequentialTarjan = new SequentialTarjan
    //val graph: GraphAdjList = Vector(Set(1,2,3), Set(4,5,6)) 
    //val g: List[Map[Int, Set[Int]]] = List(Map(1 -> Set(2,3)))
    //var results: Vector[SubGraphAdjList]  = algo.compute(graph)
    //println(results)
    
    //Second implementation
    var graph: Graph = CreateGraph.erdosRenyi(50, 0.01)
    println(graph)
    /*
    println("vertices: " + graph.vertices)
    println("before:")
    println(graph)
    //println("adj:")
   // for (i <- adj) {
     // println(i)
    //}
    //println(adj.toString())
    
    val s = graph.subGraphOf(Set(1,2,4))
    println("after of:")
    println(s)
    
    val ss = graph.subGraphWithout(Set(1,2,4))
    println("after without:")
    println(ss)
    */
    println("standard scc:")
    var c = Graph.stronglyConnectedComponents(graph)
    println(c)
    
    println("divide and conquer scc:")
    //val suc = graph.successors(1)
    //println(suc) 
    
    //val pred = graph.predecessors(2)
   // println(pred) 
    var conC = DCSC.concurrentSCC(graph)
    println(conC)
   
  }

}
