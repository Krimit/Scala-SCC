package edu.nyu.ppc.scc

/**
 * Created by asher on 3/28/14.
 */
import scala.actors._
import org.jgrapht.DirectedGraph
import org.jgrapht.graph.DefaultEdge
import java.util.ArrayList
import org.jgrapht.UndirectedGraph
import org.jgrapht._;
import org.jgrapht.graph._;

class HelloWorld {
  
}

object HelloWorld {
  def main(args: Array[String]) {
    
    val s1 = Set[Int](1,2,3)
    val s2 = Set[Int](1,2,3)
    val s3 = s1
    println("equal sets: " + s1.equals(s2) + " " + s1.equals(s3))
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
    var graph: Graph = CreateGraph.erdosRenyi(100, 0.3)
    //println(graph)
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
    
    // val g: UndirectedGraph[Integer, DefaultEdge] =
      //      new SimpleGraph[Integer, DefaultEdge]();
    println("standard scc:")
    val c = time(Graph.stronglyConnectedComponents(graph))
    //println(c)
    
    println("divide and conquer scc:")
    //val suc = graph.successors(1)
    //println(suc) 
    
    //val pred = graph.predecessors(2)
   // println(pred) 
    println("DCSC")
    val b = time(DCSC.concurrentSCC(graph))
    println("ARE WE EQUAL? " + b.toSet.equals(c.toSet))
    
    
    //println("final in main: " + b)
    println("total time building graphs: " + graph.getTimeCounter)
    
    //statistics:
    println("DCSC to standard: " + (timeList.get(1)).toDouble/timeList.get(0))
    println((timeList.get(1)-graph.getTimeCounter).toDouble/timeList.get(0))
    
    println("WDCSC")
    graph.resetTimeCounter()
    val bb = time(WDCSC.concurrentSCC(graph))
    println("ARE WE EQUAL? " + bb.toSet.equals(b.toSet))
    //println("final in main: " + b)
    println("total time building graphs: " + graph.getTimeCounter)
    println("WDCSC to standard: " + (timeList.get(2)).toDouble/timeList.get(0))
    println("WDCSC to DCSC: " + (timeList.get(2)).toDouble/timeList.get(1))
   
  }
  var timeList: ArrayList[Long] = new ArrayList[Long]()
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    timeList.add(t1 - t0)
    result
  }

}
