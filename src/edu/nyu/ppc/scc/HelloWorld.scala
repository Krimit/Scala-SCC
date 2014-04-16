package edu.nyu.ppc.scc

/**
 * Created by asher on 3/28/14.
 */
import scala.actors._
object HelloWorld {
  def main(args: Array[String]) {
    
    println("Hello, dude! you rule the world!")
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
    var graph: Graph = CreateGraph.erdosRenyi(5, 0.4)
    println(graph.edges)
    
    var c = Graph.stronglyConnectedComponents(graph)
    println(c)
   
  }

  object SillyActor extends Actor {
    def act(){
      for (i <- 1 to 5) {
        println("I'm acting doood! " + i.toString())
        Thread.sleep(1000)
      }
    }
  }

  def factorial(x: BigInt): BigInt =
    if (x == 0) 1 else x * factorial(x-1)


  def square(x: Double) = x * x

  def sumOfSquares(x: Double, y: Double) = square(x) + square(y)

}
