package edu.nyu.scc

/**
 * Created by asher on 3/28/14.
 */
import scala.actors._
import java.util.ArrayList
import com.typesafe.config.ConfigFactory
import java.io.PrintWriter
import java.io.File
import java.io.FileWriter
import scala.io.Source


class HelloWorld {
  
}

object HelloWorld {
  def main(args: Array[String]) {
    
    //val g = CreateGraph.Watts(100, 3, 0.2)
   // println(g)
    
    //testErdos(100,0.2)
    
    averageErdos(10, 0.1, 1)
    averageErdos(10, 0.2, 1)
    averageErdos(10, 0.3, 1)
    averageErdos(10, 0.4, 1)
    averageErdos(10, 0.5, 1)
    averageErdos(10, 0.6, 1)
    averageErdos(10, 0.7, 1)
    averageErdos(10, 0.8, 1)
    averageErdos(10, 0.9, 1)
    averageErdos(10, 1, 1)
    
    averageWatts(10, 2, 0.2, 1)
    averageWatts(10, 3, 0.2, 1)
    averageWatts(10, 4, 0.2, 1)
    averageWatts(10, 5, 0.2, 1)
    averageWatts(10, 1, 0.2, 1)
    averageWatts(10, 3, 0.2, 1)
    averageWatts(10, 5, 0.3, 1)
    averageWatts(10, 5, 0.4, 1)
    averageWatts(10, 5, 0.5, 1)
    averageWatts(10, 1, 0.3, 1)
    averageWatts(10, 1, 0.4, 1)
    averageWatts(10, 2, 0.5, 1)
    averageWatts(10, 2, 0.3, 1)
    averageWatts(10, 4, 0.4, 1)
    averageWatts(10, 3, 0.5, 1)
    averageWatts(10, 2, 0.3, 1)
    
    averageWatts(10, 2, 0.1, 1)
    averageWatts(10, 3, 0.2, 1)
    averageWatts(10, 4, 0.3, 1) 
    averageWatts(10, 5, 0.4, 1)
    averageWatts(10, 2, 0.5, 1)
    averageWatts(10, 3, 0.6, 1)
    //averageErdos(1000, 0.2, 10)
    
    //averageWatts(100,8, 0.4, 10)
    realGraph("gnutella_graph.txt", 6301)
    //averageWatts(1000,20, 0.4, 1)
    //averageErdos(10000, 0.1, 1)
   
    //Second implementation
   // var graph: Graph = CreateGraph.erdosRenyi(5000, 0.3)
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
   // println("standard scc:")
    //val c = time(Graph.stronglyConnectedComponents(graph))
    //println(c)
    
   // println("divide and conquer scc:")
    //val suc = graph.successors(1)
    //println(suc) 
    
    //val pred = graph.predecessors(2)
   // println(pred) 
  //  println("DCSC")
    //val b = time(DCSC.concurrentSCC(graph))
    //println("ARE WE EQUAL? " + b.toSet.equals(c.toSet))
    
    
    //println("final in main: " + b)
    /*
    println("total time building graphs: " + graph.getTimeCounter)
    
    //statistics:
    println("DCSC to standard: " + (timeList.get(1)).toDouble/timeList.get(0))
    println((timeList.get(1)-graph.getTimeCounter).toDouble/timeList.get(0))
    
    println("WDCSC")
    graph.resetTimeCounter()
    val bb = time(WDCSC.concurrentSCC(graph))
   // println("ARE WE EQUAL? " + bb.toSet.equals(b.toSet))
    //println("final in main: " + b)
    println("total time building graphs: " + graph.getTimeCounter)
    println("WDCSC to standard: " + (timeList.get(2)).toDouble/timeList.get(0))
    println("WDCSC to DCSC: " + (timeList.get(2)).toDouble/timeList.get(1))
    * 
    */
   
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
  
  def resetTimeList() = {
    timeList = new ArrayList[Long]()
  }
  
  def testErdos(v: Int, p: Double): ArrayList[Long] = {
    if (p > 1.0 || p < 0.0) throw new IllegalArgumentException("bad p")
    if (v <= 0) throw new IllegalArgumentException("bad v")
    resetTimeList()
    val graph: Graph = CreateGraph.erdosRenyi(v, p)
    println("$#$#$#$#$#$#$#$#$#$")
    println(graph.getAdj)
    println(graph.getRevAdj)
    println("$#$#$#$#$#$#$#$#$#$")
    println("made graph")
    val standard = time(Graph.stronglyConnectedComponents(graph))
    println("done standard")
    val dcsc = time(DCSC.concurrentSCC(graph))
    println("done dcsc")
    if (!standard.toSet.equals(dcsc.toSet)) throw new Exception("Resulting components not equal: " + standard.toSet.size + " "+ (dcsc.toSet.size))
    val wdcsc = time(WDCSC.concurrentSCC(graph))
    println("done wdcsc")
    
    println(standard)
    println(wdcsc)
    if (!standard.toSet.equals(wdcsc.toSet)) throw new Exception("erdos " + p + " Resulting components not equal: " + standard.toSet.size + " "+ (wdcsc.toSet.size))
    println("!!!!! Results: ")
    
    return (timeList)
  }
  
  def averageErdos(v: Int, p: Double, rounds: Int) = {
    
    val standard = new ArrayList[Long]()
    val dcsc = new ArrayList[Long]()
    val wdcsc = new ArrayList[Long]()
    
    var sum1:Long = 0
    var sum2: Long = 0
    var sum3: Long = 0
    
    for (a <- 1 to rounds) {
      val list = testErdos(v,p)
      sum1 = sum1 + list.get(0)
      sum2 = sum2 + list.get(1)
      sum3 = sum3 + list.get(2)
      standard.add(list.get(0))
      dcsc.add(list.get(1))
      wdcsc.add(list.get(2))
    }
    
    val writer = new PrintWriter(new FileWriter("erdos_"+v.toString()+"_"+ p.toString() +".txt", true ))

    writer.write(sum1/rounds + " ")
    writer.write(sum2/rounds  + " ")
    writer.write(sum3/rounds  + "\n")
    
    writer.close()
  }
  
  
  //
  //
  //
   def testWatts(v: Int, k:Int, p: Double): ArrayList[Long] = {
    if (p > 1.0 || p < 0.0) throw new IllegalArgumentException("bad p")
    if (v <= 0) throw new IllegalArgumentException("bad v")
    resetTimeList()
    val graph: Graph = CreateGraph.Watts(v, k, p)
    println("made graph")
    val standard = time(Graph.stronglyConnectedComponents(graph))
    println("done standard")
    val dcsc = time(DCSC.concurrentSCC(graph))
    println("done dcsc")
    if (!standard.toSet.equals(dcsc.toSet)) throw new Exception("Resulting components not equal: " + standard.toSet.size + " "+ (dcsc.toSet.size))
    val wdcsc = time(WDCSC.concurrentSCC(graph))
    println("done wdcsc")
    
    
    if (!standard.toSet.equals(wdcsc.toSet)) throw new Exception("Watts " + k + " " + p + " Resulting components not equal: " + standard.toSet.size + " "+ (wdcsc.toSet.size))
    println("!!!!! Results: ")
    
    return (timeList)
  }
  
  def averageWatts(v: Int, k: Int, p: Double, rounds: Int) = {
    
    val standard = new ArrayList[Long]()
    val dcsc = new ArrayList[Long]()
    val wdcsc = new ArrayList[Long]()
    
    var sum1:Long = 0
    var sum2: Long = 0
    var sum3: Long = 0
    
    for (a <- 1 to rounds) {
      val list = testWatts(v,k,p)
      sum1 = sum1 + list.get(0)
      sum2 = sum2 + list.get(1)
      sum3 = sum3 + list.get(2)
      standard.add(list.get(0))
      dcsc.add(list.get(1))
      wdcsc.add(list.get(2))
    }
    
    val writer = new PrintWriter(new FileWriter("watts_"+v.toString()+"_"+ p.toString()+"_"+k.toString() +".txt", true ))

    writer.write(sum1/rounds + " ")
    writer.write(sum2/rounds  + " ")
    writer.write(sum3/rounds  + "\n")
    
    writer.close()
  }
  
  def realGraph(filename: String, v: Int) = {
    val graph: Graph = new Graph(v)
    println(graph.vertices)
    for(line<-Source.fromFile(filename).getLines){
    var array = line.split("\t")
        val source = array(0).toInt+1
        val target = array(1).toInt+1
        println(source + " " + target)
        graph.update(source->target)       
    }
    println("made graph")
    resetTimeList()
    val standard = time(Graph.stronglyConnectedComponents(graph))
    println("done standard")
    //val dcsc = time(DCSC.concurrentSCC(graph))
    println("done dcsc")
    //if (!standard.toSet.equals(dcsc.toSet)) throw new Exception("Resulting components not equal: " + standard.toSet.diff(dcsc.toSet))
    val wdcsc = time(WDCSC.concurrentSCC(graph))
    println("done wdcsc")
    
    if (!standard.toSet.equals(wdcsc.toSet)) throw new Exception("Resulting components not equal: " + standard.toSet.size + " "+ (wdcsc.toSet.size))
    println("!!!!! Results: ")
    
    val writer = new PrintWriter(new FileWriter("real_"+filename+".txt", true ))

    writer.write(timeList.get(0) + " ")
    writer.write(timeList.get(1)  + " ")
    //writer.write(timeList.get(2)  + "\n")
    
    writer.close()
    
  }

}
