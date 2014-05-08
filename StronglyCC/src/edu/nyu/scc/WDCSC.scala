package edu.nyu.scc

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.collection.immutable.Queue
import akka.event.LoggingAdapter
import scala.collection.mutable
import java.util.concurrent.CountDownLatch
import scala.concurrent.Promise
import scala.concurrent.duration.Duration
import scala.concurrent._
import ExecutionContext.Implicits.global
import akka.pattern.{ ask, pipe }
import com.typesafe.config.ConfigFactory


 
object WDCSC {
  
  val config = ConfigFactory.load()
  
  sealed trait SCCMessage
  case class Calculate(g: Graph) extends SCCMessage
  case class Instruct(graph: Graph, listener: ActorRef) extends SCCMessage
  case class Result(component: Set[Int]) extends SCCMessage
  case class ReportResult
  case class FinalResult(components: mutable.Queue[Set[Int]])
  case class Descendant(list: Set[Int])
  case class Predecessor(list: Set[Int])
  case class Search(graph: Graph, v: Int)
  case class Relatives(set: Set[Int])
  
  class DescWorker() extends Actor {
     var g: Graph = null
    var s = 0
    def receive = {
      case Search(graph: Graph, v: Int) =>
        g = graph
        s = v
        Future(graph.successors(v)) map Relatives pipeTo self
      
      case Relatives(group) =>
        //println()
        //println("DESC%%%%" + g.getAdj  + " // " + g.getRevAdj + " source: " + s + " pred: " + group)
        //println()
         context.parent ! Descendant(group)
         context.stop(self)
    }
  }
  
  class PredWorker() extends Actor {
    var g: Graph = null
    var s = 0
    def receive = {
      case Search(graph: Graph, v: Int) =>
        g = graph
        s = v
        Future(graph.predecessors(v)) map Relatives pipeTo self
      
      case Relatives(group) =>
        //println()
        //println("PRED%%%%" + g.getAdj + " // " + g.getRevAdj + " source: " + s + " pred: " + group)
        //println()
         context.parent ! Predecessor(group)
         context.stop(self)
    }
  }
  
  class Worker() extends Actor {
    
    def receive = start(0)
    
    def foreman(g: Graph, v: Int, predWorker: ActorRef, descWorker: ActorRef, listener: ActorRef, done: Boolean): Receive = {
      case Descendant(group) =>
        if (!done) {
          predWorker ! PoisonPill
          context.become(foreman(g, v, predWorker, descWorker, listener, true))
          val descGraph = g.subGraphOf(group)
          //doing work, but can't proceed until done anyway. is that best?
          val pred = descGraph.predecessors(v)
          //println("*** graph is: " + descGraph)
          //println("*****group is: " + group + " pred is: " + pred)
          //println("union with " + v + " , have new scc: " + pred.intersect(group))
          val scc = (pred.intersect(group))
          listener ! Result(scc)
        
          val worker1 = context.actorOf(Props[Worker])
          val worker2 = context.actorOf(Props[Worker])
        
          context.watch(worker1)
          context.watch(worker2)
             
          val x1 = g.subGraphOf(group--scc)
          val x2 = g.subGraphWithout(group)
          //println("***** desc x1: " + x1)
          //println("***** desc x2: " + x2)
          worker1 ! Instruct((x1), listener) 
          worker2 ! Instruct((x2), listener)  
        
          context.become(start(0))
        }
        
        
      
        
      case Predecessor(group) =>
        if (!done) {
           descWorker ! PoisonPill
           context.become(foreman(g, v, predWorker, descWorker, listener, true))
        val predGraph = g.subGraphOf(group)
        //doing work, but can't proceed until done anyway. is that best?
        val desc = predGraph.successors(v)
        //println("*** graph is: " + g + " *****pred group is: " + group + " desc is: " + desc + " union with " + v + " , have new scc: " + desc.intersect(group))
        //println()
        //println()
        val scc = (desc.intersect(group))
        listener ! Result(scc)
        
        val worker1 = context.actorOf(Props[Worker])
        val worker2 = context.actorOf(Props[Worker])
        
        context.watch(worker1)
        context.watch(worker2)
             
        val x1 = predGraph.subGraphOf(group--scc)
        val x2 = g.subGraphWithout(group)
        //println("*****pred x1: " + x1)
        //println("*****pred x2: " + x2)
        worker1 ! Instruct((x1), listener) 
        worker2 ! Instruct((x2), listener) 
        context.become(start(0))
        }
       
    }
    
    def start(count: Int): Receive = {
      case Instruct(graph, listen) =>
        if (graph.vertices.isEmpty) {
          context.stop(self)
        } else if (graph.edges.isEmpty) { // output each vertex as component

          for (c <- graph.vertices) yield listen ! Result(Set(c))
          context.stop(self)
        } else { //do work   

          val v = graph.getRandomVertex()  
          
          val descWorker = context.actorOf(Props[DescWorker]) 
          val predWorker = context.actorOf(Props[PredWorker])
          
          descWorker ! Search(graph: Graph, v: Int)
          predWorker ! Search(graph: Graph, v: Int)
          context.become(foreman(graph, v, predWorker, descWorker, listen, false))
        }
        
     case Terminated(child) =>  
        if (count >= 1) {
          context.stop(self)
        } else {
          context.become(start(count+1))
        }
      
    }
  }

  class Master(graph: Graph, listener: ActorRef) extends Actor {
    
    val worker: ActorRef = context.actorOf(Props[Worker], "worker1")
    context.watch(worker)
    
    def receive = {
      case Calculate =>
        worker ! Instruct(graph, listener) //start calculating
      case Terminated(worker) =>
        listener ! ReportResult
        context.stop(self)
   
    }
  }
 
  class Listener(val resultingComponents: mutable.Queue[Set[Int]], p: Promise[mutable.Queue[Set[Int]]]) extends Actor with ActorLogging {
   // LoggingAdapter log = Logging.getLogger(getContext().system(), this);
    
    var count = 0 
    def receive = {
      case Result(component) =>
        count += component.size
        println("seen vertices scc: " + count)
        resultingComponents.enqueue(component)
        log.debug("Added a component {}", component.toString)
        
      case ReportResult =>
        sender ! FinalResult(resultingComponents)
        p.success(resultingComponents)
        context.stop(self)      
    }
  }
 
  def props(queue: mutable.Queue[Set[Int]], p: Promise[mutable.Queue[Set[Int]]]) = Props(classOf[Listener], queue, p)
 
  def concurrentSCC(graph: Graph): mutable.Queue[Set[Int]] =  {
    
    graph.resetTimeCounter
    // Create an Akka system
    val system = ActorSystem("SCCSystem")
 
    // create the result listener, which will collect the results
    val p = Promise[mutable.Queue[Set[Int]]]
    val components = mutable.Queue.empty[Set[Int]]
    val listener = system.actorOf(props(components, p), name = "listener")
 
 
    // create the master
    val master = system.actorOf(Props(new Master(
      graph, listener)),
      name = "master")
 
    // start the calculation
    master ! Calculate
    
    
    /*
    p.future onSuccess {
      case output =>
        system.shutdown
        println("got promise!")
        //println(output)
        //output
    }
    * 
    */
    
    val output = Await.result(p.future, Duration.Inf)
    
    println("all done now outside as well: " + output.size)
    system.shutdown
    output
  }
}