package edu.nyu.ppc.scc

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


 
object WDCSC {
  
  
  
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
    def receive = {
      case Search(graph: Graph, v: Int) =>
        Future(graph.successors(v)) map Relatives pipeTo self
      
      case Relatives(group) =>
         context.parent ! Descendant(group)
         context.stop(self)
    }
  }
  
  class PredWorker() extends Actor {
    def receive = {
      case Search(graph: Graph, v: Int) =>
        Future(graph.predecessors(v)) map Relatives pipeTo self
      
      case Relatives(group) =>
         context.parent ! Predecessor(group)
         context.stop(self)
    }
  }
  
  class Worker() extends Actor {
    
    def receive = start(0)
    
    def foreman(g: Graph, v: Int, predWorker: ActorRef, descWorker: ActorRef, listener: ActorRef): Receive = {
      case Descendant(group) =>
        predWorker ! PoisonPill
        val descGraph = g.subGraphOf(group)
        //doing work, but can't proceed until done anyway. is that best?
        val pred = descGraph.predecessors(v)
        val scc = pred & group
        listener ! Result(scc)
        
        val worker1 = context.actorOf(Props[Worker])
        val worker2 = context.actorOf(Props[Worker])
        
        context.watch(worker1)
        context.watch(worker2)
             
        worker1 ! Instruct(g.subGraphOf(group--scc), listener) 
        worker2 ! Instruct(g.subGraphWithout(group), listener)  
        
        //either watch for both to die, or kill self
        context.stop(self)
        
      
        
      case Predecessor(group) =>
        descWorker ! PoisonPill
        val predGraph = g.subGraphOf(group)
        //doing work, but can't proceed until done anyway. is that best?
        val desc = predGraph.successors(v)
        val scc = desc & group
        listener ! Result(scc)
        
        val worker1 = context.actorOf(Props[Worker])
        val worker2 = context.actorOf(Props[Worker])
             
        worker1 ! Instruct(g.subGraphOf(group--scc), listener) 
        worker2 ! Instruct(g.subGraphWithout(group), listener)  
        context.become(start(0))
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
          context.become(foreman(graph, v, predWorker, descWorker, listen))
        }
        
     case Terminated(child) =>  
        if (count == 1) {
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
    
    def receive = {
      case Result(component) =>
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
    
    
    
    p.future onSuccess {
      case output =>
        system.shutdown
        println("got promise!")
        //println(output)
        //output
    }
    
    val output = Await.result(p.future, Duration.Inf)
    
    println("all done now outside as well")
    output
  }
}