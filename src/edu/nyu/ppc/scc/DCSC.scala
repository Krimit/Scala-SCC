package edu.nyu.ppc.scc

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.collection.immutable.Queue
import akka.event.LoggingAdapter
import scala.collection.mutable


 
object DCSC {
  
  
  
  sealed trait SCCMessage
  case object Calculate extends SCCMessage
  case class Calculate(g: Graph) extends SCCMessage
  case class Work(graph: Graph, v: Int) extends SCCMessage
  case class Instruct(graph: Graph, listener: ActorRef) extends SCCMessage
  case class Result(component: Set[Int]) extends SCCMessage
  case class Descendant(list: List[Int])
  case class Predecessor(list: List[Int])
  case class Stop
  case class Done
  case class ReportResult
 
  class Worker() extends Actor {
    var numResponses = 0
    //val worker: ActorRef = context.actorOf(Props[Worker], "worker1")
    
    def receive() = {
      case Instruct(graph, listen) =>
        if (graph.edges.isEmpty) { // output each vertex as component
          for (c <- graph.vertices) yield listen ! Result(Set(c))
          context.parent ! Done
        } else { //do work
          
          val v = graph.vertices.head
          
          val pred = graph.predecessors(v)
          val desc = graph.successors(v)
          
          val scc = (pred.intersect(desc))
          
          listen ! Result(scc)
          
          val name1 = "work1"// + self.path
          val name2 = "work2"// + self.path
          val name3 = "work3"// + self.path
          val worker1 = context.actorOf(Props[Worker])
          val worker2 = context.actorOf(Props[Worker])
          val worker3 = context.actorOf(Props[Worker])

          worker1 ! Instruct(graph.subGraphOf(pred--scc), listen)
   
          worker2 ! Instruct(graph.subGraphOf(desc--scc), listen)
          
          worker3 ! Instruct(graph.subGraphWithout(pred.union(desc)), listen)
        }
      
      case Done =>
        sender ! PoisonPill
        numResponses += 1
        if (numResponses == 3) {
          context.parent ! Done
        }
     
    }
  }
  
 
 
  class Master(graph: Graph, listener: ActorRef) extends Actor {
    
    //val collector = context.actorOf(Props[Listener], "listener")  
     // def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
    
    val worker: ActorRef = context.actorOf(Props[Worker], "worker1")
    
    def receive = {
      case Calculate =>
        worker ! Instruct(graph, listener) //start calculating
      case Done =>
        sender ! PoisonPill
        listener ! ReportResult
        println("all done now")
     
    }
  }
 
  class Listener(val resultingComponents: mutable.Queue[Set[Int]]) extends Actor with ActorLogging {
   // LoggingAdapter log = Logging.getLogger(getContext().system(), this);
    
    def receive = {
      case Result(component) =>
        resultingComponents.enqueue(component)
        //println(resultingComponents)
        log.debug("Added a component {}", component.toString)
        
      case ReportResult =>
        println(resultingComponents)
        //sender ! PoisonPill
        //context.stop(self)
        
    }
  }
 
  def props(queue: mutable.Queue[Set[Int]]) = Props(classOf[Listener], queue)
 
  def concurrentSCC(graph: Graph) {
   
    // Create an Akka system
    val system = ActorSystem("SCCSystem")
 
    // create the result listener, which will collect the results
    val components = mutable.Queue.empty[Set[Int]]
    val listener = system.actorOf(props(components), name = "listener")
    
 
    // create the master
    val master = system.actorOf(Props(new Master(
      graph, listener)),
      name = "master")
 
    // start the calculation
    master ! Calculate
 
  }
}