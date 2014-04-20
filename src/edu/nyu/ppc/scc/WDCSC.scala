package edu.nyu.ppc.scc

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.collection.immutable.Queue

 
object WDCSC {
 
  sealed trait SCCMessage
  case object Calculate extends SCCMessage
  case class Calculate(g: Graph) extends SCCMessage
  case class Work(graph: Graph, v: Int) extends SCCMessage
  case class Instruct(graph: Graph, listener: ActorRef) extends SCCMessage
  case class Result(component: Set[Int]) extends SCCMessage
  case class Descendant(list: List[Int])
  case class Predecessor(list: List[Int])
  case class Stop
 
  class Foreman(nrOfWorkers: Int) extends Actor {
    var g: Graph = null
    var v: Int = -1
    var listener: ActorRef = null
    var descWorker: ActorRef = null
    var predWorker: ActorRef = null
    
    def receive = {
      case Instruct(graph, listen) =>
        if (graph.isEmpty) { // output each vertex as component
          for (c <- graph.vertices) yield listener ! Result(Set(c))
        } else { //do work
          //QUESTION: Is this only way to make variables available to other
          //case classes?
          g = graph
          listener = listen
          val name1 = "desc" + self.path
          val name2 = "pred" + self.path
          descWorker = context.actorOf(Props[DescWorker], "name1") 
          predWorker = context.actorOf(Props[PredWorker], "name2")
          v = 0
          descWorker ! Work(graph, v)
          predWorker ! Work(graph, v)
        }
      case Descendant(list) =>
        predWorker ! Stop
        sender ! Calculate(g.subGraph(g.vertices.toSet--list.toSet))
        //doing work, but can't proceed until done anyway. is that best?
        val pred = g.subGraph(list).predecessors(v)
        val scc = pred.toSet & list.toSet
        sender ! Calculate(g.subGraph(list.toSet--scc))
        listener ! Result(scc)
      
      case Predecessor(list) =>  
        descWorker ! Stop
        sender ! Calculate(g.subGraph(g.vertices.toSet--list.toSet))
        //doing work, but can't proceed until done anyway. is that best?
        val desc = g.subGraph(list).successors(v)
        val scc = desc.toSet & list.toSet
        sender ! Calculate(g.subGraph(list.toSet--scc))
        listener ! Result(scc)
    }
  }
  
  class DescWorker(nrOfWorkers: Int) extends Actor {
    var stop = false
    
    def solve(graph: Graph, v: Int) = {
      sender !  Descendant(graph.successors(v))
    }
    
    def receive = {
      case Work(graph, v) =>
        solve(graph, v)
      case Stop =>
        stop = true
    }
  }
  
  class PredWorker(nrOfWorkers: Int) extends Actor {
    def solve(graph: Graph, v: Int) = {
      sender !  Predecessor(graph.predecessors(v))
    }
    
    def receive = {
      case Work(graph, v) =>
        solve(graph, v)
    }
  }
 
  class Master(nrOfWorkers: Int, graph: Graph, listener: ActorRef)
      extends Actor {
    
 
    val workerRouter = context.actorOf(
      Props[Foreman].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")
    
    val collector = context.actorOf(Props[Listener], "listener")  
    
    
    def receive = {
      case Calculate =>
        workerRouter ! Instruct(graph, collector) //start calculating
      case Calculate(g) =>
        workerRouter ! Instruct(g, collector) //continue work
     
 
    }
  }
 
  class Listener extends Actor with ActorLogging {
    val resultingComponents = Queue.empty[Set[Int]]
    def receive = {
      case Result(component) =>
        resultingComponents.enqueue(component)
        log.debug("Added a component {}", component.toString)
    }
  }
 
 
  def concurrentSCC(nrOfWorkers: Int, graph: Graph) {
    val nrOfElements: Int = 1000 
    val nrOfMessages: Int = 1000
    // Create an Akka system
    val system = ActorSystem("SCCSystem")
 
    // create the result listener, which will collect the results
    val listener = system.actorOf(Props[Listener], name = "listener")
 
    // create the master
    val master = system.actorOf(Props(new Master(
      nrOfWorkers, graph, listener)),
      name = "master")
 
    // start the calculation
    master ! Calculate
 
  }

}