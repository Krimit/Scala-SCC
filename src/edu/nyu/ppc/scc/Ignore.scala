package edu.nyu.ppc.scc

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._

 /*
object Ignore {
 
  sealed trait SCCMessage
  case object Calculate extends SCCMessage
  case class Work(f: (Graph, Int) => List[Int] , graph: Graph, v: Int) extends SCCMessage
  case class Instruct(graph: Graph, v: Int) extends SCCMessage
  case class Result(value: List[Int]) extends SCCMessage
  case class PiApproximation(pi: Double, duration: Duration)
 
  class Foreman(nrOfWorkers: Int) extends Actor {
    
    def getPred(graph: Graph, v: Int) = {
      graph.predecessors(v)
    }
    
    def getSuc(graph: Graph, v: Int) = {
      graph.successors(v)
    }
    
    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")
      
    def receive = {
      case Instruct(graph, v) =>
        workerRouter ! Work(getPred, graph, 0)
        workerRouter ! Work(getSuc, graph, 0)
        sender ! Result(f(graph,v)) // perform the work
    }
  }
  
  class Worker(nrOfWorkers: Int) extends Actor {
    
    def receive = {
      case Work(f, graph, v) =>
        sender ! Result(f(graph,v)) // perform the work
    }
  }
 
  class Master(nrOfWorkers: Int, graph: Graph, nrOfElements: Int, listener: ActorRef)
    extends Actor {
 
    var pi: Double = _
    var nrOfResults: Int = _
    val start: Long = System.currentTimeMillis
 
    val workerRouter = context.actorOf(
      Props[Foreman].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")
    
    
    
    def receive = {
      case Calculate =>
        for (i ← 0 until 100) workerRouter ! Work(getPred, graph, 0)
      case Result(value) =>
       // pi += value
        nrOfResults += 1
        if (nrOfResults == 100) {
          // Send the result to the listener
          listener ! PiApproximation(pi, duration = (System.currentTimeMillis - start).millis)
          // Stops this actor and all its supervised children
          context.stop(self)
        }
    }
 
  }
 
  class Listener extends Actor {
    def receive = {
      case PiApproximation(pi, duration) ⇒
        println("\n\tPi approximation: \t\t%s\n\tCalculation time: \t%s"
          .format(pi, duration))
        context.system.shutdown()
    }
  }
 
 
  def concurrentSCC(nrOfWorkers: Int, graph: Graph) {
    val nrOfElements: Int = 1000 
    val nrOfMessages: Int = 1000
    // Create an Akka system
    val system = ActorSystem("PiSystem")
 
    // create the result listener, which will print the result and shutdown the system
    val listener = system.actorOf(Props[Listener], name = "listener")
 
    // create the master
    val master = system.actorOf(Props(new Master(
      nrOfWorkers, graph, nrOfElements, listener)),
      name = "master")
 
    // start the calculation
    master ! Calculate
 
  }
}
*/