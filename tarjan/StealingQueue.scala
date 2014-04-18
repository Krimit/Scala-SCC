import scala.collection.mutable.Queue

/** A queue of nodes from which workers can start searches.  */
trait StealingQueue {
  /** Get the next node to consider.  Returns null if there is no such. 
    * @param w the worker doing the get */
  def get(w: Int) : ParSearchNode

  /** Add all of nodes to the queue */
  def putAll(nodes: Seq[ParSearchNode], owner: Int) : Unit
  
  /** Initialise owner's queue with the given queue */
  def init(queue: Queue[ParSearchNode], owner: Int)

  /** Is the queue empty */
  def isEmpty : Boolean

  /** Check the queue is empty */
  def checkDone : Unit = assert(isEmpty)

  /** Clear ready for the next iteration */
  def clear : Unit
}

// -------------------------------------------------------

/** A lock-free implementation */

import ox.cads.collection.LockFreeQueue

object LockFreeStealingQueue extends StealingQueue{
  private val queue = new LockFreeQueue[ParSearchNode]

  def get(w: Int) = queue.dequeue

  def putAll(nodes: Seq[ParSearchNode], owner: Int) = queue.enqueue(nodes)
  
  /** Initialise owner's queue with the given queue */
  def init(queue: Queue[ParSearchNode], owner: Int) = 
    putAll(queue.toList, owner)

  def isEmpty = queue.isEmpty

  def clear = {}
}

// -------------------------------------------------------

/** A version using locks */
object LockingStealingQueue extends StealingQueue{
  val p = SearchParams.p

  /** The nodes that can be stolen */
  private val queues = Array.fill(p)(new Queue[ParSearchNode])

  /** Locks protecting queues */
  private val locks = Array.fill(p)(new AnyRef)

  /** The order in which each worker tries to access nodes from queues */
  private val orders = Array.tabulate(p)(w => (w until p) ++ (0 until w))

  /** Get the next node to consider.  Returns null if there is no such. 
    * @param w the worker doing the get */
  def get(w: Int) : ParSearchNode = {
    var iters = 0; var result : ParSearchNode = null
    while(iters < p && result == null){
      val i = (w+iters)%p
      if(queues(i).nonEmpty) // I'm not sure if this is useful
	 locks(i).synchronized{ 
	   // Profiler.count("StealingQueue.get")
	   if(queues(i).nonEmpty) // necessary in case last item taken since
				  // check
	     result = queues(i).dequeue 
	 }
      iters += 1
    }
    result
  } 

  /** Add all of nodes to the queue */
  def putAll(nodes: Seq[ParSearchNode], owner: Int) = 
    locks(owner).synchronized{ queues(owner) ++= nodes; () }

  /** Initialise owner's queue with the given queue */
  def init(queue: Queue[ParSearchNode], owner: Int) =
    locks(owner).synchronized{ queues(owner) = queue }

  def isEmpty = queues.forall(_.isEmpty)

  /** Reset for the next iteration */
  def clear = { } // seen.clear 
}

// -------------------------------------------------------

/** A version optimised for the unrooted case.  
  * It is initialised with an array of Ints, giving the identities of nodes;
  * and putAll is never called. */

import java.util.concurrent.atomic.AtomicInteger

class UnrootedStealingQueue(ids: Array[Int], seen: Seen) extends StealingQueue{
  private var next = new AtomicInteger(0) // index of next id to give
  private val size = ids.size

  def get(w: Int) : ParSearchNode = {
    var res : ParSearchNode = null // result to return
    var theNext = next.getAndIncrement // index of value to take
    while(theNext < size && res == null){
      val id = ids(theNext) // identity of node
      val node = 
	Profiler.notime(w, "SQ.get -- seen.getOrInit"){ seen.getOrInit(id) }
      if(node.isNew) res = node // return this one
      else theNext = next.getAndIncrement // try next one
    }
    res
  }
    
  def putAll(nodes: Seq[ParSearchNode], owner: Int) = 
    throw new UnsupportedOperationException

  def init(queue: Queue[ParSearchNode], owner: Int) = 
    throw new UnsupportedOperationException

  def isEmpty = next.get >= size

  def clear = next.set(0)
}
