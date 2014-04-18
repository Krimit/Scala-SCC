/** A queue of searches to be tidied */

import scala.collection.mutable.Queue

object TidyingQueue{
  val p = SearchParams.p

  /** Queues holding the searches, one for each worker */
  private val queue = Array.fill(p)(Queue[Search]())

  /** Locks protecting the queues */
  private val locks = Array.fill(p)(new AnyRef)

  /** Put s into the queue.
    * @param w the worker doing the put */
  def put(w: Int, s: Search) = Profiler.time(w, "TidyingQueue.put"){
    locks(w).synchronized{ queue(w) += s }
  }

  /** The next queue to try to do a get from */
  private var nextQ = 0

  /** Get a search */
  def get : Search = {
    var i = nextQ // next queue to try
    var res : Search = null // possible result
    do{
      res = locks(i).synchronized{ 
	if(queue(i).isEmpty) null else queue(i).dequeue
      }
      i = (i+1)%p
    } 
    while(res==null && i != nextQ)
    nextQ = i; res
  }

  def clear = queue.foreach(_.clear)

}
