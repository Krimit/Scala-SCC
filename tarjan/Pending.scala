/** The searches that were blocked but are now pending */

trait Pending{
  def makePending(ss: Seq[Search]) : Unit

  def getPending : Search

  def checkDone : Unit
}

// -------------------------------------------------------

/** A lock-free implementation */
import ox.cads.collection.LockFreeQueue

object LockFreePending extends Pending{  
  private val pending = new LockFreeQueue[Search]

  def makePending(ss: Seq[Search]) = { 
    /* println("makePending "+ss); */ pending.enqueue(ss) 
  }

  def getPending : Search = pending.dequeue

  def checkDone = assert(pending.isEmpty)
}

// -------------------------------------------------------

/** An implementation using synchronized blocks, using stack order */

import scala.collection.mutable.Stack

object LockingStackPending extends Pending{
  /** The searches that are currently pending */
  private var pending = Stack[Search]()

  /** Record the searches in ss as now pending.
    * Note: this should be called only after the search has been marked as 
     * Pending, and other relevant updates made.
     */
  def makePending(ss: Seq[Search]) = synchronized{
    pending.pushAll(ss) //  = ss ++ pending
  }

  /** Check that we're done */
  def checkDone = assert(pending.isEmpty)

  /** Try to get a pending search */
  def getPending : Search = 
    if(pending.nonEmpty) synchronized{
      if(pending.nonEmpty) pending.pop else null
    }
    else null
}

// -------------------------------------------------------

/** An implementation using synchronized blocks, using queue order */

import scala.collection.mutable.Queue

object LockingQueuePending extends Pending{
  /** The searches that are currently pending */
  private var pending = Queue[Search]()

  /** Record the searches in ss as now pending.
    * Note: this should be called only after the search has been marked as 
     * Pending, and other relevant updates made.
     */
  def makePending(ss: Seq[Search]) : Unit = synchronized{
    pending ++= ss //  = ss ++ pending
  }

  /** Check that we're done */
  def checkDone = assert(pending.isEmpty)

  /** Try to get a pending search */
  def getPending : Search = 
    if(pending.nonEmpty) synchronized{
      if(pending.nonEmpty) pending.dequeue else null
    }
    else null
}
