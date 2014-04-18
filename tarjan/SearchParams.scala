/** An object encapsulating all the search parameters */
object SearchParams{
  var p = 1 // the number of workers, default to sequential
  var g : Graph = null // the graph
  var rooted = true  // does the search start at a root node?
  var unrooted = false // opposite of rooted
  var loops = false // are we searching for loops (including lassos)?
  var lassos = false // are we searching for lassos?

  // The components
  var stealingQueue : StealingQueue = null // the stealing queue
  var pending : Pending = null
  var suspended : Suspended = null // the Suspended object
  var scheduler : Scheduler = null // the Scheduler object

  /** The mapping storing nodes */
  // var seen : Seen = null // the mapping storing nodes
  var seen : Seen = OpenAddressingSeen  // the mapping storing nodes
  // var seen : Seen = ShardedSeen // the mapping storing nodes
  var sccs = new SCCSet
 
  /** Initiailise everything ready for a new search
  * @param g the graph to be searched
  * @param p the number of workers to use
  * @param rooted is the search rooted
  * @param lockFree do we use lock-free queues?  
  */
  def init(g: Graph, p: Int, rooted: Boolean, loops: Boolean, lassos: Boolean,
	   lockFree: Boolean) 
  = {
    this.p = p; this.g = g; this.rooted = rooted; unrooted = !rooted
    this.loops = loops; this.lassos = lassos
    // Initialise seen
    // if(unrooted) seen = new NodeTableSeen(g.nodes.size) 
    // else{  seen = ShardedSeen; seen.clear }
    // if(rooted) seen = new CHMSeen(16) else seen = new CHMSeen(2*g.nodes.size)
    // seen = new ShardedSeen(23*p + 13) // (197)
    seen.clear; 
    Divergences.clear
    // initialise stealingQueue and pending
    if(unrooted){
      stealingQueue = new UnrootedStealingQueue(g.nodes, seen)
      pending = if(lockFree) LockFreePending else LockingStackPending
    }
    else if(lockFree){ 
      stealingQueue = LockFreeStealingQueue; pending = LockFreePending
    }
    else{
      stealingQueue = LockingStealingQueue; pending = LockingStackPending
    }
    suspended = new Suspended; scheduler = new Scheduler(p); 
    sccs = new SCCSet // scala.collection.mutable.Set()
    // Search.reset // ; TidyingQueue.clear
    // stealingQueue.clear
  }

  /** Check everything is done */
  def checkDone = {
    stealingQueue.checkDone; pending.checkDone; suspended.checkDone
  }


}
