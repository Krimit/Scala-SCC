import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer

/** Each object of this class encapsulates a search
  * @param owner the worker that owns this search
  * @param log a logging function
  * @param id this node's identity
  * @param firstNode the node from which the search should start
  * @param firstSuccs the successors of firstNode */

class Search(var owner: Int, log: (=> String) => Unit, id: String,
	     firstNode: ParSearchNode, firstSuccs: Array[Int])
{
  /** Set to true if firstNode already encountered */
  var aborted = false

  private val sccs = SearchParams.sccs
  private val g = SearchParams.g
  private val seen = SearchParams.seen
  private val loops = SearchParams.loops
  private val lassos = SearchParams.lassos

  // Logging functions
  override def hashCode = id.hashCode
  override def toString = "S"+id
  private var iter = 0 // iteration number, for logging
  private def item = id+"."+iter+":\t"
  private def taggedLog(s: => String) = log(item+s)

  /** The status of this node, as defined in the paper.
    * Only used in assertions. */
  private var status = Search.InProgress

  // -------------------------------------------------------
  /** The stack as used in Tarjan's Algorithm (owner-private) */
  private val tarjanStack = new Stack[ParSearchNode]

  /** The control stack (shared with the Pending object) */
  private val controlStack = new Stack[ParSearchNode]

  /** The current index, as in Tarjan's algorithm (owner-private) */
  private var index = 0

  /** Initialise node and add it to the stacks; create the nodes corresponding
    * to succs (if not already seen) and add them to the seen set and
    * StealingQueue.  */
  private def initChild(node: ParSearchNode, succs: Array[Int]) 
  = Profiler.notime(owner, "initChild"){
    if(SearchParams.rooted){
      // Get the nodes corresponding to succs, creating and registering them
      // with seen if new; newNodes are the new ones.
      val (nodeSuccs, newNodes) = 
	Profiler.notime(owner, "seen.putAll"){seen.putAll(owner, succs)}
      // Initialise node, setting its successor nodes.  This search processes
      // nodes in the opposite order from which they'll be taken from the
      // StealingQueue, so as to reduce clashes.
      node.init(nodeSuccs, index); index += 1
      // Register the new nodes with the stealing queue.
      Profiler.notime(owner, "stealingQueue.putAll"){ 
	SearchParams.stealingQueue.putAll(newNodes, owner)
      }
    }
    else{
      // Get the nodes corresponding to succs from seen
      // TODO: test if the isEmpty check helps
      val nodeSuccs = 
	if(succs.isEmpty) Array[ParSearchNode]()
	else Profiler.notime(owner, "seen.putAllUnrooted"){
	      seen.putAllUnrooted(owner, succs)}
      Profiler.notime(owner,"node.init"){
	node.init(nodeSuccs, index); index += 1}
    }
  
    controlStack.push(node); tarjanStack.push(node)
  }

  // Initialise this search, based on firstNode.
  // Try to take ownership of firstNode
  val isNew = firstNode.takeOwnership(this)
  if(isNew){     
    log(item+firstNode+" seen for first time (in constructor), index = "+index)
    Profiler.notime(owner, "initChildConstructor"){
      initChild(firstNode, firstSuccs)}
    iter += 1 
  }
  else{
    log(item+firstNode+" seen again in constructor, aborting")
    aborted = true // abort this search
  }
 
  // If we suspend because we encounter an in-progress node, then we store
  // that node in waitingFor, and the search we're waiting for in suspended
  // (in expandEdge).  The update to the blocking node, the update to the
  // status, and the update to suspended are done atomically.

  // This node is marked as unblocked by unblock, suspended is updated via
  // unsuspend, and the Scheduler updated via makePending (all called from
  // ParSearchNode.setComplete).  The update in scheduler is done last, so the
  // search doesn't get rescheduled prematurely.

  /** Which node, if any, is this waiting for? */
  private var waitingFor: ParSearchNode = null  

  /** Indicate that the search is no longer blocked by an 
    * in-progress node n that was encountered.  
    * Called from n.setComplete. */
  def unblock(n: ParSearchNode) = synchronized{
    log(this+" unblocked")
    assert(n==waitingFor && status==Search.Suspended, 
	   this+"; "+n+"; "+waitingFor+"; "+status)
    status = Search.Pending
  }

  /** Indicate that the search is no longer blocked, because a blocking
    * cycle was found in the lassos case.  
    * Called from Suspended.suspend. */
  // def unblock = synchronized{
  //   log(this+" unblocked by blocking cycle reduction")
  //   assert(lassos && status==Search.Suspended)
  //   status = Search.Pending
  // }

  /** Indicate that this search is about to be resumed by worker w */
  def resume(w: Int) = synchronized{ 
    assert(status==Search.Pending, this+"; "+w+"; "+status)
    owner = w // must be done before setDivergent
    // If we're looking for divergences and waitingFor is divergent, mark
    // stack as divergent and unblock any waiting
    if(lassos && waitingFor.divergent) setDivergent
    log("Resuming "+this+"; "+controlStack+tarjanStack+
	"; "+waitingFor.divergent)
    waitingFor = null; status = Search.InProgress 
  }

  /** Get the node of Search s that is blocking this, or null if s blocked 
    * before reaching a new node. */
  def getBlockingNode(s: Search) : ParSearchNode = synchronized{
    assert(waitingFor != null && status==Search.Suspended && 
	   waitingFor.search == s)
    waitingFor 
  }

  //----- Transferring nodes to break cycles of blocking
  /** Get the nodes that need to be transferred to another search in order to
    * break a cycle of suspended searches, removing them from this search's 
    * stacks.  Also update which node this is waiting for, both here and in that 
    * node.
    * @param n1 the node of this that is blocking the previous search in the 
    *        cycle
    * @return a tuple (cs, ts, oldWaitingFor, isEmpty) where:
    *         cs is the nodes to go into the control stack; ts is the nodes 
    *         to go into the Tarjan stack (both with the oldest node first);
    *         oldWaitingFor is the node this was previously waiting for; and
    *         isEmpty indicates whether this is now empty. */
  // And the searches these nodes were blocking ?
  def getTransferNodes(n1: ParSearchNode) 
  : (List[ParSearchNode], List[ParSearchNode], ParSearchNode, Boolean) 
  = synchronized{
    assert(status == Search.Suspended && waitingFor != null && !lassos)
    // assert(tarjanStack.contains(n1), n1+"; "+controlStack+"; "+tarjanStack)
    var next = tarjanStack.pop
    var ll = next.lowlink // the lowest lowlink seen so far
    var reachedN1 = next==n1 // have we reached n1 yet?
    var ts = List(next)// tarjanStack nodes to be transferred (oldest first)
    while(!reachedN1 || ll < next.index){
      next = tarjanStack.pop; ts ::= next; ll = ll.min(next.lowlink)
      if(next == n1) reachedN1 = true
    }
    val l = next // the oldest node to transfer

    // Now get the corresponding nodes from the control stack
    // assert(controlStack.contains(l), next+"; "+controlStack)
    next = controlStack.pop
    var cs = List(next) // tarjanStack nodes to be transferred (oldest first)
    while(next != l){ next = controlStack.pop; cs ::= next }

    val isEmpty = tarjanStack.isEmpty // if true, this will be removed
    // Record that we're now blocked on l (if non-empty)
    val oldWaitingFor = waitingFor
    waitingFor.noLongerBlocked(this)
    waitingFor = l; 
    if(!isEmpty) l.nowBlocked(this) 
    else{ assert(controlStack.isEmpty); status = Search.Complete }
    log(this+" now waiting for "+l)
    
    (cs, ts, oldWaitingFor, isEmpty)    
  }

  /** Transfer the nodes cs and ts onto this Search's control and Tarjan 
    * stacks, respectively.  (cs, ts) is produced by getTransferNodes
    * @return all searches that were blocked on these nodes */
  def transfer(cs: List[ParSearchNode], ts: List[ParSearchNode]) : Seq[Search]
  = synchronized{
    val delta = index - cs.head.index; log("delta = "+delta)
    var allBlocked = List[Search]() // ArrayBuffer[Search]()
    for(n <- ts){ 
      val (blocked, newIndex) = n.transfer(this, delta)
      allBlocked = blocked.toList ++ allBlocked 
      // allBlocked.appendAll(blocked) ; 
      index = index max newIndex
      tarjanStack.push(n) 
    }
    controlStack.pushAll(cs)
    index = index + 1 // index is now greater than all in the stack
    log("index = "+index)
    allBlocked
  }

  // -------------------------------------------------------
  // Methods involved in executing the search

  /** Mark all nodes in the stack as divergent; unblock any worker blocked
    * on them.
    * Pre: lassos (i.e. we are indeed looking for divergences) */
  private def setDivergent = {
    assert(lassos)
    var st = tarjanStack.toList
    log(item+"Marking "+st+" as divergent")
    while(st.nonEmpty && ! st.head.divergent){
      Divergences.add(owner, st.head) // mark st.head as divergent
      st.head.unblock(this, log) // unblock any searches blocked on st.head
      st = st.tail
    }
  }

  /** Expand edge from parent to child
   * @param branching is true iff this edge is branching from another stack
   * @return true if the expansion was possible; false if we need to suspend */
  private def expandEdge(parent: ParSearchNode, child: ParSearchNode): Boolean 
  = synchronized{
    // Note: this is synchronized to ensure that the call to child.getState
    // and the updates to waitingFor and status are atomic (or else child
    // could call unblock before this search thinks it's blocked).
    // TODO: this can probably be fixed by adding a new status value 
    // "prematurely unblocked", and making status atomic.
    val (isNew, complete, inStack, divergent, cIndex) = 
      child.getState(this, lassos)
    if(isNew) Profiler.notime(owner, "new node"){
      log(item+"Expanding "+parent+" -> "+child+"; "+
	  child+" seen for first time, index = "+cIndex)
      val succs = Profiler.notime(owner, "succs"){ g.succs(child.node) }
      Profiler.notime(owner, "initChild-EE"){initChild(child, succs)}; true 
    }
    else if(complete || lassos && divergent){ 
      // Previously seen or divergent child
      log(item+"Expanding "+parent+" -> "+child+"; "+
	  (if(lassos && divergent) "divergent" else "done")+
	  " node "+child+" seen again")
      if(lassos && divergent) setDivergent
      true
    }
    else if(inStack){
      log(item+"Expanding "+parent+" -> "+child+"; loop found.")
      if(lassos) setDivergent 
      else if(loops && parent==child) Divergences.add(owner, child)
      else parent.updateLowlink(cIndex)
      true
    }
    else Profiler.notime(owner, "suspending"){ 
      // Child is not complete, in a different search.
      // The call to getState above recorded that this search will have
      // to block until child completes, via setBlocked
      log(item+"Expanding "+parent+" -> "+child+"; active node "+child+
	  " seen again; suspending")
      // TODO: can the following be moved after the call to suspendByNode?
      waitingFor = child; status = Search.Suspended
      val (suspended, loopNode) = Profiler.notime(owner, "suspend"){
	SearchParams.suspended.suspend(this, child, owner, taggedLog)
      }
      if(suspended) false
      else Profiler.notime(owner,"unsuspend"){ 
	waitingFor = null; status = Search.InProgress 
	child.noLongerBlocked(this) // undo the blocking
	log(item+"Resuming search; Tarjan stack = "+
	    (for(n <- tarjanStack) yield n+"("+n.index+", "+n.lowlink+")").
	    mkString(", ")+
	    "\nControl stack = "+controlStack)
	// Get the loop-back edge
	if(loopNode != null){
	  val pp = controlStack.top; assert(loopNode.search == this)
	  log(item+"Loop found via edge "+pp+" -> "+loopNode)
	  pp.updateLowlink(loopNode.index); 
	  log(item+pp+".lowlink = "+pp.lowlink)
	}
	else{ 
	  // Mark all nodes s as divergent.  This will have the side effect of
	  // unblocking the search s1 blocked on this.  When s1 is resumed,
	  // all its nodes will be marked as divergent, and the search s2
	  // blocked on s will be unblocked.  And so on.  Note this must
	  // follow the call to child.noLongerBlocked(this), to avoid
	  // deadlocks.
	  assert(lassos); setDivergent 
	}
	true
      } // end of last else (unsuspended) 	  
    } // end of middle else (suspending case)
  } // end of expandEdge

  /** Run the search until complete or forced to suspend 
    * @returns Search.Suspend if forced to suspend, Search.Done if complete */
  def apply() = synchronized{
    // I think the synchronised is unnecessary: other public methods are
    // called only when this is suspended/pending
    var done = false; var suspending = false
    assert(status == Search.InProgress) 

    while(!done && !suspending){
      if(controlStack.nonEmpty){
	val node : ParSearchNode = controlStack.top
	val child : ParSearchNode = node.next
	// val (unreturnedDivChild, child): (Boolean,ParSearchNode) = node.next
	// if(lassos && unreturnedDivChild) setDivergent
	if(child != null) Profiler.notime(owner, "ExpandEdge"){
	  val ok = expandEdge(node, child)
	  if(!ok) suspending = true
	}
	else Profiler.notime(owner,"backtrack"){ // Backtrack
	  controlStack.pop
	  log(item+"Backtracking from "+node+"; "+
	      node+".index = "+node.index+"; "+node+".lowlink = "+node.lowlink)
	  // The following is necessary in case there was a divergent child
	  // not returned by node.next because it was complete 
	  if(lassos && node.unreturnedDivChild) setDivergent 
	  // Update next node's lowlink
	  if(controlStack.nonEmpty) 
	    controlStack.top.updateLowlink(node.lowlink)
	  if(node.lowlink == node.index)Profiler.notime(owner, "making SCC"){ 
	    if(!loops){
	      // Collect the scc
	      var scc = List[Int](); var w : ParSearchNode = null
	      do { 
		// Add next node w to this scc; unblock any other searches
		// blocked on it
		w = tarjanStack.pop; scc ::= w.node; w.setComplete(this, log)
	      } while(w != node)
	      log(item+"SCC found: "+scc)
	      val sccSet = scc.toSet; sccs.add(sccSet, owner)
	    }
	    else if(lassos){
	      // Mark all nodes in this SCC as complete, and unblock any
	      // searches blocked on them
	      var w : ParSearchNode = null
	      do{ 
		w = tarjanStack.pop; w.setComplete(this, log) 
		log(item+"Marking "+w+" as complete")
	      }
	      while(w != node)
	    }
	    else { // loops but not lassos
	      var scc = List[ParSearchNode](); var w : ParSearchNode = null
	      do { 
		// Add next node w to this scc
		w = tarjanStack.pop; scc ::= w; w.setComplete(this, log)
	      } while(w != node)
	      log(item+"SCC found: "+scc)
	      if(scc.size > 1) Divergences.add(owner, scc)
	      // Unblock searches blocked on each node in the SCC; must be
	      // done after marking the node as on a loop
	      // I now don't understand that comment!  Maybe it applies to 
	      // lassos
	      // scc.foreach(_.setComplete(this, log))
	    } // end of loops but not lassos
	  } // end of if(node.lowlink == node.index)
	} // end of Backtrack
      } // end of if(controlStack.nonEmpty)
      else done = true 

      iter += 1 
    } // end of while

    if(done){
      log(item+"Search complete")
      assert(controlStack.isEmpty && tarjanStack.isEmpty, 
	     this+": "+controlStack+"; "+tarjanStack )
      status = Search.Complete
    }
    else assert(suspending) // Updates were done earlier
  } // end of apply

}

// -------------------------------------------------------

object Search{
  /** The status of the search */
  type SearchStatus = Byte
  val Complete : SearchStatus = 0
  val InProgress : SearchStatus = 1
  val Suspended : SearchStatus = 2
  val Pending : SearchStatus = 3

  // private var count = 0
  // def getId = synchronized{ count += 1; count-1 }
  // def reset = count = 0

  // private val count = new java.util.concurrent.atomic.AtomicInteger(0)
  // /** Get an identity for a Search */
  // def getId = count.getAndIncrement
  // def reset = count.set(0)

}

// -------------------------------------------------------

  /** Try to help this search by tidying nodes
    * @param w the worker doing the tidying */
  // This doesn't seem useful
  // def tidy(w: Int) = {
  //   // Note: the following is unsafe; but there's no harm in tidying incorrect
  //   // nodes
  //   if(status != Search.Complete){
  //     val stack = controlStack.toList
  //     if(stack.nonEmpty) for(n <- stack.tail){ log("Tidying "+n); n.tidy(log) }
  //     TidyingQueue.put(w, this)
  //   }
  // }

	// The following attempts to explore elsewhere on the stack.  
	// It gives huge slow-downs
	// for(n <- controlStack.toList){ log("Tidying "+n); n.tidy(log) }
	// if(!child.checkStillInProgress(this)){
	//   log(item+"Expanding "+parent+" -> "+child+"; node "+child+
	//       " now complete")
	//   true
	// }
	// else
	// log(item+"Expanding "+parent+" -> "+child+"; "+child+
	//     "still in progress; suspending") 
