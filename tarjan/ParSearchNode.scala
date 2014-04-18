/** A node reached during the search  
  * @param node the node's identity  */
class ParSearchNode(val node: Int){
  override def hashCode = node
  override def toString = node.toString

  /* Has this been initialised, via init? */
  // var initialised = false 
  /** The index, as in Tarjan's Algorithm */
  @volatile var index = -1 // 
  /** Low-link of this node */
  @volatile var lowlink = index
  /** The search this node is part of */
  // TODO: I think a search /identifier/ would be enough here
  @volatile var search : Search = null
  /** The successors of this node */
  var succs : Array[ParSearchNode] = null
  // TODO: do the above need to be volatile? 

  /** Various flags, packaged into a byte to reduce memory footprint */
  private var flags : Byte = 0

  /** Has this node been claimed by a search */
  private def claimed : Boolean = (flags & 1) != 0

  /** Record that this node has been claimed by a search */
  private def setClaimed = flags = (flags | 1).toByte

  /** Has this node been claimed by a search */
  // private var claimed = false // equivalent to search!=null || complete ??

  /** Is this node complete */
  def complete : Boolean = (flags & 2) != 0

  /** Set this node as complete */
  private def setComplete : Unit = flags = (flags | 2).toByte

  /** Is this node in a loop (if we're looking for loops or lassos) */
  def divergent : Boolean = synchronized{ (flags & 4) != 0 }

  /** Mark this node as in a loop or lasso */
  def setDivergent = synchronized{ flags = (flags | 4).toByte } 
  
  /** Has a call to next skipped over a divergent child because it was 
    * complete? */
  def unreturnedDivChild : Boolean = (flags & 8) != 0

  /** Record that a call to next skipped over a divergent child because it was 
    * complete */
  private def setUnreturnedDivChild = flags = (flags | 8).toByte
  
  // -------------------------------------------------------
  
  /** Is this node unclaimed by any search?
    * Not using a synchronized block is safe here */
  def isNew = !claimed // search == null

  /** Try to take ownership of this node and set the search.
    * @returns true iff successful */
  def takeOwnership(s: Search) : Boolean = synchronized{
    if(!claimed){ assert(search==null); setClaimed; search = s; true } 
    else false
  }

  /** Try to take ownership of this node, but with an empty list of 
    * successors.
    * @returns true iff successful  */
  def takeOwnershipEmpty : Boolean = synchronized{
    if(!claimed){ setClaimed; setComplete; true }
    else false
  }

  /** Is this node in the stack of search s? */
  private def inStack(s: Search) = s==search

  /** Is this node complete? */
  // var complete = false

  /** Which searches are blocked waiting for this node to complete?  (These
    * searches encountered this, rather than branching off from this.) */
  private[this] var blocked = List[Search]()

  /** Try to take ownership of this node.  If unsuccessful, find out if this
    * node is complete and in the stack of s and its index; record if
    * search s will have to block for this node to complete.
    * @param lassos is the search looking for lassos?
    * @return a triple (isNew, complete, inStack, divergent, index) where
    * isNew is true iff this is a new node that s has taken ownership of;
    * complete is true iff this node is complete; inStack is true iff this was
    * already in the stack of s; divergent is true iff this node is divergent;
    * index is the node's index.  */
  def getState(s: Search, lassos: Boolean) 
  : (Boolean, Boolean, Boolean, Boolean, Int) 
  = synchronized{
    if(!claimed){ search = s; setClaimed; (true, false, false, false, -1) }
    else{
      if(!complete && !inStack(s) && (!lassos || !divergent)) blocked ::= s
      (false, complete, inStack(s), divergent, index)
    }
  }

  /** Initialise this node, with successors succs, and index (and lowlink)
    * index */
  def init(succs: Array[ParSearchNode], index: Int) = synchronized{
    this.succs = succs; this.index = index; lowlink = index
    // N = succs.length; // initialised = true
  }

  // This is a node with state children : Set[ParSearchNode] representing 
  // the unexplored children.
  // Equivalently, it represents the edges (node, c) for c in children
  // private var N = -1 // = succs.length, once succs is initialised
  private var nextIx = 0 // index of next successor
  // children = succs[nextIx..N)

  /** Get the next successor, and a boolean flag that indicates whether 
    * we've skipped a divergent successor */
  def next : ParSearchNode = synchronized{ 
    // assert(initialised, this+" not initialised")
    // Try to return a node that satisfies isGood
    val N = succs.length
    var badStart = N
    // succs[badStart..N) were in progress at a different node when checked

    while(nextIx<N){
      val res = succs(nextIx) 
      // The following line can lead to a deadlock
      // val (rComplete, rSearch) = res.getCompleteSearch
      // The value of res.complete, res.search below may be stale, 
      // but that's safe
      if(res.complete){ // no point in returning a node known to be complete 
	nextIx += 1
	if(res.divergent) setUnreturnedDivChild
      }
      else if(res.search == null || res.search == this.search || 
	      nextIx >= badStart){
	nextIx += 1; return res // (unreturnedDivChild, res)
      }
      else{ // swap into the "bad" section
	badStart -= 1; succs(nextIx) = succs(badStart); succs(badStart) = res
      }
    }
    succs = null; null // free up memory
  }


  /** Update lowlink to take account of ix */
  // TODO: I think this can be made unsynchronized, and lowlink made
  // not-volatile
  def updateLowlink(ix: Int) = synchronized{ 
    // assert(initialised, this+" not initialised"); 
    lowlink = lowlink min ix 
  }

  /** Set the node as completed by search; unblock 
    * any other searches that this is blocking */
  def setComplete(search: Search, log: (=> String) => Unit) : Unit 
  = synchronized{
    // assert(initialised, this+" not initialised")
    assert(search==this.search && !complete, 
	   this+", "+search+", "+this.search+", "+complete)
    setComplete // removeFromStack(search)
    unblock(search, log)
  }

  /** Unblock any other searches that this is blocking */
  def unblock(search: Search, log: (=> String) => Unit) = synchronized{
    if(blocked.nonEmpty){ // Avoid called suspended.unsuspend if empty
      log("Unblocking "+blocked)
      for(s <- blocked) s.unblock(this) 
      Profiler.notime(search.owner, "unsuspend"){
	SearchParams.suspended.unsuspend(blocked)
      }
      // The order of blocked is important here
      Profiler.notime(search.owner, "makePending"){
	SearchParams.pending.makePending(blocked)
      }
      blocked = List[Search]()
    } 
  }

  /** Record that search s is no longer blocked waiting for this search */
  def noLongerBlocked(s: Search) = synchronized{
    assert(blocked contains s); blocked = blocked.filter(_ != s)
  }

  /** Record that search s is now blocked waiting for this search */
  def nowBlocked(s: Search) : Unit = synchronized{ 
    // assert(initialised, this+" not initialised"); 
    blocked ::= s 
  }

  /** Transfer to search s, and increment index and lowlink by delta.
    * @return the searches that were blocked on this, and the new index */
  def transfer(s: Search, delta: Int) : (Seq[Search], Int) = synchronized{
    // assert(initialised, this+" not initialised")
    search = s; index += delta; lowlink += delta; 
    assert(index>=0 && lowlink>=0)
    (blocked, index)
  }  

  /** Is this node in a loop (if we're looking for loops or lassos) */
  // private var divergent_ = false

  // /** Is this node in a loop (if we're looking for loops or lassos) */
  // def divergent = synchronized{ divergent_ }

  // /** Mark this node as in a loop or lasso */
  // def setDivergent = synchronized{ divergent_ = true }

  /** Has a call to next skipped over a divergent child because it was 
    * complete? */
  // var unreturnedDivChild = false


}

// -------------------------------------------------------


  /** Deal with any unexplored edges from here that do not lead to nodes that 
    * are in progress at other nodes. */ 
  // This doesn't seem useful
  // def tidy(log: (=> String) => Unit) = synchronized{
  //   var badStart = N 
  //   // succs[badStart..N) were in progress at a different node when checked
  //   while(nextIx<badStart){
  //     val next = succs(nextIx) 
  //     // The following line deadlocks (two threads, both calling this on the
  //     // node held by the other)
  //     // val (nComplete, nSearch, nIndex) = next.getCompleteSearchIndex
  //     // No harm in having a stale value for next.complete
  //     if(next.complete){ // can remove this one
  // 	log("Removing transition "+this+" -> "+next)
  // 	Profiler.count("tidied")
  // 	nextIx += 1  
  //     }
  //     // The following seems to cause problems when collecting SCCs.  It
  //     // might be if the node next is being transferred between searches
  //     // else if(nSearch==this.search){ 
  //     // 	log("Tidy: in-stack edge "+this+" -> "+next)
  //     // 	updateLowlink(nIndex); nextIx += 1 
  //     // 	log("Tidy: "+this+".lowlink = "+lowlink)
  //     // }
  //     else{ // swap into the "bad" section
  // 	badStart -= 1; succs(nextIx) = succs(badStart); succs(badStart) = next
  //     }
  //   }
  // }

  // ---------
  /** Find out if this node is complete and in the stack of s; record if
    * search s will have to block for this node to complete.
    * @returns a pair: (is this node complete, is this node in the stack of s)?
    */
  // def getState(s: Search) : (Boolean,Boolean) = synchronized{
  //   if(!complete && !inStack(s)) blocked ::= s
  //   (complete, inStack(s))
  // }

  // def checkStillInProgress(s: Search) : Boolean = synchronized{
  //   if(!complete) blocked ::= s;
  //   ! complete
  // }

  /** Is any successor of this node divergent?  */
  // def divChild : Boolean = succs.exists(_.divergent)
  /** Clear the successors (to free up memory) */
  // def clearSuccs = succs = null

  /** Deal with any unexplored edges from here that do not lead to nodes that 
    * are in progress at other nodes.  
    * Designed to be called by the search exploring this node. */ 
  // This hinders!
  // def tidy(log: (=> String) => Unit) = {
  //   var badStart = N 
  //   // succs[badStart..N) were in progress at a different node when checked
  //   while(nextIx<badStart){
  //     val next = succs(nextIx) 
  //     // The following line deadlocks (two threads, both calling this on the
  //     // node held by the other)
  //     // val (nComplete, nSearch, nIndex) = next.getCompleteSearchIndex
  //     // No harm in having a stale value for next.complete
  //     if(next.complete){ // can remove this one
  // 	log("Removing transition "+this+" -> "+next)
  // 	// Profiler.count("tidied")
  // 	nextIx += 1  
  //     }
  //     else if(next.search==this.search){ 
  //     	log("Tidy: in-stack edge "+this+" -> "+next)
  //     	updateLowlink(next.index); nextIx += 1 
  //     	log("Tidy: "+this+".lowlink = "+lowlink)
  //     }
  //     else{ // swap into the "bad" section
  // 	badStart -= 1; succs(nextIx) = succs(badStart); succs(badStart) = next
  //     }
  //   }
  // }
