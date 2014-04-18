import scala.collection.mutable.{Set,Map}

/** An object to keep track of the suspended searches
  * @param p the number of workers */

class Suspended{
  /** The searches that are currently suspended, mapped to the nodes they
    * are waiting for */
  private var suspended = Map[Search, Search]()
  // Invariant: there's no cycle of blocking

  val lassos = SearchParams.lassos

  /** Record the search s as having been suspended, blocked by n.
    * Except, if this would create a cycle of blocked searches, perform
    * cycle reduction as described in the paper.
    * @return a pair (suspended, loopNode); suspended is true if the search 
    * was suspended; if not, then loopNode gives a node such that an edge 
    * from the last node of s.controlStack to loopNode creates a loop. */
  def suspend(s: Search, n: ParSearchNode, owner: Int, log: (=>String) => Unit)
  : (Boolean, ParSearchNode) 
  = {
    var path = List[Search]()
    log("Trying to find cycle from "+s)
    synchronized{
      // the blocking search, needs to be calculated here, in case n has been
      // transferred to a different search after suspend was called
      val blocker = n.search
      path = Profiler.notime(owner, "suspend-findCycle"){findPath(blocker, s)} 
      if(path==null) 
	suspended += s -> blocker // no cycle found; search suspended
    } // end of synchronized block
    log("cycle = "+(if(path==null) null else s :: path))
    if(path==null) (true, null)
    else Profiler.notime(owner, "suspend - else"){ 
      // Cycle found.  Traverse the cycle, transferring nodes to s
      assert(path.length>0)
      if(!lassos) transfer(s, path, owner, log)
      else 
	// The calling procedure will mark all nodes of s as divergent, and
	// recursively the other searches will be unblocked.
	(false, null)
    }
  }

  /** Transfer nodes between the searches of path, so as to unblock the 
    * waiting cycle.  Other parameters and return value are as for suspend. */
  private def transfer(s: Search, path: Seq[Search], owner: Int, 
		       log: (=>String) => Unit) 
  = {
    // Each (bs, s1) in blockedPairs means that for each b in bs, search b
    // was blocked on s1 but is now blocked on s
    var blockedPairs = List[(Seq[Search], Search)]()
    // Empties are the searches where all nodes have been transferred to s,
    // and that should now be deleted; nonEmpties are the non-empty searches
    // that are now blocked on s
    var empties = List[Search](); var nonEmpties = List[Search]()

    // Inv: n1 is the node of the current search (s1) that blocked the
    // previous search
    var n1 = s.getBlockingNode(path(0)); log(s+" blocked by "+n1)

    for(s1 <- path){
      log("Transferring nodes from "+s1)
      assert(n1 != null)
      // Get control stack/Tarjan stack nodes to transfer (oldest first)
      val (cs,ts,blocker1,isEmpty) = 
	Profiler.notime(owner, "getTransferNodes"){ s1.getTransferNodes(n1) }
      n1 = blocker1
      log("Transferring "+cs+",\n\t"+
	  ts.map(nn => nn+"("+nn.index+","+nn.lowlink+")").mkString(", ")+
	  "\n\tfrom "+s1+" to "+s+"; "+s1+" was blocked by "+n1)
      // Transfer the nodes to s; get searches blocked by s
      var bs = Profiler.notime(owner, "transfer"){ s.transfer(cs,ts) }
      val bs1 = bs.filter(b => b!=s && !path.contains(b))
      if(bs1.nonEmpty){
	log(bs1+" were blocked by "+s1+", but are now blocked by "+s)
	blockedPairs ::= (bs1, s1)
      }
      // s1 now blocked by s; if it's empty,arrange to remove it
      if(isEmpty){ log(s1+" now empty; removing"); empties ::= s1 } 
      else nonEmpties ::= s1
    } // end of for loop

    // Now transfer blocking according to blockedPairs, empties, nonEmpties
    synchronized{
      for((bs1,s1) <- blockedPairs){
	var bs = bs1
	while(bs.nonEmpty){
	  val b = bs.head; bs = bs.tail
	  if(suspended.contains(b))
	    if(suspended(b) == s1) suspended(b) = s 
	    else{
	      // b was blocked on some node n1 of s1 at the time of the call
	      // to transfer, but that hadn't yet been registered in
	      // suspended.  n1 was transferred to s by the call to transfer.
	      // Subesquently, the blocking of b was registered here, so b is
	      // blocked on s.  Note that it can't be transferred again before
	      // s is resumed.
	      assert(suspended(b) == s, suspended+"; "+b+"; "+s+"; "+s1)
	      log(b+" has already been recorded as blocked by "+s+
		  ", when "+b+" blocked")
	    }
	  else log(b+" was blocked by "+s1+", but this hasn't yet been "+
		   "registered in suspended; it will now be blocked by "+s+
		   "; this will sort itself out...")
	} // end of while loop
      }
      for(s1 <- empties) suspended -= s1
      for(s1 <- nonEmpties) suspended(s1) = s
      
      log("suspended = "+suspended) // -- expensive
    } // end of synchronized block

    (false, n1) // search not suspended, loop back to n1
  }

  /** Try to find a path of blocking searches from start to target.
    * @return the path (excluding the final copy of start),
    *         or null if there is no such */
  private def findPath(start: Search, target: Search) : List[Search] = {
    assert(target!=start); var current = start // current node
    var path = List[Search](current) // path followed so far, in reverse
    while(suspended.contains(current) && current != target){
      current = suspended(current); assert(!path.contains(current))
      if(current != target) path ::= current 
    }
    if(current!=target) null else path.reverse
  }

  /** Try to find a path of blocking searches from start to itself.
    * @return the path (excluding the final copy of start),
    *         or null if there is no such */
  // private def findCycle(start: Search) : List[Search] = {
  //   assert(suspended.contains(start))
  //   var current = suspended(start) // current node
  //   assert(current!=start)
  //   var path = List[Search](current, start) // path followed so far, in reverse
  //   while(suspended.contains(current) && current != start){
  //     current = suspended(current)
  //     if(current != start){ assert(!path.contains(current)); path ::= current }
  //   }
  //   if(!suspended.contains(current)) null
  //   else path.reverse
  // }

  /** Record the searches in ss as now pending.
    * Note: this should be called only after the search has been marked as 
    * Pending, and other relevant updates made. */
  def unsuspend(ss: Seq[Search]) = synchronized{
    assert(ss.forall(suspended.contains(_)), ss.toString)
    suspended --= ss // ; pending = ss ++ pending
  }

  /** Check that there are no remaining suspended searches */
  def checkDone = assert(suspended.isEmpty, suspended)

  /** Number of suspended searches */
  def size : Int = synchronized{ suspended.size }
  
}
