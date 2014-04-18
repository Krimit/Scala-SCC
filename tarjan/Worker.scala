/** A worker in the search.  
  * @param me the identity of this worker
  * @param log a logging function
*/

class Worker(me: Int, log: (=> String) => Unit)
{
  private val suspended = SearchParams.suspended
  private val scheduler = SearchParams.scheduler
  private val seen = SearchParams.seen
  private val g = SearchParams.g
  private val sccs = SearchParams.sccs

  /** The current search object */
  private var search : Search = null

  private var iter = 0 // iteration number, for logging
  private var searchNo = 0 // number of the current search
  /** Get the identity of the next search */
  private def getSearchId : String = {
    val res = me.toString+"."+searchNo; searchNo += 1; res 
  }

  if(SearchParams.unrooted) Profiler.notime(me, "init"){
// if(false){    
//     val gSize = SearchParams.g.nodes.size; val p = SearchParams.p
//     // This node is responsible for registering 
//     // g.nodes[me*chunk .. me*chunk+myChunk)   
//     val chunk = gSize / p
//     val myChunk = if(me == p-1) gSize-(p-1)*chunk else chunk
//     val myNodeIds = SearchParams.g.nodes.slice(me*chunk, me*chunk+myChunk)
//     // Add the nodes corresponding to myNodeIds to seen
//     val nodes = 
//       Profiler.notime(me, "seen.putAllUnrootedInit"){
// 	seen.putAllUnrootedInit(me, myNodeIds)}
//     // And register those nodes in the stealing queue
//     // Profiler.notime(me, "init3"){SearchParams.stealingQueue.putAll(nodes, me)}
//     Profiler.notime(me, "init3"){SearchParams.stealingQueue.init(nodes, me)}
// }
  }
  else if(me==0){
    // Add start node to search of worker 0
    val initNodeId = SearchParams.g.init 
    val (nodes, newNodes) =
      Profiler.notime(me, "seen.putAll"){seen.putAll(me, Array(initNodeId))}
    assert(nodes.length == 1 && newNodes.length == 0, 
	   nodes.mkString(",")+"; "+newNodes)
    // no need to call StealingQueue.putAll, as the array is a singleton
    val firstNode = nodes.head
    val firstSuccs = g.succs(firstNode.node)
    search = new Search(me, log, getSearchId, firstNode, firstSuccs)
  }
  

  /** Run the worker  */
  def apply() = { 
    var done = false // have all nodes been explored?
    def item = "Worker "+me+"."+iter+":\t"
    def taggedLog(s: => String) = log(item+s)

    while(!done){
      if(search!=null) Profiler.notime(me, "search.apply"){search()}
      // The call to search cleans up after itself

      log(item+"Trying to get new search") // get a new search to work on
      Profiler.notime(me, "scheduler.get"){ scheduler.get(me, taggedLog) } 
      match{
	case Scheduler.Resume(s) => {
	  log(item+"Resuming suspended search "+s); 
	  search = s; search.resume(me)
	}

	case Scheduler.Branched(firstNode) => Profiler.notime(me, "Branched"){
	  // Create new search
	  if(firstNode.isNew){
	    val firstSuccs = Profiler.notime(me, "succs"){ 
	      g.succs(firstNode.node) }
	    if(firstSuccs.isEmpty){ // Avoid creating new Search in this case
	      log(item+"Branching to "+firstNode+" with empty successors")
	      search = null
	      if(firstNode.takeOwnershipEmpty){
		// Produce a SCC containing just firstNode
		val scc = Set(firstNode.node)
		sccs.add(scc, me)
	      }
	      else log(item+firstNode+" already owned")
	    } // end of if(firstSuccs.isEmpty)
	    else{
	      search = Profiler.notime(me, "new search"){
		new Search(me, log, getSearchId, firstNode, firstSuccs)}
              if(search.aborted) search = null 
              else log(item+"Branching to "+firstNode+", "+search)
	      // About 2 per million are aborted
	    }
	  } // end of if(firstNode.isNew)
	  else search = null
	} // end of case Scheduler.Branched

	// case Scheduler.Tidy(s) => {
	//   log(item+" tidying "+s); 
	//   Profiler.count("Tidying")
	//   Profiler.notime(me, "Tidy"){ s.tidy(me) }
	//   search = null // or else we'll execute the previous search again!
	// }

	case Scheduler.None => { 
	  done = true; 
	  // if(SearchParams.loops) 
	  //   Profiler.notime(me, "Divs sort"){ Divergences.sort(me) }
	}
      } // end of pending.get match
      
      iter += 1
    }

    log("Worker "+me+" finishing")
  } // end of apply

}


