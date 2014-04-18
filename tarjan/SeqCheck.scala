import java.io._
import scala.collection.mutable.ArrayBuffer
/** Sequentially find SCCs of a graph */

object SeqCheck{
  
  /** Explore graph g for cycles 
    * @param rooted does the search start at the initial node of g?
    * @param loops are we searching for loops (including loops)?
    * @param lassos are we searching for lassos?
    * @return either the SCCS (if !loops), or the nodes on loops. */
  def solve(g: Graph, rooted: Boolean, loops: Boolean, lassos: Boolean,
	    log: (=> String) => Unit) : Either[SCCSet, ArrayBuffer[Int]]
  = {
    // The nodes we've seen so far
    val seen = new scala.collection.mutable.HashMap[Int, SearchNode]
    // The current search/control stack
    val stack = new scala.collection.mutable.Stack[SearchNode]
    // The current SSC stack
    val tarjanStack = new scala.collection.mutable.Stack[SearchNode]
    // The current index
    var index = 0
    // Create a new SearchNode and add it to seen and both stacks
    def addNode(n: Int) = {
      val sn = Profiler.notime("new node"){
	new SearchNode(n, index, g.succs(n))}
      Profiler.notime("seen"){seen += (n -> sn)}; 
      stack.push(sn); tarjanStack.push(sn)
      index += 1
    }
    // addNode(g.init) 

    // The SCCs found so far
    var sccs = new SCCSet 
    // The nodes on loops found so far
    var divs = new ArrayBuffer[Int]()

    /** Record w as being divergent */
    def makeDivergent(w : SearchNode) = 
      if(!w.divergent){ w.divergent = true; divs += w.node }

    /** Mark all nodes in the stack as divergent; unblock any worker blocked
     * on them.
     * Pre: lassos (i.e. we are indeed looking for divergences) */
    def makeAllDivergent = {
      assert(lassos)
      var st = tarjanStack.toList
      log("Marking "+st+" as divergent")
      while(st.nonEmpty && ! st.head.divergent){
	makeDivergent(st.head) // mark st.head as divergent
	st = st.tail
      }
    }

    /** Run Tarjan's Algorithm, starting from start */
    def strongConnect(start: Int) = {
      addNode(start)
      while(stack.nonEmpty){
	val parent : SearchNode = stack.top
	val next : Int = parent.next
	// Profiler.count("state")
	if(next == -1){ // we've explored all successors of parent
	  log("Backtracking from "+parent+"; "+parent+".index = "+parent.index+
	      "; "+parent+".lowlink = "+parent.lowlink)
	  stack.pop
	  // See if a successor is divergent
	  // TODO: remove following
	  // if(lassos && parent.succs.exists(seen(_).divergent))
	  //   makeDivergent(parent)
	  if(stack.nonEmpty) stack.top.updateLowlink(parent.lowlink) 
	  if(parent.lowlink == parent.index){
	    if(!loops){
	      var w = tarjanStack.pop; w.inStack = false
	      var scsg = List[Int](w.node)
	      while(w != parent){ 
		w = tarjanStack.pop; w.inStack = false; scsg ::= w.node 
	      }
	      log("SCC found: "+scsg)
	      Profiler.notime("adding scc"){ sccs.add(scsg.toSet, 0) }
	    }
	    else if(lassos){
	      // Remove  all nodes in this SCC from the stack
	      var w = tarjanStack.pop; w.inStack = false
	      while(w != parent){ 
		w = tarjanStack.pop; w.inStack = false; 
		log("Marking "+w+" as complete")
	      }
	    }
	    else{ // loops but not lassos
	      var w = tarjanStack.pop; w.inStack = false
	      var scsg = List[SearchNode](w)
	      while(w != parent){ 
		w = tarjanStack.pop; w.inStack = false; scsg ::= w
	      }
	      log("SCC found: "+scsg)
	      if(scsg.size > 1) scsg.foreach(makeDivergent)
	    }
	  } // end of if(parent.lowlink == parent.index)
	} // end of if(next == -1)
        else if(Profiler.notime("seen"){seen.contains(next)}){
	  val nextN = Profiler.notime("seen"){seen(next)}
	  if(nextN.inStack){ 
	    log("Loop found via edge "+parent+" -> "+next)
	    if(lassos) makeAllDivergent // ***
	    else if(loops && parent==nextN) makeDivergent(nextN)
	    else parent.updateLowlink(nextN.index)
	  }
	  else{ 
	    log("Expanding "+parent+" -> "+next+"; "+
		(if(lassos && nextN.divergent) "divergent" else "done")+
		" node "+next+" seen again")
	    if(lassos && nextN.divergent) makeAllDivergent // ***
	    // otherwise nothing more to do with next
	  }
	} // end of if(seen.contains(next))
	else{
	  log("Expanding "+parent+" -> "+next+"; "+
	      next+" seen for first time, index = "+index)
	  addNode(next) // next is a new node; continue with it
	}
      } // end of while
    } // end of StrongConnect

    // Now either run the algorithm from the start node (if rooted) or from
    // all nodes
    if(rooted) strongConnect(g.init)
    else for(n <- g.nodes; if ! seen.contains(n)) strongConnect(n)

    if(!loops) new Left(sccs) else Right(divs) // .sorted
  }

  def main(args: Array[String]) = {
    // parse arguments
    val usage = 
      "Usage: scala ParCheck\n"+
      "  (--random n p | --forward n | --oneRev n i j |\n"+
      "   --multipleLoops n k |\n"+
      "   --solitaire rows cols r0 c0 target |\n"+
      "   --wheel spokes arcLength | --fromFile f)\n"+
      "  [-N N] [--loops | --lassos]"
    var g : Graph = null
    var i = 0
    var N = 1 // number of checks to run
    var rooted = true // does the search start at a root?
    var loops = false // are we looking for loops (inclusing lassos)?
    var lassos = false // are we looking for lassos?
    var randN = -1; var randp = 0.0f // parameters in case of random graph
    while(i<args.length){
      if(args(i)=="--random"){ 
	randN = args(i+1).toInt; randp = args(i+2).toFloat; i += 3
	// g = Graph.random(args(i+1).toInt, args(i+2).toFloat); i += 3
      }
      else if(args(i)=="--forward"){
	g = Graph.fullNonDiv(args(i+1).toInt); i += 2
      }
      else if(args(i)=="--oneRev"){
	g = Graph.oneRev(args(i+1).toInt, args(i+2).toInt,args(i+3).toInt)
	i += 4
      }
      else if(args(i)=="--solitaire"){
	g = 
	  new SolitaireGraph(args(i+1).toInt, args(i+2).toInt, 
			     args(i+3).toInt, args(i+4).toInt, args(i+5).toInt)
	i += 6
      }
      else if(args(i)=="--multipleLoops"){
	g = Graph.multipleLoops(args(i+1).toInt, args(i+2).toInt)
	i += 3
      }
      else if(args(i)=="--rectangularGrid"){
	g = Graph.rectangularGrid(args(i+1).toInt, args(i+2).toInt)
	i += 3
      }
      else if(args(i)=="--independentLoops"){
	g = Graph.independentLoops(args(i+1).toInt, args(i+2).toInt)
	i += 3
      }
      else if(args(i)=="--wheel"){
	g = new WheelGraph(args(i+1).toInt, args(i+2).toInt, args(i+3).toInt)
	i += 4
      }
      else if(args(i)=="--fromFile"){
	g = GraphFromTauDump(args(i+1)); i += 2
      }
      // Other parameters
      else if(args(i)=="-N"){ N = args(i+1).toInt; i += 2 }
      else if(args(i)=="--unrooted"){ rooted = false; i += 1 }
      else if(args(i)=="--loops"){ loops = true; i += 1 }
      else if(args(i)=="--lassos"){ loops = true; lassos = true; i += 1 }
      else sys.error(usage)
    }
    assert(g!=null || randN>0)

    val logging = false

    println("Starting timer")
    val t0 = java.lang.System.currentTimeMillis()
    for(i <- 0 until N){
      val writer = 
	if(logging) new PrintWriter(new File("seqlog.txt" )) else null
      def log(st: => String) = 
	if(logging) writer.write(st+"\n") // else if(logToScreen) println(st)
      if(i!=0) log("--------------------------------------------------------")
      if(randN > 0) g = Graph.random(randN, randp) // new random graph
      val result = 
	try{ solve(g, rooted, loops, lassos, log) } 
	finally{ if(logging){ writer.flush(); writer.close() } }
      if(i==0 || randN > 0){
	println("Result:\n")
	result match{
	  case Left(sccs) => {assert(!loops); println(sccs.toStringSummary)}
	  case Right(divs) => {assert(loops); println(divs)}
	}
      }
      else if(i%20==0) println(i)
    }
    println("Time taken: "+(java.lang.System.currentTimeMillis()-t0))
    Profiler.report
  }




}
