// import ox.CSO._
import java.io._

// Concurrent search of a graph for a cycle

object ParCheck{
  def main(args: Array[String]) = {
    // parse arguments
    val usage = 
      "Usage: scala ParCheck\n"+
      "  (--random n p | --forward n | --oneRev n i j | \n"+
      "   --multipleLoops n k |\n"+
      "   --solitaire rows cols r0 c0 target |\n"+
      "   --wheel spokes arcLength | --fromFile f)\n"+
      "  [--expectTrue | --expectFalse] [--log | --logToScreen]\n"+
      "  [--unrooted] [--lockFree] [--loops | --lassos]\n"+
      "  [-N N] [-p p]"   
    var g : Graph = null
    var i = 0
    var N = 1 // number of checks to run
    var p = 8 // number of workers to use
    var rooted = true // does the search start at a root?
    var loops = false // are we looking for loops (including lassos)?
    var lassos = false // are we looking for lassos?
    var expectTrue = false; var expectFalse = false // do we expect results
    var logging = false; var logToScreen = false // are we logging (to screen)?
    var randN = -1; var randp = 0.0f // parameters in case of random graph
    var lockFree = false

    while(i<args.length){
      // choice of graph
      if(args(i)=="--random"){ 
	randN = args(i+1).toInt; randp = args(i+2).toFloat; i += 3
	// g = Graph.random(args(i+1).toInt, args(i+2).toFloat); i += 3
      }
      else if(args(i)=="--forward"){
	g = Graph.fullNonDiv(args(i+1).toInt); i += 2
      }
      else if(args(i)=="--oneRev"){
	g = Graph.oneRev(args(i+1).toInt, args(i+2).toInt,args(i+3).toInt);
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
      // other parameters
      else if(args(i)=="-N"){ N = args(i+1).toInt; i += 2 }
      else if(args(i)=="-p"){ p = args(i+1).toInt; i += 2 }
      else if(args(i)=="--unrooted"){ rooted = false; i += 1 }
      else if(args(i)=="--loops"){ loops = true; i += 1 }
      else if(args(i)=="--lassos"){ loops = true; lassos = true; i += 1 }
      else if(args(i)=="--expectTrue"){ expectTrue = true; i += 1 } 
      else if(args(i)=="--expectFalse"){ expectFalse = true; i += 1 } 
      else if(args(i)=="--log"){ logging = true; i += 1 }
      else if(args(i)=="--logToScreen"){ logToScreen = true; i += 1 }
      else if(args(i)=="--lockFree"){ lockFree = true; i += 1 }
      else sys.error(usage)
    }
    assert(g!=null || randN>0)      

    // Now create a solver and run it
    Profiler.setWorkers(p)
    println("Starting timer")
    val t0 = java.lang.System.currentTimeMillis()
    for(i <- 0 until N){
      val writer = if(logging) new PrintWriter(new File("log.txt" )) else null
      def log(st: => String) = 
	if(logging) writer.write(st+"\n") else if(logToScreen) println(st)
      if(i!=0) log("--------------------------------------------------------")
      if(randN > 0) Profiler.time(0,"make graph"){ 
	g = Graph.random(randN, randp) // new random graph
      }
      val solver = new ParSolver(g, p, rooted, loops, lassos, lockFree, log)
      // println("Solving")
      val result = 
	try{ solver.solve } 
	finally{ if(logging){ writer.flush(); writer.close() } }
      if(i==0 || randN > 0){
	println("Result:\n")
	result match{
	  case Left(sccs) => {assert(!loops); println(sccs.toStringSummary)}
	  case Right(divs) => {
	    assert(loops); 
	    if(divs.length<=100) println(divs) 
	    else println(divs.length+" loop/lasso states")
	  }
	}
      }
      else if(i%20==0) println(i)

      // if(loop){ println("Loop found"); if(expectFalse)(sys.exit()) }
      // else{ println("No loop found"); if(expectTrue)(sys.exit()) }
    }
    println("Time taken: "+(java.lang.System.currentTimeMillis()-t0))
    Profiler.report
  }

}
