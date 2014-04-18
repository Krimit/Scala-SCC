/** Experiments using randomly generated graphs */

object RandomExperiment extends ExperimentBase{

  // def log(st: => String) = { } // dummy logging function

  val N = 200000 // # states
  var prob0 = 5e-7 // smallest p to consider
  var prob1 = 1.4e-5 // max p to consider
  var pStep = 5e-7 // step between p values

  // The ith probability to use
  def getP(i: Int) : Float = (prob0 + i*pStep).toFloat
  
  override def name(i: Int) = getP(i).toString
  override def states(i: Int) = N

  def main(args: Array[String]) = {
    // parse command line args
    var i = 0
    val usage =
      "Usage: scala [-J-Xmx s] RandomExperiment [--loops | --lassos | --all]\n"+
      "        [--iters n] [--concOnly] [--noWarmUp] [-p p] [--verbose] [--latex]"
    while(i<args.length){
      if(args(i)=="--iters"){ iters = args(i+1).toInt; i += 2 }
      else if(args(i)=="--verbose"){ verbose = true; i += 1 }
      else if(args(i)=="--loops"){ loops = true; i += 1 }
      else if(args(i)=="--lassos"){ lassos = true; i += 1 }
      else if(args(i)=="--all"){ all = true; i += 1 }
      else if(args(i)=="--latex"){ latex = true; i += 1 }
      else if(args(i)=="--concOnly"){ includeSeq = false; i += 1 }
      else if(args(i)=="--noWarmUp"){ warmUp = false; i += 1 }
      else if(args(i)=="-p"){ p = args(i+1).toInt; i += 2 }
      else if(args(i)=="--prob0"){ prob0 = args(i+1).toFloat; i += 2 }
      else if(args(i)=="--prob1"){ prob1 = args(i+1).toFloat; i += 2 }
      else if(args(i)=="--pStep"){ pStep = args(i+1).toFloat; i += 2 }
      else sys.error(usage)
    }

    Profiler.setWorkers(p)

    // Initialise tables, etc.
    numExperiments = ((prob1-prob0)/pStep + 1).toInt
    nameFieldSize = (0 to numExperiments).map(name(_).length).max
    val theSearchTypes = getSearchTypes
    initTables(theSearchTypes.length)

    // Run the experiments
    for(iter <- 0 until iters){
      val include = iter>0 || ! warmUp // include this iter in the stats?
      for(i <- 0 until numExperiments){
	val prob = getP(i)
	val g = Graph.random(N, prob) // the graph to try
	runExperiment(g, i, theSearchTypes, include, include)
      }
      println
    }

    // Print results
    showStateStats(if(warmUp) iters-1 else iters)
    printResults; Profiler.report
  }
}


    // for(iter <- 0 until iters){
    //   for(i <- 0 until numExperiments){
    // 	val p = getP(i)
    // 	if(verbose) print("p = "+p)
    // 	val g = Graph.random(N,p) // the graph to try
    // 	// Run concurrent algorithm
    // 	System.gc
    // 	val t0 = java.lang.System.currentTimeMillis()
    // 	val result = new ParSolver(g, pp, isRooted, lockFree, log).solve
    // 	// TODO: record size of max component
    // 	val time0 = java.lang.System.currentTimeMillis() - t0
    // 	if(iter>0) concTimes(i) += time0
    // 	if(verbose) print("  "+time0)
    // 	// Run sequential algorithm
    // 	val seen = SearchParams.seen
    // 	if(seen != null) seen.clear
    // 	System.gc
    // 	val t1 = java.lang.System.currentTimeMillis()
    // 	SeqCheck.solve(g, isRooted, log)
    // 	val time1 = java.lang.System.currentTimeMillis() - t1
    // 	if(verbose) println("\t"+time1)
    // 	if(iter>0) seqTimes(i) += time1
    //   }
    //   println
    // }

    // Print results
    //val searchResultTypes = getSearchResultTypes
      // if(loops) List("Loops")
      // else if(lassos) List("Lassos")
      // else if(all) List("SCCs", "Loops", "Lassos")
      // else List("SCCs")
    // printResults //(searchResultTypes)
    // Profiler.report

    // val countedIters = iters-1
    // println("\nResults from "+countedIters+" iterations:\n")
    // println("p\tConc\tSeq\tratio")
    // for(i <- 0 until numExperiments) 
    //   println(getP(i)+"\t"+concTimes(i)/countedIters+"\t"+
    // 	      seqTimes(i)/countedIters+"\t"+
    // 	      (seqTimes(i).toFloat/concTimes(i)))
