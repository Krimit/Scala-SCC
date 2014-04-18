import scala.collection.mutable.ArrayBuffer

trait ExperimentBase{
  // Parameters affecting the experiments; most will be settable on the
  // command line
  var iters = 2 // # iterations
  var warmUp = true // do we include a warm-up round? 
  val lockFree = false // lock free data structures?
  val isRooted = false // is the search rooted (no)
  var includeSeq = true // include sequential checks? 
  var p = 8 // # workers
  var verbose = false // Do we give verbose reports?
  var latex = false // are we formatting output for LaTeX
  var loops = false; var lassos = false // are we looking for loops, lassos
  var all = false // are we doing all three experiments?
 
  def log(st: => String) = { } // dummy logging function

  // Arrays to store results of experiments
  var seqTimes, concTimes : Array[Array[Long]] = null
  var nums, superSCCs, trivs, loopNums, divNums : Array[Int] = null
  // Number of experiments
  var numExperiments = 0

  // Table to hold (name, # states, graph) for each experiment
  var experiments = ArrayBuffer[(String,Int,Graph)]() 
  def name(i: Int) : String = experiments(i)._1
  def states(i: Int) : Int = experiments(i)._2
  def graph(i: Int) : Graph = experiments(i)._3
  def numEdges(i: Int) : Int = graph(i).numEdges

  /** Initialise the tables
    * Pre: numExperiments has been set
    * @param numSearchTypes the number of types of searches */
  def initTables(numSearchTypes: Int) = {
    seqTimes = Array.ofDim[Long](numSearchTypes, numExperiments)
    concTimes = Array.ofDim[Long](numSearchTypes, numExperiments)
    // Arrays to store #SCCs, size of super-SCC, #trivial SCCs, #div states
    nums = new Array[Int](numExperiments)
    superSCCs = new Array[Int](numExperiments)
    trivs = new Array[Int](numExperiments)
    loopNums = new Array[Int](numExperiments)
    divNums = new Array[Int](numExperiments)
  }

  /** Get a list of (Boolean, Boolean) pairs, indicating whether we're 
    * looking for loops and lassos.
    * Pre: loops, lassos and all have been set */
  def getSearchTypes : List[(Boolean, Boolean)] = 
    if(loops) List((true,false)) 
    else if(lassos) List((true,true)) 
    else if(all) List((false,false), (true,false), (true,true))
    else List((false,false))

  def getSearchResultTypes : List[String] = 
    if(loops) List("Loops")
    else if(lassos) List("Lassos")
    else if(all) List("SCCs", "Loops", "Lassos")
    else List("SCCs")

  // ----- Printing functions
  var nameFieldSize = 0 // will hold max length of any name + 1

  // Round a Float to 2DP
  def round(f: Float): String  = "%.2f".format(f)

  // Pad st padded with spaces on the right to length nameFieldSize
  def padString(st: String) = ("%-"+nameFieldSize+"s").format(st)
 
  /** Pad the name of experiment i, padded with spaces on the right to length
    * nameFieldSize */
  def showName(i: Int) : String = padString(name(i))

  def printCols(cols: List[Any]) : Unit = {
    val tab = if(latex) " & " else "\t"
    val nl = if(latex) " \\\\" else ""
    println(cols.map(_.toString).mkString(tab)+nl)
  }

  /** Print summary of states, etc. 
    * @param n the number of experiments for which the statistics were 
    * included */
  // FIXME: this assumes that storeStats was true for only one call of 
  // runExperiment for each experiment
  def showStateStats(n: Int) = {
    if(latex) println("\\begin{tabular}{\\|l\\|*{6}{r\\|}}\n\\hline")
    printCols(
      List(padString("Graph"), "States ", "Edges") ++
      (if(all) List("SCCs", "Largest", "Trivial", "Loop", "Lassos")
       else if(lassos && !loops) List("Lasso")
       else if(!loops) List("SCCs", "Largest", "Trivial")
       else List("Loop")) )
    printCols(
      List("  ", "  ", " ") ++
      (if(all) List("   ", "SCC", "SCCs", "states", "states")
       else if(lassos && !loops) List("states")
       else if(!loops) List("  ", "SCC", "SCCs")
       else List("states")) )
    if(latex) println("\\hline")
    def scale(k: Int) = (k + n/2) / n // k/n to nearest int
    for(i <- 0 until numExperiments) 
      printCols(
	List(showName(i), states(i), numEdges(i)) ++
	(if(all) 
	  List(scale(nums(i)), scale(superSCCs(i)), scale(trivs(i)), 
	       scale(loopNums(i)), scale(divNums(i)))
	 else if(lassos && !loops) List(scale(divNums(i)))
	 else // if(!loops) 
	   List(scale(nums(i)), scale(superSCCs(i)), scale(trivs(i))) )
	// FIXME!
      )
    if(latex) println("\\hline\n\\end{tabular}")
    println
  }

  /** Print results */  
  def printResults = {
    val searchResultTypes = getSearchResultTypes
    val countedIters = if(warmUp) iters-1 else iters; 
    val numSearchTypes = searchResultTypes.length
    println("\nResults from "+countedIters+" iterations:\n")
    // Column headings
    if(latex) 
      println("\\begin{tabular}{\\|\\|l\\|*{3}{\\|r\\|r\\|l\\|}\\|}\n\\hline")
    printCols(
      List("") ++ 
      (for(sr <- searchResultTypes) 
       yield if(latex) List("\\multicolumn{3}{c\\|\\|}{"+sr+"}") 
	     else List(" ", sr, " ")
     ).flatten )
    if(latex) println("\\cline{2-"+(1+3*numSearchTypes)+"}")
    printCols(
      List(padString("Graph")) ++ 
      (for(_ <- 0 until numSearchTypes) yield List("Conc", "Seq", "ratio") 
       ).flatten )
    if(latex) println("\\hline")
    // Timing stats
    for(i <- 0 until numExperiments) 
      printCols(
	List(showName(i)) ++
	(for(j <- 0 until numSearchTypes) yield { 
	  val ct = concTimes(j)(i); val st = seqTimes(j)(i)
	  List((ct/countedIters).toInt, (st/countedIters).toInt, 
	       round(st.toFloat/ct) ) 
	} ).flatten  )
    // Totals
    if(latex) println("\\hline")
    printCols(
      List(padString("Total")) ++
      (for(j <- 0 until numSearchTypes) yield {  
	val totalS = seqTimes(j).sum; val totalC = concTimes(j).sum
	List((totalC/countedIters).toString, (totalS/countedIters).toString, 
	     round(totalS.toFloat/totalC))
      } ).flatten )
    if(latex) println("\\hline\n\\end{tabular}")
    println
  }

    /** Run the experiment exp, storing result in ith entry of tables
    * @param searchTypes list of values for (loops,lassos) to use for the 
    * searches
    * @param includeTime is the time included in the timing results?
    * @param storeStats should statistics about number of states, etc, be 
    * stored? */
  def runExperiment(exp: Graph, i: Int, searchTypes: List[(Boolean,Boolean)], 
		    includeTime: Boolean, storeStats: Boolean) 
  = for(ix <- 0 until searchTypes.length){
    val (loops,lassos) = searchTypes(ix)
    // Run concurrent algorithm
    // val exp = graph(i)
    if(verbose) print(showName(i)) else print("/")
    // Clear seen set, then garbage collect results
    Profiler.notime(0,"clear seen"){
      val seen = SearchParams.seen; if(seen != null) seen.clear
    }
    Profiler.notime(0,"gc"){ System.gc }
    val t0 = java.lang.System.currentTimeMillis()
    val result = 
      new ParSolver(exp, p, isRooted, loops, lassos, lockFree, log).solve
    val time0 = java.lang.System.currentTimeMillis() - t0
    if(includeTime) concTimes(ix)(i) += time0
    if(verbose) print("  "+time0) else print("-")
    // Run sequential algorithm
    if(includeSeq){
      System.gc
      val t1 = java.lang.System.currentTimeMillis()
      SeqCheck.solve(exp, isRooted, loops, lassos, log)
      val time1 = java.lang.System.currentTimeMillis() - t1
      if(verbose) println("\t"+time1) else print("\\")
      if(includeTime) seqTimes(ix)(i) += time1
      if(storeStats){ // summarize result
	result match{
	  case Left(sccs) => {
	    assert(!loops)
	    val (num, superSCC, triv) = sccs.summary
	    if(verbose)
	      println(num+" SCCs; largest has size "+superSCC+"; "+
	     	      triv+" trivial SCCs")
	    nums(i) += num; superSCCs(i) += superSCC; trivs(i) += triv
	  }
	  case Right(divs) => { 
	    assert(loops)
	    val numDivs = Divergences.get.length
	    if(verbose) println(numDivs+" divergent states")
	    if(lassos) divNums(i) += numDivs else loopNums(i) += numDivs		
	  }
	}
      }
    } // end of if(includeSeq)
  }

}
