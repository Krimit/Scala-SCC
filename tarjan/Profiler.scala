/** Some profiling functions */

/** The object implements a number of named timers and named counters.  Code 
  * can be timed, with the time taken added to that of the named timer.  
  * Similarly, the number of times particular points in the code are reached
  * can be counted, with the count recorded against a named counter. */
object Profiler{
  private val MAX = 1000 // max number of timers
  private val tnames = new Array[String](MAX) // names of timers
  private val times = new Array[Long](MAX) // their values
  private var tCount = 0 // number of timers
  // This represents the mapping {tnames(i) -> times(i) | i <- [0..tCount)}

  /** Time cmd, recording the duration against timer tname */
  def time[A](tname:String)(cmd: => A) : A = {
    // Find the index for the timer tname
    var i = 0
    synchronized{
      while(i<tCount && tnames(i)!=tname) i += 1
      if(i==tCount){ tnames(i) = tname; tCount += 1; times(i) = 0 }
    }
    // start the timer
    val t0 = java.lang.System.currentTimeMillis()
    // run the code
    try{ cmd } finally{
      // stop the timer and record the duration
      val duration = java.lang.System.currentTimeMillis()-t0
      synchronized{ times(i) += duration }
    }
  }

  // The above works well with single-threaded code, and the code below is
  // thread-safe.  However, it can cause a lot of contention.  The following
  // is designed for use with multi-threaded code.
  var p = 0 // number of threads
  private var ctnames = Array.ofDim[String](p,MAX) // names of timers
  private var ctimes = Array.ofDim[Long](p,MAX) // their values
  private var ctCount = new Array[Int](p) // number of timers for each thread
  
  /** set the number of concurrent workers */
  def setWorkers(p: Int) = {
    this.p = p
    ctnames = Array.ofDim[String](p,MAX)
    ctimes = Array.ofDim[Long](p,MAX)
    ctCount = new Array[Int](p)
    ccnames = Array.ofDim[String](p,MAX)
    ccounts = Array.ofDim[Int](p,MAX) 
    ccCount = new Array[Int](p)  
  }

  /** Time cmd, recording the duration against timer tname for worker w
   *
   * Note: this is NOT thread safe. */
  def time[A](w: Int, tname:String)(cmd: => A) : A = {
    // Find the index for the timer tname
    var i = 0
    while(i<ctCount(w) && ctnames(w)(i)!=tname) i += 1
    if(i==ctCount(w)){ 
      ctnames(w)(i) = tname; ctCount(w) += 1; ctimes(w)(i) = 0 
    }
    // start the timer
    val t0 = java.lang.System.currentTimeMillis()
    // run the code
    try{ cmd } finally {
      // stop the timer and record the duration
      val duration = java.lang.System.currentTimeMillis()-t0
      ctimes(w)(i) += duration 
    }
    // val res = cmd
    // // stop the timer and record the duration
    // val duration = java.lang.System.currentTimeMillis()-t0
    // ctimes(w)(i) += duration 
    // res
  }

  @inline def notime[A](w: Int, tname:String)(cmd: => A) : A = cmd
  @inline def notime[A](tname:String)(cmd: => A) : A = cmd

  private val cnames = new Array[String](MAX) // names of counters
  private val counts = new Array[Int](MAX)   // their values
  private var cCount = 0 // number of counters
  // This represents the mapping {cnames(i) -> counts(i) | i <- [0..cCount)}

  /** Increment the counter cname */
  def count(cname:String) = synchronized{
    // Find the index for the counter cname
    var i = 0
    while(i<cCount && cnames(i)!=cname) i += 1
    if(i==cCount){ cnames(i) = cname; cCount += 1; counts(i) = 0 }
    // record the occurence
    counts(i) += 1
  }

  private var ccnames = Array.ofDim[String](p,MAX) // names of counters
  private var ccounts = Array.ofDim[Int](p,MAX) // their values
  private var ccCount = new Array[Int](p) // number of counters for each thread

  /** Increment the counter cname for worker w
   *
   * Note: this is NOT thread safe. */
  def count(w: Int, cname: String) = {
    // Find the index for the counter cname
    var i = 0
    while(i<ccCount(w) && ccnames(w)(i)!=cname) i += 1
    if(i==ccCount(w)){ 
      ccnames(w)(i) = cname; ccCount(w) += 1; ccounts(w)(i) = 0
    }
    ccounts(w)(i) += 1
  }

  /** Produce report of all timers and counters, and reset */
  def report = synchronized{
    // (timer name, time) pairs
    val tPairs0 = 
      (for(i <- 0 until tCount) yield (tnames(i), times(i))) ++
      (for(w <- 0 until p; i <- 0 until ctCount(w)) 
       yield (ctnames(w)(i), ctimes(w)(i)))
    // (counter name, count) pairs
    val cPairs0 = 
      (for(i <- 0 until cCount) yield (cnames(i), counts(i))) ++
      (for(w <- 0 until p; i <- 0 until ccCount(w))
       yield (ccnames(w)(i), ccounts(w)(i)))

    // Merge pairs with same name; pre: list is sorted. 
    def mergePairs[A : Numeric](pairs: Seq[(String, A)]) : Seq[(String, A)] = 
      if(pairs.isEmpty) pairs
      else{
	val (st,n) = pairs.head
	val (matches, others) = pairs.span(_._1==st)
	(st, matches.map(_._2).sum) +: mergePairs(others)
      }
    val tPairs : Seq[(String,Long)] = mergePairs(tPairs0.sorted)
    val cPairs : Seq[(String,Int)] = mergePairs(cPairs0.sorted)

    // max width of names
    val maxW = 
      if(tPairs.isEmpty && cPairs.isEmpty) -1
      else ( (for((tn,_) <- tPairs) yield tn.length) ++
             (for((cn,_) <- cPairs) yield cn.length) ) .max

    if(tPairs.nonEmpty){
      println("TIMERS:")
      for((tn,t) <- tPairs) println(tn+": "+(" "*(maxW-tn.length))+t)
      println
    }
    if(cPairs.nonEmpty){
      println("COUNTERS:")
      for((cn,c) <- cPairs) println(cn+": "+(" "*(maxW-cn.length))+c)
    }
    // Reset here?

  }

}
