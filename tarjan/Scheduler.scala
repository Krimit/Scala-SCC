import scala.collection.mutable.{Set,Map}

/** An object to schedule a new search
  * @param p the number of workers */

class Scheduler(p: Int){

  val pending = SearchParams.pending

  // @volatile private var done = false // becomes true when search terminated

  /** number of workers waiting trying to do a get */
  // private var trying = 0

  /** Lock to protect trying */
  // private val tryingLock = new AnyRef

  /** Lock used within get */
  // We don't want the main loop in get to permanently lock this object, to
  // allow other threads to make progress
  // private val lock = new AnyRef

  val stealingQueue = SearchParams.stealingQueue

  private val MINDELAY = 1 // in nanoseconds
  private val MAXDELAY = 500000 // effective max = 0.5ms

  val random = new scala.util.Random(Scheduler.random.nextInt)

  /** Flags used in the termination protocol; worker w sets bit w */
  private val flags = new java.util.concurrent.atomic.AtomicInteger
  /** Value of flags when all set */ 
  private val ALLFLAGSSET = (1 << p) - 1

  /** Try to get a new search */
  def get(worker: Int, log: (=> String) => Unit) : Scheduler.GetResult = {
    val myMask = 1 << worker // the flag for this workers 
    flags.set(0) // clear all flags
    var needToSet = true // do we set our flag if the next iteration is
			 // unsuccessful?
    var count = 0 // number of iterations of loop below
    var delay = MINDELAY
    var done = false
    // Profiler.count("Scheduler.get")
    while(!done){
      // Try to get pending search
      val s = Profiler.notime(worker,"getPending"){ pending.getPending }
      if(s!=null){log("getPending returned "+s); return Scheduler.Resume(s)}
      // Now try the stealing queue
      val node = 
	Profiler.notime(worker,"stealingQueue.get"){stealingQueue.get(worker)}
      if(node != null) return Scheduler.Branched(node)
      // Now try tidying.  Actually, this seems to slow things down a bit,
      // perhaps because the "return rate" (about one edge per 6 attempts
      // at tidying) is low, so doesn't cover the overheads. 
      // if(trying>2){ // random heuristic
      //   val search = TidyingQueue.get
      //   if(search != null) return Scheduler.Tidy(search)
      // }
      // Give some feedback
      if(count%10000000==0 && count<200000000) log("Trying to steal")
      if(count<0) sys.exit()
      count += 1
      // Set flag if we've done an iteration since we last saw it cleared
      Profiler.notime(worker,"Scheduler - termination"){
	if(needToSet){ 
	  var flagSet = false // have we set the flag yet?
	  do{
	    val f = flags.get; val newf = f | myMask
	    flagSet = f==newf || flags.compareAndSet(f, newf)
	  }while(!flagSet)
	  needToSet = false 
	} else needToSet = (flags.get & myMask) == 0
	done = flags.get == ALLFLAGSSET
      }
      // Binary back-off, not noticably useful; but some delay helps
      if(!done){ 
	Profiler.notime(worker,"Scheduler - back-off"){
	  if(false){
	    if(count==1) Profiler.count("Scheduler.get failed")
	    else Profiler.count("Scheduler.get failed again")
	  }
	  Thread.sleep(0, random.nextInt(200))
	  // Thread.sleep(delay/1000000, delay%1000000) // delay ns
	  // if(delay<MAXDELAY) delay += delay 
	}
      }
    } // end of while
    Scheduler.None
  }

}

// TODO: move return values to this's companion object
object Scheduler{
  val random = new scala.util.Random


  /** A result that can be obtained by doing a get */
  trait GetResult

  /** Branched off to child */
  case class Branched(child: ParSearchNode) extends GetResult

  /** Branch off to child with identity id */
  

  /** Resume a previously suspended search */ 
  case class Resume(search: Search) extends GetResult

  /** Help another search by tidying it */
  case class Tidy(search: Search) extends GetResult

  /** No edge is available, because the stack is empty */
  case object None extends GetResult
}


  // /** Try to get a new search */
  // def get(worker: Int, log: (=> String) => Unit) : Scheduler.GetResult = {
  //   val myMask = 1 << worker 
  //   // tryingLock.synchronized{ trying += 1 }
  //   // Trying.inc
  //   //lock.synchronized{ 
  //   // Profiler.notime(worker, "getCR"+trying+StealingQueue.isEmpty) {
  //     // log("In get CR")
  //     // tryingLock.synchronized{ trying -= 1 }
  //     // Trying.dec
  //     // Clear flags
  //     // for(i <- 0 until p) flags.set(i, 0)
  //     flags.set(0)
  //     var needToSet = true // do we set our flag if the next iteration is
  // 			   // unsuccessful? 
  //     var count = 0 // number of iterations of loop below
  //     var delay = MINDELAY
  //     Profiler.count("Scheduler.get")
  //     while(!done){
  // 	// Profiler.count("Scheduler.get")
  // 	// If all others are trying, and this round doesn't succeed, we're done
  // 	// val lastTry = tryingLock.synchronized{ trying } == p-1
  // 	// Try to get pending search
  // 	val s = Profiler.notime(worker,"getPending"){ getPending }
  // 	if(s!=null){log("getPending returned "+s); return Scheduler.Resume(s)}
  // 	// Now try the stealing queue
  // 	val node = 
  // 	  Profiler.notime(worker,"StealingQueue.get"){
  // 	    StealingQueue.get(worker)
  // 	  }
  // 	if(node != null) return Scheduler.Branched(node)
  // 	// Now try tidying.  Actually, this seems to slow things down a bit,
  // 	// perhaps because the "return rate" (about one edge per 6 attempts
  // 	// at tidying) is low, so doesn't cover the overheads. 
  // 	// if(trying>2){ // random heuristic
  // 	//   val search = TidyingQueue.get
  // 	//   if(search != null) return Scheduler.Tidy(search)
  // 	// }
  // 	// Give some feedback
  // 	if(count%10000000==0 && count<200000000) log("Trying to steal")
  // 	if(count<0) sys.exit()
  // 	// if(count==0) Profiler.count("Scheduler.get -- empty, first time")
  // 	// Profiler.count("Scheduler.get -- empty")
  // 	// Profiler.count("Suspended"+SearchParams.suspended.size)
  // 	if(count==0) Profiler.count("Scheduler.get failed")
  // 	count += 1
  // 	// TODO: at this point, try helping some suspended/inProgress searches
  // 	// by seeing if any unexplored edge leads to a complete node
  // 	// Set flag if we've done an iteration since we last saw it cleared
  // 	if(needToSet){ 
  // 	  var flagSet = false // have we set the flag yet?
  // 	  do{
  // 	    val f = flags.get; val newf = f | myMask
  // 	    flagSet = f==newf || flags.compareAndSet(f, newf)
  // 	  }while(!flagSet)
  // 	  needToSet = false 
  // 	}
  // 	else needToSet = (flags.get & myMask) == 0

  // 	done = flags.get == ALLFLAGSSET
  // 	// done = lastTry
  // 	// Binary back-off, not noticably useful
  // 	if(!done){ 
  // 	  Thread.sleep(delay/1000000, delay%1000000) // delay ns
  // 	  if(delay<MAXDELAY) delay += delay 
  // 	}
  //     } // end of while
  //   // } // end of timing
  //   // } // end of syncronized block
  //   // The search failed
  //   // println("done")
  //   Scheduler.None
  // }
