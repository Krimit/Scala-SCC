import scala.collection.mutable.ArrayBuffer

/** Objects of this class look for loops in graph g, using p workers
  * @param g the graph to be searched
  * @param p the number of workers to use
  * @param rooted is the search rooted
  * @param lockFree do we use lock-free queues?
  * @param loops are we searching for loops (including lassos)?
  * @param lassos are we searching for lassos?
  * @param log a logging function.
  */

class ParSolver(
  g: Graph, p: Int, rooted: Boolean, loops: Boolean, lassos: Boolean,
  lockFree: Boolean, log: (=> String) => Unit){

  /** Check graph g using p workers */
  def solve : Either[SCCSet, ArrayBuffer[Int]] = { 
    // println(lassos)
    SearchParams.init(g, p, rooted, loops, lassos, lockFree)
    
    // Release the workers
    // println("Starting workers")
    Profiler.time("Running workers"){
      ox.cads.util.ThreadUtil.runIndexedSystem(p, new Worker(_, log)())
    }

    // Extract the result, and return it
    SearchParams.checkDone
    if(SearchParams.loops) Right(Profiler.notime("Divs"){Divergences.get})
    else Left(SearchParams.sccs)
  }
}
