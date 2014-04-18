/** Object recording which nodes are in tau-loops */

import scala.collection.mutable.ArrayBuffer

object Divergences{
  private val p = SearchParams.p

  /** The divergent nodes found, indexed by the worker that found them */
  private var divs = Array.fill(p)(ArrayBuffer[Int]())

  /** Record that n is divergent */
  def add(me: Int, n: ParSearchNode) : Unit = Profiler.notime(me,"Divs.add1"){ 
    if(!n.divergent){ n.setDivergent; divs(me) += n.node }
  }

  /** Record that each of ns is divergent */
  def add(me: Int, ns: Seq[ParSearchNode]) : Unit = 
    Profiler.notime(me, "Divs.add"){
      ns.foreach(add(me, _))
    // ns.foreach(_.divergent = true); divs(me) ++= ns.map(_.node)	
    }

  /** Get all the ids of divergent nodes, sorted, without repetitions
    * Pre: the individual entries of fivs are sorted*/
  def sortedget : ArrayBuffer[Int] = {
    val total = divs.map(_.length).sum // total # divs
    // println(total)
    val allDivs = new ArrayBuffer[Int](total)
    // val allDivs = new Array[Int](total); var next = 0; 
    val nexts = new Array[Int](p)
    // For each i, divs[0..nexts(i)) have been copied into allDivs, in order.
    var last = -1; var copied = 0 
    // last was the last value copied (or -1); copied items have been dealt
    // with
    while(copied<total){
      // Find next value to merge
      var min = Int.MaxValue; var minIx = -1  // Min value and position found
      var i = 0
      while(i < p){
	// Scan over duplicates of last
	// while(nexts(i) < divs(i).length && divs(i)(nexts(i)) == last){
	//   nexts(i) += 1; copied += 1
	// }
	if(nexts(i) < divs(i).length && divs(i)(nexts(i)) < min){
	  min = divs(i)(nexts(i)); minIx = i
	}
	i += 1
      }
      // Copy it across
      if(minIx>=0){
	if(min != last){ allDivs += min; last = min }
	nexts(minIx) += 1; copied += 1
	/* allDivs(next) = min; next += 1; */ 
      }
    }
    // println(next); println(allDivs.distinct.length)
    allDivs // .take(next)
  }

  /** Get all the ids of divergent nodes */
  def get : ArrayBuffer[Int] = divs.foldLeft(ArrayBuffer[Int]())(_ ++ _)

// {
//     val allEls = divs.foldLeft(ArrayBuffer[Int]())(_ ++ _)
//     allEls.distinct.sorted
//   }

  /** Sort the divergences found by me */
  def sort(me: Int) = divs(me) = divs(me).sorted

  /** Clear, ready for a new search */
  def clear = divs = Array.fill(p)(ArrayBuffer[Int]())
}
