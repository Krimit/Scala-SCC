/** A node reached during the search 
  * @param node the node's identity
  * @param index the node's index in the search
  * @param succs the successors of the node */
class SearchNode(val node: Int, val index: Int, val succs: Array[Int]){
  private val N = succs.length
  private var nextIx = 0 // index of next successor

  /** Get next successor, or -1 if there is no such */
  def next = if(nextIx==N) -1 else{ nextIx += 1; succs(nextIx-1) }

  /** Is this node complete */
  def complete = nextIx==N

  /** Is this node in the stack */
  var inStack = true

  /** Low-link of this node */
  var lowlink = index
  
  /** Update lowlink to take account of ix */
  def updateLowlink(ix: Int) = lowlink = lowlink min ix

  /** Is this node in a tau-loop (if we're looking for tau-loops) */
  var divergent = false

  /** Is any child divergent? */
  // def divChild : Boolean = succs.exists(_.divergent)

  override def toString = node.toString
}
  
