/** A trait representing mappings from Ints to corresponding ParSearchNodes */

trait NodeTable{
  /** Clear the table */
  def clear : Unit

  /** Get the node associated with n, or create a new node and return it */
  def getOrInit(n: Int) : ParSearchNode

  /** Get the node associated with n, or create a new node and return it;
    * also return a boolean saying whether the node is new */
  def getOrInitFlagged(n: Int) : (ParSearchNode, Boolean)
}

// -------------------------------------------------------

/** A table of fixed size.
  * @param size the size of the table; must be at least the number of nodes
  * to be stored, preferably somewhat larger. */

class ConcurrentStaticNodeTable(size: Int) extends NodeTable{ 
  /** Array holding the keys */
  /* @volatile */ private var keys = Array.fill(size)(-1)
  // Keys is volatile to prevent a perverse reordering of code that would
  // break getOrInit.  I'm probably being paranoid here. 

  /** Array holding the nodes */
  private var nodes = new Array[ParSearchNode](size)

  // This represents the mapping 
  // { keys(i) -> nodes(i) | i <- [0..size), keys(i) != -1 }.

  // Each entry with key n is placed as close as posible after position
  // n%size; i.e., it is placed in the first unused position after this
  // (looping round).

  // The entries in position i are protected by locks(i%NUMLOCKS)
  private val NUMLOCKS = 293
  private val locks = Array.fill(NUMLOCKS)(new AnyRef)

  /** Get the node associated with n, or create a new node and return it */
  def clear = for(i <- 0 until size){ keys(i) = -1; nodes(i) = null }

  /** Get the node associated with n, or create a new node and return it;
    * also return a boolean saying whether the node is new */
  def getOrInit(n: Int) : ParSearchNode = {
    var i = n%size; var done = false
    while(!done){
      // Note the order of checking branches is important, in case of a race
      // with another thread writing in the same key.
      if(keys(i) == -1) locks(i%NUMLOCKS).synchronized{
	// recheck keys(i), in case of race conditions, or if we've read a
	// stale -1.
	if(keys(i) == -1){ 
	  keys(i) = n; nodes(i) = new ParSearchNode(n); done = true
	}
	// else go round the loop and check this location again
      }
      else if(keys(i) == n) // make sure the write to nodes(i) is finished
	locks(i%NUMLOCKS).synchronized{ done = true }
      else i = (i+1)%size
    }
    nodes(i)
  }

  /** Get the node associated with n, or create a new node and return it;
    * also return a boolean saying whether the node is new */
  def getOrInitFlagged(n: Int) : (ParSearchNode, Boolean) =
    throw new UnsupportedOperationException
}
