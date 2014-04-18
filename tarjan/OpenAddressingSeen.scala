/** Implementation of Seen based on sharding and open addressing */
import scala.collection.mutable.Queue

object OpenAddressingSeen extends Seen{

  /** Number of shards */
  private val shards = 197

  /** Number of entries in each OpenHashMap */
  private val initSize = 1024

  /** Maximum load factor times 1000 in each OpenHashMap */
  private val maxLoadFactor = 600 // corresponds to 0.6

  /** The individual OpenHashMaps */
  private val tables = Array.fill(shards)(new OpenHashMap)

  @inline private def mapFor(n: Int) = tables(n%shards)

  /** If n is not in the map, initialise it to a new node; otherwise 
    * return the node */
  @inline def getOrInit(n: Int) : ParSearchNode = mapFor(n).getOrInit(n)

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * @return a pair (nodes, newNodes), where:
    *   nodes is the nodes corresponding to ns;
    *   newNodes is the new nodes encountered (in reverse order), excluding
    *     the node that will be explored first by this worker */
  def putAll(me: Int, ns: Array[Int]) 
  : (Array[ParSearchNode], Seq[ParSearchNode]) = {
    val size = ns.size; var nodes = new Array[ParSearchNode](size); var i = 0
    var newNodes = List[ParSearchNode]()
    while(i<size){ 
      val n = ns(i); val (node, isNew) = mapFor(n).getOrInitIsNew(n); 
      nodes(i) = node
      if(i != 0 && isNew) newNodes ::= node 
      i += 1
    }
    (nodes, newNodes)
  }

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in an array */
  def putAllUnrooted(me: Int, ns: Array[Int]) : Array[ParSearchNode] = {
    val size = ns.size; var nodes = new Array[ParSearchNode](size); var i = 0
    while(i<size){ nodes(i) = getOrInit(ns(i)); i += 1 }
    nodes
  }

  /** Clear the map, ready for a new search */
  def clear = {
    var j = 0
    while(j < shards){ tables(j).clear; j += 1 }
  }


  // -------------------------------------------------------

  /** Each object of this class represents a single map from Ints to 
    * ParSearchNodes */ 
  class OpenHashMap{
    /** The table holding the entries */
    private var entries = new Array[ParSearchNode](initSize)

    /** Current size of the table */
    private var tableSize = initSize

    /** Current number of entries */
    private var numEntries = 0

    /** Threshold beyond which we should resize */
    private var threshold = tableSize * maxLoadFactor / 1000

    /** Find the location in entries where a node with id n is or 
      * should be placed */
    @inline private def findPlace(n: Int) : Int = {
      var i = n%tableSize
      while(entries(i) != null && entries(i).node != n) i = (i+1)%tableSize
      i
    }

    /** If n is not in the map, initialise it to a new node; otherwise 
      * return the node */
    def getOrInit(n: Int) : ParSearchNode = synchronized{
      if(numEntries >= threshold) resize
      val i = findPlace(n)
      if(entries(i) == null){
	entries(i) = new ParSearchNode(n); numEntries += 1
      }
      entries(i)
    }

    /** If n is not in the map, initialise it to a new node; otherwise 
      * return the node.  Also return a boolean that says whether the node 
      * is new */
    def getOrInitIsNew(n: Int) : (ParSearchNode, Boolean) = synchronized{
      if(numEntries >= threshold) resize
      val i = findPlace(n)
      if(entries(i) == null){
	entries(i) = new ParSearchNode(n); numEntries += 1; (entries(i), true)
      }
      else (entries(i), false)
    }

    /** Resize the table */
    private def resize = {
      val newSize = tableSize * 2
      val newTable = new Array[ParSearchNode](newSize)
      // Copy nodes from entries to newTable
      var i = 0
      while(i < tableSize){ 
	if(entries(i) != null){
	  val n = entries(i).node; var j = n%newSize
	  while(newTable(j) != null) j += 1
	  newTable(j) = entries(i)
	}
	i += 1
      }
      // Set new table
      entries = newTable; tableSize = newSize; threshold = threshold * 2
    }

    /** Clear this entry */
    def clear = {
      var i = 0
      while(i < tableSize){ entries(i) = null; i += 1 }
      numEntries = 0
    }

  } // End of OpenHashMap



}
