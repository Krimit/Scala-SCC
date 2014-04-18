/** This object basically maintains a Map[Int,ParSearchNode], with 
  * various operations on it.  */
import scala.collection.mutable.Queue

trait Seen{
  /** Clear the map, ready for a new search */
  def clear 

  /** If n is not in the map, initialise it to a new node; otherwise 
    * return the node */
  def getOrInit(n: Int) : ParSearchNode

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * @return a pair (nodes, newNodes), where:
    *   nodes is the nodes corresponding to ns;
    *   newNodes is the new nodes encountered (in reverse order), excluding
    *     the node that will be explored first by this worker */
  def putAll(me: Int, ns: Array[Int]) : (Array[ParSearchNode], Seq[ParSearchNode])
  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in a queue */
  // def putAllUnrootedInit(me: Int, ns: Array[Int]) : Queue[ParSearchNode]

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in an array */
  def putAllUnrooted(me: Int, ns: Array[Int]) : Array[ParSearchNode]
}

// -------------------------------------------------------

/** An implementation of Seen, using a ShardedMap 
  * @param shards the number of shardes
*/
import ox.cads.collection.ShardedMap

object ShardedSeen extends Seen{
// class ShardedSeen(shards: Int) extends Seen{
  private val seen = new ShardedMap[Int,ParSearchNode](197)  // (shards)
  // private val seen = new ox.cads.collection.ShardedMapTTASLocks[Int,ParSearchNode]

  /** See default way of creating new entries */
  private def initNode(n:Int) = new ParSearchNode(n)
  seen.setDefaultInit(initNode)


  /** Clear the map, ready for a new search */
  def clear = seen.clear
  // Profiler.time(0, "seen.clear"){ //seen.clear
  //   seen = new ShardedMap[Int,ParSearchNode](197); 
  //   seen.setDefaultInit(initNode)
  // }

  /** If n is not in the map, initialise it to a new node; otherwise 
    * return the node */
  def getOrInit(n: Int) : ParSearchNode = 
    seen.getOrElseDefaultInit(n) // ; seen.setDefaultInit(initNode)
    // seen.getOrElseUpdate(n, new ParSearchNode(n))
  // {
  //   val node = seen.getOrElseNull(n)
  //   if(node == null) seen.getOrElseUpdate(n, new ParSearchNode(n)) else node
  // }

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * @return a pair (nodes, newNodes), where:
    *   nodes is the nodes corresponding to ns;
    *   newNodes is the new nodes encountered (in reverse order), excluding
    *     the node that will be explored first by this worker */
  def putAll(me: Int, ns: Array[Int]) 
  : (Array[ParSearchNode], Seq[ParSearchNode]) = {
    var nodes = new Array[ParSearchNode](ns.size); 
    var newNodes = List[ParSearchNode]()
    // var newNodes = new scala.collection.mutable.ArrayBuffer[ParSearchNode]()
    // Create a new node corresponding to ns(i); add the node to newNodes.
    // This is called only if there is not already such a node.
    def mkNode(i: Int) = {
      val nd = new ParSearchNode(ns(i)); 
      if(i!=0) newNodes ::= nd // += nd // do this only if it's a new node
      nd
    }
    val size = ns.size; 
    for(i <- 0 until size){
      val node = Profiler.notime(me, "Seen.putAll1"){
	seen.getOrElseUpdate(ns(i), mkNode(i))}
      nodes(i) = node
    }
    (nodes, newNodes)
  }

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in a queue */
  // FIXME: I think we no longer need this
  def putAllUnrootedInit(me: Int, ns: Array[Int]) : Queue[ParSearchNode] = {
    val size = ns.size; var nodes = Queue[ParSearchNode](); var i = 0
    while(i<size){
      val n = ns(i)
      val node = Profiler.notime(me, "Seen.putAllUnrootedInit1"){
	seen.getOrElseUpdate(n, new ParSearchNode(n))}
      nodes += node
      i += 1
    }
    nodes
  }

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in an array */
  def putAllUnrooted(me: Int, ns: Array[Int]) : Array[ParSearchNode] = {
    val size = ns.size; var nodes = new Array[ParSearchNode](size); var i = 0
    while(i<size){
      val n = ns(i); 
      nodes(i) = Profiler.notime(me, "Seen.putAllUnrooted1"){
	// seen.getOrElseDefaultInit(n) }
	seen.getOrElseUpdate(n, new ParSearchNode(n)) }
      i += 1
    }
    nodes
  }

}

// -------------------------------------------------------

/** An implementation of Seen, using a ConcurrentHaspMap.
  * @param cap the initial capacity */
// This seems slower in practice.
import java.util.concurrent.ConcurrentHashMap

class CHMSeen(cap: Int) extends Seen{
  private val seen = new ConcurrentHashMap[Int,ParSearchNode](cap)

  /** Clear the map, ready for a new search */
  def clear = seen.clear

  def getOrInit(n: Int) : ParSearchNode = {
    val node = new ParSearchNode(n)
    val old = seen.putIfAbsent(n, node)
    if(old == null) node else old
  }

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * @return a pair (nodes, newNodes), where:
    *   nodes is the nodes corresponding to ns;
    *   newNodes is the new nodes encountered (in reverse order), excluding
    *     the node that will be explored first by this worker */
  def putAll(me: Int, ns: Array[Int]) 
  : (Array[ParSearchNode], List[ParSearchNode]) = {
    var nodes = new Array[ParSearchNode](ns.size)
    var newNodes = List[ParSearchNode]()
    val size = ns.size
    for(i <- 0 until size){
      val n = Profiler.notime(me, "new node"){new ParSearchNode(ns(i))}
      val n1 = Profiler.notime(me, "Seen.putAll1"){ seen.putIfAbsent(ns(i), n) }
      if(n1 == null){ // this was a new node; n inserted
	nodes(i) = n; if(i != 0) newNodes ::= n
      }
      else nodes(i) = n1 // n1 was already there
    }
    (nodes, newNodes)
  }

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in a queue */
  def putAllUnrootedInit(me: Int, ns: Array[Int]) : Queue[ParSearchNode] = {
    val size = ns.size; var nodes = Queue[ParSearchNode](); var i = 0
    while(i<size){
      val n = Profiler.notime(me, "new node"){new ParSearchNode(ns(i))}
      val n1 = Profiler.notime(me, "Seen.putAllUnrootedInit1"){
	seen.putIfAbsent(ns(i), n) }
      if(n1 == null) nodes += n // this was a new node; n inserted
      else nodes += n1  // n1 was already there
      i += 1
    }
    nodes
  }

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in an array */
  def putAllUnrooted(me: Int, ns: Array[Int]) : Array[ParSearchNode] = {
    val size = ns.size; var nodes = new Array[ParSearchNode](size); var i = 0
    while(i<size){
      val n = Profiler.notime(me, "new node"){new ParSearchNode(ns(i))}
      val n1 = Profiler.notime(me, "Seen.putAllUnrooted1"){
	seen.putIfAbsent(ns(i), n) }
      if(n1 == null) nodes(i) = n // this was a new node; n inserted
      else nodes(i) = n1
      i += 1
    }
    nodes
  }
}

// -------------------------------------------------------

/** An implementation based around a NodeTable.
  * @param size an upper bound on the number of nodes to be stored. */
// This doesn't seem to help, and is arguably cheating
class NodeTableSeen(maxSize: Int) extends Seen{
  private val nt : NodeTable = new ConcurrentStaticNodeTable(maxSize*3/2)

  /** Clear the map, ready for a new search */
  def clear = nt.clear

  /** If n is not in the map, initialise it to a new node; otherwise 
    * return the node */
  def getOrInit(n: Int) : ParSearchNode = nt.getOrInit(n)

  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * @return a pair (nodes, newNodes), where:
    *   nodes is the nodes corresponding to ns;
    *   newNodes is the new nodes encountered (in reverse order), excluding
    *     the node that will be explored first by this worker */
  def putAll(me: Int, ns: Array[Int]) 
  : (Array[ParSearchNode], Seq[ParSearchNode]) =
    throw new UnsupportedOperationException

 /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in a queue */
  def putAllUnrootedInit(me: Int, ns: Array[Int]) : Queue[ParSearchNode] =
    throw new UnsupportedOperationException
 
  /** For each n in ns, if there is no node stored against n, create a new 
    * (uninitialised) node and store it
    * This version is designed for use in the unrooted case.
    * @return the nodes corresponding to ns, in an array */
  def putAllUnrooted(me: Int, ns: Array[Int]) : Array[ParSearchNode] = {
    val size = ns.size; var nodes = new Array[ParSearchNode](size); var i = 0
    while(i<size){
      nodes(i) = 
	Profiler.notime(me, "Seen.putAllUnrooted1"){nt.getOrInit(ns(i))}
      i += 1
    }
    nodes
  }

}
