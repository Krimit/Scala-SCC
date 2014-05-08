package edu.nyu.scc

import collection.mutable
import scala.util.Random

/**
 * A semi mutable weighted graph representation using adjacency list
 * Can add/remove/update edges but cannot add/remove vertices
 *
 * @param numberOfVertices Number of vertices in graph
 * @param isDirected true iff a directed graph
 */
class Graph(val numberOfVertices: Int, val isDirected: Boolean = true) {
  import Graph.EndPoints

  //private val adjacencyList = Array.fill(numberOfVertices)(mutable.Map.empty[Int, Double] withDefaultValue Double.PositiveInfinity)
    private var adjacencyList:mutable.Map[Int, List[Int]] = build(numberOfVertices)
    private var reversedAdjacencyList:mutable.Map[Int, List[Int]] = build(numberOfVertices)

  //private val adjacencyList = (0 to numberOfVertices) map { i => (i, Map[Int, Double]) } toMap(mutable.Map.empty[Int, Double] withDefaultValue Double.PositiveInfinity)
  //private var adjacencyList:Map[Int, Map[Int, Double]] = (0 to numberOfVertices) map { i => (i, Map[Int, Double]()) } toMap
  
    
  
  private implicit class Edge(points: EndPoints) {
    val (u, v) = points
    assume(hasVertices(u, v))
  }
    
  def build(n: Integer) = {
    val m:mutable.Map[Int,List[Int]] = mutable.Map()
    var i = 0
    for (i <- 1 to n) {
      m += (i->List[Int]())
    }
    m
  }
  
  def buildFromSet(vs: Set[Int]):mutable.Map[Int, List[Int]] = {
    val m:mutable.Map[Int,List[Int]] = mutable.Map()
    for (i <- vs) {
      m += (i->List[Int]())
    }
    m
  }
  
  def getAdj() = {
    adjacencyList
  }
  
  def getRevAdj() = {
    reversedAdjacencyList
  }
  
  def withTheseAdjLists(adj :mutable.Map[Int, List[Int]], revAdj :mutable.Map[Int, List[Int]]) = {
    adjacencyList = adj
    reversedAdjacencyList = revAdj
  }

  /**
   * Edge between points
   * This is more readable alternative to traditional g(u,v) i.e. g(u->v)
   *
   * @param points (from,to)
   * @return edge value (else 0 if from==to or +infinity if from and to has no edge)
   */
  def apply(points: EndPoints): Boolean = if (points.u == points.v) true else (
      (adjacencyList(points.u).contains(points.v)) && (reversedAdjacencyList(points.v).contains(points.u)))

  /**
   * curried alternative to @see apply(EndPoints)
   *
   * @param u from
   * @param v to
   * @return edge value of u->v
   */
  def apply(u: Int)(v: Int): Boolean = this(u->v)
  
  /**
   * Check if edge exists
   * @param points (from,to)
   * @return true iff from->to edge exists
   */
  def has(points: EndPoints) = ((adjacencyList(points.u) contains points.v) && (reversedAdjacencyList(points.v) contains points.u))

  /**
   * @return true iff all vertices in graph
   */
  def hasVertices(vs: Int*) = vs forall vertices.contains

  /**
   * @return neighbors of u
   */
  def outNeighbours(u: Int) = adjacencyList(u)
  
   /**
   * @return neighbors of u
   */
  def inNeighbours(u: Int) = reversedAdjacencyList(u)
  
  /**
   * Update edges
   * To remove use -=
   *
   * @param points (from, to)
   * @param weight (from,to) = weight
   */
  def update(points: EndPoints) {
    if(!adjacencyList.contains(points.u)) {
       adjacencyList += (points.u->List[Int]())
    }
    if(!adjacencyList.contains(points.v)) {
       adjacencyList += (points.v->List[Int]())
    }
    if (!adjacencyList(points.u).contains((points.v))) {
      adjacencyList(points.u) = (points.v)::adjacencyList(points.u)
    }
    if(!reversedAdjacencyList.contains(points.u)) {
       reversedAdjacencyList += (points.u->List[Int]())
    }
    if(!reversedAdjacencyList.contains(points.v)) {
       reversedAdjacencyList += (points.v->List[Int]())
    }
    if (!reversedAdjacencyList(points.v).contains((points.u))) {
      reversedAdjacencyList(points.v) = (points.u)::reversedAdjacencyList(points.v)
    }
    
    if (!isDirected) {
      adjacencyList(points.v) = (points.u)::adjacencyList(points.v)
    }
  }

  /**
   * Delete an edge between (from,to)
   * @param points (from,to)
   */
  def -=(points: EndPoints) {
    adjacencyList(points.u) = adjacencyList(points.u) diff List(points.v)
    reversedAdjacencyList(points.v) = reversedAdjacencyList(points.v) diff List(points.u)
    if (!isDirected) {
      adjacencyList(points.v) = adjacencyList(points.v) diff List(points.u)
    }
  }

  /**
   * @return vertices in graph
   */
  def vertices = adjacencyList.keySet

  /**
   * @return edges in graph
   */
  def edges = for (u <- vertices; v <- outNeighbours(u)) yield u->v //TODO: TEST!

  /*
  /**
   * @return the adjacency matrix of this graph
   */
  def adjacencyMatrix = Array.tabulate(numberOfVertices, numberOfVertices)((u, v) => this(u->v))
  */
  def isEmpty = edges.isEmpty
  
  def subGraphOf(group: List[Int]) = {
    //NOTE: this makes a new graph. Maybe better - return a view? How to do this?
    val h = new Graph(0)
    val newAdj = adjacencyList.filterKeys( group.contains ).mapValues( x => x intersect ( group ) )
    val newRevAdj = reversedAdjacencyList.filterKeys( group.contains ).mapValues( x => x intersect ( group ) )
    h.withTheseAdjLists(mutable.Map(newAdj.toSeq: _*), mutable.Map(newRevAdj.toSeq: _*))
    h
  }
  
  def subGraphWithout(group: List[Int]) = {
    //NOTE: this makes a new graph. Maybe better - return a view? How to do this?
    val h = new Graph(0)
    val newAdj = adjacencyList.filterKeys(x => !group.contains(x) ).mapValues( x => x.diff(group) )
    val newRevAdj = reversedAdjacencyList.filterKeys(x => !group.contains(x) ).mapValues( x => x.diff(group) )
    h.withTheseAdjLists(mutable.Map(newAdj.toSeq: _*), mutable.Map(newRevAdj.toSeq: _*))
    h
  }
  
  def subGraphWithout(onlyTheseVertexes: Set[Int]): Graph = {
    subGraphWithout(onlyTheseVertexes.toList)
  }
  
  def subGraphOf(onlyTheseVertexes: Set[Int]): Graph = {
    subGraphOf(onlyTheseVertexes.toList)
  }
  
  def remove(num: Int) = {
    adjacencyList -= num; 
    reversedAdjacencyList -= num; 
  }
  
  override def toString() = {
    adjacencyList.toString()
  }
  
  def getRandomVertex(): Int = {
    val i = Random.nextInt(vertices.size)
    vertices.slice(i, i+1).head
  }
  
  var totalTime: Integer = 0
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    totalTime += (t1 - t0).intValue()
    result
  }
  
  def getTimeCounter(): Int = {
    totalTime
  }
  
  def resetTimeCounter() = {
    totalTime = 0
  }
  
  
   /**
   * Breadth first search from source in g
   * If we replace queue with stack, we get DFS
   * O(V + E) - each vertex/edge is examined atmost once
   *
   * @param f Apply f to each vertex in bfs order from source
   * @return If f is true at a vertex v, return Some(v) else None
   */
  def successors(source: Int): Set[Int] = {
    var (seen, queue) = (Set[Int](source), mutable.Queue.empty[Int])

    def visit(i: Int) = {
      if (source != i) seen = seen + i
      queue += i
    }

    visit(source)

    while (!queue.isEmpty) {
      val u = queue.dequeue()
      
      this outNeighbours u filterNot seen foreach visit
    }

    seen
  }
  
  /**
   * Idea: Invert edges and call successors
   *
   * @param f Apply f to each vertex in bfs order from source
   * @return If f is true at a vertex v, return Some(v) else None
   */
  def predecessors(source: Int): Set[Int] = {
    
    /*
    var h: Graph = new Graph(0)
    val m:mutable.Map[Int,List[Int]] = mutable.Map()
    for (i <- this.vertices) {
      m += (i->List[Int]())
    }
    h.withThisAdjList(m)

    
    for (u <- this.vertices) {
      for (v <- this.vertices) {
        if ((u != v) && (this.has((u,v)))) {
            h.update((v,u))   
        }
      }    
    }
    h.successors(source)
    * 
    */
    
    var (seen, queue) = (Set[Int](source), mutable.Queue.empty[Int])

    def visit(i: Int) = {
      if (source != i) seen = seen + i
      queue += i
    }

    visit(source)

    while (!queue.isEmpty) {
      val u = queue.dequeue()
      
      this inNeighbours u filterNot seen foreach visit
    }

    seen
  }
}

/**
 * Collection of graph algorithms
 */
object Graph {

  private type EndPoints = Pair[Int, Int]


  /**
   * Run Tarjan's strongly connected component algorithm in G
   * O(E + V) - each edge is examined once
   *          - each vertex is pushed/popped once
   * Trivially finds all cycles too
   * TODO: Return a DisjointSet?
   * TODO: http://apps.topcoder.com/forums/?module=Thread&threadID=785825&mc=1#1714843
   *
   * @param g input graph
   * @return the set of strongly connected components
   *         either a set is of size 1 or for every pair of vertex u,v in each set v is reachable from u
   */
  def stronglyConnectedComponents(g: Graph) = {
    var count = 0
    val (index, lowLink) = (mutable.Map.empty[Int, Int], mutable.Map.empty[Int, Int])
    val stack = mutable.Stack[Int]()         //TODO: try empty here
    val inProcess = mutable.LinkedHashSet.empty[Int]
    val sccs = mutable.Queue.empty[Set[Int]]

    def dfs(u: Int) {
      index(u) = count        // set u.index to lowest unused count
      lowLink(u) = count
      stack push u
      inProcess += u
      count += 1

      g outNeighbours u foreach {v =>
        if(!(index contains v)) {
          dfs(v)
          lowLink(u) = lowLink(u) min lowLink(v)
        } else if (inProcess contains v) {
          lowLink(u) = lowLink(u) min index(v)
        }
      }

      if (index(u) == lowLink(u)) {
       var v = -1
       val scc = mutable.Set.empty[Int]
       do {
         v = stack.pop()
         inProcess -= v
         scc += v
       } while(u != v)
       sccs += scc.toSet
      }
    }

    //todo: g.vertices filterNot index.contains foreach dfs
    for {
      u <- g.vertices
      if (!(index contains u))
    } dfs(u)
    println("all done now outside as well: " + sccs.size)
    //how many singles are there?
    var i = 0
    
    for (c <- sccs) {
      if (c.size == 1) {
        i += 1
      } 
    }
    println(i + " components of size 1")
    
    sccs.toSeq
  }



  /**
   * Breadth first search from source in g
   * If we replace queue with stack, we get DFS
   * O(V + E) - each vertex/edge is examined atmost once
   *
   * @param f Apply f to each vertex in bfs order from source
   * @return If f is true at a vertex v, return Some(v) else None
   */
  def bfs(g: Graph, source: Int, f: Int => Boolean): Option[Int] = {
    val (seen, queue) = (mutable.Set.empty[Int], mutable.Queue.empty[Int])

    def visit(i: Int) = {
      seen += i
      queue += i
    }

    visit(source)

    while (!queue.isEmpty) {
      val u = queue.dequeue()
      if (f(u)) {
        return Some(u)
      }
      g outNeighbours u filterNot seen foreach visit
    }

    None
  }

 
  
  /**
   * Recursive depth first search from u in g
   * TODO: Change BFS, DFS to cost too
   * O(V + E) - each vertex/edge is examined atmost once
   *
   * @param f Apply f to each vertex in dfs order from source
   * @return If f is true at a vertex v, return Some(v) else None
   */
  def dfs(g: Graph, u: Int, f: Int => Boolean, seen: Set[Int] = Set.empty[Int]): Option[Int] =
    if(f(u)) Some(u) else g outNeighbours u filterNot seen find {v => dfs(g, u, f, seen + u).isDefined}
}