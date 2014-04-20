package edu.nyu.ppc.scc

import collection.mutable

/**
 * A semi mutable weighted graph representation using adjacency list
 * Can add/remove/update edges but cannot add/remove vertices
 *
 * @param numberOfVertices Number of vertices in graph
 * @param isDirected true iff a directed graph
 */
class Graph(val numberOfVertices: Int, val isDirected: Boolean = true) {
  import Graph.EndPoints

  private val adjacencyList = Array.fill(numberOfVertices)(mutable.Map.empty[Int, Double] withDefaultValue Double.PositiveInfinity)

  private implicit class Edge(points: EndPoints) {
    val (u, v) = points
    assume(hasVertices(u, v))
  }

  /**
   * Edge between points
   * This is more readable alternative to traditional g(u,v) i.e. g(u->v)
   *
   * @param points (from,to)
   * @return edge value (else 0 if from==to or +infinity if from and to has no edge)
   */
  def apply(points: EndPoints): Double = if (points.u == points.v) 0.0 else adjacencyList(points.u)(points.v)

  /**
   * curried alternative to @see apply(EndPoints)
   *
   * @param u from
   * @param v to
   * @return edge value of u->v
   */
  def apply(u: Int)(v: Int): Double = this(u->v)

  /**
   * Check if edge exists
   * @param points (from,to)
   * @return true iff from->to edge exists
   */
  def has(points: EndPoints) = adjacencyList(points.u) contains points.v

  /**
   * @return true iff all vertices in graph
   */
  def hasVertices(vs: Int*) = vs forall vertices.contains

  /**
   * @return neighbors of u
   */
  def neighbours(u: Int) = adjacencyList(u).keySet

  /**
   * Update edges
   * To remove use -=
   *
   * @param points (from, to)
   * @param weight (from,to) = weight
   */
  def update(points: EndPoints, weight: Double) {
    adjacencyList(points.u)(points.v) = weight
    if (!isDirected) {
      adjacencyList(points.v)(points.u) = weight
    }
  }

  /**
   * Delete an edge between (from,to)
   * @param points (from,to)
   */
  def -=(points: EndPoints) {
    adjacencyList(points.u) -= points.v
    if (!isDirected) {
      adjacencyList(points.v) -= (points.u)
    }
  }

  /**
   * @return vertices in graph
   */
  def vertices = adjacencyList.indices

  /**
   * @return edges in graph
   */
  def edges = for (u <- vertices; v <- neighbours(u)) yield u->v

  /**
   * @return the adjacency matrix of this graph
   */
  def adjacencyMatrix = Array.tabulate(numberOfVertices, numberOfVertices)((u, v) => this(u->v))
  
  def isEmpty = edges.isEmpty
  
  def subGraph(onlyTheseVertexes: Set[Int]): Graph = {
    //val cpy = adjacencyList.clone()
    //Array.fill(numberOfVertices)(mutable.Map.empty[Int, Double] withDefaultValue Double.PositiveInfinity)
    //NOTE: this makes a new graph. Maybe better - return a view? How to do this?
    val h = new Graph(onlyTheseVertexes.size)
    println(onlyTheseVertexes.size + " vertices: ")
    println(h.vertices)
    for (v <- onlyTheseVertexes) {
      println(adjacencyList(v).keySet)
      for (u <- adjacencyList(v).keySet) {
        println(v + " " + u)
        if (v != u && onlyTheseVertexes.contains(u)) {
          h.update((v,u),1) 
        }
        
        //h.apply(v,u)
      }
    }
    h
  }
  
  def subGraph(onlyTheseVertexes: List[Int]): Graph = {
    subGraph(onlyTheseVertexes.toSet)
  }
  
  def remove(num: Int) = adjacencyList diff List(num)
  
   /**
   * Breadth first search from source in g
   * If we replace queue with stack, we get DFS
   * O(V + E) - each vertex/edge is examined atmost once
   *
   * @param f Apply f to each vertex in bfs order from source
   * @return If f is true at a vertex v, return Some(v) else None
   */
  def successors(source: Int): List[Int] = {
    val (seen, queue) = (mutable.Set.empty[Int], mutable.Queue.empty[Int])

    def visit(i: Int) = {
      if (source != i) seen += i
      queue += i
    }

    visit(source)

    while (!queue.isEmpty) {
      val u = queue.dequeue()
      
      this neighbours u filterNot seen foreach visit
    }

    seen.toList
  }
  
  /**
   * Idea: Invert edges and call successors
   *
   * @param f Apply f to each vertex in bfs order from source
   * @return If f is true at a vertex v, return Some(v) else None
   */
  def predecessors(source: Int): List[Int] = {
    
    var h: Graph = new Graph(this.numberOfVertices)
    
    for (u <- this.vertices) {
      for (v <- this.vertices) {
        if ((u != v) && (this.has((u,v)))) {
            h.update((v,u),1)   
        }
      }    
    }
    h.successors(source)
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

      g neighbours u foreach {v =>
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
      g neighbours u filterNot seen foreach visit
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
    if(f(u)) Some(u) else g neighbours u filterNot seen find {v => dfs(g, u, f, seen + u).isDefined}
}