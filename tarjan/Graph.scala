/** A graph
  * @param nodes the nodes of the graph, all >= 0
  * @param the successors
  * @param the initial node */

class Graph(
  val nodes: Array[Int], val succs: Int => Array[Int], val init: Int)
{
  /** Constructor ignoring the nodes; suitable when a rooted search is 
    * being performed */
  def this(succs: Int => Array[Int], init: Int) = this(null, succs, init)

  /** dummy constructor */
  def this() = this((i:Int) => Array[Int](), -1)

  /** The number of edges in the graph.  Only valid for unrooted searches 
    * (when nodes != null). */
  def numEdges : Int = {
    assert(nodes != null); nodes.map(succs(_).length).sum
  }


}

object Graph{
  /** The graph ({0..n-1}, {(i,j) | 0 <= i < j < n}, 0) */
  def fullNonDiv(n: Int) : Graph = 
    new Graph((0 until n).toArray, i => (i+1 until n).toArray, 0)
  // The "reverse" makes a huge difference to the timings.

  /** The same as fullNonDiv(n), with an additional edge i -> j */
  def oneRev(n: Int, i:Int, j: Int) : Graph = 
    new Graph((0 until n).toArray, 
	      i1 => if(i1==i) (i1+1 until n).toArray ++ Array(j)
  		    else (i1+1 until n).toArray, 
  	      0)

  /** Random graph with N nodes.  Each edge i -> j is included with
    * probability p */
  def random(N: Int, p: Float) : Graph = {
    // The code is oriented towards the case that N is large, but N.p (the
    // expected number of edges from each node) is small
    val edges = new Array[Array[Int]](N)
    val random = new scala.util.Random
    val q = 1.0-p; val ratio = p/q
    for(i <- 0 until N){
      // Generate edges from node i.  First pick the number of edges: there
      // are k edges with probability p_k = N!/((N-k)! k!) * p^k q^(N-K); pick
      // the number that makes the cumulative probability at least k
      val rand = random.nextDouble
      var k = 0
      var pp = scala.math.pow(1.0-p, N)
      var accum = pp
      // Invariant: pp = p_k and accum = sum_{i in [0..k]} p_i
      while(accum < rand){
	// p_{k+1} = p_k . (N-k)/(k+1) . p/q
	pp = pp * (N-k)/(k+1) * ratio
	accum += pp
	k += 1
      }
      // So we'll have k edges
      var theEdges = List[Int](); var length = 0
      while(length<k){
	val j = random.nextInt(N) // have an edge from i to j
	if(! theEdges.contains(j)){ theEdges ::= j; length += 1 }
      }
      edges(i) = theEdges.toArray
    } // end of for

    new Graph((0 until N).toArray, i => edges(i), 0)

    // val edges = 
    //   for(i <- 0 until n)
    //   yield (for(j <- 0 until n; if random.nextFloat <= p) yield j).toArray
    // new Graph(i => edges(i), 0)
  }

  /** Nodes are pairs (i,j) on an m*n grid; edges are
    * {(i,j) -> (i+1,j) | i < m-1} U
    * {(i,j) -> (i,j+1) | j < n-1} U
    * {(i,j) -> (i-1,j), (i,j) -> (i,j-1) | i%2 = j%2 = 1}.
    * The node (i,j) is represented by the int i*n+j */
  def rectangularGrid(m: Int, n: Int) : Graph = {
    def es(i: Int, j: Int) : Array[Int] = {
      val ps = 
  	(if(i<m-1) List((i+1,j)) else List()) ++
  	(if(j<n-1) List((i,j+1)) else List()) ++
  	(if(i%2==1 && j%2==1) List((i-1,j), (i,j-1)) else List())
      ps.map{ case(i1,j1) => i1*n+j1 }.toArray
    }
    new Graph(k => es(k/n, k%n), 0)
  }

  /** Graph with k separate loops, each with n nodes, each loop reachable 
    * from the start node */
  def independentLoops(k: Int, n: Int) : Graph = {
    def es(i: Int) : Array[Int] = 
      if(i==0) (1 to (k-1)*n+1 by n).toArray
      else if(i%n==0) Array(i+1-n)
      else Array(i+1)
    new Graph(es, 0)
  }


  /** Graph with n nodes, edges i -> i+1, and if i%k = 0 and i>0 then an edge
    * i -> i+1-k */
  def multipleLoops(n: Int, k: Int) : Graph = {
    def es(i: Int) = 
      (if(i>0 && i%k==0) List(i+1-k) else List()) ++
      (if(i<n-1) List(i+1) else List())
    new Graph(i => es(i).toArray, 0)
  }

  // /** Solitaire board of size rows by cols, with an initial blank in
  //   * position (r0,c0), and a target of getting at most target pegs */ 
  // def solitaire(rows: Int, cols: Int, r0: Int, c0: Int, target: Int) = {
  //   assert(rows*cols <= 32)
  //   // Internally represent a board by an array of Boolean.  Externally
  //   // represent is by an Int, whose kth least significant digit is 1 iff the
  //   // kth square is non-empty

  //   // Convert an Int to the corresponding board
  //   def intToBoard(n: Int) : Array[Array[Boolean]] = {
  //     val board = Array.ofDim[Boolean](rows,cols)
  //     var n1 = n
  //     for(r <- 0 until rows; c <- 0 until cols){
  //   	board(r)(c) = (n1%2 == 1); n1 = n1/2
  //     }
  //     board
  //   }

  //   // Convert a board to the corresonding Int
  //   def boardToInt(board: Array[Array[Boolean]]) : Int = {
  //     var n = 0
  //     for(r <- rows-1 to 0 by -1; c <- cols-1 to 0 by -1){
  //   	n = n*2; if(board(r)(c)) n += 1
  //     }
  //     n
  //   }

  //   // Calculate moves from n
  //   def succs1(n: Int) : Array[Int] = { 
  //     // Contribution of position (r,c)
  //     def peg(r: Int, c: Int) = 1 << (r*cols+c)
  //     // Is there a peg in (r,c)?
  //     def board(r: Int, c: Int) = (n & peg(r,c)) != 0
  //     // val board = intToBoard(n)
  //     var pegs = 0
  //     for(r <- 0 until rows; c <- 0 until cols; if board(r,c)) pegs += 1
  //     var succs0 = if(pegs <= target) List(n) else List()
  //     val dirs = Array((1,0),(0,1),(-1,0),(0,-1))
  //     var r = 0 // iterate over row number
  //     while(r < rows){
  //     	var c = 0 // iterate over column number
  //     	while(c < cols){
  //     	  if(board(r,c)){
  //     	    var dir = 0 // iterate over direction to move
  //     	    while(dir < 4){
  //     	      val (dr,dc) = dirs(dir)
  //     	      val r1 = r+2*dr; val c1 = c+2*dc // move to (r1,c1)
  //     	      val rm = r+dr; val cm = c+dc // over (rm,cm) in middle
  //     	      if(r1>=0 && r1<rows && c1>=0 && c1<cols && // still on board
  //     		 !board(r1,c1) && board(rm,cm)) // peg in middle, empty dest
  //     		succs0 ::= n - peg(r,c) - peg(rm,cm) + peg(r1,c1) // Make move
  //     	      dir += 1
  //     	    } // end of while(dir...)
  //     	  } // end of if(board(r)(c))
  //     	  c += 1
  //     	} // end of while(c...)
  //     	r += 1
  //     } // end of while(r...)
  //     succs0.toArray
  //   }

  //   // Calculate moves from n
  //   def succs4(n: Int) : Array[Int] = {
  //     val board = intToBoard(n)
  //     val pegs = board.map(_.count(identity)).sum 
  //     var succs0 = if(pegs <= target) List(n) else List[Int]()
  //     val dirs = Array((1,0),(0,1),(-1,0),(0,-1))
  //     var r = 0 // iterate over row number
  //     while(r < rows){
  //   	var c = 0 // iterate over column number
  //   	while(c < cols){
  //   	  if(board(r)(c)){
  //   	    var dir = 0 // iterate over direction to move
  //   	    while(dir < 4){
  //   	      val (dr,dc) = dirs(dir)
  //   	      val r1 = r+2*dr; val c1 = c+2*dc // move to (r1,c1)
  //   	      val rm = r+dr; val cm = c+dc // over (rm,cm) in middle
  //   	      if(r1>=0 && r1<rows && c1>=0 && c1<cols && // still on board
  //   		 !board(r1)(c1) && board(rm)(cm)) // peg in middle, empty dest
  //   		{ // Make move
  //   		  board(r)(c) = false; board(rm)(cm) = false; 
  //   		  board(r1)(c1) = true
  //   		  // Store resulting state
  //   		  succs0 ::= boardToInt(board)
  //   		  // Undo move for next iteration
  //   		  board(r)(c) = true; board(rm)(cm) = true; 
  //   		  board(r1)(c1) = false
  //   		} 
  //   	      dir += 1
  //   	    } // end of while(dir...)
  //   	  } // end of if(board(r)(c))
  //   	  c += 1
  //   	} // end of while(c...)
  //   	r += 1
  //     } // end of while(r...)
  //     succs0.toArray
  //   }

  //   // Calculate moves from n, using a for loop
  //   def succs3(n: Int) : Iterable[Int] = {
  //     val board = intToBoard(n)
  //     val pegs = board.map(_.count(identity)).sum 
  //     var succs0 = if(pegs <= target) List(n) else List()
  //     for(r <- 0 until rows; c <- 0 until cols; if board(r)(c);
  //   	  // Consider moving peg from position (r,c)
  //   	  (dr,dc) <- List((1,0),(0,1),(-1,0),(0,-1)))
  //   	{
  //   	  val r1 = r+2*dr; val c1 = c+2*dc // move to (r1,c1)
  //   	  val rm = r+dr; val cm = c+dc // over (rm,cm) in middle
  //   	  if(r1>=0 && r1<rows && c1>=0 && c1<cols && // still on board
  //   	     !board(r1)(c1) && board(rm)(cm)) // peg in middle, empty dest
  //   	    {
  //   	      // Make move
  //   	      board(r)(c) = false; board(rm)(cm) = false; board(r1)(c1) = true
  //   	      // Store resulting state
  //   	      succs0 ::= boardToInt(board)
  //   	      // Undo move for next iteration
  //   	      board(r)(c) = true; board(rm)(cm) = true; board(r1)(c1) = false
  //   	    } 
  //   	}
  //     succs0
  //   }

  //   // Using for expression takes about twice as long as using a for loop 
  //   def succs2(n: Int) : Iterable[Int] = {
  //     val board = intToBoard(n)
  //     val pegs = board.map(_.count(b => b)).sum 
  //     val succs0 = 
  //     	for(r <- 0 until rows; c <- 0 until cols; if board(r)(c);
  //     	    // Consider moving peg from position (r,c)
  //     	    (dr,dc) <- List((1,0),(0,1),(-1,0),(0,-1));
  //     	    val r1 = r+2*dr; val c1 = c+2*dc; // move to (r1,c1)
  //     	    val rm = r+dr; val cm = c+dc; // over (rm,cm) in middle
  //     	    if r1>=0 && r1<rows && c1>=0 && c1<cols; // still on board
  //     	    if !board(r1)(c1) && board(rm)(cm)) // peg in middle, empty dest
  //     	yield{
  //     	  // Clone board
  //     	  val b1 = Array.ofDim[Boolean](rows,cols)
  //     	  for(rr <- 0 until rows; cc <- 0 until cols) b1(rr)(cc) = board(rr)(cc)
  //     	  b1(r)(c) = false; b1(rm)(cm) = false; b1(r1)(c1) = true
  //     	  boardToInt(b1)
  //     	} 
  //     succs0.reverse ++ (if(pegs <= target) List(n) else List())
  //   }

  //   val initB : Int = { 
  //     val board = Array.ofDim[Boolean](rows,cols)
  //     for(r <- 0 until rows; c <- 0 until cols) board(r)(c) = true
  //     board(r0)(c0) = false
  //     boardToInt(board)
  //   }

  //   new Graph(succs1(_), initB)

  // } 

}
