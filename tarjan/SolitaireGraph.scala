class SolitaireGraph(rows: Int, cols: Int, r0: Int, c0: Int, target: Int)
extends Graph(
  SolitaireGraph.mkSuccs(rows, cols, r0, c0, target),
  SolitaireGraph.init(rows, cols, r0, c0)
)

object SolitaireGraph{
  // Calculate moves from n
  def mkSuccs(rows: Int, cols: Int, r0: Int, c0: Int, target: Int)(n: Int) 
  : Array[Int] = { 
    // Contribution of position (r,c)
    def peg(r: Int, c: Int) = 1 << (r*cols+c)
    // Is there a peg in (r,c)?
    def board(r: Int, c: Int) = (n & peg(r,c)) != 0
    // val board = intToBoard(n)
    var pegs = 0
    for(r <- 0 until rows; c <- 0 until cols; if board(r,c)) pegs += 1
    var succs0 = if(pegs <= target) List(init(rows, cols, r0, c0)) else List()
    val dirs = Array((1,0),(0,1),(-1,0),(0,-1))
    var r = 0 // iterate over row number
    while(r < rows){
      var c = 0 // iterate over column number
      while(c < cols){
      	if(board(r,c)){
      	  var dir = 0 // iterate over direction to move
      	  while(dir < 4){
      	    val (dr,dc) = dirs(dir)
      	    val r1 = r+2*dr; val c1 = c+2*dc // move to (r1,c1)
      	    val rm = r+dr; val cm = c+dc // over (rm,cm) in middle
      	    if(r1>=0 && r1<rows && c1>=0 && c1<cols && // still on board
      	       !board(r1,c1) && board(rm,cm)) // peg in middle, empty dest
      	      succs0 ::= n - peg(r,c) - peg(rm,cm) + peg(r1,c1) // Make move
      	    dir += 1
      	  } // end of while(dir...)
      	} // end of if(board(r)(c))
      	c += 1
      } // end of while(c...)
      r += 1
    } // end of while(r...)
    succs0.toArray
  }

  def init(rows: Int, cols: Int, r0: Int, c0: Int) : Int = { 
    // Convert a board to the corresonding Int
    def boardToInt(board: Array[Array[Boolean]]) : Int = {
      var n = 0
      for(r <- rows-1 to 0 by -1; c <- cols-1 to 0 by -1){
    	n = n*2; if(board(r)(c)) n += 1
      }
      n
    }

    val board = Array.ofDim[Boolean](rows,cols)
    for(r <- 0 until rows; c <- 0 until cols) board(r)(c) = true
    board(r0)(c0) = false
    boardToInt(board)
  }

}
