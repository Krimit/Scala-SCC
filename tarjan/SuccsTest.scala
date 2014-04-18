import ox.CSO._
import scala.actors.Actor._

object SuccsTest{
  val N = 2000000 // number of iterations
  
  val rows = 5; val cols = 6; val target = 1

  // Convert an Int to the corresponding board
  def intToBoard(n: Int) : Array[Array[Boolean]] = {
    val board = Array.ofDim[Boolean](rows,cols)
    var n1 = n
    for(r <- 0 until rows; c <- 0 until cols){
      board(r)(c) = (n1%2 == 1); n1 = n1/2
    }
    board
  }

  // Convert a board to the corresonding Int
  def boardToInt(board: Array[Array[Boolean]]) : Int = {
    var n = 0
    for(r <- rows-1 to 0 by -1; c <- cols-1 to 0 by -1){
      n = n*2; if(board(r)(c)) n += 1
    }
    n
  }



  def nested = {
    var s = 0
    for (a <- 0 until 2000000) 
      for(r <- 0 until 2) for(x <- 0 until 2) for(z <- 0 until 2) 
	s += 1
    s
  }

  def unnested = {
    var s = 0
    for(i <- 0 until 2000000000) s += 1
    for(i <- 0 until 2000000000) s += 1
    s
  }
  
 
  
  def main(args: Array[String]) = {
    def choice = 
      // if(args(0)=="integral") one 
      // else if(args(0)=="succs") four
      // else 
	if(args(0)=="nested") nested
      else if(args(0)=="unnested") unnested
      // else sys.error("argument not recognized")

    println("Sequential")
    Profiler.time("sequential"){ choice }

    println("CSO")
    val sys = || (for(_ <- 0 until 8) yield proc{ choice; () })
    Profiler.time("CSO"){ sys() }

    Profiler.report

    print("Actors:     ")
    val t0 = java.lang.System.currentTimeMillis()
    val controller = actor{
      for(i <- 0 until 8) receive{ case msg => { } }
      println(java.lang.System.currentTimeMillis() - t0)
    }
    for(i <- 0 until 8) actor{ choice; controller!"done" }
    

  }
}
