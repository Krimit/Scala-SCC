import org.scalatest.FunSuite
import java.io._

class Test extends FunSuite{
  val writer = new PrintWriter(new File("log.txt" ))
  def log(st: => String) = writer.write(st+"\n")
  val p = 8 // number of workers
  Profiler.setWorkers(p)

  val N = 100
  val g1 = Graph.fullNonDiv(N)
  val g2 = Graph.oneRev(N, 2*N/3, N/3)

  try{
    for(i <- 0 until 50){
      test("forward"+i){ 
	assert(new ParSolver(g1, p, false, false, log).solve.toSet === 
	  (for(i <- 0 until N) yield Set(i)).toSet )
	log("-------------------------------------------------------")
      }
      test("oneRev"+i){  
	val expected =
	  ((for(i <- 0 until N/3) yield Set(i)) ++ 
	   ((N/3 to 2*N/3).toSet +: (for(i <- 2*N/3+1 until N) yield Set(i)))
	 ).toSet
	assert(new ParSolver(g2, p, false, false, log).solve.toSet === expected)
	log("-------------------------------------------------------")
      }
      test("random"+i){
	val g3 = Graph.random(N,0.02F)
	assert(new ParSolver(g3, p, false, false, log).solve === 
	  SeqCheck.solve(g3, false, log))
	log("-------------------------------------------------------")
      }
      test("4 x 4 solitaire"+i){
	val g = new SolitaireGraph(4,4,0,0,1)
	assert(new ParSolver(g, p, true, false, log).solve === 
	  SeqCheck.solve(g, true, log))
      }
      if(false) test("5 x 4 solitaire"+i){
	val g = new SolitaireGraph(5,4,0,0,1)
	assert(new ParSolver(g, p, false, false, log).solve === 
	  SeqCheck.solve(g, false, log))
      }
    }
  } finally{ writer.flush(); writer.close() }

    // test("5 x 4 solitaire"+i){
    //   val g = Graph.solitaire(5,4,0,0,1)
    //   assert(new ParSolver(g, p, log).solve)
    // }
    // test("Solvable 5 x 5 solitaire"+i){
    //   val g = Graph.solitaire(5,5,0,0,2)
    //   assert(new ParSolver(g, p, log).solve)
    // }
  // test("Unsolvable 5 x 5 solitaire"){
  //   val g = Graph.solitaire(5,5,0,0,1)
  //   assert(! new ParSolver(g, p, log).solve)
  // }

 
}
