import java.io._

object MyTests{

  val p = 8 // # workers
  var N = 1000 // size of graph
  var prob = 0.002F // probability of an edge between any two given nodes
  var iters = 1000 // number of iterations
  Profiler.setWorkers(p)

  def main(args: Array[String]) = {
    var rooted = false; var lockFree = false; 
    var loops = false; var lassos = false
    var i = 0
    // Parse arguments
    while(i<args.length){
      if(args(i)=="--rooted"){ rooted = true; i += 1 }
      else if(args(i)=="--lockFree"){ lockFree = true; i += 1 }
      else if(args(i)=="--loops"){ loops = true; i += 1 }
      else if(args(i)=="--lassos"){ loops = true; lassos = true; i += 1 }
      else if(args(i)=="--iters"){ iters = args(i+1).toInt; i += 2 }
      else if(args(i)=="--N"){ N = args(i+1).toInt; i += 2 }
      else if(args(i)=="--prob"){ prob = args(i+1).toFloat; i += 2 }
      else sys.error("Incorrect usage")
    }

    var g3 : Graph = null

    var theLog = List[String]() // held in reverse order
    // def log(st: => String) = synchronized{ theLog ::= st }
    // def writeLog = {
    //   // write log to file
    //   val writer = new PrintWriter(new File("log.txt" ))
    //   for(st <- theLog.reverse) writer.write(st+"\n")
	
    var writer : PrintWriter = null
    try{
      for(iter <- 0 until iters){
	g3 = Graph.random(N,prob) // ; theLog = List[String]()
	writer = new PrintWriter(new File("log.txt" ))
	def log(st: => String) = { writer.write(st+"\n"); writer.flush() }
	// Run sequential version
	val res1 = SeqCheck.solve(g3, rooted, loops, lassos, log)
	log("-------------------------------------------------------")
	// Run concurrent version
	val res2 = 
	  new ParSolver(g3, p, rooted, loops, lassos, lockFree, log).solve
	log("-------------------------------------------------------")
      
	// Compare results
	(res1,res2) match{
	  case (Left(sccs1), Left(sccs2)) => {
	    assert(!loops)
	    if(sccs1 != sccs2){
	      writer.flush; writer.close
	      println(sccs1+"\n"); println(sccs2+"\n")
	      println(sccs1.diff(sccs2)+"\n"); println(sccs2.diff(sccs1)+"\n")
	      sys.error("Not equal!")
	    }
	  }
	  case (Right(divs1), Right(divs2)) => {
	    assert(loops)
	    if(divs1.sorted != divs2.sorted){
	      writer.flush; writer.close
	      println(divs1.sorted+"\n"); println(divs2.sorted+"\n")
	      println(divs1--divs2+"\n"); println(divs2--divs1+"\n")
	      sys.error("Not equal!")
	    }
	  }
	  case _ => sys.error("Results of different types!")
	}

	writer.close
	if(iter%10 == 0) println(iter)
      } // end of for 
    } finally{ writer.flush; writer.close }

    Profiler.report

  }

}
	// if(res1!=res2){ 
	//   writer.flush; writer.close // writeLog; 	
	//   println(res1+"\n"); println(res2+"\n")
	//   (res1,res2) match{
	//     case (Left(sccs1), Left(sccs2)) => {
	//       assert(!loops)
	//       println(sccs1.diff(sccs2)+"\n"); println(sccs2.diff(sccs1)+"\n")
	//     }
	//     case (Right(divs1), Right(divs2)) => {
	//       assert(loops)
	//       println(divs1--divs2+"\n"); println(divs2--divs1+"\n")
	//     }
	//     case _ => sys.error("Results of different types!")
	//   }
	//   sys.error("Not equal!") 
	// }
