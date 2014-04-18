/** Some experiments on the Concurrent Tarjan's Algorithm */

object Experiments{
  val experiments : Array[(String, Unit => Graph, Boolean)] = Array(
    ("forward(3000)           ", _ => Graph.fullNonDiv(3000), true),
    ("oneRev(3000,2000,1000)  ", _ => Graph.oneRev(3000,2000,1000), true),
//    ("random(100000,0.00002)  ", _ => Graph.random(100000,0.00002F), false),
    ("random(200000,0.00001)  ", _ => Graph.random(200000,0.00001F), false),
    ("random(200000,0.000005) ", _ => Graph.random(200000,0.000005F), false),
//    ("random(200000,0.000003) ", _ => Graph.random(200000,0.000003F), false),
    ("random(200000,0.000001) ", _ => Graph.random(200000,0.000001F), false),
    ("solitaire(5, 5, 0, 0, 2)", _ => new SolitaireGraph(5,5,0,0,2), true),
    ("solitaire(5, 5, 0, 0, 1)", _ => new SolitaireGraph(5,5,0,0,1), true),
    ("solitaire(5, 5, 1, 1, 1)", _ => new SolitaireGraph(5,5,1,1,1), true),
    ("tringm.1                ", 
       _ => GraphFromTauDump("CSP/ucs_examples/should_pass/tringm.1.graph"),
       false),
    ("swp.2                   ", 
       _ => GraphFromTauDump("CSP/ucs_examples/should_pass/swp.2.graph"),
       false)
  )

//     ("random(400000,0.000004) ", _ => Graph.random(400000,0.000004F)),
//     ("random(400000,0.0000035)", _ => Graph.random(400000,0.0000035F)),
//     ("random(400000,0.000003) ", _ => Graph.random(400000,0.000003F)),
//     ("random(400000,0.0000025)", _ => Graph.random(400000,0.0000025F)),
//     ("random(400000,0.000002) ", _ => Graph.random(400000,0.000002F)),
//     ("random(400000,0.000001) ", _ => Graph.random(400000,0.000001F)),
//     ("random(400000,0.0000025)", _ => Graph.random(400000,0.00000025F))
//     ("random(1000000,0.000002)", _ => Graph.random(1000000,0.000002F))

  val p = 8 // # workers
  Profiler.setWorkers(p)
  val N = experiments.size // # experiments
  var iters = 10 // # iterations
  val lockFree = false
  val includeSeq = true
  def log(st: => String) = { }
  def name(i: Int) : String = experiments(i)._1
  def graph(i: Int) : Graph = experiments(i)._2()
  def rooted(i: Int) : Boolean = experiments(i)._3

  def main(args: Array[String]) = {
    if(args.length>0){
      assert(args.length == 2 && args(0) == "-N")
      iters = args(1).toInt
    }

    val seqTimes = new Array[Long](N); val concTimes = new Array[Long](N)

    for(iter <- 0 until iters){
      for(i <- 0 until N){ // iterate over experiments
	val exp = graph(i)
	val isRooted = rooted(i)
	print(name(i))
	// Run concurrent algorithm
	System.gc
	val t0 = java.lang.System.currentTimeMillis()
	new ParSolver(exp, p, isRooted, lockFree, log).solve
	val time0 = java.lang.System.currentTimeMillis() - t0
	if(iter>0) concTimes(i) += time0
	// Run sequential algorithm
	if(includeSeq){
	  System.gc
	  val t1 = java.lang.System.currentTimeMillis()
	  SeqCheck.solve(exp, isRooted, log)
	  val time1 = java.lang.System.currentTimeMillis() - t1
	  println("\t"+time0+"\t"+time1)
	  if(iter>0) seqTimes(i) += time1
	}
	else println
      }
    }

    // Print results
    println("\nResults from "+(iters-1)+" experiments:\n")
    for(i <- 0 until N) 
      println(name(i)+"\t"+concTimes(i)+"\t"+"\t"+seqTimes(i))
    Profiler.report
  }
}
