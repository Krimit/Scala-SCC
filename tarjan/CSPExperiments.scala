/** A suite of experiments based on CSP files */

import java.io.File
import scala.collection.mutable.ArrayBuffer

object CSPExperiments extends ExperimentBase{
  /** Get the last part of a path (i.e. following the final "/") with the
    * final ".graph" removed */
  def getSuffix(st: String) = st.reverse.drop(6).takeWhile(_ != '/').reverse

  /** Get all .graph files included in f, recursively
    * This will probably diverge if there's a symbolic link back up the 
    * directory tree! */
  def flatten(f: File) : List[String] = {
    if(f.isDirectory) f.listFiles.toList.map(flatten).flatten
    else if(f.getPath.endsWith(".graph")) List(f.toString)
    else List[String]()
  }

  def main(args: Array[String]) = {
    // Parse arguments
    // The root of the library, relative to the directory from which the
    // program is run (or absolute)
    var root = "CSP"  
    // Min and max number of states in graphs to consider
    var minStates = 100000; var maxStates = 5000000 
    var i = 0
    val usage =
      "Usage: scala [-J-Xmx s] CSPExperiments [--root d] [--minStates n] [--maxStates n] [--loops | --lassos | --all]\n"+
      "        [--iters n] [--concOnly] [--noWarmUp] [-p p] [--verbose] [--latex]"
    while(i<args.length){
      if(args(i)=="--iters"){ iters = args(i+1).toInt; i += 2 }
      else if(args(i)=="--minStates"){ minStates = args(i+1).toInt; i += 2 }
      else if(args(i)=="--maxStates"){ maxStates = args(i+1).toInt; i += 2 }
      else if(args(i)=="--verbose"){ verbose = true; i += 1 }
      else if(args(i)=="--root"){ root = args(i+1); i += 2 }      
      else if(args(i)=="--loops"){ loops = true; i += 1 }
      else if(args(i)=="--lassos"){ loops = true; lassos = true; i += 1 }
      else if(args(i)=="--all"){ all = true; i += 1 }
      else if(args(i)=="--latex"){ latex = true; i += 1 }
      else if(args(i)=="--concOnly"){ includeSeq = false; i += 1 }
      else if(args(i)=="--noWarmUp"){ warmUp = false; i += 1 }
      else if(args(i)=="-p"){ p = args(i+1).toInt; i += 2 }
      else sys.error(usage)
    }

    Profiler.setWorkers(p)
    val files = flatten(new File(root)) // all the names of files in root

    // Create the graphs of those files with appropriate number of states
    // the graphs to include, together with the filename and number of states
    for(file <- files){
      val states = scala.io.Source.fromFile(file).getLines.length - 1
      if(minStates <= states && states <= maxStates){
	println("Including "+file+"; "+states+" states")
	val g = GraphFromTauDump(file)
	// println(g.numEdges+" edges")
	experiments += ((getSuffix(file), states, GraphFromTauDump(file)))
      }
    }  

    // Size of the longest suffix, plus 1
    nameFieldSize = 
      if(experiments.isEmpty) -1 
      else experiments.map(_._1).map(_.size).max + 1 

    // Initialise tables, etc.
    val theSearchTypes = getSearchTypes
    numExperiments = experiments.length
    initTables(theSearchTypes.length)

    // Now run experiments
    for(iter <- 0 until iters){
      for(i <- 0 until numExperiments) 
	runExperiment(graph(i), i, theSearchTypes, iter>0 || ! warmUp, iter==0)
      println
      if(iter == 0) showStateStats(1) 
    }

    // Print results
    printResults; Profiler.report
  }

}
