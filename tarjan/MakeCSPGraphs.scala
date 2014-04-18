/** A utility to create tau dumps for all the CSP files in the library */

import java.io.{File,FileWriter,BufferedReader,InputStreamReader}

object MakeCSPGraphs{
  // The root of the library, relative to the directory from which the program
  // is run (or absolute)
  var root = "CSP" // "/users/gavinl/Verification/ParTarjan/Code/Tarjan/CSP"
  // The dump_tau_graph program
  val dump_tau_graph = "/users/gavinl/bin/fdr3-utilities/bin/dump_tau_graph"
  // The file that lists all the created files
  val createdFileName = "allGraphFiles.txt"

  /** Get all .csp files included in f, recursively
    * This will probably diverge if there's a symbolic link back up the 
    * directory tree! */
  def flatten(f: File) : List[File] = {
    if(f.isDirectory) f.listFiles.toList.map(flatten).flatten
    else if(f.getPath.endsWith(".csp")) List(f)
    else List[File]()
  }

  /** Create the graph for process p in file f, putting the output into out */
  def makeGraph(f: String, p: String, out: String) = {
    print(f+"\t"+p+"...")
    val outFile = new FileWriter(out)
    val command = Array(dump_tau_graph, f, p)
    // println(command.mkString(" "))
    val proc = Runtime.getRuntime().exec(command)
    val results =
      new BufferedReader(
        new InputStreamReader(proc.getInputStream(),"ISO-8859-1") )
    // Consume output from dump_tau_graph, and write to outFile
    var st = results.readLine(); var lines = 1
    while(st != null){ 
      outFile.write(st+"\n"); st = results.readLine(); lines += 1
    }
    outFile.close
    println(".  Done; "+(lines-1)+" lines.")
  }


  def main(args: Array[String]) = {
    if(args.nonEmpty){ assert(args(0) == "--root"); root = args(1) }

    val dir = new File(root)
    val files = flatten(dir) // all the files
    var outputFiles = List[String]() // output file names
    var count = 0 // number of files produced

    for(file <- files){
      // Find all implementation processes appearing in assertions 
      println(file.getPath)
      val lines = scala.io.Source.fromFile(file).getLines
      var procs = List[String]() // all processes found
      for(line <- lines){
	if(line.dropWhile(_ == ' ').startsWith("assert")){
	  // println(line)
	  // Find where refinement operator appears
	  val tfrefPlace = 
	    line.indexOfSlice("[T=") max line.indexOfSlice("[F=") max
	    line.indexOfSlice("[R=") 
	  val fdrefPlace = line.indexOfSlice("[FD=")
	  val afterRef = // the substring after the refinement operator
	    if(tfrefPlace > 0) line.drop(tfrefPlace+3)
	    else if(fdrefPlace > 0) line.drop(fdrefPlace+4) else ""
	  if(afterRef.nonEmpty){
	    // Position of any qualifier
	    val qualPlace = afterRef.indexOfSlice(":[")
	    // The process itself
	    val proc = if(qualPlace>0) afterRef.take(qualPlace) else afterRef
	    if(! procs.contains(proc)) procs ::= proc
	  }
	  else{
	    // No refinement operator
	    val assertPlace = line.indexOf("assert")
	    val afterAssert = line.drop(assertPlace+6)//"assert" removed
	    val notPlace = afterAssert.indexOf("not")
	    val afterNot = //"not" removed
	      if(notPlace>=0) afterAssert.drop(notPlace+3) else afterAssert
	    val qualPlace = afterNot.indexOf(":[")
	    // FIXME: remove comments and spaces before testing for equality 
	    if(qualPlace>0){
	      val proc = afterNot.take(qualPlace) // the process itself
	      // println("***"+proc); println(line)
	      if(! procs.contains(proc)) procs ::= proc
	    }
	    else println("*** Failed to parse "+line+" ***")
	  }
	}	
      } // end of for(line <- lines)
      // println(procs)
      
      // Now produce tau dumps for each such process
      for((p,i) <- procs.zipWithIndex){
	val cspFile = file.getPath
	val outputFile = cspFile.dropRight(4)+"."+i+".graph"
	makeGraph(cspFile, p, outputFile)
	outputFiles ::= outputFile
	count += 1
      }
    } // end of for(file <- files)
    
    println("Produced "+count+" files: "+outputFiles)
    val createdFile = new FileWriter(createdFileName)
    for(f <- outputFiles) createdFile.write(f+"\n")
    
  }

} 
