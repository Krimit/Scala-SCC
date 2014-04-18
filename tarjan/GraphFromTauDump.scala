/** Object to produce a graph based on a tau-dump produced by dump_tau_graph 
 */ 

object GraphFromTauDump{
  /** Produce a graph based on the tau-dump produced by dump_tau_graph 
    * @param fname the file name */
  def apply(fname: String) : Graph = {
    val lines = scala.io.Source.fromFile(fname).getLines.toArray
    val size = lines.size // # lines; states are [0..size-1)
    val succs = new Array[Array[Int]](size-1)
    // Get root
    val line0 = lines(0)
    assert(line0.startsWith("root = "));
    val root = line0.drop(7).toInt
    // println("root = "+root+"\n")

    // Now get the transitions
    for(i <- 1 until size){
      val line = lines(i)
      val split = line.split(Array(':',' '))
      // This should give something like Array("1","","2","3") or Array("1")
      assert(split(0).toInt == i-1 && (split.length==1 || split(1)==""))
      if(split.length==1) succs(i-1) = Array[Int]()
      else succs(i-1) = split.drop(2).map(_.toInt)
      // println(i-1+" -> "+succs(i-1).mkString(","))
    }

    // Produce the graph
    new Graph((0 until size-1).toArray, (i:Int) => succs(i), root)

  }
}
