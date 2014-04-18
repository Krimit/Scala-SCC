/** A graph that looks like a wheel, with spokes spokes, each containing
  * spokeLength nodes (spokeLength>=0), and with arcLength nodes (arcLength>=1)
  * between spokes. */

class WheelGraph(spokes: Int, spokeLength: Int, arcLength: Int) 
extends Graph(
  WheelGraph.mkSuccs(spokes, spokeLength, arcLength), 
  spokes*(spokeLength+arcLength))
  // (i:Int) => 
  //   if(i == spokes*arcLength) (0 until spokes*arcLength by arcLength).toArray
  //   else Array((i+1) % (spokes*arcLength)),
  // spokes*arcLength)


object WheelGraph{
  def mkSuccs(spokes: Int, spokeLength: Int, arcLength: Int)(i: Int) 
  : Array[Int] = {
    assert(spokeLength>=0 && arcLength>=1)
    val interval = spokeLength+arcLength // interval between symmetric nodes
    val origin = spokes*interval
    assert(0<=i && i<=origin)
    if(i==origin) (0 until origin by interval).toArray
    else if((i+1)%interval == 0) // last node on an arc
      Array((i+1)%origin + spokeLength)
    else Array(i+1)
  }
}
