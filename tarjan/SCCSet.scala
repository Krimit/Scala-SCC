/** An object representing a set of SCCs */
class SCCSet{
  val p = SearchParams.p
  // This is optimized to make use of the fact that most SCCs contains many
  // singleton SCCs, and a small number of larger ones.  We want to avoid
  // calculating the hashCode of the large ones.

  type SCC = Set[Int]

  /** Each n in singles(w) represents the singleton SCC {n}.  singles(w)
    * is owned by worker w */
  private val singles = Array.fill(p)(scala.collection.mutable.ArrayBuffer[Int]())
  // private val singles = Array.fill(p)(List[Int]())
  // private val singles = Array.fill(p)(scala.collection.mutable.Set[Int]())

  /** Each set S in nonSingletons represents itself.
    * Each member of nonSingletons has size > 1 */
  private var nonSingletons = List[SCC]()

  def add(scc: SCC, owner: Int) : Unit = 
    // if(scc.size == 1) singles(owner) ::= scc.head 
    if(scc.size == 1) singles(owner) += scc.head 
    // No need to sync the above, as it's done by owner
    else synchronized{ nonSingletons ::= scc }

  override def toString = 
    (nonSingletons.map(_.toString) ++ singles.map("{"+ _ +"}") ) .
    mkString ("{", ",", "}")

  def toStringSummary : String = {
    val singSts = (
      for(s <- nonSingletons)
      yield if(s.size<1000) s.toString else ("An SCC of size "+s.size))
    singSts.mkString("\n")+"\n"+singles.map(_.size).sum+" singleton SCCs"
  }

  /** Gives summary of the SCCs.
    * @return a triple (num, super, trivs) where num is the number of SCCS, 
    * super is the size of the largest SCC, and trivs is the number of 
    * trivial SCCs */
  def summary : (Int,Int,Int) = {
    val trivs = singles.map(_.size).sum
    (trivs + nonSingletons.size, (1::nonSingletons.map(_.size)).max, trivs)
  }

  /** All the singleton SCCs */
  private def singletons : Set[SCC] = 
    (for(s <- singles; n <- s) yield Set(n)).toSet

  override def equals(other: Any) = other match{
    case that: SCCSet => 
      this.singletons == that.singletons &&
      this.nonSingletons.forall(scc => that.nonSingletons.contains(scc)) &&
      that.nonSingletons.forall(scc => this.nonSingletons.contains(scc))
    case _ => false
  }

  /** String representing everything in this but not in other */
  def diff(that: SCCSet) : Set[SCC] = {
    this.singletons.diff(that.singletons) union
    this.nonSingletons.filter(! that.nonSingletons.contains(_)).toSet
  }

  override def hashCode = singles.hashCode 

  // The following is expensive for large structures
  def toSet : Set[SCC] = nonSingletons.toSet union singletons
}
