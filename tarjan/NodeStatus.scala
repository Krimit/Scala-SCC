/** Each subclass of NodeStatus represents some information about the status
  * of a node in the search.  */
abstract class NodeStatus

/** The status Complete indicates that the node and all its descendents have
  *  been explored without findig any loop. */
case class Complete() extends NodeStatus

/** The status InProgress indicates that the node is in the stack of 
  * worker, so worker is exploring the graph below this node. */
case class InProgress() extends NodeStatus

/** The status Provisional(worker, dependents) indicates that worker explored
  * below this node, and found no loops, but the nodes dependents were in
  * progress at that point. */
case class Provisional() // , dependents: List[ParSearchNode]) 
     extends NodeStatus

// TODO: do we need the worker values?
