GRAPHS = Graph.class WheelGraph.class SolitaireGraph.class	\
GraphFromTauDump.class

COMPONENTS = SearchNode.class SeqCheck.class Profiler.class Pending.class	\
StealingQueue.class SCCSet.class Divergences.class SearchParams.class		\
ParSearchNode.class NodeTable.class Suspended.class Seen.class			\
OpenAddressingSeen.class Scheduler.class Search.class Worker.class		\
ParSolver.class

# TidyingQueue.class 

EXPERIMENTS = Test.class MyTests.class Experiments.class MakeCSPGraphs.class	\
ExperimentBase.class CSPExperiments.class RandomExperiment.class

ALL = $(GRAPHS) $(COMPONENTS) $(EXPERIMENTS)

# SpinLock.class Semaphore.class Monitor.class 	\
# StealingQueue.class Pending.class MyHashMap.class SCSG.class			\
# MyHashMapTest.class Dependencies.class Done.class Worker.class	\
#  Test.classPending.class ThreadUtil.class CompleteSearch.class 

# SEQCOMPONENTS = Graph.class SearchNode.class

# SeqCheck: $(SEQCOMPONENTS)
# 	fsc SeqCheck.scala

ParCheck: $(ALL)
	fsc ParCheck.scala

%.class: %.scala
	 fsc $<

# NodeStatus.class
#  Message.class MessageQueue.class Joins.class ArrayJoins.class
# IndexedGuard.class DoublyIndexedGuard.class \ SemIndexedGuard.class
# AllJoins.class SetPrecedes.class ExtendableArray.class

# -optimise -Yinline -Yclosure-elim