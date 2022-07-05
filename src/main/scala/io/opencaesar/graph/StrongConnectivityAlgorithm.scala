package io.opencaesar.graph

import scala.collection.immutable.{Map,Seq,SortedSet}

/**
 * See org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm
 * See org.jgrapht.alg.connectivity.AbstractStrongConnectivityInspector
 * @tparam V vertex
 */
trait StrongConnectivityAlgorithm[V : Ordering]:

  val g: DiGraph[V]

  def stronglyConnectedSets: Seq[SortedSet[V]]

  def isStronglyConnected: Boolean = 1 == stronglyConnectedSets.size

  def getStronglyConnectedComponents: Seq[DiGraph[V]] =
    stronglyConnectedSets.map { scc =>
      AsSubgraph.asSubgraph(g)(scc, SortedSet.empty[(V, V)])
    }

  given graphOrdering(using o: Ordering[V]): Ordering[DiGraph[V]] = (x: DiGraph[V], y: DiGraph[V]) =>
    if x.isEmpty && y.isEmpty then
      0
    else
      x.vs.size.compare(y.vs.size) match
        case 0 =>
          (x.vs.headOption, y.vs.headOption) match
            case (Some(hx), Some(hy)) =>
              o.compare(hx,hy)
            case (Some(_), None) =>
              1
            case (None, Some(_)) =>
              -1
            case _ =>
              0
        case r =>
          r

  def getCondensation: DiGraph[DiGraph[V]] =
    val sccs = stronglyConnectedSets
    val v2components: Map[V, DiGraph[V]] =
      sccs.flatMap { scc =>
        val component = AsSubgraph.asSubgraph(g)(scc, SortedSet.empty[(V, V)])
        scc.map { v => v -> component}
      }.toMap
    val cv: DiGraph[DiGraph[V]] =
      v2components.values.foldLeft(DiGraph.empty[DiGraph[V]])(_.addVertex(_))
    g.es.foldLeft(cv) { case (ci, (s,t)) =>
      (v2components.get(s), v2components.get(t)) match
        case (Some(cs), Some(ct)) if cs != ct =>
          ci.addEdge(cs,ct)
        case _ =>
          ci
    }



