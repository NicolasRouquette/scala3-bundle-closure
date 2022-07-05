package io.opencaesar.graph

import scala.collection.immutable.SortedSet

object AsSubgraph:

  extension[V : Ordering](g: DiGraph[V])

    def asSubgraph(subVs: SortedSet[V], subEs: SortedSet[(V, V)]): DiGraph[V] =
      val g1 = subVs.foldLeft(DiGraph.empty[V])(subVertexAdd)
      val g2 = if subEs.nonEmpty then
        subEs.foldLeft(g1)(subEdgeAdd)
      else
        g.es.foldLeft(g1)(subEdgeAdd)
      g2

    private def subVertexAdd(subg: DiGraph[V], subv: V): DiGraph[V] =
      subg.addVertex(subv)

    private def subEdgeAdd(subg: DiGraph[V], sube: (V, V)): DiGraph[V] =
      if subg.vs.contains(sube._1) && subg.vs.contains(sube._2) then
        subg.addEdge.tupled(sube)
      else
        subg
