package io.opencaesar.graph
import scala.collection.immutable.SortedSet
import scala.collection.mutable

/**
 * See org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector
 * @param g graph
 * @tparam V vertex type
 */
case class KosarajuStrongConnectivityInspector[V : Ordering](g: DiGraph[V])
  extends StrongConnectivityAlgorithm[V]:

  val orderedVertices: mutable.Queue[VertexData] = mutable.Queue.empty

  val vertexToVertexData: mutable.Map[V, VertexData] =
    val map = mutable.Map.empty[V, VertexData]
    g.vs.foreach { v =>
      map.update(v, new VertexData2(v))
    }
    map

  override def stronglyConnectedSets: Seq[SortedSet[V]] =

    vertexToVertexData.values.foreach { data =>
      if !data.discovered then
        dfsVisit(g, data, None)
    }

    val inverseGraph = g.reverse()

    vertexToVertexData.values.foreach { data =>
      data.finished = false
      data.discovered = false
    }

    var sccs: Seq[SortedSet[V]] = Seq.empty

    for (data <- orderedVertices)
      if !data.discovered then
        val scc: mutable.Set[V] = mutable.Set.empty
        dfsVisit(inverseGraph, data, Some(scc))
        sccs = sccs.appended(SortedSet.empty[V] ++ scc)

    sccs

  abstract class VertexData(v: V, d: Boolean = false, f: Boolean = false):
    val vertex: V = v
    var discovered: Boolean = d
    var finished: Boolean = f

    def getFinishedData: VertexData

  class VertexData1(finishedData: VertexData, d: Boolean = false, f: Boolean = false)
    extends VertexData(finishedData.vertex, d, f):

    override def getFinishedData: VertexData = finishedData

  class VertexData2(v: V, d: Boolean = false, f: Boolean = false)
    extends VertexData(v, d, f):

    override def getFinishedData: VertexData = ???

  private def dfsVisit(graph: DiGraph[V], vertexData: VertexData, vertices: Option[mutable.Set[V]]): Unit =

    val stack: mutable.Queue[VertexData] = mutable.Queue.empty
    stack.append(vertexData)

    while stack.nonEmpty do
      val data = stack.removeLast()

      if !data.discovered then

        data.discovered = true
        vertices.foreach(vs => vs.add(data.vertex))

        stack.append(new VertexData1(data, true, true))

        for (v <- graph.childrenOf(data.vertex))
          assert(vertexToVertexData.contains(v))
          val targetData = vertexToVertexData(v)
          if !targetData.discovered then
            stack.append(targetData)

      else if data.finished && vertices.isEmpty then

        orderedVertices.prepend(data.getFinishedData)