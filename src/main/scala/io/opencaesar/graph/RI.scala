package io.opencaesar.graph

import scala.annotation.tailrec
import scala.collection.immutable.*

extension[V : Ordering](g: DiGraph[V])

  /**
   * Reachability index
   *
   * @see https://ieeexplore.ieee.org/document/9006804
   *      One Edge at a Time: A Novel Approach Towards Efficient Transitive Reduction Computation on DAGs
   *      X. Tang, J. Zhou, Y. Qiu, X. Liu, Y. Shi and J. Zhao
   *      Digital Object Identifier 10.1109/ACCESS.2020.2975650
   *      
   * Here, the reachability index of a node u in g is derived by a depth-first walk of g starting at u,
   * collecting all visited nodes v.
   * 
   * @return a map(u, vs) such that all v in vs are reachable from u via edges in g.
   */
  def ri(): SortedMap[V, SortedSet[V]] =
    // Note: toSet conversions necessary to avoid needing an Ordering[(V, ...)]
    val colors: SortedMap[V, Color] = SortedMap.empty[V, Color] ++ g.vs.toSet.map(v => v -> Color.White)
    val index = SortedMap.empty[V, SortedSet[V]] ++ 
      g.vs.toSet.map(v => v -> ri(g.childrenOf(v), colors.updated(v, Color.Gray), SortedSet.empty[V]))
    index

  @annotation.tailrec
  private def ri(vs: SortedSet[V], colors: SortedMap[V, Color], acc: SortedSet[V]): SortedSet[V] =
    if vs.isEmpty then
      acc
    else
      val (v, vt) = (vs.head, vs.tail)
      if colors.getOrElse(v, Color.Black) == Color.White then
        ri(vt ++ g.childrenOf(v), colors.updated(v, Color.Gray), acc + v)
      else
        ri(vt, colors, acc)