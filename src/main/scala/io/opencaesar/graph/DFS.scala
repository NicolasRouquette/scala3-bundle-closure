package io.opencaesar.graph

import scala.collection.immutable.{Seq,SortedMap,SortedSet}

extension[V : Ordering](g: DiGraph[V])

  /**
   * Depth-first search
   * 
   * @see Introduction to Algorithms, section 22.3
   *      T. Cormen, C. Keiserson, R. Rivest and C. Stein
   *      
   * @return A topological sorting of vertices of g.
   */
  def dfs(): Seq[V] =
    val colors: SortedMap[V, Color] = SortedMap.empty[V, Color] ++ g.vs.toSet.map(v => v -> Color.White)
    dfs(colors, Seq.empty[V])

  private def dfs(colors: SortedMap[V, Color], topo: Seq[V]): Seq[V] =
    colors.find(_._2 == Color.White) match
      case None =>
        topo
      case Some((u, _)) =>
        val (c,t) = dfsVisit(u, g.out(u), colors.updated(u, Color.Gray), topo)
        dfs(c, t)

  private def dfsVisit(u: V, vs: SortedSet[V], colors: SortedMap[V, Color], topo: Seq[V]): (SortedMap[V, Color], Seq[V]) =
    if vs.isEmpty then
      (colors.updated(u, Color.Black), u +: topo)
    else
      val (v, vt) = (vs.head, vs.tail)
      if colors.getOrElse(v, Color.Black) == Color.White then
        val (c, t) = dfsVisit(v, g.out(v), colors.updated(v, Color.Gray), topo)
        dfsVisit(u, vt, c, t)
      else
        dfsVisit(u, vt, colors, topo)
  