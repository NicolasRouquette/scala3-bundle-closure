package io.opencaesar.graph

import scala.collection.immutable.{SortedMap,SortedSet}

/**
 * Algorithm 1 TR-B(G=(V,E))
 * 
 * @see https://ieeexplore.ieee.org/document/9006804
 *      One Edge at a Time: A Novel Approach Towards Efficient Transitive Reduction Computation on DAGs
 *      X. Tang, J. Zhou, Y. Qiu, X. Liu, Y. Shi and J. Zhao
 *      Digital Object Identifier 10.1109/ACCESS.2020.2975650
 */
object TR_B:

  /**
   * Transitive reduction of a directed graph according to algorithm 1 TR-B.
   * 
   * @param g directed graph
   * @tparam V the type of graph nodes
   * @return a new graph whose edges are not redundant and whose reachability index is equivalent to that of g.
   */
  def transitiveReduction[V:Ordering](g:DiGraph[V]):DiGraph[V]=
    given ri as SortedMap[V,SortedSet[V]]=g.ri()
    val es=g.sortEdges
    val gr=es.foldLeft(g) {
      case(gi:DiGraph[V],(x,y))=>
        if gi.isRedundant(x,y)then
          gi.deleteEdge(x,y)
        else
          gi
    }
    assert(gr.ri() == ri)
    assert(gr.sortEdges.forall(!gr.isRedundant(_,_)))
    gr

  extension[V:Ordering](g:DiGraph[V]):

    /**
     * sortEdges returns an a-priori sorting of all edges in g.
     * 
     *  7 Function sortEdge(G)
     *  8 for each (u ∈ V) do
     *  9   for each (v ∈ outG(u)) do
     * 10     enQueue(E, (u, v))
     * 11 return E
     * 
     * @return The collection of edges (u,v) from all nodes u in g and all neighbors v of u.
     */
    def sortEdges:Seq[(V,V)]=
      g.vs.foldLeft[Seq[(V,V)]](Seq.empty[(V,V)]) { case (acc1: Seq[(V,V)], u: V) =>
        g.out(u).foldLeft[Seq[(V,V)]](acc1) { case (acc2: Seq[(V,V)], v: V) =>
          if !acc2.contains((u,v)) then
            acc2 :+ (u,v)
          else
            acc2
        }
      }

    /**
     * isRedundant(u,v) is true if there is an indirect path from u to a neighbor node w!=v and v is reachable from w. 
     *
     * 12 Function isRedundant(u, v)
     * 13 for each (w ∈ outG(u),w 6= v) do
     * 14   if (RI(w, v) = TRUE ) then
     * 15     return TRUE /*w v, and (u, v) is redundant*/
     * 16 return FALSE
     *
     * @param u a node of g
     * @param v a node of g
     * @param ri The reachability index of g
     * @return true if u has a neighbor node w != v such that ri(w,v)
     */
    def isRedundant(u:V,v:V)(using ri:SortedMap[V,SortedSet[V]]):Boolean= 
      g.out(u).exists(w=>w!=v&&ri.getOrElse(w,SortedSet.empty[V]).contains(v))
