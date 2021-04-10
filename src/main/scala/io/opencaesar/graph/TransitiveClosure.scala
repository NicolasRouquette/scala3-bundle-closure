package io.opencaesar.graph

import scala.collection.immutable.{SortedMap,SortedSet}

case class TransitiveClosure[V: Ordering](data: SortedMap[V, SortedSet[V]]):

  def successors(from: V): SortedSet[V] =
    data.getOrElse(from, SortedSet.empty[V])
    
  def reachable(from: V, to: V): Boolean =
    successors(from).contains(to)
    
object TransitiveClosure:

  extension[V: Ordering](g: DiGraph[V])

    def transitiveClosure(): TransitiveClosure[V] =

      def dfs(s: V, u: V, tc: TransitiveClosure[V]): TransitiveClosure[V] =
        val tc1 = tc.copy(data = tc.data.updated(s, tc.data.getOrElse(u, SortedSet.empty[V]) + u))
        g.childrenOf(u).foldLeft(tc1) {
          case (tc2, v) =>
            if !tc2.reachable(s,v) then
              dfs(s, v, tc2)
            else
              tc2
        }

      g.vs.foldLeft(TransitiveClosure(SortedMap.empty[V, SortedSet[V]])) {
        case (tc, s) =>
          dfs(s, s, tc)
      }

