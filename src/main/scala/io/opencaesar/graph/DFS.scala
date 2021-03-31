package io.opencaesar.graph

import scala.collection.immutable.{Map,Seq,SortedMap,SortedSet}

case class DFS[V : Ordering](
  colors: SortedMap[V, Color], 
  kind: Map[(V, V), Kind] = Map.empty[(V, V), Kind],
  topo: Seq[V] = Seq.empty[V])

object DFS:

  extension[V : Ordering](g: DiGraph[V])

    /**
     * Depth-first search for all vertices.
     * 
     * @see Introduction to Algorithms, section 22.3
     *      T. Cormen, C. Keiserson, R. Rivest and C. Stein
     *      
     * @return A topological sorting of vertices of g.
     */
    def dfs(): DFS[V] =
      val init = DFS(colors = SortedMap.empty[V, Color] ++ g.vs.toSet.map(v => v -> Color.White))
      dfs(init)
  
    /**
     * dfs starting from a vertex, v
     */
    def dfs(v: V): Seq[V] =
      require(g.vs.contains(v))
      val init = DFS(colors = SortedMap.empty[V, Color].updated(v, Color.White))
      val result = dfs(init) 
      result.topo

    private def dfs(state: DFS[V]): DFS[V] =
      require(state.colors.keys.forall(g.vs.contains))
      require(state.topo.forall(t => g.vs.contains(t) && state.colors.getOrElse(t, Color.White) == Color.Black))
      state.colors.find(_._2 == Color.White) match
        case None =>
          state

        case Some((u, _)) =>
          val update = state.copy(colors = state.colors.updated(u, Color.Gray))
          val next = dfsVisit(u, g.out(u), update)
          dfs(next)

    private def dfsVisit(u: V, vs: SortedSet[V], state: DFS[V]): DFS[V] =
      require(g.vs.contains(u))
      require(vs.forall(g.vs.contains))
      require(state.colors.keys.forall(g.vs.contains))
      require(state.topo.forall(t => g.vs.contains(t) && state.colors.getOrElse(t, Color.White) == Color.Black))
      require(vs.forall(v => !state.kind.contains((u,v))))
      if vs.isEmpty then
        state.copy(
          colors = state.colors.updated(u, Color.Black),
          topo = u +: state.topo)
      else
        val (v, vt) = (vs.head, vs.tail)
        state.colors.getOrElse(v, Color.White) match {
          case Color.White =>
            val update = state.copy(
              colors = state.colors.updated(v, Color.Gray),
              kind =  state.kind.updated((u,v), Kind.Tree))
            val next = dfsVisit(v, g.out(v), update)
            dfsVisit(u, vt, next)
          case Color.Gray =>
            val update = state.copy(kind = state.kind.updated((u,v), Kind.Back))
            dfsVisit(u, vt, update)
          case Color.Black =>
            val update = state.copy(kind = state.kind.updated((u,v), Kind.ForwardOrCross))
            dfsVisit(u, vt, update)
        }
    
    /**
     * Taxonomy.descendantsOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L58
     */
    def descendantsOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      SortedSet.empty[V] ++ dfs(v) - v

    /**
     * Taxonomy.directChildrenOf
     * @see: https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L62
     */
    def directChildrenOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      val c: SortedSet[V] = g.out(v)
      val cd: SortedSet[V] = c.flatMap(descendantsOf)
      val result = c -- cd - v
      assert(result.forall(w => g.out(v).contains(w)))
      result

    /**
     * Taxonomy.ancestorsOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L73
     */
    def ancestorsOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      val rg = g.reverse()
      SortedSet.empty[V] ++ rg.dfs(v) - v

    /**
     * Taxonomy.directParentsOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L77
     */
    def directParentsOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      val p: SortedSet[V] = g.in(v)
      val ap: SortedSet[V] = p.flatMap(ancestorsOf)
      val result = p -- ap - v
      assert(result.forall(w => g.in(v).contains(w)))
      result

    /**
     * Taxonomy.multiParentChild
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L84
     */
    def multiParentChild(): Option[V] =
      dfs().topo.find(v => directParentsOf(v).size > 1)

    /**
     * Taxonomy.bypassParent
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L162
     * A new graph with the edge from parent to child removed and new edges from the direct grandparents to the child added.
     */
    def bypassParent(child: V, parent: V): DiGraph[V] =
      require(g.vs.contains(child))
      require(g.vs.contains(parent))
      // copy all vertices including bypassed parent
      val g1 = g.vs.foldLeft(DiGraph.empty)(_.addVertex(_))
      // copy all edges except from parent to child
      val g2 = (g.es - (parent -> child)).foldLeft(g1)(_.addEdge.tupled(_))
      // add edges from direct grandparents to child
      val g3 = directParentsOf(parent).foldLeft(g2)(_.addEdge(_,child))
      assert(g3.vs.contains(parent))
      assert(g3.vs.contains(child))
      g3

    /**
     * Taxonomy.bypassParents
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L189
     */
    def bypassParents(child: V, parents: Set[V]): DiGraph[V] =
      require(g.vs.contains(child))
      require(parents.forall(g.vs.contains))
      if parents.isEmpty then
        g
      else
        val (first, rest) = (parents.head, parents.tail)
        bypassParent(child, first).bypassParents(child, rest)

    /**
     * Taxonomy.reduceChild
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L205
     */
    def reduceChild(child: V): DiGraph[V] =
      require(g.vs.contains(child))
      // copy all vertices
      val g1 = g.vs.foldLeft(DiGraph.empty)(_.addVertex(_))
      // copy all edges except those to the child
      val g2 = g.es.filter(_._2 != child).foldLeft(g1)(_.addEdge.tupled(_))
      // eliminate redundant edges above child
      val g3 = directParentsOf(child).foldLeft(g2)(_.addEdge(_, child))
      assert(g3.vs == g.vs)
      assert(g3.es.size <= g.es.size)
      g3

    /**
     * Taxonomy.isolateChildFromOne
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L232
     */
    def isolateChildFromOne(child: V, parent: V)(using ops: Difference[V]): DiGraph[V] =
      require(g.vs.contains(child))
      require(g.vs.contains(parent))
      if g.in(parent).isEmpty then
        g
      else
        val diff = ops.difference(parent, child)
        val g1 = (g.vs - parent + diff).foldLeft(DiGraph.empty)(_.addVertex(_))
        val g2 = g.es.foldLeft(g1) { case (gi, (s,t)) =>
          if s == parent then
            if t != child then
              gi.addEdge(diff, t)
            else
              gi
          else if t == parent then
            gi.addEdge(s, diff)
          else
            gi.addEdge(s, t)
        }
        g2

    /**
     * Taxonomy.isolateChild
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L268
     */
    def isolateChild(child: V, parents: Set[V])(using ops: Difference[V]): DiGraph[V] =
      require(g.vs.contains(child))
      require(parents.forall(g.vs.contains))
      if parents.isEmpty then
        g
      else
        val (first, rest) = (parents.head, parents.tail)
        isolateChildFromOne(child, first).isolateChild(child, rest)

    /**
     * Taxonomy.treeify
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L283
     */
    def treeify()(using ops: Difference[V]): DiGraph[V] =
      multiParentChild().fold(g) { child =>
        val parents = g.in(child)
        val bp = bypassParents(child, parents)
        val rd = bp.reduceChild(child)
        val t = rd.isolateChild(child, parents).treeify()
        t
      }