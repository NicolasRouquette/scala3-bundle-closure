package io.opencaesar.graph

import scala.annotation.unused
import scala.collection.immutable.{Map, Seq, SortedMap, SortedSet}

case class DFS[V : Ordering](
  time: Int,
  d: SortedMap[V, Int],
  f: SortedMap[V, Int],
  colors: SortedMap[V, Color], 
  kind: SortedMap[(V, V), Kind],
  topo: Seq[V])

object DFS:

  def dfsInit[V](vs: Set[V])(using ordering: Ordering[V]): DFS[V] =
    DFS[V](
      time = 0,
      d = SortedMap.empty(ordering),
      f = SortedMap.empty(ordering),
      colors = SortedMap.empty(ordering) ++ vs.map(v => v -> Color.White),
      kind = SortedMap.empty(Ordering.Tuple2(ordering, ordering)),
      topo = Seq.empty)

  extension[V](g: DiGraph[V])(using ordering: Ordering[V])

    /**
     * Depth-first search for all vertices.
     * 
     * @see Introduction to Algorithms, section 22.3
     *      T. Cormen, C. Keiserson, R. Rivest and C. Stein
     *      
     * @return A topological sorting of vertices of g.
     */
    def dfs(): DFS[V] =
      val init = dfsInit(g.vs)
      dfsInternal(init)
  
    /**
     * dfs starting from a vertex, v
     */
    def dfs(v: V): DFS[V] =
      require(g.vs.contains(v))
      val init = dfsInit(Set[V](v))
      dfsInternal(init)

    @annotation.tailrec
    private def dfsInternal(state: DFS[V]): DFS[V] =
      require(state.colors.keys.forall(g.vs.contains))
      require(state.topo.forall(t => g.vs.contains(t) && state.colors.getOrElse(t, Color.White) == Color.Black))
      state.colors.find(_._2 == Color.White) match
        case None =>
          state

        case Some((u, _)) =>
          val update = state.copy(colors = state.colors.updated(u, Color.Gray))
          val next = dfsVisit(u, g.childrenOf(u), update)
          dfsInternal(next)

    private def dfsVisit(u: V, vs: SortedSet[V], s0: DFS[V]): DFS[V] =
      require(g.vs.contains(u))
      require(vs.forall(g.vs.contains))
      require(s0.colors.keys.forall(g.vs.contains))
      require(s0.topo.forall(t => g.vs.contains(t) && s0.colors.getOrElse(t, Color.White) == Color.Black))
      require(vs.forall(v => !s0.kind.contains((u,v))))
      require(!s0.f.contains(u))

      val s1 = 
      if s0.d.contains(u) then
        s0
      else
        val td = 1 + s0.time
        s0.copy(time = td, d = s0.d.updated(u, td))

      if vs.isEmpty then
        val tf = 1 + s1.time
        s1.copy(
          time = tf,
          f = s1.f.updated(u, tf),
          colors = s1.colors.updated(u, Color.Black),
          topo = u +: s1.topo)
      else
        val (v, vt) = (vs.head, vs.tail)
        s1.colors.getOrElse(v, Color.White) match
          case Color.White =>
            val s2 = s1.copy(
              colors = s1.colors.updated(v, Color.Gray),
              kind =  s1.kind.updated((u,v), Kind.Tree))
            val s3 = dfsVisit(v, g.childrenOf(v), s2)
            dfsVisit(u, vt, s3)
          case Color.Gray =>
            val s2 = s1.copy(kind = s1.kind.updated((u,v), Kind.Back))
            dfsVisit(u, vt, s2)
          case Color.Black =>
            val ud = s1.d.getOrElse(u, 0)
            val vd = s1.d.getOrElse(v, 0)
            val s2 = s1.copy(kind = s1.kind.updated((u,v), if ud < vd then Kind.Forward else Kind.Cross))
            dfsVisit(u, vt, s2)
    
    /**
     * Taxonomy.descendantsOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L58
     */
    def descendantsOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      SortedSet.empty[V] ++ dfs(v).topo - v

    /**
     * Taxonomy.directChildrenOf
     * See: https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L62
     */
    def directChildrenOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      val c: SortedSet[V] = g.childrenOf(v)
      val cd: SortedSet[V] = c.flatMap(descendantsOf)
      val result = c -- cd - v
      assert(result.forall(w => g.childrenOf(v).contains(w)))
      result

    /**
     * Taxonomy.ancestorsOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L73
     */
    def ancestorsOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      val rg = g.reverse()
      SortedSet.empty[V] ++ rg.dfs(v).topo - v

    /**
     * Taxonomy.directParentsOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L77
     */
    def directParentsOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      val p: SortedSet[V] = g.parentsOf(v)
      val ap: SortedSet[V] = p.flatMap(ancestorsOf)
      val result = p -- ap - v
      assert(result.forall(w => g.parentsOf(v).contains(w)))
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
    def isolateChildFromOne(child: V, parent: V)(using @unused ops: HasDifference[V]): DiGraph[V] =
      require(g.vs.contains(child))
      require(g.vs.contains(parent))
      if g.parentsOf(parent).isEmpty then
        g
      else
        val diff = parent.difference(child)
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
    def isolateChild(child: V, parents: Set[V])(using ops: HasDifference[V]): DiGraph[V] =
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
    def treeify()(using ops: HasDifference[V]): DiGraph[V] =
      multiParentChild().fold(g) { child =>
        val parents = g.parentsOf(child)
        val bp = bypassParents(child, parents)
        val rd = bp.reduceChild(child)
        val t = rd.isolateChild(child, parents).treeify()
        t
      }

    def isConnected: Boolean =
      val r = g.roots()
      r.size == 1

    def ensureConnected(): Unit =
      val r = g.roots()
      if  r.size > 1 then
        throw UnconnectedTaxonomyException(s"A single connected graph should have only one root vertex instead of ${r.size}.")

    def isTree: Boolean =
      val r = g.roots()
      r.size match
        case 1 =>
          val r0 = r.head
          val s: DFS[V] = dfs(r0)
          val nonTree = s.kind.filter(_._2 != Kind.Tree)
          nonTree.isEmpty

        case _ =>
          false


    def ensureTree(): Unit = 
      val r = g.roots()
      if r.size != 1 then
        throw InvalidTreeException(s"A tree should have a single single root vertex instead of ${r.size}.")
      else
        val r0 = r.head
        val s: DFS[V] = dfs(r0)
        val nonTree = s.kind.filter(_._2 != Kind.Tree)
        if nonTree.nonEmpty then
          throw InvalidTreeException(s"The graph is not a tree because there are ${nonTree.size} non-tree edges starting from the root $r0")
        
      