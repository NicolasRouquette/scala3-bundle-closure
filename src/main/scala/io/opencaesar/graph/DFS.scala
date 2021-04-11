package io.opencaesar.graph

import scala.collection.immutable.{Map,Seq,SortedMap,SortedSet,Vector}

/**
 * DFS from Introduction to Algorithms, 3rd edition by Cormen et al.
 * Chapter 22.3
 *
 */
case class DFS[V : Ordering](
  d: SortedMap[V, Int],
  low: SortedMap[V, Int],
  f: SortedMap[V, Int],
  kind: SortedMap[(V, V), Kind],
  topo: Seq[V],
  scc: SortedMap[V, DiGraph[V]])

object DFS:

  case class State[V : Ordering](
    time: Int,
    colors: SortedMap[V, Color],
    stack: Vector[V])

  def dfsInit[V : Ordering](vs: Set[V]): (DFS[V], State[V]) =
    Tuple2(
      DFS[V](
        d = SortedMap.empty,
        low = SortedMap.empty,
        f = SortedMap.empty,
        kind = SortedMap.empty,
        topo = Seq.empty,
        scc = SortedMap.empty),
      State[V](
        time = 0,
        colors = SortedMap.empty ++ vs.map(v => v -> Color.White),
        stack = Vector.empty))

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
      val (init, s0) = dfsInit(g.vs)
      dfs(init, s0)
  
    /**
     * dfs starting from a vertex, v
     */
    def dfs(v: V): DFS[V] =
      require(g.vs.contains(v))
      val (init, s0) = dfsInit(Set[V](v))
      dfs(init, s0)

    def dfs(init: DFS[V], s0: State[V]): DFS[V] =
      require(s0.colors.keys.forall(g.vs.contains))
      require(init.topo.forall(t => g.vs.contains(t) && s0.colors.getOrElse(t, Color.White) == Color.Black))
      s0.colors.find(_._2 == Color.White) match
        case None =>
          init

        case Some((u, _)) =>
          val s1 = s0.copy(colors = s0.colors.updated(u, Color.Gray))
          val (next, s2) = dfsVisit(u, g.childrenOf(u), init, s1)
          dfs(next, s2)

    final def dfsVisit(u: V, vs: SortedSet[V], i0: DFS[V], s0: State[V]): (DFS[V], State[V]) =
      require(g.vs.contains(u))
      require(vs.forall(g.vs.contains))
      require(s0.colors.keys.forall(g.vs.contains))
      require(i0.topo.forall(t => g.vs.contains(t) && s0.colors.getOrElse(t, Color.White) == Color.Black))
      require(vs.forall(v => !i0.kind.contains((u,v))))
      require(!i0.f.contains(u))

      val (i1, s1) =
      if i0.d.contains(u) then
        Tuple2(i0, s0)
      else
        val td = 1 + s0.time
        Tuple2(
          i0.copy(
            d = i0.d.updated(u, td),
            low = i0.low.updated(u, td)),
          s0.copy(
            time = td,
            stack = s0.stack.prepended(u)))

      if vs.isEmpty then

        val tf = 1 + s1.time
        val i2 = i1.copy(
          f = i1.f.updated(u, tf),
          topo = u +: i1.topo)
        val s2 = s1.copy(
          time = tf,
          colors = s1.colors.updated(u, Color.Black))
        assert(i2.low.contains(u))
        assert(i2.d.contains(u))
        if i2.low(u) == i2.d(u) then
          val n = s2.stack.indexOf(u)
          val scc_vs = SortedSet(u) ++ s2.stack.take(n)
          val scc_es = g.es.filter { case (e1,e2) => scc_vs.contains(e1) && scc_vs.contains(e2) }
          val ssc_g0 = scc_vs.foldLeft(DiGraph.empty[V])(_.addVertex(_))
          val ssc_g1 = scc_es.foldLeft(ssc_g0)(_.addEdge.tupled(_))
          Tuple2(
            i2.copy(scc = i2.scc.updated(u, ssc_g1)),
            s2.copy(stack = s2.stack.drop(n+1)))
        else
          Tuple2(i2, s2)

      else

        val (v, vt) = (vs.head, vs.tail)
        s1.colors.getOrElse(v, Color.White) match
          case Color.White =>
            val i2 = i1.copy(kind =  i1.kind.updated((u,v), Kind.Tree))
            val s2 = s1.copy(colors = s1.colors.updated(v, Color.Gray))
            val (i3, s3) = dfsVisit(v, g.childrenOf(v), i2, s2)
            assert(i3.low.contains(u))
            assert(i3.low.contains(v))
            val i4 = i3.copy(low = i3.low.updated(u, i3.low(u).min(i3.low(v))))
            dfsVisit(u, vt, i4, s3)

          case Color.Gray =>
            assert(i1.low.contains(u))
            assert(i1.d.contains(v))
            val (i2, s2) =
              Tuple2(
                i1.copy(
                  low = if s1.stack.contains(v) then i1.low.updated(u, i1.low(u).min(i1.d(v))) else i1.low,
                  kind = i1.kind.updated((u,v), Kind.Back)),
                s1)
            dfsVisit(u, vt, i2, s2)

          case Color.Black =>
            assert(i1.low.contains(u))
            assert(i1.d.contains(u))
            val ud = i1.d(u)
            assert(i1.d.contains(v))
            val vd = i1.d(v)
            val (i2, s2) =
              Tuple2(
                i1.copy(
                  low = if s1.stack.contains(v) then i1.low.updated(u, ud.min(vd)) else i1.low,
                  kind = i1.kind.updated((u,v), if ud < vd then Kind.Forward else Kind.Cross)),
                s1)
            dfsVisit(u, vt, i2, s2)
    
    /**
     * Taxonomy.descendantsOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L58
     */
    def descendantsOf(v: V): SortedSet[V] =
      require(g.vs.contains(v))
      SortedSet.empty[V] ++ dfs(v).topo - v

    /*
     * Taxonomy.directChildrenOf
     * @see: https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L62
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
    def isolateChildFromOne(child: V, parent: V)(using ops: HasDifference[V]): DiGraph[V] =
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
        
    def transitiveClosure(): DiGraph[V] =
      val info = dfs()
      info.topo.reverse
      g