package io.opencaesar.graph

import scala.collection.immutable.{Seq,SortedMap,SortedSet}

extension[V : Ordering](g: DiGraph[V])

  /**
   * Depth-first search for all vertices.
   * 
   * @see Introduction to Algorithms, section 22.3
   *      T. Cormen, C. Keiserson, R. Rivest and C. Stein
   *      
   * @return A topological sorting of vertices of g.
   */
  def dfs(): Seq[V] =
    val colors: SortedMap[V, Color] = SortedMap.empty[V, Color] ++ g.vs.toSet.map(v => v -> Color.White)
    dfs(colors, Seq.empty[V])
 
  /**
   * dfs starting from a vertex, v
   */
  def dfs(v: V): Seq[V] =
    require(g.vs.contains(v))
    val colors: SortedMap[V, Color] = SortedMap.empty[V, Color].updated(v, Color.White)
    val result = dfs(colors, Seq.empty[V])
    assert(result.forall(g.vs.contains))
    result

  private def dfs(colors: SortedMap[V, Color], topo: Seq[V]): Seq[V] =
    require(colors.keys.forall(g.vs.contains))
    require(topo.forall(t => g.vs.contains(t) && colors.getOrElse(t, Color.White) == Color.Black))
    colors.find(_._2 == Color.White) match
      case None =>
        topo
      case Some((u, _)) =>
        val (c,t) = dfsVisit(u, g.out(u), colors.updated(u, Color.Gray), topo)
        dfs(c, t)

  private def dfsVisit(u: V, vs: SortedSet[V], colors: SortedMap[V, Color], topo: Seq[V]): (SortedMap[V, Color], Seq[V]) =
    require(g.vs.contains(u))
    require(vs.forall(g.vs.contains))
    require(colors.keys.forall(g.vs.contains))
    require(topo.forall(t => g.vs.contains(t) && colors.getOrElse(t, Color.White) == Color.Black))
    if vs.isEmpty then
      (colors.updated(u, Color.Black), u +: topo)
    else
      val (v, vt) = (vs.head, vs.tail)
      if colors.getOrElse(v, Color.White) == Color.White then
        val (c, t) = dfsVisit(v, g.out(v), colors.updated(v, Color.Gray), topo)
        dfsVisit(u, vt, c, t)
      else
        dfsVisit(u, vt, colors, topo)
  
  
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
    dfs().find(v => directParentsOf(v).size > 1)

