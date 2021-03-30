package io.opencaesar.graph

import scala.collection.immutable.{SortedMap,SortedSet}

/**
 * Directed Graph
 * 
 * @param vs nodes
 * @param es edges (ordered pair of nodes)
 * @param inNeighbors incoming neighbors of a node: u is inNeighbors(v) iff (u,v) is in es
 * @param outNeighbors outgoing neighbors of a node: v is outNeighbors(u) iff (u,v) is in es
 * @param ordering given ordering of the type of nodes, V
 * @tparam V type of graph nodes
 */
case class DiGraph[V : Ordering]
(vs: SortedSet[V],
 es: SortedSet[(V, V)],
 inNeighbors: SortedMap[V, SortedSet[V]],
 outNeighbors: SortedMap[V, SortedSet[V]]):
    require(DiGraph.hasAllVertices(vs, es.map(_._1).toSet))
    require(DiGraph.hasAllVertices(vs, es.map(_._2).toSet))
    require(DiGraph.hasAllVertices(vs, inNeighbors.keySet))
    require(inNeighbors.values.forall(DiGraph.hasAllVertices(vs, _)))
    require(DiGraph.hasAllVertices(vs, outNeighbors.keySet))
    require(outNeighbors.values.forall(DiGraph.hasAllVertices(vs, _)))

    /**
     * Add a node to the graph
     * @param v node
     * @return a new graph whose nodes includes v
     */
    def addVertex(v: V): DiGraph[V] =
      if vs.contains(v) then
        this
      else
        copy(
          vs = vs + v,
          inNeighbors = inNeighbors + (v -> SortedSet.empty[V]),
          outNeighbors = outNeighbors + (v -> SortedSet.empty[V]))

    /**
     * Add an edge to the graph from a source node to a target node
     * 
     * @param x source node
     * @param y target node
     * @return a new graph whose edges includes (x,y)
     */
    def addEdge(x: V, y: V): DiGraph[V] =
      val g1 = addVertex(x).addVertex(y)
      val g2 = g1.copy(
        es = g1.es + (x -> y),
        inNeighbors = g1.inNeighbors.updated(y, g1.in(y) + x),
        outNeighbors = g1.outNeighbors.updated(x, g1.out(x) + y))
      assert(g2.in(y).contains(x))
      assert(g2.out(x).contains(y))
      g2

    /**
     * Delete an edge from a source node to a target node
     * 
     * @param x source node
     * @param y target node
     * @return a new graph without an edge (x,y)
     */
    def deleteEdge(x: V, y: V): DiGraph[V] =
      if es.contains((x,y)) then
        copy(
          es = es - (x->y),
          inNeighbors = inNeighbors.updated(y, in(y) - x),
          outNeighbors = outNeighbors.updated(x, out(x) - y))
      else
        this

    /**
     * Query the in neighbors of a target node 
     * 
     * Taxonomy.parentsOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L69
     * 
     * @param v target node
     * @return all the source nodes for which there is an edge to the target node
     */
    def in(v: V): SortedSet[V] =
      require(vs.contains(v))
      val result = inNeighbors.getOrElse(v, SortedSet.empty[V])
      assert(result.forall(vs.contains))
      result

    /**
     * Query the neighbors of a source node
     * 
     * Taxonomy.childrenOf
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L54
     * 
     * @param v source node
     * @return all the target nodes for which there is an edge from the source node
     */
    def out(v: V): SortedSet[V] =
      require(vs.contains(v))
      val result = outNeighbors.getOrElse(v, SortedSet.empty[V])
      assert(result.forall(vs.contains))
      result

    def reverse(): DiGraph[V] =
      val r1 = vs.foldLeft(DiGraph.empty)(_.addVertex(_))
      val r2 = es.foldLeft(r1){ case (ri, (x,y)) =>
        ri.addEdge(y,x)
      }
      assert(r2.vs == vs)
      assert(r2.es.forall{ case (y,x) => es.contains((x,y))})
      r2

    /**
     * Taxonomy.rootAt
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L139
     */
    def rootAt(root: V): DiGraph[V] =
      require(!vs.contains(root))
      val g =  vs.filter(v => in(v).isEmpty).foldLeft(addVertex(root))(_.addEdge(root, _))
      assert(g.vs.size == 1 + vs.size)
      g

    /**
     * Taxonomy.exciseVertex
     * @see https://github.com/opencaesar/owl-tools/blob/e1d7708d206fa262aeea5d96cbc69366487748b5/owl-close-world/src/main/java/io/opencaesar/closeworld/Taxonomy.java#L94
     */
    def exciseVertex(v: V): DiGraph[V] =
      require(vs.contains(v))
      // copy all vertices except v
      val g1 = (vs - v).foldLeft(DiGraph.empty)(_.addVertex(_))
      // remember children of v, parents of v and copy all edges not involving v
      val (children: SortedSet[V], parents: SortedSet[V], g2) = 
      es.foldLeft((SortedSet.empty[V], SortedSet.empty[V], g1)) {
        case ((ci, pi, gi), (s, t)) =>
          if s == v then
            (ci + t, pi, gi)
          else if t == v then
            (ci, pi + s, gi)
          else
            (ci, pi, gi.addEdge(s,t))
      }
      // add edges from parents to children
      val result = parents.foldLeft(g2) { (gi, p) =>
        children.foldLeft(gi) { (gj, c) =>
          gj.addEdge(p,c)
        }
      }
      assert(!result.vs.contains(v))
      assert(result.es.forall((s,t) => s != v && s != t))
      result

object DiGraph:

  /**
   * Predicate to check whether a set of nodes contains a subset of nodes
   * 
   * @param vs set of nodes
   * @param x subset of nodes
   * @tparam V node type
   * @return true if all nodes in the subset are in the set of nodes
   */
  def hasAllVertices[V: Ordering](vs: SortedSet[V], x: Set[V]): Boolean = 
    x.forall(vs.contains)

  /**
   * Constructor for an empty directed graph
   * 
   * @tparam V type of node
   * @return a new empty directed graph
   */
  def empty[V:Ordering]: DiGraph[V] =
    DiGraph[V](
      vs = SortedSet.empty[V], 
      es = SortedSet.empty[(V, V)], 
      inNeighbors = SortedMap.empty[V, SortedSet[V]], 
      outNeighbors = SortedMap.empty[V, SortedSet[V]])