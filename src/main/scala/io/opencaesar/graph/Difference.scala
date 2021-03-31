package io.opencaesar.graph

trait Difference[V] {

  def difference(a: V, b: V): V
  
}
