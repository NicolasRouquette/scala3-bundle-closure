package io.opencaesar.graph

trait HasDifference[V]:

  extension (x: V) def difference(y: V): V
