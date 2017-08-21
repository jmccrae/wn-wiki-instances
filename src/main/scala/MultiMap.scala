object MultiMap {
  import scala.language.higherKinds
  implicit class MultiMapFromTraversable[A, B, T[X] <: scala.collection.TraversableLike[X,T[X]]](t : T[(A, B)]) {
    def toMultiMap(implicit cbf: scala.collection.generic.CanBuildFrom[T[A],B,T[B]], cbf2 : scala.collection.generic.CanBuildFrom[T[(A,B)], B, T[B]]) : Map[A, T[B]] = {
      t.groupBy(_._1).mapValues(_.map(_._2)) } }

  implicit class MultiMapFromIterator[A, B](t : Iterator[(A,B)]) {
    def toMultiMap : Map[A, Set[B]] = {
      t.toSeq.groupBy(_._1).mapValues(_.map(_._2).toSet) } }
}


object AutoClose {
  def withResource[A,C <: java.io.Closeable](c : C)(foo : C => A) : A = {
    val a = foo(c)
    c.close
    a
  }
}

