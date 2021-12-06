package fp

trait Monoid[A]:
  extension (x: A) def |+|(y: A): A
  def empty: A

object Monoid:
  given Monoid[Int] with
    extension (x: Int) def |+|(y: Int): Int = x + y
    def empty: Int = 0

  given [K, V: Monoid]: Monoid[Map[K, V]] with
    extension (x: Map[K, V]) def |+|(y: Map[K, V]): Map[K, V] =
      val ks = x.keySet union y.keySet
      ks.map { k =>
        val xv = x.getOrElse(k, summon[Monoid[V]].empty)
        val yv = y.getOrElse(k, summon[Monoid[V]].empty)
        k -> (xv |+| yv)
      }.toMap

    def empty: Map[K, V] = Map.empty
