package heaps

sealed trait Fibonacci[T] extends Heap[T, Fibonacci[T]]

object Fibonacci {

    case class Empty[T](implicit comparator: Ordering[T]) extends Fibonacci[T] {
        override def size(): Int = 0

        override def peek(): Option[T] = None

        override def append(v: T): Fibonacci[T] = singleton(v)

        override def decrease_key(v: T): Unit = throw new IndexOutOfBoundsException()

        override def pop(): (Option[T], Fibonacci[T]) = throw new NoSuchElementException()
    }

    case class NonEmpty[T](roots: Seq[T])(implicit comparator: Ordering[T]) extends Fibonacci[T] {
        override def size(): Int = ???

        override def peek(): Option[T] = ???

        override def append(v: T): Fibonacci[T] = ???

        override def decrease_key(v: T): Unit = ???

        override def pop(): (Option[T], Fibonacci[T]) = ???
    }

    def singleton[T](v: T)(implicit comparator: Ordering[T]) = NonEmpty(Seq(v))(comparator)
}
