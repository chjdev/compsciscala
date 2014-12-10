package heaps

sealed trait Fibonacci[T] extends Heap[T, Fibonacci[T]]

object Fibonacci {

    case class Empty[T](implicit comparator: Ordering[T]) extends Fibonacci[T] {
        override def size(): Int = ???

        override def peek(): Option[T] = ???

        override def append(v: T): Fibonacci[T] = ???

        override def decrease_key(v: T): Unit = ???

        override def pop(): (Option[T], Fibonacci[T]) = ???
    }

    case class NonEmpty[T](implicit comparator: Ordering[T]) extends Fibonacci[T] {
        override def size(): Int = ???

        override def peek(): Option[T] = ???

        override def append(v: T): Fibonacci[T] = ???

        override def decrease_key(v: T): Unit = ???

        override def pop(): (Option[T], Fibonacci[T]) = ???
    }

}
