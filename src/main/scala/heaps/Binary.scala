package heaps

import scala.annotation.tailrec

/** WARNING: implemented very inefficiently */
sealed trait Binary[T] extends Heap[T, Binary[T]] {
    def height: Int

    def full: Boolean

    def decrease_key(v: T)(by: Int) = ???

    def delete(v: T) = ???
}

object Binary {

    case class Empty[T](implicit comparator: Ordering[T]) extends Binary[T] {
        override val size = 0

        override val height = 0

        override def append(v: T) = singleton(v)(comparator)

        override val full = false

        override val toString = "Empty"

        override val peek = None

        override lazy val pop = (None, Empty[T])
    }

    case class NonEmpty[T](root: T, left: Binary[T], right: Binary[T])(implicit comparator: Ordering[T]) extends Binary[T] {
        override lazy val size = left.size + right.size + 1

        override lazy val height = 1 + math.max(left.height, right.height)

        override def toString = "(" + root + ": (" + left + ", " + right + "))"

        override def append(v: T): Binary[T] = {
            val nroot = if (comparator.gt(v, root)) v else root
            val nchild = if (root == nroot) v else root
            if (left.full)
                if (right.full) NonEmpty(nroot, left.append(nchild), right)
                else NonEmpty(nroot, left, right.append(nchild))
            else NonEmpty(nroot, left.append(nchild), right)
        }

        override lazy val full: Boolean = left.size == math.pow(2, left.height) - 1 && right.size == math.pow(2, right.height) - 1

        override val peek = Some(root)

        override lazy val pop: (Option[T], Binary[T]) = (Some(root), merge(left, right))
    }

    def singleton[T](v: T)(implicit comparator: Ordering[T]) = NonEmpty(v, Empty[T], Empty[T])(comparator)

    @tailrec
    def merge[T](a: Binary[T], b: Binary[T]): Binary[T] = b.pop match {
        case (Some(head), tail) => merge(a.append(head), tail)
        case _ => a
    }
}

