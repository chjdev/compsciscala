package heaps

import scala.annotation.tailrec

sealed trait Binary[T] extends Heap[T, Binary[T]] {
    def height(): Int

    def full(): Boolean

    def decrease_key(v: T): Unit = ???
}

object Binary {

    case class Empty[T](implicit comparator: Ordering[T]) extends Binary[T] {
        override def size() = 0

        override def height() = 0

        override def append(v: T) = singleton(v)(comparator)

        override def full() = false

        override def toString = "Empty"

        override def peek() = None

        override def pop() = (Option.empty, Empty[T])
    }

    case class NonEmpty[T](root: T, left: Binary[T], right: Binary[T])(implicit comparator: Ordering[T]) extends Binary[T] {
        override def size() = left.size() + right.size() + 1

        override def height() = 1 + math.max(left.height(), right.height())

        override def toString() = "(" + root + ": (" + left + ", " + right + "))"

        override def append(v: T): Binary[T] = {
            val nroot = if (comparator.gt(v, root)) v else root
            val nchild = if (root == nroot) v else root
            if (left.full())
                if (right.full()) NonEmpty(nroot, left.append(nchild), right)
                else NonEmpty(nroot, left, right.append(nchild))
            else NonEmpty(nroot, left.append(nchild), right)
        }

        override def full(): Boolean = left.size() == math.pow(2, left.height()) - 1 && right.size() == math.pow(2, right.height()) - 1

        override def peek() = Some(root)

        override def pop(): (Option[T], Binary[T]) = (Some(root), merge(left, right))
    }

    def singleton[T](v: T)(implicit comparator: Ordering[T]) = NonEmpty(v, Empty[T], Empty[T])(comparator)

    @tailrec
    def merge[T](a: Binary[T], b: Binary[T]): Binary[T] = b.pop() match {
        case (Some(head), tail) => merge(a.append(head), tail)
        case _ => a
    }
}

