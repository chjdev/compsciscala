package heaps

import scala.annotation.tailrec

sealed trait Fibonacci[T] extends Heap[T, Fibonacci[T]] {
    def degree: Int
}

object Fibonacci {

    case class Empty[T](implicit comparator: Ordering[T]) extends Fibonacci[T] {
        override def degree = 0

        override def size = 0

        override def peek = None

        override def append(v: T) = singleton(v)

        override def decrease_key(v: T)(by: Int) = throw new IndexOutOfBoundsException()

        override def pop = (None, Empty[T])

        override def delete(v: T) = throw new IndexOutOfBoundsException()
    }

    type Element[T] = (T, Fibonacci[T])
    type RootSeq[T] = Seq[Element[T]]

    case class NonEmpty[T](roots: RootSeq[T], minIdx: Int)(implicit comparator: Ordering[T]) extends Fibonacci[T] {
        def degree: Int = roots.size

        override def size: Int = roots.map((element) => element._2.size).foldLeft(degree)(_ + _)

        override def peek: Option[T] = roots(minIdx) match {
            case (v, _) => Some(v)
            case _ => None
        }

        override def append(v: T): Fibonacci[T] = merge(singleton(v), this)

        override def decrease_key(v: T)(by: Int): NonEmpty[T] = ???

        implicit class ElementWrapper(element: Element[T]) {
            val (v, NonEmpty(children, _)) = element

            def _link_(other: Element[T]): Element[T] = other match {
                case (vo, NonEmpty(childrenO, _)) if comparator.gt(vo, v) => (vo, NonEmpty(element +: childrenO, 0)) // index???
                case _ => (v, NonEmpty(other +: children, 0)) //index???
            }
        }

        implicit class RootSeqWrapper(seq: RootSeq[T]) {
            def delIndex(idx: Int) = if (seq.size < idx) seq
            else seq.take(idx) ++ seq.drop(idx + 1)

            def minRootIdx = seq.zipWithIndex.minBy(_._1._1)(comparator)._2

            def optimized: RootSeq[T] = {
                @tailrec
                def _optimized(roots: RootSeq[T], accu: Map[Int, Element[T]]): RootSeq[T] = roots match {
                    case head :: tail => {
                        val (_, fib) = head
                        val subDegree = fib.degree
                        accu.get(subDegree) match {
                            case Some(prior) => _optimized((head _link_ prior) +: tail, accu - subDegree)
                            case None => _optimized(tail, accu + (subDegree -> head))
                        }
                    }
                    case _ => accu.values.toSeq
                }
                _optimized(seq, Map.empty)
            }
        }

        override def pop: (Option[T], Fibonacci[T]) = {
            val (minVal, subTree) = roots(minIdx)
            val shortened = roots.delIndex(minIdx)
            val newRoots = subTree match {
                case NonEmpty(children, _) => (children ++ shortened).optimized
                case _ => shortened.optimized
            }
            (Some(minVal), NonEmpty(newRoots, newRoots.minRootIdx))
        }

        override def delete(v: T): Fibonacci[T] = decrease_key(v)(Int.MinValue).pop._2 // real min value????

    }

    def singleton[T](v: T)(implicit comparator: Ordering[T]) = NonEmpty(Seq((v, Empty[T])), 0)

    def merge[T](a: Fibonacci[T], b: Fibonacci[T])(implicit comparator: Ordering[T]): Fibonacci[T] = {
        def newIndex(rootsA: RootSeq[T], minIdxA: Int, rootsB: RootSeq[T], minIdxB: Int): Int =
            if (comparator.gt(rootsA(minIdxA)._1, rootsB(minIdxB)._1)) rootsA.size + minIdxB
            else minIdxA

        (a, b) match {
            case (NonEmpty(rootsA, minIdxA), NonEmpty(rootsB, minIdxB)) =>
                NonEmpty(rootsA ++ rootsB, newIndex(rootsA, minIdxA, rootsB, minIdxB))
            case (NonEmpty(_, _), Empty()) => a
            case _ => b
        }
    }
}
