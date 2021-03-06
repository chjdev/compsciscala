package heaps

import scala.annotation.tailrec

sealed trait Fibonacci[T] extends Heap[T, Fibonacci[T]] {
    def degree: Int
}

object Fibonacci {

    case class Empty[T](implicit comparator: Ordering[T]) extends Fibonacci[T] {
        override val degree = 0

        override val size = 0

        override val peek = None

        override def append(v: T) = singleton(v)

        override def decrease_key(v: T)(by: Int) = throw new IndexOutOfBoundsException()

        override lazy val pop = (None, Empty[T])

        override def delete(v: T) = throw new IndexOutOfBoundsException()
    }

    type Element[T] = (T, Fibonacci[T])
    type RootSeq[T] = Seq[Element[T]]

    case class NonEmpty[T](roots: RootSeq[T], minIdx: Int)(implicit comparator: Ordering[T]) extends Fibonacci[T] {
        override val degree: Int = roots.size

        override lazy val size: Int = roots.map((element) => element._2.size).foldLeft(degree)(_ + _)

        override def peek: Option[T] = roots(minIdx) match {
            case (v, _) => Some(v)
            case _ => None
        }

        override def append(v: T): Fibonacci[T] = merge(singleton(v), this)

        override def decrease_key(v: T)(by: Int): NonEmpty[T] = ???

        implicit class ElementWrapper(element: Element[T]) {
            val (v, roots) = element
            val children = roots match {
                case NonEmpty(children, _) => children
                case Empty() => Seq()
            }

            def withMe(subTree: Fibonacci[T]): RootSeq[T] = subTree match {
                case NonEmpty(otherChildren, _) => element +: otherChildren
                case _ => Seq(element)
            }

            def ?+?(other: Element[T]): Element[T] = {
                other match {
                    case (otherv, subTree) if comparator.lt(otherv, v) =>
                        (otherv, NonEmpty(withMe(subTree), -1)) /// ??? index
                    case _ => (v, NonEmpty(other +: children, -1)) //index???
                }
            }

            def !+! : PartialFunction[Element[T], Element[T]] = {
                case other if (element._2.degree == other._2.degree) => ?+?(other)
            }

        }

        implicit class RootSeqWrapper(seq: RootSeq[T]) {
            def delIndex(idx: Int) = if (seq.size < idx) seq else seq.take(idx) ++ seq.drop(idx + 1)

            lazy val minRootIdx: Int = seq.zipWithIndex.minBy(_._1._1)(comparator)._2

            lazy val optimized: RootSeq[T] = {
                @tailrec
                def _optimized(roots: RootSeq[T], accu: Map[Int, Element[T]]): RootSeq[T] = roots match {
                    case head +: tail => {
                        val (_, fib) = head
                        val subDegree = fib.degree
                        accu.get(subDegree) match {
                            case Some(prior) => _optimized((head !+! prior) +: tail, accu - subDegree)
                            case None => _optimized(tail, accu + (subDegree -> head))
                        }
                    }
                    case _ => accu.values.toSeq
                }
                if (roots.isEmpty) roots
                else _optimized(seq, Map.empty)
            }
        }

        override lazy val pop: (Option[T], Fibonacci[T]) = {
            val (minVal, subTree) = roots(minIdx)
            val shortened = roots.delIndex(minIdx)
            val newRoots = subTree match {
                case NonEmpty(children, _) => (children ++ shortened)
                case _ => shortened
            }
            (Some(minVal), fromRoots(newRoots))
        }

        def fromRoots(roots: RootSeq[T]): Fibonacci[T] =
            if (roots.isEmpty) Empty[T]
            else {
                val optimizedRoots = roots.optimized
                NonEmpty(optimizedRoots, optimizedRoots.minRootIdx)
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
