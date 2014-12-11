package heaps

import org.scalatest.FlatSpec

import scala.annotation.tailrec
import scala.util.Random

trait HeapTest[T <: Heap[Int, T]] extends FlatSpec {
    def empty: T

    def heapSize: Int

    val (heap, maximum) = HeapTest.buildHeap[T](empty, heapSize)

    "peek" should "be empty" in {
        assertResult(None)(Binary.Empty[Int].peek)
    }

    "peek" should "return the maximum element of the heap" in {
        assertResult(maximum)(heap.peek.get)
    }

    "exhaustive pops" should "yield a seq that's sorted in asc order" in {
        val list = HeapTest.buildList(heap)
        assert(list == list.sorted)
    }
}

object HeapTest {

    val rand: Random = new Random(100)

    def buildHeap[T <: Heap[Int, T]](from: T, size: Int): (T, Int) = {
        @tailrec
        def _buildHeap(curSize: Int, from: T, max: Int): (T, Int) =
            if (curSize == 0) (from, max)
            else {
                val gen = rand.nextInt(1000)
                (_buildHeap(curSize - 1, from.append(gen), math.max(gen, max)))
            }
        _buildHeap(size, from, Int.MinValue)
    }

    @tailrec
    def buildList[T <: Heap[Int, T]](heap: T, accu: Seq[Int] = Nil): Seq[Int] = heap.pop match {
        case (Some(head), tail) => buildList(tail, head +: accu)
        case _ => accu
    }
}
