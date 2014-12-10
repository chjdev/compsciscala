package heaps

import org.scalatest._

import scala.annotation.tailrec
import scala.util.Random

class BinaryTests extends FlatSpec {
    val rand: Random = new Random(100)

    def buildHeap(size: Int): (Binary[Int], Int) = {
        @tailrec
        def _buildHeap(size: Int, from: Binary[Int], max: Int): (Binary[Int], Int) = size match {
            case 0 => (from, max)
            case size => {
                val gen = rand.nextInt(1000)
                (_buildHeap(size - 1, from.append(gen), math.max(gen, max)))
            }
        }
        _buildHeap(size, Binary.Empty(), Int.MinValue)
    }

    val (heap, maximum) = buildHeap(64)

    "peek" should "be empty" in {
        assertResult(None)(Binary.Empty().peek())
    }

    "peek" should "return the maximum element of the heap" in {
        assertResult(heap.peek().get)(maximum)
    }

    "exhaustive pops" should "yield a seq that's sorted in asc order" in {
        @tailrec
        def buildList[S <: Heap[Int, S]](heap: S, accu: Seq[Int]): Seq[Int] = heap.pop() match {
            case (Some(head), tail) => buildList(tail, head +: accu)
            case _ => accu
        }
        val list = buildList(heap, Nil)
        assert(list == list.sorted)
    }

}
