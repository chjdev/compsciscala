package heaps

import java.lang.System.{currentTimeMillis => _time}

import org.scalatest.FlatSpec

import scala.collection.mutable
import scala.util.Random

case class PriorityQueueBaseline(size: Int) {
    val rand = new Random(100)
    val queue = mutable.PriorityQueue[Int]()
    for (_ <- 0 to size) {
        queue.enqueue(rand.nextInt)
    }

    def runThrough(accu: Seq[Int] = Nil): Seq[Int] =
        if (queue.isEmpty) accu
        else runThrough(queue.dequeue +: accu)

    val sorted = runThrough()
}

case class SeqBaseline(size: Int) {
    val rand = new Random(100)
    val seq: Seq[Int] = (0 to size) map (_ => rand.nextInt)

    def runThrough(remaining: Seq[Int], result: Seq[Int] = Nil): Seq[Int] =
        if (remaining.isEmpty) result
        else {
            val min = remaining.min
            runThrough(remaining.filterNot(_ == min), min +: result)
        }

    val sorted = runThrough(seq)
}

case class FibonacciBaseline(size: Int) {
    val (heap, maximum) = HeapTest.buildHeap[Fibonacci[Int]](Fibonacci.Empty[Int], size)
    val sorted = HeapTest.buildList(heap)
}

object HeapProfile extends App {
    def profile[R](code: => R, t: Long = _time) = (code, _time - t)

    def profileTest[T <: FlatSpec](tests: T): Unit = {
        val (_, time) = profile(tests.execute)
        printf("%s >> %dms\n", tests.suiteName, time)
    }

    profileTest(new BinaryTests)
    profileTest(new FibonacciTests)
    println()
    printf("Pri Baseline >> %s\n", profile(PriorityQueueBaseline(500)))
    printf("Seq Baseline >> %s\n", profile(SeqBaseline(500)))
    printf("Fib Baseline >> %s\n", profile(FibonacciBaseline(500)))

}
