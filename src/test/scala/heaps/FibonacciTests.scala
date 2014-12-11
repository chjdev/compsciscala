package heaps

class FibonacciTests extends HeapTest[Fibonacci[Int]] {

    implicit object Compare extends Ordering[Int] {
        override def compare(x: Int, y: Int): Int = if (x < y) 1 else -1
    }

    def empty = Fibonacci.Empty[Int]

    def heapSize = 500
}
