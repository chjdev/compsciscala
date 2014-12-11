package heaps

class BinaryTests extends HeapTest[Binary[Int]] {
    override def empty = Binary.Empty[Int]

    override def heapSize = 500
}
