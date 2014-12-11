package heaps

class BinaryTests extends HeapTest[Binary[Int]] {
    def empty = Binary.Empty[Int]
    def heapSize = 64
}
