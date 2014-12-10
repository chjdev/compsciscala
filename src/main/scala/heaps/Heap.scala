package heaps

trait Heap[T, Self <: Heap[T, Self]] {
    def size: Int

    def append(v: T): Self

    def peek: Option[T]

    def pop: (Option[T], Self)

    @throws[IndexOutOfBoundsException]("If Heap is empty or key element cannot be found")
    def decrease_key(v: T)(by: Int = 1): Self

    @throws[IndexOutOfBoundsException]("If Heap is empty or key element cannot be found")
    def delete(v: T): Self
}


