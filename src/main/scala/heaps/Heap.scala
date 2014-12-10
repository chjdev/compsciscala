package heaps

trait Heap[T, Self <: Heap[T, Self]] {
    def size(): Int

    def append(v: T): Self

    def peek(): Option[T]

    def pop(): (Option[T], Self)

    def decrease_key(v: T): Unit
}

