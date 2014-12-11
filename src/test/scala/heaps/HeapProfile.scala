package heaps

import java.lang.System.{currentTimeMillis => _time}

import org.scalatest.FlatSpec

object HeapProfile extends App {
    def profile[R](code: => R, t: Long = _time) = (code, _time - t)

    def printProfile[R](code: => R, t: Long = _time): Unit = println(profile(code))

    def profileTest[T <: FlatSpec](tests: T): Unit = printProfile(tests.execute())

    profileTest(new BinaryTests)
    profileTest(new FibonacciTests)

}
