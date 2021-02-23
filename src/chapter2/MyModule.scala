package chapter2

import scala.annotation.tailrec

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x));
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1);
  }

  def fib(n: Int): Int = {
    @tailrec def loop(n: Int, previous: Int, current: Int): Int =
      if (n == 0) previous
      else loop(n - 1, current, previous + current)

    loop(n, 0, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0);
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0);
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))

    println(isSorted[Int](Array(1, 2, 3, 4), (x, y) => x < y))
    println(isSorted[Int](Array(1, 3, 2, 4), (x, y) => x < y))
    println(isSorted[Int](Array(4, 3, 2, 1), (x, y) => x > y))
  }
}
