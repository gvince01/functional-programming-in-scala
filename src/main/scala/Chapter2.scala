/**
 * @author gvince01
 *
 */

object Chapter2 extends App {

  def fib(n: Int): Int = {
    def helper(index: Int, prev: Int, acc: Int): Int = {
      index match {
        case _ if index == 0 =>
          acc

        case _ =>
          helper(index - 1, prev + acc, prev)
      }
    }

    helper(n, prev = 1, acc = 0)
  }

  /**
   * Implemented isSorted, which checks whether an Array[A] is sorted according to a given
   * comparison function
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def helper(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) helper(n + 1)
      else false
    }
    helper(0)
  }


  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
