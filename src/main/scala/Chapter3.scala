/**
 * @author gvince01
 *
 */

object Chapter3 extends App {

  case class Cons[+A](override val head: A, override val tail: List[A]) extends List[A]

  def tail[A](lst: List[A]): List[A] = lst match {
    case h :: t => t
    case Nil => List()
  }

  def setHead[A](lst: List[A], a: A) = lst match {
    case h :: t => a :: t
    case _ => List(a)
  }

  def drop[A](lst: List[A], n: Int): List[A] = {
    def helper(index: Int, acc: List[A]): List[A] = {
      if (index >= n || acc.isEmpty) acc
      else helper(index + 1, tail(lst))
    }
    helper(0, lst)
  }


  def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = {
    def helper(acc: List[A]): List[A] = {
      if (f(acc.head)) helper(tail(acc))
      else acc
    }
    helper(lst)
  }

  def init[A](l: List[A]): List[A] = l match {
    case a :: Nil => Nil
    case h :: t => h :: init(t)
  }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }


  def length[A](l: List[A]): Int = {

  }
}
