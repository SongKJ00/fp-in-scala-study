package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)
  // Practice 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Practice 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }

  // Practice 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }

  // Practice 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @scala.annotation.tailrec
    def go(l: List[A], n: Int): List[A] =
      if(n <= 0) l
      else
        l match {
          case Nil => Nil
          case Cons(h, t) => go(t, n-1)
        }
    go(l, n)
  }

  // Practice 3.5
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    @scala.annotation.tailrec
    def go(l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(h, t) if f(h) => go(t, f)
        case _ => l    // return immediately if f is false or Nil
      }
    go(l, f)
  }

  // Practice 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // Practice 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => 1 + acc)

  // Practice 3.10
  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // Practice 3.11
  def sum3[A](ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3[A](ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  // Practice 3.12
  def reverse[A](l: List[A]) =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
}
