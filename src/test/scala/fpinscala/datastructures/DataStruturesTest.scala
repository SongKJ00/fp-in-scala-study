package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.FunSuite

class DataStruturesTest extends FunSuite {
  test("List.Practice3.1") {
    val x =  List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  test("List.Practice3.2") {
    val x = List(1, 2, 3, 4, 5)
    assert(tail(x) == List(2, 3, 4, 5))
  }

  test("List.Practice3.3") {
    val x = List(1, 2, 3, 4, 5)
    assert(setHead(x, 10) == List(10, 2, 3, 4, 5))
  }

  test("List.Practice3.4") {
    val x = List(1, 2, 3, 4, 5)
    assert(drop(x, 2) == List(3, 4, 5))
  }

  test("List.Practice3.5") {
    val x = List(1, 2, 3, 4, 5)
    assert(dropWhile(x)(x => x <= 3) == List(4, 5))
  }

  test("List.Practice3.6") {
    val x = List(1, 2, 3, 4, 5)
    assert(init(x) == List(1, 2, 3, 4))
  }

  test("List.Practice3.8") {
    val x = List(1, 2, 3, 4, 5)
    assert(length(x) == 5)
  }

  test("List.Practice3.12") {
    val x = List(1, 2, 3)
    assert(reverse(x) == List(3, 2, 1))
  }
}
