package fpinscala.gettingstarted

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // This definition and `formatAbs` are very similar..
  private def formatFactorial(n: Int): String = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println("2.1 fib : " + fib(5))
  }

  // Practice 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int = {
      if(n == 0) prev
      else go(n-1, cur, prev+cur)
    }
    go(n, 0, 1);
  }
}

object MonomorphicFunctions {
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n+1)

    loop(0)
  }
}

object PolymorphicFunctions {
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if(n >= as.length) -1
      else if(p(as(n))) n
      else loop(n+1)

    loop(0)
  }

  // Practice 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if(n >= as.length-1) true
      else if(!ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  // Practice 2.3
  def curry[A,B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Practice 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Practice 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
    // or, f compose g
}
