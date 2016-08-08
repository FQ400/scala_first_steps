def abs(n: Int): Int = if (n < 0) -n else n

abs(-1)

def formatAbs(n: Int): String = {
  val msg = "The absolute value of %d is %d."
  msg.format(n, abs(n))
}

formatAbs(-22)

def factorial(n: Int): Int = {
  def go(n: Int, acc: Int): Int = {
    if (n <= 0) acc
    else go(n - 1, n * acc)
  }

  go(n, 1)
}

def fib(n: Int): Int = {
  // 0, 1, 1, 2, 3, 5
  // fib(1) -> 0
  // fib(2) -> 1
  // fib(3) -> 1
  // fib(4) -> 2
  // fib(5) -> 3

  def go(n: Int, last: Int, before_last: Int): Int = {
    if (n < 1) {
      last + before_last
    } else {
      go(n-1, last + before_last, last)
    }
  }

  go(n, 1, 0)
}

println(fib(1))

def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def loop(n: Int): Boolean = {
    if (n + 1 >= as.length) {
      true
    } else {
      ordered(as(n), as(n + 1))
    }
  }
  loop(0)
}

val arr = Array(5,1,2,3)

def f(a:Int, b:Int): Boolean = {
  if (a < b) true else false
}

println(isSorted(arr, f))

def curry[A,B,C] (f: (A,B) => C): A => (B => C) = {
    (a: A) => {
      (b: B) => f(a, b)
    }
}

def curryFn[A,B,C] (f: (A,B) => C): A => (B => C) = {
  val h = (a: A, b: B) => f(a,b)
  val g = (f: (A) => B) => h(a: A)
}

val sum = (x: Int, y: Int) => x + y
println(sum(1,2))

val sum_x = curry(sum)(5)
println(sum_x(1))

