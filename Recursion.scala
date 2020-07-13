package ru.philit.bigdata.vsu.scalaLang

import scala.annotation.tailrec

object Recursion {

  def fibonachi(n: Int): Int = if(n > 1)
    fibonachi(n - 1) + fibonachi(n - 2) else n

  def fibonachiTail(n: Int): Int = {
    def loop(prev: Int, current: Int, n: Int): Int =
      if(n == 0) current else loop(current, prev + current, n - 1)
    if (n > 1) loop(1, 1, n - 2) else n
  }

  def factorial(n: Int): Int =
    if (n == 1) 1 else n * factorial(n - 1)

  def factorialTailRec(n: Int): Int = {
    def loop(n: Int, acc: Int): Int = {
      if(n == 1) 1 else loop(n - 1, acc * n)
    }
    loop(n, 1)
  }

  @tailrec
  def nod(a: Int, b: Int): Int = if(b == 0) a else nod(b, a % b)

  def cycleSum(num: Int): Int =
    if(num == 1) 1 else cycleSum(num - 1) + num

  @tailrec
  def cycleSumTail(num: Int, acc: Int): Int =
    if(num == 1) acc else cycleSumTail(num - 1, acc + num)


  def loopSum(num: Int): Int = {
    var sum: Int = 0
    while(num > 0) {
      sum += num
      num - 1
    }
    sum
  }

  def forComprehensiveSum(num: Int): Int = {
    var sum: Int = 0
    for(i <- 1 to num) {
      sum += i
    }
    sum
  }

  val sum = (num: Int) => (1 to num).reduce((x, y) => x + y)
}
