/**
 * Various functions related to prime factors, lowest common denominator, common divisor.
 */
object GCD {

  /**
   * Computes the prime factors of the given argument.
   */
  def primeFactors(num: Int): List[Int] = {
    var n = num;
    var i = 2
    val pending = collection.mutable.ListBuffer[Int]()
    while (i <= n) {
      while (n % i == 0) {
        pending += i
        n = n / i
      }
      i += 1
    }
    if (n > 1) {
      pending += n
    }
    pending.toList
  }

  /**
   * Returns the lowest common divisor, if any, between the two arguments.
   */
  def lcd(x: Int, y: Int): Option[Int] = {
    (primeFactors(x) intersect primeFactors(y)) match {
      case empty if empty == Set.empty => None
      case full => Some(full.product)
    }
  }

  /**
   * Returns a common divisor, if one exists, across the list of numbers.
   */
  def commonDivisor(numbers: Traversable[Int]): Option[Int] = {
    val numSeq = numbers.toSeq
    numSeq.zip(numSeq.tail).map(p => lcd(p._1, p._2)).flatten match {
      case Nil => None
      case list => Some(list.min)
    }
  }
}
