/*
 * Copyright 2011 Eric Bowman
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
    ((primeFactors(x) intersect primeFactors(y)).toSet -- Set(1)) match {
      case empty if empty.isEmpty => None
      case full => Some(full.product)
    }
  }

  /**
   * Returns a common divisor, if one exists, across the list of numbers.
   */
  def commonDivisor(numbers: Iterable[Int]): Option[Int] = {
    numbers.toList match {
      case Nil => None
      case a :: Nil => None
      case head :: tail => (Option(head) /: tail) {
        case (None, _) => None
        case (Some(a), b) => lcd(a, b)
      }
    }
  }
}
