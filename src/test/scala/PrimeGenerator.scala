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

import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}


trait PrimeGenerator {
  case class Prime(prime: Int)

  case class NonPrime(nonPrime: Int)

  /** From http://pavelfatin.com/scala-for-project-euler/ Problem 7 */
  lazy val ps: Stream[Int] = 2 #:: ps.map(i => Stream.from(i + 1).
    find(j => ps.takeWhile(k => k * k <= j).forall(j % _ > 0)).get)

  // 46th prime is 199, and with the choose(1, 4), we are careful to know that 199*199*199*199 fits into an int.
  val manyPrimes: Array[Int] = ps.take(46).toArray

  // generates a random array of non-prime values
  val nonPrimes = (for (i <- 2 to manyPrimes.last if !manyPrimes.contains(i)) yield i).toArray

  // generates a random prime
  val prime: Gen[Prime] = choose(0, manyPrimes.size - 1).map(index => Prime(manyPrimes(index)))

  // generates a random non-primes
  val nonPrime: Gen[NonPrime] = choose(0, nonPrimes.size - 1).map(index => NonPrime(nonPrimes(index)))

  // generates a random list of primes
  val primes: Gen[List[Prime]] = for (s <- choose(1, 4); p <- listOfN(s, prime)) yield p

  implicit def arbPrime: Arbitrary[Prime] = Arbitrary {
    prime
  }

  implicit def arbNonPrime: Arbitrary[NonPrime] = Arbitrary {
    nonPrime
  }

  implicit def arbPrimes: Arbitrary[List[Prime]] = Arbitrary {
    primes
  }
}