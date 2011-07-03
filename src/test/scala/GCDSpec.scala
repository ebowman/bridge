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

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks
import org.scalatest.FlatSpec

class GCDSpec extends FlatSpec with ShouldMatchers with PropertyChecks with PrimeGenerator {
  type ? = this.type

  "The GCD object" should "correctly compute prime factors" in {
    forAll {
      (primes: List[Prime]) =>
        assert(GCD.primeFactors(primes.map(_.prime).product).toSet === primes.map(_.prime).toSet)
    }
  }

  it should "correctly compute the lowest common divisor" in {
    forAll {
      (a: List[Prime], b: List[Prime]) =>
        whenever(a.size > 0 && b.size > 0) {
          val isect: Set[Int] = a.map(_.prime).toSet.intersect(b.map(_.prime).toSet)
          if (isect.size == 0) {
            GCD.lcd(a.map(_.prime).product, b.map(_.prime).product) should equal(None)
          } else {
            GCD.lcd(a.map(_.prime).product, b.map(_.prime).product) should equal(Some(isect.product))
          }
        }
    }
  }

  it should "correctly find common divisors" in {
    forAll {
      (a: List[Prime], b: NonPrime) =>
        whenever(a.size > 1 && a == a.distinct) {
          // we don't guarantee to find the best one, just something.
          // So whatever we get should multiply perfectly into b.
          GCD.commonDivisor(a.map(_.prime * b.nonPrime)) match {
            case Some(div) => b.nonPrime % div should equal(0)
            case None => sys.error("Should not be possible")
          }
        }
    }
  }
}
