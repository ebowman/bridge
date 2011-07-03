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
import org.scalatest.FlatSpec

class BridgeSpec extends FlatSpec with ShouldMatchers with Bridge {
  type ? = this.type

  val people = Set(Person("Alice", 5), Person("Bob", 3), Person("Candace", 7), Person("Dave", 6))
  val scale = 1

  "The bridge solver" should "solve the test case" in {
    solve match {
      case Some((bestTime, solutions: Stream[Stream[State]])) =>
        bestTime should equal(24)
        for (solution <- solutions) {
          // the head is the solution state
          solution.head.timeRemaining should equal(0)
          solution.head.left should equal(Set.empty)
          solution.head.right should equal(people)

          // the last is the start state
          solution.last.timeRemaining should equal(bestTime)
          solution.last.left should equal(people)
          solution.last.right should equal(Set.empty)
        }
      case None => sys.error("Solver failed")
    }
  }
}
