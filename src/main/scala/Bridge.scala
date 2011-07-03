import java.util.Date

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

trait Bridge {

  // set of people involved
  val people: Set[Person]

  // scale factor for simplifying the problem
  val scale: Int

  def progress(batteryCharge: Int): Unit = {}

  // heuristic: we start our search with a battery
  // life equal to the sum of everyone's crossing
  // time. If you think about it, no possible solution
  // can exist that takes less time than this.
  lazy val sum = people.map(_.crossingTime).sum

  // Lazily stream until we find a solution, however long
  // that takes, starting from the heuristic mentioned
  def solve: Option[(Int, Stream[Stream[State]])] = {
    Stream.from(sum).map {
      i =>
        progress(i)
        (i, State(timeRemaining = i).generate)
    }.find(!_._2.isEmpty)
  }

  // A light is either on the left side, or the right side,
  // as we do state changes through solution space.
  object LightPosition extends Enumeration {
    type LightPosition = Value
    val OnLeft, OnRight = Value
  }

  import LightPosition._

  // Represents a person, and how it takes them to cross the bridge.
  case class Person(name: String, crossingTime: Int)

  // Represents a state in the game, and can generate all possible next states.
  case class State(left: Set[Person] = people, lightPos: LightPosition = OnLeft, timeRemaining: Int) {

    // set of people on the right of the bridge.
    def right: Set[Person] = people.filterNot(left.contains)

    // Each state is able to enumerate all its subsequent possible states,
    // mapping State => Stream[State]. Note that stream may be empty if
    // this state is a terminal state.  We use a Stream in order to keep
    // careful control over memory management.
    def next: Stream[State] = lightPos match {
      // when the light is on the left, we come up with all combinations of
      // 2 to consider what happens when that pair crosses the bridge.
      // We adjust the time left based on the slower of the two, and
      // synthesize a new state. Note also that we emit an empty stream
      // if there is no further step to be made with the amount of battery left.
      case OnLeft => for {pair <- left.toSeq.combinations(2).toStream
                          timeLeft = timeRemaining - pair.map(_.crossingTime).max
                          newState = State(left.diff(pair.toSet), OnRight, timeLeft) if timeLeft >= 0} yield newState
      // when the light is on the right, we try each person on the right
      // crossing back across the bridge, as long as there is some battery
      // left when they get there.
      case OnRight => for {person <- right.toStream
                           timeLeft = timeRemaining - person.crossingTime
                           newState = State(left + person, OnLeft, timeLeft) if timeLeft > 0} yield newState
    }

    // This is some high voodoo, and it took quite awhile to get it right. We recurse
    // through the state generation process ... each State spawns a (finite) stream of States,
    // and we lazily assemble these streams, terminating them as end up empty.  The for
    // comprehension with recurse is difficult to explain in detail, although conceptually
    // it's not too hard to understand that it is recursively enumerating every possible path
    // through state space.
    def generate: Stream[Stream[State]] = {
      def unit[T](t: T): Stream[T] = List(t).toStream
      def recurse(states: Stream[State]): Stream[Stream[State]] = {
        (for (nextPath: Stream[State] <- states.head.next.map(_ #:: states)) yield {
          if (nextPath.head.left.isEmpty) {
            unit(nextPath)
          } else {
            recurse(nextPath)
          }
        }).flatten
      }
      recurse(unit(this))
    }
  }

  // pretty prints a set of solutions. what we end up, typically, is a set of paths
  // that are slight variations of each other, that result in the same time.
  // e.g.:
  //   Bob & Dave crossed in 6 m (6)
  //   Bob came back in 3 m (9)
  //   Candace & Bob crossed in 7 m (16)
  //   Bob came back in 3 m (19)
  //   Alice & Bob crossed in 5 m (24)
  def prettyPrint(paths: Stream[Stream[State]], scale: Int): String = {
    val builder = new StringBuilder
    @annotation.tailrec
    def recurse(paths: Stream[Stream[State]]) {
      builder.append("\n")
      if (!paths.isEmpty) {
        var sum = 0
        val forwardPath = paths.head.reverse
        forwardPath.zip(forwardPath.tail).toIterator.foreach {
          case (from: State, to: State) =>
            val cost = from.timeRemaining - to.timeRemaining
            sum += cost
            from.lightPos match {
              case OnLeft =>
                builder.append(from.left.diff(to.left).map(_.name).mkString(" & ") +
                  " crossed in " + (cost * scale) + " m (" + (sum * scale) + ")\n")
              case OnRight =>
                builder.append(to.left.diff(from.left).map(_.name).mkString + " came back in " + (cost * scale) +
                  " m (" + (sum * scale) + ")\n")
            }
        }
        recurse(paths.tail)
      }
    }
    recurse(paths)
    builder.toString()
  }
}

object BridgeApp extends App with Bridge {

  val (people: Set[Person], scale: Int) = {

    require(args.size == 1, "Usage: BridgeApp [path to people file]")

    println("Starting at " + new Date)

    // load contents of source file into scaledPeople
    val scaledPeople: Set[Person] = io.Source.fromFile(args(0)).getLines().foldLeft(Set[Person]()) {
      (set: Set[Person], line: String) =>
        line.split("\\s+").toList match {
          case first :: _ if first.startsWith("#") => set
          case name :: minutes :: Nil => set + Person(name = name, crossingTime = minutes.toInt)
          case _ => set
        }
    }

    // We may be able to transform this to a simpler problem, if a greatest common divisor exists
    // for the set of crossing times.  We return from this block here the set of people and crossing
    // times to solve, as well as a scaling factor to multiply the results by, to match the actual
    // input instead of the
    GCD.commonDivisor(scaledPeople.map(_.crossingTime)) match {
      case Some(divisor) =>
        (scaledPeople.map(p => p.copy(crossingTime = (p.crossingTime / divisor))), divisor)
      case None =>
        (scaledPeople, 1)
    }
  }

  override def progress(batteryCharge: Int) {
    println(new java.util.Date + ": " + batteryCharge)
  }

  solve match {
    case Some((minimumMinutes: Int, solutions: Stream[Stream[State]])) =>
      println("Best time = " + (scale * minimumMinutes))
      println(prettyPrint(solutions, scale))
    case None => println("No solution found")
  }
  println("Finished at " + new Date)
}
