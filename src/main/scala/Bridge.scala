object Bridge extends App {

  require(args.size == 1, "Usage: Bridge [path to people file]")

  val (people: Set[Person], scale: Int) = {

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

  // heuristic: we start our search with a battery
  // life equal to the sum of everyone's crossing
  // time. If you think about it, no possible solution
  // can exist that takes less time than this.
  val sum = people.map(_.crossingTime).sum

  // Using our heuristic to bound the solution, we use
  // a view -> map -> find pattern to make sure we do
  // only the minimal amount of work required.  Without
  // the view, we'd compute all the solutions before we
  // started looking through them for a solution.
  (sum to 2 * sum).view.map {
    i =>
      println(new java.util.Date + ": " + i)
      (i, State(timeRemaining = i).generate)
  }.find(!_._2.isEmpty) match {
    case Some((minimumMinutes: Int, solutions: Stream[Stream[State]])) =>
      println("\nBest time = " + (scale * minimumMinutes) + "\n")
      prettyPrint(solutions, scale)
    case None => println("No solution found")
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
  @annotation.tailrec
  private def prettyPrint(paths: Stream[Stream[State]], scale: Int) {
    if (!paths.isEmpty) {
      var sum = 0
      val forwardPath = paths.head.reverse
      forwardPath.zip(forwardPath.tail).toIterator.foreach {
        case (from: State, to: State) =>
          val cost = from.timeRemaining - to.timeRemaining
          sum += cost
          from.lightPos match {
            case OnLeft =>
              println(from.left.diff(to.left).map(_.name).mkString(" & ") + " crossed in " + (cost * scale) + " m (" +
                (sum * scale) + ")")
            case OnRight =>
              println(to.left.diff(from.left).map(_.name).mkString + " came back in " + (cost * scale) + " m (" +
                (sum * scale) + ")")
          }
      }
      println()
      prettyPrint(paths.tail, scale)
    }
  }
}

