object Bridge extends App {

  val People: Set[Person] = io.Source.fromFile("People.txt").getLines().foldLeft(Set[Person]()) {
    (set: Set[Person], line: String) =>
      line.split("\\s+").toList match {
        case first :: _ if first.startsWith("#") => set
        case name :: minutes :: Nil => set + Person(name = name, crossingTime = minutes.toInt)
        case _ => set
      }
  }

  val sum = People.map(_.crossingTime).sum // heuristic: sum/2 <= min time <= 2*sum
  (sum / 2 to 2 * sum).view.map(i => (i, State(timeRemaining = i).search)).find(!_._2.isEmpty) match {
    case Some((minimumMinutes, solutions)) =>
      println("best time = " + minimumMinutes)
      prettyPrint(solutions)
    case None => println("No solution found")
  }

  object LightPosition extends Enumeration {
    type LightPosition = Value
    val OnLeft, OnRight = Value
  }
  import LightPosition._

  case class Person(name: String, crossingTime: Int)

  case class State(left: Set[Person] = People, lightPos: LightPosition = OnLeft, timeRemaining: Int) {
    def right = People.filterNot(left.contains)

    def next: Stream[State] = lightPos match {
      case OnLeft => for {pair <- left.toSeq.combinations(2).toStream
                          timeLeft = timeRemaining - pair.map(_.crossingTime).max
                          newState = State(left.diff(pair.toSet), OnRight, timeLeft) if timeLeft >= 0} yield newState
      case OnRight => for {p <- right.toStream
                           timeLeft = timeRemaining - p.crossingTime
                           newState = State(left + p, OnLeft, timeLeft) if timeLeft > 0} yield newState
    }

    def search: Stream[Stream[State]] = {
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

  @annotation.tailrec
  private def prettyPrint(paths: Stream[Stream[State]]) {
    if (!paths.isEmpty) {
      var sum = 0
      val forwardPath = paths.head.reverse
      forwardPath.zip(forwardPath.tail).toIterator.foreach {
        case (from: State, to: State) =>
          val cost = from.timeRemaining - to.timeRemaining
          sum += cost
          from.lightPos match {
            case OnLeft =>
              println(from.left.diff(to.left).map(_.name).mkString(" & ") + " crossed in " + cost + " m (" + sum + ")")
            case OnRight =>
              println(to.left.diff(from.left).map(_.name).mkString + " came back in " + cost + " m (" + sum + ")")
          }
      }
      println()
      prettyPrint(paths.tail)
    }
  }
}
