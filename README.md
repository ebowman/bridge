Memory-Efficient Bridge Solver
==============================

15 April 2011 (Updated 3 July 2011)

Inspired by 
[Logic Programming in Scala, Part 1](http://ambassadortothecomputers.blogspot.com/2011/04/logic-programming-in-scala-part-1.html),
this program reads a configuration file listing some people, and how long each
of them takes to cross the bridge, and solves the problem, indicating what is
the minimum amount of time the people can take to cross the bridge, then prints
all the solutions.

First, update People.txt to contain the people and walking times you want.  A
line starting with #, or that otherwise can't be parsed reasonably, is ignored,
mostly.

    ./sbt

You'll get the sbt shell, then you can do:

    > run People.txt

and it should should download everything needed, and run the program.

A nice hard input to solve is this:

    Alice 7
    Bob 11
    Candace 9
    Dave 6
    Eric 10
    Fred 5
    George 5
    Henry 9

...which took around 15 minutes.  Using the other implementations to solve this
input tends to exhaust memory; this version is extremely memory efficient.

Note there is a code snippet from [Pavel Fatin's
blog](http://pavelfatin.com/scala-for-project-euler/), for generating prime
numbers.  It is (c) 2011 by Pavel Fatin and is used with permission.

Enjoy.
