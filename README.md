This was initially just an implementation of A* in Scala with some crazy stuff accompanying it.  Now, it's A*, some crazy stuff, and much, much more!

Originally, it was designed for use in Ants (for the Google AI challenge) as a preliminary attempt at implementing A* before actually doing so in OCaml (which is the language that my partner and I were using for Ants).

The dependencies are as follows:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`a`: **Tester**<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`b`: **DataStructure** [relies on `a`]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`c`: **PathFindingCore** [relies on `a`, `b`]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`d`: **AStar** [relies on `a`, `b`, `c`]

###Tester:
A highly-flexible test-running suite.  It can quickly and easily be told to run tests with particular flags on, and you can specify precisely what tests you would like to run in a jiffy.  For example, you can run tests `(1,52)U(58)U(75,91)` in the verbose mode on `ModuleX` by creating a script that extends `TestScript`, importing `tester.testscript.dialect.TestCriteriaDialect._`, and calling this in the body of your script: `run((1 >&> 91) && (53 >!> 74) && 58 && Talkative, ModuleX)`.  This module's code is very generic, so it can be used quite dynamically.  (See `AStar.src.test.AStarDependencyTest` or `AStar.src.test.astar.AStarTest` for good examples of its use.)

###DataStructure:
You can pretty easily ignore the silly things in this package; `Heap` and `PriorityQueue` were made for my preliminary attempt at implementing a priority queue (since OCaml doesn't have a native implementation of one, and including the Base library wasn't really a good option).  `BiHashMap` is kind of interesting, but it's also a little weird, and it's not entirely complete at the moment (though, it's worth mentioning that `TerrainCharConverter` in the 'AStar' module relies on it).

###PathFindingCore:
The bare necessities of building and visually representing your own pathfinding algorithm.

###AStar:
Implementations of A* and bidirectional A* (which makes use of actors).