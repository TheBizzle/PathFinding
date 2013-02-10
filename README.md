## Summary

Scala implementation of unidirectional and bidirectional versions of the A* pathfinding algorithm, along with a lot of other stuff that I've sandboxed around with whilst learning about Scala.

## Dependencies

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`a`: **Tester**<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`b`: **DataStructure** [relies on `a`]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`c`: **PathFindingCore** [relies on `a`, `b`]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`d`: **AStar** [relies on `a`, `b`, `c`]

## Tester
See [here](https://github.com/TheBizzle/Tester).

## DataStructure:
Primarily consists of a mostly-complete implementation of `BiHashMap`.  `BiHashMap` is currently utilized by `TerrainCharConverter` in the `AStar` module.

## PathFindingCore:
Building blocks for constructing your own pathfinding algorithm in Scala and visually representing it in ASCII.

## AStar:
Implementations of unidirectional A* and bidirectional A*.  The bidirectional version utilizes Akka actors for little more than learning purposes.
