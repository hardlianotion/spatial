# sim-opt

Map-based delivery simulation based on Uber's [H3 spatial index](https://www.uber.com/blog/h3/).  
We use simulation to analyse tree-based optimisation methods for route delivery.

Optimizer work-in-progress:
 - H3Tree - a space partitioning tree based on work done for Urbantz on timeslot optimization. [implemented]
 - H3BinTree - a time-and-space partitioning tree useful for searches of geographical locations in given time intervals.
 - H3Optimizer - a hierarchical route solver based on H3BinTree.

Simulation work-in-progress TBA ...
 

## Getting started

Get the latest version of sbt from [here](https://www.scala-sbt.org) and run the tests:

```sbt test```

