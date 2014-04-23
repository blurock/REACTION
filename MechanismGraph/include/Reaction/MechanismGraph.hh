/*  FILE     MechanismGraph.hh
**  PACKAGE  MechanismGraph
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "MechanismGraph" package in the Reaction environment
**
**  COPYRIGHT (C) 2000 Edward S. Blurock
*/
 
#ifndef Reaction_MECHANISMGRAPH_HH
#define Reaction_MECHANISMGRAPH_HH

#define MECHANISMGRAPH_BASE               990070
#define MECHANISMGRAPH_NODE_NAME          "MechanismNode"
#define MECHANISMGRAPH_EDGE_NAME          "MechanismEdge"
#define MECHANISMGRAPH_GRAPH_NAME         "MechanismGraph"
#define MECHANISMGRAPH_CREATEGRAPH_NAME   "CreateMechanismGraph"
#define MECHANISMGRAPH_REDUCTION_NAME     "MechanismGraphReduction"

#define MECHANISMGRAPH_NODE_ID            MECHANISMGRAPH_BASE + 1
#define MECHANISMGRAPH_EDGE_ID            MECHANISMGRAPH_BASE + 2
#define MECHANISMGRAPH_GRAPH_ID           MECHANISMGRAPH_BASE + 3
#define MECHANISMGRAPH_CREATEGRAPH_ID     MECHANISMGRAPH_BASE + 4
#define MECHANISMGRAPH_REDUCTION_ID       MECHANISMGRAPH_BASE + 5


/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "MechanismGraphType.hh"

/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
void InitialMechanismGraphDecodeFunctions();
void AddMechanismGraphClasses(DataSetOfObjectsClass& set);

#endif
