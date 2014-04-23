/*  FILE     MechanismGraphType.hh
**  PACKAGE  MechanismGraph
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "MechanismGraph" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_MECHANISMGRAPHTYPE_HH
#define Reaction_MECHANISMGRAPHTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

/*C RxnDataMechanismNode  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MechanismNode class definitions
**
**  REMARKS
**    Inheirits BaseDataNode
*/
class RxnDataMechanismNode : public BaseDataNode
{
public:
  RxnDataMechanismNode();
  RxnDataMechanismNode(const RxnDataMechanismNode& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnMechanismNodeClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataNodeClass
*/
class RxnMechanismNodeClass : public DataNodeClass
{
public:
  RxnMechanismNodeClass();
  RxnMechanismNodeClass(const RxnMechanismNodeClass& data);
  RxnMechanismNodeClass(const int id, 
			const String& name,
			const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataMechanismEdge  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MechanismEdge class definitions
**
**  REMARKS
**    Inheirits BaseDataEdge
*/
class RxnDataMechanismEdge : public BaseDataEdge
{
public:
  String Name;
  bool reverseReaction;
  RxnDataMechanismEdge();
  RxnDataMechanismEdge(const RxnDataMechanismEdge& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnMechanismEdgeClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataEdgeClass
*/
class RxnMechanismEdgeClass : public DataEdgeClass
{
public:
  RxnMechanismEdgeClass();
  RxnMechanismEdgeClass(const RxnMechanismEdgeClass& data);
  RxnMechanismEdgeClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataMechanismGraph  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MechanismGraph class definitions
**
**  REMARKS
**    Inheirits BaseDataGraph
*/
class RxnDataMechanismGraph : public BaseDataGraph
{
public:
  RxnDataMechanismGraph();
  RxnDataMechanismGraph(const RxnDataMechanismGraph& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
};
/*C RxnMechanismGraphClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataGraphClass
*/
class RxnMechanismGraphClass : public DataGraphClass
{
  RxnMechanismNodeClass *MoleculeNodeClass;
  RxnMechanismNodeClass *ReactionNodeClass;
  RxnMechanismEdgeClass *EdgeClass;
public:
  RxnMechanismGraphClass();
  RxnMechanismGraphClass(const RxnMechanismGraphClass& data);
  RxnMechanismGraphClass(const int id, 
		    const String& name,
		    const String& descr);

  RxnMechanismNodeClass *getMoleculeNodeClass();
  RxnMechanismNodeClass *getReactionNodeClass();
  RxnMechanismEdgeClass *getEdgeClass();
  
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataCreateMechanismGraph  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the CreateMechanismGraph class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataCreateMechanismGraph : public BaseDataAlgorithmOperation
{
  String                     MechanismS;
  RxnDataMechanism          *Mechanism;
  String                     GraphClassNameS;
  BaseDataString             *GraphClassName;

  String                     GraphClassS;
  RxnDataMechanismGraph     *Graph;
  RxnMechanismGraphClass    *GraphClass;

  RxnMechanismNodeClass *MoleculeNodeClass;
  RxnMechanismNodeClass *ReactionNodeClass;
  RxnMechanismEdgeClass *EdgeClass;
public:
  RxnDataCreateMechanismGraph();
  RxnDataCreateMechanismGraph(const RxnDataCreateMechanismGraph& data);
  bool AddMechanismNodes(RxnMechanismNodeClass *moleculenode,
			 RxnMechanismNodeClass *reactionnode);
  bool AddMechanismEdges(RxnMechanismEdgeClass *edgeclass);
  bool AddMechanismEdges(RxnMechanismEdgeClass *edgeclass,
			 RxnMechanismClass *mechclass,
			 BaseDataSetOfInstances *instances,
			 DataSetOfInstancesClass *instancesclass);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C RxnCreateMechanismGraphClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnCreateMechanismGraphClass : public DataAlgorithmOperationClass
{
public:
  RxnCreateMechanismGraphClass();
  RxnCreateMechanismGraphClass(const RxnCreateMechanismGraphClass& data);
  RxnCreateMechanismGraphClass(const int id, 
			       const String& name,
			       const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C BaseDataMechanismGraphReduction  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MechanismGraphReduction class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class BaseDataMechanismGraphReduction : public BaseDataAlgorithmOperation
{
  String MechanismInstanceNameS;
  BaseDataString *MechanismInstanceName;
  String StandardMoleculeSummaryS;
  RxnDataMoleculeSummarySet *StandardMoleculeSummary;

  BaseDataInstance *Instance;
  String MechanismS;
  RxnDataMechanism *Mechanism;
  RxnMechanismClass *MechanismClass;
  String MechanismGraphS;
  RxnDataMechanismGraph *MechanismGraph;

  String LeavesS;
  BaseDataKeyWords *Leaves;

  bool findLeaves;
  bool eliminateLeaves;
  bool allowReverse;
  bool iterate;

  public:
  BaseDataMechanismGraphReduction();
  BaseDataMechanismGraphReduction(const BaseDataMechanismGraphReduction& data);
  
  void FindLeaves();
  BaseDataKeyWords *GetLeaves(BaseDataGraph *graph, BaseDataKeyWords *nodes);
  void EliminateNodes(BaseDataGraph *graph, BaseDataKeyWords *nodes);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C DataMechanismGraphReductionClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class DataMechanismGraphReductionClass : public DataAlgorithmOperationClass
{
public:
  DataMechanismGraphReductionClass();
  DataMechanismGraphReductionClass(const DataMechanismGraphReductionClass& data);
  DataMechanismGraphReductionClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};


#endif
