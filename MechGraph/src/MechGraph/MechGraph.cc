/*  FILE     MechGraph.cc
**  PACKAGE  MechGraph
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "MechGraph" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "Basis/System.hh"
#include "Basis/Pairs.hh"
#include "Reaction/LstOps.hh"
#include "Basis/Graph.hh"
#include "Basis/GraphCycle.hh"
#include "Basis/Matrix.hh"
#include "Basis/Statistics.hh"
#include "Reaction/Molecule.hh"
#include "Reaction/MoleculeSet.hh"
#include "Reaction/Rxn.hh"
#include "Reaction/DbaseRxn.hh"
#include "Reaction/DbaseMolRxn.hh"
#include "Reaction/RxnMech.hh"
#include "Reaction/ThermoTables.hh"
#include "Reaction/Senkin.hh"
#include "Reaction/SenkinRates.hh"
#include "Basis/Graph.hh"
#include "Reaction/MechGraph.hh"
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
bool Encode(CommBuffer& buffer, BasicMechanismGraph& mgraph);
bool Decode(CommBuffer& buffer, BasicMechanismGraph& mgraph);
static void FormGraphFromSenkin(ObjectList<DbaseReaction>& rxns,
				ObjectList<SimpleMolecule>& mols,
				const MechanismSenkinDataPoints points,
				PairList<double,DbaseReaction>& selected,
				BasicMechanismGraph& mechgraph,
				double mechtime,
				double ratecutoff,
				int maxrxns);
static void SetOfSenkinMechanisms(ObjectList<DbaseReaction>& rxns,
				  ObjectList<SimpleMolecule>& mols,
				  const MechanismSenkinDataPoints points,
				  double mechtimebegin,double mechtimeend,double mechtimeinc,
				  double ratecutoff,
				  unsigned int maxrxns);

template class list<PathsSearchNode<Identify> >;

/*S Constructors
*/
/*F AddMechGraphNode(mechgraph) . . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    mechgraph: The mechanism as Graph
**
**  REMARKS
**
*/
AddMechGraphNode::AddMechGraphNode(BasicMechanismGraph& mechgraph)
: MGraph(mechgraph)
     {
     }
 

/*S IO
*/

/*S CreateMechanismGraph
*/
/*F ForwardReactionName?(rxn) . . . . . . . . . . . .  name coded as forward?
**
**  DESCRIPTION
**    rxn: The reaction
**
**    The prefix is checked:
**    - "For:" is a forward reaction
**    - "Rev:" is a reverse reaction
**
**  REMARKS
**
*/
bool ForwardReactionName(const DbaseReaction& rxn)
{
  String name(rxn.NameTag);
  String prefix;
  name.IsolateNextWord(prefix,':');
  bool ret;
  cout << "ReactionNameForward?: ";
  cout << rxn.NameTag;
  
  if(prefix == "For")
    {
      cout << " is Forward\n";
      ret = true;
    }
  
  else
    {
      cout << " is Reverse\n";
      ret = false;
    }
  return ret;
}

/*F PutNodesInGraph(id,nodecode,reaction)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void AddMechGraphNode::PutNodesInGraph(const int id, 
				       const int nodecode,
				       const DbaseReaction& reaction)
{
  bool forward = ForwardReactionName(reaction);
  
  Identify rxnid(id,reaction.NameTag);
  Identify nodeid(nodecode,reaction.NameTag);
  
  MGraph.AddNodeToGraph(nodeid,rxnid);
  
  ObjectList<Identify>::const_iterator mol;
  for(mol=reaction.Reactants.begin();
      mol != reaction.Reactants.end();
	   mol++)
    {
      Identify reacid(MOLECULE_NODE,(*mol).NameTag);
      if(forward)
	MGraph.AddBondToGraph(reacid,nodeid);
      else
	MGraph.AddBondToGraph(nodeid,reacid);
    }
  for(mol=reaction.Products.begin();
      mol != reaction.Products.end();
      mol++)
	 {
	   Identify prodid(MOLECULE_NODE,(*mol).NameTag);
	   if(forward)
	     MGraph.AddBondToGraph(nodeid,prodid);
	   else
	     MGraph.AddBondToGraph(prodid,nodeid);
	 }
}

/*F operator()(molecule)  . . . . . . . . . . . . . . . . .  AddMechGraphNode
**
**  DESCRIPTION
**    molecule: The molecule to add
**
**    Adds a molecule to the graph by creating a node.
**
**  REMARKS
**
*/
void AddMechGraphNode::operator()(const SimpleMolecule& molecule)
     {
     Identify molid(molecule.Identification, molecule.NameTag);
     Identify nodeid(MOLECULE_NODE,molecule.NameTag);
     
     MGraph.AddNodeToGraph(nodeid,molid);
     }
 
/*F operator()(reaction)  . . . . . . . . . . Add reaction and bonds to graph
**
**  DESCRIPTION
**    reaction: The reaction
**
**    The reaction node is created and then the bonds to the molecules
**    are made.  The reactants are the parent nodes and the products are 
**    sons.
**
**  REMARKS
**
*/
void AddMechGraphNode::operator()(const double rate, const DbaseReaction& reaction)
{
  int id = reaction.Identification;
  int irate = (int) (10.0+rate)*4;
  int jrate = irate * 1000;
  
  int nodecode = REACTION_NODE + jrate;
  PutNodesInGraph(id,nodecode,reaction);
}
/*F operator()(reaction)  . . . . . . . . . . Add reaction and bonds to graph
**
**  DESCRIPTION
**    reaction: The reaction
**
**    The reaction node is created and then the bonds to the molecules
**    are made.  The reactants are the parent nodes and the products are 
**    sons.
**
**  REMARKS
**
*/
void AddMechGraphNode::operator()(const DbaseReaction& reaction)
{
  int id = reaction.Identification;
  int nodecode = REACTION_NODE;
  PutNodesInGraph(id,nodecode,reaction);
}
 
/*F CreateMechanismGraph(mech)  . . . . .  read in mechanism and create graph
**
**  DESCRIPTION
**    mech: The mechanism ID
**
**    Loop through all the reactions and molecules to create the graph
**    for the mechanism
**
**  REMARKS
**
*/
void MechanismGraphInfo::CreateMechanismGraph(const int mech)
     {
     SetUp(mech);
     AddMechGraphNode build(MechGraph);
     
     for_each(Molecules.begin(),
	      Molecules.end(),
	      build);
     for_each(Reactions.begin(),
	      Reactions.end(),
	      build);
     }
     
/*F ans = StoreMechanismGraphAsProperty(name) . . . . . . . . . . store graph
**
**  DESCRIPTION
**    name: The name under which to store the mechanism graph
**    ans: 0 if successful
**
**    The mechanism graph is stored in the Property list of the mechanism
**
**  REMARKS
**
*/
int MechanismGraphInfo::StoreMechanismGraphAsProperty(const String& name)
     {
     PropertyTypeByName<BasicMechanismGraph> *prop
	  = new PropertyTypeByName<BasicMechanismGraph>(name,MechGraph);
     bool result = Mechanism.Properties.AddObject(prop,"BasicMechanismGraph");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }

/*S Commands
*/
/*F ans = CreateFullMechGraph(g)  . . . . . create a graph from the mechanism
**
**  DESCRIPTION
**    g: The System Information
**    ans: 0 if successful
**
**  REMARKS
**
*/
int CreateFullMechGraph(ReactionSystemBase *g)
     {
     GraphMechRun *global = (GraphMechRun *) g;
     int ret = 0;
     
     String mechname;
     if(global->Inputs.size() < 1)
	  {
	  cout << "Expecting one argument:\n";
	  cout << "     Mechanism Name\n";
	  ret = 1;
	  }
     else
	  {
	  String mechname = global->Inputs.front();
	  global->Inputs.pop_front();
	  
	  cout << "Create Graph for ";
	  cout << mechname;
	  cout << "\n";
	  cout << global->MechGraphInfo.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  
	  int id = global->MechGraphInfo.DBMechanisms.DBMechanisms.Names[mechname];
     	  cout << "(With ID= " << id << ")\n";
	  if(id != 0)
	       {
	       global->MechGraphInfo.CreateMechanismGraph(id);
	       cout << "Graph Created:\n";
	       cout << "-----------------------------------------------\n";
	       global->MechGraphInfo.MechGraph.print(cout);
	       cout << "-----------------------------------------------\n";
	       global->MechGraphInfo.StoreMechanismGraphAsProperty("FullGraph");
	       global->MechGraphInfo.DBMechanisms.DBMechanisms.WriteDBObject(global->MechGraphInfo.Mechanism);
	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       }
	  }
     return 0;
     }
/*F ans = MechGraphOut(g) . . . . . . . . . .  output mechanism for java demo
**
**  DESCRIPTION
**    g: The System Information
**    ans: 0 if successful
**
**  REMARKS
**
*/
int MechGraphOut(ReactionSystemBase *g)
     {
     GraphMechRun *global = (GraphMechRun *) g;
     int ret = 0;
     
     String mechname;
     if(global->Inputs.size() < 1)
	  {
	  cout << "Expecting one argument:\n";
	  cout << "     Mechanism Name\n";
	  ret = 1;
	  }
     else
	  {
	  String mechname = global->Inputs.front();
	  global->Inputs.pop_front();
	  
	  cout << "Create Graph for ";
	  cout << mechname;
	  cout << "\n";
	  cout << global->MechGraphInfo.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  
	  int id = global->MechGraphInfo.DBMechanisms.DBMechanisms.Names[mechname];
     	  cout << "(With ID= " << id << ")\n";
	  if(id != 0)
	       {
	       global->MechGraphInfo.CreateMechanismGraph(id);
	       cout << "Graph Created:\n";
	       Graph<Identify,Identify> mgraph(global->MechGraphInfo.MechGraph);
	       
	       GraphOut(cout,
			mgraph,
			true,true);
	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       }
	  }
     return 0;
     }
/*F ans = FindPathsInMechanism(g) . . . . . . . . . . . . . .  find all paths
**
**  DESCRIPTION
**    g: The System information
**    ans: 0 if successful
**
**  REMARKS
**
*/
int FindPathsInMechanism(ReactionSystemBase *g)
     {
     GraphMechRun *global = (GraphMechRun *) g;
     int ret = 0;
     
     String mechname;
     if(global->Inputs.size() < 3)
	  {
	  cout << "Expecting two argument:\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Start Type (Molecule, Reaction)\n";
          cout << "     Start Molecule or Reaction\n";
          cout << "     Path Type (default: FullPaths)\n";
	  cout << "     Graph name (default: FullGraph)\n";
          cout << "     Number Iterations (default: -1)\n";
	  ret = 1;
	  }
     else
	  {
	  String mechname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String starttype = global->Inputs.front();
	  global->Inputs.pop_front();
	  String start = global->Inputs.front();
	  global->Inputs.pop_front();
	  String pathtype = "FullPaths";
	  String graphname = "FullGraph";
	  int numiter = -1;
	  
	  if(global->Inputs.size() > 0 )
	       {
	       pathtype = global->Inputs.front();
	       global->Inputs.pop_front();
	       if(global->Inputs.size() > 0 )
		    {
		    graphname = global->Inputs.front();
		    global->Inputs.pop_front();
		    if(global->Inputs.size() > 0 )
			 {
			 String iterations = global->Inputs.front();
			 global->Inputs.pop_front();
			 numiter = iterations.ToInteger();
			 }
		    }
	       }
	  
	  cout << "Create Graph for ";
	  cout << mechname;
	  cout << "\n";
	  cout << "With start " << starttype << ": " << start << "\n";
	  int strt = 0;
	  if(starttype == "Molecule")
	       strt = MOLECULE_NODE;
	  else if(starttype == "Reaction")
	       strt = REACTION_NODE;
	  else
	       cout << "Illegal Start Point: " << starttype << "\n";
	  if(strt != 0)
	       {
	       int id = global->MechGraphInfo.DBMechanisms.DBMechanisms.Names[mechname];
	       cout << "(With Mech ID= " << id << ")\n";
	       if(id != 0)
		    {
		    Identify startid(strt,start);
		    global->MechGraphInfo.FindPathsInMechanism(id,startid,
							       graphname,
							       pathtype,
							       numiter);
		    }
	       else
		    {
		    cout << "Illegal Mechanism Name\n";
		    }
	       }
	  
	  }
     return 0;
     }
/*F ans = FindGraphInSenkinMechanism(g) . . . find  paths in SENKIN mechanism
**
**  DESCRIPTION
**    g: The System information
**    ans: 0 if successful
**
**  REMARKS
**
*/
int FindGraphInSenkinMechanism(ReactionSystemBase *g)
     {
     GraphMechRun *global = (GraphMechRun *) g;
     int ret = 0;
     
     String mechname;
     if(global->Inputs.size() != 6)
	  {
	  cout << "Expecting six arguments:\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Senkin Run Name\n";
	  cout << "     Time\n";
	  cout << "     Rate Cutoff\n";
	  cout << "     Max. Reactions to show\n";
	  cout << "     First Molecule in Path (and cycles)\n";
	  
	  ret = 1;
	  }
     else
	  {
	  String mechname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String senkin = global->Inputs.front();
	  global->Inputs.pop_front();
	  String stime = global->Inputs.front();
	  global->Inputs.pop_front();
	  String sratecutoff = global->Inputs.front();
	  global->Inputs.pop_front();
	  String smaxrxns = global->Inputs.front();
	  global->Inputs.pop_front();
	  String molname = global->Inputs.front();
	  global->Inputs.pop_front();
	  double time = stime.ToFloat();
	  double ratecutoff = sratecutoff.ToFloat();
	  int maxrxns = smaxrxns.ToInteger();
	  
	  cout << "Create Graph for ";
	  cout << mechname;
	  cout << " from Senkin Run ";
	  cout << senkin;
	  int id = global->MechGraphInfo.DBMechanisms.DBMechanisms.Names[mechname];
	  cout << "(With Mech ID= " << id << ")";
	  cout << " at time ";
	  cout << time;
	  cout << " with cutoff";
	  cout << ratecutoff;
	  cout << "\n";

	  if(id != 0)
	    {
	      global->MechGraphInfo.SetUp(id);
	      
	      PropertyTypeByName<MechanismSenkinDataPoints> *prop
		= (PropertyTypeByName<MechanismSenkinDataPoints> *)
		global->MechGraphInfo.Mechanism.Properties.GetPointerToProperty(senkin);

	      cout << "\n The Properties\n";	       
	      MechanismSenkinDataPoints points = prop->Object;
	      cout << "\n The Properties\n";	       	       
	      cout << points;
	      
	      Identify graphid(id,molname);
	      MechGraphFromSenkin senkingraph(graphid,
					      global->MechGraphInfo.Reactions,
					      global->MechGraphInfo.Molecules,
					      points,
					      time,ratecutoff,maxrxns);
	      senkingraph.print(cout);

	      cout << "--------------------------------------------------------\n";
	      PathGraphFromSenkin senkinpaths(graphid,molname,senkingraph);
	      senkinpaths.print(cout);
	    }
	  else
	    {
	      cout << "Illegal Mechanism Name\n";
	    }
	  }
     return 0;
     }
/*F ans = DisplayGraphInSenkinMechanism(g)  . . . .  display SENKIN mechanism
**
**  DESCRIPTION
**    g: The System information
**    ans: 0 if successful
**
**  REMARKS
**
*/
int DisplayGraphInSenkinMechanism(ReactionSystemBase *g)
     {
     GraphMechRun *global = (GraphMechRun *) g;
     int ret = 0;
     
     String mechname;
     if(global->Inputs.size() != 6)
	  {
	  cout << "Expecting six arguments:\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Senkin Run Name\n";
	  cout << "     Time\n";
	  cout << "     Rate Cutoff\n";
	  cout << "     Max. Reactions to show\n";
	  cout << "     First Molecule in Path (and cycles)\n";
	  
	  ret = 1;
	  }
     else
	  {
	  String mechname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String senkin = global->Inputs.front();
	  global->Inputs.pop_front();
	  String stime = global->Inputs.front();
	  global->Inputs.pop_front();
	  String sratecutoff = global->Inputs.front();
	  global->Inputs.pop_front();
	  String smaxrxns = global->Inputs.front();
	  global->Inputs.pop_front();
	  String molname = global->Inputs.front();
	  global->Inputs.pop_front();
	  double time = stime.ToFloat();
	  double ratecutoff = sratecutoff.ToFloat();
	  int maxrxns = smaxrxns.ToInteger();
	  
	  cout << "Create Graph for ";
	  cout << mechname;
	  cout << " from Senkin Run ";
	  cout << senkin;
	  int id = global->MechGraphInfo.DBMechanisms.DBMechanisms.Names[mechname];
	  cout << "(With Mech ID= " << id << ")";
	  cout << " at time ";
	  cout << time;
	  cout << " with cutoff";
	  cout << ratecutoff;
	  cout << "\n";

	  if(id != 0)
	    {
	      global->MechGraphInfo.SetUp(id);
	      
	      PropertyTypeByName<MechanismSenkinDataPoints> *prop
		= (PropertyTypeByName<MechanismSenkinDataPoints> *)
		global->MechGraphInfo.Mechanism.Properties.GetPointerToProperty(senkin);

	      cout << "\n The Properties\n";	       
	      MechanismSenkinDataPoints points = prop->Object;
	      cout << "\n The Properties\n";	       	       
	      cout << points;
	      
	      Identify graphid(id,molname);
	      MechGraphFromSenkin senkingraph(graphid,
					      global->MechGraphInfo.Reactions,
					      global->MechGraphInfo.Molecules,
					      points,
					      time,ratecutoff,maxrxns);

	      Graph<Identify,Identify> mgraph(senkingraph.MechGraph);
	      GraphOut(cout,
		       mgraph,
		       true,true);
	      
	    }
	  else
	    {
	      cout << "Illegal Mechanism Name\n";
	    }
	  }
     return 0;
     }
/*F ans = FindSetGraphInSenkinMechanism(g)  . find  paths in SENKIN mechanism
**
**  DESCRIPTION
**    g: The System information
**    ans: 0 if successful
**
**  REMARKS
**
*/
int FindGraphSetInSenkinMechanism(ReactionSystemBase *g)
     {
     GraphMechRun *global = (GraphMechRun *) g;
     int ret = 0;
     
     String mechname;
     if(global->Inputs.size() != 7)
	  {
	  cout << "Expecting eight arguments:\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Senkin Run Name\n";
	  cout << "     Rate Cutoff\n";
	  cout << "     Max. Reactions to show\n";
	  cout << "     Begin Time\n";
	  cout << "     end Time\n";
	  cout << "     Increment Time\n";
	  
	  ret = 1;
	  }
     else
	  {
	  String mechname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String senkin = global->Inputs.front();
	  global->Inputs.pop_front();

	  String sratecutoff = global->Inputs.front();
	  global->Inputs.pop_front();
	  String smaxrxns = global->Inputs.front();
	  global->Inputs.pop_front();
	  double ratecutoff = sratecutoff.ToFloat();
	  int maxrxns = smaxrxns.ToInteger();

	  String sbtime = global->Inputs.front();
	  global->Inputs.pop_front();
	  String setime = global->Inputs.front();
	  global->Inputs.pop_front();
	  String sitime = global->Inputs.front();
	  global->Inputs.pop_front();
	  double btime = sbtime.ToFloat();
	  double etime = setime.ToFloat();
	  double itime = sitime.ToFloat();


	  
	  cout << "Create Graph for ";
	  cout << mechname;
	  cout << " from Senkin Run ";
	  cout << senkin;
	  int id = global->MechGraphInfo.DBMechanisms.DBMechanisms.Names[mechname];
	  cout << "(With Mech ID= " << id << ")";
	  cout << " with cutoff";
	  cout << ratecutoff;
	  cout << "\n";

	  if(id != 0)
	    {
	      global->MechGraphInfo.SetUp(id);
	      
	      PropertyTypeByName<MechanismSenkinDataPoints> *prop
		= (PropertyTypeByName<MechanismSenkinDataPoints> *)
		global->MechGraphInfo.Mechanism.Properties.GetPointerToProperty(senkin);

	      cout << "\n The Properties\n";	       
	      MechanismSenkinDataPoints points = prop->Object;
	      cout << "\n The Properties\n";	       	       
	      cout << points;
	      
	      cout << " Times: ";
	      cout << btime << " " << etime << " " << itime << "\n";
	      
	      SetOfSenkinMechanisms(global->MechGraphInfo.Reactions,
				    global->MechGraphInfo.Molecules,
				    points,btime,etime,itime,
				    ratecutoff,maxrxns);
	    }
	  else
	    {
	      cout << "Illegal Mechanism Name\n";
	    }
	  }
     return 0;
     }
/*F InitializeGraphMechCommands() . . . . . . . . . . . . . . . command setup
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void GraphMechRun::InitializeGraphMechCommands()
     {
     SingleSystemCommand graph("Graph",
			      "Create Graph from mechanism",
			      &CreateFullMechGraph);
     Commands.AddObject("Graph",graph);
     SingleSystemCommand paths("Paths",
			      "Find Paths Within Mechanism",
			      &FindPathsInMechanism);
     Commands.AddObject("Paths",paths);
     SingleSystemCommand gr("Display",
			      "Output Graph to Draw",
			      &MechGraphOut);
     Commands.AddObject("Display",gr);
     SingleSystemCommand sgr("SenkinGraph",
			      "Graph From Senkin Time",
			      &FindGraphInSenkinMechanism);
     Commands.AddObject("SenkinGraph",sgr);
     SingleSystemCommand ssgr("SenkinGraphSet",
			      "Graph Sets From Senkin",
			      &FindGraphSetInSenkinMechanism);
     Commands.AddObject("SenkinGraphSet",ssgr);
     SingleSystemCommand dsgr("DisplaySenkin",
			      "Display Graph From Senkin Time",
			      &DisplayGraphInSenkinMechanism);
     Commands.AddObject("DisplaySenkin",dsgr);
     }
void InitializeMechanismPropDecodeFunctions();
/*F Initialize()  . . . . . . . . . . . . . . . . . . . . . .  intialize  Run
**
**  DESCRIPTION
**    General Initialize routine
**
**  REMARKS
**
*/
void GraphMechRun::Initialize()
     {
     InitializeGraphMechCommands();
     }

/*S MechanismPaths
*/ 
/*F ans = FindPathsInMechanism(mech,id,graphname,conditions)  . .  find paths
**
**  DESCRIPTION
**    mech: The mechanism id
**    id: The node id from where to start
**    graphname: The name of the mechanism graph
**    conditions: The conditions to search for path
**    ans: 0 if successful
**
**  REMARKS
**
*/
int
MechanismGraphInfo::FindPathsInMechanism(const int mech, 
					 const Identify& id,
					 String& graphname,
					 String& conditions,
					 const int numiter)

     {
     bool okay = true;
     DbaseMechanism *mechanism = DBMechanisms.DBMechanisms.ReadDBObject(mech);
     PropertyTypeByName<BasicMechanismGraph> *graph 
	  = (PropertyTypeByName<BasicMechanismGraph> *) 
	       mechanism->Properties.GetPointerToObject(graphname);
     
     GraphNode<Identify,Identify> node = graph->Object.FindGraphNode(id);
     if(node.GetKey() != id)
	  {
	  cout << "Node not found: " << id.NameTag << "\n";
	  okay = false;
	  }
     PathSearchConditions cond(true,true,numiter,true,true);
     if(conditions == "AllPaths")
	  {
	  }
     else if(conditions == "FullPaths")
	  {
	  cond.IncludeIncompletePaths = false;
	  }
     else if(conditions == "OnlyCycles")
	  {
	  cond.IncludeIncompletePaths = false;
	  cond.Paths = false;
	  }
     else if(conditions == "ForwardPaths")
	  {
	  cond.IncludeIncompletePaths = false;
	  cond.IncludeParents = false;
	  }
     else 
	  {
	  okay = false;
	  cout << "Illegal Path Condition\n";
	  cout << "     AllPaths: All forward and reverse paths and incomplete paths\n";
	  cout << "     FullPaths:  All full forward and reverse paths\n";
	  cout << "     ForwardPaths: All full forward paths\n";
	  }
     int ret = 0;
     if(okay)
	  {
	  String name;
	  String period = ".";
	  String idname(id.NameTag);
	  
	  name.AppendToEnd(period);
	  name.AppendToEnd(graphname);
	  name.AppendToEnd(period);
	  name.AppendToEnd(conditions);
	  name.AppendToEnd(period);
	  name.AppendToEnd(idname);

	  String cyclename = "Cycle";
	  cyclename.AppendToEnd(name);
	  String pathname = "Paths";
	  pathname.AppendToEnd(name);
	  

	  FindPathsInGraph<Identify,Identify> paths(id,graph->Object,cond);
	  DbaseMechPaths straight(paths.Done,pathname);
	  DbaseMechPaths cycles(paths.Cycle,cyclename);
	  
	  cout << "\n-----------------------------------------------------\n";
	  
	  cout << "\nStraight Paths\n";
	  straight.print(cout);
	  cout << "\n-----------------------------------------------------\n";
	  
	  cout << "Cycles\n";
	  cycles.print(cout);
	  cout << "\n-----------------------------------------------------\n";
	  PropertyTypeByName<DbaseMechPaths> *prop1
	       = new PropertyTypeByName<DbaseMechPaths>(pathname,straight);
	  bool result = Mechanism.Properties.AddObject(prop1,"DbaseMechPaths");
	  
	  PropertyTypeByName<DbaseMechPaths> *prop2
	       = new PropertyTypeByName<DbaseMechPaths>(cyclename,cycles);
	  result = result && Mechanism.Properties.AddObject(prop2,"DbaseMechPaths");
	  if(!result)
	       ret = 1;
	  }
     else
	  {
	  ret = 1;
	  cout << "Paths and Cycles not added to Properties\n";
	  }
     return ret;
     }

 
/*F GetMolFromRxnForGraph(build,ids,mols) . . . . . .  put molecules in graph
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
static void GetMolFromRxnForGraph(AddMechGraphNode build,
			   ObjectList<Identify>& ids,
			   ObjectList<SimpleMolecule>& mols)
{
  ObjectList<SimpleMolecule>::iterator mol;
  
  for(ObjectList<Identify>::iterator id=ids.begin();
      id != ids.end();
      id++)
    {
      FindMoleculeByName findmol((*id).NameTag);
      mol = find_if(mols.begin(),
		    mols.end(),
		    findmol);
      build.operator()(*mol);
    }
}

 
/*F time = FindTimePoint(points,mechtime) . . . . . . . . . find a time point
**
**  DESCRIPTION
**    points: The Senkin information
**    mechtime: The time to find
**
**  REMARKS
**
*/
static int FindTimePoint(const MechanismSenkinDataPoints points,
			 double mechtime)
{
  ObjectList<double>::const_iterator time = points.Times.begin();
  int itime = 0;  
  while(*time < mechtime)
    {
      itime++;
      time++;
    }
  return itime;
}

 
/*C TopReaction . . . . . . . . . . . . . . . . . Used for ordering reactions
**
**  DESCRIPTION
**     Reaction: The reaction
**     Rate: The rate
**     Forward: true if forward
**
**  REMARKS
**
*/
class TopReactions
{
public:
  DbaseReaction Reaction;
  double Rate;
  bool Forward;

  TopReactions()
    {
    }
  
  TopReactions(const TopReactions& top)
    : Reaction(top.Reaction),
    Rate(top.Rate),
    Forward(top.Forward)
    {
    }
  
  TopReactions(DbaseReaction& rxn,
	       double rate,
	       bool forward)
    : Reaction(rxn),
    Rate(rate),
    Forward(forward)
    {
    }
  
  void CopyClone(TopReactions *top)
    {
      Reaction = top->Reaction;
      Rate = top->Rate;
      Forward = top->Forward;
      
    }
  TopReactions *Clone()
    {
      TopReactions *top = new TopReactions(*this);
      return top;
    }
  ostream& print(ostream& out) const
    {
      if(Forward)
	out << "Forward: ";
      else
	out << "Backward: ";
      out << (Identify) Reaction << " - ";
      out << Rate;
      out << "\n";
      return out;
    }
  bool EncodeThis(CommBuffer& buffer)
    {
      //bool result = (*this).EncodeThis(buffer);
      bool result = result && Encode(buffer,Rate);
      int forward;
      
      if(Forward)
	forward = 1;
      else
	forward = 0;
      result = result && Encode(buffer,forward);
      return result;
    }
  bool DecodeThis(CommBuffer& buffer)
    {
      //bool result = (*this).DecodeThis(buffer);
      bool result = result && Decode(buffer,Rate);
      int forward;
      result = result && Encode(buffer,forward);
      if(forward == 1)
	Forward = true;
      else
	Forward = false;
      return result;
    }
};
  bool operator<(const TopReactions& top1, const TopReactions& top2)
    {
      return top1.Rate > top2.Rate;
   }
  bool operator==(const TopReactions& top1, const TopReactions& top2)
    {
      return top1.Rate == top2.Rate;
    }
ostream& operator<<(ostream& out, const TopReactions& top)
{
  return top.print(out);
}

     
/*F ans = Decode(buffer,top)  . . . . . . . . . . . . . . . . .  TopReactions
**
**  DESCRIPTION
**    buffer: The buffer
**    top: the structure
**    
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, TopReactions& top)
{
  return top.EncodeThis(buffer);
}
/*F ans = Decode(buffer,top)  . . . . . . . . . . . . . . . . .  TopReactions
**
**  DESCRIPTION
**    buffer: The buffer
**    top: the structure
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, TopReactions& top)
{
  return top.DecodeThis(buffer);
}
/*F ReactionValuePairs(selected,rxns,mat,points,mechtime,ratecutoff)  . pairs
**
**  DESCRIPTION
**    selected: Reactions and the current rate (will be filled in)
**    rxns: The full set of reactions
**    mat: The time, reaction rate matrix
**    points: The raw time versus concentration data
**    mechtime: The time to sample
**    ratecutoff: The minimum rate (log scale)
**
**    The time point (mechtime) in the 'points' is search for (FindTimePoint),
**    and the column in the rate-time matrix (mat) is determined.
**    All the reactions are looped through and if the rate is
**    above the cutoff (ratecutoff) then the rate-reaction pair is 
**    inserted into 'selected' (which is then sorted).
**    
**  REMARKS
**
*/
static void ReactionValuePairs(PairList<double,DbaseReaction>& selected,
			       ObjectList<DbaseReaction>& rxns,
			       Matrix<double>& mat,
			       const MechanismSenkinDataPoints& points,
			       double mechtime,
			       double ratecutoff)
{
  unsigned int itime = FindTimePoint(points,mechtime);
  unsigned int rlength = 2*rxns.size();
  
  ObjectList<DbaseReaction>::iterator rxnorig = rxns.begin();
  for(unsigned int irxn=0; irxn < rlength; irxn++)
    {
      double rate = mat[irxn][itime];
      DbaseReaction *rxn = new DbaseReaction(*rxnorig);
      
      if(rate > ratecutoff)
	{
	  bool forward;
	  if(irxn % 2 == 0)
	    forward = true;
	  else
	    forward = false;

	  String name;
	  if(forward)
	    {
	      String forname("For:");
	      name.AppendToEnd(forname);
	    }
	  else
	    {
	      String rev("Rev:");
	      name.AppendToEnd(rev);
	    }
	  name.AppendToEnd((*rxn).NameTag);
	  (*rxn).NameTag = name;
	  
	  BasicPair<double,DbaseReaction> top(rate,*rxn);

	  selected.AddObject(top);
	}
      if(irxn % 2 == 1)
	rxnorig++;
    }

  selected.Sort();
}
 
  
/*F BuildGraphFromReactionValuePairs(selected,mols,mechgraph,maxnum)  . graph
**
**  DESCRIPTION
**    selected: The reaction-rate pair
**    mols: The list of molecules
**    mechgraph: The mechanism graph to create
**    maxnum: The maximum number of nodes allowed in the graph
**
**  REMARKS
**
*/
static void BuildGraphFromReactionValuePairs(PairList<double,DbaseReaction>& selected,
					     ObjectList<SimpleMolecule>& mols,
					     BasicMechanismGraph& mechgraph,
					     unsigned int maxnum)
{
  AddMechGraphNode build(mechgraph);
  PairList<double,DbaseReaction>::iterator top = selected.begin();
  unsigned int ibegin=0;
  unsigned int iend = selected.size();
  
  if(maxnum > selected.size())
    {
      maxnum = selected.size();
      cout << "\n Only " << maxnum << " Reactions\n";
    }
  else
    {
      ibegin = selected.size() - maxnum;
      for(unsigned int irxn = 0; irxn < ibegin;irxn++)
	top++;
    }
  
  for(unsigned int irxn=ibegin ; irxn < iend; irxn++)
    {
      GetMolFromRxnForGraph(build,
			    (*top).J.Products,
			    mols);
      GetMolFromRxnForGraph(build,
			    (*top).J.Reactants,
			    mols);
      build.operator()((*top).I,(*top).J);
      top++;
    }
}
/*F FindTopReactions(rxns,mat,points,itime,ratecufoff,maxnum) . . build graph
**
**  DESCRIPTION
**    rxns: The list of reactions in the mechanism
**    mols: The list of molecules in the mechanism
**    mat: The matrix of time versus rates
**    points: The raw SENKIN run info
**    selected: rate-reaction pairs (filled in)
**    mechgraph: The mechanism as graph (filled in)
**    mechtime: The time to sample
**
**    The rate-reaction pairs (selected) are determined (ReactionValuePairs).
**    With the selected reactions, build the graph (BuildGraphFromReactionValuePairs).
**
**  REMARKS
**
*/
static void FindTopReactions(ObjectList<DbaseReaction>& rxns,
			     ObjectList<SimpleMolecule>& mols,
			     Matrix<double>& mat,
			     const MechanismSenkinDataPoints& points,
			     PairList<double,DbaseReaction>& selected,
			     BasicMechanismGraph& mechgraph,
			     double mechtime,
			     double ratecutoff,
			     unsigned int maxnum)
{
  ReactionValuePairs(selected,rxns,mat,points,mechtime,ratecutoff);
  BuildGraphFromReactionValuePairs(selected,mols,mechgraph,maxnum);
}
/*F FormGraphFromSenkin(rxns,mols,points,mechtime,ratecutoff,maxrxns) . Graph
**
**  DESCRIPTION
**    rxns: The list of reactions
**    mols: The list of molecules
**    points: The times, temperatures and concentrations of the SENKIN run.
**    mechtime: The time to sample the mechanism values
**    ratecutoff: The minimum rate to print
**
**  REMARKS
**
*/
static void FormGraphFromSenkin(ObjectList<DbaseReaction>& rxns,
				ObjectList<SimpleMolecule>& mols,
				const MechanismSenkinDataPoints points,
				PairList<double,DbaseReaction>& selected,
				BasicMechanismGraph& mechgraph,
				double mechtime,
				double ratecutoff,
				int maxrxns)
{
  int length = points.Times.size();
  int rlength = 2*rxns.size();
  
  Matrix<double> mat(rlength,length);
  FindReactionValues values(mat,points,true,points.Times.size(),1);
  
  CalculateValuesFromReactions(rxns,values,true,false);
  
  FindTopReactions(rxns,mols,mat,points,
		   selected,mechgraph,
		   mechtime,ratecutoff,maxrxns);
}
 
 
 
 
/*C RateRxnPairCompare
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class RateRxnPairCompare
{
  unsigned int ID;
public:
  RateRxnPairCompare(const BasicPair<double,DbaseReaction>& r)
    : ID(r.J.Identification)
    {
    }
  
  operator()(const BasicPair<double,DbaseReaction>& r)
    {
      return r.J.Identification == ID;
    }
};
 
/*C IsolateOutReactionsFromRateRxnPair 
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class IsolateOutReactionsFromRateRxnPair
{
public:
  String operator()(const BasicPair<double,DbaseReaction>& r)
    {
      return r.J.NameTag;
    }
};
 
/*F ans = RxnSetEqual(s1,s2)  . . . . . . . . . . . .  same set of reactions?
**
**  DESCRIPTION
**    s1,s2: The set of reactions
**
**  REMARKS
**
*/
static bool RxnSetEqual(PairList<double,DbaseReaction>& s1,
			PairList<double,DbaseReaction>& s2)
{
  PairList<double,DbaseReaction>::iterator iter1 = s1.begin();
  PairList<double,DbaseReaction>::iterator iter2;
  bool stillthesame = true;
  
  if(s1.size() == s2.size())
    {
      unsigned int cnt = 0;
      while(cnt < s1.size() && stillthesame) 
	{
	  RateRxnPairCompare comp(*iter1);
	  
	  iter2 = find_if(s2.begin(),s2.end(),comp);
	  if(iter2 == s2.end())
	    stillthesame = false;
	  iter1++;
	  cnt++;
	}
    }
  else
    stillthesame = false;
  return stillthesame;
}
/*F SetOfSenkinMechanisms(rxns,mols,points,mechtimeinc,ratecutoff,maxrxns)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
static void SetOfSenkinMechanisms(ObjectList<DbaseReaction>& rxns,
				  ObjectList<SimpleMolecule>&,
				  const MechanismSenkinDataPoints points,
				  double mechtimebegin,double mechtimeend,double mechtimeinc,
				  double ratecutoff,
				  unsigned int maxrxns)
{
  int length = points.Times.size();
  int rlength = 2*rxns.size();
  
  Matrix<double> mat(rlength,length);
  FindReactionValues values(mat,points,true,points.Times.size(),1);
  
  CalculateValuesFromReactions(rxns,values,true,false);

  PairList<double,DbaseReaction> *lastselected = new PairList<double,DbaseReaction>();

  SetOfPairSets<double,DbaseReaction> pairset;  
  ObjectList<double> times;
  
  for(double mechtime = mechtimebegin;
      mechtime < mechtimeend;
      mechtime += mechtimeinc)
    {
      unsigned int maxs = maxrxns;
      
      PairList<double,DbaseReaction> *selected = new PairList<double,DbaseReaction>();
 
      ReactionValuePairs(*selected,rxns,mat,points,mechtime,ratecutoff);
      
      if(maxs > (*selected).size())
	{
	  maxs = (*selected).size();
	}
      unsigned int cnt = (*selected).size();
      while(cnt > maxs)
	{
	  (*selected).pop_front();
	  cnt--;
	}
      bool newselect = false;
      if(mechtimebegin < mechtime)
	newselect = RxnSetEqual(*selected,*lastselected);
      if(!newselect)
	{
	  pairset.AddObject(*selected);
	  times.AddObject(mechtime);
	  IsolateOutReactionsFromRateRxnPair iso;
	  ObjectList<String> ids;
	  
	  transform((*selected).begin(),
		    (*selected).end(),
		    back_insert_iterator< ObjectList<String> >(ids),
		    iso);
	  cout << "Time: " << setw(10) << mechtime;
	  cout << ": " << ids;
	  cout << "\n";
	  
	}
      
      if(mechtimebegin < mechtime)
	delete lastselected;
      lastselected = selected;
      
    }

  cout << "Times:\n";
  cout << times;
  cout << "\n";
  
}

/*S ReactionPathEvaluation
 */
/*F ReactionPathEvaluation(eval)  . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    eval: Structure to be copied
**
**  REMARKS
**
*/
ReactionPathEvaluation::ReactionPathEvaluation(const ReactionPathEvaluation& eval)
  : ListEvaluationClassesIdentify(eval),
    MolCounts(eval.MolCounts),
    MolWeights(eval.MolWeights)
{
}
/*F ReactionPathEvaluation()  . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
ReactionPathEvaluation::ReactionPathEvaluation()
{
}
/*F ReactionPathEvaluation(path,selected) . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    path: The reaction/molecule path
**    selected: The selected reactions
**
**  REMARKS
**
*/
ReactionPathEvaluation::ReactionPathEvaluation(const ObjectList<Identify>& path,
					      PairList<double,DbaseReaction>& selected)
  : ListEvaluationClassesIdentify(path,false)
{
  ObjectList<int> molrxns;
  molrxns.AddObject(MOLECULE_NODE);
  molrxns.AddObject(REACTION_NODE);
  CreateClasses(molrxns);
  Classes.front().ChangeDelimitor("  ");
  Classes.back().ChangeDelimitor("  ");
  FindReactionPathInformation(selected);
}
/*C ReactionNameInPairJ
**
**  DESCRIPTION
**     From an string name, find the corresponding
**     reaction in a list of 
**     BasicPair<double,DbaseReaction> 
**  REMARKS
**
*/
class ReactionNameInPairJ
{
  String Name;
  
public:
  ReactionNameInPairJ(const String& name)
    : Name(name)
    {
    }
  bool operator()(BasicPair<double,DbaseReaction>& rxn)
    {
      return Name == rxn.J.NameTag;
    }
};
/*F FindReactionPathInformation(selected) . . .  mol weights and counts
**
**  DESCRIPTION
**    rxns: The list of possible reactions with weights
**  REMARKS
**
*/
void ReactionPathEvaluation::FindReactionPathInformation(PairList<double,DbaseReaction>& selected)
{
  ObjectList<Identify>::iterator rxn;
  for(rxn = List.begin();
      rxn != List.end();
      rxn++)
    {
      if((*rxn).Identification == REACTION_NODE)
	{
	  ReactionNameInPairJ comp((*rxn).NameTag);
	  ObjectList< BasicPair<double,DbaseReaction> >::iterator  rxn 
	    = find_if(selected.begin(),
		      selected.end(),
		      comp);
	  if(rxn != selected.end())
	    {
	      FindReactionMoleculeInfo(*rxn);
	    }
	  else
	    {
	      cout << "ReactionPathEvaluation: Reaction - ";
	      cout << (*rxn).J.NameTag;
	      cout << "   Reaction Not Found\n";
	    }
	}
      
    }
}
 
/*F FindReactionMoleculeInfo(rxn) . . . . . . . . . .  products and reactants
**
**  DESCRIPTION
**    rxn: Reaction and weight info
**
**    This isolates the molecule counts (MolCounts, MolWeights) using
**    the CountMoleculeInfo routine.  The reactants are subtracted from the 
**    counts and the products are added to the counts
**
**  REMARKS
**
*/
void ReactionPathEvaluation::FindReactionMoleculeInfo(BasicPair<double,DbaseReaction>& rxn)
{
  double weight = rxn.I;
  
  CountMoleculeInfo(true,weight,rxn.J.Products);
  CountMoleculeInfo(false,weight,rxn.J.Reactants);
}
 
/*F CountMoleculeInfo(weight,mols)  . . . . Update mol list counts and weight
**
**  DESCRIPTION
**    weight: The weight to add 
**    mols: The list of molecules
**  REMARKS
**
*/
void ReactionPathEvaluation::CountMoleculeInfo(bool products,
					       double weight,
					       ObjectList<Identify> mols)
{
  int addcount = 1;
  if(!products)
    {
      addcount = -1;
      weight = -weight;
    }
  
  ObjectList<Identify>::iterator mol;
  for(mol = mols.begin();
      mol != mols.end();
      mol++)
    {
      EqualPairI<Identify,int> comp(*mol);
      PairList<Identify,int>::iterator p = 
	find_if(MolCounts.begin(),
		MolCounts.end(),
		comp);
      if( p == MolCounts.end())
	{
	  BasicPair< Identify, int> *info = new BasicPair< Identify, int>(*mol,addcount);
	  MolCounts.AddObject(*info);
	}
      else
	{
	  (*p).J += addcount;
	}
      EqualPairI<Identify,double> compw(*mol);
      PairList<Identify,double>::iterator pw = 
	find_if(MolWeights.begin(),
		MolWeights.end(),
		compw);
      if( pw == MolWeights.end())
	{
	  BasicPair< Identify, double> *infow = new BasicPair< Identify, double>(*mol,weight);
	  MolWeights.AddObject(*infow);
	}
      else
	{
	  (*pw).J += weight;
	}
    }
}


/*F ans EncodeThis(buffer)  . . . . . . . . . . . . .  ReactionPathEvaluation
**
**  DESCRIPTION
**    buffer: The output buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool ReactionPathEvaluation::EncodeThis(CommBuffer& buffer)
{
  bool result = ListEvaluationClassesIdentify::EncodeThis(buffer);
  result = result && MolCounts.EncodeThis(buffer);
  result = result && MolWeights.EncodeThis(buffer);
  
  return result;
}
/*F ans DecodeThis(buffer)  . . . . . . . . . . . . .  ReactionPathEvaluation
**
**  DESCRIPTION
**    buffer: The output buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool ReactionPathEvaluation::DecodeThis(CommBuffer& buffer)
{
  bool result = ListEvaluationClassesIdentify::DecodeThis(buffer);
  result = result && MolCounts.DecodeThis(buffer);
  result = result && MolWeights.DecodeThis(buffer);
  
  return result;
} 
/*F ans = Encode(buffer,eval) . . . . . . . . . . . .  ReactionPathEvaluation
**
**  DESCRIPTION
**    buffer: The output buffer
**    eval: The structure
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, ReactionPathEvaluation& eval)
{
  return eval.EncodeThis(buffer);
}
/*F ans = Decode(buffer,eval) . . . . . . . . . . . .  ReactionPathEvaluation
**
**  DESCRIPTION
**    buffer: The output buffer
**    eval: The structure
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, ReactionPathEvaluation& eval)
{
  return eval.DecodeThis(buffer);
}

/*C PrintIdentify
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class PrintIdentify
{
  bool PrintName;
  bool PrintID;
  ostream& Out;
  
public:
  PrintIdentify(ostream& out,
		const bool id,
		const bool name)
    : PrintName(name),
    PrintID(id),
    Out(out)
    {
    }
  void operator()(const Identify& id)
    {
      if(PrintName && PrintID)
	Out << "[";
      if(PrintID)
	Out << id.Identification;
      if(PrintName && PrintID)
	Out << ",";
      else
	Out << "  ";
      
      if(PrintName)
	Out << id.NameTag;
      if(PrintName && PrintID)
	Out << "]";
    }
};
/*C MoleculeWeightOutput  . . . . . . . . . . . . . . . . . . . .  operator()
**
**  DESCRIPTION
**    Help to print out weighted molecule expression
**
**  REMARKS
**
*/
template <class W>
class MoleculeWeightOutput
{
  bool Reactants;
  unsigned int Width;
  ostream& Out;
  bool NotFirst;
  
public:
  MoleculeWeightOutput(ostream& out,
		       const unsigned int width)
    : Reactants(true),
    Width(width),
    Out(out),
    NotFirst(false)
    {
    }
  void Toggle()
    {
      if(Reactants)
	Reactants = false;
      else
	Reactants = true;
      NotFirst = false;
    }
  
  void operator()(BasicPair<Identify,W>& p)
    {
      double weight;
      if(Reactants)
	weight = - ((double) p.J);
      else
	weight = (double) p.J;
      
      if(weight > 0.0)
	{
	  if(NotFirst)
	    Out << " +";
	  Out << " (";
	  Out << setw(Width) << weight;
	  Out << ") ";
	  Out << p.I.NameTag;
	}
    }
};
/*F MoleculeWeightsAsReaction(out,weights)  . . . . . . . . .  print reaction
**
**  DESCRIPTION
**    out: The output stream
**    weights: The weights and the molecules
**
**    The reactants and products are printed according to their weights
**    like a reaction.  
**
**  REMARKS
**
*/
template <class W>
ostream& MoleculeWeightsAsReaction(ostream& out,
				   PairList<Identify,W> weights)
{
  MoleculeWeightOutput<W> molo(out,2);
  
  for_each(weights.begin(),weights.end(),molo);
  molo.Toggle();
  out << " --> ";
  for_each(weights.begin(),weights.end(),molo);
  out << "\n";
  
  return out;
}

/*F out = print(out)  . . . . . . . . . . . . . . . .  ReactionPathEvaluation
**
**  DESCRIPTION
**    out: The output buffer
**
**  REMARKS
**
*/
ostream& ReactionPathEvaluation::print(ostream& out) const
{
  PrintIdentify idout(out,false,true);
  out << "\nReactions: ";
  for_each(Classes.front().begin(),
	     Classes.front().end(),
	     idout);
  out << "\nMolecules: ";
  for_each(Classes.back().begin(),
	     Classes.back().end(),
	     idout);
  out << "\nOverall Reaction\n";
  MoleculeWeightsAsReaction(out,MolCounts);
  out << "\n";
  MoleculeWeightsAsReaction(out,MolWeights);
  out << "\n";
  return out;
} 
 
/*F ReactionPathAnalysis(paths,selected,evals) . . . . . . . . .  Analyse set of paths
**
**  DESCRIPTION
**    paths: The set of paths to be analysed
**    selected: The reaction information
**    evals: The list of analysed paths (will be inserted into)
**
**   This is repeated calls to ReactionPathEvaluation
**
**  REMARKS
**
*/
void ReactionPathAnalysis(DbaseMechPaths& paths, 
			  PairList<double,DbaseReaction>& selected,
			  ObjectList<ReactionPathEvaluation>& evals)
{
  for(DbaseMechPaths::iterator path = paths.begin();
      path != paths.end();
      path++)
    {
      ReactionPathEvaluation eval((*path).Path,selected);
      evals.AddObject(eval);
    }
}
 
/*F ans = operator==(eval1,eval2) . . . . . . . . . .  ReactionPathEvaluation
**
**  DESCRIPTION
**    eval1,eval2: The evaluated paths
**    ans: true if the MolCounts are the same
**
**  REMARKS
**
*/
bool operator==(const ReactionPathEvaluation& eval1,
		const ReactionPathEvaluation& eval2)
{
  return eval1.MolCounts == eval2.MolCounts;
}
/*F ans = operator==(eval1,eval2) . . . . . . . . . .  ReactionPathEvaluation
**
**  DESCRIPTION
**    eval1,eval2: The evaluated paths
**    ans: true if the MolCounts are the same
**
**  REMARKS
**
*/
bool operator<(const ReactionPathEvaluation& eval1,
		const ReactionPathEvaluation& eval2)
{
  return eval1.MolCounts < eval2.MolCounts;
}
/*F out = operator<<(out,eval)  . . . . . . . . . . .  ReactionPathEvaluation
**
**  DESCRIPTION
**    out: The output buffer
**    eval: The evaluated path
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out,
		    const ReactionPathEvaluation& eval)
{
  return eval.print(out);
}

/*S MechGraphFromSenkin
 */
 
/*F MechGraphFromSenkin(id,rxns,mols,points,molname,time,ratecutoff,maxrxns)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
MechGraphFromSenkin::MechGraphFromSenkin(Identify& id,
					 ObjectList<DbaseReaction>& rxns,
					 ObjectList<SimpleMolecule>& mols,
					 MechanismSenkinDataPoints& points,
					 const double time,
					 const double ratecutoff,
					 const unsigned int maxrxns)
  : Identify(id),
    Time(time),
    RateCutOff(ratecutoff),
    MaxRxns(maxrxns),
    DrawGraph(false)
{
  FormGraphFromSenkin(rxns,mols,
		      points,Selected,MechGraph,
		      Time,RateCutOff,MaxRxns);
}
 
 
/*F PathGraphFromSenkin(id,molname,mechgraphinfo) . . . . paths from molecule
**
**  DESCRIPTION
**    id: The Identify of the paths
**    molname: The molecule to start from
**    mechgraphinfo: The mechanism graph info needed
**
**  REMARKS
**
*/  
PathGraphFromSenkin::PathGraphFromSenkin(Identify& id,
					 const String& molname,
					 MechGraphFromSenkin& mechgraph)
  : Identify(id),
    Conditions(false,false,-1,true,true),
    MolID(MOLECULE_NODE,molname),
    Paths(MolID,mechgraph.MechGraph,Conditions),
    Straight(Paths.Done,"Paths"),
    Cycles(Paths.Cycle,"Cycles")
{
  PathEvaluation.ChangeTitle(  "\n\n===========     Path Evaluation   =========== \n");
  PathEvaluation.ChangeDelimitor("\n----------------------------------------------\n");
  ReactionPathAnalysis(Straight,mechgraph.Selected,PathEvaluation);
  
  CycleEvaluation.ChangeTitle(  "\n\n===========    Cycle Evaluation   =========== \n");
  CycleEvaluation.ChangeDelimitor("\n----------------------------------------------\n");
  ReactionPathAnalysis(Cycles,mechgraph.Selected,CycleEvaluation);
}


/*F out = print(out)  . . . . . . . . . . . . . . . . . . MechGraphFromSenkin
**
**  DESCRIPTION
**    out: The output buffer
**
**  REMARKS
**
*/
ostream& MechGraphFromSenkin::print(ostream& out) const
{
  if(!DrawGraph)
    {
      MechGraph.print(out);
      out << "\n-----------------------------------------------------\n";
      out << "Criteria: Time = ";
      out << Time;
      out << "  log10(Rate Cutoff) = ";
      out << RateCutOff;
      
      out << "\n   Weight      Reaction\n";
      for(ObjectList< BasicPair<double,DbaseReaction> >::const_iterator b = Selected.begin();
	  b != Selected.end();
	  b++)
	{
	  out << setw(15) << (*b).I;
	  out << " ";
	  out << (*b).J.NameTag;
	  out << "\n";
	}
    }
  else
    {
      Graph<Identify,Identify> mgraph(MechGraph);
      
      GraphOut(out,
	       mgraph,
	       true,true);
    }
  
  return out;
}
/*F out = print(out)  . . . . . . . . . . . . . . . . . . PathGraphFromSenkin
**
**  DESCRIPTION
**    out: The output buffer
**
**  REMARKS
**
*/
ostream& PathGraphFromSenkin::print(ostream& out) const
{
  out << PathEvaluation;
  out << CycleEvaluation;
  return out;
  
}



