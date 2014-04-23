/*  FILE     RxnMech.cc
**  PACKAGE  RxnMech
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "RxnMech" package.
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
#include "Basis/Graph.hh"
#include "Basis/GraphCycle.hh"
#include "Reaction/Molecule.hh"
#include "Reaction/Rxn.hh"
#include "Basis/BasicLaTeXTable.hh"
#include "Basis/Vector.hh"
#include "Basis/MatrixNumeric.hh"
#include "Basis/MatrixUtilities.hh"
#include "Reaction/DbaseRxn.hh"
#include "Reaction/DbaseMolRxn.hh"
#include "Reaction/RxnMech.hh"

template class ReactionMechanism<Identify>;
template bool Decode(CommBuffer &, ReactionMechanism<Identify> &);
template bool Encode(CommBuffer &, ReactionMechanism<Identify> &);
template bool operator!=(DbaseMechanism const &, DbaseMechanism const &);
template ostream& operator<<(ostream&, ReactionMechanism<Identify> const &);

template bool TopDecode(CommBuffer &, PropertyTypeByName<DbaseMechPaths> *&);
template class PropertyTypeByName<DbaseMechPaths>;


template class PropertyTypeByName<BasicMechanismGraph>;
template bool TopDecode(CommBuffer &, PropertyTypeByName<BasicMechanismGraph> *&);
template bool TopDecode(CommBuffer &, PropertyTypeByName<DbaseMechPaths> *&);

/*F InitializeMechanismPropDecodeFunctions()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitializeMechanismPropDecodeFunctions()
     {
     bool (*rout1)(CommBuffer&,PropertyTypeByName<BasicMechanismGraph>*&) = TopDecode;
     SingleDecodeRoutine p1("BasicMechanismGraph",(bool (*&)(CommBuffer &, Identify *&)) rout1);
     (*SetOfEncodeDecodeRoutines)[p1.StructureName] = p1;

     bool (*rout2)(CommBuffer&,PropertyTypeByName<DbaseMechPaths>*&) = TopDecode;
     SingleDecodeRoutine p2("DbaseMechPaths",(bool (*&)(CommBuffer &, Identify *&)) rout2);
     (*SetOfEncodeDecodeRoutines)[p2.StructureName] = p2;

     bool (*rout3)(CommBuffer&,PropertyTypeByName<MechanismSenkinDataPoints>*&) = TopDecode;
     SingleDecodeRoutine p3("MechanismSenkinDataPoints",(bool (*&)(CommBuffer &, Identify *&)) rout3);
     (*SetOfEncodeDecodeRoutines)[p3.StructureName] = p3;
     }
/*S Constructors
*/
/*F SenkinInitialConditions(temp,pressure,time,inputs)  . . . .  Senkin setup
**
**  DESCRIPTION
**    temp: The initial temperature
**    pressure: The initial pressure
**    time: The final time
**    inputs: The set of remaining inputs
**
**    The temperature, pressure and initial time are converted to 
**    doubles and put in the class.  The time interval is taken to be
**    one tenth of the final time.  
**
**    From the inputs, a set of pairs with the molecule name and its initial 
**    mol fraction are expected.  The set of strings are read until all pairs
**    are extracted.
**
**  REMARKS
**
*/
SenkinInitialConditions::SenkinInitialConditions(String temp,
						 String pressure,
						 String time,
						 ObjectList<String> inputs)
     {
     Temperature = temp.ToFloat();
     Pressure = pressure.ToFloat();
     FinalTime = time.ToFloat();
     DeltaTime = FinalTime / 10.0;
     
     String name,molfrac;
     double frac;
     while(inputs.size() >= 2)
	  {
	  name = inputs.front();
	  inputs.pop_front();
	  molfrac = inputs.front();
	  inputs.pop_front();
	  
	  frac = molfrac.ToFloat();
	  InitialSpecies.AddObject(name);
	  InitialMolFractions.AddObject(frac);
	  }
     }
 
/*F SenkinInitialConditions(conditions) . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    conditions: The initial conditions
**
**  REMARKS
**
*/
SenkinInitialConditions::SenkinInitialConditions(const SenkinInitialConditions& conditions)
: Temperature(conditions.Temperature),
Pressure(conditions.Pressure),
FinalTime(conditions.FinalTime),
DeltaTime(conditions.DeltaTime),
InitialSpecies(conditions.InitialSpecies),
InitialCHEMKINSpecies(conditions.InitialCHEMKINSpecies),
InitialMolFractions(conditions.InitialMolFractions)
     {
     }
 
/*F SenkinMoleculeDataPoints()  . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
SenkinMoleculeDataPoints::SenkinMoleculeDataPoints()
     {
     }
 
/*F SenkinMoleculeDataPoints(data)  . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: The data points
**
**  REMARKS
**
*/
SenkinMoleculeDataPoints::SenkinMoleculeDataPoints(const SenkinMoleculeDataPoints& data)
: Molecule(data.Molecule),
Concentrations(data.Concentrations)
     {
     }
 
/*F SenkinMoleculeDataPoints(molecule)  . . . . . . . . . . . . .  initialize
**
**  DESCRIPTION
**    molecule: The molecule of the data points
**
**    This is the initialization of the data points.  The data points
**    filled in by other routines
**
**  REMARKS
**
*/
SenkinMoleculeDataPoints::SenkinMoleculeDataPoints(const Identify& molecule)
: Molecule(molecule)
     {
//     Concentrations.ChangeTitle("Concentrations\n");
     }
 
 
/*F MechanismSenkinDataPoints() . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
MechanismSenkinDataPoints::MechanismSenkinDataPoints()
     {
     }
 
/*F MechanismSenkinDataPoints(points) . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    points: The data points
**
**  REMARKS
**
*/
MechanismSenkinDataPoints::MechanismSenkinDataPoints(const MechanismSenkinDataPoints& points)
: Mechanism(points.Mechanism),
Conditions(points.Conditions),
MoleculePoints(points.MoleculePoints),
Times(points.Times),
Temperatures(points.Temperatures)
     {
     }
 
/*C InitializeSenkinMolData . . . . . . . . . create SenkinMoleculeDataPoints
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class InitializeSenkinMolInfo
     {
 public:
     InitializeSenkinMolInfo()
	  {
	  }
     SenkinMoleculeDataPoints operator()(const SimpleMolecule& mol)
	  {
	  SenkinMoleculeDataPoints data((Identify) mol);
	  return data;
	  }
     };
template InitializeSenkinMolInfo transform(list<SimpleMolecule>::const_iterator, 
					   list<SimpleMolecule>::const_iterator, 
					   back_insert_iterator<ObjectList<SenkinMoleculeDataPoints> >, 
					   InitializeSenkinMolInfo);
/*F MechanismSenkinDataPoints(mechanism,conditions,mols,in) . . .  initialize
**
**  DESCRIPTION
**    mechanism: The mechanism name
**    conditions: The conditions of the run
**    mols: The list of molecules
**    in: The output file of the run
**
**  REMARKS
**
*/
MechanismSenkinDataPoints::MechanismSenkinDataPoints(const String& mechanism,
						     const SenkinInitialConditions& conditions,
						     const ObjectList<SimpleMolecule>& mols,
						     istream& in)
: Mechanism(mechanism),
Conditions(conditions)
     {
     InitializeSenkinMolInfo info;
     transform(mols.begin(),
	      mols.end(),
	      back_insert_iterator< ObjectList<SenkinMoleculeDataPoints> >(MoleculePoints),
	      info);
     String line;
     in >> line;
     while(line[0] != '\000')
	  {
	  String time;
	  String temperature;
	  line.EliminateLeadingBlanks();
	  line.IsolateNextWord(time,' ');
	  line.IsolateNextWord(temperature,' ');
	  double t = time.ToFloat();
	  double tmp = temperature.ToFloat();
	  Times.AddObject(t);
	  Temperatures.AddObject(tmp);
	  
	  ObjectList<SimpleMolecule>::const_iterator mol;
	  ObjectList<SenkinMoleculeDataPoints>::iterator data = MoleculePoints.begin();
	  for(mol = mols.begin();
	      mol != mols.end();
	      mol++)
	       {
	       String conc;
	       line.EliminateLeadingBlanks();
	       line.IsolateNextWord(conc,' ');
	       double c = conc.ToFloat();
	       (*data).AddConcentrationValue(c);
	       data++;
	       }
	  in >> line;
	  }
     cout << "MechanismSenkinDataPoints\n";
     cout << "Times\n";
     cout << Times;
     cout << "Temperatures\n";
     cout << Temperatures;
     cout << "\n";
     
     }
/*S EncodeDecode
 */
/*F ans = EncodeThis(buffer)  . . . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool SenkinMoleculeDataPoints::EncodeThis(CommBuffer& buffer)
     {
     bool result = Encode(buffer,Molecule);
     result = result && Encode(buffer,Concentrations);
     return result;
     }
/*F ans = DecodeThis(buffer)  . . . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool SenkinMoleculeDataPoints::DecodeThis(CommBuffer& buffer)
     {
     bool result = Decode(buffer,Molecule);
     result = result && Decode(buffer,Concentrations);
     return result;
     }
 
/*F ans = Encode(buffer,data) . . . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    buffer: The buffer
**    data: The data points
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, SenkinMoleculeDataPoints& data)
     {
     return data.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,data) . . . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    buffer: The buffer
**    data: The data points
**    ans: True if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, SenkinMoleculeDataPoints& data)
     {
     return data.DecodeThis(buffer);
     }
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . MechanismSenkinDataPoints
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool MechanismSenkinDataPoints::EncodeThis(CommBuffer& buffer)
     {
     bool result = Encode(buffer,Mechanism);
     result = result && Encode(buffer,Conditions);
     result = result && Encode(buffer,MoleculePoints);
     result = result && Encode(buffer,Times);
     result = result && Encode(buffer,Temperatures);

     return result;
     }
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . MechanismSenkinDataPoints
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool MechanismSenkinDataPoints::DecodeThis(CommBuffer& buffer)
     {
     bool result = Decode(buffer,Mechanism);
     result = result && Decode(buffer,Conditions);
     result = result && Decode(buffer,MoleculePoints);
     result = result && Decode(buffer,Times);
     result = result && Decode(buffer,Temperatures);

     return result;
     }
/*F ans = Encode(buffer,data) . . . . . . . . . . . MechanismSenkinDataPoints
**
**  DESCRIPTION
**    buffer: The buffer
**    data: The senkin output data
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, MechanismSenkinDataPoints& data)
     {
     return data.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,data) . . . . . . . . . . . MechanismSenkinDataPoints
**
**  DESCRIPTION
**    buffer: The buffer
**    data: The senkin output data
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, MechanismSenkinDataPoints& data)
     {
     return data.DecodeThis(buffer);
     }
 

/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . SenkinInitialConditions
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool SenkinInitialConditions::EncodeThis(CommBuffer& buffer)
     {
     bool result = Encode(buffer,Temperature);
     result = result && Encode(buffer,Pressure);
     result = result && Encode(buffer,FinalTime);
     result = result && Encode(buffer,DeltaTime);
     result = result && Encode(buffer,InitialSpecies);
     result = result && Encode(buffer,InitialCHEMKINSpecies);
     result = result && Encode(buffer,InitialMolFractions);
     return result;
     }
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . SenkinInitialConditions
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool SenkinInitialConditions::DecodeThis(CommBuffer& buffer)
     {
     bool result = Decode(buffer,Temperature);
     result = result && Decode(buffer,Pressure);
     result = result && Decode(buffer,FinalTime);
     result = result && Decode(buffer,DeltaTime);
     result = result && Decode(buffer,InitialSpecies);
     result = result && Decode(buffer,InitialCHEMKINSpecies);
     result = result && Decode(buffer,InitialMolFractions);
     return result;
     }
 
/*F ans = Encode(buffer,conditions) . . . . . . . . . SenkinInitialConditions
**
**  DESCRIPTION
**    buffer: The buffer
**    conditions: The initial conditions
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, SenkinInitialConditions& conditions)
     {
     return conditions.EncodeThis(buffer);
     }
 
/*F ans = Decode(buffer,conditions) . . . . . . . . . SenkinInitialConditions
**
**  DESCRIPTION
**    buffer: The buffer
**    conditions: The initial conditions
**    ans: true if successful
**    
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, SenkinInitialConditions& conditions)
     {
     return conditions.DecodeThis(buffer);
     }
/*F ans = Encode(buffer,paths)  . . . . . . . . . . . . . . .  DbaseMechPaths
**
**  DESCRIPTION
**    buffer: The buffer
**    paths: The paths
**    ans: True if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, DbaseMechPaths& paths)
     {
     return paths.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,paths)  . . . . . . . . . . . . . . .  DbaseMechPaths
**
**  DESCRIPTION
**    buffer: The buffer
**    paths: The paths
**    ans: True if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, DbaseMechPaths& paths)
     {
     return paths.DecodeThis(buffer);
     }
/*F ans = Encode(buffer,mgraph) . . . . . . . . . . . . . BasicMechanismGraph
**
**  DESCRIPTION
**    buffer: The buffer
**    mgraph: The mechanism as graph
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, BasicMechanismGraph& mgraph)
     {
     return mgraph.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,mgraph) . . . . . . . . . . . . . BasicMechanismGraph
**
**  DESCRIPTION
**    buffer: The buffer
**    mgraph: The mechanism as graph
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, BasicMechanismGraph& mgraph)
     {
     return mgraph.DecodeThis(buffer);
     }
/*S Clone
*/ 
/*F Clone(data) . . . . . . . . . . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    data: The data
**
**  REMARKS
**
*/
void SenkinMoleculeDataPoints::Clone(SenkinMoleculeDataPoints *data)
     {
     *this = *data;
     }
 
/*F CopyClone() . . . . . . . . . . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
SenkinMoleculeDataPoints *SenkinMoleculeDataPoints::CopyClone()
     {
     SenkinMoleculeDataPoints *data = new SenkinMoleculeDataPoints;
     data->Clone(this);
     return data;
     }
  
/*F Clone(data) . . . . . . . . . . . . . . . . . . MechanismSenkinDataPoints
**
**  DESCRIPTION
**    data: The data to clone
**
**  REMARKS
**
*/
void MechanismSenkinDataPoints::Clone(MechanismSenkinDataPoints *data)
     {
     *this = *data;
     }
 
/*F data = CopyClone()  . . . . . . . . . . . . . . MechanismSenkinDataPoints
**
**  DESCRIPTION
**    data: The cloned copy
**
**  REMARKS
**
*/
MechanismSenkinDataPoints *MechanismSenkinDataPoints::CopyClone()
     {
     MechanismSenkinDataPoints *data = new MechanismSenkinDataPoints;
     data->Clone(this);
     return data;
     }

/*F Clone(conditions) . . . . . . . . . . . . . . . . SenkinInitialConditions
**
**  DESCRIPTION
**    conditions: The SENKIN conditions
**
**  REMARKS
**
*/
void SenkinInitialConditions::Clone(SenkinInitialConditions *conditions)
	  {
	  *this = *conditions;
	  }
 
/*F clone = CopyClone() . . . . . . . . . . . . . . . SenkinInitialConditions
**
**  DESCRIPTION
**    clone: A copy of the conditions
**
**  REMARKS
**
*/
SenkinInitialConditions *SenkinInitialConditions::CopyClone()
	  {
	  SenkinInitialConditions *conditions = new SenkinInitialConditions;
	  conditions->Clone(this);
	  return conditions;
	  }
/*S Boolean
*/
 
/*F ans = operator==(data1,data2) . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    data1,data2: The data points
**    ans: True if the same molecule
**
**  REMARKS
**
*/
bool operator==(const SenkinMoleculeDataPoints& data1, const SenkinMoleculeDataPoints& data2)
     {
     return data1.Molecule == data2.Molecule;
     }
/*F ans = operator<(data1,data2)  . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    data1,data2: The data points
**    ans: Molecule name compared
**
**  REMARKS
**
*/
bool operator<(const SenkinMoleculeDataPoints& data1, const SenkinMoleculeDataPoints& data2)
     {
     return data1.Molecule < data2.Molecule;
     }

/*S IO
*/
/*F out = print(out)  . . . . . . . . . . . . . . . . . . BasicMechanismGraph
**
**  DESCRIPTION
**    out: The output stream 
**
**  REMARKS
**
*/
ostream& BasicMechanismGraph::print(ostream& out) const
     {
     SearchableObjectList<Identify,GraphNode<Identify,Identify> >::const_iterator node;
     
     for(node = Nodes.begin();
	 node != Nodes.end();
	 node++)
	  {
	    GraphNode<Identify,Identify> gnode = (*node).second;
	    
	    if(gnode.GetKey().Identification == MOLECULE_NODE)
	       PrintMoleculeGraphNode(out,(*node).second,"  ",4);
	  }
     return out;
     }
/*F newout = print(out) . . . . . . . . . . . . . . . SenkinInitialConditions
**
**  DESCRIPTION
**    out: The output stream
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& SenkinInitialConditions::print(ostream& out) const
     {
       //out << "SENS\n";
     out << "CONP\n";
     out << "PRES " << Pressure << "\n";
     out << "TEMP " << Temperature << "\n";
     out << "TIME " << FinalTime << "\n";
     out << "DELT " << DeltaTime << "\n";
     
     ObjectList<double>::const_iterator frac = InitialMolFractions.begin();
     ObjectList<String>::const_iterator name;
     for(name = InitialCHEMKINSpecies.begin();
	 name != InitialCHEMKINSpecies.end();
	 name++)
	  {
	  out << "REAC ";
	  out << *name;
	  out << " ";
	  out << *frac;
	  out << "\n";
	  frac++;
	  }
     out << "END";
     return out;
     }
 
/*F newout = operator<<(out,conditions) . . . . . . . SenkinInitialConditions
**
**  DESCRIPTION
**    out: The output stream
**    conditions: The input conditions
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SenkinInitialConditions& conditions)
     {
     return conditions.print(out);
     }
/*F newout = print(out) . . . . . . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    out: The output stream
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& SenkinMoleculeDataPoints::print(ostream& out) const
     {
     out << "Molecule: ";
     out << Molecule.NameTag;
     out << "\n";
     out << Concentrations;
     return out;
     }
 
/*F newout = operator<<(out,data) . . . . . . . . .  SenkinMoleculeDataPoints
**
**  DESCRIPTION
**    out: The output buffer
**    data: The data points
**    newout: The output buffer
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SenkinMoleculeDataPoints& data)
     {
     return data.print(out);
     } 
/*F newout = print(out) . . . . . . . . . . . . . . MechanismSenkinDataPoints
**
**  DESCRIPTION
**    out: The output buffer
**    newout: The output buffer after the print
**
**    This does not print the data point, just the conditions.
**
**  REMARKS
**
*/
ostream& MechanismSenkinDataPoints::print(ostream& out) const
     {
     out << "Mechanism: ";
     out << Mechanism;
     out << "\n Run under Conditions:\n";
     out << Conditions;
     out << "\n";
     out << Times.size() << " Data points recorded\n";
     
     return out;
     }
 
/*F newout = operator<<(out,data) . . . . . . . . . MechanismSenkinDataPoints
**
**  DESCRIPTION
**    out: The output buffer
**    data: The set of data points
**    newout: the output buffer after the print
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const MechanismSenkinDataPoints& data)
     {
     return data.print(out);
     }
/*F out = PrintMechGraphNode(out,node,prefix,maxline) . . .  print graph node
**
**  DESCRIPTION
**    out: The output stream
**    node: The node to print out (either a molecule or reaction)
**    prefix: The prefix before each line
**    maxline: The maximum number of classes to print on a line
**    
**  REMARKS
**
*/
ostream& PrintMechGraphNode(ostream& out,
			    GraphNode<Identify,Identify> node,
			    const String& prefix,
			    const int maxline)
     {
     if(node.GetKey().Identification == MOLECULE_NODE)
	  PrintMoleculeGraphNode(out,node,prefix,maxline);
     else
	  PrintReactionGraphNode(out,node,prefix,maxline);
     return out;
     }
/*F nout = operator<<(out,mgraph) . . . . . . . . . . . . BasicMechanismGraph
**
**  DESCRIPTION
**    out: The output buffer
**    mgraph: The mechanism graph
**    nout: The new output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const BasicMechanismGraph& mgraph)
{
  return mgraph.print(out);
}

/*F PrintMoleculeGraphNode(out,node,prefix,maxline) . . . . . . molecule node
**
**  DESCRIPTION
**    out: The output stream
**    node: The molecule node to print out
**    prefix: The prefix before each line
**    maxline: The maximum number of classes to print on a line
**    
**  REMARKS
**
*/
ostream& PrintMoleculeGraphNode(ostream& out,
			    GraphNode<Identify,Identify> node,
			    const String& prefix,
			    const int maxline)
     {
     out << prefix;
     out << "Molecule: ";
     out << node.GetKey().NameTag;
     out << "\n";
     
     String prods = "                   ";
     String rxn   = "                   ";
     
     String newprefix1 = prefix;
     String newprefix2 = prefix;
     newprefix1.AppendToEnd(prods);
     newprefix2.AppendToEnd(rxn);
     cout << prefix;
     cout << " -    As Product:  ";
     PrintIdentifyListByName(out,node.Parents,newprefix1,maxline);
     cout << prefix;
     cout << " -    As Reactant: ";
     PrintIdentifyListByName(out,node.Sons,newprefix2,maxline);
     
     return out;
     }
/*F out = PrintReactionGraphNode(out,node,prefix,maxline) . . . reaction node
**
**  DESCRIPTION
**    out: The output stream
**    node: The reaction node to print out
**    prefix: The prefix before each line
**    maxline: The maximum number of classes to print on a line
**    
**  REMARKS
**
*/
ostream& PrintReactionGraphNode(ostream& out,
			    GraphNode<Identify,Identify> node,
			    const String& prefix,
			    const int maxline)
     {
     out << prefix;
     out << "Reaction: ";
     out << node.GetKey().NameTag;
     out << "\n";
     
     String react = " -    Reactants:  ";
     String prod  = " -    Products:   ";
     
     String newprefix1 = prefix;
     String newprefix2 = prefix;
     newprefix1.AppendToEnd(react);
     newprefix2.AppendToEnd(prod);
     PrintIdentifyList(out,node.Parents,newprefix1,maxline);
     PrintIdentifyList(out,node.Sons,newprefix2,maxline);
     
     return out;
     }
/*F nout = operator<<(out,paths)  . . . . . . . . . . . . . .  DbaseMechPaths
**
**  DESCRIPTION
**    out: The output buffer
**    paths: The graph paths
**    nout: The new output stream
**    
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const DbaseMechPaths& paths)
{
  return paths.print(out);
}
/*F out = PrintIdentifyList(out,ilist,prefix,maxline) . . . . . . . . . print
**
**  DESCRIPTION
**    out: The output stream
**    ilist: The list of Identify classes to print
**    prefix: The prefix before each line
**    maxline: The maximum number of classes to print on a line
**
**  REMARKS
**
*/
ostream& PrintIdentifyList(ostream& out,
			   const ObjectList<Identify>& ilist,
			   const String& prefix,
			   const int maxline)
     {
     ObjectList<Identify>::const_iterator iden;
     int count = 1;
     for(iden=ilist.begin();
	 iden != ilist.end();
	 iden++)
	  {
	  if(count % maxline == 0)
	       {
	       out << "\n";
	       out << prefix;
	       }
	  out << *iden;
	  out << " ";
	  count++;
	  }
     out << "\n";
     return out;
     
     }
/*F out = PrintIdentifyListByName(out,ilist,prefix,maxline) . print only name
**
**  DESCRIPTION
**    out: The output stream
**    ilist: The list of Identify classes to print
**    prefix: The prefix before each line
**    maxline: The maximum number of classes to print on a line
**
**  REMARKS
**
*/
ostream& PrintIdentifyListByName(ostream& out,
				 const ObjectList<Identify>& ilist,
				 const String& prefix,
				 const int maxline)
     {
     ObjectList<Identify>::const_iterator iden;
     int count = 1;
     for(iden=ilist.begin();
	 iden != ilist.end();
	 iden++)
	  {
	  if(count % maxline == 0)
	       {
	       out << "\n";
	       out << prefix;
	       }
	  out << (*iden).NameTag;
	  out << "  ";
	  count++;
	  }
     out << "\n";
     return out;
     
     }
/*F out = PrintIdentifyListByID(out,ilist,prefix,maxline) . . . print only id
**
**  DESCRIPTION
**    out: The output stream
**    ilist: The list of Identify classes to print
**    prefix: The prefix before each line
**    maxline: The maximum number of classes to print on a line
**
**  REMARKS
**
*/
ostream& PrintIdentifyListByID(ostream& out,
			       const ObjectList<Identify>& ilist,
			       const String& prefix,
			       const int maxline)
     {
     ObjectList<Identify>::const_iterator iden;
     int count = 1;
     for(iden=ilist.begin();
	 iden != ilist.end();
	 iden++)
	  {
	  if(count % maxline == 0)
	       {
	       out << "\n";
	       out << prefix;
	       }
	  out << (*iden).Identification;
	  out << "   ";
	  count++;
	  }
     out << "\n";
     return out;
     
     }
