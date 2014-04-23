/*  FILE     
**  PACKAGE     REACTION    
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    
**
**  REFERENCES
**
**  COPYRIGHT (C) 1995  REACTION Project / Edward S. Blurock 
*/
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "BasisSystem.hh"
#include "Matrix.hh"
#include "Statistics.hh"
#include "Pairs.hh"
#include "Graph.hh"
#include "GraphCycle.hh"
#include "LstOps.hh"
#include "Molecule.hh"
#include "MoleculeSet.hh"
#include "Rxn.hh"
#include "DbaseRxn.hh"
#include "RxnMech.hh"
#include "ThermoTables.hh"
#include "Senkin.hh"
#include "MechGraph.hh"

template class list<BasicPair<double, DbaseReaction> >;
template class list<BasicPair<Identify, double> >;
template class list<BasicPair<Identify, int> >;
template class list<BasicPair<int, int> >;
template class list<BasicPair<Identify, Identify> >;
template class list<ObjectList<Identify> >;
template class list<int>;
template class vector<int>;
template bool operator==(BasicPair<double, DbaseReaction> const &, BasicPair<double, DbaseReaction> const &);
template bool operator<(BasicPair<double, DbaseReaction> const &, BasicPair<double, DbaseReaction> const &);
template bool operator==(BasicPair<Identify,int> const &, BasicPair<Identify,int> const &);
template bool operator<(BasicPair<Identify,int> const &, BasicPair<Identify,int> const &);

int main(int argc, char *argv[])
     {
     GraphMechRun runit(argc,argv);
     InitializeThermPropertyDecodeFunctions();
     InitializeRxnPropertyDecodeFunctions();
     InitializeONamePropertyDecodeFunctions();
     return runit.Run();
     }
