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
#define TEMPLATE_INSTANTIATION
#include "Reaction/Molecule.hh"
#include "Basis/LineGraph.hh"
#include "Reaction/MolProps.hh"
#include "Reaction/MolValence.hh"
#include "Reaction/MolStats.hh"
#include "Reaction/SECharge.hh"
#include "Reaction/MoleculeSet.hh"
#include "Reaction/GenerateID.hh"
#include "Reaction/MolCreate.hh"
#include "Basis/Matrix.hh"
#include "Basis/Statistics.hh"
#include "Reaction/Rxn.hh"
#include "Reaction/ThermoTables.hh"
#include "Reaction/Chemkin.hh"

int DEBUGPRINT;

template class vector<int>;
template class list<int>;
template class list<BasicPair<int, int> >;


/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int main(int argc, char *argv[])
{
  ChemkinSystemBase runit(argc,argv);
  runit.Initialize();
  
  return runit.Run();
}

