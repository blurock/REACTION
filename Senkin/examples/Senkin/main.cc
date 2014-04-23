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
#include "Basis/System.hh"
#include "Basis/Pairs.hh"
#include "Basis/Graph.hh"
#include "Basis/GraphCycle.hh"
#include "Basis/BasicLaTeXTable.hh"
#include "Basis/Vector.hh"
#include "Basis/MatrixNumeric.hh"
#include "Basis/MatrixUtilities.hh"
#include "Basis/MatrixOut.hh"

#include "Reaction/Molecule.hh"
#include "Reaction/MoleculeSet.hh"
#include "Reaction/MolProps.hh"
#include "Reaction/MolValence.hh"
#include "Reaction/MolStats.hh"
#include "Reaction/Rxn.hh"
#include "Reaction/DbaseRxn.hh"
#include "Reaction/DbaseMolRxn.hh"
#include "Reaction/RxnMech.hh"
#include "Reaction/ThermoTables.hh"
#include "Reaction/Senkin.hh"

int main(int argc, char *argv[])
     {
     SenkinSystemBase runit(argc,argv);
     InitializeONamePropertyDecodeFunctions();
     InitializeThermPropertyDecodeFunctions();
     InitializeRxnPropertyDecodeFunctions();
     return runit.Run();
     }

     
     

