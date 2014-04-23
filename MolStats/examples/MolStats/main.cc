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
#define TEMPLATE_INSTANTIATION

#include "CoreDataObjects.hh"
#include "Vector.hh"
#include "NumericObjects.hh"
#include "LogicalObjects.hh"
#include "DataObjects.hh"
#include "FunctionReal1DObjects.hh"
#include "OperationObjects.hh"
#include "LogicalOperations.hh"
#include "DataObjects.hh"
#include "InstanceObjects.hh"
#include "MenuObjects.hh"
#include "DirectedTreeObjects.hh"
#include "SelectObjects.hh"
#include "AlgorithmObjects.hh"
#include "Dbase.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "System.hh"


int main()
     {
     cout << "\n--------------------------------------------\n";
     cout << "AtomInformation Read\n";
     ifstream sfrom("/home/reaction/Reaction/data/stat-inf.dat");
     ifstream mfrom("/home/reaction/Reaction/data/StandardMeta.mta");
     RxnDataAtomInformation atominfo(&sfrom,&mfrom);

//      cout << "\n--------------------------------------------\n";
//      cout << "MolFileMolecule\n";
      ifstream smolfrom("moleculetest.txt");
//      MolFileMolecule mfmol1(smolfrom,atominfo);
//      MolFileMolecule mfmol2(smolfrom,atominfo);
//      MolFileMolecule mfmol3(smolfrom,atominfo);
     cout << "\n--------------------------------------------\n";
     cout << "SimpleMolecule\n";
     RxnDataSimpleMolecule *simp1 = new RxnDataSimpleMolecule();
     simp1->ReadMolFileMolecule(smolfrom,atominfo);
     RxnDataSimpleMolecule *simp2 = new RxnDataSimpleMolecule();
     simp2->ReadMolFileMolecule(smolfrom,atominfo);
     RxnDataSimpleMolecule *simp3 = new RxnDataSimpleMolecule();
     simp3->ReadMolFileMolecule(smolfrom,atominfo);

     cout << "\n=================================================\n";
     AtomCountList<int> counts(simp1,"Valence:Standard","ByName-Integer");
     
     cout << counts;
     cout << "\n--------------------------------------------\n";
     cout << counts.ValStats[6];
     
     return(0);
     }

     
