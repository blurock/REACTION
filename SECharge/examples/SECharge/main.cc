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
#include "CoreDataObjects.hh"
#include "Vector.hh"
#include "NumericObjects.hh"
#include "LogicalObjects.hh"
#include "FunctionReal1DObjects.hh"
#include "OperationObjects.hh"
#include "LogicalOperations.hh"
#include "DataObjects.hh"
#include "InstanceObjects.hh"
#include "DirectedTreeObjects.hh"
#include "SelectObjects.hh"
#include "AlgorithmObjects.hh"
#include "MenuObjects.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Dbase.hh"
#include "Molecule.hh"
#include "SECharge.hh"
void *STATICATOMINFO = NULL;
int Test(ReactionSystemBase* sys);

/*C MoleculeSystemBase  . . . . . . . . . . . . . . . . . . .  Molecule Basis
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class SEChargeSystemBase : public MoleculeSystemBase
{
public:
  String MoleculeDataBaseS;
  RxnDataMolecularStructuresDataBase *MoleculeDataBase;
  String MoleculeDataBaseClassS;
  RxnMolecularStructuresDataBaseClass *MoleculeDataBaseClass;

  SEChargeSystemBase(int argc, char *argv[])
    : MoleculeSystemBase(argc,argv)
    {
    }
  void EncodeDecodeObjectsSetUp()
    {
      MoleculeSystemBase::EncodeDecodeObjectsSetUp();
      InitialSetOfSEChargeEncodeDecodeRoutines();
    }
  void StandardObjectsSetUp()
    {
      MoleculeSystemBase::StandardObjectsSetUp();
      AddSEChargeClasses(getStandard());
    }
  void Initialization()
    {
      MoleculeSystemBase::Initialization();
    }
  void CommandSetUp()
    {
      MoleculeSystemBase::CommandSetUp();
      SingleSystemCommand molread("Test",
				  "Test SECharge",
				  &Test);
      Commands.AddObject(molread.getName(),molread);
    }
};


/*F main  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int main(int argc, char *argv[])
{
  SEChargeSystemBase test(argc,argv);
  
  test.Initialization();
  test.Run();
  test.Exit();

}
int Test(ReactionSystemBase* sys)
{
  SEChargeSystemBase *molsystem = (SEChargeSystemBase *) sys;
  int result = 1;

  if(molsystem->Inputs.size() != 2)
    {
      cerr << "Inputs:" << endl;
      cerr << "         Instance: The instance name with the molecule" << endl;
      cerr << "         Molecule: The molecule" << endl;
    }
  String instS = molsystem->GetNextInput();
  String moleculeS = molsystem->GetNextInput();

  String moltype("Molecule");
  
  BaseDataInstance *instance = molsystem->Instances.GetInstance(instS);
  RxnDataSimpleMolecule *molecule = (RxnDataSimpleMolecule *) instance->GetObject(moleculeS);
  //unsigned int ptype = molecule->GetType();
  RxnSimpleMoleculeClass molclass;
  // = (DataSimpleMoleculeClass *) molsystem->InstanceClasses.GetObjectClass(ptype);

  RxnDataCalculateElectronegativity molele;
  
  BaseDataDoubleVector *vect = (BaseDataDoubleVector *) molele.operator()(molecule,&molclass);
  vect->print(cout);
  cout << endl;

  return result;
}
