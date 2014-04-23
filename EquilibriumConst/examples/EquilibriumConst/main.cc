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
#include "FunctionReal1DObjects.hh"
#include "OperationObjects.hh"
#include "LogicalOperations.hh"
#include "DataObjects.hh"
#include "InstanceObjects.hh"
#include "MenuObjects.hh"
#include "DirectedTreeObjects.hh"
#include "SelectObjects.hh"
#include "AlgorithmObjects.hh"
#include "ExpressionTree.hh"
#include "Dbase.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "ThermoTables.hh"
#include "ThermoProps.hh"
#include "Rxn.hh"
#include "Utilities.hh"
#include "EquilibriumConst.hh"

/*C EquilibriumTest
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class EquilibriumTest : public RxnSystemBase
{
 public:
  EquilibriumTest(int argc, char *argv[])
    : RxnSystemBase(argc,argv)
    {
    }
  void EncodeDecodeObjectsSetUp()
    {
      RxnSystemBase::EncodeDecodeObjectsSetUp();
      InitialSetOfThermoPropsDecodeFunctions();
      InitialRxnUtilitiesDecodeFunctions();
      InitialSetOfExpressionTreeEncodeDecodeRoutines();
      InitialParameterizedEncodeDecodeRoutines();
      InitialSetOfEquilibriumConstDecodeFunctions();
    }
  void StandardObjectsSetUp()
    {
      RxnSystemBase::StandardObjectsSetUp();
      AddThermPropClasses(getStandard());
      AddRxnUtilitiesClasses(getStandard());
      AddParameterizedClasses(getStandard());
      AddBaseExpressionTreeClasses(getStandard());
      AddEquilibriumConstClasses(getStandard());
    }
  void Initialization()
    {
      RxnSystemBase::Initialization();
    }
  void CommandSetUp()
    {
      RxnSystemBase::CommandSetUp();
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
  EquilibriumTest test(argc,argv);
  
  test.Initialization();
  test.Run();
  test.Exit();

}
