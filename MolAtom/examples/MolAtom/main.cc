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
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"

class StaticInfoTest : public InstanceSystemMenu
{
public:

  StaticInfoTest(int argc, char *argv[])
    : InstanceSystemMenu(argc,argv)
    {
    }
  void StandardObjectsSetUp()
    {
      InstanceSystemMenu::StandardObjectsSetUp();
      AddMolAtomClasses(getStandard());
      AddStaticAtomClasses(getStandard());
    }

  void EncodeDecodeObjectsSetUp()
    {
      InstanceSystemMenu::EncodeDecodeObjectsSetUp();
      InitialSetOfMolAtomEncodeDecodeRoutines();
      InitialSetOfStaticAtomEncodeDecodeRoutines();
    }
  
  void CommandSetUp()
    {
      InstanceSystemMenu::CommandSetUp();
    }
};
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int main(int argc, char *argv[])
{
  StaticInfoTest test(argc,argv);
  
  test.Initialization();
  test.Run();
  test.Exit();
}
