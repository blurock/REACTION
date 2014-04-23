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
#include "StaticAtom.hh"

class StaticInfoTest : public InstanceSystemSave
{
public:

  StaticInfoTest(int argc, char *argv[])
    : InstanceSystemSave(argc,argv)
    {
    }
  void StandardObjectsSetUp()
    {
      InstanceSystem::StandardObjectsSetUp();
      AddStaticAtomClasses(getStandard());
    }

  void EncodeDecodeObjectsSetUp()
    {
      InstanceSystem::EncodeDecodeObjectsSetUp();
      InitialSetOfStaticAtomEncodeDecodeRoutines();
    }
  
  void CommandSetUp()
    {
      InstanceSystemSave::CommandSetUp();
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
