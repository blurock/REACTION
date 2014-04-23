/*  FILE     main.cc
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
#define TEMPLATE_INSTANTIAION
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
#include "Dbase.hh"

int DbaseTestRoutine(ReactionSystemBase* sys);
int DbaseStart( ReactionSystemBase* sys);

class DbaseTest : public InstanceSystemMenu
{
public:

  DbaseTest(int argc, char *argv[])
    : InstanceSystemMenu(argc,argv)
    {
    }
  void StandardObjectsSetUp()
    {
      InstanceSystemMenu::StandardObjectsSetUp();
      AddStaticAtomClasses(getStandard());
      AddDBaseClasses(getStandard());
    }

  void EncodeDecodeObjectsSetUp()
    {
      InstanceSystemMenu::EncodeDecodeObjectsSetUp();
      InitialSetOfStaticAtomEncodeDecodeRoutines();
      InitialSetOfDBaseEncodeDecodeRoutines();
    }
  
  void CommandSetUp()
    {
      InstanceSystemMenu::CommandSetUp();
      SingleSystemCommand dbasetest("DbaseTest",
				    "Data Base Test",
				    &DbaseTestRoutine);
      Commands.AddObject(dbasetest.getName(),dbasetest);

      SingleSystemCommand dbasestart("DbaseStart",
				    "Start Data Base",
				    &DbaseStart);
      Commands.AddObject(dbasestart.getName(),dbasestart);
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
  DbaseTest test(argc,argv);
  
  test.Initialization();
  test.Run();
  test.Exit();
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int DbaseStart( ReactionSystemBase* sys)
{
  DbaseTest *test = (DbaseTest *) sys;
  int result = 0;
  if(test->Inputs.size() < 1)
    {
      cerr << "Database name (from attributes)" << endl;
      return 1;
    }

  String dbaseS     = test->GetNextInput();
  if(test->Instances.IsInList(dbaseS))
    {
      BaseDataDataBaseInformation *dbase = (BaseDataDataBaseInformation *) 
	test->Instances.GetObject(dbaseS);
      unsigned int dbasetype = dbase->GetType();
      DataDataBaseInformationClass *dbaseclass = (DataDataBaseInformationClass *)
	test->InstanceClasses.GetObjectClass(dbasetype);

      dbase->OpenUpDataBase(dbaseclass);
    }

  return result;
}
/*F ans = DbaseTest(sys)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int DbaseTestRoutine(ReactionSystemBase* sys)
{
  DbaseTest *test = (DbaseTest *) sys;
  int result = 0;

  if(test->Inputs.size() < 3)
    {
      cerr << "Database Name: The name of the attribute with the database" << endl;
      cerr << "Key:  'Read' or 'Write'" << endl;
      cerr << "Attributes: The attribute to read/write" << endl;
      return 1;
    }
  String dbaseS     = test->GetNextInput();
  String rwkey      = test->GetNextInput();
  String attributeS = test->GetNextInput();

  if(test->Instances.IsInList(dbaseS))
    {
      BaseDataDataBaseInformation *dbase = (BaseDataDataBaseInformation *) 
	test->Instances.GetObject(dbaseS);
      unsigned int dbasetype = dbase->GetType();
      DataDataBaseInformationClass *dbaseclass = (DataDataBaseInformationClass *)
	test->InstanceClasses.GetObjectClass(dbasetype);
      dbase->OpenUpDataBase(dbaseclass);
      if(rwkey == "Write")
	{
	  if(test->Instances.IsInList(attributeS))
	    {
	      BaseDataObject *attribute = test->Instances.GetObject(attributeS);
	      bool res = dbase->StoreElement(attribute);
	      if(!res)
		{
		  cerr << "StoreElement failed" << endl;
		  result = 1;
		}
	    }
	  else
	    {
	      cerr << "Attribute to write not in list" << endl;
	      result = 1;
	    }
	}
      else
	{
	  BaseDataObject *element;
	  bool result = dbase->FetchElement(attributeS,dbaseclass,element);
	  if(result)
	    {
	      cout << "The Retrieved Element:" << endl;
	      element->print(cout);
	      cout << endl;
	    }
	  else
	    {
	      cerr << "Element not retrieved" << endl;
	    }
	}
    }
  else 
    {
      cerr << "Database not in list of attributes" << endl;
      result = 1;
    }

  return result;
}
