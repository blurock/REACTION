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
#include "Reaction/STL.hh"
#include "Reaction/defines.h"
#define TEMPLATE_INSTANTIATION
#include "Reaction/CommBuffer.hh"
#include "Reaction/String.hh"
#include "Reaction/Objects.hh"
#include "Reaction/LstOps.hh"

 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

static void SimpleTest();

/* Programs
 */
int main()
{
  SimpleTest();
  
  return 0;
}

static void SimpleTest()
{
  Identify id1(0,"Zero");
  Identify id2(1,"One");
  Identify id3(0,"Two");
  Identify id4(1,"Three");
  Identify id5(0,"Four");
  
  ListEvaluationFilterIdentifyByID filter(0);

  filter.ElementEquality(id1);
  filter.ElementEquality(id2);
  

  ObjectList<Identify> objs;
  objs.AddObject(id1);
  objs.AddObject(id2);
  objs.AddObject(id3);
  objs.AddObject(id4);
  objs.AddObject(id5);
  
  ListEvaluationClassesIdentify classes(objs,false);
  
  ObjectList<int> idclasses;
  idclasses.AddObject(0);
  idclasses.AddObject(1);
  
  classes.CreateClasses(idclasses);
  
  classes.print(cout);
  cout << "\n";
  
}
