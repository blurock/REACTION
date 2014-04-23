/*  FILE     LstOps.cc
**  PACKAGE  LstOps
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "LstOps" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "Basis/System.hh"
#include "Reaction/LstOps.hh"

 
/*F ListEvaluationClassesIdentify() . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
ListEvaluationClassesIdentify::ListEvaluationClassesIdentify()
{
}
 
/*F ListEvaluationClassesIdentify(eval) . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    eval: Structure to be copied
**
**  REMARKS
**
*/
ListEvaluationClassesIdentify::ListEvaluationClassesIdentify(const ListEvaluationClassesIdentify&
							     eval)
  : ListEvaluationClasses<Identify>(eval)
{
}
 
/*F ListEvaluationClassesIdentify(lst,unique) constructor
**
**  DESCRIPTION
**    lst: The list of objects
**    unique: true if only unique objects in classes
**
**  REMARKS
**
*/
ListEvaluationClassesIdentify::ListEvaluationClassesIdentify(const ObjectList<Identify>& lst,
				const bool unique)
  : ListEvaluationClasses<Identify>(lst,unique)
{
}
/*F CreateClasses(filters)
**
**  DESCRIPTION
**    filters: The set of filters to create the classes
**
**  REMARKS
**
*/
void ListEvaluationClassesIdentify::CreateClasses(const ObjectList< String >& names)
{
  ObjectList<Identify> *orig = new ObjectList<Identify>(List);
  for(ObjectList<String>::const_iterator name = names.begin();
      name != names.end();
      name++)
    {
      ListEvaluationFilterIdentifyByName filter(*name);
      ObjectList<Identify> *newlist = new ObjectList<Identify>(List);
      ObjectList<Identify>::iterator iter = remove_if(newlist->begin(),
						      newlist->end(),
						      filter);
      newlist->erase(iter,newlist->end());
      
      Classes.AddObject(*newlist);
      
      if(UniqueElements)
	{
	  filter.ToggleEquality();
	  ObjectList<Identify>::iterator iter1 = remove_if(orig->begin(),
							  orig->end(),
							  filter);
	  orig->erase(iter1,orig->end());
	  
	  filter.ToggleEquality();
	}
    }
}
/*F CreateClasses(filters)
**
**  DESCRIPTION
**    filters: The set of filters to create the classes
**
**  REMARKS
**
*/
void ListEvaluationClassesIdentify::CreateClasses(const ObjectList< int >& ids)
{
  ObjectList<Identify> *orig = new ObjectList<Identify>(List);
  
  for(ObjectList<int>::const_iterator id = ids.begin();
      id != ids.end();
      id++)
    {
      ListEvaluationFilterIdentifyByID filter(*id);
      ObjectList<Identify> *newlist = new ObjectList<Identify>(List);
      ObjectList<Identify>::iterator iter = remove_if(newlist->begin(),
						      newlist->end(),
						      filter);
      newlist->erase(iter,newlist->end());
      
      Classes.AddObject(*newlist);
      
      if(UniqueElements)
	{
	  filter.ToggleEquality();
	  ObjectList<Identify>::iterator iter1 = remove_if(orig->begin(),
							   orig->end(),
							   filter);
	  orig->erase(iter1,orig->end());
	  filter.ToggleEquality();
	}
    }
}

