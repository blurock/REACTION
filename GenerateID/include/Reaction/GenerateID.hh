/*  FILE     GenerateID.hh
**  PACKAGE  GenerateID
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "GenerateID" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_GENERATEID_HH
#define REACTION_GENERATEID_HH

/*C CalculateAndInsertObject  . . . . . . . . . . . . . . . . . virtual class
**
**  DESCRIPTION
**
**  REMARKS
**
*/
template <class Object>
class CalculateAndInsertObject
     {
 public:
     
     virtual Object& InsertIDIntoObject(Object& object,
				     const int id,
				     const int num) = 0;
     virtual int CalculateBaseID(Object& molecule) = 0;
     };
/*C IdentifyName  . . . . . . . . . . . . . work: search for name in Identify
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class IdentifyNameOfObject
     {
     String NameOfObject;
 public:
     
     IdentifyNameOfObject(const String& name)
	  : NameOfObject(name)
	       {
	       }
	       
     operator()(const Identify& id)
	  {
	  int ans = strcmp(id.NameTag.chars(),NameOfObject.chars());
	  if(ans == 0)
	       return true;
	  else 
	       return false;
	  }
     };
/*C EquivalentIDs . . . . . . . . . . . . . . list of semi-equivalent objects
**
**  DESCRIPTION
**    Since several objects can have the same ID, a further mechanism
**    (i.e. this class is needed to keep track of the equivalent
**    objects.  The String name plays a key role here.  
**
**  REMARKS
**
*/
template <class Object>
class EquivalentIDs
: public ObjectList<Identify>
     {
 public:
     EquivalentIDs()
	  {
	  }
     Identify SearchForElement(const String& name)
	  {
	  IdentifyNameOfObject findname(name);
	  Identify id;
	  
	  
	  ObjectList<Identify>::iterator object = find_if(begin(),end(),
							  findname);
	  if(object == end())
	       {
	       return Identify(0,"");
	       }
	  else 
	       {
	       return *object;
	       }
	  }
     Object& InsertIDElement(CalculateAndInsertObject<Object>* insert,
			  Object& object,
			  const int identification, 
			  const String& name)
	  {
	  Identify id = SearchForElement(name);
	  
	  if(id.Identification == 0)
	       {
	       int num = size();
	       object = insert->InsertIDIntoObject(object,identification,num);
	       Identify newid(object.Identification,name);
	       AddObject(newid);
	       }
	  else
	       {
	       object.Identification = id.Identification;
	       }
	  
	  return object;
	  }
     void RemoveIDElement(const String& name)
	  {
	  Identify id = SearchForElement(name);
	  RemoveObject(id);
	  }
     };
/*C SetOfIDs  . . . . . . . . . . . . . . . . . . . . . . Generate ID numbers
**
**  DESCRIPTION
**     From a given Object, the ID number is generated
**
**  REMARKS
**
*/
template <class Object>
class SetOfIDs
: public SearchableObjectListSimple<int,EquivalentIDs<Object> >
     {
     CalculateAndInsertObject<Object>* InsertAndCalc;
     
 public:
     SetOfIDs() : InsertAndCalc(0)
	  {
	  }
     
     SetOfIDs(CalculateAndInsertObject<Object>* insert)
	  : InsertAndCalc(insert)
	       {
	       }
     Object& InsertNewElement(Object& object)
	  {
	  int id = InsertAndCalc->CalculateBaseID(object);
	  String& name = object.NameTag;
	  
	  object = (*this)[id].InsertIDElement(InsertAndCalc,object,id,name);
	  
	  return object;
	  }
     void RemoveIDElement(const int id, const String& name)
	  {
	  (*this)[id].RemoveIDElement(name);
	  }
     ostream& operator<<(ostream& out)
       {
	 return print(out);
       }
     
     };     
     
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
template <class Object>
ostream& operator<<(ostream& out,const EquivalentIDs<Object>& equiv);
template <class Object>
ostream& operator<<(ostream& out, const SearchableObjectList<int,EquivalentIDs<Object> >& list);
template <class Object>
bool Encode(CommBuffer& buffer, EquivalentIDs<Object>& equiv);
template <class Object>
bool Decode(CommBuffer& buffer, EquivalentIDs<Object>& equiv);

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#ifdef TEMPLATE_INSTANTIATION
#include "ReactionSrc/GenerateID/GenerateID.itc"
#endif

#endif
