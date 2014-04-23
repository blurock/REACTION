/*  FILE     LstOps.hh
**  PACKAGE  LstOps
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "LstOps" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_LSTOPS_HH
#define REACTION_LSTOPS_HH
 
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

/*C ListEvaluationFilter  . . . . . . . . . . . . . . . . .  filter for lists
**
**  DESCRIPTION
**     Identify: Identifier for equality
**     Element: The element to be compared
**     Equality: true if equality means true and
**               false if equality means false
**
**  REMARKS
**
*/
template <class T>
class ListEvaluationFilter : public Identify
{
public:
  virtual bool ElementEquality(const T& element)
    {
      return Element == element;
    }
  void ToggleEquality()
    {
      if(Equality)
	Equality = false;
      else
	Equality = true;
    }
  
  T Element;
  bool Equality;

  ListEvaluationFilter()
    : Equality(true)
    {
    }
  
  ListEvaluationFilter(const Identify& id,
		       const T& element)
    : Identify(id),
    Element(element), 
    Equality(true)
   {
    }
  bool operator()(const T& element)
    {
      bool ans;
      if(Equality)
	ans = ElementEquality(element);
      else 
	ans =  -ElementEquality(element);
      return ans;
    }
  
};

    
/*C ListEvaluation
**
**  DESCRIPTION
**     
**  REMARKS
**
*/
template <class T>
class ListEvaluation
{
public:
  ObjectList<T> List;
  T First;
  T Last;

  ListEvaluation();
  ListEvaluation(const ListEvaluation<T>& eval);
  ListEvaluation(const ObjectList<T>& lst);
  void CopyClone(ListEvaluation<T>* eval);
  ListEvaluation<T>* Clone();
  ostream& print(ostream& out) const;
  bool EncodeThis(CommBuffer& buffer);
  bool DecodeThis(CommBuffer& buffer);

  ObjectList<T> Filter(ListEvaluationFilter<T>& filter,
		       bool replace);
  friend ListEvaluationFilter<T>;
  
};

 
/*C ListEvaluationClasses
**
**  DESCRIPTION
**
**  REMARKS
**
*/
template <class T>
class ListEvaluationClasses : public ListEvaluation<T>
{
public:
  bool UniqueElements;
  ObjectList< ObjectList<T> > Classes;
  
  ListEvaluationClasses();
  ListEvaluationClasses(const ListEvaluationClasses<T>& eval);
  ListEvaluationClasses(const ObjectList<T>& lst,
			const bool unique);
  void CopyClone(ListEvaluationClasses<T>* eval);
  ListEvaluationClasses<T>* Clone();
  ostream& print(ostream& out) const;
  bool EncodeThis(CommBuffer& buffer);
  bool DecodeThis(CommBuffer& buffer);
  void CreateClasses(ObjectList< ListEvaluationFilter<T> >& filters);
  
};


/*C ListEvaluationFilterIdentifyByName
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class ListEvaluationFilterIdentifyByName : public ListEvaluationFilter<Identify>
{
public:
  ListEvaluationFilterIdentifyByName(const String& name)
    : ListEvaluationFilter<Identify>(),
    Identify::NameTag("Identify by Name"),
    ListEvaluationFilter<Identify>::Element(name)
    {
    }
  
  virtual bool ElementEquality(const Identify& id)
    {
      return id.NameTag == Element.NameTag;
    }
  friend ListEvaluationFilter<Identify>;
  
};

/*C ListEvaluationFilterIdentifyByID
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class ListEvaluationFilterIdentifyByID : public ListEvaluationFilter<Identify>
{
public:
  ListEvaluationFilterIdentifyByID(const int id)
    : ListEvaluationFilter<Identify>(),
    Identify::NameTag("Identify by ID"),
    ListEvaluationFilter<Identify>::Element(id)
    {
    }

  virtual bool ElementEquality(const Identify& id)
    {
      return id.Identification == Element.Identification;
    }
  friend ListEvaluationFilter<Identify>;
  
};


    
/*C ListEvaluationClassesIdentify
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class ListEvaluationClassesIdentify : public ListEvaluationClasses<Identify>
{
public:
  ListEvaluationClassesIdentify();
  ListEvaluationClassesIdentify(const ListEvaluationClassesIdentify& eval);
  ListEvaluationClassesIdentify(const ObjectList<Identify>& lst,
				const bool unique);
  
  void CreateClasses(const ObjectList<int>& ids);
  void CreateClasses(const ObjectList<String>& names);
};


#include "ReactionSrc/LstOps/LstOps.icc"
#ifdef TEMPLATE_INSTANTIATION
#include "ReactionSrc/LstOps/LstOps.itc"
#endif

#endif


