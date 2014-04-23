/*  FILE     MolStats.cc
**  PACKAGE  MolStats
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "MolStats" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "FullSystem.hh"
#include "Dbase.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "MolStats.hh"
#include "ExpressionTree.hh"
#include "DistributionAlgorithm.hh"

template class AtomicNumberCount<int>;
template ostream& operator<<(ostream &, AtomicNumberCount<int> const &);
template ostream& operator<<(ostream &, AtomCountList<int> const &);
template bool Encode(CommBuffer &, AtomicNumberCount<int> &);
template bool Decode(CommBuffer &, AtomicNumberCount<int> &);
//template SearchableObjectListSimple<int, AtomicNumberCount<int> >;
/*S Classes
*/
/*C GroupValenceI
**
**  DESCRIPTION
**
**  REMARKS
**
*/
template <class ValenceType>
class GroupValenceInJ
     {
 public:
     
     GroupValenceInJ()
	  {
	  }
     BasicPair<int,ValenceType> operator()(const PairList<int,ValenceType>& pairs)
	  {
	  BasicPair<int,ValenceType> firstpair = pairs.front();
	  BasicPair<int,ValenceType> pair( firstpair.J, pairs.size());
	  return pair;
	  }
     };
/*C GroupAtomicNumberInI<ValenceType>
**
**  DESCRIPTION
**     For every set of pairs where the first element is the same 
**     atomic number, the valences (in the J element) are grouped
**     and counted (using GroupValenceInJ)
**
**  REMARKS
**
*/
template <class ValenceType>
class GroupAtomicNumberInI
     {
 public:
     GroupAtomicNumberInI()
	  {
	  }
     
     AtomicNumberCount<ValenceType> operator()(const PairList<int,ValenceType>& pairs)
	  {
	  AtomicNumberCount<ValenceType> counts;
	  
	  BasicPair<int,ValenceType> firstpair = pairs.front();
	  counts.AtomicNumber = firstpair.I;
	  counts.AtomCount = pairs.size();
	  
	  GroupValenceInJ<ValenceType> groupval;
	  SetOfPairSets<int,ValenceType> grouped;
	  grouped.IterativeValueJ(pairs);
	  transform(grouped.begin(),
		    grouped.end(),
		    back_insert_iterator< PairList<int,ValenceType> > (counts.ValenceAndCounts),
		    groupval);
	  return counts;
	  }
     };
/*S CountConversion
 */
/*F CountConversion . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
template <class VType> 
    CountConversion<VType>::CountConversion(VType base)
	: BaseType(base)
	{
	}

/*F operator()(obj) . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
template <class VType> int CountConversion<VType>::operator()(BaseDataObject *obj)
{
  Convert(obj);
  return BaseType;
}

/*F operator()(obj) . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
template <class VType> void CountConversion<VType>::Convert(BaseDataObject *obj)
{
  BaseDataNumeric *numobj = (BaseDataNumeric *) obj;
  double dist = numobj->Distance();
  BaseType = (VType) dist;
}
/*S IntegerPropertyFromNumericOperation
 */
    IntegerPropertyFromNumericOperation::IntegerPropertyFromNumericOperation()
	: CountConversion<int>(0)
	{
	}
  void IntegerPropertyFromNumericOperation::Convert(BaseDataObject *num)
	{
	    BaseDataNumeric *numobj = (BaseDataNumeric *) num;
	    double dist = numobj->Distance();
	    BaseType = (int) dist;
	}

/*S AtomicNumberCount
 */
 
/*S AtomCountList
 */
/*F AtomCountList(molecule,type)  . . . . . . . . . . .  create and calculate
**
**  DESCRIPTION
**    molecule: The molecule on which to do the statistics
**    type: The valence (property) type
**
**    The statistics are formed with the constructor
**
**  REMARKS
**
*/
template <class ValenceType>
    AtomCountList<ValenceType>::AtomCountList(RxnDataSimpleMolecule& molecule, 
					      const String& property,
					      CountConversion<ValenceType> *convert)
{
  BaseDataSetOfObjects *atoms = molecule.getNodes();
  ObjectList<String> names = atoms->ListOfObjectNames();
  for(ObjectList<String>::iterator name = names.begin(); name != names.end();name++)
    {
      RxnDataMoleculeAtom *a = (RxnDataMoleculeAtom *) molecule.getNode(*name);
      RxnDataBasicAtomData *data = a->getBasicAtomData();
      AtomicNumbers.push_back(data->AtomicNumber);
      BaseDataObject *obj = a->GetObject(property);

      /*      
      cout << "--------------------------------------------------------------" << endl;
      cout << "Property: '" << property << "'" << endl;
      a->print(cout);
      cout << endl;
      obj->print(cout);
      cout << endl;
      int aaa = convert->operator()(obj);
      cout << "VAL:" << aaa << endl;
      cout << "--------------------------------------------------------------" << endl;
      */
      Valences.push_back(convert->operator()(obj));
      for(unsigned int hcount = 0; hcount < data->HydrogenCount;hcount++)
	AtomicNumbers.push_back(1);
    }
  
    PairList<int,ValenceType> valatm(AtomicNumbers,Valences);
    SetOfPairSets<int,ValenceType> grouped;
    
    grouped.IterativeValueI(valatm);
    GroupAtomicNumberInI<ValenceType> groupi;
    
    typename SetOfPairSets<int,ValenceType>::iterator i;
    for(i=grouped.begin();i!=grouped.end();i++)
	{
	    AtomicNumberCount<ValenceType> element = groupi( (*i) );
	    /*
	    cout << "Add ValStats: " << element.AtomicNumber << "(" << element.AtomCount << ")" << endl;
	    element.print(cout);
	    cout << endl;
	    */
	    ValStats.AddObject(element.AtomicNumber,element );
	}
    
    Valences.Unique();
    AtomicNumbers.Unique();
}

/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
template <class ValenceType>
ostream& AtomCountList<ValenceType>::print(ostream& out) const
{
  out << "Unique Valences: ";
  out << Valences;
  out << "\nUnique Atomic Numbers = ";
  out << AtomicNumbers;
  out << "\n--------------------------------\n";
  ValStats.print(out);
  out << "\n--------------------------------\n";
  return out;
}
/*S RxnDataAtomStatistics
 */ 
/*F RxnDataAtomStatistics()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataAtomStatistics::RxnDataAtomStatistics()
{
  Identification = MOLSTATS_ATOMS_ID;
  NameTag = MOLSTATS_ATOMS_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataAtomStatistics(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataAtomStatistics::RxnDataAtomStatistics(const RxnDataAtomStatistics& data)
  : BaseDataSetOfObjects(data),
    AtomCounts(data.AtomCounts)
{
}
/*F RxnDataAtomStatistics(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataAtomStatistics::RxnDataAtomStatistics(RxnDataSimpleMolecule& molecule,
					     const String& property,
					     RxnDataAtomInformation& atomsinfo,
					     CountConversion<int> *convert)
     : AtomCounts(molecule,property,convert)
{
  Identification = MOLSTATS_ATOMS_ID;
  NameTag = MOLSTATS_ATOMS_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;

  ObjectList<int>::iterator a;
  for(a = AtomCounts.AtomicNumbers.begin();
      a != AtomCounts.AtomicNumbers.end();
      a++)
    {
      AtomicNumberCount<int> atc = AtomCounts.ValStats[*a];
      int a1 = atc.AtomCount;
      BaseDataInteger *count = new BaseDataInteger;
      count->NameTag = atomsinfo.AtomNameFromAtomicNumber(*a);
      count->SetValue(a1);
      AddObject(count);
      delete count;
    }
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataAtomStatistics
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataAtomStatistics::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataAtomStatistics
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataAtomStatistics::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataSetOfObjects::Read(in,objc,name);
  return result;
} 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataAtomStatistics
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataAtomStatistics::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  AtomCounts.print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataAtomStatistics
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataAtomStatistics::Clone()
{
  RxnDataAtomStatistics *obj = new RxnDataAtomStatistics(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataAtomStatistics
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataAtomStatistics::CopyClone(Identify * obj)
{
  RxnDataAtomStatistics *objfull = (RxnDataAtomStatistics *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataAtomStatistics
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataAtomStatistics::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  result = result && AtomCounts.EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataAtomStatistics
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataAtomStatistics::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  result = result && AtomCounts.EncodeThis(buffer);
  return result;
}
/*F getAtomicNumbers() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
ObjectList<int>& RxnDataAtomStatistics::getAtomicNumbers() {
  return AtomCounts.AtomicNumbers;
}
/*F getAtomicNumberCount(int atnum) . . . . . . . . . . . . . . . . Atomic Number count
**
**  DESCRIPTION
**
**  REMARKS
**
*/
AtomicNumberCount<int>& RxnDataAtomStatistics::getAtomicNumberCount(int atnum) {
  return AtomCounts.ValStats[atnum];
}

/*S RxnAtomStatisticsClass
 */
/*F RxnAtomStatisticsClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnAtomStatisticsClass::RxnAtomStatisticsClass()
{
  Identification = MOLSTATS_ATOMS_ID;
  NameTag = MOLSTATS_ATOMS_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
} 
/*F RxnAtomStatisticsClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnAtomStatisticsClass::RxnAtomStatisticsClass(const RxnAtomStatisticsClass& data)
  : DataSetOfObjectsClass(data)
{
} 
 
/*F RxnAtomStatisticsClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnAtomStatisticsClass::RxnAtomStatisticsClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = "AtomStatistics";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnAtomStatisticsClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnAtomStatisticsClass::print(ostream& out) const
{
  DataSetOfObjectsClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnAtomStatisticsClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnAtomStatisticsClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnAtomStatisticsClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataSetOfObjectsClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnAtomStatisticsClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnAtomStatisticsClass::CopyClone(Identify *  objc)
{
  RxnAtomStatisticsClass *objcfull = (RxnAtomStatisticsClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnAtomStatisticsClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnAtomStatisticsClass::Clone()
    {
      RxnAtomStatisticsClass* id = new RxnAtomStatisticsClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnAtomStatisticsClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnAtomStatisticsClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnAtomStatisticsClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnAtomStatisticsClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::DecodeThis(buffer);
  //result = result && PointerDecode(buffer,(BaseDataObject *&) Class);
  //result = result && Decode(buffer,-----);

  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnAtomStatisticsClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataAtomStatistics();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnAtomStatisticsClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnAtomStatisticsClass*& obj)
     {
     obj = new RxnAtomStatisticsClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataAtomStatistics
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataAtomStatistics*& obj)
     {
     obj = new RxnDataAtomStatistics;
     return obj->DecodeThis(buffer);
     }

/*S RxnDataFormMoleculeSetStatistics
 */ 
/*F RxnDataFormMoleculeSetStatistics()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataFormMoleculeSetStatistics::RxnDataFormMoleculeSetStatistics()
  : MoleculeNameListS("MoleculeNameList"),
    RootNameS("RootName"),
    ValencesInMoleculeKey("ValencesInMolecule"),
    AtomsInMoleculeKey("AtomsInMolecule")
{
  Identification = MOLSTATS_FORMSTATS_ID;
  NameTag = MOLSTATS_FORMSTATS_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataFormMoleculeSetStatistics(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataFormMoleculeSetStatistics::RxnDataFormMoleculeSetStatistics(const RxnDataFormMoleculeSetStatistics& data)
  : BaseDataAlgorithmOperation(data),
    MoleculeNameListS(data.MoleculeNameListS),
    RootNameS(data.RootNameS),
    ValencesInMoleculeKey(data.ValencesInMoleculeKey),
    AtomsInMoleculeKey(data.AtomsInMoleculeKey)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataFormMoleculeSetStatistics
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataFormMoleculeSetStatistics
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataFormMoleculeSetStatistics
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataFormMoleculeSetStatistics::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Algorithm to calculate distributions of molecular atom properties" << endl;
  out << "Input: '" << RootNameS <<"'       The root name of the distributions" << endl;
  out << "       '" << MoleculeNameListS << "'     The list of molecules to use" << endl;
  out << "Possible Keys: " << ValencesInMoleculeKey << ", " << AtomsInMoleculeKey << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataFormMoleculeSetStatistics
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataFormMoleculeSetStatistics::Clone()
{
  RxnDataFormMoleculeSetStatistics *obj = new RxnDataFormMoleculeSetStatistics(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataFormMoleculeSetStatistics
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataFormMoleculeSetStatistics::CopyClone(Identify * obj)
{
  RxnDataFormMoleculeSetStatistics *objfull = (RxnDataFormMoleculeSetStatistics *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataFormMoleculeSetStatistics
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,MoleculeNameListS);
  result = result && Encode(buffer,RootNameS);
  result = result && Encode(buffer,ValencesInMoleculeKey);
  result = result && Encode(buffer,AtomsInMoleculeKey);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataFormMoleculeSetStatistics
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,MoleculeNameListS);
  result = result && Decode(buffer,RootNameS);
  result = result && Decode(buffer,ValencesInMoleculeKey);
  result = result && Decode(buffer,AtomsInMoleculeKey);
  return result;
}
/*F ans = SetUpAlgorithms(instances,instancesclass,run,runclass)  . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::SetUpAlgorithms(BaseDataSetOfInstances *instances,
							DataSetOfInstancesClass *instancesclass,
							BaseDataAlgorithmRun *run,
							DataAlgorithmRunClass *runclass)
{
  return true;
}
/*F ans = CheckInput(instances,instancesclass,run,runclass) . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::CheckInput(BaseDataSetOfInstances *instances,
						   DataSetOfInstancesClass *instancesclass,
						   BaseDataAlgorithmRun *run,
						   DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result && CheckInputVariable(MoleculeNameListS,"Molecule Name List",run);
  result = result && CheckInputVariable(RootNameS,"Root Name of Distributions",run);
  return result;
}
/*F ans = SetUpInput(instances,instancesclass,run,runclass) . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::SetUpInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;

  BaseDataString *namelistname = (BaseDataString *) run->ParameterValue(MoleculeNameListS);
  BaseDataString *rootname     = (BaseDataString *) run->ParameterValue(RootNameS);
  namelistname->print(cout);
  RootName = rootname->getString();

  MoleculeNameList = namelistname->getString();
  if(instances->IsInList(MoleculeNameList)) {
    MoleculeNames = (BaseDataKeyWords *) instances->GetObject(MoleculeNameList);
  } else {
    cerr << "The names of the molecules '" << MoleculeNameList << "' is not in attributes" << endl;
    result = false;
  }
  return result;
}
/*F ans = Calculate(instances,instancesclass,run,runclass)  . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::Calculate(BaseDataSetOfInstances *instances,
						 DataSetOfInstancesClass *instancesclass,
						 BaseDataAlgorithmRun *run,
						 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  cout << "Calculate Molecule Set Statistics" << endl;
  if(run->AlgorithmSummary.KeyWordInList(ValencesInMoleculeKey))
    ValencesInMolecule = true;
  else 
    ValencesInMolecule = false;
  if(run->AlgorithmSummary.KeyWordInList(AtomsInMoleculeKey))
    AtomsInMolecule = true;
  else
    AtomsInMolecule = false;
  BaseDataKeyWords *names = (BaseDataKeyWords *) MoleculeNames->Clone();
  Distributions = new BaseDataSetOfObjects();
  CountNames = new BaseDataKeyWords();
  VariablesInInstance = new BaseDataKeyWords();
  VariablesInInstance->NameTag = "VariablesInInstance";
  NumberOfMolecules = names->SizeOf();
  int cnt = 0;
  while(names->SizeOf()) {
    String name = names->NextKey();
    cout << "Molecule: '" << name << "'" << endl;
    if(instances->InstanceInSet(name)) {
      BaseDataInstance *molinstance = instances->GetInstance(name);
      if(molinstance->IsInList(MoleculeInInstance)) {
	RxnDataSimpleMolecule *molecule = (RxnDataSimpleMolecule *) molinstance->GetObject(MoleculeInInstance);
	int mtype = molecule->GetType();
	RxnSimpleMoleculeClass *molclass = (RxnSimpleMoleculeClass *) instancesclass->PointerToAllowedClasses()->GetObjectClass(mtype);
	//cout << "atomclass = (RxnMoleculeAtomClass *) molclass->NodeClass;"<< endl;
	//molclass->print(cout);
	//cout << endl;
	RxnMoleculeAtomClass *atomclass = (RxnMoleculeAtomClass *) molclass->NodeClass;
	//cout << "atomdataclass = atomclass->getBasicAtomDataClass();"<< endl;
	RxnBasicAtomDataClass *atomdataclass = atomclass->getBasicAtomDataClass();
	//cout << "atominfo = atomdataclass->getAtomInformation();"<< endl;
	RxnDataAtomInformation  *atominfo = atomdataclass->getAtomInformation();
	IntegerPropertyFromNumericOperation op;
	//cout << "RxnDataAtomStatistics atomstats(*molecule,STANDARD_VALENCE,*atominfo,&op);"<< endl;
	RxnDataAtomStatistics atomstats(*molecule,STANDARD_VALENCE,*atominfo,&op);
	//cout << "Calculate Statistics" << endl;
	if(AtomsInMolecule)
	  AddAtomDistributions(molinstance,atomstats,cnt);
	if(ValencesInMolecule)
	  AddValenceDistributions(molinstance,atomstats,cnt);
      } else {
	cerr << "Molecule information not found within instance '" << name << "'" << endl;
      }
    } else {
      cerr << "Molecule not found in instances: '" << name << "'" << endl;
    }
    cnt++;
  }
  while(CountNames->SizeOf() > 0) {
    String name = CountNames->NextKey();
    BaseDataDoubleVector *vec = (BaseDataDoubleVector *) Distributions->GetObject(name);
    BaseDataAttributeDistribution dist(vec->CurrentVector());
    dist.NameTag = name;
    cout << endl << "-------------------------------" << endl;
    dist.print(cout);
    cout << endl << "-------------------------------" << endl;
    Distributions->AddObject(&dist);
  }
  return result;
}
/*F AddAtomDistributions(atomstats)    from atoms
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataFormMoleculeSetStatistics::AddAtomDistributions(BaseDataInstance *molecule,
							    RxnDataAtomStatistics& atomstats, 
							    int cnt) {
     ObjectList<String> names = atomstats.ListOfObjectNames();
     for(ObjectList<String>::iterator aname = names.begin();
	 aname != names.end();
	 aname++) {
       String *cntname = MakeCountName(*aname);
       if(!Distributions->IsInList(*cntname)) {
	 CountNames->AddKeyWord(*cntname);
	 VariablesInInstance->AddKeyWord(*aname);
	 VectorNumeric *vec = new VectorNumeric(NumberOfMolecules,0.0);
	 BaseDataDoubleVector *cnts = new BaseDataDoubleVector(*vec);
	 cnts->NameTag = *cntname;
	 Distributions->AddObject(cnts);
       }
       BaseDataDoubleVector *counts = (BaseDataDoubleVector *) Distributions->GetObject(*cntname);
       BaseDataInteger *avalue = (BaseDataInteger *) atomstats.GetObject(*aname);
       counts->CurrentVector()[cnt] = avalue->GetValue();
       BaseDataInteger acount(*avalue);
       acount.NameTag = *aname;
       molecule->AddObject(&acount);
     }
}
/*F AddValenceDistributions(atomstats) from valences
**
**  DESCRIPTION

**  REMARKS
**
*/
void RxnDataFormMoleculeSetStatistics::AddValenceDistributions(BaseDataInstance *molecule,
							       RxnDataAtomStatistics& atomstats, 
							       int cnt) {
  ObjectList<int> atomnums =  atomstats.getAtomicNumbers();
  for(ObjectList<int>::iterator atm = atomnums.begin();
      atm != atomnums.end();
      atm++) {
    AtomicNumberCount<int> atmcnt = atomstats.getAtomicNumberCount(*atm);
    PairList<int,int> valnumpairs = atmcnt.ValenceAndCounts;
    for(PairList<int,int>::iterator pair = valnumpairs.begin();
	pair != valnumpairs.end();
	pair++) {
      int num = (*pair).I;
      int val = (*pair).J;
      String valname = Int2String(val);
       if(!Distributions->IsInList(valname)) {
	 CountNames->AddKeyWord(valname);
	 VectorNumeric *vec = new VectorNumeric(NumberOfMolecules,0.0);
	 BaseDataDoubleVector *cnts = new BaseDataDoubleVector(*vec);
	 cnts->NameTag = valname;
	 Distributions->AddObject(cnts);
       }
       BaseDataDoubleVector *counts = (BaseDataDoubleVector *) Distributions->GetObject(valname);
       counts->CurrentVector()[cnt] = num;
       BaseDataInteger acount;
       acount.SetValue(num);
       acount.NameTag = valname;
       molecule->AddObject(&acount);
    }
  }
}
/*F AddValenceDistributions(atomstats) from valences
**
**  DESCRIPTION

**  REMARKS
**
*/
String *RxnDataFormMoleculeSetStatistics::MakeCountName(String& name) {
  String *cntname = new String(RootName);
  cntname->AppendToEnd(name);
  return cntname;
}
/*F ans = WriteOutputValues(instances,instancesclass,run,runclass)  algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  instances->AddObject(VariablesInInstance);
  ObjectList<String> names = VariablesInInstance->GetKeyWords();
  for(ObjectList<String>::iterator name=names.begin(); name != names.end();name++) {
    String *cntname = MakeCountName(*name);
    BaseDataObject *obj = Distributions->GetObject(*cntname);
    instances->AddObject(obj);
  }
//  run->AddParameter(ctree);

  return result;
}
/*F ans = ConcludeRun(instances,instancesclass,run,runclass)  . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataFormMoleculeSetStatistics::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 

/*S RxnFormMoleculeSetStatisticsClass
 */
/*F RxnFormMoleculeSetStatisticsClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnFormMoleculeSetStatisticsClass::RxnFormMoleculeSetStatisticsClass()
{
  Identification = MOLSTATS_FORMSTATS_ID;
  NameTag = MOLSTATS_FORMSTATS_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnFormMoleculeSetStatisticsClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnFormMoleculeSetStatisticsClass::RxnFormMoleculeSetStatisticsClass(const RxnFormMoleculeSetStatisticsClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnFormMoleculeSetStatisticsClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnFormMoleculeSetStatisticsClass::RxnFormMoleculeSetStatisticsClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "FormMoleculeSetStatistics";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnFormMoleculeSetStatisticsClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnFormMoleculeSetStatisticsClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnFormMoleculeSetStatisticsClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnFormMoleculeSetStatisticsClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnFormMoleculeSetStatisticsClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnFormMoleculeSetStatisticsClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnFormMoleculeSetStatisticsClass::CopyClone(Identify *  objc)
{
  RxnFormMoleculeSetStatisticsClass *objcfull = (RxnFormMoleculeSetStatisticsClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnFormMoleculeSetStatisticsClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnFormMoleculeSetStatisticsClass::Clone()
    {
      RxnFormMoleculeSetStatisticsClass* id = new RxnFormMoleculeSetStatisticsClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnFormMoleculeSetStatisticsClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnFormMoleculeSetStatisticsClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnFormMoleculeSetStatisticsClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnFormMoleculeSetStatisticsClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::DecodeThis(buffer);
  //result = result && PointerDecode(buffer,(BaseDataObject *&) Class);
  //result = result && Decode(buffer,-----);

  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnFormMoleculeSetStatisticsClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataFormMoleculeSetStatistics();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnFormMoleculeSetStatisticsClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnFormMoleculeSetStatisticsClass*& obj)
     {
     obj = new RxnFormMoleculeSetStatisticsClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataFormMoleculeSetStatistics
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataFormMoleculeSetStatistics*& obj)
     {
     obj = new RxnDataFormMoleculeSetStatistics;
     return obj->DecodeThis(buffer);
     }
/*S Utilities
 */ 
/*F AddMolStatsClasses(set)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void AddMolStatsClasses(DataSetOfObjectsClass& set)
{
  String atomstatdescr("The Atom Statistics Class");
  RxnAtomStatisticsClass atomstatclass(MOLSTATS_ATOMS_ID,MOLSTATS_ATOMS_NAME,atomstatdescr);
  set.AddObjectClass(atomstatclass);
  
  String formdescr("The Form Molecule Set Statistics Class");
  RxnFormMoleculeSetStatisticsClass formclass(MOLSTATS_FORMSTATS_ID,MOLSTATS_FORMSTATS_NAME,formdescr);
  set.AddObjectClass(formclass);
}
 
/*F InitialSetOfMolStatsDecodeFunctions()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialSetOfMolStatsDecodeFunctions()
{
  EncodeDecodeRegisterClass(RxnAtomStatisticsClass,RxnDataAtomStatistics,MOLSTATS_ATOMS_NAME);
  EncodeDecodeRegisterClass(RxnFormMoleculeSetStatisticsClass,RxnDataFormMoleculeSetStatistics,MOLSTATS_FORMSTATS_NAME);
}
