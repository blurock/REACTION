/*  FILE     Mechanism.cc
**  PACKAGE  Mechanism
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "Mechanism" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "FullSystem.hh"
#include "Dbase.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "ThermoProps.hh"
#include "Rxn.hh"
#include "Mechanism.hh"
#include "MechanismGraph.hh"
#include "MolStats.hh"
#include "MechLumping.hh"
#include "Flame.hh"

//#include <ios>
#include <iomanip>

int InputMechanism(ReactionSystemBase* sys);

/*S RxnDataMoleculeSummary
 */ 
/*F RxnDataMoleculeSummary()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeSummary::RxnDataMoleculeSummary()
{
  Identification = MECHANISM_MOLECULE_ID;
  NameTag = MECHANISM_MOLECULE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMoleculeSummary(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMoleculeSummary::RxnDataMoleculeSummary(const RxnDataMoleculeSummary& data)
  : BaseDataObject(data),
    ThermodynamicInfo(data.ThermodynamicInfo),
    ShortName(data.ShortName)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMoleculeSummary
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMoleculeSummary::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMoleculeSummary
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMoleculeSummary::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataObject::Read(in,objc,name);
  StreamObjectInput str(in,' ');
  ThermodynamicInfo = str.ReadNext();
  ShortName = str.ReadNext();
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMoleculeSummary
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMoleculeSummary::print(ostream& out) const
{
  BaseDataObject::print(out);
  out << "Thermodynamic: '" << ThermodynamicInfo << "'   ShortName: '" << ShortName << "'" << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeSummary
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMoleculeSummary::Clone()
{
  RxnDataMoleculeSummary *obj = new RxnDataMoleculeSummary(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeSummary
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMoleculeSummary::CopyClone(Identify * obj)
{
  RxnDataMoleculeSummary *objfull = (RxnDataMoleculeSummary *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeSummary
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeSummary::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::EncodeThis(buffer);
  result = result && ThermodynamicInfo.EncodeThis(buffer);
  result = result && ShortName.EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeSummary
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeSummary::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::DecodeThis(buffer);
  result = result && ThermodynamicInfo.DecodeThis(buffer);
  result = result && ShortName.DecodeThis(buffer);
  return result;
}
/*S RxnMoleculeSummaryClass
 */
/*F RxnMoleculeSummaryClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMoleculeSummaryClass::RxnMoleculeSummaryClass()
{
  Identification = MECHANISM_MOLECULE_ID;
  NameTag = MECHANISM_MOLECULE_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMoleculeSummaryClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMoleculeSummaryClass::RxnMoleculeSummaryClass(const RxnMoleculeSummaryClass& data)
  : DataObjectClass(data)
{
} 
 
/*F RxnMoleculeSummaryClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMoleculeSummaryClass::RxnMoleculeSummaryClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataObjectClass(id,name,descr)
{
  SubClass = "Object";
  EncodeDecodeClass = "MoleculeSummary";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMoleculeSummaryClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMoleculeSummaryClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSummaryClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMoleculeSummaryClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMoleculeSummaryClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataObjectClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSummaryClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMoleculeSummaryClass::CopyClone(Identify *  objc)
{
  RxnMoleculeSummaryClass *objcfull = (RxnMoleculeSummaryClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSummaryClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMoleculeSummaryClass::Clone()
    {
      RxnMoleculeSummaryClass* id = new RxnMoleculeSummaryClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeSummaryClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeSummaryClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeSummaryClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeSummaryClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
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
BaseDataObject * RxnMoleculeSummaryClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMoleculeSummary();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMoleculeSummaryClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMoleculeSummaryClass*& obj)
     {
     obj = new RxnMoleculeSummaryClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMoleculeSummary
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMoleculeSummary*& obj)
     {
     obj = new RxnDataMoleculeSummary;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataMoleculeSummarySet
 */ 
/*F RxnDataMoleculeSummarySet()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeSummarySet::RxnDataMoleculeSummarySet()
{
  Identification = MECHANISM_MOLSUMSET_ID;
  NameTag = MECHANISM_MOLSUMSET_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMoleculeSummarySet(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMoleculeSummarySet::RxnDataMoleculeSummarySet(const RxnDataMoleculeSummarySet& data)
  : BaseDataSetOfObjects(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMoleculeSummarySet
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMoleculeSummarySet::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMoleculeSummarySet
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMoleculeSummarySet::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataSetOfObjects::Read(in,objc,name);
  RxnMoleculeSummarySetClass *setclass = (RxnMoleculeSummarySetClass *) objc;
  RxnMoleculeSummaryClass *sumclass = setclass->getMoleculeSummaryClass();
  
  StreamObjectInput str(in,' ');
  String molname = str.ReadNext();
  while(!(molname == "END"))
    {
      RxnDataMoleculeSummary *molsum = (RxnDataMoleculeSummary *) sumclass->BaseDataObjectExample();
      molsum->NameTag = molname;
      molsum->Read(in,sumclass,molname);
      AddObject(molsum);
      molname = str.ReadNext();
      delete molsum;
    }
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMoleculeSummarySet
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMoleculeSummarySet::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeSummarySet
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMoleculeSummarySet::Clone()
{
  RxnDataMoleculeSummarySet *obj = new RxnDataMoleculeSummarySet(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeSummarySet
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMoleculeSummarySet::CopyClone(Identify * obj)
{
  RxnDataMoleculeSummarySet *objfull = (RxnDataMoleculeSummarySet *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeSummarySet
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeSummarySet::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeSummarySet
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeSummarySet::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  return result;
}
/*S RxnMoleculeSummarySetClass
 */
/*F RxnMoleculeSummarySetClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMoleculeSummarySetClass::RxnMoleculeSummarySetClass()
  : MoleculeSummaryClass(NULL)
{
  Identification = MECHANISM_MOLSUMSET_ID;
  NameTag = MECHANISM_MOLSUMSET_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnMoleculeSummarySetClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMoleculeSummarySetClass::RxnMoleculeSummarySetClass(const RxnMoleculeSummarySetClass& data)
  : DataSetOfObjectsClass(data)
{
  MoleculeSummaryClass = (RxnMoleculeSummaryClass *) PointerClone(data.MoleculeSummaryClass);
} 
/*F RxnMoleculeSummarySetClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMoleculeSummarySetClass::RxnMoleculeSummarySetClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr),
    MoleculeSummaryClass(NULL)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = "MoleculeSummarySet";
  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMoleculeSummarySetClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMoleculeSummarySetClass::print(ostream& out) const
{
  DataSetOfObjectsClass::print(out);
  PointerPrint(out,"  The Molecule Summary Class: "," No Class Defined ",MoleculeSummaryClass);
  return out;
}
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSummarySetClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMoleculeSummarySetClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMoleculeSummarySetClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  //bool result = DataSetOfObjectsClass::Read(in,set);
  bool result = true;
  result = result && PointerClassRead(in,(DataObjectClass *&) MoleculeSummaryClass,
				      MECHANISM_MOLECULE_NAME,
				      set," No Molecule Summary Class ");
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSummarySetClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMoleculeSummarySetClass::CopyClone(Identify *  objc)
{
  RxnMoleculeSummarySetClass *objcfull = (RxnMoleculeSummarySetClass *) objc;
  *this = *objcfull;
  MoleculeSummaryClass = (RxnMoleculeSummaryClass *) PointerClone(objcfull->MoleculeSummaryClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSummarySetClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMoleculeSummarySetClass::Clone()
{
  RxnMoleculeSummarySetClass* id = new RxnMoleculeSummarySetClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeSummarySetClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeSummarySetClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,MoleculeSummaryClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeSummarySetClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeSummarySetClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) MoleculeSummaryClass);
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
BaseDataObject * RxnMoleculeSummarySetClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMoleculeSummarySet();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMoleculeSummarySetClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMoleculeSummarySetClass*& obj)
     {
     obj = new RxnMoleculeSummarySetClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMoleculeSummarySet
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMoleculeSummarySet*& obj)
     {
     obj = new RxnDataMoleculeSummarySet;
     return obj->DecodeThis(buffer);
     }
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnMoleculeSummarySetClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}
/*F sumclass = getMoleculeSummaryClass()  . . . .  RxnMoleculeSummarySetClass
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnMoleculeSummaryClass *RxnMoleculeSummarySetClass::getMoleculeSummaryClass()
{
  return MoleculeSummaryClass;
}

/*S RxnDataReactionSummary
 */ 
/*F RxnDataReactionSummary()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionSummary::RxnDataReactionSummary()
{
  Identification = MECHANISM_SUMMARY_ID;
  NameTag = MECHANISM_SUMMARY_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataReactionSummary(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataReactionSummary::RxnDataReactionSummary(const RxnDataReactionSummary& data)
  : BaseDataObject(data),
    ReactionName(data.ReactionName),
    ForwardRate(data.ForwardRate),
    ReverseRate(data.ReverseRate)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataReactionSummary
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataReactionSummary::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataReactionSummary
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataReactionSummary::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataObject::Read(in,objc,name);
  StreamObjectInput str(in,' ');
  ReactionName = name;
  ForwardRate = str.ReadNext();
  ReverseRate = str.ReadNext();
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataReactionSummary
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataReactionSummary::print(ostream& out) const
{
  BaseDataObject::print(out);
  out << "Reaction: '" << ReactionName << "' F(" << ForwardRate << "), B(" << ReverseRate << ")" << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionSummary
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataReactionSummary::Clone()
{
  RxnDataReactionSummary *obj = new RxnDataReactionSummary(*this);
  return obj;
}
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionSummary
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataReactionSummary::CopyClone(Identify * obj)
{
  RxnDataReactionSummary *objfull = (RxnDataReactionSummary *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionSummary
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionSummary::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::EncodeThis(buffer);
  result = result && Encode(buffer,ReactionName);
  result = result && Encode(buffer,ForwardRate);
  result = result && Encode(buffer,ReverseRate);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionSummary
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionSummary::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::DecodeThis(buffer);
  result = result && Decode(buffer,ReactionName);
  result = result && Decode(buffer,ForwardRate);
  result = result && Decode(buffer,ReverseRate);
  return result;
}
 
/*F name = getReactionName()    
**
**  DESCRIPTION
**    name: The reaction name
**
**  REMARKS
**
*/
String& RxnDataReactionSummary::getReactionName()
{
  return ReactionName;
}
 
/*F forward = getForwardRate()  . . . . . . . . . . . . . . . name of forward
**
**  DESCRIPTION
**    forward: The name of the forward reaction
**
**  REMARKS
**
*/
String& RxnDataReactionSummary::getForwardRate()
{
  return ForwardRate;
} 
/*F reverse = getReverseRate()  . . . . . . . . . . . . . .  get reverse name
**
**  DESCRIPTION
**    reverse: Te name of the reverse
**
**  REMARKS
**
*/
String& RxnDataReactionSummary::getReverseRate()
{
  return ReverseRate;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionSummary::RxnDataReactionSummary(const String& rxn,
					       const String& forward,
					       const String& reverse)
{
  Identification = MECHANISM_SUMMARY_ID;
  NameTag = MECHANISM_SUMMARY_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;

  NameTag = rxn;
  ReactionName = name;
  ForwardRate  = forward;
  ReverseRate  = reverse;
}
 
/*F setReactionName(name)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataReactionSummary::setReactionName(const String& name)
{
  ReactionName = name;
}
 
/*F setForwardRate(name)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataReactionSummary::setForwardRate(const String& name)
{
  ForwardRate = name;
}
 
/*F setReverseRate(name)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataReactionSummary::setReverseRate(const String& name)
{
  ReverseRate = name;
}

/*S RxnReactionSummaryClass
 */
/*F RxnReactionSummaryClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionSummaryClass::RxnReactionSummaryClass()
{
  Identification = MECHANISM_SUMMARY_ID;
  NameTag = MECHANISM_SUMMARY_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
} 
/*F RxnReactionSummaryClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnReactionSummaryClass::RxnReactionSummaryClass(const RxnReactionSummaryClass& data)
  : DataObjectClass(data)
{
} 
 
/*F RxnReactionSummaryClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnReactionSummaryClass::RxnReactionSummaryClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataObjectClass(id,name,descr)
{
  SubClass = "Object";
  EncodeDecodeClass = MECHANISM_SUMMARY_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnReactionSummaryClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnReactionSummaryClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  return out;
}
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnReactionSummaryClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnReactionSummaryClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnReactionSummaryClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataObjectClass::Read(in,set);
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnReactionSummaryClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnReactionSummaryClass::CopyClone(Identify *  objc)
{
  RxnReactionSummaryClass *objcfull = (RxnReactionSummaryClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnReactionSummaryClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnReactionSummaryClass::Clone()
{
  RxnReactionSummaryClass* id = new RxnReactionSummaryClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionSummaryClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionSummaryClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionSummaryClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionSummaryClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
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
BaseDataObject * RxnReactionSummaryClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataReactionSummary();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnReactionSummaryClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnReactionSummaryClass*& obj)
     {
     obj = new RxnReactionSummaryClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataReactionSummary
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataReactionSummary*& obj)
     {
     obj = new RxnDataReactionSummary;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataMechanism
 */ 
/*F RxnDataMechanism()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMechanism::RxnDataMechanism()
{
  Identification = MECHANISM_MECHANISM_ID;
  NameTag = MECHANISM_MECHANISM_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
  MoleculeNames.NameTag = "MoleculeNames";
  ReactionNames.NameTag = "ReactionNames";
} 
/*F RxnDataMechanism(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMechanism::RxnDataMechanism(const RxnDataMechanism& data)
  : BaseDataSetOfObjects(data),
    MoleculeNames(data.MoleculeNames),
    ReactionNames(data.ReactionNames),
    MoleculeSummaries(data.MoleculeSummaries),
    ReactionSummaries(data.ReactionSummaries),
    Thermodynamics(data.Thermodynamics)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMechanism::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMechanism::Read(istream& in, DataObjectClass* objc, const String& name)
{
  RxnMechanismClass *mechclass = (RxnMechanismClass *) objc;
  RxnReactionSummaryClass *summclass = mechclass->getSummaryClass();
  bool result = BaseDataSetOfObjects::Read(in,objc,name);
  StreamObjectInput str(in,' ');
  DataKeyWordsClass keywordclass;
  if(!(name == "END")) {
      ReadInMoleculeInformation(in,mechclass);
      String rxn = str.ReadNext();
      while(!(rxn == "END"))
	{
	  cout << "Read Reaction Summary: '" << rxn << "'" << endl;
	  RxnDataReactionSummary *summary = (RxnDataReactionSummary *) summclass->BaseDataObjectExample();

	  String forward = str.ReadNext();
	  String reverse = str.ReadNext();
	  addToMechanism(rxn,forward,reverse,mechclass);
	  addReaction(rxn);
	  delete summary;
	  rxn = str.ReadNext();

	  rxn.EliminateLeadingBlanks();
	  cout << "Reaction: '" << rxn << "'" << endl;
	}
    }
  else
    result = false;
  return result;
}
 
/*F ans = ReadInMoleculeInformation(in) . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    in: The input stream
**    ans: true if successfull
**
**  REMARKS
**
*/
bool RxnDataMechanism::ReadInMoleculeInformation(istream& in, RxnMechanismClass *mechclass)
{
  bool result = true;
  StreamObjectInput str(in,' ');
  RxnMoleculeSummaryClass *summary = mechclass->getMoleculeSummaryClass();

  String molecule = str.ReadNext();
  while(!(molecule == "END"))
    {
      RxnDataMoleculeSummary *molsum = (RxnDataMoleculeSummary *) summary->BaseDataObjectExample();
      molsum->NameTag = molecule;
      molsum->Read(in,summary,molecule);
      MoleculeNames.AddKeyWord(molecule);
      MoleculeSummaries.AddObject(molsum);
      delete molsum;
      molecule = str.ReadNext();
    }
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMechanism::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  out << "List of Molecules:" << endl;
  MoleculeNames.print(out);
  out << endl << "List of Reactions:" << endl;
  ReactionNames.print(out);
  out << "The Molecule Summaries:" << endl;
  MoleculeSummaries.print(out);
  out << endl;
  out << "The Reaction Summaries:" << endl;
  ReactionSummaries.print(out);
  out << endl;
  out << "Thermodynamics:" << endl;
  Thermodynamics.print(out);
  out << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanism
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMechanism::Clone()
{
  RxnDataMechanism *obj = new RxnDataMechanism(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanism
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMechanism::CopyClone(Identify * obj)
{
  RxnDataMechanism *objfull = (RxnDataMechanism *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanism::EncodeThis(CommBuffer& buffer)
{
  bool result =  BaseDataSetOfObjects::EncodeThis(buffer);
  result = result &&     MoleculeNames.EncodeThis(buffer);
  result = result &&     ReactionNames.EncodeThis(buffer);
  result = result && MoleculeSummaries.EncodeThis(buffer);
  result = result && ReactionSummaries.EncodeThis(buffer);
  result = result && Thermodynamics.EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanism::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  result = result && MoleculeNames.DecodeThis(buffer);
  result = result && ReactionNames.DecodeThis(buffer);
  result = result && MoleculeSummaries.DecodeThis(buffer);
  result = result && ReactionSummaries.DecodeThis(buffer);
  result = result && Thermodynamics.DecodeThis(buffer);
  return result;
}
/*F ans = addToMechanism(name,forward,reverse,mechclass)  .  RxnDataMechanism
**
**  DESCRIPTION
**    name: The name of the reaction
**    forward: The name of the forward rate constant
**    reverse: The name of the reverse rate constant
**    mechclass: The mechanism class
**
**  REMARKS
**
*/
bool RxnDataMechanism::addToMechanism(const String& name,
				      const String& forward,
				      const String& reverse,
				      RxnMechanismClass *mechclass)
{
  bool result = true;
  
  RxnReactionSummaryClass *summaryclass = mechclass->getSummaryClass();
  RxnDataReactionSummary *summary = (RxnDataReactionSummary *) summaryclass->BaseDataObjectExample();
  summary->setReactionName(name);
  summary->setForwardRate(forward);
  summary->setReverseRate(reverse);
  summary->NameTag = name;

  result = result && addReactionSummary(summary);
  delete summary;
  return result;
}
/*F ans = addMolecule(name) . . . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    name: Adds the molecule name to the list of molecule names
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataMechanism::addMolecule(String& name)
{
  MoleculeNames.AddKeyWord(name);
  return true;
}
/*F ans = addMolecule(molecule) . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    mol: A molecule specific to the mechanism
**    ans: true if successful
**
**    The molecule is added to the list of special molecules to the mechanism and the name is registered
**    in the molecule names
**
**  REMARKS
**
*/
bool RxnDataMechanism::addMolecule(RxnDataSimpleMolecule *molecule)
{
  MoleculeNames.AddKeyWord(molecule->NameTag);
  return AddObject(molecule);
}
/*F ans = addReaction(name) . . . . . . . . . . . . . . . .  RxnDataMechanism
** 
**  DESCRIPTION
**    name: The name of the reaction of the mechanism
**    ans: true if successful
**  
**  REMARKS
**
*/
bool RxnDataMechanism::addReaction(String& name)
{
  return ReactionNames.AddKeyWord(name);
}
 
/*F ans = addReaction(reaction) . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    reaction: The reaction special to this mechanism
**    ans: true if successfull
**
**    The reaction is added the list of special reactions to the mechanism and the
**    name is registered in the set of reaction names
**
**  REMARKS
**
*/
bool RxnDataMechanism::addReaction(RxnDataReaction *reaction)
{
  bool result = ReactionNames.AddKeyWord(reaction->NameTag);
  result = result && AddObject(reaction);
  return result;
}
/*F addReactionSummary(summary) . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    summary: The reaction summary to add
**    ans: true if successful
**
**    This adds the reaction summary to the mechanism and registers the 
**    reaction name (the NameTag of the summary);
**
**  REMARKS
**
*/
bool RxnDataMechanism::addReactionSummary(RxnDataReactionSummary *summary)
{
  bool result = ReactionSummaries.AddObject(summary);
  return result;
}
/*F ans = addMoleculeSummary(summary) . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    summary: the molecule summary to add
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataMechanism::addMoleculeSummary(RxnDataMoleculeSummary *summary)
{
  bool result = true;
  if(!MoleculeSummaries.IsInList(summary->NameTag))
     result = MoleculeSummaries.AddObject(summary);
  return result;
} 
/*F ans = AddThermodynamic(molecule, thermo)  . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    molecule: The name of the molecule of this thermo value
**    thermo: The thermodynamic value
**    ans: true if successfull
**
**  REMARKS
**
*/
bool RxnDataMechanism::AddThermodynamic(String &molecule, RxnDataThermoProperty *thermo)
{
  String name = thermo->NameTag;
  thermo->NameTag = molecule;
  bool result = Thermodynamics.AddObject(thermo);
  thermo->NameTag = name;
  return result;
}
 
/*F thermo = getThermoValueForMolecule(molecule)  . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    molecule: The name of the molecule of the thermo quantity
**    thermo: The thermo value (NULL if not in the list)
**
**  REMARKS
**
*/
RxnDataThermoProperty *RxnDataMechanism::getThermoValueForMolecule(String& molname,
								   RxnSimpleMoleculeClass *molclass,
								   BaseDataSetOfInstances *instances,
								   DataSetOfInstancesClass *instancesclass)
{
    RxnDataThermoProperty *thermo = NULL;
    if(Thermodynamics.IsInList(molname)) {
      thermo = (RxnDataThermoProperty *) Thermodynamics.GetObject(molname);
    } else {
      RxnDataSimpleMolecule *molecule = getMolecule(molname,molclass,instances,instancesclass);
      if(molecule != NULL) {
	thermo = getThermoValueForMolecule(molname,molecule);
      }
    }
    return thermo;
}
/*F thermo = getThermoValueForMolecule(molecule)  . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    molecule: The name of the molecule of the thermo quantity
**    thermo: The thermo value (NULL if not in the list)
**
**  REMARKS
**
*/
RxnDataThermoProperty *RxnDataMechanism::getThermoValueForMolecule(String& name, RxnDataSimpleMolecule *molecule)
{
  RxnDataThermoProperty *thermo = NULL;
  if(molecule != NULL) {
    if(MoleculeSummaries.IsInList(name)) {
      RxnDataMoleculeSummary *molsum = (RxnDataMoleculeSummary *) MoleculeSummaries.GetObject(name);
      thermo = (RxnDataThermoProperty *) molecule->RetrieveProperty(molsum->ThermodynamicInfo);
      if(thermo == NULL) {
	cerr << "Thermodynamic Quanitity: '" << molsum->ThermodynamicInfo;
	cerr << "' not found in '" << molecule->NameTag << "' (" << name << ")" << endl;
      }
    } else {
      cerr << "Molecule Summary: '" << name << "' not found in mechanism" << endl;
    }
  }
  return thermo;
}
/*F mol = getMolecule(name,instances,instancesclass)  . . .  RxnDataMechanism
**
**  DESCRIPTION
**    name: The name of the molecule to find
**    instances: The set of instances (where the molecule could be)
**    instancesclass: The class of the instances
**    mol: The molecule (NULL if not found)
**
**    This routine looks first in the mechanism to find the molecule, i.e.
**    it is a special molecule to the mechanism.  If not found, it
**    looks in the set of instances
**
**  REMARKS
**
*/
RxnDataSimpleMolecule *RxnDataMechanism::getMolecule(String& name,
						     RxnSimpleMoleculeClass *molclass,
						     BaseDataSetOfInstances *instances,
						     DataSetOfInstancesClass *instancesclass)
{
    RxnDataSimpleMolecule *molecule = NULL;
    if(MoleculeNames.KeyWordInList(name)) {
	    if(IsInList(name))
		molecule = (RxnDataSimpleMolecule *) GetObject(name);
	    else if(instances->InstanceInSet(name)) {
		    BaseDataInstance *instance = instances->GetInstance(name);
		    if(instance->IsInList(molclass->GetNameInInstance())) {
		      //cout << "Get Molecule Instance: '" << name << "' and molecule info '" << molclass->GetNameInInstance() << "'" << endl;
			molecule = (RxnDataSimpleMolecule *) instance->GetObject(molclass->GetNameInInstance());
		    }
		} else {
		    cerr << "getMolecule: '" << name << "' not found" << endl;
		}
	} else {
	    cerr << "getMolecule: '" << name << "' not in list of Molecule names" << endl;
	    cerr << endl;
	}
    return molecule;
}
/*F rxn = getReaction(name,instances,instancesclass)  . . .  RxnDataMechanism
**
**  DESCRIPTION
**    name: The name of the reaction to find
**    instances: The set of instances (where the reaction could be)
**    instancesclass: The class of the instances
**    rxn: The reaction (NULL if not found)
**
**    This routine looks first in the mechanism to find the reaction, i.e.
**    it is a special reaction to the mechanism.  If not found, it
**    looks in the set of instances
**
**  REMARKS
**
*/
RxnDataReaction *RxnDataMechanism::getReaction(String& name,
					       RxnReactionClass *rxnclass,
					       BaseDataSetOfInstances *instances,
					       DataSetOfInstancesClass *instancesclass)
{
  RxnDataReaction *reaction = NULL;
  if(ReactionNames.KeyWordInList(name)) {
    if(IsInList(name))
      reaction = (RxnDataReaction *) GetObject(name);
    else if(instances->InstanceInSet(name)) {
      BaseDataInstance *instance = instances->GetInstance(name);
      if(instance->IsInList(rxnclass->getNameInInstance()))
	reaction = (RxnDataReaction *) instance->GetObject(rxnclass->getNameInInstance());
    } else {
      cerr << "Reaction not found in either Mechanism or instances: '" << name << "'" << endl;
    }
  } else {
    cerr << "Reaction: '" << name << "' not found in Reaction List" << endl;
  }
  return reaction;
}
 
/*F names = getMoleculeNames()  . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    names: molecules names
**
**  REMARKS
**
*/
BaseDataKeyWords& RxnDataMechanism::getMoleculeNames()
{
  return MoleculeNames;
}
/*F names = getReactionNames()  . . . . . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    names: reaction names
**
**  REMARKS
**
*/
BaseDataKeyWords& RxnDataMechanism::getReactionNames()
{
  return ReactionNames;
}
 
/*F summary = getReactionSummary(name)  . . . . . . . . . .  RxnDataMechanism
**
**  DESCRIPTION
**    name: The name of the reaction summary
**    summary: The reaction summary
**
**  REMARKS
**
*/
RxnDataReactionSummary *RxnDataMechanism::getReactionSummary(String& name)
{
  RxnDataReactionSummary *summary = NULL;
  if(ReactionSummaries.IsInList(name))
    {
      summary = (RxnDataReactionSummary *) ReactionSummaries.GetObject(name);
    }
  else
    {
      cerr << "ReactionSummary '" << name << "' not found" << endl;
    }
  return summary;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeSummary *RxnDataMechanism::getMoleculeSummary(String& name)
{
  RxnDataMoleculeSummary *summary = NULL;
  if(MoleculeSummaries.IsInList(name))
    {
      summary = (RxnDataMoleculeSummary *) MoleculeSummaries.GetObject(name);
    }
  else
    {
      cerr << "MoleculeSummary '" << name << "' not found" << endl;
    }
  return summary;  
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataMechanism::setUpMoleculeSummaries(BaseDataSetOfObjects *molsum)
{
  
  MoleculeSummaries.MergeSetOfObjects(molsum);
}
/*F ans = equalTo(key1,key2)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool sameKeySet(BaseDataKeyWords *key1, BaseDataKeyWords *key2) {
  bool ans = true;
  if(key1->SizeOf() == key2->SizeOf()) {
    ObjectListString& k1 = key1->GetKeyWords();
    ObjectListString& k2 = key2->GetKeyWords();
    k1.Sort();
    k2.Sort();
    ObjectList<String>::iterator k1name = k1.begin();
    ObjectList<String>::iterator k2name = k2.begin();
    while(k1name != k1.end() && ans) {
      ans = *k1name == *k2name; 
      k1name++;
      k2name++;
    }
  } else 
    ans = false;
  return ans;
}
/*F equalTo(rxn)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool sameReactantsAndProducts(RxnDataReaction *rxn1, RxnDataReaction *rxn2) {
  bool ans = true;
  if(sameKeySet(&(rxn1->getReactantNames()),&(rxn2->getReactantNames()))) {
    ans = sameKeySet(&(rxn1->getProductNames()),&(rxn2->getProductNames()));
  } else {
    ans = false;
  }
  return ans;
}
/*F equalTo(rxn)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataKeyWords *RxnDataMechanism::IdentifyIdenticalReactions(RxnReactionClass *rxnclass,
						  BaseDataSetOfInstances *instances,
						  DataSetOfInstancesClass *instancesclass) {
  BaseDataKeyWords *duplicates = new BaseDataKeyWords();
  BaseDataKeyWords *names = (BaseDataKeyWords *) getReactionNames().Clone();
  while(names->SizeOf() > 0) {
    String rxnname = names->NextKey();
    RxnDataReaction *rxn = getReaction(rxnname,
				       rxnclass,instances,instancesclass);
    BaseDataKeyWords *rest = (BaseDataKeyWords *) names->Clone();
    while(rest->SizeOf()) {
      String compname = rest->NextKey();
      RxnDataReaction *rxncomp = getReaction(compname,
					     rxnclass,instances,instancesclass);
      if(sameReactantsAndProducts(rxn,rxncomp)) {
	duplicates->AddKeyWord(rxnname);
	duplicates->AddKeyWord(compname);
	cout << "Identical: " << rxnname << " and " << compname << endl;
      }
    }
  }
  return duplicates;
}
/*S RxnMechanismClass
 */
/*F RxnMechanismClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMechanismClass::RxnMechanismClass()
  : NameInInstance("Mechanism"),
    SummaryClass(NULL),
    MoleculeClass(NULL),
    ReactionClass(NULL),
    MoleculeSummaryClass(NULL)
{
  Identification = MECHANISM_MECHANISM_ID;
  NameTag = MECHANISM_MECHANISM_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnMechanismClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMechanismClass::RxnMechanismClass(const RxnMechanismClass& data)
  : DataSetOfObjectsClass(data),
    NameInInstance(data.NameInInstance),
    MoleculeData(data.MoleculeData),
    MolDatVariables(data.MolDatVariables)
{
  SummaryClass = (RxnReactionSummaryClass *) PointerClone(data.SummaryClass);
  MoleculeSummaryClass = (RxnMoleculeSummaryClass *) PointerClone(data.MoleculeSummaryClass);
  MoleculeClass = (RxnSimpleMoleculeClass *) PointerClone(data.MoleculeClass);
  ReactionClass = (RxnReactionClass *) PointerClone(data.ReactionClass);
} 
 
/*F RxnMechanismClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMechanismClass::RxnMechanismClass(const int id, 
				     const String& name,
				     const String& descr)
  : DataSetOfObjectsClass(id,name,descr),
    NameInInstance("Mechanism"),
    SummaryClass(NULL),
    MoleculeClass(NULL),
    ReactionClass(NULL),
    MoleculeSummaryClass(NULL)
    
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = MECHANISM_MECHANISM_NAME;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMechanismClass::print(ostream& out) const
{
  DataSetOfObjectsClass::print(out);
  out << "Name in Instance: '" << NameInInstance << "'" << endl;
  PointerPrint(out,"  The Molecule Summary Class: "," No Class Defined ",MoleculeSummaryClass);
  PointerPrint(out,"  The Reaction Summary Class: "," No Class Defined ",SummaryClass);
  PointerPrint(out,"  The Molecule Class: "," No Class Defined ",MoleculeClass);
  PointerPrint(out,"  The Reaction Class: "," No Class Defined ",ReactionClass);
  MolDatVariables.print(out);
  out << endl;
  return out;
}
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMechanismClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMechanismClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = true;
  //DataSetOfObjectsClass::Read(in,set);
  String keyw("NameInInstance:");
  if(result && CheckReadKeyWord(in,keyw))
    {
      StreamObjectInput str(in,' ');
      NameInInstance = str.ReadNext();
    }
  else
    result = false;
  result = result && PointerClassRead(in,(DataObjectClass *&) MoleculeSummaryClass,
				      MECHANISM_MOLECULE_NAME,
				      set," No Molecule Summary Class ");
  result = result && PointerClassRead(in,(DataObjectClass *&) SummaryClass,
				      MECHANISM_SUMMARY_NAME,
				      set," No Reaction Summary Class ");
  result = result && PointerClassRead(in,(DataObjectClass *&) MoleculeClass,
				      MOLECULE_SIMPLE_NAME,
				      set," No Molecule Class ");
  result = result && PointerClassRead(in,(DataObjectClass *&) ReactionClass,
				      REACTION_REACTION_NAME,
				      set," No Reaction Class ");
  DataKeySetClass keyclass;
  ReadMolDat(in);
  return result;
} 

/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMechanismClass::CopyClone(Identify *  objc)
{
  RxnMechanismClass *objcfull = (RxnMechanismClass *) objc;
  *this = *objcfull;
  MoleculeSummaryClass = (RxnMoleculeSummaryClass *) PointerClone(objcfull->MoleculeSummaryClass);
  SummaryClass = (RxnReactionSummaryClass *) PointerClone(objcfull->SummaryClass);
  MoleculeClass = (RxnSimpleMoleculeClass *) PointerClone(objcfull->MoleculeClass);
  ReactionClass = (RxnReactionClass *) PointerClone(objcfull->ReactionClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMechanismClass::Clone()
    {
      RxnMechanismClass* id = new RxnMechanismClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::EncodeThis(buffer);
  result = result && Encode(buffer,NameInInstance);
  result = result && PointerEncode(buffer,SummaryClass);
  result = result && PointerEncode(buffer,MoleculeSummaryClass);
  result = result && PointerEncode(buffer,MoleculeClass);
  result = result && PointerEncode(buffer,ReactionClass);
  result = result && MolDatVariables.EncodeThis(buffer);
  result = result && MoleculeData.EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::DecodeThis(buffer);
  result = result && Decode(buffer,NameInInstance);
  result = result && PointerDecode(buffer,(BaseDataObject *&) SummaryClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) MoleculeSummaryClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) MoleculeClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) ReactionClass);
  result = result && MolDatVariables.DecodeThis(buffer);
  result = result && MoleculeData.DecodeThis(buffer);
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
BaseDataObject * RxnMechanismClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMechanism();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMechanismClass*& obj)
{
  obj = new RxnMechanismClass;
  return obj->DecodeThis(buffer);
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMechanism
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMechanism*& obj)
{
  obj = new RxnDataMechanism;
  return obj->DecodeThis(buffer);
} 
/*F summ = getSummaryClass() Get the default summary class
**
**  DESCRIPTION
**    summ: The summary class
**
**  REMARKS
**
*/
RxnReactionSummaryClass *RxnMechanismClass::getSummaryClass(void)
{
  return SummaryClass;
}
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnMechanismClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}
 
/*F name = getNameInInstance()  . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    name: The name of the mechanism within an instance
**
**  REMARKS
**
*/
String RxnMechanismClass::getNameInInstance()
{
  return NameInInstance;
}
 
/*F getReactionClass()  . . . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**   
**  REMARKS
**
*/
RxnReactionClass *RxnMechanismClass::getReactionClass()
{
  return ReactionClass;
} 
/*F cls = getMoleculeSummaryClass() . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    cls: The molecule summary class
**
**  REMARKS
**
*/
RxnMoleculeSummaryClass *RxnMechanismClass::getMoleculeSummaryClass()
{
  return MoleculeSummaryClass;
}
 
/*F molclass = getMoleculeClass() . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    molclass: The standard molecule of the mechanism
**
**  REMARKS
**
*/
RxnSimpleMoleculeClass *RxnMechanismClass::getMoleculeClass()
{
  return MoleculeClass;
}
/*F ans = ReadMolDat(in)  . . . . . . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    in: The input buffer
**
**  REMARKS
**
*/
bool RxnMechanismClass::ReadMolDat(istream& in)
{
  bool result = true;
  StreamObjectInput str(in,' ');
  String name = str.ReadNext();
  while(result && !(name == "END"))
    {
      MolDatVariables.AddKeyWord(name);
      String classname = str.ReadNext();
      DataObjectClass *objclass = PointerToAllowedClasses()->GetObjectClass(classname);
      BaseDataObject *obj = objclass->BaseDataObjectExample();
      obj->NameTag = name;
      result = result && obj->Read(in,objclass,name);
      MoleculeData.AddObject(obj);
      delete obj;
      name = str.ReadNext();
  }
  return result;
}
/*F obj = MolDatVariable(name)  . . . . . . . . . . . . . . . . . . . RxnMechanismClass
**
**  DESCRIPTION
**    name: 
**
**  REMARKS
**
*/
BaseDataObject *RxnMechanismClass::GetMolDat(String& name)
{
  return MoleculeData.GetObject(name);
}

/*S RxnDataPrintOutMechanism
 */ 
/*F RxnDataPrintOutMechanism()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataPrintOutMechanism::RxnDataPrintOutMechanism()
  : PrintParametersS("PrintParameters"),
    SubMoleculesS("SubMolecules")
{
  Identification = MECHANISM_PRINT_ID;
  NameTag = MECHANISM_PRINT_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataPrintOutMechanism(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataPrintOutMechanism::RxnDataPrintOutMechanism(const RxnDataPrintOutMechanism& data)
  : BaseDataAlgorithmOperation(data),
    PrintParametersS(data.PrintParametersS),
    SubMoleculesS(data.SubMoleculesS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataPrintOutMechanism::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Parameters: '" << PrintParametersS << "'" << endl;
  out << "SubMolecules: '" << SubMoleculesS << "'" << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataPrintOutMechanism::Clone()
{
  RxnDataPrintOutMechanism *obj = new RxnDataPrintOutMechanism(*this);
  return obj;
}
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataPrintOutMechanism::CopyClone(Identify * obj)
{
  RxnDataPrintOutMechanism *objfull = (RxnDataPrintOutMechanism *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,PrintParametersS);
  result = result && Encode(buffer,SubMoleculesS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,PrintParametersS);
  result = result && Decode(buffer,SubMoleculesS);
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
bool RxnDataPrintOutMechanism::SetUpAlgorithms(BaseDataSetOfInstances*,
					       DataSetOfInstancesClass*,
					       BaseDataAlgorithmRun* ,
					       DataAlgorithmRunClass*)
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
bool RxnDataPrintOutMechanism::CheckInput(BaseDataSetOfInstances *instances,
					  DataSetOfInstancesClass *instancesclass,
					  BaseDataAlgorithmRun *run,
					  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  if(run->ParameterInList(PrintParametersS))
    {
      if(run->ParameterInList(SubMoleculesS))
	{
	}
      else
	{
	  cerr << "The submechanism molecule list, '" << SubMoleculesS << "', was not in the list of parameters";
	  result = false;
	}
    }
  else
    {
      cerr << "The print parameters '" << PrintParametersS << "' was not in the list of parameters";
      result = false;
    }
  
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
bool RxnDataPrintOutMechanism::SetUpInput(BaseDataSetOfInstances* instances,
					  DataSetOfInstancesClass* instancesclass,
					  BaseDataAlgorithmRun *run,
					  DataAlgorithmRunClass* rclass)
{
  bool result = true;
  oneFile = true;
  SubMolecules = (BaseDataKeyWords *) run->ParameterValue(SubMoleculesS)->Clone();
  BaseDataKeyWords *PrintParameters = (BaseDataKeyWords *) run->ParameterValue(PrintParametersS)->Clone();
  if(PrintParameters->SizeOf() < 7)
    {
      cerr << "Expecting 7 parameters in '" << PrintParameters->NameTag << "'" << endl;
      cerr << "  Mechanism:          The mechanism" << endl;
      cerr << "  MechanismClass:     The mechanism class" << endl;
      cerr << "  RxnProperty:        The reaction property" << endl;
      cerr << "  CpUnits:            The heat capacity units" << endl;
      cerr << "  EnthalpyUnits:      The enthalpy property units" << endl;
      cerr << "  EntropyUnits:       The entropy property units" << endl;
      cerr << "  Filename:           The file name" << endl;

      result = false;
    }
  else
    {
      MechanismS        = PrintParameters->NextKey();
      MechanismClassS   = PrintParameters->NextKey();
      Property          = PrintParameters->NextKey();
      CpProperty        = PrintParameters->NextKey();
      EnthalpyProperty  = PrintParameters->NextKey();
      EntropyProperty   = PrintParameters->NextKey();
      FileNameS         = PrintParameters->NextKey();

      cout << "Mechanism:          " << MechanismS << endl;
      cout << "Reaction Property:  " << Property << endl;
      cout << "Enthalpy Property:  " << EnthalpyProperty << endl;
      cout << "Entropy Property:   " << EntropyProperty << endl;
      cout << "Cp Property:        " << CpProperty << endl;
      cout << "FileName            " << FileNameS << endl;

      delete PrintParameters;
      if(instances->InstanceInSet(MechanismS))
	{
	  if(instancesclass->IsInList(MechanismClassS))
	    {
	      MechanismClass = (RxnMechanismClass *) instancesclass->GetObjectClass(MechanismClassS);
	      BaseDataInstance *instance = (BaseDataInstance *) instances->GetInstance(MechanismS);
	      if(instance->IsInList(MechanismClass->getNameInInstance()))
		{
		  Mechanism = (RxnDataMechanism *) instance->GetObject(MechanismClass->getNameInInstance());
		  cout << "Mechanism in Instances: '" << MechanismClass->getNameInInstance() << "'" << endl;
		  cout << "Open File: '" << FileNameS << "'" << endl;
		  if(run->AlgorithmSummary.KeyWordInList("LaTeX"))
		    {
		      Output = new OpenOutputFile(FileNameS,"tex");
		      OutputThm = Output;
		      OutputCorr = Output;
		    }
		  else if(run->AlgorithmSummary.KeyWordInList("Standard"))
		    {
		      if(oneFile) {
			Output = new OpenOutputFile(FileNameS,"ckm");
			OutputThm = Output;
			SpeciesList = Output;
			OutputBase = new OpenOutputFile(FileNameS,"base");
			OutputCorr = new OpenOutputFile(FileNameS,"corr");
			MolDat = new OpenOutputFile(FileNameS,"moldat");
		      } else {
			Output = new OpenOutputFile(FileNameS,"mech");
			OutputBase = new OpenOutputFile(FileNameS,"base");
			OutputThm = new OpenOutputFile(FileNameS,"thm");
			OutputCorr = new OpenOutputFile(FileNameS,"corr");
			MolDat = new OpenOutputFile(FileNameS,"moldat");
			SpeciesList = new OpenOutputFile(FileNameS,"species");
		      }
		    }
		  else
		    {
		      Output = new OpenOutputFile(FileNameS,"tex");
		      OutputThm = Output;
		      OutputCorr = Output;
		      MolDat = Output;
		      SpeciesList = Output;
		    }
		}
	      else
		{
		  cerr << "Mechanism (under the name '" << MechanismClass->getNameInInstance() << "') not found in instance" << endl;
		  result = false;
		}
	    }
	  else
	    {
	      cerr << "Mechanism Class '" << MechanismClassS << "' not found in classes";
	      result = false;
	    }
	}
      else
	{
	  cerr << "Mechanism '" << MechanismS << "' not in instances" << endl;
	  result = false;
	}
      
    }
  return result;
}
/*F ans = UpdateShortName(instances,instancesclass)  . . . . 
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    
**  REMARKS
**
*/
void RxnDataPrintOutMechanism::UpdateShortName(BaseDataSetOfInstances *instances,
					       DataSetOfInstancesClass *instancesclass) {
  BaseDataKeyWords *molnames = (BaseDataKeyWords *) Mechanism->getMoleculeNames().Clone();
  while(molnames->SizeOf() > 0) {
     String molname = molnames->NextKey();
     //cout << molname << endl;
     if(SubMolecules->KeyWordInList(molname)) {
       RxnDataMoleculeSummary *molsum = Mechanism->getMoleculeSummary(molname);
       BaseDataInstance *molinstance = instances->GetInstance(molname);
       if(molinstance->IsInList(molsum->ShortName)) {
	 RxnDataSimpleMolecule *molecule = Mechanism->getMolecule(molname,
								  MechanismClass->getMoleculeClass(),
								  instances,instancesclass);
	 BaseDataString *name = (BaseDataString *) molinstance->GetObject(molsum->ShortName);
	 //cout << molname << "  ->  " << name->getString() << endl;
	 BaseDataString *chemkinname = new BaseDataString();
	 chemkinname->NameTag = molsum->ShortName;
	 chemkinname->setString(name->getString());
	 molecule->StoreProperty(chemkinname);
	 delete chemkinname;
       }
     }
  }
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
bool RxnDataPrintOutMechanism::Calculate(BaseDataSetOfInstances *instances,
  DataSetOfInstancesClass *instancesclass,
  BaseDataAlgorithmRun *run,
  DataAlgorithmRunClass *runclass) {
  bool result = true;
  UpdateShortName(instances,instancesclass);
  if(oneFile) {
    Output->Stream << "ELEMENTS" << endl;
    Output->Stream << "o h c n ar" << endl;
    Output->Stream << "END" << endl;
  }
     cout << "-------------------- PrintMoleculeInformation ---------------------" << endl;
     result = result & PrintMoleculeInformation(instances,instancesclass,run,runclass);
     cout << "-------------------- PrintThermodynamics ---------------------" << endl;
result = result & PrintThermodynamics(instances,instancesclass,run,runclass);
cout << "-------------------- PrintReactions ---------------------***" << endl;
result = result & PrintReactions(instances,instancesclass,run,runclass);
cout << "-------------------- ******************* ---------------------" << endl;

return result;
}

/*F ans = PrintMoleculeInformation(instances,instancesclass,run,runclass)  . . . . algorithm
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
bool RxnDataPrintOutMechanism::PrintMoleculeInformation(BaseDataSetOfInstances *instances,
							DataSetOfInstancesClass *instancesclass,
							BaseDataAlgorithmRun *run,
							DataAlgorithmRunClass *runclass)
{
  bool result = true;

  BaseDataKeyWords *molnames = (BaseDataKeyWords *) Mechanism->getMoleculeNames().Clone();
  if(run->ParameterInList("SpeciesHeader")) {
    SpeciesList->Stream << "SPECIES" << endl;
  } else {
    SpeciesList->Stream << "SPECIES" << endl;
  }
  while(molnames->SizeOf() > 0)
    {
      String molname = molnames->NextKey();
      if(!SubMolecules->KeyWordInList(molname))
	{
	  RxnDataSimpleMolecule *molecule = Mechanism->getMolecule(molname,MechanismClass->getMoleculeClass(),
								   instances,instancesclass);
	  RxnDataMoleculeSummary *molsum = Mechanism->getMoleculeSummary(molname);
	  if(molsum != NULL) {
	      BaseDataString *shortname = (BaseDataString *) molecule->RetrieveProperty(molsum->ShortName);
	      if(shortname == NULL) {
		cerr << "In molecule: '" << molecule->NameTag << "' the shortname, '" << molsum->ShortName << "' was not found" << endl;
		cerr << ": Error in MoleculeSummary" << endl;
		PrintMolDatInformation(molname,molecule,instancesclass);
		OutputCorr->Stream << molname << " " << molname << endl;
		//SpeciesList->Stream << molname << " = \"" << molname << "\"" << endl;
		SpeciesList->Stream << molname << endl;
	      } else {
		String name = FillStringToLength(30,shortname->getString());
		PrintMolDatInformation(shortname->getString(),molecule,instancesclass);
		OutputCorr->Stream << name << " " << molname << endl;
		//SpeciesList->Stream << name << " = \"" << molname << "\"" << endl;
		SpeciesList->Stream << name << endl ;
	      }
	    }
	}
    }
  if(run->ParameterInList("SpeciesHeader"))
  {
    SpeciesList->Stream << "END_OF_SPECIES" << endl;
    if(!oneFile) {
      SpeciesList->Stream << "REACTIONS" << endl;
      SpeciesList->Stream << "ENDOFREACTIONS" << endl;
      SpeciesList->Stream << "END_OF_FILE" << endl;
    }
  } else {
    SpeciesList->Stream << "END_OF_SPECIES" << endl;
    if(!oneFile) {
      SpeciesList->Stream << "REACTIONS" << endl;
      SpeciesList->Stream << "ENDOFREACTIONS" << endl;
      SpeciesList->Stream << "END_OF_FILE" << endl;
    }
  }
  return result;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**123456789/123456789/123456789/123456789/123456789/123456789/123456789/
**aaaaaaaaaaaaaaaaiiii123456789/123456789/123456789/123456789/123456789/                      
**H                  0   145.000     2.050     0.000     0.000     0.000
**nC10H22            1         252         4.76         0         0         1
*/
bool RxnDataPrintOutMechanism::PrintMolDatInformation(String& molname,
						      RxnDataSimpleMolecule *molecule,
						      DataSetOfObjectsClass *instancesclasses)
{
  MolDat->Stream.setf(ios::fixed,ios::floatfield);
  MolDat->Stream.setf(ios::showpoint);
  MolDat->Stream.precision(3);
  bool result = true;
  BaseDataKeySet *vars = (BaseDataKeySet *) MechanismClass->MolDatVariables.Clone();
  String name = FillStringToLength(16,molname);
  MolDat->Stream << name;
  while(vars->SizeOf() > 0)
    {
      String varname = vars->NextKey();
      BaseDataObject *obj;
      obj = molecule->RetrieveProperty(varname);
      if(obj == NULL)
	{
	  obj = MechanismClass->GetMolDat(varname);
	}
      if(instancesclasses->IsOfClass(*obj,NUMERIC_INTEGER_NAME))
	{
	  BaseDataInteger *intobj = (BaseDataInteger *) obj;
	  MolDat->Stream  << setiosflags(ios::right);
	  MolDat->Stream  << setw(4) << intobj->GetValue();
	}
      else if(instancesclasses->IsOfClass(*obj,NUMERIC_BASE_NAME))
	{
	  BaseDataNumeric *numobj = (BaseDataNumeric *) obj;
	  MolDat->Stream.width(10);
	  //MolDat->Stream.setiosflags(ios::fixed | ios::showpoint | ios::right);
	  //MolDat->Stream.setf(ios::right);
	  MolDat->Stream.setf(ios::fixed);
	  MolDat->Stream.setf(ios::showpoint,ios::right);
	  MolDat->Stream.precision(3);
	  MolDat->Stream << numobj->Distance();
	  //MolDat->Stream   << setw(10) << Float2String(numobj->Distance());
	  //OutputStringInWidth(MolDat->Stream,10,Float2String(numobj->Distance()));
	}
      else
	{
	  DataObjectClass *objclass = instancesclasses->PointerToAllowedClasses()->GetObjectClass(obj->GetType());
	  MolDat->Stream << "  ";
	  obj->WriteAsASCII(MolDat->Stream,objclass);
	}

    }
  MolDat->Stream << endl;
  return result;
}
/*F ans = PrintThermodynamics(instances,instancesclass,run,runclass)  . . . . algorithm
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
bool RxnDataPrintOutMechanism::PrintThermodynamics(BaseDataSetOfInstances *instances,
						   DataSetOfInstancesClass *instancesclass,
						   BaseDataAlgorithmRun *run,
						   DataAlgorithmRunClass *runclass)
{
  bool result = true;
  VectorNumeric temps;
  temps.push_back(500.0);
  temps.push_back(700.0);
  temps.push_back(1000.0);
  temps.push_back(1500.0);
  temps.push_back(2000.0);
  if(oneFile) {
    OutputThm->Stream << "THERMO ALL" << endl;
    OutputThm->Stream << "   300.000  1000.000  5000.000" << endl;
  }
  PrintOutMechanismThermoSeries(OutputThm->Stream,temps,run,runclass,instances,instancesclass);
  if(oneFile) {
    OutputThm->Stream << "END" << endl;
  }
  return result;
}
 
/*F ans = ThermoSeriesPostfix(out,run,runclass) . .  RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::ThermoSeriesPostfix(ostream& out,
						   BaseDataAlgorithmRun* run,
						   DataAlgorithmRunClass *runclass)
{
  bool result = true;
  if(run->AlgorithmSummary.KeyWordInList("LaTeX"))
    {
      out << "\\end{tabular}" << endl;
    }  
  return result;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::ThermoSeriesPrefix(ostream& out,
						  VectorNumeric& temperatures,
						  BaseDataAlgorithmRun* run,
						  DataAlgorithmRunClass *runclass)
{
  bool result = true;

  if(run->AlgorithmSummary.KeyWordInList("LaTeX"))
    {
      out << "\\begin{tabular}{|l|c|c|c";
      for(VectorNumeric::iterator temp = temperatures.begin();
	  temp != temperatures.end();
	  temp++)
	out << "|c";
      out << "|}\\hline" << endl;
      out << "         & \\multicolumn{3}{|c|}{Standard} & \\multicolumn{";
      out << temperatures.size();
      out << "}{|c|}{Heat Capacity} \\\\ \\hline" << endl;
      out << "Molecule & Enthalpy & Entropy & Cp ";
      for(VectorNumeric::iterator temp = temperatures.begin();
	  temp != temperatures.end();
	  temp++)
	out << " & " << *temp;
      out << " \\\\\\hline" << endl;
    }
  else if(!(run->AlgorithmSummary.KeyWordInList("Standard")))
    {
      out << "Molecule       Enthalpy     Entropy         Cp     ";
      for(VectorNumeric::iterator temp = temperatures.begin();
	  temp != temperatures.end();
	  temp++)
	out << "  " << setw(10) << *temp;
      out << endl;
    }
  return result;
}
/*F ans = ThermoSeriesTable(out,molecule,temperatures,thermo,thermoclass,run,runclass)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::ThermoSeriesTable(ostream& out,
						 String& molname,
						 RxnDataSimpleMolecule *molecule,
						 VectorNumeric& temperatures,
						 RxnDataThermoProperty *thermo,
						 RxnThermoPropertyClass *thermoclass,
						 BaseDataAlgorithmRun* run,
						 DataAlgorithmRunClass *runclass,
						 bool ischemkin)
{
  bool result = true;
  bool asLatex = run->AlgorithmSummary.KeyWordInList("LaTeX");
  if(run->AlgorithmSummary.KeyWordInList("Standard")) {
    String name(molecule->NameTag);
    RxnDataMoleculeSummary *molsum = Mechanism->getMoleculeSummary(molname);
    if(molsum != NULL) {
	  BaseDataString *shortname = (BaseDataString *) molecule->RetrieveProperty(molsum->ShortName);
	  if(shortname != NULL) {
	    name = shortname->getString();
	    if(ischemkin) {
	      RxnDataChemkinThermo *chemkin = (RxnDataChemkinThermo *) thermo;
	      chemkin->WriteAsNASAPolynomial(out);
	    } else {
	      RxnDataChemkinThermo chemkin(name,
					   CpProperty,EnthalpyProperty,EntropyProperty,
					   molecule,MechanismClass->getMoleculeClass(),
					   thermo,thermoclass);
	      chemkin.WriteAsNASAPolynomial(out);
	    }
	  } else { 
	    cerr << "Short Name '" << molsum->ShortName << "' not in molecule properties" << endl; 
	    cerr << "Thermodynamics not written" << endl;
	  }
    } else {
      cerr << "Molecule Summary not found in '" << molname << "' " << endl;
    }
  } else {
    out << endl;
    out << setw(20) << molname << " ";
    if(asLatex) out << "& ";
    double enthalpy = thermo->getStandardEnthalpy(thermoclass);
    enthalpy = thermoclass->Convert(true,EnthalpyProperty,enthalpy);
    out << setw(10) << enthalpy << "  ";
    if(asLatex) out << "& ";
    double entropy = thermo->getStandardEntropy(thermoclass);
    entropy = thermoclass->Convert(true,EntropyProperty,entropy);
    out << setw(10) << entropy << "  ";
    if(asLatex) out << "& ";
    double cp = thermo->CalculateHeatCapacity(thermoclass,thermoclass->getStandardTemperature());
    out << setw(10) << cp;
    for(VectorNumeric::iterator temp = temperatures.begin();
	temp != temperatures.end();
	temp++)
      {
	if(asLatex) out << "& ";
	double cp1 = thermo->CalculateHeatCapacity(thermoclass,*temp);
	cp1 = thermoclass->Convert(true,CpProperty,cp1);
	out << setw(10) << cp1 << "  ";
      }
    if(asLatex) out << "\\\\\\hline ";
    out << endl;
  }
  return result;
}
/*F ans = PrintOutMechanismThermoSeries(out,asLatex,temperatures,mechanism,mechclass,instancesclass)
**
**  DESCRIPTION
**    out: The output stream
**    asLatex: Print as a latex table
**    temperatures: The list of temperatures to print at
**    mechanism: The mechanism
**    mechclass: The mechanism class
**    instancesclass: The instances class
**
**    This prints a (LaTeX) table of the molecule thermodynamic information of a mechanism as
**    the entropy, enthalpy and heat capacity at standard temperatures and then a set of 
**    heat capacities at the given temperatures.
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintOutMechanismThermoSeries(ostream& out,
							     VectorNumeric& temperatures,
							     BaseDataAlgorithmRun* run,
							     DataAlgorithmRunClass *runclass,
							     BaseDataSetOfInstances *instances,
							     DataSetOfInstancesClass *instancesclass)
{
  bool result = true;
  RxnSimpleMoleculeClass *molclass = (RxnSimpleMoleculeClass *) MechanismClass->getMoleculeClass();

  ThermoSeriesPrefix(out,temperatures,run,runclass);

  BaseDataKeyWords *molnames = (BaseDataKeyWords *) Mechanism->getMoleculeNames().Clone();
  while(molnames->SizeOf() > 0)
    {
      String molname = molnames->NextKey();
      if(!SubMolecules->KeyWordInList(molname)) {
	RxnDataSimpleMolecule *molecule = Mechanism->getMolecule(molname,molclass,instances,instancesclass);
	if(molecule != NULL) {
	  RxnDataThermoProperty *thermo = Mechanism->getThermoValueForMolecule(molname,molecule);
	  if(thermo != NULL) {
	    RxnThermoPropertyClass *thermoclass = (RxnThermoPropertyClass *) instancesclass->GetObjectClass(thermo->GetType());
	    bool ischemkin = instancesclass->IsOfClass(*thermo,"ChemkinThermo");
	    ThermoSeriesTable(out,molname,molecule,temperatures,thermo,thermoclass,run,runclass,ischemkin);
	  } else {
	    cerr << "Thermodynamic Value not found for '" << molname << "'" << endl;
	    result = false;
	  }      
	} else { 
	  cerr << "Molecule: '" << molname << "' not found for Thermodynamics" << endl;
	}
      }
    }
  ThermoSeriesPostfix(out,run,runclass);
  
  return result;
} 

/*F ans = (rxn,molclass,instances,instancesclass)  . . . . algorithm
**
**  DESCRIPTION
      rxn:            The reaction
      molclass:       The molecule class
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    
**  REMARKS
**
*/
void RxnDataPrintOutMechanism::FillMoleculesInReaction(RxnDataMechanism *mech,
						       RxnDataReaction *rxn,
						       RxnSimpleMoleculeClass *molclass,
						       BaseDataSetOfInstances *instances,
						       DataSetOfInstancesClass *instancesclass)
{
  BaseDataKeySet *reactantnames = (BaseDataKeySet *) rxn->getReactantNames().Clone();
  RxnDataMoleculeSet *reactants = rxn->getReactants();
  bool notlast = true;
  while(notlast)
    {
      String name = reactantnames->NextKey();
      RxnDataSimpleMolecule *mol = Mechanism->getMolecule(name,molclass,instances,instancesclass);
      if(mol != NULL) {
	mol->NameTag = name;
	reactants->AddObject(mol);
      } else {
	cerr << "Reactant '" << name << "' not found in reaction " << endl;
      }
      notlast = reactantnames->SizeOf() > 0;
    }

  BaseDataKeySet *productnames = (BaseDataKeySet *) rxn->getProductNames().Clone();
  RxnDataMoleculeSet *products = rxn->getProducts();
  notlast = true;
  while(notlast)
    {
      String name = productnames->NextKey();
      RxnDataSimpleMolecule *mol = Mechanism->getMolecule(name,molclass,instances,instancesclass);
      if(mol != NULL) {
	mol->NameTag = name;
	products->AddObject(mol);
      } else {
	cerr << "Product '" << name << "' not found in reaction, so not added " << endl;
      }
      notlast = productnames->SizeOf() > 0;
    }
}
/*F ans = BaseReaction(rxn,)  . . . . algorithm
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::BaseReaction(RxnDataReaction *rxn)
{
  bool answer = true;
  BaseDataKeySet *reactantnames = (BaseDataKeySet *) rxn->getReactantNames().Clone();
  BaseDataKeySet *productnames  = (BaseDataKeySet *) rxn->getProductNames().Clone();
  bool notlast = true;
  while(notlast && answer) {
    String name = reactantnames->NextKey();
    answer = SubMolecules->KeyWordInList(name);
    notlast = reactantnames->SizeOf() > 0;
  }
  notlast = true;
  while(notlast && answer) {
    String name = productnames->NextKey();
    answer = SubMolecules->KeyWordInList(name);
    notlast = productnames->SizeOf() > 0;

  }
  return answer;
}
					    

/*F ans = PrintReactions(instances,instancesclass,run,runclass)  . . . . algorithm
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
bool RxnDataPrintOutMechanism::PrintReactions(BaseDataSetOfInstances *instances,
					      DataSetOfInstancesClass *instancesclass,
					      BaseDataAlgorithmRun *run,
					      DataAlgorithmRunClass *runclass)
{
  bool result = true;
  RxnReactionClass *rxnclass = (RxnReactionClass *) MechanismClass->getReactionClass();
  cout << "Begin: -------------------- Check Identity ---------------------" << endl;
  BaseDataKeyWords *duplicates = Mechanism->IdentifyIdenticalReactions(rxnclass,instances,instancesclass);
  cout << "End: -------------------- Check Identity ---------------------" << endl;

  bool lundformat = false;
  if(run->AlgorithmSummary.KeyWordInList("LundFormat"))
    lundformat = true;
  ObjectList<String> names = Mechanism->getReactionNames().GetKeyWords();
  ObjectList<String>::iterator name;
  DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
  result = result && PrintMechanismPrefix(Output->Stream,run,runclass);
  for(name = names.begin(); result && name != names.end(); name++) {
      RxnDataReactionSummary *summary = (RxnDataReactionSummary *) Mechanism->getReactionSummary(*name);
      if(summary != NULL) {
	RxnDataReaction *rxn = Mechanism->getReaction(summary->getReactionName(),
						      rxnclass,
						      instances,instancesclass);
	if(rxn != NULL) {

	  FillMoleculesInReaction(Mechanism,rxn,MechanismClass->getMoleculeClass(),instances,instancesclass);
	  if(rxn->IsInList(summary->getForwardRate())) {
	    RxnDataReactionRates *forward = (RxnDataReactionRates *) rxn->GetObject(summary->getForwardRate());
	    if(rxn->IsInList(summary->getReverseRate())) {
	      RxnDataReactionRates *reverse = (RxnDataReactionRates *) rxn->GetObject(summary->getReverseRate());
	      if(BaseReaction(rxn)) {
		OutputBase->Stream << "!!!           " << *name << endl;
		result = PrintMechanismRates(OutputBase->Stream,*name,Property,rxn,classes,forward,reverse,run,runclass);
	      } else {
		Output->Stream << "!!!           " << *name << endl;
		if(duplicates->KeyWordInList(summary->getReactionName()) && lundformat) {
		  Output->Stream << "DUPLICATE" << endl;
		}
		result = PrintMechanismRates(Output->Stream,*name,Property,rxn,classes,forward,reverse,run,runclass);
		if(duplicates->KeyWordInList(summary->getReactionName()) && !lundformat) {
		  Output->Stream << "DUPLICATE" << endl;
		}
	      }
	    } else {
	      cerr << "Reverse Rate not found: '" << summary->getReverseRate();
	      cerr << "' in reaction '" << rxn->NameTag << "'";
	      result = false;
	    }
	  } else {
	    cerr << "Forward Rate not found: '" << summary->getForwardRate();
	    cerr << "' in reaction '" << rxn->NameTag << "'";
	    result = false;
	  }
	} else {
	  cerr << "Reaction not printed out: '" << *name << "'" << endl;
	}
      } else {
	cerr << "Reaction not printed out: '" << *name << "'" << endl;
      }
  }
  result = result && PrintMechanismPostfix(Output->Stream,run,runclass);
  return result;
}

/*F ans = PrintMechanismPrefix(run,runclass) 
**
**  DESCRIPTION
**    run: The run parameters
**    runclass: The run class
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintMechanismPrefix(ostream& out,
						    BaseDataAlgorithmRun* run,
						    DataAlgorithmRunClass *runclass)
{
  bool result = true;
  if(run->AlgorithmSummary.KeyWordInList("LaTeX"))
    result = PrintMechanismPrefixLaTeX(out,run,runclass);
  else {
    if(!oneFile) {
      out << "SPECIES" << endl;
      out << "END_OF_SPECIES" << endl;
    }
    out << "REACTIONS" << endl;
    //out << "e_Unit = \"kJ\"" << endl;
  }
  return true;
}
 
/*F ans = PrintMechanismPrefix(run,runclass) 
**
**  DESCRIPTION
**    run: The run parameters
**    runclass: The run class
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintMechanismPrefixLaTeX(ostream& out,
							 BaseDataAlgorithmRun* run,
							 DataAlgorithmRunClass *runclass)
{
  if(run->AlgorithmSummary.KeyWordInList("PrintReverse"))
    {
      out << "\\begin{tabular}{|l|cccc|cccc|}\\hline" << endl;
      out << "Reaction & \\multicolumn{4}{|c|}{Forward Rate} & \\multicolumn{4}{|c|}{Reverse Rate} \\\\ \\hline" << endl;
    }
  else
    {
      out << "\\begin{tabular}{|l|cccc|}\\hline" << endl;
      out << "Reaction & \\multicolumn{4}{|c|}{Forward Rate} \\\\ \\hline" << endl;
    }
  return true;
}
/*F PrintMechanismPostfix(out,run,runclass)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintMechanismPostfix(ostream& out,
						     BaseDataAlgorithmRun* run,
						     DataAlgorithmRunClass *runclass)
{
  bool result = true;
  if(run->AlgorithmSummary.KeyWordInList("LaTeX"))
    result = PrintMechanismPostfixLaTeX(out,run,runclass);
  else {
    if(oneFile)
      out << "END_OF_FILE" << endl;
    else
      out << "END_OF_FILE" << endl;
  }
  return true;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintMechanismPostfixLaTeX(ostream& out,
							  BaseDataAlgorithmRun* run,
							  DataAlgorithmRunClass *runclass)
{
  out << "\\end{tabular}" << endl;
  out << "\\begin{thebibliography}{99}" << endl;


  ObjectList<String> names = References.ListOfObjectNames();
  ObjectList<String>::iterator name;
  for(name = names.begin(); name != names.end();name++)
    {
      RxnDataLiteratureReference *ref = (RxnDataLiteratureReference *) References.GetObject(*name);
      out << "\\bibitem{" << ref->NameTag << "}" << endl;
      out << ref->getAuthor() << endl;
      out << "{\\em " << ref->getTitle() << "} " << endl;
      out << ref->getSource() << endl;
    }
  out << "\\end{thebibliography}" << endl;
  return true;
}
/*F ans = PrintMechanismRates(property,rxn,classes,forward,reverse,run,runclass)
**
**  DESCRIPTION
**    property: The property to convert the rates to
**    rxn: The reaction information
**    classes: The list of classes
**    forward: The forward reaction rate
**    reverse: The reverse reaction rate
**    run: The run parameters
**    runclass: The run class
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintMechanismRates(ostream& out,
						   String& name,
						   String property,
						   RxnDataReaction *rxn,
						   DataSetOfObjectsClass *classes,
						   RxnDataReactionRates *forward,
						   RxnDataReactionRates *reverse,
						   BaseDataAlgorithmRun* run,
						   DataAlgorithmRunClass *runclass)
{
  bool result = true;
  if(run->AlgorithmSummary.KeyWordInList("LaTeX"))
    result = PrintMechanismRatesLaTeX(out,name,property,rxn,classes,forward,reverse,run,runclass);
  else if(run->AlgorithmSummary.KeyWordInList("Standard"))
    result = PrintMechanismRatesStandard(out,name,property,rxn,classes,forward,reverse,run,runclass);
  return result;
}
/*F ans = PrintMechanismRates(property,rxn,classes,forward,reverse,run,runclass)
**
**  DESCRIPTION
**    property: The property to convert the rates to
**    rxn: The reaction information
**    classes: The list of classes
**    forward: The forward reaction rate
**    reverse: The reverse reaction rate
**    run: The run parameters
**    runclass: The run class
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintMechanismRatesLaTeX(ostream& out,
							String& name,
							String property,
							RxnDataReaction *rxn,
							DataSetOfObjectsClass *classes,
							RxnDataReactionRates *forward,
							RxnDataReactionRates *reverse,
							BaseDataAlgorithmRun* run,
							DataAlgorithmRunClass *runclass)
{
  bool result = true;
  out << name << " & ";

  RxnReactionRatesClass *rateclass = (RxnReactionRatesClass *) classes->GetObjectClass(forward->GetType());
  PrintRateLaTeX(out,rxn->getNumberOfReactants(),forward,rateclass,classes,property);
  if(run->AlgorithmSummary.KeyWordInList("PrintReverse"))
    {
      out << " & ";
      rateclass = (RxnReactionRatesClass *) classes->GetObjectClass(reverse->GetType());
      PrintRateLaTeX(out,rxn->getNumberOfProducts(),reverse,rateclass,classes,property);
    }
  out << "\\\\ \\hline" << endl;

  RxnDataLiteratureReference *ref = forward->getReference();
  if(ref != NULL)
      References.AddObject(ref);
  ref = reverse->getReference();
  if(ref != NULL)
      References.AddObject(ref);

  return result;

}
bool RxnDataPrintOutMechanism::DecideIfRateListed(RxnDataReactionRates *rate,RxnReactionRatesClass *rateclass) {
  bool brate = true;
  if(rate == NULL) 
    brate = false;
  else {
    double arr = rate->getArrheniusValue(rateclass);
    if(arr == 0.0) brate = false;
  }
  return brate;
}
/*F ans = PrintMechanismRates(property,rxn,classes,forward,reverse,run,runclass)
**
**  DESCRIPTION
**    property: The property to convert the rates to
**    rxn: The reaction information
**    classes: The list of classes
**    forward: The forward reaction rate
**    reverse: The reverse reaction rate
**    run: The run parameters
**    runclass: The run class
**
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintMechanismRatesStandard(ostream& out,
							   String& name,
							   String property,
							   RxnDataReaction *rxn,
							   DataSetOfObjectsClass *classes,
							   RxnDataReactionRates *forward,
							   RxnDataReactionRates *reverse,
							   BaseDataAlgorithmRun* run,
							   DataAlgorithmRunClass *runclass)
{
  bool result = true;
  bool thirdbody =  
    classes->IsOfClass(*forward,REACTION_THIRDBODY_NAME) || 
    classes->IsOfClass(*forward,REACTION_HILOW_NAME);
  bool chemkin   =  run->AlgorithmSummary.KeyWordInList("ChemkinName");
  bool lundformat = false;
  if(run->AlgorithmSummary.KeyWordInList("LundFormat"))
    lundformat = true;
  RxnReactionRatesClass *frateclass = NULL;
  RxnReactionRatesClass *rrateclass = NULL;
  if(forward != NULL)
    frateclass = (RxnReactionRatesClass *) classes->GetObjectClass(forward->GetType());
  if(reverse != NULL)
    rrateclass = (RxnReactionRatesClass *) classes->GetObjectClass(reverse->GetType());
  bool bforward = DecideIfRateListed(forward,frateclass);
  bool breverse = DecideIfRateListed(reverse,rrateclass);
  String forS(" => ");
  String forR(" => ");
  double arrF = forward->getArrheniusValue(rrateclass);
  double arrR = reverse->getArrheniusValue(rrateclass);
  if(!lundformat) {
    if(bforward && !breverse)
      forS = " = ";
    if(bforward && breverse && arrR > 1e-20)
      forS = " = ";
  }
  String *thirdS;
  if(lundformat) {
    if(rxn->IsInList("ThirdBodyName")) {
      BaseDataString *thrd = (BaseDataString *) rxn->GetObject("ThirdBodyName");
      String s = thrd->getString();
      thirdS = new String(" + ");
      thirdS->AppendToEnd(s);
    } else {
      thirdS = new String("+ M");
    }
  } else {
    if(classes->IsOfClass(*forward,REACTION_HILOW_NAME))
      thirdS = new String("(+m)");
    else
      thirdS = new String("+ M");
  }
  String delim(" ");
  String shortname("ChemkinName");
  unsigned int n;
  if(bforward) {
    n = rxn->getNumberOfReactants();
    result = result && rxn->WriteReactionEquation(out,true,chemkin,thirdbody,forS,*thirdS,shortname);
    out << "          ";
    result = result && forward->WriteOutRates(out,n,frateclass,property,delim,false);
    if(breverse && arrR > 1e-20 && !lundformat) {
      result = result && reverse->WriteOutRates(out,n,rrateclass,property,delim,true);
    }
    out << endl;
  } else {
    out << "%%% Reaction defined just as reverse" << endl;
    n = rxn->getNumberOfProducts();
    result = result && rxn->WriteReactionEquation(out,false,chemkin,thirdbody,forS,*thirdS,shortname);
    out << "          ";
    result = result && reverse->WriteOutRates(out,n,rrateclass,property,delim,true);
    cout << endl;
    }
  if(run->AlgorithmSummary.KeyWordInList("PrintReverse") && breverse) {
    n = rxn->getNumberOfProducts();
    if(lundformat) {
      result = result && rxn->WriteReactionEquation(out,false,chemkin,thirdbody,forS,*thirdS,shortname);
      out << "          ";
      result = result && reverse->WriteOutRates(out,n,rrateclass,property,delim,false);
      out << endl;
    }
  }
  delete thirdS;
  return result;

}
/*F ans = PrintRateLaTeX(n,rate,rateclass,property)  . .  RxnDataPrintOutMechanism
** 
**  DESCRIPTION
**    n: The number of reactants
**    rate: The reaction information
**    rateclass: The list of classes
**    property: The property to convert the rates to
**    
**  REMARKS
**
*/
bool RxnDataPrintOutMechanism::PrintRateLaTeX(ostream& out,
						 unsigned int n, 
						 RxnDataReactionRates *rate, 
						 RxnReactionRatesClass *rateclass, 
						 DataSetOfObjectsClass *classes,
						 String property)
{
  
  String delim(" & ");
  bool result = rate->WriteOutRates(out,n,rateclass,property,delim,false);
  if(classes->IsOfClass(*rate,REACTION_THIRDBODY_NAME))
    out << "(TB) ";
  RxnDataLiteratureReference *ref = rate->getReference();
  if(ref != NULL)
      out << "\\cite{" << ref->NameTag << "}";
  return result;
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
bool RxnDataPrintOutMechanism::WriteOutputValues(BaseDataSetOfInstances*,
							DataSetOfInstancesClass*,
						 BaseDataAlgorithmRun* run,
							DataAlgorithmRunClass*)
{
  bool result = true;
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
bool RxnDataPrintOutMechanism::ConcludeRun(BaseDataSetOfInstances*,
					   DataSetOfInstancesClass*,
					   BaseDataAlgorithmRun*,
					   DataAlgorithmRunClass*)
{
  bool result = true;
  delete Output;
  return result;
} 
/*S RxnPrintOutMechanismClass
 */
/*F RxnPrintOutMechanismClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnPrintOutMechanismClass::RxnPrintOutMechanismClass()
{
  Identification = MECHANISM_PRINT_ID;
  NameTag = MECHANISM_PRINT_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnPrintOutMechanismClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnPrintOutMechanismClass::RxnPrintOutMechanismClass(const RxnPrintOutMechanismClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnPrintOutMechanismClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnPrintOutMechanismClass::RxnPrintOutMechanismClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "PrintOutMechanism";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnPrintOutMechanismClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnPrintOutMechanismClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnPrintOutMechanismClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnPrintOutMechanismClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnPrintOutMechanismClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnPrintOutMechanismClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnPrintOutMechanismClass::CopyClone(Identify *  objc)
{
  RxnPrintOutMechanismClass *objcfull = (RxnPrintOutMechanismClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnPrintOutMechanismClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnPrintOutMechanismClass::Clone()
    {
      RxnPrintOutMechanismClass* id = new RxnPrintOutMechanismClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnPrintOutMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnPrintOutMechanismClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnPrintOutMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnPrintOutMechanismClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnPrintOutMechanismClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataPrintOutMechanism();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnPrintOutMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnPrintOutMechanismClass*& obj)
     {
     obj = new RxnPrintOutMechanismClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataPrintOutMechanism
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataPrintOutMechanism*& obj)
     {
     obj = new RxnDataPrintOutMechanism;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataGetGeneratedMechanism
 */ 
/*F RxnDataGetGeneratedMechanism()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataGetGeneratedMechanism::RxnDataGetGeneratedMechanism()
  : ParametersS("Generated")
{
  Identification = MECHANISM_GENERATED_ID;
  NameTag = MECHANISM_GENERATED_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataGetGeneratedMechanism(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataGetGeneratedMechanism::RxnDataGetGeneratedMechanism(const RxnDataGetGeneratedMechanism& data)
  : BaseDataAlgorithmOperation(data),
    ParametersS(data.ParametersS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataGetGeneratedMechanism::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataGetGeneratedMechanism::Clone()
{
  RxnDataGetGeneratedMechanism *obj = new RxnDataGetGeneratedMechanism(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataGetGeneratedMechanism::CopyClone(Identify * obj)
{
  RxnDataGetGeneratedMechanism *objfull = (RxnDataGetGeneratedMechanism *) obj;
  *this = *objfull;
} 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,ParametersS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,ParametersS);
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
bool RxnDataGetGeneratedMechanism::SetUpAlgorithms(BaseDataSetOfInstances *instances,
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
bool RxnDataGetGeneratedMechanism::CheckInput(BaseDataSetOfInstances *instances,
					      DataSetOfInstancesClass *instancesclass,
					      BaseDataAlgorithmRun *run,
					      DataAlgorithmRunClass *runclass)
{
  bool result = true;

  if(run->ParameterInList(ParametersS))
    {
    }
  else
    {
      cerr << "The Get Generated Parameter List  '" << ParametersS << "' was not in the list of parameters";
      result = false;
    }

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
bool RxnDataGetGeneratedMechanism::SetUpInput(BaseDataSetOfInstances *instances,
					      DataSetOfInstancesClass *instancesclass,
					      BaseDataAlgorithmRun *run,
					      DataAlgorithmRunClass *runclass)
{
  bool result = true;
  DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();

  Parameters = (BaseDataKeyWords *) run->ParameterValue(ParametersS);
  if(Parameters->SizeOf() == 10)
    {
      RootName          = Parameters->NextKey();
      DBaseName         = Parameters->NextKey();
      MoleculeDBaseS    = Parameters->NextKey();
      ReactionDBaseS    = Parameters->NextKey();
      MechanismDBaseS   = Parameters->NextKey();
      ChemkinClassS     = Parameters->NextKey();
      ChemkinThermoName = Parameters->NextKey();
      ChemkinName       = Parameters->NextKey();
      ReferenceS        = Parameters->NextKey();
      MoleculeSummaryS  = Parameters->NextKey();

      cout << "RootName:              '" << RootName << "'" << endl;
      cout << "DBaseName:             '" << DBaseName << "'" << endl;
      cout << "MoleculeDBaseS:        '" << MoleculeDBaseS << "'" << endl;
      cout << "ReactionDBaseS:        '" << ReactionDBaseS << "'" << endl;
      cout << "MechanismDBaseS:       '" << MechanismDBaseS << "'" << endl;
      cout << "ChemkinClassS:         '" << ChemkinClassS << "'" << endl;
      cout << "ChemkinThermoName:     '" << ChemkinThermoName << "'" << endl;
      cout << "ChemkinName:           '" << ChemkinName << "'" << endl;
      cout << "ReferenceS:            '" << ReferenceS << "'" << endl;
      cout << "MoleculeSummaryS:      '" << MoleculeSummaryS << "'" << endl;
    }
  else
    {
      cerr << "Expecting 10 Parameters:";
      cerr << "RootName, DataBaseName, MoleculeDBase, ReactionDBase, MechanismDBase,";
      cerr << "ChemkinClass, ChemkinThermoName, ChemkinName, ReferenceVariable, MoleculeSummary" << endl;
      result = false;
    }
  if(result && instances->IsInList(ReferenceS))
    Reference = (RxnDataLiteratureReference *) instances->GetObject(ReferenceS);
  else
    {
      cerr << "Reference: '" << ReferenceS << "' not found" << endl;
      result = false;
    }

  if(result)
    {
      ChemkinFile      = new OpenInputFile(RootName,"thm");
      MoleculeFile     = new OpenInputFile(RootName,"sdf");
      MechanismFile    = new OpenInputFile(RootName,"mech");
      if(ChemkinFile->Stream == NULL ||
	 MoleculeFile->Stream == NULL ||
	 MechanismFile->Stream == NULL)
	{
	  cerr << "Couldn't Open Input Files (.thm,.sdf,.mech) with root: '" << RootName << "'" << endl;
	  result = false;
	}
    }
  if(result && instances->IsInList(DBaseName))
    DataBase = (RxnDataMolecularStructuresDataBase  *) instances->GetObject(DBaseName);
  else
    {
      cerr << "Database not found '" << DBaseName << "'" << endl;
      result = false;
    }
  if(result)
    {
    MoleculeDBase = (BaseDataDataBaseInformation  *) DataBase->getDatabaseInfo(MoleculeDBaseS);
    ReactionDBase = (BaseDataDataBaseInformation  *) DataBase->getDatabaseInfo(ReactionDBaseS);
    MechanismDBase = (BaseDataDataBaseInformation  *) DataBase->getDatabaseInfo(MechanismDBaseS);
    if(MoleculeDBase == NULL || ReactionDBase == NULL || MechanismDBase == NULL)
      {
	cerr << "Error in setting up databases:";
	cerr << " '" << MoleculeDBaseS  << "'";
	cerr << " '" << ReactionDBaseS  << "'";
	cerr << " '" << MechanismDBaseS  << "'";
	cerr << endl;
	result = false;
      }
    }
  if(result)
    {
      DataBaseClass = (RxnMolecularStructuresDataBaseClass *) 
	classes->GetObjectClass(DataBase->GetType());
      DBaseClass = (DataDataBaseInformationClass *) classes->GetObjectClass(MoleculeDBase->GetType());

      MoleculeClass = (RxnSimpleMoleculeClass *) MoleculeDBase->getDataElementClass(DBaseClass);
      ReactionClass = (RxnReactionClass *) ReactionDBase->getDataElementClass(DBaseClass);
      MechanismClass = (RxnMechanismClass *)  MechanismDBase->getDataElementClass(DBaseClass);
      cout << "Mechanism Class: '" << MechanismClass->NameTag << '"' << endl;
      cout << endl;
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
bool RxnDataGetGeneratedMechanism::Calculate(BaseDataSetOfInstances *instances,
					     DataSetOfInstancesClass *instancesclass,
					     BaseDataAlgorithmRun *run,
					     DataAlgorithmRunClass *runclass)
{
  bool result = true;

  SetUpMechanismInInstance(instances);

  result = result && ReadInMolecules(instances,instancesclass);
  result = result && ReadReactionClasses(MechanismFile->Stream,instances);
  return result;
}
 
/*F SetUpMechanismInInstance(instances)
**
**  DESCRIPTION
**    instances: The set of instances to store mechanism instance
**
**    This sets up the mechanism instance.  The Mechanism slot in the
**    class is set up also (pointer within instance to spare extra Clone()ing
**
**  REMARKS
**
*/
void  RxnDataGetGeneratedMechanism::SetUpMechanismInInstance(BaseDataSetOfInstances *instances)
{
  cout << "Mechanism: " << RootName << endl;
  
  Mechanism = (RxnDataMechanism *) MechanismClass->BaseDataObjectExample();
  Mechanism->NameTag = MechanismClass->getNameInInstance();

  BaseDataInstance *instance = (BaseDataInstance *) new BaseDataInstance;
  instance->NameTag = RootName;
  instances->AddInstance(*instance);
  delete instance;

  instance = (BaseDataInstance *) instances->GetInstance(RootName);
  instance->AddObject(Mechanism);
  delete Mechanism;
  Mechanism = (RxnDataMechanism *) instance->GetObject(MechanismClass->getNameInInstance());

  if(instances->IsInList(MoleculeSummaryS)) {
      BaseDataSetOfObjects *molsummary = (BaseDataSetOfObjects *) instances->GetObject(MoleculeSummaryS);
      Mechanism->setUpMoleculeSummaries(molsummary);
    } else {
      cerr << "No Standard Molecule Summary found in instances: '" << MoleculeSummaryS << "'" << endl;
      cerr << "Continuing" << endl;
    }
}
/*F ans = ReadInMolecules(instances)  . . . . .  RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    instances: The set of instances
**    ans: True if successfull
**
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::ReadInMolecules(BaseDataSetOfInstances *instances,
						   DataSetOfInstancesClass *instancesclass)
{
  bool success = true;
  BaseDataKeyWords *moleculelist = new BaseDataKeyWords();
  moleculelist->NameTag = MOLECULE_LIST;
  DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
  RxnChemkinThermoClass *chemkinclass = (RxnChemkinThermoClass *) classes->GetObjectClass(ChemkinClassS);
  while(success)
    {
      RxnDataSimpleMolecule *simple = (RxnDataSimpleMolecule *) MoleculeClass->BaseDataObjectExample();
      success = simple->ReadMolFileMolecule(MoleculeFile->Stream,MoleculeClass);
      
      if(success) {
	cout << "Molecule: " << simple->NameTag << endl;
	  BaseDataInstance *instance = InstanceInInstances(simple->NameTag,instances,instancesclass);

	  RxnDataChemkinThermo *chemkin = (RxnDataChemkinThermo *) chemkinclass->BaseDataObjectExample();
	  chemkin->ReadBasicChemkinElement(ChemkinFile->Stream,chemkinclass);
	  chemkin->setReference(Reference);
	  chemkin->NameTag = ChemkinThermoName;
	  RxnMoleculeSummaryClass *molsumclass = MechanismClass->getMoleculeSummaryClass();
	  RxnDataMoleculeSummary *molsum = (RxnDataMoleculeSummary *) molsumclass->BaseDataObjectExample();
	  molsum->NameTag = simple->NameTag;
	  molsum->ThermodynamicInfo = chemkin->NameTag;
	  molsum->ShortName = ChemkinName;
	  
	  Mechanism->addMolecule(simple->NameTag);
	  moleculelist->AddKeyWord(simple->NameTag);
	  if(MoleculeDBase->ElementExists(simple->NameTag)) {
	      if(instancesclass->getDebugLevel() > 0)
		cout << "Using Database Molecule Information: '" << simple->NameTag << "'" << endl;
	      RxnDataSimpleMolecule *molecule = (RxnDataSimpleMolecule *) MoleculeClass->BaseDataObjectExample();
	      MoleculeDBase->FetchElement(simple->NameTag,DBaseClass,(BaseDataObject *&) molecule);
	      molecule->NameTag = MoleculeClass->GetNameInInstance();


	      BaseDataString *cn = (BaseDataString *) molecule->RetrieveProperty(ChemkinName);
	      if(cn == NULL) {
		cerr << "In molecule: '" << molecule->NameTag << "' the chemkin name, '" << ChemkinName << "' was not found" << endl;
	      } else {
		cerr << "The chemkin name for '" << molecule->NameTag << "' is '" << cn->getString() << "'" << endl;
	      }
	      //StoreChemkinNameInMolecule(chemkin->getSpeciesName(), molecule, instance);
	      //StoreThermoInMolecule(chemkin,molecule,instance);
	      instance->AddObject(molecule);
	      delete molecule;
	  } else {
	      if(instancesclass->getDebugLevel() > 0) {
		cout << "Special Molecule to Mechanism: '" << simple->NameTag;
	        cout << "' (" << chemkin->getSpeciesName() << ")" << endl;
	      }
	      StoreChemkinNameInMolecule(chemkin->getSpeciesName(), simple, instance);
	      StoreThermoInMolecule(chemkin,simple,instance);
	      Mechanism->AddObject(simple);
	      simple->NameTag = MoleculeClass->GetNameInInstance();
	      instance->AddObject(simple);
	    }
	  Mechanism->addMoleculeSummary(molsum);
	  molsum->NameTag = MOLECULE_SUMMARY_NAME;
	  instance->AddObject(molsum);
	  delete molsum;
	  delete simple;
	  delete chemkin;
      } else {
	cerr << "ReadMolFileMolecule: not successful" << endl;
      }
    }
  instances->AddObject(moleculelist);
  return true;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataInstance *RxnDataGetGeneratedMechanism::InstanceInInstances(String& name, 
								    BaseDataSetOfInstances *instances,
								    DataSetOfInstancesClass *instancesclass)
{
  BaseDataInstance *instance = new BaseDataInstance();
  instance->NameTag = name;
  instances->AddInstance(*instance);
  delete instance;
  instance = instances->GetInstance(name);
  return instance;
}
 
/*F ans = StoreChemkinNameInMolecule(molecule,instance)
**
**  DESCRIPTION
**    molecule: The molecule to store chemkin name
**    instance: THe instance
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::StoreChemkinNameInMolecule(String& name,
							      RxnDataSimpleMolecule *molecule,
							      BaseDataInstance *instance)
{
  bool result = true;
  BaseDataString *chemkinname = new BaseDataString();
  chemkinname->NameTag = ChemkinName;
  chemkinname->setString(name);
  result = molecule->StoreProperty(chemkinname);
  instance->AddObject(chemkinname);
  delete chemkinname;
  return result;
}
 
/*F ans = StoreThermoInMolecule(chemkin,simple,instance)RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::StoreThermoInMolecule(RxnDataChemkinThermo *chemkin,
							 RxnDataSimpleMolecule *molecule,
							 BaseDataInstance *instance)
{
  if(molecule->RetrieveProperty(chemkin->NameTag) == NULL)
    {
      molecule->StoreProperty(chemkin);
      instance->AddObject(chemkin);
    }
  else 
    {
      //cerr << "StoreThermoInMolecule: Thermal Value already defined, not stored: '";
      //cerr << molecule->NameTag << "'" << endl;
    }
  return true;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::ReadReactionClasses(istream& in,
						       BaseDataSetOfInstances *instances)
{
  bool result = true;
  StreamObjectInput str(in,' ');

  cout << "Reading Reaction Class Coefficients" << endl;
  String key = str.ReadNext();
  while(!(key == "CLASSCOEFFICIENTS"))
    key = str.ReadNext();
  

  ReadInClassCoefficients(in,instances);
  ReactionList = new BaseDataKeyWords(0);
  ReactionList->NameTag = REACTION_LIST;

  key = str.ReadNext();
  while(!(key == "REACTIONCLASS"))
    key = str.ReadNext();
  cout << "Reading in Reaction Class Information (the reactions)" << endl;
  while(key == "REACTIONCLASS")
    {
      ReadInReactionSet(in,instances);
      key = str.ReadNext();
    }
  cout << "Done Reading Mechanism" << endl;
  instances->AddObject(ReactionList);
  delete ReactionList;
  return result;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::ReadInReactionSet(istream& in,
						     BaseDataSetOfInstances *instances)
{
  bool result = true;
  StreamObjectInput str(in,' ');
  String key = str.ReadNext();
  String rxnclassS = str.ReadNext();
  cout << "Reaction Class: " << rxnclassS << endl;

  //  An instance of reaction class  'instance'
  BaseDataInstance *instance = new BaseDataInstance();
  instance->NameTag = rxnclassS;
  instances->AddInstance(*instance);
  delete instance;
  instance = (BaseDataInstance *) instances->GetInstance(rxnclassS);

  // Get previously read in coefficients in 'rxn'  (stored in instance)
  RxnDataReaction *rxn = (RxnDataReaction *) Reactions.GetObject(rxnclassS);
  String name = ReactionClass->getNameInInstance();
  rxn->NameTag = name;
  instance->AddObject(rxn);
  rxn->NameTag = rxnclassS;
  rxn = (RxnDataReaction *) instance->GetObject(name);
  RxnDataReactionRates *forward = (RxnDataReactionRates *) rxn->GetObject("Forward");
  RxnDataReactionRates *reverse = (RxnDataReactionRates *) rxn->GetObject("Reverse");

  unsigned int count = 0;
  key = str.ReadNext();
  while(!(key == "END"))
    {
      String rxnname = ReactionName(rxnclassS,count);
      ReactionList->AddKeyWord(rxnname);
      double multiplicity = key.ToFloat();
      RxnDataReaction *newrxn = MakeReaction(rxnname,forward,reverse);

      key = ReadInReaction(key,multiplicity,in,newrxn);
      rxn->AddObject(newrxn);
      Mechanism->addReaction(newrxn);

      BaseDataInstance rinstance;
      rinstance.NameTag = newrxn->NameTag;
      newrxn->NameTag = name;
      rinstance.AddObject(newrxn);
      AddReactionSummaryToMechanism(rxnname,&rinstance,forward,reverse);

      BaseDataKeyWords *reactants = (BaseDataKeyWords *) newrxn->getReactantNames().Clone();
      reactants->NameTag = "Reactants";
      BaseDataKeyWords *products = (BaseDataKeyWords *) newrxn->getProductNames().Clone();
      products->NameTag = "Products";
      rinstance.AddObject(reactants);
      rinstance.AddObject(products);

      instances->AddInstance(rinstance);
      delete newrxn;
      count++;
    }
  return result;
} 
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReaction *RxnDataGetGeneratedMechanism::MakeReaction(String& rxnname,
							    RxnDataReactionRates *forward,
							    RxnDataReactionRates *reverse)
{
  RxnDataReaction *newrxn = (RxnDataReaction *) ReactionClass->BaseDataObjectExample();
  newrxn->NameTag = rxnname;
  
  RxnDataMoleculeSet *Reactants = new RxnDataMoleculeSet();
  RxnDataMoleculeSet *Products  = new RxnDataMoleculeSet();
  
  newrxn->AddObject(forward);
  newrxn->AddObject(reverse);
  
  newrxn->setReactants(Reactants);
  newrxn->setProducts(Products);
  return newrxn;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataGetGeneratedMechanism::AddReactionSummaryToMechanism(String& rxnname,
								 BaseDataInstance *instance,
								 RxnDataReactionRates *forward,
								 RxnDataReactionRates *reverse)
{ 
  RxnDataReactionSummary *summary = (RxnDataReactionSummary *)
    MechanismClass->getSummaryClass()->BaseDataObjectExample();
  summary->NameTag = rxnname;
  summary->setReactionName(rxnname);
  summary->setForwardRate(forward->NameTag);
  summary->setReverseRate(reverse->NameTag);
  Mechanism->addReactionSummary(summary);
  summary->NameTag = REACTION_SUMMARY_NAME;
  instance->AddObject(summary);
  delete summary;
}
/*F name = ReactionName(rxnclassS,count)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
String RxnDataGetGeneratedMechanism::ReactionName(String& rxnclassS, unsigned int count)
{
  String countS = Int2String(count);
  String rxnname(rxnclassS);
  String lp("[");
  String rp("]");
  rxnname.AppendToEnd(lp);
  rxnname.AppendToEnd(countS);
  rxnname.AppendToEnd(rp);

  return rxnname;
}
/*F next = ReadInReaction(key,str,forward,reverse)
**
**  DESCRIPTION
**    key: The next keyword (should be the multiplicity)
**    str: The input stream object
**    forward: The forward rate constants
**    reverse: The reverse rate constants
**
**  REMARKS
**
*/
String& RxnDataGetGeneratedMechanism::ReadInReaction(String& key,
						     double multiplicity,
						     istream& in,
						     RxnDataReaction *rxn)
{
  StreamObjectInput str(in,' ');

  RxnDataMoleculeSet *Reactants = rxn->getReactants();
  RxnDataMoleculeSet *Products  = rxn->getProducts();
  RxnDataReactionRates *forward = (RxnDataReactionRates *) rxn->GetObject("Forward");
  BaseDataReal *arr = (BaseDataReal *) forward->getArrhenius(ReactionClass->getRatesClass());
  arr->SetValue( multiplicity * (arr->GetValue()));

  key = str.ReadNext();
  while(!(key == "="))
    {
      String name = StripParensFromName(key);
      Reactants->AddMolecule(name);
      key = str.ReadNext();
      if(key == "+")
	key = str.ReadNext();
    }
  key = str.ReadNext();
  while(key.c_str()[0] == '{')
    {
      String name = StripParensFromName(key);
      Products->AddMolecule(name);
      key = str.ReadNext();
      if(key == "+")
	key = str.ReadNext();
    }
  return key;
}
/*F name = StripParensFromName(key) . . . . . .  RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    key: The original name
**    newname: The stripped name
**
**  REMARKS
**
*/
String RxnDataGetGeneratedMechanism::StripParensFromName(String& name)
{
  unsigned int n = name.size();
  String newname = name.Isolate(1,n-3);
  return newname;
}
/*F ans = ReadInClassCoefficients(str)  . . . . .  RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    str: input stream object
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataGetGeneratedMechanism::ReadInClassCoefficients(istream& in,
							   BaseDataSetOfInstances *instances)
{
  StreamObjectInput str(in,' ');
  bool result = true;

  RxnDataReaction *rxn;
  RxnReactionRatesClass *rateclass = ReactionClass->getRatesClass();

  BaseDataKeyWords rxnclasslist;
  rxnclasslist.NameTag = REACTION_CLASS_LIST;
  String key = str.ReadNext();
  while(!(key == "END"))
    {
      rxnclasslist.AddKeyWord(key);
      //cout << "Reaction Pattern: " << key << endl;
      if(instances->InstanceInSet(key))
	{
	  BaseDataInstance *instance = (BaseDataInstance *) instances->GetInstance(key);
	  rxn = (RxnDataReaction *) instance->GetObject(ReactionClass->getNameInInstance())->Clone();
	}
      else
	{
	  rxn = (RxnDataReaction *) ReactionClass->BaseDataObjectExample();
	  rxn->NameTag = key;
	}

      key = str.ReadNext();
      
      RxnDataReactionRates *forward = (RxnDataReactionRates *) rateclass->BaseDataObjectExample();
      forward->NameTag = "Forward";
      forward->ReadConstants(in,rateclass);
      RxnDataReactionRates *reverse = (RxnDataReactionRates *) rateclass->BaseDataObjectExample();
      reverse->NameTag = "Reverse";
      reverse->ReadConstants(in,rateclass);

      rxn->AddObject(forward);
      rxn->AddObject(reverse);

      delete forward;
      delete reverse;

      Reactions.AddObject(rxn);
      delete rxn;

      key = str.ReadNext();
    }
  instances->AddObject(&rxnclasslist);
  Mechanism->AddObject(&rxnclasslist);
  return result;
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
bool RxnDataGetGeneratedMechanism::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  
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
bool RxnDataGetGeneratedMechanism::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 

/*S RxnGetGeneratedMechanismClass
 */
/*F RxnGetGeneratedMechanismClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnGetGeneratedMechanismClass::RxnGetGeneratedMechanismClass()
{
  Identification = MECHANISM_GENERATED_ID;
  NameTag = MECHANISM_GENERATED_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnGetGeneratedMechanismClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnGetGeneratedMechanismClass::RxnGetGeneratedMechanismClass(const RxnGetGeneratedMechanismClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnGetGeneratedMechanismClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnGetGeneratedMechanismClass::RxnGetGeneratedMechanismClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "GetGeneratedMechanism";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnGetGeneratedMechanismClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnGetGeneratedMechanismClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnGetGeneratedMechanismClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnGetGeneratedMechanismClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnGetGeneratedMechanismClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnGetGeneratedMechanismClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnGetGeneratedMechanismClass::CopyClone(Identify *  objc)
{
  RxnGetGeneratedMechanismClass *objcfull = (RxnGetGeneratedMechanismClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnGetGeneratedMechanismClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnGetGeneratedMechanismClass::Clone()
    {
      RxnGetGeneratedMechanismClass* id = new RxnGetGeneratedMechanismClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnGetGeneratedMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnGetGeneratedMechanismClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnGetGeneratedMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnGetGeneratedMechanismClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnGetGeneratedMechanismClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataGetGeneratedMechanism();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnGetGeneratedMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnGetGeneratedMechanismClass*& obj)
     {
     obj = new RxnGetGeneratedMechanismClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataGetGeneratedMechanism
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataGetGeneratedMechanism*& obj)
     {
     obj = new RxnDataGetGeneratedMechanism;
     return obj->DecodeThis(buffer);
     }

/*S RxnDataMechanismDataBase
 */ 
/*F RxnDataMechanismDataBase()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMechanismDataBase::RxnDataMechanismDataBase()
  : DBMechanisms(NULL),
    DBMechanismPatterns(NULL)
{
  Identification = MECHANISM_DBASE_ID;
  NameTag = MECHANISM_DBASE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMechanismDataBase(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMechanismDataBase::RxnDataMechanismDataBase(const RxnDataMechanismDataBase& data)
  : RxnDataReactionStructuresDataBase(data)
{
    DBMechanisms = (BaseDataDataBaseInformation *) PointerClone(data.DBMechanisms);
    DBMechanismPatterns = (BaseDataDataBaseInformation *) PointerClone(data.DBMechanismPatterns);
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMechanismDataBase
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMechanismDataBase::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMechanismDataBase
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMechanismDataBase::Read(istream& in, DataObjectClass* objc, const String& name)
{
  RxnMechanismDataBaseClass *mechclass = (RxnMechanismDataBaseClass *) objc;
  
  bool result = RxnDataReactionStructuresDataBase::Read(in,objc,name);
  String notdefined("Not Defined");
  result = result && PointerObjectRead(in,(BaseDataObject *&) DBMechanisms,mechclass->getMoleculeDBClass(),notdefined);
  result = result && PointerObjectRead(in,(BaseDataObject *&) DBMechanismPatterns,mechclass->getMoleculeDBClass(),notdefined);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMechanismDataBase
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMechanismDataBase::print(ostream& out) const
{
  RxnDataReactionStructuresDataBase::print(out);
  PointerPrint(out,"The Mechanism Database: ",
	       "No Database defined",DBMechanisms);
  PointerPrint(out,"The Mechanism Pattern Database: ",
	       "No Database defined",DBMechanismPatterns);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanismDataBase
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMechanismDataBase::Clone()
{
  RxnDataMechanismDataBase *obj = new RxnDataMechanismDataBase(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanismDataBase
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMechanismDataBase::CopyClone(Identify * obj)
{
  RxnDataMechanismDataBase *objfull = (RxnDataMechanismDataBase *) obj;
  *this = *objfull;
  DBMechanisms        = (BaseDataDataBaseInformation *) PointerClone(objfull->DBMechanisms);
  DBMechanismPatterns        = (BaseDataDataBaseInformation *) PointerClone(objfull->DBMechanismPatterns);
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanismDataBase
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanismDataBase::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionStructuresDataBase::EncodeThis(buffer);
  result = result && PointerEncode(buffer,DBMechanisms);
  result = result && PointerEncode(buffer,DBMechanismPatterns);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanismDataBase
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanismDataBase::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionStructuresDataBase::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) DBMechanisms);
  result = result && PointerDecode(buffer,(BaseDataObject *&) DBMechanismPatterns);
  return result;
}
/*F ans = Initialize(dbclass)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataMechanismDataBase::Initialize(DataDataBaseInformationClass *dbclass)
{
  RxnDataReactionStructuresDataBase::Initialize(dbclass);
  bool result = true;
  if( DBMechanisms != NULL &&
      DBMechanismPatterns != NULL)
    {
      DBMechanisms->OpenUpDataBase(dbclass);
      DBMechanismPatterns->OpenUpDataBase(dbclass);
    }
  else
    {
      cerr << "Databases not defined yet" << endl;
      result = false;
    }

  return result;
}
/*F getDatabaseInfo(type) . . . . . . . .  RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    type: The type of database
**
**  REMARKS
**
*/
BaseDataDataBaseInformation *RxnDataMechanismDataBase::getDatabaseInfo(String& type)
{
  BaseDataDataBaseInformation *dbaseinfo = RxnDataReactionStructuresDataBase::getDatabaseInfo(type);
  if(type == "Mechanisms")
    {
      dbaseinfo = DBMechanisms;
      cout << "Mechanism Database used" << endl;
    }
  else if(type == "MechanismPatterns")
    dbaseinfo = DBMechanismPatterns;
  return dbaseinfo;
}
/*S RxnMechanismDataBaseClass
 */
/*F RxnMechanismDataBaseClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMechanismDataBaseClass::RxnMechanismDataBaseClass()
{
  Identification = MECHANISM_DBASE_ID;
  NameTag = MECHANISM_DBASE_NAME;
  SubClass = "ReactionStructuresDataBase";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMechanismDataBaseClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMechanismDataBaseClass::RxnMechanismDataBaseClass(const RxnMechanismDataBaseClass& data)
  : RxnReactionStructuresDataBaseClass(data)
{
} 
 
/*F RxnMechanismDataBaseClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMechanismDataBaseClass::RxnMechanismDataBaseClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnReactionStructuresDataBaseClass(id,name,descr)
{
  SubClass = "ReactionStructuresDataBase";
  EncodeDecodeClass = MECHANISM_DBASE_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMechanismDataBaseClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMechanismDataBaseClass::print(ostream& out) const
{
  RxnReactionStructuresDataBaseClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMechanismDataBaseClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMechanismDataBaseClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMechanismDataBaseClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnReactionStructuresDataBaseClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMechanismDataBaseClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMechanismDataBaseClass::CopyClone(Identify *  objc)
{
  RxnMechanismDataBaseClass *objcfull = (RxnMechanismDataBaseClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMechanismDataBaseClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMechanismDataBaseClass::Clone()
    {
      RxnMechanismDataBaseClass* id = new RxnMechanismDataBaseClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismDataBaseClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionStructuresDataBaseClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismDataBaseClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionStructuresDataBaseClass::DecodeThis(buffer);
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
BaseDataObject * RxnMechanismDataBaseClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMechanismDataBase();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMechanismDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMechanismDataBaseClass*& obj)
     {
     obj = new RxnMechanismDataBaseClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMechanismDataBase
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMechanismDataBase*& obj)
     {
     obj = new RxnDataMechanismDataBase;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataFillMechanism
 */ 
/*F RxnDataFillMechanism()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataFillMechanism::RxnDataFillMechanism()
  : parametersS("Parameters")
{
  Identification = MECHANISM_FILLALG_ID;
  NameTag = MECHANISM_FILLALG_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataFillMechanism(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataFillMechanism::RxnDataFillMechanism(const RxnDataFillMechanism& data)
  : BaseDataAlgorithmOperation(data),
    parametersS(data.parametersS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataFillMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataFillMechanism::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataFillMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataFillMechanism::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);  
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataFillMechanism
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataFillMechanism::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Parameter: '" << parametersS << "'" << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataFillMechanism
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataFillMechanism::Clone()
{
  RxnDataFillMechanism *obj = new RxnDataFillMechanism(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataFillMechanism
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataFillMechanism::CopyClone(Identify * obj)
{
  RxnDataFillMechanism *objfull = (RxnDataFillMechanism *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataFillMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataFillMechanism::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,parametersS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataFillMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataFillMechanism::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,parametersS);
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
bool RxnDataFillMechanism::SetUpAlgorithms(BaseDataSetOfInstances *instances,
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
bool RxnDataFillMechanism::CheckInput(BaseDataSetOfInstances *instances,
				      DataSetOfInstancesClass *instancesclass,
				      BaseDataAlgorithmRun *run,
				      DataAlgorithmRunClass *runclass)
{
  bool result = true;

  if(run->ParameterInList(parametersS))
    {
    }
  else
    {
      cerr << "The list of parameters '" << parametersS << "' was not in the list of attributes";
      result = false;
    }

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
bool RxnDataFillMechanism::SetUpInput(BaseDataSetOfInstances *instances,
				      DataSetOfInstancesClass *instancesclass,
				      BaseDataAlgorithmRun *run,
				      DataAlgorithmRunClass *runclass)
{
  bool result = true;
  
  parameters = (BaseDataKeyWords *) run->ParameterValue(parametersS)->Clone();
  
  if(parameters->SizeOf() >= 4) 
    {
      mechClassS = parameters->NextKey();
      mechanismS = parameters->NextKey();
      dataBaseS  = parameters->NextKey();
      rxnTypeS = parameters->NextKey();

      if(instancesclass->IsInList(mechClassS))
	{
	  mechClass = (RxnMechanismClass *) instancesclass->GetObjectClass(mechClassS);
	  String mechname = mechClass->getNameInInstance();
	  
	  if(instances->InstanceInSet(mechanismS))
	    {
	      BaseDataInstance *instance = instances->GetInstance(mechanismS);
	      if(instance->IsInList(mechname))
		{
		  mechanism = (RxnDataMechanism *) instance->GetObject(mechname);
		  if(instances->IsInList(dataBaseS))
		    {
		      dataBase = (RxnDataMolecularStructuresDataBase  *) instances->GetObject(dataBaseS);		  
		      rxndbaseinfo = dataBase->getDatabaseInfo(rxnTypeS);
		      rxnClass = (RxnReactionClass *)
			rxndbaseinfo->getDataElementClass(dbaseinfoclass);

		      if(rxndbaseinfo != NULL)
			{
			  dataBaseClass = (RxnMolecularStructuresDataBaseClass *)
			    instancesclass->GetObjectClass(dataBase->GetType());
			  dbaseinfoclass = dataBaseClass->getMoleculeDBClass();
			}
		      else
			{
			  cerr << "Reaction Class '" << rxnTypeS << "' not found" << endl;
			  result = false;
			}
		    }
		  else
		    {
		      cerr << "Database not found '" << dataBaseS << "'" << endl;
		      result = false;
		    }
		}
	      else
		{
		  cerr << "Mechanism: '" << mechname << "' not in instance '" << mechanismS << "'" << endl;
		  result = false;
		}
	    }
	  else
	    {
	      cerr << "Instance not found: '" << mechanismS << "'" << endl;
	      result = false;
	    }
	}
      else
	{
	  cerr << "Mechanism Class '" << mechClassS << "' not found" << endl;
	  result = false;
	}
    }
  else
    {
      cerr << "Expected three parameters:" << endl;
      cerr << "         MechClass: The name of the mechanism class" << endl;
      cerr << "         Mechanism: Name of the Mechanism (In instances)" << endl;
      cerr << "         Database: The database specification (name in attributes)" << endl;
      cerr << "         RxnType: Reactions, ReactionPatterns" << endl;
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
bool RxnDataFillMechanism::Calculate(BaseDataSetOfInstances *instances,
				     DataSetOfInstancesClass *instancesclass,
				     BaseDataAlgorithmRun *run,
				     DataAlgorithmRunClass *runclass)
{
  bool result = true;
  //if(run->AlgorithmSummary.KeyWordInList("")){}

  BaseDataKeyWords rxns = mechanism->getReactionNames();
  ObjectList<String> names = rxns.GetKeyWords();

  ObjectList<String>::iterator name;
  bool success = true;
  for(name = names.begin(); success && name != names.end(); name++)
    {
      RxnDataReaction *rxn = (RxnDataReaction *) rxnClass->BaseDataObjectExample();
      if(!instances->InstanceInSet(*name))
	{
	  success = rxndbaseinfo->FetchElement(*name,dbaseinfoclass,(BaseDataObject *&) rxn);
	  if(success)
	    {
	      MakeNewInstance(*name,instances);
	      BaseDataInstance *instance = (BaseDataInstance *) instances->GetInstance(*name);
	      rxn->NameTag = *name;
	      instance->AddObject(rxn);
	      delete rxn;
	    }
	  else
	    {
	      cerr << "Failed to Fetch Element: " << *name << endl;
	      result = false;
	    }
	}
      else
	{
	  cerr << "Reaction instance '" << *name << "' already there" << endl;
	}
    }
  return result;
}
 
/*F MakeNewInstance(name,instances)
**
**  DESCRIPTION
**    name: Name of empty instance to create
**    instances: The set of instances
**
**  REMARKS
**
*/
void RxnDataFillMechanism::MakeNewInstance(String name, BaseDataSetOfInstances *instances)
{
  BaseDataInstance *newinstance = new BaseDataInstance;
  newinstance->NameTag = name;
  instances->AddInstance(*newinstance);
  delete newinstance;
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
bool RxnDataFillMechanism::WriteOutputValues(BaseDataSetOfInstances *instances,
					     DataSetOfInstancesClass *instancesclass,
					     BaseDataAlgorithmRun *run,
					     DataAlgorithmRunClass *runclass)
{
  bool result = true;
  
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
bool RxnDataFillMechanism::ConcludeRun(BaseDataSetOfInstances *instances,
				       DataSetOfInstancesClass *instancesclass,
				       BaseDataAlgorithmRun *run,
				       DataAlgorithmRunClass *runclass)
{
  bool result = true;
  //  delete something
  return result;
}
 
/*S RxnFillMechanismClass
 */
/*F RxnFillMechanismClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnFillMechanismClass::RxnFillMechanismClass()
{
  Identification = MECHANISM_FILLALG_ID;
  NameTag = MECHANISM_FILLALG_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnFillMechanismClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnFillMechanismClass::RxnFillMechanismClass(const RxnFillMechanismClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnFillMechanismClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnFillMechanismClass::RxnFillMechanismClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "FillMechanism";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnFillMechanismClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnFillMechanismClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnFillMechanismClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnFillMechanismClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnFillMechanismClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnFillMechanismClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnFillMechanismClass::CopyClone(Identify *  objc)
{
  RxnFillMechanismClass *objcfull = (RxnFillMechanismClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnFillMechanismClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnFillMechanismClass::Clone()
    {
      RxnFillMechanismClass* id = new RxnFillMechanismClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnFillMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnFillMechanismClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnFillMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnFillMechanismClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnFillMechanismClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataFillMechanism();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnFillMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnFillMechanismClass*& obj)
     {
     obj = new RxnFillMechanismClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataFillMechanism
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataFillMechanism*& obj)
     {
     obj = new RxnDataFillMechanism;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataBuildMechanism
 */ 
/*F RxnDataBuildMechanism()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataBuildMechanism::RxnDataBuildMechanism()
  : ReactionListS(REACTION_LIST),
    MoleculeListS(MOLECULE_LIST),
    MechanismNameS(MECHANISM_PARAMETER),
    UseReactionClassesS(MECHANISM_USE_REACTION_CLASSES)
{
  Identification = MECHANISM_BUILD_ID;
  NameTag = MECHANISM_BUILD_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataBuildMechanism(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataBuildMechanism::RxnDataBuildMechanism(const RxnDataBuildMechanism& data)
  : BaseDataAlgorithmOperation(data),
    ReactionListS(data.ReactionListS),
    MoleculeListS(data.MoleculeListS),
    MechanismNameS(data.MechanismNameS),
    UseReactionClassesS(data.UseReactionClassesS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataBuildMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataBuildMechanism::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataBuildMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataBuildMechanism::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataBuildMechanism
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataBuildMechanism::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Algorithm to build a mechanism, '" << MechanismNameS << "'";
  out << " using '" << MoleculeListS << "' and '" << ReactionListS << "'";
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataBuildMechanism
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataBuildMechanism::Clone()
{
  RxnDataBuildMechanism *obj = new RxnDataBuildMechanism(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataBuildMechanism
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataBuildMechanism::CopyClone(Identify * obj)
{
  RxnDataBuildMechanism *objfull = (RxnDataBuildMechanism *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataBuildMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBuildMechanism::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,ReactionListS);
  result = result && Encode(buffer,MoleculeListS);
  result = result && Encode(buffer,MechanismNameS);
  result = result && Encode(buffer,UseReactionClassesS);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataBuildMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBuildMechanism::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,ReactionListS);
  result = result && Decode(buffer,MoleculeListS);
  result = result && Decode(buffer,MechanismNameS);
  result = result && Decode(buffer,UseReactionClassesS);
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
bool RxnDataBuildMechanism::SetUpAlgorithms(BaseDataSetOfInstances *instances,
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
bool RxnDataBuildMechanism::CheckInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result && CheckInputVariable(ReactionListS,"Reaction List",run);
  result = result && CheckInputVariable(MoleculeListS,"Molecule List",run);
  result = result && CheckInputVariable(MechanismNameS,"MechanismName",run);
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
bool RxnDataBuildMechanism::SetUpInput(BaseDataSetOfInstances *instances,
				       DataSetOfInstancesClass *instancesclass,
				       BaseDataAlgorithmRun *run,
				       DataAlgorithmRunClass *runclass)
{
  bool result = true;

  ReactionList = (BaseDataKeyWords *) run->ParameterValue(ReactionListS)->Clone();
  MoleculeList = (BaseDataKeyWords *) run->ParameterValue(MoleculeListS)->Clone();
  MechanismName = (BaseDataString *) run->ParameterValue(MechanismNameS);
  NewMechanismName = MechanismName->getString();
  BaseDataInstance *instance = new BaseDataInstance();
  Mechanism = new RxnDataMechanism();
  Mechanism->NameTag = "Mechanism";
  instance->NameTag = NewMechanismName;
  instance->AddObject(Mechanism);
  instances->AddInstance(*instance);
  delete instance;
  instance = instances->GetInstance(NewMechanismName);
  Mechanism = (RxnDataMechanism *) instance->GetObject("Mechanism");
  cout << "The new Mechanism will be: '" << Mechanism->NameTag << "' under instance '" << NewMechanismName << "'" << endl;
  cout << "Mechanism Type: " << Mechanism->GetType() << endl;
  MechanismClass = (RxnMechanismClass *) instancesclass->GetObjectClass("StandardMechanism");
  cout << "MechanismClass: '" << MechanismClass->NameTag << "'" << endl;
  UseReactionClasses = false;
  if(run->AlgorithmSummary.KeyWordInList(UseReactionClassesS)){
    UseReactionClasses = true;
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
bool RxnDataBuildMechanism::Calculate(BaseDataSetOfInstances *instances,
				 DataSetOfInstancesClass *instancesclass,
				 BaseDataAlgorithmRun *run,
				 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  cout << "RxnDataBuildMechanism::Calculate" << endl;
  BuildMoleculeData(instances,instancesclass);
  BuildReactionData(instances,instancesclass);
  
  return result;
}
void RxnDataBuildMechanism::BuildMoleculeData(BaseDataSetOfInstances *instances,
		       DataSetOfInstancesClass *instancesclass) {
  ChemkinMoleculeList = new BaseDataKeyWords();
  BaseDataKeyWords *moleculeList = new BaseDataKeyWords(*MoleculeList);
  while(moleculeList->SizeOf() > 0) {
    String name = moleculeList->NextKey();
    //cout << "Molecule: " << name << endl;
    if(instances->InstanceInSet(name)) {
      BaseDataInstance *instance = instances->GetInstance(name);
      RxnDataSimpleMolecule *molecule = NULL;
      if(instance->IsInList(MechanismClass->getMoleculeClass()->GetNameInInstance())) {
	molecule = (RxnDataSimpleMolecule *) instance->GetObject(MechanismClass->getMoleculeClass()->GetNameInInstance());
      }
      if(molecule != NULL) {
	if(instance->IsInList(MOLECULE_SUMMARY_NAME)) {
	  RxnDataMoleculeSummary *summary = (RxnDataMoleculeSummary *) instance->GetObject(MOLECULE_SUMMARY_NAME);
	  summary->NameTag = name;
	  BaseDataString *chemkinname = (BaseDataString *) instance->GetObject(summary->ShortName);
	  ChemkinMoleculeList->AddKeyWord(chemkinname->getString());
	  molecule->StoreProperty(chemkinname);
	  Mechanism->addMoleculeSummary(summary);
	  summary->NameTag = MOLECULE_SUMMARY_NAME;
	  Mechanism->addMolecule(name);
	} else {
	  cerr << "Molecule Summary '" << MOLECULE_SUMMARY_NAME << "' not found in reaction instance '" << name << "'" << endl;
	}
      } else {
	cerr << "Molecule: '" << name << "' has not molecule element named '" 
	     << MechanismClass->getMoleculeClass()->GetNameInInstance() << "'" << endl;
      }
    } else {
      cerr << "Molecule: '" << name << "' not found in instances" << endl;
    }
  }
}
void RxnDataBuildMechanism::BuildReactionData(BaseDataSetOfInstances *instances,
					      DataSetOfInstancesClass *instancesclass) {
  while(ReactionList->SizeOf() > 0) {
    String name = ReactionList->NextKey();
    //cout << "Build Reaction: " << name << endl;
    if(instances->InstanceInSet(name)) {
      RxnDataReaction *rxn = NULL;
      BaseDataInstance *instance = instances->GetInstance(name);
      if(instance->IsInList(MechanismClass->getReactionClass()->getNameInInstance())) {
	rxn = (RxnDataReaction *) instance->GetObject(MechanismClass->getReactionClass()->getNameInInstance());
      }
      // Have to look at the CHEMKIN (from base) and full names (from generated)
      if(rxn->withinMoleculeSet(*ChemkinMoleculeList) || rxn->withinMoleculeSet(*MoleculeList) ) {
	if(instance->IsInList(REACTION_SUMMARY_NAME)) {
	  RxnDataReactionSummary *summary = (RxnDataReactionSummary *) instance->GetObject(REACTION_SUMMARY_NAME)->Clone();
	  summary->NameTag = name;
	  summary->setReactionName(name);
	  Mechanism->addReactionSummary(summary);
	  Mechanism->addReaction(name);
	  delete summary;
	} else {
	  cerr << "Reaction Summary '" << REACTION_SUMMARY_NAME << "' not found in reaction instance '" << name << "'" << endl;
	}
      } else {
	cout << "Reaction: " << name << "not included in build reaction set" << endl;
      }
    } else {
      cerr << "Reaction: '" << name << "' not found in instances" << endl;
    }
  }
  delete ChemkinMoleculeList;
  delete MoleculeList;
  ChemkinMoleculeList = NULL;
  MoleculeList = NULL;
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
bool RxnDataBuildMechanism::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  
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
bool RxnDataBuildMechanism::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 
 
 
/*S RxnBuildMechanismClass
 */
/*F RxnBuildMechanismClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnBuildMechanismClass::RxnBuildMechanismClass()
{
  Identification = MECHANISM_BUILD_ID;
  NameTag = MECHANISM_BUILD_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnBuildMechanismClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnBuildMechanismClass::RxnBuildMechanismClass(const RxnBuildMechanismClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnBuildMechanismClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnBuildMechanismClass::RxnBuildMechanismClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "BuildMechanism";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnBuildMechanismClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnBuildMechanismClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnBuildMechanismClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnBuildMechanismClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnBuildMechanismClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnBuildMechanismClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnBuildMechanismClass::CopyClone(Identify *  objc)
{
  RxnBuildMechanismClass *objcfull = (RxnBuildMechanismClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnBuildMechanismClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnBuildMechanismClass::Clone()
    {
      RxnBuildMechanismClass* id = new RxnBuildMechanismClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnBuildMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBuildMechanismClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnBuildMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBuildMechanismClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnBuildMechanismClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataBuildMechanism();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnBuildMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnBuildMechanismClass*& obj)
     {
     obj = new RxnBuildMechanismClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataBuildMechanism
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataBuildMechanism*& obj)
     {
     obj = new RxnDataBuildMechanism;
     return obj->DecodeThis(buffer);
     }

/*S MechanismSystemBase 
 */
/*F MechanismSystemBase(argc,argv) . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    argc,argv: The input arguments
**
**  REMARKS
**
*/
MechanismSystemBase::MechanismSystemBase(int argc, char *argv[])
  : RxnSystemBase(argc,argv)
{
}
/*F EncodeDecodeObjectsSetUp()  . . . . . . . . . . . set up molecule objects
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MechanismSystemBase::EncodeDecodeObjectsSetUp()
{
  RxnSystemBase::EncodeDecodeObjectsSetUp();
  InitialSetOfMechanismDecodeFunctions();
  InitialMechLumpingDecodeFunctions();
  InitialMechanismGraphDecodeFunctions();
}
/*F StandardObjectsSetUp()  . . . . . . . . . . . . . set up molecule objects
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MechanismSystemBase::StandardObjectsSetUp()
{
  RxnSystemBase::StandardObjectsSetUp();
  AddMechanismClasses(getStandard());
  AddMechLumpingClasses(getStandard());
  AddMechanismGraphClasses(getStandard());
}
/*F CommandSetUp()  . . . . . . . . . . . . . . . . . . . . .  InstanceSystem
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MechanismSystemBase::CommandSetUp()
{
  RxnSystemBase::CommandSetUp();

  SingleSystemCommand mechread("ReadMech",
			      "Read in a Mechanism Specification File",
			      &InputMechanism);
  Commands.AddObject(mechread.getName(),mechread);

  SingleSystemCommand rxnread("MechanismReactions",
			      "Read Reactions from Mechanism to Instances",
			      &MechanismReactionsAsInstances);
  Commands.AddObject(rxnread.getName(),rxnread);
}
/*F Initialization()  . . . . . . . . . . . . . . . . . .  MechanismSystemBase
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MechanismSystemBase::Initialization()
{
  RxnSystemBase::Initialization();
}
/*F ans = InputMechanism(sys)
**
**  DESCRIPTION
**    sys:

**  REMARKS
**
*/
int InputMechanism(ReactionSystemBase* sys)
{
  MechanismSystemBase *rxnsystem = (MechanismSystemBase *) sys;
  int result = 1;

  if(rxnsystem->Inputs.size() != 2)
    {
      cerr << "Inputs:" << endl;
      cerr << "         Type: Mechanisms, MechanismPatterns" << endl;
      cerr << "         File: The RxnFile" << endl;
    }
  String type = rxnsystem->GetNextInput();
  String mechfile = rxnsystem->GetNextInput();

  BaseDataDataBaseInformation *dbaseinfo = rxnsystem->MoleculeDataBase->getDatabaseInfo(type);
  DataDataBaseInformationClass *dbaseinfoclass = rxnsystem->MoleculeDataBaseClass->getMoleculeDBClass();
  if(dbaseinfo != NULL)
    {
      OpenInputFile file(mechfile);
      RxnMechanismClass* simpleclass = (RxnMechanismClass *)
	dbaseinfo->getDataElementClass(dbaseinfoclass);
      bool success = true;
      while(success)
	{
	  RxnDataMechanism *simple = (RxnDataMechanism *) simpleclass->BaseDataObjectExample();
	  cout << "Read Mechanism" << endl;
	  
	  success = simple->Read(file.Stream,simpleclass);
	  if(success)
	    {
	      BaseDataInstance *instance = new BaseDataInstance;
	      instance->NameTag = simple->NameTag;
	      rxnsystem->Instances.AddInstance(*instance);
	      delete instance;
	      instance = (BaseDataInstance *) rxnsystem->Instances.GetInstance(simple->NameTag);
	      simple->NameTag = simpleclass->getNameInInstance();
	      instance->AddObject(simple);
	      dbaseinfo->StoreElement(simple);
	    }
	  delete simple;
	}
    }
  else
    {
      cerr << "Database Information not found" << endl;
      result = false;
    }
  return result;
}
/*F ans = MechanismReactionsAsInstances(sys)
**
**  DESCRIPTION
**    sys:

**  REMARKS
**
*/
int MechanismReactionsAsInstances(ReactionSystemBase* sys)
{
  MechanismSystemBase *rxnsystem = (MechanismSystemBase *) sys;
  int result = 1;

  if(rxnsystem->Inputs.size() != 2)
    {
      cerr << "Inputs:" << endl;
      cerr << "         RxnType: Reactions, ReactionPatterns" << endl;
      cerr << "         MechClass: The name of the mechanism class" << endl;
      cerr << "         Mechanism: Name of the Mechanism (In instances)" << endl;
    }
  String rxntype = rxnsystem->GetNextInput();
  String mechClassS = rxnsystem->GetNextInput();
  String mechanismS = rxnsystem->GetNextInput();

  RxnDataMechanism *mechanism = NULL;
  if(rxnsystem->InstanceClasses.IsInList(mechClassS))
    {
      RxnMechanismClass *mechclass = (RxnMechanismClass *) rxnsystem->InstanceClasses.GetObjectClass(mechClassS);
      String mechname = mechclass->getNameInInstance();

      if(rxnsystem->Instances.InstanceInSet(mechanismS))
	{
	  BaseDataInstance *instance = rxnsystem->Instances.GetInstance(mechanismS);
	  if(instance->IsInList(mechname))
	    {
	      mechanism = (RxnDataMechanism *) instance->GetObject(mechname);
	    }
	  else
	      cerr << "Mechanism: '" << mechname << "' not in instance '" << mechanismS << "'" << endl;
	}
      else
	cerr << "Instance not found: '" << mechanismS << "'" << endl;
    }
  else
    cerr << "Mechanism Class '" << mechClassS << "' not found" << endl;

      
  BaseDataDataBaseInformation *rxndbaseinfo = rxnsystem->MoleculeDataBase->getDatabaseInfo(rxntype);
  DataDataBaseInformationClass *dbaseinfoclass = rxnsystem->MoleculeDataBaseClass->getMoleculeDBClass();

  if(mechanism && rxndbaseinfo != NULL)
    {
      RxnReactionClass* rxnclass = (RxnReactionClass *)
	rxndbaseinfo->getDataElementClass(dbaseinfoclass);
      String rxnname = rxnclass->getNameInInstance();
      ObjectList<String> names = mechanism->ListOfObjectNames();
      ObjectList<String>::iterator name;
      bool success = true;
      for(name = names.begin(); success && name != names.end(); name++)
	{
	  RxnDataReaction *rxn = (RxnDataReaction *) rxnclass->BaseDataObjectExample();
	  success = rxndbaseinfo->FetchElement(*name,dbaseinfoclass,(BaseDataObject *&) rxn);
	  if(success)
	    {
	      //cout << "Reaction: '" << rxn->NameTag << "'" << endl;
	      if(!rxnsystem->Instances.InstanceInSet(*name))
		{
		  BaseDataInstance *newinstance = new BaseDataInstance;
		  newinstance->NameTag = *name;
		  rxnsystem->Instances.AddInstance(*newinstance);
		  delete newinstance;
		}
	      BaseDataInstance *instance = (BaseDataInstance *) rxnsystem->Instances.GetInstance(*name);
	      rxn->NameTag = rxnname;
	      instance->AddObject(rxn);
	      delete rxn;
	    }
	  else
	    {
	      cerr << "Failed to Fetch Element: " << rxnname << endl;
	    }
	}
    }
  else
    {
      cerr << "Mechanism or Database Information not found" << endl;
      result = 0;
    }
  return result;
}

/*S Utilities
 */
 
/*F InitialSetOfMechanismDecodeFunctions()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialSetOfMechanismDecodeFunctions()
{
  EncodeDecodeRegisterClass(RxnMechanismDataBaseClass,RxnDataMechanismDataBase,MECHANISM_DBASE_NAME);
  EncodeDecodeRegisterClass(RxnReactionSummaryClass,RxnDataReactionSummary,MECHANISM_SUMMARY_NAME);
  EncodeDecodeRegisterClass(RxnMechanismClass,RxnDataMechanism,MECHANISM_MECHANISM_NAME);
  EncodeDecodeRegisterClass(RxnPrintOutMechanismClass,RxnDataPrintOutMechanism,MECHANISM_PRINT_NAME);
  EncodeDecodeRegisterClass(RxnGetGeneratedMechanismClass,RxnDataGetGeneratedMechanism,MECHANISM_GENERATED_NAME);
  EncodeDecodeRegisterClass(RxnFillMechanismClass,RxnDataFillMechanism,MECHANISM_FILLALG_NAME);
  EncodeDecodeRegisterClass(RxnMoleculeSummaryClass,RxnDataMoleculeSummary,MECHANISM_MOLECULE_NAME);
  EncodeDecodeRegisterClass(RxnMoleculeSummarySetClass,RxnDataMoleculeSummarySet,MECHANISM_MOLSUMSET_NAME);
  EncodeDecodeRegisterClass(RxnBuildMechanismClass,RxnDataBuildMechanism,MECHANISM_BUILD_NAME);
}
/*F AddMechanismClasses(set)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void AddMechanismClasses(DataSetOfObjectsClass& set)
{
  String dbasedescr("The Mechanism Database Class");
  RxnMechanismDataBaseClass dbaseclass(MECHANISM_DBASE_ID,MECHANISM_DBASE_NAME,dbasedescr);
  set.AddObjectClass(dbaseclass);

  String mechdescr("The Mechanism Class");
  RxnMechanismClass mechclass(MECHANISM_MECHANISM_ID,MECHANISM_MECHANISM_NAME,mechdescr);
  set.AddObjectClass(mechclass);

  String sumdescr("The Class");
  RxnReactionSummaryClass sumclass(MECHANISM_SUMMARY_ID,MECHANISM_SUMMARY_NAME,sumdescr);
  set.AddObjectClass(sumclass);

  String printdescr("The Print Out the Mechanism");
  RxnPrintOutMechanismClass printclass(MECHANISM_PRINT_ID,MECHANISM_PRINT_NAME,printdescr);
  set.AddObjectClass(printclass);

  String gendescr("The Read Generated Mechanism Class");
  RxnGetGeneratedMechanismClass genclass(MECHANISM_GENERATED_ID,MECHANISM_GENERATED_NAME,gendescr);
  set.AddObjectClass(genclass);

  String mfilldescr("The Fill instances with reactions from mechanism Class");
  RxnFillMechanismClass mfillclass(MECHANISM_FILLALG_ID,MECHANISM_FILLALG_NAME,mfilldescr);
  set.AddObjectClass(mfillclass);

  String molsummarydescr("The Class");
  RxnMoleculeSummaryClass molsummaryclass(MECHANISM_MOLECULE_ID,MECHANISM_MOLECULE_NAME,molsummarydescr);
  set.AddObjectClass(molsummaryclass);

  String molsumsetdescr("The Molecule Summary Set Class");
  RxnMoleculeSummarySetClass molsumsetclass(MECHANISM_MOLSUMSET_ID,MECHANISM_MOLSUMSET_NAME,molsumsetdescr);
  set.AddObjectClass(molsumsetclass);

  String builddescr("The Mechanism Build Class");
  RxnBuildMechanismClass buildclass(MECHANISM_BUILD_ID,MECHANISM_BUILD_NAME,builddescr);
  set.AddObjectClass(buildclass);
}
