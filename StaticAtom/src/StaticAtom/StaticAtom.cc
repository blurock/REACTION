/*  FILE     StaticAtom.cc
**  PACKAGE  StaticAtom
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "StaticAtom" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "CoreDataObjects.hh"
#include "NumericObjects.hh"
#include "LogicalObjects.hh"
#include "Vector.hh"
#include "StaticAtom.hh"

/*S RxnDataStaticAtomInfo
 */ 
/*F RxnDataStaticAtomInfo()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataStaticAtomInfo::RxnDataStaticAtomInfo()
  : AtomicNumber(0)
{
  Identification = STATICATOM_ATOMINFO_ID;
  NameTag = STATICATOM_ATOMINFO_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;

  String title("\n ---------- Static Atom Information ---------- \n");
  setTitle(title);
  String delim("\n");
  setDelimitor(delim);

} 
/*F RxnDataStaticAtomInfo(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataStaticAtomInfo::RxnDataStaticAtomInfo(const RxnDataStaticAtomInfo& data)
  : BaseDataSetOfObjects(data),
    AtomicNumber(data.AtomicNumber)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataStaticAtomInfo::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataStaticAtomInfo::Read(istream& in, DataObjectClass* objc, const String& name)
{
  StreamObjectInput str(in,' ');
  String atnS = str.ReadNext();
  AtomicNumber = atnS.ToInteger();
  bool result = BaseDataSetOfObjects::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataStaticAtomInfo::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  out << "Atomic Number: " << AtomicNumber << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataStaticAtomInfo::Clone()
{
  RxnDataStaticAtomInfo *obj = new RxnDataStaticAtomInfo(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataStaticAtomInfo::CopyClone(Identify * obj)
{
  RxnDataStaticAtomInfo *objfull = (RxnDataStaticAtomInfo *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataStaticAtomInfo::EncodeThis(CommBuffer& buffer)
{
  bool result = true;
  result = result && Encode(buffer,AtomicNumber);
  result = result && BaseDataSetOfObjects::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataStaticAtomInfo::DecodeThis(CommBuffer& buffer)
{
  bool result = true;
  result = result && Decode(buffer,AtomicNumber);
  result = result && BaseDataSetOfObjects::DecodeThis(buffer);
  return result;
}
/*F ans = operator==(y) . . . . . . . . . . . . . . StaticAtomInfo Equality
**
**  DESCRIPTION
**    y: The info to compare
**    ans: true if equal 
**
**    If either atomic number is non-zero, then equality is done with
**    AtomicNumber, otherwise, the atom name is compared (string equality).
**
**  REMARKS
**
*/
bool RxnDataStaticAtomInfo::operator==(const RxnDataStaticAtomInfo& y)
{
  if(AtomicNumber == 0 || y.AtomicNumber == 0)
    {
      if(Identification == y.Identification)
	return true;
      else
	return false;
    }
  else
    return(AtomicNumber == y.AtomicNumber);
}
 
/*F atn = getAtomicNumber() . . . . . . . . . . . . . . RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    atn: The atomic number
**
**  REMARKS
**
*/
unsigned int RxnDataStaticAtomInfo::getAtomicNumber()
{
  return AtomicNumber;
}
/*F setAtomicNumber(atn)  . . . . . . . . . . . . . . . RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    atn: The atomic number
**
**  REMARKS
**
*/
void RxnDataStaticAtomInfo::setAtomicNumber(unsigned int atn)
{
  AtomicNumber = atn;
}
/*S RxnStaticAtomInfoClass
 */
/*F RxnStaticAtomInfoClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnStaticAtomInfoClass::RxnStaticAtomInfoClass()
{
  Identification = STATICATOM_ATOMINFO_ID;
  NameTag = STATICATOM_ATOMINFO_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;

  ReadClassPairs = true;
  ReadAllowedClasses = false;
} 
/*F RxnStaticAtomInfoClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnStaticAtomInfoClass::RxnStaticAtomInfoClass(const RxnStaticAtomInfoClass& data)
  : DataSetOfObjectsClass(data)
{
} 
 
/*F RxnStaticAtomInfoClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnStaticAtomInfoClass::RxnStaticAtomInfoClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = STATICATOM_ATOMINFO_NAME;

  ReadClassPairs = true;
  ReadAllowedClasses = false;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnStaticAtomInfoClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnStaticAtomInfoClass::print(ostream& out) const
{
  DataSetOfObjectsClass::print(out);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnStaticAtomInfoClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnStaticAtomInfoClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnStaticAtomInfoClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataSetOfObjectsClass::Read(in,set);
  return result;
} 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnStaticAtomInfoClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnStaticAtomInfoClass::CopyClone(Identify *  objc)
{
  RxnStaticAtomInfoClass *objcfull = (RxnStaticAtomInfoClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnStaticAtomInfoClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnStaticAtomInfoClass::Clone()
    {
      RxnStaticAtomInfoClass* id = new RxnStaticAtomInfoClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnStaticAtomInfoClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnStaticAtomInfoClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnStaticAtomInfoClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnStaticAtomInfoClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::DecodeThis(buffer);
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
BaseDataObject *RxnStaticAtomInfoClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataStaticAtomInfo();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnStaticAtomInfoClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnStaticAtomInfoClass*& obj)
     {
     obj = new RxnStaticAtomInfoClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataStaticAtomInfo
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataStaticAtomInfo*& obj)
     {
     obj = new RxnDataStaticAtomInfo;
     return obj->DecodeThis(buffer);
     }
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnStaticAtomInfoClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S RxnDataMetaAtomData
 */ 
/*F RxnDataMetaAtomData()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMetaAtomData::RxnDataMetaAtomData()
{
  Identification = STATICATOM_METADATA_ID;
  NameTag = STATICATOM_METADATA_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMetaAtomData(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMetaAtomData::RxnDataMetaAtomData(const RxnDataMetaAtomData& data)
  : BaseDataNValued(data),
    AtomName(data.AtomName)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMetaAtomData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMetaAtomData::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMetaAtomData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMetaAtomData::Read(istream& in, DataObjectClass* objc, const String& name)
{
  StreamObjectInput str(in,' ');
  AtomName = str.ReadNext();
  bool result = BaseDataNValued::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMetaAtomData
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMetaAtomData::print(ostream& out) const
{
  BaseDataNValued::print(out);
  out << endl << "AtomName: " << AtomName << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMetaAtomData
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMetaAtomData::Clone()
{
  RxnDataMetaAtomData *obj = new RxnDataMetaAtomData(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMetaAtomData
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMetaAtomData::CopyClone(Identify * obj)
{
  RxnDataMetaAtomData *objfull = (RxnDataMetaAtomData *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMetaAtomData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMetaAtomData::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataNValued::EncodeThis(buffer);
  result = result && Encode(buffer,AtomName);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMetaAtomData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMetaAtomData::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataNValued::DecodeThis(buffer);
  result = result && Decode(buffer,AtomName);
  return result;
}
/*S RxnMetaAtomDataClass
 */
/*F RxnMetaAtomDataClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMetaAtomDataClass::RxnMetaAtomDataClass()
{
  Identification = STATICATOM_METADATA_ID;
  NameTag = STATICATOM_METADATA_NAME;
  SubClass = "NValued";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMetaAtomDataClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMetaAtomDataClass::RxnMetaAtomDataClass(const RxnMetaAtomDataClass& data)
  : DataNValuedClass(data)
{
} 
 
/*F RxnMetaAtomDataClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMetaAtomDataClass::RxnMetaAtomDataClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataNValuedClass(id,name,descr)
{
  SubClass = "NValued";
  EncodeDecodeClass = STATICATOM_METADATA_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMetaAtomDataClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMetaAtomDataClass::print(ostream& out) const
{
  DataNValuedClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMetaAtomDataClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMetaAtomDataClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMetaAtomDataClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataNValuedClass::Read(in,set);
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMetaAtomDataClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMetaAtomDataClass::CopyClone(Identify *  objc)
{
  RxnMetaAtomDataClass *objcfull = (RxnMetaAtomDataClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMetaAtomDataClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMetaAtomDataClass::Clone()
    {
      RxnMetaAtomDataClass* id = new RxnMetaAtomDataClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMetaAtomDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMetaAtomDataClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataNValuedClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMetaAtomDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMetaAtomDataClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataNValuedClass::DecodeThis(buffer);
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
BaseDataObject * RxnMetaAtomDataClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMetaAtomData();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMetaAtomDataClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMetaAtomDataClass*& obj)
     {
     obj = new RxnMetaAtomDataClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMetaAtomData
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMetaAtomData*& obj)
     {
     obj = new RxnDataMetaAtomData;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataCompleteMetaAtom
 */ 
/*F RxnDataCompleteMetaAtom()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataCompleteMetaAtom::RxnDataCompleteMetaAtom()
  : Index(0)
{
  Identification = STATICATOM_METAATOM_ID;
  NameTag = STATICATOM_METAATOM_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataCompleteMetaAtom(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataCompleteMetaAtom::RxnDataCompleteMetaAtom(const RxnDataCompleteMetaAtom& data)
  : BaseDataSetOfObjects(data),
    Index(data.Index)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataCompleteMetaAtom
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataCompleteMetaAtom::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataCompleteMetaAtom
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataCompleteMetaAtom::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = true;
  RxnCompleteMetaAtomClass *atomclass = (RxnCompleteMetaAtomClass *) objc;
  RxnMetaAtomDataClass *metaclass = (RxnMetaAtomDataClass *) atomclass->getMetaAtomClass();

  String metaatomS("MetaAtom:");
  CheckReadKeyWord(in,metaatomS);

  StreamObjectInput str(in,' ');
  String indexS = str.ReadNext();
  Index = indexS.ToInteger();
  
  String nextS = str.ReadNext();
  while(!(nextS == "END") && result)
    {
      RxnDataMetaAtomData *meta = (RxnDataMetaAtomData *) metaclass->BaseDataObjectExample();
      meta->NameTag = nextS;
      result = result && meta->Read(in,metaclass,nextS);

      AddObject(meta);
      delete meta;

      nextS = str.ReadNext();
    }

  return result;
} 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataCompleteMetaAtom
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataCompleteMetaAtom::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);

  out << "Meta Atom Index: " << Index << endl;

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataCompleteMetaAtom
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataCompleteMetaAtom::Clone()
{
  RxnDataCompleteMetaAtom *obj = new RxnDataCompleteMetaAtom(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataCompleteMetaAtom
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataCompleteMetaAtom::CopyClone(Identify * obj)
{
  RxnDataCompleteMetaAtom *objfull = (RxnDataCompleteMetaAtom *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataCompleteMetaAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataCompleteMetaAtom::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  result = result && Encode(buffer,Index);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataCompleteMetaAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataCompleteMetaAtom::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  result = result && Decode(buffer,Index);
  return result;
}
/*S RxnCompleteMetaAtomClass
 */
/*F RxnCompleteMetaAtomClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnCompleteMetaAtomClass::RxnCompleteMetaAtomClass()
  : MetaAtomClass(NULL)
{
  Identification = STATICATOM_METAATOM_ID;
  NameTag = STATICATOM_METAATOM_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
} 
/*F RxnCompleteMetaAtomClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnCompleteMetaAtomClass::RxnCompleteMetaAtomClass(const RxnCompleteMetaAtomClass& data)
  : DataSetOfObjectsClass(data)
{
  MetaAtomClass = (RxnMetaAtomDataClass *) PointerClone(data.MetaAtomClass);
} 
/*F RxnCompleteMetaAtomClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnCompleteMetaAtomClass::RxnCompleteMetaAtomClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr),
    MetaAtomClass(NULL)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = STATICATOM_METAATOM_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnCompleteMetaAtomClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnCompleteMetaAtomClass::print(ostream& out) const
{
  DataSetOfObjectsClass::print(out);
  PointerPrint(out,"  The Meta Atom Class: "," No Class Defined ",MetaAtomClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnCompleteMetaAtomClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnCompleteMetaAtomClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnCompleteMetaAtomClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = PointerClassRead(in,(DataObjectClass *&) MetaAtomClass,
				 STATICATOM_METADATA_NAME,
				 set," No Class ");
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnCompleteMetaAtomClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnCompleteMetaAtomClass::CopyClone(Identify *  objc)
{
  RxnCompleteMetaAtomClass *objcfull = (RxnCompleteMetaAtomClass *) objc;
  *this = *objcfull;
  MetaAtomClass = (RxnMetaAtomDataClass *) PointerClone(objcfull->MetaAtomClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnCompleteMetaAtomClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnCompleteMetaAtomClass::Clone()
{
  RxnCompleteMetaAtomClass* id = new RxnCompleteMetaAtomClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnCompleteMetaAtomClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnCompleteMetaAtomClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,MetaAtomClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnCompleteMetaAtomClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnCompleteMetaAtomClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) MetaAtomClass);
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
BaseDataObject * RxnCompleteMetaAtomClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataCompleteMetaAtom();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnCompleteMetaAtomClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnCompleteMetaAtomClass*& obj)
     {
     obj = new RxnCompleteMetaAtomClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataCompleteMetaAtom
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataCompleteMetaAtom*& obj)
     {
     obj = new RxnDataCompleteMetaAtom;
     return obj->DecodeThis(buffer);
     }
 
/*Fmatomclass = getMetaAtomClass()
**
**  DESCRIPTION
**    matomclass: The class information of the meta atom data
**
**  REMARKS
**
*/
RxnMetaAtomDataClass *RxnCompleteMetaAtomClass::getMetaAtomClass()
{
  return MetaAtomClass;
}
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnCompleteMetaAtomClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S RxnDataAtomInformation
 */ 
/*F RxnDataAtomInformation()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataAtomInformation::RxnDataAtomInformation()
  : AtomicNumberToName(NULL)
{
  Identification = STATICATOM_ATOMINFOSET_ID;
  NameTag = STATICATOM_ATOMINFOSET_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataAtomInformation(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataAtomInformation::RxnDataAtomInformation(const RxnDataAtomInformation& data)
  : BaseDataSetOfObjects(data),
    MetaAtoms(data.MetaAtoms)
{
  if(data.AtomicNumberToName != NULL)
    {
      AtomicNumberToName = new VectorSimple<String>(*(data.AtomicNumberToName));
    }
  else
    {
      AtomicNumberToName = NULL;
    }
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataAtomInformation
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataAtomInformation::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataAtomInformation
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataAtomInformation::Read(istream& in, DataObjectClass* objc, const String& name)
{
  RxnAtomInformationClass *infoclass = (RxnAtomInformationClass *) objc;
  bool result = true;
  String aname,atnS;
  RxnStaticAtomInfoClass *staticinfoclass = infoclass->getStaticAtomInfoClass();
  RxnCompleteMetaAtomClass *metaatomclass = infoclass->getCompleteMetaAtomClass();
  if(AtomicNumberToName != NULL)
    delete AtomicNumberToName;
  AtomicNumberToName = new VectorSimple<String>(MAX_POSSIBLE_ATOMIC_NUMBER);

  RxnDataStaticAtomInfo *next;

  StreamObjectInput str(in,' ');
  aname = str.ReadNext();
  while(!(aname == "END") && result)
    {
      next = (RxnDataStaticAtomInfo *) staticinfoclass->BaseDataObjectExample();
      next->NameTag = aname;
      result = result && next->Read(in,staticinfoclass,aname);
      
      (*AtomicNumberToName)[next->getAtomicNumber()] = aname;

      AddObject(next);
      delete next;

      aname = str.ReadNext();
    }
  cout << "The Atoms Read in:" << endl;
  AtomicNumberToName->print(cout);
  cout << endl;

  aname = str.ReadNext();
  while(!(aname == "END") && result)
    {
      next = (RxnDataStaticAtomInfo *) metaatomclass->BaseDataObjectExample();
      next->NameTag = aname;
      result = result && next->Read(in,metaatomclass,aname);

      AddObject(next);
      delete next;

      aname = str.ReadNext();
    }

  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataAtomInformation
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataAtomInformation::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataAtomInformation
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataAtomInformation::Clone()
{
  RxnDataAtomInformation *obj = new RxnDataAtomInformation(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataAtomInformation
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataAtomInformation::CopyClone(Identify * obj)
{
  RxnDataAtomInformation *objfull = (RxnDataAtomInformation *) obj;
  *this = *objfull;
  if(objfull->AtomicNumberToName != NULL)
    {
      AtomicNumberToName = new VectorSimple<String>(*(objfull->AtomicNumberToName));
    }
  else
    {
      AtomicNumberToName = NULL;
    }
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataAtomInformation
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataAtomInformation::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  if(AtomicNumberToName != NULL)
    {
      String dummy("Vector<String>");
      result = result && Encode(buffer,dummy);
      result = result && AtomicNumberToName->EncodeThis(buffer);
    }
  else
    result = result && Encode(buffer,NoStructure);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataAtomInformation
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataAtomInformation::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  String name;
  result = result && Decode(buffer,name);
  if(name != NoStructure)
    {
      AtomicNumberToName = new VectorSimple<String>();
      AtomicNumberToName->DecodeThis(buffer);
    }

  return result;
}
 
/*F atn = AtomicNumberFromSymbol(symbol)  . . . . . .  RxnDataAtomInformation
**
**  DESCRIPTION
**    symbol: The atom name
**    atn: The corresponding atomic number
**
**  REMARKS
**
*/
unsigned int RxnDataAtomInformation::AtomicNumberFromSymbol(String& symbol)
{
  unsigned int atn = 0;
  if(IsInList(symbol)) {
    RxnDataStaticAtomInfo *stat = (RxnDataStaticAtomInfo *) GetObject(symbol);
    atn = stat->getAtomicNumber();
  } else {
    char trans[symbol.size()];
    trans[0] = (char) toupper(*(symbol.c_str()));
    if(symbol.size() > 1)
      trans[1] = tolower(*(symbol.c_str()+1));
    
    String t(trans);
    if(IsInList(t)) {
      RxnDataStaticAtomInfo *stat = (RxnDataStaticAtomInfo *) GetObject(t);
      atn = stat->getAtomicNumber();      
    } else {
      cerr << "ERROR: Atomic Symbol Not found: '" << symbol << "' (or '" << t << "')" << endl;
    }
  }
  return atn;
}
/*Fsymbol = AtomNameFromAtomicNumber(atn) . . . . . .  RxnDataAtomInformation
**
**  DESCRIPTION
**    symbol: The atom name
**    atn: The corresponding atomic number
**    
**  REMARKS
**
*/
String RxnDataAtomInformation::AtomNameFromAtomicNumber(unsigned int atn)
{
  return (*AtomicNumberToName)[atn];
}
/*F atn = FindMetaAtomSymbol(symbol)  . . . . . . . .  RxnDataAtomInformation
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
unsigned int RxnDataAtomInformation::FindMetaAtomSymbol(String& symbol)
{
  unsigned int meta = 0;
  if(MetaAtoms.IsInList(symbol))
    {
      RxnDataCompleteMetaAtom *metaatom = (RxnDataCompleteMetaAtom *) MetaAtoms.GetObject(symbol);
      meta = metaatom->Index;
    }
  return meta;
}
/*Fradius = ReadCovalentRadius(atomicnumber)  . . . .  RxnDataAtomInformation
**
**  DESCRIPTION
**    atomicnumber: The atomic number
**    radius: The covalent radius
**
**  REMARKS
**
*/
double RxnDataAtomInformation::ReadCovalentRadius(unsigned int atomicnumber)
{
  double radius = 0.0;
  String symbol = AtomNameFromAtomicNumber(atomicnumber);
  if(IsInList(symbol))
    {
      RxnDataStaticAtomInfo *atominfo = (RxnDataStaticAtomInfo *) GetObject(symbol);
      if(atominfo->IsInList("CovalentRadius"))
	{
	  BaseDataReal *r = (BaseDataReal *) atominfo->GetObject("CovalentRadius");
	  radius = r->GetValue();
	}
    }
  return radius;
}
/*S RxnAtomInformationClass
 */
/*F RxnAtomInformationClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnAtomInformationClass::RxnAtomInformationClass()
  : StaticAtomInfoClass(NULL),
    CompleteMetaAtomClass(NULL)
{
  Identification = STATICATOM_ATOMINFOSET_ID;
  NameTag = STATICATOM_ATOMINFOSET_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;

  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnAtomInformationClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnAtomInformationClass::RxnAtomInformationClass(const RxnAtomInformationClass& data)
  : DataSetOfObjectsClass(data)
{
  StaticAtomInfoClass = (RxnStaticAtomInfoClass *) PointerClone(data.StaticAtomInfoClass);
  CompleteMetaAtomClass = (RxnCompleteMetaAtomClass *) PointerClone(data.CompleteMetaAtomClass);
}  
/*F RxnAtomInformationClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnAtomInformationClass::RxnAtomInformationClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr),
    StaticAtomInfoClass(NULL),
    CompleteMetaAtomClass(NULL)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = STATICATOM_ATOMINFOSET_NAME;

  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnAtomInformationClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  cout << endl;
  PointerPrint(out,"StaticAtomClass"," not defined ",StaticAtomInfoClass);
  PointerPrint(out,"MetaAtomClass"," not defined ",CompleteMetaAtomClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnAtomInformationClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnAtomInformationClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  cout << "RxnAtomInformationClass::Read" << endl;
  //bool result = DataSetOfObjectsClass::Read(in,set);
  bool result = PointerClassRead(in,(DataObjectClass *&) StaticAtomInfoClass,
				      STATICATOM_ATOMINFO_NAME,
				      set," No Class ");
  result = result && PointerClassRead(in,(DataObjectClass *&) CompleteMetaAtomClass,
				      STATICATOM_METAATOM_NAME,
				      set," No Class ");
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnAtomInformationClass::CopyClone(Identify *  objc)
{
  RxnAtomInformationClass *objcfull = (RxnAtomInformationClass *) objc;
  *this = *objcfull;
  StaticAtomInfoClass   = (RxnStaticAtomInfoClass *)   PointerClone(objcfull->StaticAtomInfoClass);
  CompleteMetaAtomClass = (RxnCompleteMetaAtomClass *) PointerClone(objcfull->CompleteMetaAtomClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnAtomInformationClass::Clone()
{
  RxnAtomInformationClass* id = new RxnAtomInformationClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnAtomInformationClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,StaticAtomInfoClass);
  result = result && PointerEncode(buffer,CompleteMetaAtomClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnAtomInformationClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) StaticAtomInfoClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) CompleteMetaAtomClass);
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
BaseDataObject * RxnAtomInformationClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataAtomInformation();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnAtomInformationClass*& obj)
     {
     obj = new RxnAtomInformationClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataAtomInformation
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataAtomInformation*& obj)
     {
     obj = new RxnDataAtomInformation;
     return obj->DecodeThis(buffer);
     }
 
/*F sclass = getStaticAtomInfoClass() . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    sclass: The static information class
**
**  REMARKS
**
*/
RxnStaticAtomInfoClass *RxnAtomInformationClass::getStaticAtomInfoClass()
{
  return StaticAtomInfoClass;
}
/*F sclass = getCompleteMetaAtomClass() . . . . . . . . RxnAtomInformationClass
**
**  DESCRIPTION
**    sclass: The meta  information class
**
**  REMARKS
**
*/
RxnCompleteMetaAtomClass *RxnAtomInformationClass::getCompleteMetaAtomClass()
{
  return CompleteMetaAtomClass;
}
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnAtomInformationClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S StaticAtomUtilities
 */
 
/*F AddStaticAtomClasses(set) . . . . . . . . . . . . . . StaticAtom Registry
**
**  DESCRIPTION
**    set: The set of Classes to add StaticAtom classes to
**
**  REMARKS
**
*/
void AddStaticAtomClasses(DataSetOfObjectsClass& set)
{
  String infodescr("The Static Atom Information Class");
  RxnStaticAtomInfoClass infoclass(STATICATOM_ATOMINFO_ID,STATICATOM_ATOMINFO_NAME,infodescr);
  set.AddObjectClass(infoclass);

  String atomsdescr("The Set of Static Atoms and Meta Atoms Class");
  RxnAtomInformationClass atomsclass(STATICATOM_ATOMINFOSET_ID,STATICATOM_ATOMINFOSET_NAME,atomsdescr);
  set.AddObjectClass(atomsclass);

  String metaddescr("The Meta Atom Information");
  RxnMetaAtomDataClass metadclass(STATICATOM_METADATA_ID,STATICATOM_METADATA_NAME,metaddescr);
  set.AddObjectClass(metadclass);

  String metaadescr("The Complete Meta Atom Class");
  RxnCompleteMetaAtomClass metaaclass(STATICATOM_METAATOM_ID,STATICATOM_METAATOM_NAME,metaadescr);
  set.AddObjectClass(metaaclass);
}
/*F InitialSetOfStaticAtomEncodeDecodeRoutines() . . . . . . . . set of routines
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void InitialSetOfStaticAtomEncodeDecodeRoutines()
{
  EncodeDecodeRegisterClass(RxnStaticAtomInfoClass,RxnDataStaticAtomInfo,STATICATOM_ATOMINFO_NAME);
  EncodeDecodeRegisterClass(RxnMetaAtomDataClass,RxnDataMetaAtomData,STATICATOM_METADATA_NAME);
  EncodeDecodeRegisterClass(RxnCompleteMetaAtomClass,RxnDataCompleteMetaAtom,STATICATOM_METAATOM_NAME);
  EncodeDecodeRegisterClass(RxnAtomInformationClass,RxnDataAtomInformation,STATICATOM_ATOMINFOSET_NAME);
}
