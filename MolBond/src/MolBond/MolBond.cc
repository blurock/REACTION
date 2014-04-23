/*  FILE     MolBond.cc
**  PACKAGE  MolBond
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "MolBond" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
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
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"

/*S RxnDataBasicBondData
 */ 
/*F RxnDataBasicBondData()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataBasicBondData::RxnDataBasicBondData()
  : BondOrder(0),
    DeltaCharge(0.0),
    DeltaElectronegativity(0.0)
{
  Identification = MOLBOND_BASIC_ID;
  NameTag = MOLBOND_BASIC_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataBasicBondData(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataBasicBondData::RxnDataBasicBondData(const RxnDataBasicBondData& data)
  : BaseDataObject(data),
    BondOrder(data.BondOrder),
    DeltaCharge(data.DeltaCharge),
    DeltaElectronegativity(data.DeltaElectronegativity)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataBasicBondData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataBasicBondData::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataBasicBondData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataBasicBondData::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataObject::Read(in,objc,name);
  StreamObjectInput str(in,' ');
  String BondOrderS = str.ReadNext();
  BondOrder = BondOrderS.ToInteger();
  String DeltaChargeS = str.ReadNext();
  DeltaCharge = DeltaChargeS.ToInteger();
  String DeltaElectronegativityS = str.ReadNext();
  DeltaElectronegativity = DeltaElectronegativityS.ToInteger();

  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataBasicBondData
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataBasicBondData::print(ostream& out) const
{
  BaseDataObject::print(out);
  out << "BondOrder[" << BondOrder << "], ";
  out << "Charge[" << DeltaCharge << "], ";
  out << "Electronegativity[" << DeltaElectronegativity << "]" << endl;

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataBasicBondData
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataBasicBondData::Clone()
{
  RxnDataBasicBondData *obj = new RxnDataBasicBondData(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataBasicBondData
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataBasicBondData::CopyClone(Identify * obj)
{
  RxnDataBasicBondData *objfull = (RxnDataBasicBondData *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataBasicBondData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBasicBondData::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::EncodeThis(buffer);
  result = result && Encode(buffer,BondOrder);
  result = result && Encode(buffer,DeltaCharge);
  result = result && Encode(buffer,DeltaElectronegativity);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataBasicBondData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBasicBondData::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::DecodeThis(buffer);
  result = result && Decode(buffer,BondOrder);
  result = result && Decode(buffer,DeltaCharge);
  result = result && Decode(buffer,DeltaElectronegativity);
  return result;
}
 
/*F ans = WriteAsLine(out,objc) . . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object one one line
**
**  REMARKS
**
*/
bool RxnDataBasicBondData::WriteAsLine(ostream& out, DataObjectClass *objc)
{
  out << "(" << BondOrder << ") with ";
  out << "E(" << DeltaElectronegativity << ")  C(" << DeltaCharge << ")";
  return true;
}
/*F ans = WriteAsASCII(out,objc)  . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object as complement to input
**
**  REMARKS
**
*/
bool RxnDataBasicBondData::WriteAsASCII(ostream& out, DataObjectClass *objc)
{
  bool result = true;
  out << " " << BondOrder;
  out << " " << DeltaCharge;
  out << " " << DeltaElectronegativity << endl;
  return result;
}
/*F ans = WriteAsLatex(out,objc)  . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object prepared for latex
**
**  REMARKS
**
*/
bool RxnDataBasicBondData::WriteAsLatex(ostream& out, DataObjectClass* objc)
{
  bool result = true;
  out << " " << BondOrder << " \\> ";
  out << " " << DeltaCharge << " \\> ";
  out << " " << DeltaElectronegativity << " \\> ";
  out << " \\\\" << endl;
  
  return result;
}
 
/*F ans = StoreAsProperties(props)  . . . . . . . . . . store standard values
**
**  DESCRIPTION
**    props: Where to store the bond properties
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataBasicBondData::StoreAsProperties(BaseDataSetOfObjects *props)
{
  bool result = true;
  BaseDataInteger *p1 = new BaseDataInteger();
  p1->NameTag = "BondOrder";
  p1->SetValue(BondOrder);
  props->AddObject(p1);
  delete p1;

  BaseDataReal *p2 = new BaseDataReal();
  p2->NameTag = "DeltaCharge";
  p2->SetValue(DeltaCharge);
  props->AddObject(p2);
  delete p2;

  BaseDataReal *p3 = new BaseDataReal();
  p3->NameTag = "DeltaElectronegativity";
  p3->SetValue(DeltaElectronegativity);
  props->AddObject(p3);
  delete p3;
  return result;
}

/*S RxnBasicBondDataClass
 */
/*F RxnBasicBondDataClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnBasicBondDataClass::RxnBasicBondDataClass()
{
  Identification = MOLBOND_BASIC_ID;
  NameTag = MOLBOND_BASIC_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
} 
/*F RxnBasicBondDataClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnBasicBondDataClass::RxnBasicBondDataClass(const RxnBasicBondDataClass& data)
  : DataObjectClass(data)
{
} 
 
/*F RxnBasicBondDataClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnBasicBondDataClass::RxnBasicBondDataClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataObjectClass(id,name,descr)
{
  SubClass = "Object";
  EncodeDecodeClass = MOLBOND_BASIC_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnBasicBondDataClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnBasicBondDataClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnBasicBondDataClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnBasicBondDataClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnBasicBondDataClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataObjectClass::Read(in,set);
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnBasicBondDataClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnBasicBondDataClass::CopyClone(Identify *  objc)
{
  RxnBasicBondDataClass *objcfull = (RxnBasicBondDataClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnBasicBondDataClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnBasicBondDataClass::Clone()
    {
      RxnBasicBondDataClass* id = new RxnBasicBondDataClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnBasicBondDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBasicBondDataClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnBasicBondDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBasicBondDataClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnBasicBondDataClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataBasicBondData();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnBasicBondDataClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnBasicBondDataClass*& obj)
     {
     obj = new RxnBasicBondDataClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataBasicBondData
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataBasicBondData*& obj)
     {
     obj = new RxnDataBasicBondData;
     return obj->DecodeThis(buffer);
     }
/*F ans = PrintLaTeXTablePrefix(out)  . . . . . . . . . print begining of set
**
**  DESCRIPTION
**    out: The output stream
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBasicBondDataClass::PrintLaTeXTablePrefix(ostream& out)
{
  bool result = true;
  out << "\\begin{tabbing}" << endl;
  out << "XXX \\= ";
  out << "XXX \\= ";
  out << "XXXXXXXXX \\= ";
  out << "XXXXXXXXX \\= ";
  out << "XXXXXXXXX \\= \\\\";

  out << " \\>  \\>  BO \\> C \\> E \\> \\\\" << endl;
  
  return result;
}
 
/*F ans = PrintLaTeXTablePostfix(out)  . . . . . . . . . print end of set
**
**  DESCRIPTION
**    out: The output stream
**    ans: true if successful
**    
**  REMARKS
**
*/
bool RxnBasicBondDataClass::PrintLaTeXTablePostfix(ostream& out)
{
  bool result = true;
  out << "\\end{tabbing}" << endl;
  return result;
}

/*S RxnDataMolFileBond
 */ 
/*F RxnDataMolFileBond()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMolFileBond::RxnDataMolFileBond()
  : BondI(0),
    BondJ(0)
{
  Identification = MOLBOND_MOLFILE_ID;
  NameTag = MOLBOND_MOLFILE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMolFileBond(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMolFileBond::RxnDataMolFileBond(const RxnDataMolFileBond& data)
  : RxnDataBasicBondData(data),
    BondI(data.BondI),
    BondJ(data.BondJ),
    BondData(data.BondData)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMolFileBond
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMolFileBond::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMolFileBond
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMolFileBond::Read(istream& in, DataObjectClass* objc, const String& name)
{
  //bool result = RxnDataBasicBondData::Read(in,objc,name);
  RxnMolFileBondClass *mfclass = (RxnMolFileBondClass *) objc;
  bool result = true;

  String line;
  line.ReadFullLine(in);

  BondI     = (unsigned int) line.ToInteger(0,2);
  BondJ     = (unsigned int) line.ToInteger(3,5);
  BondOrder = (unsigned int) line.ToInteger(6,8);

  unsigned int i=9;
  for(unsigned int cnt; cnt < mfclass->MaxBondData;cnt++)
    {
      int dat = (int) line.ToInteger(i,i+2);
      BondData.push_back(dat);
      i +=3;
    }
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMolFileBond
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMolFileBond::print(ostream& out) const
{
  RxnDataBasicBondData::print(out);
  out << "Bond[" << BondI << "," << BondJ << "] ";
  for(unsigned int i=0;i != BondData.size();i++)
    {
      out << BondData[i] << " ";
    }
  out << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMolFileBond
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMolFileBond::Clone()
{
  RxnDataMolFileBond *obj = new RxnDataMolFileBond(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMolFileBond
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMolFileBond::CopyClone(Identify * obj)
{
  RxnDataMolFileBond *objfull = (RxnDataMolFileBond *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMolFileBond
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMolFileBond::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataBasicBondData::EncodeThis(buffer);
  result = result && Encode(buffer,BondI);
  result = result && Encode(buffer,BondJ);

  int ss = BondData.size();
  result = result && Encode(buffer,ss);
  vector<int>::iterator i;
  for(i=BondData.begin();i<BondData.end();i++)
    {
      result = result && Encode(buffer,*i);
    }
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMolFileBond
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMolFileBond::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataBasicBondData::DecodeThis(buffer);
  result = result && Decode(buffer,BondI);
  result = result && Decode(buffer,BondJ);

  int siz;
  result = result && Decode(buffer,siz);
  int dat;
  for(int i=0;i < siz;i++)
    {
      result = result && Decode(buffer,dat);
      BondData.push_back(dat);
    }

  return result;
}
/*S RxnMolFileBondClass
 */
/*F RxnMolFileBondClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMolFileBondClass::RxnMolFileBondClass()
  : MaxBondData(0)
{
  Identification = MOLBOND_MOLFILE_ID;
  NameTag = MOLBOND_MOLFILE_NAME;
  SubClass = "BasicBondData";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMolFileBondClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMolFileBondClass::RxnMolFileBondClass(const RxnMolFileBondClass& data)
  : RxnBasicBondDataClass(data),
    MaxBondData(data.MaxBondData)
{
} 
/*F RxnMolFileBondClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMolFileBondClass::RxnMolFileBondClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnBasicBondDataClass(id,name,descr),
    MaxBondData(0)
{
  SubClass = "BasicBondData";
  EncodeDecodeClass = MOLBOND_MOLFILE_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMolFileBondClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMolFileBondClass::print(ostream& out) const
{
  RxnBasicBondDataClass::print(out);
  out << " BondDataSize: " << MaxBondData << endl;
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMolFileBondClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMolFileBondClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMolFileBondClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnBasicBondDataClass::Read(in,set);
  StreamObjectInput str(in,' ');
  String sizeS = str.ReadNext();
  MaxBondData = (unsigned int) sizeS.ToInteger();
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMolFileBondClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMolFileBondClass::CopyClone(Identify *  objc)
{
  RxnMolFileBondClass *objcfull = (RxnMolFileBondClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMolFileBondClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMolFileBondClass::Clone()
    {
      RxnMolFileBondClass* id = new RxnMolFileBondClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMolFileBondClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMolFileBondClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnBasicBondDataClass::EncodeThis(buffer);
  result = result && Encode(buffer,MaxBondData);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMolFileBondClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMolFileBondClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnBasicBondDataClass::DecodeThis(buffer);
  result = result && Decode(buffer,MaxBondData);
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
BaseDataObject * RxnMolFileBondClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMolFileBond();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMolFileBondClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMolFileBondClass*& obj)
     {
     obj = new RxnMolFileBondClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMolFileBond
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMolFileBond*& obj)
     {
     obj = new RxnDataMolFileBond;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataMoleculeBond
 */ 
/*F RxnDataMoleculeBond()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeBond::RxnDataMoleculeBond()
  : BasicBondData(NULL)
{
  Identification = MOLBOND_MOLBOND_ID;
  NameTag = MOLBOND_MOLBOND_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMoleculeBond(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMoleculeBond::RxnDataMoleculeBond(const RxnDataMoleculeBond& data)
  : BaseDataEdge(data)
{
  BasicBondData = (RxnDataBasicBondData *) PointerClone(data.BasicBondData);
}
/*F RxnDataMoleculeBond(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMoleculeBond::RxnDataMoleculeBond(RxnDataBasicBondData *data)
{
  Identification = MOLBOND_MOLBOND_ID;
  NameTag = MOLBOND_MOLBOND_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
  BasicBondData = (RxnDataBasicBondData *) PointerClone(data);
}
/*F ~RxnDataMoleculeBond()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeBond::~RxnDataMoleculeBond()
{
  if(BasicBondData != NULL)
    delete BasicBondData;
  BasicBondData = NULL;
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMoleculeBond
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMoleculeBond::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMoleculeBond
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMoleculeBond::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataEdge::Read(in,objc,name);
  RxnMoleculeBondClass *molbondclass = (RxnMoleculeBondClass *) objc;

  String notdefined("Basic Bond Data Class not defined");
  result = result && PointerObjectRead(in, (BaseDataObject *&) BasicBondData,
				       molbondclass->getBasicBondDataClass(), notdefined);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMoleculeBond
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMoleculeBond::print(ostream& out) const
{
  BaseDataEdge::print(out);
  PointerPrint(out,"The Basic Bond Information: ","No Bond Info",BasicBondData);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeBond
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMoleculeBond::Clone()
{
  RxnDataMoleculeBond *obj = new RxnDataMoleculeBond(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeBond
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMoleculeBond::CopyClone(Identify * obj)
{
  RxnDataMoleculeBond *objfull = (RxnDataMoleculeBond *) obj;
  *this = *objfull;
  BasicBondData = (RxnDataBasicBondData *) PointerClone(objfull->BasicBondData);
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeBond
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeBond::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataEdge::EncodeThis(buffer);
  result = result && PointerEncode(buffer,BasicBondData);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeBond
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeBond::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataEdge::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) BasicBondData);
  return result;
}
/*F ans = WriteAsLine(out,objc) . . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object one one line
**
**  REMARKS
**
*/
bool RxnDataMoleculeBond::WriteAsLine(ostream& out, DataObjectClass *objc)
{
  bool result = true;
  out << "Bond[" << getNode1() << "," << getNode2() << "]";
  if(BasicBondData != NULL)
    result = BasicBondData->WriteAsLine(out,objc);
  else
    result = false;
  return false;
}
/*F ans = WriteAsASCII(out,objc)  . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object as complement to input
**
**  REMARKS
**
*/
bool RxnDataMoleculeBond::WriteAsASCII(ostream& out, DataObjectClass *objc)
{
  bool result = true;

  if(BasicBondData != NULL)
    result = BasicBondData->WriteAsASCII(out,objc);
  else
    result = false;
  return false;
}
/*F ans = WriteAsLatex(out,objc)  . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object prepared for latex
**
**  REMARKS
**
*/
bool RxnDataMoleculeBond::WriteAsLatex(ostream& out, DataObjectClass* objc)
{
  bool result = true;

  out << " " << getNode1() << " \\> ";
  out << " " << getNode2() << " \\> ";
  if(BasicBondData != NULL)
    result = BasicBondData->WriteAsLatex(out,objc);
  else
    result = false;
  return false;
}
 
/*F ans = TransferBasicAtomDataToProperties()
**
**  DESCRIPTION
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeBond::TransferBasicAtomDataToProperties()
{
  bool result = true;
  if(BasicBondData != NULL)
    {
      result = BasicBondData->StoreAsProperties(this);
    }
  else
    result = false;

  return result;
}
 
/*F bonddata = getBasicBondData() . . . . . . . . . . . . RxnDataMoleculeBond
**
**  DESCRIPTION
**    bonddata: The basic standard information for a bond
**
**  REMARKS
**
*/
RxnDataBasicBondData *RxnDataMoleculeBond::getBasicBondData()
{
  return BasicBondData;
}
/*S RxnMoleculeBondClass
 */
/*F RxnMoleculeBondClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMoleculeBondClass::RxnMoleculeBondClass()
  : BasicBondDataClass(NULL)
{
  Identification = MOLBOND_MOLBOND_ID;
  NameTag = MOLBOND_MOLBOND_NAME;
  SubClass = "Edge";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMoleculeBondClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMoleculeBondClass::RxnMoleculeBondClass(const RxnMoleculeBondClass& data)
  : DataEdgeClass(data)
{
  BasicBondDataClass = (RxnBasicBondDataClass *) PointerClone(data.BasicBondDataClass);
} 
 
/*F RxnMoleculeBondClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMoleculeBondClass::RxnMoleculeBondClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataEdgeClass(id,name,descr),
    BasicBondDataClass(NULL)
{
  SubClass = "Edge";
  EncodeDecodeClass = MOLBOND_MOLBOND_NAME;
}
/*F RxnMoleculeBondClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMoleculeBondClass::~RxnMoleculeBondClass()
{
  if(BasicBondDataClass != NULL)
    delete BasicBondDataClass;
  BasicBondDataClass = NULL;
} 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMoleculeBondClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMoleculeBondClass::print(ostream& out) const
{
  DataEdgeClass::print(out);
  PointerPrint(out,"  The Basic Bond Info Class: "," No Class Defined ",BasicBondDataClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeBondClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMoleculeBondClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMoleculeBondClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = PointerClassRead(in,(DataObjectClass *&) BasicBondDataClass,
				      MOLBOND_BASIC_NAME,
				      set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMoleculeBondClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMoleculeBondClass::CopyClone(Identify *  objc)
{
  RxnMoleculeBondClass *objcfull = (RxnMoleculeBondClass *) objc;
  *this = *objcfull;
  BasicBondDataClass = (RxnBasicBondDataClass *) PointerClone(objcfull->BasicBondDataClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeBondClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMoleculeBondClass::Clone()
{
  RxnMoleculeBondClass* id = new RxnMoleculeBondClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeBondClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeBondClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataEdgeClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,BasicBondDataClass);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeBondClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeBondClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataEdgeClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) BasicBondDataClass);
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
BaseDataObject * RxnMoleculeBondClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMoleculeBond();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMoleculeBondClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMoleculeBondClass*& obj)
     {
     obj = new RxnMoleculeBondClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMoleculeBond
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMoleculeBond*& obj)
     {
     obj = new RxnDataMoleculeBond;
     return obj->DecodeThis(buffer);
     }
 
/*F bonddataclass = getBasicBondDataClass()
**
**  DESCRIPTION
**    bonddataclass: The Bond data class
**
**  REMARKS
**
*/
RxnBasicBondDataClass *RxnMoleculeBondClass::getBasicBondDataClass()
{
  return BasicBondDataClass;
}
/*F ans = PrintLaTeXTablePrefix(out)  . . . . . . . . . print begining of set
**
**  DESCRIPTION
**    out: The output stream
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeBondClass::PrintLaTeXTablePrefix(ostream& out)
{
  bool result = true;

  if(BasicBondDataClass != NULL)
    result = BasicBondDataClass->PrintLaTeXTablePrefix(out);
  else
    result = false;
  return result;
}
/*F ans = PrintLaTeXTablePostfix(out)  . . . . . . . . . print end of set
**
**  DESCRIPTION
**    out: The output stream
**    ans: true if successful
**    
**  REMARKS
**
*/
bool RxnMoleculeBondClass::PrintLaTeXTablePostfix(ostream& out)
{
  bool result = true;

  if(BasicBondDataClass != NULL)
    result = BasicBondDataClass->PrintLaTeXTablePostfix(out);
  else
    result = false;
  return result;
}
/*S Utilities
 */
/*F InitialSetOfMolAtomEncodeDecodeRoutines()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void InitialSetOfMolBondEncodeDecodeRoutines()
{
EncodeDecodeRegisterClass(RxnMoleculeBondClass,RxnDataMoleculeBond,MOLBOND_MOLBOND_NAME);
EncodeDecodeRegisterClass(RxnMolFileBondClass,RxnDataMolFileBond,MOLBOND_MOLFILE_NAME);
EncodeDecodeRegisterClass(RxnBasicBondDataClass,RxnDataBasicBondData,MOLBOND_BASIC_NAME);
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void AddMolBondClasses(DataSetOfObjectsClass& set)
{
  String bdescr("The Basic Bond Description Class");
  RxnMoleculeBondClass bclass(MOLBOND_MOLBOND_ID,MOLBOND_MOLBOND_NAME,bdescr);
  set.AddObjectClass(bclass);

  String mfdescr("The MolFile BondClass");
  RxnMolFileBondClass mfclass(MOLBOND_MOLFILE_ID,MOLBOND_MOLFILE_NAME,mfdescr);
  set.AddObjectClass(mfclass);

  String moldescr("The Molecular Bond Class");
  RxnBasicBondDataClass molclass(MOLBOND_BASIC_ID,MOLBOND_BASIC_NAME,moldescr);
  set.AddObjectClass(molclass);
}

