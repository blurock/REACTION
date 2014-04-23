/*  FILE     ThermoProps.cc
**  PACKAGE  ThermoProps
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "ThermoProps" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "FullSystem.hh"
#include "Dbase.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"

#include "MatrixNumeric.hh"
#include "MatrixUtilities.hh"
#include "PrimitiveStats.hh"
#include "ThermoTables.hh"
#include "ThermoProps.hh"
#include "MolStats.hh"

#include <iomanip>
/*S RxnDataLiteratureReference
 */ 
/*F RxnDataLiteratureReference()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataLiteratureReference::RxnDataLiteratureReference()
{
  Identification = THERMO_LIT_ID;
  NameTag = THERMO_LIT_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataLiteratureReference(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataLiteratureReference::RxnDataLiteratureReference(const RxnDataLiteratureReference& data)
  : BaseDataObject(data),
    Source(data.Source),
    Author(data.Author),
    Title(data.Title)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataLiteratureReference
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataLiteratureReference::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataLiteratureReference
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataLiteratureReference::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataObject::Read(in,objc,name);
  StreamObjectInput str(in,';');
  Source = str.ReadNext();
  Author = str.ReadNext();
  Title = str.ReadNext();
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataLiteratureReference
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataLiteratureReference::print(ostream& out) const
{
  BaseDataObject::print(out);
  out << endl;
  out << "Source: " << Source << endl;
  out << "Author: " << Author << endl;
  out << "Title:  " << Title  << endl;

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataLiteratureReference
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataLiteratureReference::Clone()
{
  RxnDataLiteratureReference *obj = new RxnDataLiteratureReference(*this);
  return obj;
}
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataLiteratureReference
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataLiteratureReference::CopyClone(Identify * obj)
{
  RxnDataLiteratureReference *objfull = (RxnDataLiteratureReference *) obj;
  *this = *objfull;
} 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataLiteratureReference
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataLiteratureReference::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::EncodeThis(buffer);
  result = result && Encode(buffer,Source);
  result = result && Encode(buffer,Author);
  result = result && Encode(buffer,Title);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataLiteratureReference
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataLiteratureReference::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::DecodeThis(buffer);
  result = result && Decode(buffer,Source);
  result = result && Decode(buffer,Author);
  result = result && Decode(buffer,Title);

  return result;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
String& RxnDataLiteratureReference::getSource()
{
  return Source;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
String& RxnDataLiteratureReference::getAuthor()
{
  return Author;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
String& RxnDataLiteratureReference::getTitle()
{
  return Title;
}
/*S RxnLiteratureReferenceClass
 */
/*F RxnLiteratureReferenceClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnLiteratureReferenceClass::RxnLiteratureReferenceClass()
{
  Identification = THERMO_LIT_ID;
  NameTag = THERMO_LIT_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
} 
/*F RxnLiteratureReferenceClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnLiteratureReferenceClass::RxnLiteratureReferenceClass(const RxnLiteratureReferenceClass& data)
  : DataObjectClass(data)
{
} 
 
/*F RxnLiteratureReferenceClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnLiteratureReferenceClass::RxnLiteratureReferenceClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataObjectClass(id,name,descr)
{
  SubClass = "Object";
  EncodeDecodeClass = "LiteratureReference";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnLiteratureReferenceClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnLiteratureReferenceClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnLiteratureReferenceClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnLiteratureReferenceClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnLiteratureReferenceClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataObjectClass::Read(in,set);
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnLiteratureReferenceClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnLiteratureReferenceClass::CopyClone(Identify *  objc)
{
  RxnLiteratureReferenceClass *objcfull = (RxnLiteratureReferenceClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnLiteratureReferenceClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnLiteratureReferenceClass::Clone()
    {
      RxnLiteratureReferenceClass* id = new RxnLiteratureReferenceClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnLiteratureReferenceClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnLiteratureReferenceClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnLiteratureReferenceClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnLiteratureReferenceClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnLiteratureReferenceClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataLiteratureReference();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnLiteratureReferenceClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnLiteratureReferenceClass*& obj)
     {
     obj = new RxnLiteratureReferenceClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataLiteratureReference
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataLiteratureReference*& obj)
     {
     obj = new RxnDataLiteratureReference;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataConversionFactors
 */ 
/*F RxnDataConversionFactors()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataConversionFactors::RxnDataConversionFactors()
  : AdditiveFactor(0.0),
    MultiplicativeFactor(1.0)
{
  Identification = THERMO_CONVERSION_ID;
  NameTag = THERMO_CONVERSION_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataConversionFactors(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataConversionFactors::RxnDataConversionFactors(const RxnDataConversionFactors& data)
  : BaseDataObject(data),
    AdditiveFactor(data.AdditiveFactor),
    MultiplicativeFactor(data.MultiplicativeFactor)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataConversionFactors
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataConversionFactors::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataConversionFactors
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataConversionFactors::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataObject::Read(in,objc,name);
  StreamObjectInput str(in,' ');
  String AdditiveFactorS = str.ReadNext();
  String MultiplicativeFactorS = str.ReadNext();

  AdditiveFactor = AdditiveFactorS.ToFloat();
  MultiplicativeFactor = MultiplicativeFactorS.ToFloat();

  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataConversionFactors
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataConversionFactors::print(ostream& out) const
{
  BaseDataObject::print(out);
  out << "Conversion = " << AdditiveFactor << " + Value * " << MultiplicativeFactor << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataConversionFactors
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataConversionFactors::Clone()
{
  RxnDataConversionFactors *obj = new RxnDataConversionFactors(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataConversionFactors
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataConversionFactors::CopyClone(Identify * obj)
{
  RxnDataConversionFactors *objfull = (RxnDataConversionFactors *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataConversionFactors
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataConversionFactors::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::EncodeThis(buffer);
  result = result && Encode(buffer,AdditiveFactor);
  result = result && Encode(buffer,MultiplicativeFactor);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataConversionFactors
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataConversionFactors::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::DecodeThis(buffer);
  result = result && Decode(buffer,AdditiveFactor);
  result = result && Decode(buffer,MultiplicativeFactor);
  return result;
}
/*F ans = ConvertFrom(normal,value) . . . . . . . . . RxnDataConversionFactor
**
**  DESCRIPTION
**    value: The value to be converted
**    normal: if true, use value, if false, use inverse values
**    ans: The converted value
**   
**  REMARKS
**
*/
double RxnDataConversionFactors::Convert(bool normal,double value)
{
  double cvalue;
  if(normal)
    cvalue = AdditiveFactor + MultiplicativeFactor * value;
  else
    cvalue = value/MultiplicativeFactor - AdditiveFactor;
  return cvalue;
}
/*F inv = ComputeInverseFactor()  . . . . . . . . .  RxnDataConversionFactors
**
**  DESCRIPTION
**    factorclass: The factor class
**    inv: The inverse
**
**  REMARKS
**
*/
RxnDataConversionFactors *RxnDataConversionFactors::ComputeInverseFactor(RxnConversionFactorsClass *factorclass)
{
  RxnDataConversionFactors *inv = (RxnDataConversionFactors *) Clone();
  String invS(factorclass->getInversePrefix());
  invS.AppendToEnd(NameTag);
  inv->NameTag = invS;
  inv->AdditiveFactor = - AdditiveFactor;
  inv->MultiplicativeFactor = 1.0/MultiplicativeFactor;
  return inv;
}
/*S RxnConversionFactorsClass
 */
/*F RxnConversionFactorsClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnConversionFactorsClass::RxnConversionFactorsClass()
  : InversePrefix("Inverse-")
{
  Identification = THERMO_CONVERSION_ID;
  NameTag = THERMO_CONVERSION_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
} 
/*F RxnConversionFactorsClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnConversionFactorsClass::RxnConversionFactorsClass(const RxnConversionFactorsClass& data)
  : DataObjectClass(data),
    InversePrefix(data.InversePrefix)
{
} 
 
/*F RxnConversionFactorsClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnConversionFactorsClass::RxnConversionFactorsClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataObjectClass(id,name,descr),
    InversePrefix("Inverse-")
{
  SubClass = "Object";
  EncodeDecodeClass = "ConversionFactors";
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnConversionFactorsClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnConversionFactorsClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnConversionFactorsClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnConversionFactorsClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnConversionFactorsClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataObjectClass::Read(in,set);
  StreamObjectInput str(in,' ');
  InversePrefix = str.ReadNext();
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnConversionFactorsClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnConversionFactorsClass::CopyClone(Identify *  objc)
{
  RxnConversionFactorsClass *objcfull = (RxnConversionFactorsClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnConversionFactorsClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnConversionFactorsClass::Clone()
    {
      RxnConversionFactorsClass* id = new RxnConversionFactorsClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnConversionFactorsClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnConversionFactorsClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  result = result && Encode(buffer,InversePrefix);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnConversionFactorsClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnConversionFactorsClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
  result = result && Decode(buffer,InversePrefix);
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
BaseDataObject * RxnConversionFactorsClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataConversionFactors();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnConversionFactorsClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnConversionFactorsClass*& obj)
     {
     obj = new RxnConversionFactorsClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataConversionFactors
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataConversionFactors*& obj)
     {
     obj = new RxnDataConversionFactors;
     return obj->DecodeThis(buffer);
     }
/*F name = getInversePrefix() . . . . . . . . . . . RxnConversionFactorsClass
**
**  DESCRIPTION
**    name: The prefix of the inverse values of the quantites
**
**  REMARKS
**
*/
String& RxnConversionFactorsClass::getInversePrefix()
{
  return InversePrefix;
}
/*S RxnDataSingleConversionSet
 */ 
/*F RxnDataSingleConversionSet()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataSingleConversionSet::RxnDataSingleConversionSet()
{
  Identification = THERMO_SINGLE_ID;
  NameTag = THERMO_SINGLE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataSingleConversionSet(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataSingleConversionSet::RxnDataSingleConversionSet(const RxnDataSingleConversionSet& data)
  : BaseDataSetOfObjects(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataSingleConversionSet
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataSingleConversionSet::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataSingleConversionSet
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataSingleConversionSet::Read(istream& in, DataObjectClass* objc, const String& name)
{
  //bool result = BaseDataSetOfObjects::Read(in,objc,name);
  bool result = true;
  RxnSingleConversionSetClass *objclass = (RxnSingleConversionSetClass *) objc;
  RxnConversionFactorsClass *factorsclass = objclass->getConversionClass();

  RxnDataConversionFactors *factor;
  StreamObjectInput str(in,' ');
  String fname = str.ReadNext();
  while(!(fname == "END"))
    {
      factor = (RxnDataConversionFactors *) factorsclass->BaseDataObjectExample();
      factor->NameTag = fname;
      factor->Read(in,factorsclass,fname);
      AddConversionFactor(factor,factorsclass);
      fname = str.ReadNext();
    }
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataSingleConversionSet
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataSingleConversionSet::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataSingleConversionSet
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataSingleConversionSet::Clone()
{
  RxnDataSingleConversionSet *obj = new RxnDataSingleConversionSet(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataSingleConversionSet
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataSingleConversionSet::CopyClone(Identify * obj)
{
  RxnDataSingleConversionSet *objfull = (RxnDataSingleConversionSet *) obj;
  *this = *objfull;
} 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataSingleConversionSet
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataSingleConversionSet::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataSingleConversionSet
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataSingleConversionSet::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  return result;
}
/*F ans = Convert(name,convclass,value)
**
**  DESCRIPTION
**   normal: true, use factors directly, false, use inverse factors
**   keys: The set of factor keys for conversion
**   value: The value to be converted
**
**  REMARKS
**
*/
double RxnDataSingleConversionSet::Convert(bool normal,BaseDataKeyWords& keys,
					   RxnSingleConversionSetClass *convclass, double value)
{
  ObjectList<String> names = keys.GetKeyWords();
  ObjectList<String>::iterator name;
  for(name = names.begin(); name != names.end(); name++)
    {
      if(IsInList(*name))
	value = Convert(normal,*name,convclass,value);
    }
  for(name = names.begin(); name != names.end(); name++)
    value = convclass->FunctionalConversions(normal,*name,value);

  return value;
} 
/*F ans = Convert(name,convclass,value)
**
**  DESCRIPTION
**   normal: true, use factors directly, false, use inverse factors
**   keys: The set of factor keys for conversion
**   value: The value to be converted
**
**  REMARKS
**
*/
double RxnDataSingleConversionSet::Convert(bool normal,
					   BaseDataKeyWords& keys,
					   BaseDataKeySet& filter,
					   RxnSingleConversionSetClass *convclass, double value)
{
  ObjectList<String> names = keys.GetKeyWords();
  ObjectList<String>::iterator name;
  for(name = names.begin(); name != names.end(); name++)
    {
      if(IsInList(*name) && filter.KeyWordInList(*name))
	value = Convert(normal,*name,convclass,value);
    }
  for(name = names.begin(); name != names.end(); name++)
    {
      if(filter.KeyWordInList(*name))
	value = convclass->FunctionalConversions(normal,*name,value);
    }
  return value;
} 
/*F ans = ConvertTo(name,convclass,value)
**
**  DESCRIPTION
**   normal: true, use factors directly, false, use inverse factors
**   name: The name of the conversion factor
**   value: The value to be converted
**
**  REMARKS
**
*/
double RxnDataSingleConversionSet::Convert(bool normal, String& name,RxnSingleConversionSetClass *convclass, double value)
{
  double ans;
  if(IsInList(name))
    {
      //cout << "Convert: '" << name << "'" << endl;
      //ListOfObjectNames().print(cout);
      //cout << endl;
      RxnDataConversionFactors *conversion = (RxnDataConversionFactors *) GetObject(name);
      ans = conversion->Convert(normal,value);
    }
  else
      ans = convclass->FunctionalConversions(normal,name,value);
  return ans;
}
/*F ans = AddConversionFactor(factors)  . . . . .  RxnDataSingleConversionSet
**
**  DESCRIPTION
**    factors: The factors for this unit
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataSingleConversionSet::AddConversionFactor(RxnDataConversionFactors *factors,
						     RxnConversionFactorsClass *factorclass)
{
  bool result = AddObject(factors);
  result = result && AddInverseFactor(factors,factorclass);
  return result;
}
 
/*F ans = AddInverseFactor(factor)  . . . . . .  RxnDataSingleConversionSet::
**
**  DESCRIPTION
**    factor: The factor to be complemented
**    ans: True if successfull
**
**  REMARKS
**
*/
bool RxnDataSingleConversionSet::AddInverseFactor(RxnDataConversionFactors *factor,
						  RxnConversionFactorsClass *factorclass)
{
  RxnDataConversionFactors *inversefactors = factor->ComputeInverseFactor(factorclass);
  bool result = AddObject(inversefactors);
  delete inversefactors;
  return result;
}
/*S RxnSingleConversionSetClass
 */
/*F RxnSingleConversionSetClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnSingleConversionSetClass::RxnSingleConversionSetClass()
  : ConversionClass(NULL)
{
  Identification = THERMO_SINGLE_ID;
  NameTag = THERMO_SINGLE_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnSingleConversionSetClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnSingleConversionSetClass::RxnSingleConversionSetClass(const RxnSingleConversionSetClass& data)
  : DataSetOfObjectsClass(data)
{
  ConversionClass = (RxnConversionFactorsClass *) PointerClone(data.ConversionClass);
} 
 
/*F RxnSingleConversionSetClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnSingleConversionSetClass::RxnSingleConversionSetClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr),
    ConversionClass(NULL)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = "SingleConversionSet";
  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnSingleConversionSetClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnSingleConversionSetClass::print(ostream& out) const
{
  DataSetOfObjectsClass::print(out);
  PointerPrint(out,"  The Conversion Class: "," No Class Defined ",ConversionClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnSingleConversionSetClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnSingleConversionSetClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnSingleConversionSetClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = PointerClassRead(in,(DataObjectClass *&) ConversionClass,
				      THERMO_CONVERSION_NAME,
				      set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnSingleConversionSetClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnSingleConversionSetClass::CopyClone(Identify *  objc)
{
  RxnSingleConversionSetClass *objcfull = (RxnSingleConversionSetClass *) objc;
  *this = *objcfull;
  ConversionClass = (RxnConversionFactorsClass *) PointerClone(objcfull->ConversionClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnSingleConversionSetClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnSingleConversionSetClass::Clone()
{
  RxnSingleConversionSetClass* id = new RxnSingleConversionSetClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnSingleConversionSetClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnSingleConversionSetClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,ConversionClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnSingleConversionSetClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnSingleConversionSetClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) ConversionClass);
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
BaseDataObject * RxnSingleConversionSetClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataSingleConversionSet();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnSingleConversionSetClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnSingleConversionSetClass*& obj)
     {
     obj = new RxnSingleConversionSetClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataSingleConversionSet
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataSingleConversionSet*& obj)
     {
     obj = new RxnDataSingleConversionSet;
     return obj->DecodeThis(buffer);
     }
 
/*F ans = FunctionalConversionsTo(normal,func,value)
**
**  DESCRIPTION
**    func: The functional form name
**    value: The value to operate on
**    ans: The result (if func not found, the value stays the same)
**    
**    Possible functions Exp, Log, Log10, Power10
**
**  REMARKS
**
*/
double RxnSingleConversionSetClass::FunctionalConversions(bool normal,String& func, double value)
{
  double ans = value;
  if(normal)
    {
      if(func == "Exp")
	ans = exp(value);
      else if(func == "Log")
	ans = log(value);
      else if(func == "Log10")
	ans = log10(value);
      else if(func == "Power10")
	ans = pow(10.0,value);
    }
  else
    {
  if(func == "Exp")
    ans = log(value);
  else if(func == "Log")
    ans = exp(value);
  else if(func == "Log10")
    ans = pow(10.0,value);
  else if(func == "Power10")
    ans = log10(value);
    }
  return ans;
}
/*F convclass = getConversionClass() 
**
**  DESCRIPTION
**    convclass: The conversion factor class
**
**  REMARKS
**
*/
RxnConversionFactorsClass *RxnSingleConversionSetClass::getConversionClass()
{
  return ConversionClass;
}
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnSingleConversionSetClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S RxnDataConversionSet
 */ 
/*F RxnDataConversionSet()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataConversionSet::RxnDataConversionSet()
{
  Identification = THERMO_CONVSET_ID;
  NameTag = THERMO_CONVSET_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataConversionSet(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataConversionSet::RxnDataConversionSet(const RxnDataConversionSet& data)
  : BaseDataSetOfObjects(data),
    Objects(data.Objects),
    StandardUnits(data.StandardUnits)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataConversionSet
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataConversionSet::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataConversionSet
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataConversionSet::Read(istream& in, DataObjectClass* objc, const String& name)
{
  //bool result = BaseDataSetOfObjects::Read(in,objc,name);
  bool result = true;
  RxnConversionSetClass *objclass = (RxnConversionSetClass *) objc;

  RxnSingleConversionSetClass *factorsclass = objclass->getConversionUnitClass();
  RxnDataSingleConversionSet *factor;
  DataKeyWordsClass keyclass;
  String standardunits("StandardUnits:");
  if(CheckReadKeyWord(in,standardunits))
    {
      StreamObjectInput str(in,' ');
      String name = str.ReadNext();
      while(!(name == "END"))
	{
	  factor = (RxnDataSingleConversionSet *) factorsclass->BaseDataObjectExample();
	  factor->NameTag = name;
	  StandardUnits.AddKeyWord(name);
	  factor->Read(in,factorsclass,name);
	  AddObject(factor);
	  delete factor;
	  name = str.ReadNext();
	}
    }
  String propertyunits("PropertyConversions:");
  String numerator("Numerator:");
  String denominator("Denominator:");
  if(CheckReadKeyWord(in,propertyunits))
    {
      StreamObjectInput str(in,' ');
      String name = str.ReadNext();
      while(result && !(name == "END"))
	{
	  BaseDataKeySet *units = new BaseDataKeySet();
	  if(CheckReadKeyWord(in,numerator))
	    {
	      units->NameTag = name;
	      name = str.ReadNext();
	      while(!(name == denominator))
		{
		  units->AddKeyWord(name);
		  name = str.ReadNext();
		}
	      name = str.ReadNext();
	      while(!(name == "END"))
		{
		  String invS(factorsclass->getConversionClass()->getInversePrefix());
		  invS.AppendToEnd(name);
		  units->AddKeyWord(invS);
		  name = str.ReadNext();
		}
	    }
	  else
	    result = false;
	  AddPropertyUnits(units);
	  delete units;
	  name = str.ReadNext();
	}
    }
  
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataConversionSet
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataConversionSet::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  out << "Standard Units:" << endl;
  StandardUnits.print(out);
  cout << endl;
  out << "Standard Property Conversions:" << endl;
  Objects.print(out);
  out << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataConversionSet
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataConversionSet::Clone()
{
  RxnDataConversionSet *obj = new RxnDataConversionSet(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataConversionSet
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataConversionSet::CopyClone(Identify * obj)
{
  RxnDataConversionSet *objfull = (RxnDataConversionSet *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataConversionSet
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataConversionSet::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  result = result && Objects.EncodeThis(buffer);
  result = result && StandardUnits.EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataConversionSet
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataConversionSet::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  result = result && Objects.DecodeThis(buffer);
  result = result && StandardUnits.DecodeThis(buffer);
  return result;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
double RxnDataConversionSet::ReadRealProperty(istream& in,
					      String& propertyname,
					      RxnConversionSetClass *factorclass)
{
  StreamObjectInput str(in,' ');
  String propS = str.ReadNext();
  double value = propS.ToFloat();
  double converted = ConvertPropertyToUnits(true,propertyname,value,factorclass);
  return converted;
}				    
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
double RxnDataConversionSet::ConvertPropertyToUnits(bool tounits,
			      String& propertyname,
			      double value,
			      RxnConversionSetClass *factorclass)
{
  if(Objects.IsInList(propertyname))
    {
      BaseDataKeySet *set = (BaseDataKeySet *) Objects.GetObject(propertyname);
      value = Convert(tounits,*set,factorclass,value);
    }
  else
    {
      cerr << "Property '" << propertyname << "' not found in '" << factorclass->NameTag << "'" << endl;
      cerr << "Expected: ";
      Objects.print(cerr);
      cerr << endl;
      cerr << "Value: " << value << " unchanged" << endl;
    }
  return value;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
double RxnDataConversionSet::ConvertPropertyToUnits(bool tounits,
						    String& propertyname,
						    BaseDataKeySet& filter,
						    double value,
						    RxnConversionSetClass *factorclass)
{
  if(Objects.IsInList(propertyname))
    {
      BaseDataKeySet *set = (BaseDataKeySet *) Objects.GetObject(propertyname);
      value = Convert(tounits,*set,filter,factorclass,value);
    }
  else
    {
      cerr << "Property '" << propertyname << "' not found in '" << factorclass->NameTag << "'" << endl;
      cerr << "Expected: ";
      Objects.print(cerr);
      cerr << "with filter: ";
      filter.print(cerr);
      cerr << endl;
      cerr << "Value: " << value << " unchanged" << endl;
    }
  return value;
}
/*F ans = Convert(convertto,units,value)  . . . . . . .  RxnDataConversionSet
**
**  DESCRIPTION
**    convertto: if true convert to and if false, convert from standard units
**    units: The set of conversions for this unit
**    value: The value to convert
**
**  REMARKS
**
*/
double RxnDataConversionSet::Convert(bool convertto, 
				     BaseDataKeyWords &units, 
				     RxnConversionSetClass *convclass,
				     double value)
{
  ObjectList<String> standardunits = StandardUnits.GetKeyWords();
  ObjectList<String>::iterator standardunit;
  for(standardunit = standardunits.begin(); 
      standardunit != standardunits.end();
      standardunit++)
    {
      RxnDataSingleConversionSet *unit = (RxnDataSingleConversionSet *) GetObject(*standardunit);
      value = unit->Convert(convertto,units,convclass->getConversionUnitClass(),value);
    }
  return value;
}
/*F ans = Convert(convertto,units,value)  . . . . . . .  RxnDataConversionSet
**
**  DESCRIPTION
**    convertto: if true convert to and if false, convert from standard units
**    units: The set of conversions for this unit
**    value: The value to convert
**
**  REMARKS
**
*/
double RxnDataConversionSet::Convert(bool convertto, 
				     BaseDataKeyWords &units,
				     BaseDataKeySet& filter,
				     RxnConversionSetClass *convclass,
				     double value)
{
  ObjectList<String> standardunits = StandardUnits.GetKeyWords();
  ObjectList<String>::iterator standardunit;
  for(standardunit = standardunits.begin(); 
      standardunit != standardunits.end();
      standardunit++)
    {
      RxnDataSingleConversionSet *unit = (RxnDataSingleConversionSet *) GetObject(*standardunit);
      value = unit->Convert(convertto,units,filter,convclass->getConversionUnitClass(),value);
    }
  return value;
}
 
/*F ans = AddConversion(factorset)  . . . . . . . . . .  RxnDataConversionSet
**
**  DESCRIPTION
**    factorset: 
**    ans:  True if successful
**  REMARKS
**
*/
bool RxnDataConversionSet::AddConversion(RxnDataSingleConversionSet *factorset)
{
  return AddObject(factorset);
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataConversionSet::AddPropertyUnits(BaseDataKeySet *set)
{
  return Objects.AddObject(set);
}
/*S RxnConversionSetClass
 */
/*F RxnConversionSetClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnConversionSetClass::RxnConversionSetClass()
  : ConversionUnitClass(NULL)
{
  Identification = THERMO_CONVSET_ID;
  NameTag = THERMO_CONVSET_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnConversionSetClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnConversionSetClass::RxnConversionSetClass(const RxnConversionSetClass& data)
  : DataSetOfObjectsClass(data),
    ConversionUnitClass(data.ConversionUnitClass)
{
} 
/*F RxnConversionSetClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnConversionSetClass::RxnConversionSetClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr),
    ConversionUnitClass(NULL)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = "ConversionSet";
  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnConversionSetClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnConversionSetClass::print(ostream& out) const
{
  DataSetOfObjectsClass::print(out);
  PointerPrint(out,"  The Conversion Units Class: "," No Class Defined ",ConversionUnitClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnConversionSetClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnConversionSetClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnConversionSetClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  //  bool result = DataSetOfObjectsClass::Read(in,set);
  bool result = PointerClassRead(in,(DataObjectClass *&) ConversionUnitClass,
				      THERMO_SINGLE_NAME,
				      set," No Class ");
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnConversionSetClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnConversionSetClass::CopyClone(Identify *  objc)
{
  RxnConversionSetClass *objcfull = (RxnConversionSetClass *) objc;
  *this = *objcfull;
  ConversionUnitClass = (RxnSingleConversionSetClass *) PointerClone(objcfull->ConversionUnitClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnConversionSetClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnConversionSetClass::Clone()
{
  RxnConversionSetClass* id = new RxnConversionSetClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnConversionSetClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnConversionSetClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,ConversionUnitClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnConversionSetClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnConversionSetClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) ConversionUnitClass);
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
BaseDataObject * RxnConversionSetClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataConversionSet();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnConversionSetClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnConversionSetClass*& obj)
     {
     obj = new RxnConversionSetClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataConversionSet
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataConversionSet*& obj)
{
  obj = new RxnDataConversionSet;
  return obj->DecodeThis(buffer);
}
 
/*F unitclass = getConversionUnitClass()  . . . . . . . RxnConversionSetClass
**
**  DESCRIPTION
**    unitclass: conversion units set class
**
**  REMARKS
**
*/
RxnSingleConversionSetClass *RxnConversionSetClass::getConversionUnitClass()
{
  return ConversionUnitClass;
}
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnConversionSetClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S RxnDataRealBasedProperty
 */ 
/*F RxnDataRealBasedProperty()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataRealBasedProperty::RxnDataRealBasedProperty()
  : Reference(NULL)
{
  Identification = THERMO_REAL_ID;
  NameTag = THERMO_REAL_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataRealBasedProperty(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataRealBasedProperty::RxnDataRealBasedProperty(const RxnDataRealBasedProperty& data)
  : BaseDataSetOfObjects(data)
{
  Reference = (RxnDataLiteratureReference *) PointerClone(data.Reference);
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataRealBasedProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataRealBasedProperty::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataRealBasedProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataRealBasedProperty::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataSetOfObjects::Read(in,objc,name);
  RxnRealBasedPropertyClass *objcfull = (RxnRealBasedPropertyClass *) objc;
  if(objcfull->getReferenceClass() != 0)
    {
      Reference = (RxnDataLiteratureReference *) objcfull->getReferenceClass()->BaseDataObjectExample();
      Reference->Read(in,objcfull->getReferenceClass());
    }
  else
    {
      cerr << "Literature Reference not defined in property '" << NameTag << "'" << endl;
      result = false;
    }
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataRealBasedProperty
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataRealBasedProperty::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  PointerPrint(out,"Reference: ","No Reference Specified",Reference);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataRealBasedProperty
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataRealBasedProperty::Clone()
{
  RxnDataRealBasedProperty *obj = new RxnDataRealBasedProperty(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataRealBasedProperty
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataRealBasedProperty::CopyClone(Identify * obj)
{
  RxnDataRealBasedProperty *objfull = (RxnDataRealBasedProperty *) obj;
  *this = *objfull;
  Reference = (RxnDataLiteratureReference *) PointerClone(objfull->Reference);
} 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataRealBasedProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataRealBasedProperty::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  result = result && PointerEncode(buffer,Reference);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataRealBasedProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataRealBasedProperty::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) Reference);
  return result;
}
 
/*F ref = getReference()  . . . . . . . . . . . . .  RxnDataRealBasedProperty
**
**  DESCRIPTION
**    ref: The full reference for the property
**
**  REMARKS
**
*/
RxnDataLiteratureReference *RxnDataRealBasedProperty::getReference()
{
  return Reference;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataRealBasedProperty::setReference(RxnDataLiteratureReference *ref)
{
  Reference = (RxnDataLiteratureReference *) ref->Clone();
}


/*S RxnRealBasedPropertyClass
 */
/*F RxnRealBasedPropertyClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnRealBasedPropertyClass::RxnRealBasedPropertyClass()
  : Conversions(NULL),
    ConversionsClass(NULL),
    ReferenceClass(NULL)
{
  Identification = THERMO_REAL_ID;
  NameTag = THERMO_REAL_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnRealBasedPropertyClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnRealBasedPropertyClass::RxnRealBasedPropertyClass(const RxnRealBasedPropertyClass& data)
  : DataSetOfObjectsClass(data)
{
  Conversions      = (RxnDataConversionSet *) PointerClone(data.Conversions);
  ConversionsClass = (RxnConversionSetClass *) PointerClone(data.ConversionsClass);
  ReferenceClass   = (RxnLiteratureReferenceClass *) PointerClone(data.ReferenceClass);
}
/*F RxnRealBasedPropertyClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnRealBasedPropertyClass::RxnRealBasedPropertyClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr),
    Conversions(NULL),
    ConversionsClass(NULL),
    ReferenceClass(NULL)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = "RealBasedProperty";
  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnRealBasedPropertyClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnRealBasedPropertyClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  PointerPrint(out,"  The Conversion Constants: "," Not Defined ",Conversions);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnRealBasedPropertyClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnRealBasedPropertyClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnRealBasedPropertyClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  //bool result = DataSetOfObjectsClass::Read(in,set);

  String notdefined("Not Defined");
  bool result = PointerClassRead(in,(DataObjectClass *&) ReferenceClass,
				      THERMO_LIT_NAME,set,notdefined);
  result = result && PointerClassRead(in,(DataObjectClass *&) ConversionsClass,
				      THERMO_CONVSET_NAME,set,notdefined);
  result = result && PointerObjectRead(in,(BaseDataObject *&) Conversions,ConversionsClass,notdefined);
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnRealBasedPropertyClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnRealBasedPropertyClass::CopyClone(Identify *  objc)
{
  RxnRealBasedPropertyClass *objcfull = (RxnRealBasedPropertyClass *) objc;
  *this = *objcfull;
  ReferenceClass = (RxnLiteratureReferenceClass *) PointerClone(objcfull->ReferenceClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnRealBasedPropertyClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnRealBasedPropertyClass::Clone()
{
  RxnRealBasedPropertyClass* id = new RxnRealBasedPropertyClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnRealBasedPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnRealBasedPropertyClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,ConversionsClass);
  result = result && PointerEncode(buffer,Conversions);
  result = result && PointerEncode(buffer,ReferenceClass);
  
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnRealBasedPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnRealBasedPropertyClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) ConversionsClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) Conversions);
  result = result && PointerDecode(buffer,(BaseDataObject *&) ReferenceClass);
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
BaseDataObject * RxnRealBasedPropertyClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataRealBasedProperty();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnRealBasedPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnRealBasedPropertyClass*& obj)
{
  obj = new RxnRealBasedPropertyClass;
  return obj->DecodeThis(buffer);
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataRealBasedProperty
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataRealBasedProperty*& obj)
{
  obj = new RxnDataRealBasedProperty;
  return obj->DecodeThis(buffer);
}
/*F ans = addConversion(f)  . . . . . . . . . . . .  RxnThermoPropertyClass::
**
**  DESCRIPTION
**    f: The conversion factor to standard units
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnRealBasedPropertyClass::addConversion(RxnDataSingleConversionSet *set)
{
  return Conversions->AddConversion(set);
}
/*F ans = Convert(factor,value) . . . . . . . . . . .  RxnThermoPropertyClass
**
**  DESCRIPTION
**    factor: The conversion factor name
**    value: The value to be transformed
**    ans: The converted value
**
**  REMARKS
**
*/
double RxnRealBasedPropertyClass::Convert(bool convertto,BaseDataKeyWords& units, double value)
{
  return Conversions->Convert(convertto,units,ConversionsClass,value);
}
/*F ans = Convert(convertto,property,value) . . . . . . . . . . .  RxnThermoPropertyClass
**
**  DESCRIPTION
**    factor: The conversion factor name
**    value: The value to be transformed
**    ans: The converted value
**
**  REMARKS
**
*/
double RxnRealBasedPropertyClass::Convert(bool convertto,String& property, double value)
{
  return Conversions->ConvertPropertyToUnits(convertto,property,
					     value,ConversionsClass);
}
/*F ans = Convert(convertto,property,filter,value) . . . . . . . . . . .  RxnThermoPropertyClass
**
**  DESCRIPTION
**    factor: The conversion factor name
**    value: The value to be transformed
**    ans: The converted value
**
**  REMARKS
**
*/
double RxnRealBasedPropertyClass::Convert(bool convertto,String& property, BaseDataKeySet& filter, double value)
{
  return Conversions->ConvertPropertyToUnits(convertto,property,filter,
					     value,ConversionsClass);
}
/*F refclass = getReferenceClass()  . . . . . . . .  RxnRealBasedPropertyClas
**
**  DESCRIPTION
**    refclass: The reference class
**
**  REMARKS
**
*/
RxnLiteratureReferenceClass *RxnRealBasedPropertyClass::getReferenceClass()
{
  return ReferenceClass;
}
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnRealBasedPropertyClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S RxnDataThermoProperty
 */ 
/*F RxnDataThermoProperty()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataThermoProperty::RxnDataThermoProperty()
  : StandardEntropy(0.0),
    StandardEnthalpy(0.0)
{
  Identification = THERMO_PROPERTY_ID;
  NameTag = THERMO_PROPERTY_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataThermoProperty(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataThermoProperty::RxnDataThermoProperty(const RxnDataThermoProperty& data)
  : RxnDataRealBasedProperty(data),
    StandardEntropy(data.StandardEntropy),
    StandardEnthalpy(data.StandardEnthalpy)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataThermoProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataThermoProperty::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataThermoProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataThermoProperty::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = RxnDataRealBasedProperty::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataThermoProperty
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataThermoProperty::print(ostream& out) const
{
  RxnDataRealBasedProperty::print(out);
  out << "StandardEntropy: " << StandardEntropy << ", StandardEnthalpy: " << StandardEnthalpy << endl;

  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataThermoProperty
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataThermoProperty::Clone()
{
  RxnDataThermoProperty *obj = new RxnDataThermoProperty(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataThermoProperty
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataThermoProperty::CopyClone(Identify * obj)
{
  RxnDataThermoProperty *objfull = (RxnDataThermoProperty *) obj;
  *this = *objfull;
  //Parameter = (RxnData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataThermoProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataThermoProperty::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataRealBasedProperty::EncodeThis(buffer);
  result = result && Encode(buffer,StandardEntropy);
  result = result && Encode(buffer,StandardEnthalpy);
  //result = result && ---.EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataThermoProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataThermoProperty::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataRealBasedProperty::DecodeThis(buffer);
  result = result && Decode(buffer,StandardEntropy);
  result = result && Decode(buffer,StandardEnthalpy);
  return result;
} 
/*F HpDeriv = CalculateHeatCapacityDerivative(classinfo,Temper)
**
**  DESCRIPTION
**    Temper: The temperature at which to calculate
**    Hp: The calculated heat capacity derivative
**
**  REMARKS
**
*/
double RxnDataThermoProperty::CalculateHeatCapacityDerivative(RxnThermoPropertyClass *classinfo,
							      const double& Temper)
{
  double tupper = Temper*1.05;
  double tlower = Temper*0.95;
  double eupper = CalculateHeatCapacity(classinfo,tupper);
  double elower = CalculateHeatCapacity(classinfo,tlower);
  double deriv = (eupper - elower)/(tupper - tlower);

  return deriv;
}
 
/*F Hp = CalculateHeatCapacity(classinfo,Temper)  . . . . . . . . .  from thermo object
**
**  DESCRIPTION
**    Temper: The temperature at whcih to calculate
**    Hp: The heat capacity
**
**  REMARKS
**
*/
double RxnDataThermoProperty::CalculateHeatCapacity(RxnThermoPropertyClass *classinfo,
						    const double& Temper)
{
  return 0.0;
}
/*F entropy = CalculateEntropy(classinfo,Temper)
**
**  DESCRIPTION
**    classinfo: the thermodynamic class
**    Temper: The temperature to calculate
**    entropy: The calculated entropy
**
**  REMARKS
**
*/
double RxnDataThermoProperty::CalculateEntropy(RxnThermoPropertyClass *classinfo,
					       const double& Temper) {
  cout << "RxnDataThermoProperty::CalculateEntropy" << endl;
  double standard = getStandardEntropy(classinfo);
  double standardtemp = classinfo->getStandardTemperature();
  double cp = CalculateHeatCapacity(classinfo,Temper);
  double entropy = standard + cp*log(Temper/standardtemp);
  cout << "Std Entropy=" << standard << " T=" << standardtemp 
       << " cp=" << cp << " entropy=" << endl;
  return entropy;
}
/*F entropy = CalculateEnthalpy(classinfo,Temper)
**
**  DESCRIPTION
**    classinfo: the thermodynamic class
**    Temper: The temperature to calculate
**    enthalpy: The calculated enthalpy
**
**  REMARKS
**
*/
double RxnDataThermoProperty::CalculateEnthalpy(RxnThermoPropertyClass *classinfo,
						const double& Temper)
{
  double standardenthalpy = getStandardEnthalpy(classinfo);
  double standardtemp = classinfo->getStandardTemperature();
  double deltatemp = Temper - standardtemp;
  double cp = CalculateHeatCapacity(classinfo,Temper);
  double enthalpy = standardenthalpy + deltatemp * cp;
  return enthalpy;
}
/*F freeenergy = CalculateFreeEnergy(classinfo,Temper)
**
**  DESCRIPTION
**    classinfo: the thermodynamic class
**    Temper: The temperature to calculate
**    freeenergy: The calculated free energy
**
**  REMARKS
**
*/
double RxnDataThermoProperty::CalculateFreeEnergy(RxnThermoPropertyClass *classinfo,
				     const double& Temper)
{
  double entropy = CalculateEntropy(classinfo,Temper);
  double enthalpy = CalculateEnthalpy(classinfo,Temper);
  double freeenergy = enthalpy - Temper * entropy;
  cout << "Free Energy: " << freeenergy << " = " << enthalpy << " - " << Temper << " * " << entropy << endl;
  return freeenergy;
}

/*F equil = CalculateEquilibrium(classinfo,Temper)
**
**  DESCRIPTION
**    classinfo: the thermodynamic class
**    Temper: The temperature to calculate
**    equil: The calculated equilibrium
**
**  REMARKS
**
*/
double RxnDataThermoProperty::CalculateEquilibrium(RxnThermoPropertyClass *classinfo,
						   const double& Temper)
{
  double freeenergy = CalculateFreeEnergy(classinfo,Temper);
  double efenergy   = -freeenergy/(Temper*classinfo->getGasConstant());
  double equil = 1.0;
  equil = exp(efenergy);
  cout << "CalculateEquilibrium: " << equil << " == ";
  cout << "exp(-" << freeenergy << " / (" << Temper << " * " << classinfo->getGasConstant() << " )" << endl;
  return equil;
}
 
/*F enthalpy = getStandardEnthalpy(classinfo)
**
**  DESCRIPTION
**    classinfo: The thermodynamic class
**    enthalpy: The standard enthalpy
**
**  REMARKS
**
*/
double RxnDataThermoProperty::getStandardEnthalpy(RxnThermoPropertyClass *classinfo)
{
  return StandardEnthalpy;
}
/*F entropy = getStandardEntropy(classinfo)
**
**  DESCRIPTION
**    classinfo: The thermodynamic class
**    entropy: The standard enthalpy
**
**  REMARKS
**
*/
double RxnDataThermoProperty::getStandardEntropy(RxnThermoPropertyClass *classinfo)
{
  return StandardEntropy;
}
 
/*F setStandardEnthalpy(enthalpy,classinfo) . . . . . . RxnDataThermoProperty
**
**  DESCRIPTION
**    enthalpy: The standard enthalpy
**    classinfo: The thermodynamic class
**
**  REMARKS
**
*/
void RxnDataThermoProperty::setStandardEnthalpy(double enthalpy, 
						RxnThermoPropertyClass *classinfo)
{
  StandardEnthalpy = enthalpy;
}
 
/*F setStandardEntropy(entropy,classinfo) . . . . . . . RxnDataThermoProperty
**
**  DESCRIPTION
**    entropy: The standard entropy
**    classinfo: The thermodynamic class
**
**  REMARKS
**
*/
void RxnDataThermoProperty::setStandardEntropy(double entropy,
					       RxnThermoPropertyClass *classinfo)
{
  StandardEntropy = entropy;
}
 
/*F mat = CalculateThermoUnderUnits(punits,clsinfo,temps)
**
**  DESCRIPTION
**    punits: The units of the Heat Capacity, Enthalpy and Entropy
**    clsinfo: The property class
**    temps: The list of temperatures to calculate
**
**  REMARKS
**
*/
BaseDataDoubleMatrix *RxnDataThermoProperty::CalculateThermoUnderUnits(BaseDataKeyWords *punits,
								       RxnThermoPropertyClass *clsinfo,
								       BaseDataDoubleVector *temps)
{
  ObjectList<String> unitsnames = punits->GetKeyWords();
  String cpunits = unitsnames.front();
  unitsnames.pop_front();
  String enthalpyunits = unitsnames.front();
  unitsnames.pop_front();
  String entropyunits = unitsnames.front();
  unitsnames.pop_front();

  cout << "  Cp       Units: " << cpunits;
  cout << "  Enthalpy Units: " << enthalpyunits;
  cout << "  Entropy  Units: " << entropyunits;
  cout << endl;

  VectorNumeric vec = temps->CurrentVector();
  MatrixNumeric m(vec.size(),4);
  BaseDataDoubleMatrix *mat = new BaseDataDoubleMatrix(m);
  VectorNumeric::iterator iter;
  unsigned int count = 0;
  for(iter = vec.begin(); iter != vec.end(); iter++)
    {
      double cp       = CalculateHeatCapacity(clsinfo,*iter);
      double entropy  = CalculateEnthalpy(clsinfo,*iter);
      double enthalpy = CalculateEntropy(clsinfo,*iter);

      double cpnew      = clsinfo->Convert(true,cpunits,cp);
      double entropynew = clsinfo->Convert(true,entropyunits,entropy);
      double enthalpynew = clsinfo->Convert(true,enthalpyunits,enthalpy);

      mat->CurrentMatrix()[count][0] = *iter;
      mat->CurrentMatrix()[count][1] = cpnew;
      mat->CurrentMatrix()[count][2] = enthalpynew;
      mat->CurrentMatrix()[count][3] = entropynew;
      count++;
    }
  return mat;
}

/*S RxnThermoPropertyClass
 */
/*F RxnThermoPropertyClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnThermoPropertyClass::RxnThermoPropertyClass()
  : StandardTemperature(0.0),
    GasConstant(0.0)
{
  Identification = THERMO_PROPERTY_ID;
  NameTag = THERMO_PROPERTY_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
} 
/*F RxnThermoPropertyClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnThermoPropertyClass::RxnThermoPropertyClass(const RxnThermoPropertyClass& data)
  : RxnRealBasedPropertyClass(data),
    StandardTemperature(data.StandardTemperature),
    GasConstant(data.GasConstant)
{
}
/*F RxnThermoPropertyClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnThermoPropertyClass::RxnThermoPropertyClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnRealBasedPropertyClass(id,name,descr),
    StandardTemperature(0.0),
    GasConstant(0.0)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = THERMO_PROPERTY_NAME;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnThermoPropertyClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnThermoPropertyClass::print(ostream& out) const
{
  RxnRealBasedPropertyClass::print(out);
  out << "GasConstant: " << GasConstant;
  out << "  StandardTemperature: " << StandardTemperature << endl;
  return out;
}
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnThermoPropertyClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnThermoPropertyClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnThermoPropertyClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnRealBasedPropertyClass::Read(in,set);
  StreamObjectInput str(in,' ');
  String StandardTemperatureS = str.ReadNext();
  String GasConstantS = str.ReadNext();
  cout << "ThermoProperty: " << StandardTemperatureS << ", " << GasConstantS << endl;
  StandardTemperature = StandardTemperatureS.ToFloat();
  GasConstant = GasConstantS.ToFloat();
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnThermoPropertyClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnThermoPropertyClass::CopyClone(Identify *  objc)
{
  RxnThermoPropertyClass *objcfull = (RxnThermoPropertyClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnThermoPropertyClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnThermoPropertyClass::Clone()
{
  RxnThermoPropertyClass* id = new RxnThermoPropertyClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnThermoPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnThermoPropertyClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnRealBasedPropertyClass::EncodeThis(buffer);
  result = result && Encode(buffer,StandardTemperature);
  result = result && Encode(buffer,GasConstant);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnThermoPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnThermoPropertyClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnRealBasedPropertyClass::DecodeThis(buffer);
  result = result && Decode(buffer,StandardTemperature);
  result = result && Decode(buffer,GasConstant);

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
BaseDataObject * RxnThermoPropertyClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataThermoProperty();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnThermoPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnThermoPropertyClass*& obj)
     {
     obj = new RxnThermoPropertyClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataThermoProperty
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataThermoProperty*& obj)
{
  obj = new RxnDataThermoProperty;
  return obj->DecodeThis(buffer);
} 
/*F getStandardTemperature()
**
**  DESCRIPTION
**    temp: The standard temperature
**
**  REMARKS
**
*/
double RxnThermoPropertyClass::getStandardTemperature()
{
  return StandardTemperature;
}
/*F const = getGasConstant()
**
**  DESCRIPTION
**    const: The gas constant
**
**  REMARKS
**
*/
double RxnThermoPropertyClass::getGasConstant()
{
  return GasConstant;
}
/*S RxnDataChemkinThermo
 */ 
/*F RxnDataChemkinThermo()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataChemkinThermo::RxnDataChemkinThermo()
  : UpperCoeff(7, 0.0), 
    LowerCoeff(7, 0.0)
{
  Identification = THERMO_CHEMKIN_ID;
  NameTag = THERMO_CHEMKIN_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataChemkinThermo(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataChemkinThermo::RxnDataChemkinThermo(const RxnDataChemkinThermo& data)
  : RxnDataThermoProperty(data),
    SpeciesName(data.SpeciesName),
    Date(data.Date),
    AtSymbAndFormula(data.AtSymbAndFormula),
    PhaseDescriptor(data.PhaseDescriptor),
    FormulaDescriptor(data.FormulaDescriptor),
    LowerBoundTemp(data.LowerBoundTemp),
    UpperBoundTemp(data.UpperBoundTemp),
    CommonTemp(data.CommonTemp),
    SimplyOne(data.SimplyOne),
    SimplyTwo(data.SimplyTwo),
    SimplyThree(data.SimplyThree),
    SimplyFour(data.SimplyFour),
    y(data.y),
    UpperCoeff(data.UpperCoeff),
    LowerCoeff(data.LowerCoeff)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataChemkinThermo
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataChemkinThermo::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataChemkinThermo
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataChemkinThermo::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = RxnDataThermoProperty::Read(in,objc,name);
  RxnChemkinThermoClass *chemclass = (RxnChemkinThermoClass *) objc;
  result = result && ReadBasicChemkinElement(in,chemclass);
  return result;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataChemkinThermo::ReadBasicChemkinElement(istream& in,
						   RxnChemkinThermoClass *chemclass)
{
  bool result = true;
  String word; String ChemkinLine;
  
  ChemkinLine.ReadFullLine(in);
  if(ChemkinLine.size() > 1)
    {
      SpeciesName = ChemkinLine.Isolate(0,17);
      SpeciesName.EliminateBlanks();
      Date = ChemkinLine.Isolate(18,23);
      AtSymbAndFormula=ChemkinLine.Isolate(24,43);
      PhaseDescriptor=ChemkinLine.Isolate(44,44);
      PhaseDescriptor.EliminateLeadingBlanks();
      word=ChemkinLine.Isolate(45,55);
      LowerBoundTemp=word.ToFloat();
      word=ChemkinLine.Isolate(55,65);
      UpperBoundTemp=word.ToFloat();
      word=ChemkinLine.Isolate(65,73);
      CommonTemp=word.ToFloat();
      FormulaDescriptor=ChemkinLine.Isolate(73,78);
      
      ChemkinLine.ReadFullLine(in);
      word=ChemkinLine.Isolate(0,15);
      UpperCoeff[0]=word.ToFloat();
      word=ChemkinLine.Isolate(15,30);
      UpperCoeff[1]=word.ToFloat(); 
      word=ChemkinLine.Isolate(30,45);
      UpperCoeff[2]=word.ToFloat(); 
      word=ChemkinLine.Isolate(45,60);
      UpperCoeff[3]=word.ToFloat(); 
      word=ChemkinLine.Isolate(60,75);
      UpperCoeff[4]=word.ToFloat(); 
      
      ChemkinLine.ReadFullLine(in);
      word=ChemkinLine.Isolate(0,15);
      UpperCoeff[5]=word.ToFloat();
      word=ChemkinLine.Isolate(15,30);
      UpperCoeff[6]=word.ToFloat(); 
      word=ChemkinLine.Isolate(30,45);
      LowerCoeff[0]=word.ToFloat(); 
      word=ChemkinLine.Isolate(45,60);
      LowerCoeff[1]=word.ToFloat(); 
      word=ChemkinLine.Isolate(60,75);
      LowerCoeff[2]=word.ToFloat(); 
      
      ChemkinLine.ReadFullLine(in);
      word=ChemkinLine.Isolate(0,15);
      LowerCoeff[3]=word.ToFloat();
      word=ChemkinLine.Isolate(15,30);
      LowerCoeff[4]=word.ToFloat(); 
      word=ChemkinLine.Isolate(30,45);
      LowerCoeff[5]=word.ToFloat(); 
      word=ChemkinLine.Isolate(45,60);
      LowerCoeff[6]=word.ToFloat(); 
      double stdenthalpy = CalculateEnthalpy(chemclass,chemclass->getStandardTemperature());
      double stdentropy  = CalculateEntropy(chemclass,chemclass->getStandardTemperature());
      setStandardEntropy(stdentropy,chemclass);
      setStandardEnthalpy(stdenthalpy,chemclass);
    } else 
      result = false;

  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataChemkinThermo
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataChemkinThermo::print(ostream& out) const
{
  RxnDataThermoProperty::print(out);
  return WriteAsNASAPolynomial(out);
}
 
/*F o = WriteAsNASAPolynomial(out)  . . . . . . . . . .  RxnDataChemkinThermo
**
**  DESCRIPTION
**    out: The output stream
**    o: The output stream
**
**  REMARKS
**
*/
ostream& RxnDataChemkinThermo::WriteAsNASAPolynomial(ostream& out) const
{
  out.fill(' ');
  String name = FillStringToLength(18,SpeciesName);
  out << name;
  String date = FillStringToLength(6,Date);
  out << date;
  out << AtSymbAndFormula;
  if(PhaseDescriptor.size() <= 1)
    out << setw(1) << "G";
  else
    out << setw(1) << PhaseDescriptor;
  out.width(10);
  
  out.setf(ios::fixed, ios::floatfield);
  out.precision(2);

  out << setfill(' ') << LowerBoundTemp;
  out.width(1);
  out.width(10);
  out.setf(ios::fixed, ios::floatfield);
  out.precision(2);
  out << setfill(' ') << UpperBoundTemp;
  out.width(1);
  out.width(10);
  out.setf(ios::fixed, ios::floatfield);
  out.precision(2);
  out << setfill(' ') << CommonTemp;
  out.width(3);
  out << "    ";


  out.width(1);
  out << "1";
  out << endl;
  
  //
  out.width(15);
  out.setf(ios::scientific, ios::floatfield);
  out.precision(8);
  out.setf(ios::showpos);
  out.setf(ios::showpoint);
  
  //  Coefficients a1-a5 for upper temperature region
  
  out << UpperCoeff[0];
  out.width(15);
  out << setw(15) << UpperCoeff[1];
  out << setw(15) << UpperCoeff[2];
  out << setw(15) << UpperCoeff[3];
  out << setw(15) << UpperCoeff[4];
  
  // Four blanks     
  
  out << "    " ;
  out.width(1);
  out.unsetf(ios::showpos);
  out << "2";
  out << endl;
  
  
  //   Coefficients a6-a7 for upper temperature region
  //   and a1, a2, a3 for lower temperature region
  
  out.width(15);
  out.setf(ios::showpos);
  out << setw(15) << UpperCoeff[5];
  out << setw(15) << UpperCoeff[6];
  out << setw(15) << LowerCoeff[0];
  out << setw(15) << LowerCoeff[1];
  out << setw(15) << LowerCoeff[2];
  
  //   Four blanks
  
  out << "    " ;
  out.width(1);
  out.unsetf(ios::showpos);
  out << "3";
  out << endl;
  
  //  Coefficients a4, a5, a6, a7 for lower temperature region
  
  out.width(15);
  out.setf(ios::showpos);
  out << setw(15) << LowerCoeff[3];
  out << setw(15) << LowerCoeff[4];
  out << setw(15) << LowerCoeff[5];
  out << setw(15) << LowerCoeff[6];
  
  // Fifteen blanks 
  
  out << "                   " ;
  out.width(1);
  out.unsetf(ios::showpos);
  out << "4";
  out << endl;
  
  //
  
  
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataChemkinThermo
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataChemkinThermo::Clone()
{
  RxnDataChemkinThermo *obj = new RxnDataChemkinThermo(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataChemkinThermo
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataChemkinThermo::CopyClone(Identify * obj)
{
  RxnDataChemkinThermo *objfull = (RxnDataChemkinThermo *) obj;
  *this = *objfull;
  //Parameter = (RxnData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataChemkinThermo
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataChemkinThermo::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataThermoProperty::EncodeThis(buffer);
  result = result && Encode(buffer,SpeciesName);
  result = result && Encode(buffer,Date);
  result = result && Encode(buffer,AtSymbAndFormula);
  result = result && Encode(buffer,PhaseDescriptor);
  result = result && Encode(buffer,FormulaDescriptor);
  result = result && Encode(buffer,LowerBoundTemp);
  result = result && Encode(buffer,UpperBoundTemp);
  result = result && Encode(buffer,CommonTemp);
  result = result && Encode(buffer,SimplyOne);
  result = result && Encode(buffer,SimplyTwo);
  result = result && Encode(buffer,SimplyThree);
  result = result && Encode(buffer,SimplyFour);
  result = result && Encode(buffer,y);
  //double *f;
  int nu = UpperCoeff.size();
  result = result && Encode(buffer,nu);
  vector<double>::iterator f;
  for(f=UpperCoeff.begin();
      f != UpperCoeff.end();
      f++)
    {
      result = result && Encode(buffer,*f);
    }
  
  int nl = LowerCoeff.size();
  Encode(buffer,nl);
  for(f=LowerCoeff.begin();
      f != LowerCoeff.end();
      f++)
    {
      result = result && Encode(buffer,*f);
    }
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataChemkinThermo
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataChemkinThermo::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataThermoProperty::DecodeThis(buffer);
  result = result && Decode(buffer,SpeciesName);
  result = result && Decode(buffer,Date);
  result = result && Decode(buffer,AtSymbAndFormula);
  result = result && Decode(buffer,PhaseDescriptor);
  result = result && Decode(buffer,FormulaDescriptor);
  result = result && Decode(buffer,LowerBoundTemp);
  result = result && Decode(buffer,UpperBoundTemp);
  result = result && Decode(buffer,CommonTemp);
  result = result && Decode(buffer,SimplyOne);
  result = result && Decode(buffer,SimplyTwo);
  result = result && Decode(buffer,SimplyThree);
  result = result && Decode(buffer,SimplyFour);
  result = result && Decode(buffer,y);
  int nu;
  VectorNumeric::iterator f;
  result = result && Decode(buffer,nu);
  for(f=UpperCoeff.begin();f != UpperCoeff.end();f++)
    {
      double fd;
      result = result && Decode(buffer,fd);
      *f = fd;
    }
  
  result = result && Decode(buffer,nu);
  for(f=LowerCoeff.begin();f != LowerCoeff.end();f++)
    {
      double fd;
      result = result && Decode(buffer,fd);
      *f = fd;
    }
  return result;
}

 
/*F Cp = CalculateHeatCapacity(classinfo,Temper)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
double RxnDataChemkinThermo::CalculateHeatCapacity(RxnThermoPropertyClass *classinfo,
						   const double& Temper)
{
  double CpOverR;
  if(Temper < CommonTemp)
    {
      CpOverR = LowerCoeff[0] + 
	LowerCoeff[1] *  Temper + 
	LowerCoeff[2] *  pow(Temper,2.0) + 
	LowerCoeff[3] *  pow(Temper,3.0) +
	LowerCoeff[4] *  pow(Temper,4.0);
    }
  else
    {
      CpOverR = UpperCoeff[0] + 
	UpperCoeff[1] *  Temper + 
	UpperCoeff[2] *  pow(Temper,2.0) + 
	UpperCoeff[3] *  pow(Temper,3.0) +
	UpperCoeff[4] *  pow(Temper,4.0);
    }

  return CpOverR * classinfo->getGasConstant();
}
/*F enthalpy = CalculateEnthalpy(classinfo,Temper)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
double RxnDataChemkinThermo::CalculateEnthalpy(RxnThermoPropertyClass *classinfo,
					       const double& Temper)
{
  double HOverRT;
  if(Temper < CommonTemp)
    {
      HOverRT = LowerCoeff[0] + 
	Temper * ( LowerCoeff[1] / 2.0 +
		  Temper * ( LowerCoeff[2] / 3.0 +
			    Temper * ( LowerCoeff[3] / 4.0 +
				      Temper * ( LowerCoeff[4] / 5.0 )))) +
	LowerCoeff[5] / Temper;
      
    }
  else
    {
      HOverRT = UpperCoeff[0] + 
	Temper *( UpperCoeff[1] / 2.0 +
		  Temper *( UpperCoeff[2] / 3.0 +
			    Temper *( UpperCoeff[3] / 4.0 +
				      Temper *( UpperCoeff[4] / 5.0 )))) +
	UpperCoeff[5] / Temper;
    }

  return HOverRT * Temper * classinfo->getGasConstant();
}
/*F entropy = CalculateEntropy(classinfo,Temper)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
double RxnDataChemkinThermo::CalculateEntropy(RxnThermoPropertyClass *classinfo,
					       const double& Temper) {
  cout << "RxnDataChemkinThermo::CalculateEntropy" << endl;
  double SOverRT;
  if(Temper < CommonTemp)
    {
      SOverRT = LowerCoeff[0] * log(Temper) + 
	LowerCoeff[1] *  Temper + 
	LowerCoeff[2] *  pow(Temper,2.0) / 2.0 + 
	LowerCoeff[3] *  pow(Temper,3.0) / 3.0 +
	LowerCoeff[4] *  pow(Temper,4.0) / 4.0 +
	LowerCoeff[6];
    }
  else
    {
      SOverRT = UpperCoeff[0] * log(Temper) + 
	UpperCoeff[1] *  Temper + 
	UpperCoeff[2] *  pow(Temper,2.0) / 2.0 + 
	UpperCoeff[3] *  pow(Temper,3.0) / 3.0 +
	UpperCoeff[4] *  pow(Temper,4.0) / 4.0 +
	UpperCoeff[6];
    }
  cout << "T=" << Temper << " SOverRT=" << SOverRT << " S=" 
       << SOverRT * classinfo->getGasConstant() << endl;
  return SOverRT * classinfo->getGasConstant();
}
#define COMMON_TEMP 1000.00     
#define STANDARD_TEMP 298.00

/*F BuildChemkinMatrix(VectorOfTemps) . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    VectorOfTemps: param.
**
**    Builds matrix for system of linear equations, which solution represents  Chemkin 
**    coefficients a1-a5. Continuity of heat capacity function at the common temperature
**    is taken into account and is reflected in the structure of matrix.
**    
**  REMARKS
**
*/
MatrixNumeric BuildChemkinMatrixFromTemperatures(const VectorNumeric& VectorOfTemps)
     {
     unsigned int i;
     unsigned int j;
     int count1;
     int count2;
     
     MatrixNumeric Mat1(VectorOfTemps.size()+1, VectorOfTemps.size()+1);
     for(i=0; i<Mat1.size()-1; i++)
	  for(j=0; j<Mat1[0].size(); j++)
	       Mat1[i][j]=pow(VectorOfTemps[i], (double) j);
//   Last row - derivative
     count1=Mat1.size()-1;
     count2=VectorOfTemps.size()-1;    
     Mat1[count1][0]=0.0;
     for(j=1; j<Mat1[0].size(); j++)
	  Mat1[count1][j]=j*pow(VectorOfTemps[count2], (double) j-1);
     return Mat1;
     
     }
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataChemkinThermo::RxnDataChemkinThermo(String& name,
					   String& cpunits,
					   String& enthalpyunits,
					   String& entropyunits,
					   RxnDataSimpleMolecule *molecule,
					   RxnSimpleMoleculeClass *molclass,
					   RxnDataThermoProperty *data,
					   RxnThermoPropertyClass *thermoclass)
  : Date("      "),
    UpperCoeff(7, 0.0), 
    LowerCoeff(7, 0.0)
{
     CalculateAtSymbAndFormula(molecule,molclass);
  FillInChemkinInfo(cpunits,enthalpyunits,entropyunits,data,thermoclass);
  SpeciesName = name;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataChemkinThermo::RxnDataChemkinThermo(String& cpunits,
					   String& enthalpyunits,
					   String& entropyunits,
					   RxnDataSimpleMolecule *molecule,
					   RxnSimpleMoleculeClass *molclass,
					   RxnDataThermoProperty *data,
					   RxnThermoPropertyClass *thermoclass)
  : Date("      "),
    UpperCoeff(7, 0.0), 
    LowerCoeff(7, 0.0)
{
     CalculateAtSymbAndFormula(molecule,molclass);
  FillInChemkinInfo(cpunits,enthalpyunits,entropyunits,data,thermoclass);
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataChemkinThermo::RxnDataChemkinThermo(String& cpunits,
					   String& enthalpyunits,
					   String& entropyunits,
					   RxnDataThermoProperty *data,
					   RxnThermoPropertyClass *thermoclass)
  : Date("      "),
    UpperCoeff(7, 0.0), 
    LowerCoeff(7, 0.0)
{
  FillInChemkinInfo(cpunits,enthalpyunits,entropyunits,data,thermoclass);
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void  RxnDataChemkinThermo::FillInChemkinInfo(String& cpunits,
					      String& enthalpyunits,
					      String& entropyunits,
					      RxnDataThermoProperty *data,
					      RxnThermoPropertyClass *thermoclass)
{
  Identification = THERMO_CHEMKIN_ID;
  NameTag = THERMO_CHEMKIN_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
  
  unsigned int i;
  double array1[]= {300.00, 500.00, 800.00, 1000.00};
  double array2[]= {1300.00, 1500.00, 1700.00, 1000.00};
  
  VectorNumeric LowerChemkinTemps;
  VectorNumeric UpperChemkinTemps;
  for(i=0;i<4;i++)
    {
      LowerChemkinTemps.AddObject(array1[i]);
      UpperChemkinTemps.AddObject(array2[i]);
    }
  SpeciesName=data->NameTag;
  LowerBoundTemp=LowerChemkinTemps[0];
  UpperBoundTemp=UpperChemkinTemps[2];
  CommonTemp=LowerChemkinTemps[3];
     
     
     SimplyOne=1;
     SimplyTwo=2;
     SimplyThree=3;
     SimplyFour=4;

//   ***********************************************************************
     ComputeCoeffs(LowerChemkinTemps,cpunits,data,thermoclass,LowerCoeff);
     ComputeCoeffs(UpperChemkinTemps,cpunits,data,thermoclass,UpperCoeff);

     CalculateLowerA6(data,thermoclass,enthalpyunits);
     CalculateUpperA6(data,thermoclass,enthalpyunits);
     CalculateLowerA7(data,thermoclass,entropyunits);
     CalculateUpperA7(data,thermoclass,entropyunits);

     double t;
     t = 298.0;
     //cout << setw(20) << CalculateEquilibrium(thermoclass,t) << endl;
     t = 1000.0;
     //cout << setw(20) << CalculateEquilibrium(thermoclass,t) << endl;
     t = 1500.0;
     //cout << setw(20) << CalculateEquilibrium(thermoclass,t) << endl;
     t = 2000.0;
     //cout << setw(20) << CalculateEquilibrium(thermoclass,t) << endl;
     t = 3000.0;
     //cout << setw(20) << CalculateEquilibrium(thermoclass,t) << endl;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataChemkinThermo::CalculateLowerA6(RxnDataThermoProperty *data,
					    RxnThermoPropertyClass *thermoclass, 
					    String& enthalpyunits)
{
//   Get lower a6 from enthalpy fit: a6=H0-(a1*T+a2/2*T^2+a3/3*T^3+a4/4*T^4+a5/5*T^5)
//   T=300.00 K
  double Tlower = COMMON_TEMP - 10.0;
  double enthalpy = data->CalculateEnthalpy(thermoclass,Tlower);
  double En = thermoclass->Convert(true,enthalpyunits,enthalpy);
  double  En1 = En/thermoclass->getGasConstant();
  double PEn = 0.0;
  for(unsigned int i=0; i<=5; i++) {
    double p = i+1;
    PEn += LowerCoeff[i]*pow(Tlower,p)/p;
  }
  
  LowerCoeff[5]= En1 - PEn;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataChemkinThermo::CalculateUpperA7(RxnDataThermoProperty *data,
					    RxnThermoPropertyClass *thermoclass,
					    String& entropyunits)
{
  cout << "RxnDataChemkinThermo::CalculateUpperA7" << endl;
  double Tupper = COMMON_TEMP + 10.0;
  double PEntropy=UpperCoeff[0]*log(Tupper);
  for(unsigned int i=1; i<5; i++)
    {
      double p = i;
      PEntropy+=UpperCoeff[i]*pow(Tupper, p)/p;
    }
  double entropy = data->CalculateEntropy(thermoclass,Tupper);
  double entropychemkin = thermoclass->Convert(true,entropyunits,entropy);
  UpperCoeff[6]= entropychemkin/thermoclass->getGasConstant() - PEntropy;
  cout << "Entropy=" << entropy << " Adjusted=" << entropychemkin << " A7=" << UpperCoeff[6] << endl;
} 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataChemkinThermo::CalculateLowerA7(RxnDataThermoProperty *data,
					    RxnThermoPropertyClass *thermoclass,
					    String& entropyunits)
{
  double Tlower = COMMON_TEMP - 10.0;
  //   Get lower a7 from entropy fit: a7=S0-(a1*lnT+a2*T+a3/2*T^2+a4/3*T^3+a5/4*T^4)
  //   T=300.00 K  
  double PEntropy=LowerCoeff[0]*log(Tlower);
  for(unsigned int i=1; i<5; i++)
    {
      double p = i;
      PEntropy+=LowerCoeff[i]*pow(Tlower, p)/p;
    }
  double entropy = data->CalculateEntropy(thermoclass,Tlower);
  double entropychemkin = thermoclass->Convert(true,entropyunits,entropy);
  LowerCoeff[6]= entropychemkin/thermoclass->getGasConstant() - PEntropy;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataChemkinThermo::CalculateUpperA6(RxnDataThermoProperty *data,
					    RxnThermoPropertyClass *thermoclass, 
					    String& enthalpyunits)
{
  double Tupper = COMMON_TEMP + 10.0;
  //    Get upper a6 from enthalpy fit at common temperature
  double enthalpy = data->CalculateEnthalpy(thermoclass,Tupper);
  double En = thermoclass->Convert(true,enthalpyunits,enthalpy);
  double En1 = En/thermoclass->getGasConstant();
  double PEn = 0.0;
  for(unsigned int i=0; i<=5; i++) {
    double p = i+1;
    PEn+=UpperCoeff[i]*pow(Tupper, p)/p;
     }
  UpperCoeff[5]= En1 - PEn;
}
/*F CalculateAtSymbAndFormula(molecule,molclass,atominfo) RxnDataChemkinThermo
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataChemkinThermo::CalculateAtSymbAndFormula(RxnDataSimpleMolecule *molecule,
						     RxnSimpleMoleculeClass *molclass)
{
  if(molecule != NULL)
    {
     RxnMoleculeAtomClass *atomclass = (RxnMoleculeAtomClass *) molclass->NodeClass;
     RxnBasicAtomDataClass *atomdataclass = atomclass->getBasicAtomDataClass();
     RxnDataAtomInformation  *atominfo = atomdataclass->getAtomInformation();
     IntegerPropertyFromNumericOperation op;
     RxnDataAtomStatistics atomstats(*molecule,"Valence",*atominfo,&op);

     ObjectList<String> names = atomstats.ListOfObjectNames();
     AtSymbAndFormula = "";
     for(ObjectList<String>::iterator aname = names.begin();
	 aname != names.end();
	 aname++)
       {
	 AtSymbAndFormula.AppendToEnd(*aname);
	 String blank(" ");
	 if((*aname).size() == 2)
	   AtSymbAndFormula.AppendToEnd(blank);
	 BaseDataInteger *avalue = (BaseDataInteger *) atomstats.GetObject(*aname);
	 String prefix("");
	 String num = PositveIntegerToString(avalue->GetValue(),prefix,3);
	 AtSymbAndFormula.AppendToEnd(num);
       }
     unsigned int n = names.size();
     String Blanks5("     ");
     while(n < 4)
       {
	 n++;
	 AtSymbAndFormula.AppendToEnd(Blanks5);
       }
    }
  else
    {
      AtSymbAndFormula = "                    ";
    }
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataChemkinThermo::ComputeCoeffs(VectorNumeric &ChemkinTemps,
					 String& cpunits,
					 RxnDataThermoProperty *data,
					 RxnThermoPropertyClass *thermoclass,
					 VectorNumeric &Coeffs)
{
  unsigned int nsize = ChemkinTemps.size()+1;
  VectorNumeric ChemkinCpS(nsize);     

  MatrixNumeric origmat(nsize,1);
  unsigned int count = 0;
  VectorNumeric::iterator iter;
  for(iter=ChemkinTemps.begin();
      iter != ChemkinTemps.end();
      iter++)
    {
      double cp = data->CalculateHeatCapacity(thermoclass,*iter);
      ChemkinCpS[count] = thermoclass->Convert(true,cpunits,cp);
      origmat[count][0] = ChemkinCpS[count];
      count++;
    }
  double deriv = data->CalculateHeatCapacityDerivative(thermoclass,COMMON_TEMP);
  ChemkinCpS[nsize-1] = thermoclass->Convert(true,cpunits,deriv);
  origmat[nsize-1][0] = ChemkinCpS[count];

  VectorNumeric norm_vector=ChemkinTemps;
  for(iter = norm_vector.begin();iter !=norm_vector.end();iter++)
    *iter /= COMMON_TEMP;
  MatrixNumeric MatForFit=BuildChemkinMatrixFromTemperatures(norm_vector);
  MatrixNumeric tempmat = MatForFit;     

  MatrixUtilities matutil;

  MatrixNumeric unit(nsize,nsize);
  VectorNumeric c(nsize);
  for(unsigned int j1=0;j1<nsize;j1++)
    {
      for(unsigned int j2=0;j2<nsize;j2++)
	unit[j1][j2] = 0.0;
      unit[j1][j1] = 1.0;
      c[j1] = 0.0;
    }

  matutil.gauss_jordan_invert(MatForFit,unit,c,nsize);
  MatrixNumeric ans1 = unit * origmat;
  
  for(unsigned i=0; i<nsize; i++)
    Coeffs[i]=ans1[i][0]/(thermoclass->getGasConstant() * pow(COMMON_TEMP,(int) i));
}
 
/*F name = getSpeciesName() . . . . . . . . . . . . . .  RxnDataChemkinThermo
**
**  DESCRIPTION
**    name: The name of the chemkin species
**
**  REMARKS
**
*/
String& RxnDataChemkinThermo::getSpeciesName()
{
  return SpeciesName;
}

/*S RxnChemkinThermoClass
 */
/*F RxnChemkinThermoClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnChemkinThermoClass::RxnChemkinThermoClass()
{
  Identification = THERMO_CHEMKIN_ID;
  NameTag = THERMO_CHEMKIN_NAME;
  SubClass = "ThermoProperty";
  EncodeDecodeClass = NameTag;
} 
/*F RxnChemkinThermoClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnChemkinThermoClass::RxnChemkinThermoClass(const RxnChemkinThermoClass& data)
  : RxnThermoPropertyClass(data)
{
} 
 
/*F RxnChemkinThermoClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnChemkinThermoClass::RxnChemkinThermoClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnThermoPropertyClass(id,name,descr)
{
  SubClass = "ThermoProperty";
  EncodeDecodeClass = THERMO_CHEMKIN_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnChemkinThermoClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnChemkinThermoClass::print(ostream& out) const
{
  RxnThermoPropertyClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnChemkinThermoClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnChemkinThermoClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnChemkinThermoClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnThermoPropertyClass::Read(in,set);
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnChemkinThermoClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnChemkinThermoClass::CopyClone(Identify *  objc)
{
  RxnChemkinThermoClass *objcfull = (RxnChemkinThermoClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnChemkinThermoClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnChemkinThermoClass::Clone()
    {
      RxnChemkinThermoClass* id = new RxnChemkinThermoClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnChemkinThermoClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnChemkinThermoClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnThermoPropertyClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnChemkinThermoClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnChemkinThermoClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnThermoPropertyClass::DecodeThis(buffer);
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
BaseDataObject * RxnChemkinThermoClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataChemkinThermo();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnChemkinThermoClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnChemkinThermoClass*& obj)
     {
     obj = new RxnChemkinThermoClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataChemkinThermo
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataChemkinThermo*& obj)
     {
     obj = new RxnDataChemkinThermo;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataBensonThermo
 */ 
/*F RxnDataBensonThermo()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataBensonThermo::RxnDataBensonThermo()
{
  Identification = THERMO_BENSON_ID;
  NameTag = THERMO_BENSON_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataBensonThermo(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataBensonThermo::RxnDataBensonThermo(const RxnDataBensonThermo& data)
  : RxnDataThermoProperty(data),
    BensonFormulaDescriptor(data.BensonFormulaDescriptor),
    CpS(data.CpS),
    GapTrack(data.GapTrack),
    PolyFlag(data.PolyFlag),
    CommonBensonTemp(data.CommonBensonTemp)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataBensonThermo
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataBensonThermo::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataBensonThermo
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataBensonThermo::Read(istream& in, DataObjectClass* objc, const String& name)
{
  //bool result = RxnDataThermoProperty::Read(in,objc,name);
  RxnBensonThermoClass *bensonclass = (RxnBensonThermoClass *) objc;
  bool result = true;
  unsigned int i;

  VectorNumeric ExistData;
  VectorNumeric TempsForExistData;
  VectorNumeric Data(bensonclass->getNumberOfTemperatures());

  StreamObjectInput str(in,' ');
      
  String StandardEnthalpyS = str.ReadNext();
  setStandardEnthalpy(StandardEnthalpyS.ToFloat(),bensonclass);

  String StandardEntropyS = str.ReadNext();
  setStandardEntropy(StandardEntropyS.ToFloat(),bensonclass);     

  for(i=0; i < bensonclass->getNumberOfTemperatures(); i++)
    {
      String word = str.ReadNext();
      if(word[0]==GAP_FILL_CHAR)
	{
	  GapTrack.push_back(IS_GAP);
	} 
      else
	{
	  GapTrack.push_back(NO_GAP);
	  Data[i]=word.ToFloat();
	  ExistData.push_back(word.ToFloat());
	  TempsForExistData.push_back(bensonclass->getTemperature(i));
	}
    }
  
  CpS = Data;
  if(ExistData.size() != CpS.size())
    {
      FillHeatCapacityGaps(bensonclass,TempsForExistData, ExistData);
    }
  else
    {
      PolyFlag=1;
    };
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataBensonThermo
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataBensonThermo::print(ostream& out) const
{
  RxnDataThermoProperty::print(out);
  int i;     
  
  out.width(22);
  out << BensonFormulaDescriptor;
  
  
  out.setf(ios::fixed, ios::floatfield);
  out.setf(ios::showpos);
  out.setf(ios::showpoint);
  out.precision(2);
  
  out.width(14);
  //out << getStandardEnthalpy();
  //out.width(10);
  //out << getStandardEntropy();
  
  int tmpsize = CpS.size();     
  for(i=0; i<tmpsize; i++)
    {
      out.width(1);
      out.width(10);
      out << CpS[i];
    }
  out << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataBensonThermo
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataBensonThermo::Clone()
{
  RxnDataBensonThermo *obj = new RxnDataBensonThermo(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataBensonThermo
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataBensonThermo::CopyClone(Identify * obj)
{
  RxnDataBensonThermo *objfull = (RxnDataBensonThermo *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataBensonThermo
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBensonThermo::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataThermoProperty::EncodeThis(buffer);
  result = result && Encode(buffer,BensonFormulaDescriptor);
  result = result && Encode(buffer,CpS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataBensonThermo
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBensonThermo::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataThermoProperty::DecodeThis(buffer);
  result = result && Decode(buffer,BensonFormulaDescriptor);
  result = result && Decode(buffer,CpS);
  return result;
}
/*F CalculateHeatCapacityDerivative(classinfo,Temper) . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.
**
**    Calculates heat capacity derivative for some particular temperature. 
**    Uses information (namely value of PolyFlag) which interpolation method
**    was used for constructing CpS data.
**      
**  REMARKS
**
*/
double RxnDataBensonThermo::CalculateHeatCapacityDerivative(RxnThermoPropertyClass *classinfo, const double& Temper)
     {
     double summa;
     RxnBensonThermoClass *bensonclass = (RxnBensonThermoClass *) classinfo;
          
     if(PolyFlag==1)
	  {	  
	  VectorNumeric sigmas(CpS.size(), 0.0);
	  VectorNumeric params(CpS.size(), 0.0);  
   
	  PolyLeastSquares XXX(CpS.size());
	  
	  XXX.Calculate(bensonclass->getTemperatures(), CpS, sigmas, params);
	  
          summa=0.0;
	  int parsize = params.size();
	  for(int i=1;i<parsize;i++)
	       summa+=i*params[i]*pow(Temper,i-1);
	  
	  }          
     else
	  {
          VectorNumeric sigmas(CpS.size(), 0.0);
	  VectorNumeric params(3, 0.0);
	  
	  LogLeastSquares YYY(CpS.size());
	  
	  YYY.Calculate(bensonclass->getTemperatures(), CpS, sigmas, params);
	
          summa=0.0;  	  
	  summa+=params[1]/(1+Temper);
	  summa+=params[2];
              	  	 	  
	  };
     
     return summa;
                         
     }
/*F CalculateHeatCapacity(classinfo,Temper) . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.

**    Calculates heat capacity for some particular temperature.
**        
**  REMARKS
**
*/
double RxnDataBensonThermo::CalculateHeatCapacity(RxnThermoPropertyClass *classinfo, const double& Temper)
{
  int i;
  double summa;
  RxnBensonThermoClass *bensonclass = (RxnBensonThermoClass *) classinfo;
  
  summa=0.0;
  
  if (PolyFlag==1)
    {
      VectorNumeric sigmas(bensonclass->getNumberOfTemperatures(),0.0);
      VectorNumeric params(bensonclass->getNumberOfTemperatures(),0.0);
      PolyLeastSquares XXX(bensonclass->getNumberOfTemperatures());
      XXX.Calculate(bensonclass->getTemperatures(), CpS, sigmas, params);
      int parsize = params.size();
      for(i=0;i<parsize;i++)
	summa+=params[i]*pow(Temper,i);	     
    } 
  else 
    {
      VectorNumeric sigmas(3,0.0);
      VectorNumeric params(3,0.0);
      LogLeastSquares YYY(bensonclass->getNumberOfTemperatures());
      YYY.Calculate(bensonclass->getTemperatures(), CpS, sigmas, params);
      summa=params[0];	       
      summa+=params[1]*log(1+Temper);
      summa+=params[2]*Temper;       
    }; 
  return summa;  
}
/*F FillHeatCapacityGaps(TempsForExistData, ExistData)  . . . . . . . . . .  
**
**  DESCRIPTION
**    TempsForExistData: param.
**    ExistData: param.

**    Calculates heat capacity for some particular temperature. Uses information
**    (namely value of PolyFlag) which interpolation method was used for constructing
**    CpS data.
**        
**  REMARKS
**
*/
void RxnDataBensonThermo::FillHeatCapacityGaps(RxnThermoPropertyClass *classinfo,
					       const VectorNumeric& TempsForExistData,
					       const VectorNumeric& ExistData)
{
    int i;
    int j;
    RxnBensonThermoClass *bensonclass = (RxnBensonThermoClass *) classinfo;
    
    if(ExistData.size()>3)
	{	  
	    VectorNumeric sigmas(ExistData.size(), 0.0);
	    VectorNumeric params(ExistData.size(), 0.0);  
	    
	    PolyLeastSquares XXX(ExistData.size());
	    XXX.Calculate(TempsForExistData, ExistData, sigmas, params);
	    fill(CpS.begin(), CpS.end(), 0.0);
	    int cpssize = CpS.size();
	    int parsize = params.size();
	    for(i=0;i<cpssize;i++)
		for(j=0;j<parsize;j++)
		    CpS[i]+=params[j]*pow(bensonclass->getTemperature(i),j);
	    PolyFlag=1;
	}          
    else
	{
	    VectorNumeric sigmas(3, 0.0);
	    VectorNumeric params(3, 0.0);
	    
	    LogLeastSquares YYY(ExistData.size());
	    YYY.Calculate(TempsForExistData, ExistData, sigmas, params);
	    fill(CpS.begin(), CpS.end(), 0.0);  
	    int cpssize = CpS.size();
	    for(i=0;i<cpssize;i++)
		{
		    CpS[i]=params[0];
		    
		    CpS[i]+=params[1]*log(1+bensonclass->getTemperature(i));
		    CpS[i]+=params[2]*(bensonclass->getTemperature(i));
		};	  
	    
	    PolyFlag=2;
	    
	};
}
/*F value = CalculateEntropy(classinfo,Temper)  . . . . . RxnDataBensonThermo
**
**  DESCRIPTION
**    classinfo: The property class
**    Temper: The temperature at which to calculate
**    value: The entropy value
**
**  REMARKS
**
*/
double RxnDataBensonThermo::CalculateEntropy(RxnThermoPropertyClass *classinfo,
					     const double& Temper)
{
  double value = 0.0;
  return value;
}
 
/*F value = CalculateEnthapy(classinfo,Temper)  . . . . . RxnDataBensonThermo
**
**  DESCRIPTION
**    classinfo: The property class
**    Temper: The temperature at which to calculate
**    value: The enthalpy value
**    
**  REMARKS
**
*/
double RxnDataBensonThermo::CalculateEnthalpy(RxnThermoPropertyClass *classinfo,
					      const double& Temper)
{
  double value = 0.0;
  return value;
}

/*F temps = getTemperatures() . . . . . . . . . . . . .  RxnBensonThermoClass
**
**  DESCRIPTION
**    temps: The temperatures for the benson thermo class
**
**  REMARKS
**
*/
VectorNumeric& RxnBensonThermoClass::getTemperatures()
{
    return Temps;
}

/*F temp = getTemperature(i)  . . . . . . . . . . . . .  RxnBensonThermoClass
**
**  DESCRIPTION
**    i: Which temperature in the set to get
**    temp: The temperature
**
**  REMARKS
**
*/
double RxnBensonThermoClass::getTemperature(unsigned int i)
{
    return Temps[i];
}

/*F num = getNumberOfTemperatures() . . . . . . . . . .  RxnBensonThermoClass
**
**  DESCRIPTION
**    num: The number of temperatures
**
**  REMARKS
**
*/
unsigned int RxnBensonThermoClass::getNumberOfTemperatures()
{
    return Temps.size();
}

/*S RxnBensonThermoClass
 */
/*F RxnBensonThermoClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnBensonThermoClass::RxnBensonThermoClass()
{
  Identification = THERMO_BENSON_ID;
  NameTag = THERMO_BENSON_NAME;
  SubClass = "ThermoProperty";
  EncodeDecodeClass = NameTag;
} 
/*F RxnBensonThermoClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnBensonThermoClass::RxnBensonThermoClass(const RxnBensonThermoClass& data)
  : RxnThermoPropertyClass(data),
    Temps(data.Temps)
{
} 
 
/*F RxnBensonThermoClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnBensonThermoClass::RxnBensonThermoClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnThermoPropertyClass(id,name,descr)
{
  SubClass = "ThermoProperty";
  EncodeDecodeClass = THERMO_BENSON_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnBensonThermoClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnBensonThermoClass::print(ostream& out) const
{
    RxnThermoPropertyClass::print(out);
    out << endl;
    out << "Temperatures: ";
    Temps.print(out);
    out << endl;
    return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnBensonThermoClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnBensonThermoClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnBensonThermoClass::Read(istream& in, DataSetOfObjectsClass &set)
{
    bool result = RxnThermoPropertyClass::Read(in,set);
    Temps.Read(in);
    return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnBensonThermoClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnBensonThermoClass::CopyClone(Identify *  objc)
{
  RxnBensonThermoClass *objcfull = (RxnBensonThermoClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnBensonThermoClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnBensonThermoClass::Clone()
{
    RxnBensonThermoClass* id = new RxnBensonThermoClass(*this);
    return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnBensonThermoClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBensonThermoClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnThermoPropertyClass::EncodeThis(buffer);
  result = result && Encode(buffer,Temps);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnBensonThermoClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBensonThermoClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnThermoPropertyClass::DecodeThis(buffer);
  result = result && Decode(buffer,Temps);
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
BaseDataObject * RxnBensonThermoClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataBensonThermo();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnBensonThermoClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnBensonThermoClass*& obj)
     {
     obj = new RxnBensonThermoClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataBensonThermo
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataBensonThermo*& obj)
{
  obj = new RxnDataBensonThermo;
  return obj->DecodeThis(buffer);
}
/*S RxnDataPolynomialCp
 */ 
/*F RxnDataPolynomialCp()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataPolynomialCp::RxnDataPolynomialCp()
{
  Identification = THERMO_POLY_ID;
  NameTag = THERMO_POLY_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataPolynomialCp(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataPolynomialCp::RxnDataPolynomialCp(const RxnDataPolynomialCp& data)
  : RxnDataThermoProperty(data),
    Cps(data.Cps)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataPolynomialCp
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataPolynomialCp::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataPolynomialCp
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataPolynomialCp::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = RxnDataThermoProperty::Read(in,objc,name);
  RxnThermoPropertyClass *cpclass = (RxnThermoPropertyClass *) objc;

  StreamObjectInput str(in,' ');
  String enthalpyS = str.ReadNext();
  String entropyS = str.ReadNext();
  
  setStandardEnthalpy( enthalpyS.ToFloat(),cpclass );
  setStandardEntropy( entropyS.ToFloat(), cpclass );

  return result && Cps.Read(in);
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataPolynomialCp
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataPolynomialCp::print(ostream& out) const
{
  RxnDataThermoProperty::print(out);
  Cps.print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataPolynomialCp
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataPolynomialCp::Clone()
{
  RxnDataPolynomialCp *obj = new RxnDataPolynomialCp(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataPolynomialCp
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataPolynomialCp::CopyClone(Identify * obj)
{
  RxnDataPolynomialCp *objfull = (RxnDataPolynomialCp *) obj;
  *this = *objfull;
} 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataPolynomialCp
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataPolynomialCp::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataThermoProperty::EncodeThis(buffer);
  result = result && Cps.EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataPolynomialCp
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataPolynomialCp::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataThermoProperty::DecodeThis(buffer);
  result = result && Cps.DecodeThis(buffer);
  return result;
}
 
/*F value = CalculateHeatCapacity(classinfo,Temper) RxnDataPolynomialCp
**
**  DESCRIPTION
**    classinfo: The property class
**    Temper: The temperature
**    value: The heat capacity
**
**  REMARKS
**
*/
double RxnDataPolynomialCp::CalculateHeatCapacity(RxnThermoPropertyClass *classinfo,
						  const double& Temper)
{
  double value = 0.0;
  double tempvalue = Temper/1000.0;
  unsigned int n = Cps.size();
  for(unsigned int i=0; i < n; i++)
    {
      double p = (double) i;
      value += Cps[i] * pow(tempvalue,p);
    }
  return value;
}
/*F value = CalculateEntropy(classinfo,Temper)  . . . . . RxnDataPolynomialCp
**
**  DESCRIPTION
**    classinfo: The property class
**    Temper: The temperature
**    value: The entropy
**    
**  REMARKS
**
*/
double RxnDataPolynomialCp::CalculateEntropy(RxnThermoPropertyClass *classinfo,
					     const double& Temper)
{
  double value = 0.0;
  double tempvalue = Temper/1000.0;
  double temp = classinfo->getStandardTemperature()/1000.0;
  unsigned int n = Cps.size();
  if(n > 0)
    value += Cps[0]*log(tempvalue/temp);
  if(n > 1)
    value += Cps[1]*(tempvalue-temp);
  for(unsigned int i=2; i < n; i++)
    {
      double p = (double) i;
      double tl = pow(temp,p);
      double tu = pow(tempvalue,p);
      double cp = Cps[i];
      value += cp * (tu-tl)/p;
    }
  value = value + getStandardEntropy(classinfo);
  return value;
}
/*F value = CalculateEnthalpy(classinfo,Temper) . . . . . RxnDataPolynomialCp
**
**  DESCRIPTION
**    classinfo: The property class
**    Temper: The temperature
**    value: The enthalpy
**    
**  REMARKS
**
*/
double RxnDataPolynomialCp::CalculateEnthalpy(RxnThermoPropertyClass *classinfo,
					      const double& Temper)
{
  double value = 0.0;
  double tempvalue = Temper/1000.0;
  double temp = classinfo->getStandardTemperature()/1000.0;
  unsigned int n = Cps.size();
  for(unsigned int i=0; i < n; i++)
    {
      double p = (double) i+1;
      double tl = pow(temp,p);
      double tu = pow(tempvalue,p);
      double cp = Cps[i];
      value += cp * (tu-tl)/p;
    }
  value = value + getStandardEnthalpy(classinfo);
  return value;  
}

/*S RxnPolynomialCpClass
 */
/*F RxnPolynomialCpClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnPolynomialCpClass::RxnPolynomialCpClass()
{
  Identification = THERMO_POLY_ID;
  NameTag = THERMO_POLY_NAME;
  SubClass = "ThermoProperty";
  EncodeDecodeClass = NameTag;
} 
/*F RxnPolynomialCpClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnPolynomialCpClass::RxnPolynomialCpClass(const RxnPolynomialCpClass& data)
  : RxnThermoPropertyClass(data)
{
} 
 
/*F RxnPolynomialCpClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnPolynomialCpClass::RxnPolynomialCpClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnThermoPropertyClass(id,name,descr)
{
  SubClass = "ThermoProperty";
  EncodeDecodeClass = "PolynomialCp";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnPolynomialCpClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnPolynomialCpClass::print(ostream& out) const
{
  RxnThermoPropertyClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnPolynomialCpClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnPolynomialCpClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnPolynomialCpClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnThermoPropertyClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnPolynomialCpClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnPolynomialCpClass::CopyClone(Identify *  objc)
{
  RxnPolynomialCpClass *objcfull = (RxnPolynomialCpClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnPolynomialCpClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnPolynomialCpClass::Clone()
    {
      RxnPolynomialCpClass* id = new RxnPolynomialCpClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnPolynomialCpClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnPolynomialCpClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnThermoPropertyClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnPolynomialCpClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnPolynomialCpClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnThermoPropertyClass::DecodeThis(buffer);
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
BaseDataObject * RxnPolynomialCpClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataPolynomialCp();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnPolynomialCpClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnPolynomialCpClass*& obj)
     {
     obj = new RxnPolynomialCpClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataPolynomialCp
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataPolynomialCp*& obj)
     {
     obj = new RxnDataPolynomialCp;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataThermoValuesAlgorithm
 */ 
/*F RxnDataThermoValuesAlgorithm()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataThermoValuesAlgorithm::RxnDataThermoValuesAlgorithm()
  : ValueUnitsS("ThermoUnits"),
    instanceNameListS("InstanceNameList"),
    temperaturesS("Temperatures"),
    matrixOutS("MatrixObject")
{
  Identification = THERMO_THERMVALS_ID;
  NameTag = THERMO_THERMVALS_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataThermoValuesAlgorithm(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataThermoValuesAlgorithm::RxnDataThermoValuesAlgorithm(const RxnDataThermoValuesAlgorithm& data)
  : BaseDataAlgorithmOperation(data),
    ValueUnitsS(data.ValueUnitsS),
    instanceNameListS(data.instanceNameListS),
    temperaturesS(data.temperaturesS),
    matrixOutS(data.matrixOutS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataThermoValuesAlgorithm
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataThermoValuesAlgorithm::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataThermoValuesAlgorithm
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataThermoValuesAlgorithm::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataThermoValuesAlgorithm
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataThermoValuesAlgorithm::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataThermoValuesAlgorithm
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataThermoValuesAlgorithm::Clone()
{
  RxnDataThermoValuesAlgorithm *obj = new RxnDataThermoValuesAlgorithm(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataThermoValuesAlgorithm
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataThermoValuesAlgorithm::CopyClone(Identify * obj)
{
  RxnDataThermoValuesAlgorithm *objfull = (RxnDataThermoValuesAlgorithm *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataThermoValuesAlgorithm
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataThermoValuesAlgorithm::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,ValueUnitsS);
  result = result && Encode(buffer,instanceNameListS);
  result = result && Encode(buffer,temperaturesS);
  result = result && Encode(buffer,matrixOutS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataThermoValuesAlgorithm
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataThermoValuesAlgorithm::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,ValueUnitsS);
  result = result && Decode(buffer,instanceNameListS);
  result = result && Decode(buffer,temperaturesS);
  result = result && Decode(buffer,matrixOutS);
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
bool RxnDataThermoValuesAlgorithm::SetUpAlgorithms(BaseDataSetOfInstances*,
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
bool RxnDataThermoValuesAlgorithm::CheckInput(BaseDataSetOfInstances *instances,
						 DataSetOfInstancesClass *instancesclass,
						 BaseDataAlgorithmRun *run,
						 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  if(run->ParameterInList(ValueUnitsS))
    {
      if(run->ParameterInList(instanceNameListS))
	{
	  if(run->ParameterInList(temperaturesS))
	    {
	    }
	  else
	    {
	      cerr << "The temperature list '" << temperaturesS << "' was not in the list of parameters";
	      result = false;
	    }
	}
      else
	{
	  cerr << "The instance list '" << instanceNameListS << "' was not in the list of parameters";
	  result = false;
	}
    }
  else
    {
      cerr << "The value units '" << ValueUnitsS << "' was not in the list of parameters";
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
bool RxnDataThermoValuesAlgorithm::SetUpInput(BaseDataSetOfInstances* instances,
						 DataSetOfInstancesClass* instancesclass,
						 BaseDataAlgorithmRun* run,
						 DataAlgorithmRunClass* rclass)
{
  bool result = true;

  ValueUnits = (BaseDataKeyWords *) run->ParameterValue(ValueUnitsS);
  instanceNameList = (BaseDataKeyWords *) run->ParameterValue(instanceNameListS);
  temperatures = (BaseDataDoubleVector *) run->ParameterValue(temperaturesS);
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
bool RxnDataThermoValuesAlgorithm::Calculate(BaseDataSetOfInstances *instances,
					      DataSetOfInstancesClass *instancesclass,
					      BaseDataAlgorithmRun *run,
					      DataAlgorithmRunClass *runclass)
{
  bool result = true;
  ObjectList<String> valuekeys = ValueUnits->GetKeyWords();
  ObjectList<String> inputList = instanceNameList->GetKeyWords();

  String thermoS = valuekeys.front();
  valuekeys.pop_front();
  BaseDataKeyWords keys(valuekeys);
  DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
  for(ObjectList<String>::iterator name = inputList.begin(); 
      name != inputList.end();
      name++)
    {
      BaseDataInstance *instance = instances->GetInstance(*name);
      if(instance->IsInList(thermoS))
	{
	  cout << "Instance: " << instance->NameTag << endl;
	  RxnDataThermoProperty *thermo = (RxnDataThermoProperty *) instance->GetObject(thermoS);
	  RxnThermoPropertyClass *thermoclass = (RxnThermoPropertyClass *) classes->GetObjectClass(thermo->GetType());
	  BaseDataDoubleMatrix *mat = thermo->CalculateThermoUnderUnits(&keys,thermoclass,temperatures);
	  mat->NameTag = matrixOutS;
	  instance->AddObject(mat);
	  delete mat;
	}
      else
	{
	  cerr << "Thermodynamic property: '" << thermoS << "' not found in '" << *name << "'" << endl;
	  result = false;
	}
    }
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
bool RxnDataThermoValuesAlgorithm::WriteOutputValues(BaseDataSetOfInstances*,
							DataSetOfInstancesClass*,
							BaseDataAlgorithmRun* run,
							DataAlgorithmRunClass*)
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
bool RxnDataThermoValuesAlgorithm::ConcludeRun(BaseDataSetOfInstances*,
						  DataSetOfInstancesClass*,
						  BaseDataAlgorithmRun*,
						  DataAlgorithmRunClass*)
{
  bool result = true;
//  delete something
  return result;
}
 

/*S RxnThermoValuesAlgorithmClass
 */
/*F RxnThermoValuesAlgorithmClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnThermoValuesAlgorithmClass::RxnThermoValuesAlgorithmClass()
{
  Identification = THERMO_THERMVALS_ID;
  NameTag = THERMO_THERMVALS_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnThermoValuesAlgorithmClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnThermoValuesAlgorithmClass::RxnThermoValuesAlgorithmClass(const RxnThermoValuesAlgorithmClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnThermoValuesAlgorithmClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnThermoValuesAlgorithmClass::RxnThermoValuesAlgorithmClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "ThermoValuesAlgorithm";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnThermoValuesAlgorithmClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnThermoValuesAlgorithmClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnThermoValuesAlgorithmClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnThermoValuesAlgorithmClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnThermoValuesAlgorithmClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnThermoValuesAlgorithmClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnThermoValuesAlgorithmClass::CopyClone(Identify *  objc)
{
  RxnThermoValuesAlgorithmClass *objcfull = (RxnThermoValuesAlgorithmClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnThermoValuesAlgorithmClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnThermoValuesAlgorithmClass::Clone()
    {
      RxnThermoValuesAlgorithmClass* id = new RxnThermoValuesAlgorithmClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnThermoValuesAlgorithmClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnThermoValuesAlgorithmClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnThermoValuesAlgorithmClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnThermoValuesAlgorithmClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnThermoValuesAlgorithmClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataThermoValuesAlgorithm();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnThermoValuesAlgorithmClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnThermoValuesAlgorithmClass*& obj)
     {
     obj = new RxnThermoValuesAlgorithmClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataThermoValuesAlgorithm
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataThermoValuesAlgorithm*& obj)
     {
     obj = new RxnDataThermoValuesAlgorithm;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataConvertToChemkin
 */ 
/*F RxnDataConvertToChemkin()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataConvertToChemkin::RxnDataConvertToChemkin()
  : instanceNameListS("InstanceNameList"),
    thermoInfoS("ThermoInfo"),
    unitsS("ThermoUnits")
{
  Identification = THERMO_CHEMKINCONV_ID;
  NameTag = THERMO_CHEMKINCONV_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataConvertToChemkin(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataConvertToChemkin::RxnDataConvertToChemkin(const RxnDataConvertToChemkin& data)
  : BaseDataAlgorithmOperation(data),
    instanceNameListS(data.instanceNameListS),
    thermoInfoS(data.thermoInfoS),
    unitsS(data.unitsS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataConvertToChemkin
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataConvertToChemkin::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataConvertToChemkin
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataConvertToChemkin::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataConvertToChemkin
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataConvertToChemkin::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Inputs: " << instanceNameListS << " and " << thermoInfo << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataConvertToChemkin
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataConvertToChemkin::Clone()
{
  RxnDataConvertToChemkin *obj = new RxnDataConvertToChemkin(*this);
  return obj;
}
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataConvertToChemkin
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataConvertToChemkin::CopyClone(Identify * obj)
{
  RxnDataConvertToChemkin *objfull = (RxnDataConvertToChemkin *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataConvertToChemkin
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataConvertToChemkin::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,instanceNameListS);
  result = result && Encode(buffer,thermoInfoS);
  result = result && Encode(buffer,unitsS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataConvertToChemkin
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataConvertToChemkin::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,instanceNameListS);
  result = result && Decode(buffer,thermoInfoS);
  result = result && Decode(buffer,unitsS);
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
bool RxnDataConvertToChemkin::SetUpAlgorithms(BaseDataSetOfInstances*,
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
bool RxnDataConvertToChemkin::CheckInput(BaseDataSetOfInstances*,
					  DataSetOfInstancesClass*,
					  BaseDataAlgorithmRun *run,
					  DataAlgorithmRunClass*)
{
  bool result = true;

  if(run->ParameterInList(instanceNameListS))
    {
      if(run->ParameterInList(thermoInfoS))
	{
	  if(run->ParameterInList(unitsS))
	    {
	    }
	  else
	    {
	      cerr << "The cp, enthalpy, entropy unit conversions  attribute list '" << unitsS;
	      cerr << "' was not in the list of parameters";
	      result = false;
	    }
	}
      else
	{
	  cerr << "The chemkin class and thermo attribute list '" << thermoInfoS << "' was not in the list of parameters";
	  result = false;
	}
    }
  else
    {
      cerr << "The list of instance names '" << instanceNameListS << "' was not in the list of parameters";
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
bool RxnDataConvertToChemkin::SetUpInput(BaseDataSetOfInstances* instances,
					  DataSetOfInstancesClass* instancesclass,
					  BaseDataAlgorithmRun *run,
					  DataAlgorithmRunClass* rclass)
{
  bool result = true;
  DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
  instanceNameList = (BaseDataKeyWords *) run->ParameterValue(instanceNameListS);
  thermoInfo = (BaseDataKeyWords *) run->ParameterValue(thermoInfoS);
  unitkeys = (BaseDataKeyWords *) run->ParameterValue(unitsS);

  ObjectList<String> info = thermoInfo->GetKeyWords();
  if(info.size() == 4)
    {
      chemkinS = info.front();
      info.pop_front();
      thermoS = info.front();
      info.pop_front();
      chemkinNameS = info.front();
      info.pop_front();
      moleculeS = info.front();
      info.pop_front();
      if(classes->IsInList(chemkinS))
	{
	  ObjectList<String> units = unitkeys->GetKeyWords();
	  cpunits = units.front();
	  units.pop_front();
	  enthalpyunits = units.front();
	  units.pop_front();
	  entropyunits = units.front();
	  units.pop_front();
	}
      else
	{
	  cerr << "Chemkin class '" << chemkinS << "' was not found" << endl;
	  result = false;
	}
    }
  else
    {
      cerr << "Not enough arguments in '" << thermoInfoS << "' expected:" << endl;
      cerr << "   ChemkinClass: The class of the resulting chemkin" << endl;
      cerr << "   Thermo:       The name of the input thermo quantity" << endl;
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
bool RxnDataConvertToChemkin::Calculate(BaseDataSetOfInstances *instances,
					DataSetOfInstancesClass *instanceclasses,
					BaseDataAlgorithmRun *,
					DataAlgorithmRunClass *)
{
  bool result = true;

  BaseDataKeyWords *keys = new BaseDataKeyWords();
  keys->AddKeyWord(cpunits);
  keys->AddKeyWord(enthalpyunits);
  keys->AddKeyWord(entropyunits);


  BaseDataDoubleVector *temperatures = new BaseDataDoubleVector(7);
  temperatures->CurrentVector()[0] = 300;
  temperatures->CurrentVector()[1] = 500;
  temperatures->CurrentVector()[2] = 800;
  temperatures->CurrentVector()[3] = 1000;
  temperatures->CurrentVector()[4] = 1300;
  temperatures->CurrentVector()[5] = 1500;
  temperatures->CurrentVector()[6] = 1700;
  

  RxnThermoPropertyClass *thermoclass = (RxnThermoPropertyClass *) 
    instanceclasses->GetObjectClass(chemkinS);
  RxnSimpleMoleculeClass *molclass = (RxnSimpleMoleculeClass *)
    instanceclasses->GetObjectClass(moleculeS);

  ObjectList<String> names = instanceNameList->GetKeyWords();
  ObjectList<String>::iterator iname;
  for(iname = names.begin();iname != names.end(); iname++)
    {
      BaseDataInstance *instance = (BaseDataInstance *) instances->GetInstance(*iname);
      RxnDataSimpleMolecule *molecule = NULL;
      if(instance->IsInList(molclass->GetNameInInstance()))
	{
	  molecule = (RxnDataSimpleMolecule *) instance->GetObject(molclass->GetNameInInstance());
	}	  
      RxnDataThermoProperty *prop = (RxnDataThermoProperty *) instance->GetObject(thermoS);
      RxnThermoPropertyClass *propclass = (RxnThermoPropertyClass *) 
	instanceclasses->GetObjectClass(prop->GetType());
      BaseDataDoubleMatrix *orig = prop->CalculateThermoUnderUnits(keys,propclass,temperatures);
      RxnDataChemkinThermo *thermo = new RxnDataChemkinThermo(*iname,
							      cpunits,enthalpyunits,entropyunits,
							      molecule,molclass,prop,propclass);
      thermo->NameTag = chemkinNameS;
      cout << "CHEMKIN:" << endl;
      thermo->print(cout);
      cout << endl;
      cout << "Out: " << thermo->GetType() << " " << thermo->CalculateEnthalpy(thermoclass,1000.0);
      cout << endl;
      BaseDataDoubleMatrix *chemkin = thermo->CalculateThermoUnderUnits(keys,thermoclass,temperatures);
      cout << "Original:" << endl;
      orig->print(cout);
      cout << "Chemkin:" << endl;
      chemkin->print(cout);
      instance->AddObject(thermo);
      delete thermo;
    }
  
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
bool RxnDataConvertToChemkin::WriteOutputValues(BaseDataSetOfInstances*,
							DataSetOfInstancesClass*,
							BaseDataAlgorithmRun* run,
							DataAlgorithmRunClass*)
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
bool RxnDataConvertToChemkin::ConcludeRun(BaseDataSetOfInstances*,
						  DataSetOfInstancesClass*,
						  BaseDataAlgorithmRun*,
						  DataAlgorithmRunClass*)
{
  bool result = true;
//  delete something
  return result;
}
 

/*S RxnConvertToChemkinClass
 */
/*F RxnConvertToChemkinClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnConvertToChemkinClass::RxnConvertToChemkinClass()
{
  Identification = THERMO_CHEMKINCONV_ID;
  NameTag = THERMO_CHEMKINCONV_NAME;
  SubClass = "AlgorithmObject";
  EncodeDecodeClass = NameTag;
} 
/*F RxnConvertToChemkinClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnConvertToChemkinClass::RxnConvertToChemkinClass(const RxnConvertToChemkinClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnConvertToChemkinClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnConvertToChemkinClass::RxnConvertToChemkinClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmObject";
  EncodeDecodeClass = "ConvertToChemkin";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnConvertToChemkinClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnConvertToChemkinClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnConvertToChemkinClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnConvertToChemkinClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnConvertToChemkinClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnConvertToChemkinClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnConvertToChemkinClass::CopyClone(Identify *  objc)
{
  RxnConvertToChemkinClass *objcfull = (RxnConvertToChemkinClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnConvertToChemkinClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnConvertToChemkinClass::Clone()
    {
      RxnConvertToChemkinClass* id = new RxnConvertToChemkinClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnConvertToChemkinClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnConvertToChemkinClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnConvertToChemkinClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnConvertToChemkinClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnConvertToChemkinClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataConvertToChemkin();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnConvertToChemkinClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnConvertToChemkinClass*& obj)
     {
     obj = new RxnConvertToChemkinClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataConvertToChemkin
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataConvertToChemkin*& obj)
     {
     obj = new RxnDataConvertToChemkin;
     return obj->DecodeThis(buffer);
     }
/*S Utilities
 */
/*F InitialSetOfThermoPropsDecodeFunctions()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialSetOfThermoPropsDecodeFunctions()
{
  EncodeDecodeRegisterClass(RxnConversionFactorsClass,RxnDataConversionFactors,THERMO_CONVERSION_NAME);
  EncodeDecodeRegisterClass(RxnSingleConversionSetClass,RxnDataSingleConversionSet,THERMO_SINGLE_NAME);
  EncodeDecodeRegisterClass(RxnConversionSetClass,RxnDataConversionSet,THERMO_CONVSET_NAME);
  EncodeDecodeRegisterClass(RxnThermoPropertyClass,RxnDataThermoProperty,THERMO_PROPERTY_NAME);
  EncodeDecodeRegisterClass(RxnChemkinThermoClass,RxnDataChemkinThermo,THERMO_CHEMKIN_NAME);
  EncodeDecodeRegisterClass(RxnBensonThermoClass,RxnDataBensonThermo,THERMO_BENSON_NAME);
  EncodeDecodeRegisterClass(RxnLiteratureReferenceClass,RxnDataLiteratureReference,THERMO_LIT_NAME);
  EncodeDecodeRegisterClass(RxnRealBasedPropertyClass,RxnDataRealBasedProperty,THERMO_REAL_NAME);
  EncodeDecodeRegisterClass(RxnPolynomialCpClass,RxnDataPolynomialCp,THERMO_POLY_NAME);
  EncodeDecodeRegisterClass(RxnThermoValuesAlgorithmClass,RxnDataThermoValuesAlgorithm,THERMO_THERMVALS_NAME);
  EncodeDecodeRegisterClass(RxnConvertToChemkinClass,RxnDataConvertToChemkin,THERMO_CHEMKINCONV_NAME);
}
/*F AddThermoPropsClasses(set)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void AddThermPropClasses(DataSetOfObjectsClass& set)
{

  String condescr("The Conversion Factor Class");
  RxnConversionFactorsClass conclass(THERMO_CONVERSION_ID,THERMO_CONVERSION_NAME,condescr);
  set.AddObjectClass(conclass);

  String singdescr("The Single Unit Conversion Class");
  RxnSingleConversionSetClass singclass(THERMO_SINGLE_ID,THERMO_SINGLE_NAME,singdescr);
  set.AddObjectClass(singclass);

  String consetdescr("The Set of Conversion Factors Class");
  RxnConversionSetClass consetclass(THERMO_CONVSET_ID,THERMO_CONVSET_NAME,consetdescr);
  set.AddObjectClass(consetclass);

  String thrmdescr("The Base Thermo Property Class");
  RxnThermoPropertyClass thrmclass(THERMO_PROPERTY_ID,THERMO_PROPERTY_NAME,thrmdescr);
  set.AddObjectClass(thrmclass);

  String chemkindescr("The Chemkin Class");
  RxnChemkinThermoClass chemkinclass(THERMO_CHEMKIN_ID,THERMO_CHEMKIN_NAME,chemkindescr);
  set.AddObjectClass(chemkinclass);
  
  String bensondescr("The Benson Thermodynamic Class");
  RxnBensonThermoClass bensonclass(THERMO_BENSON_ID,THERMO_BENSON_NAME,bensondescr);
  set.AddObjectClass(bensonclass);

  String litdescr("The Class");
  RxnLiteratureReferenceClass litclass(THERMO_LIT_ID,THERMO_LIT_NAME,litdescr);
  set.AddObjectClass(litclass);

  String realdescr("The Real Property Class");
  RxnRealBasedPropertyClass realclass(THERMO_REAL_ID,THERMO_REAL_NAME,realdescr);
  set.AddObjectClass(realclass);

  String polydescr("The Heat Capacity in Polynomical Form Class");
  RxnPolynomialCpClass polyclass(THERMO_POLY_ID,THERMO_POLY_NAME,polydescr);
  set.AddObjectClass(polyclass);

  String valuesdescr("The Thermodynamic Values Algorithm Class");
  RxnThermoValuesAlgorithmClass valuesclass(THERMO_THERMVALS_ID,THERMO_THERMVALS_NAME,valuesdescr);
  set.AddObjectClass(valuesclass);

  String convdescr("The Convert To Chemkin Class");
  RxnConvertToChemkinClass convclass(THERMO_CHEMKINCONV_ID,THERMO_CHEMKINCONV_NAME,convdescr);
  set.AddObjectClass(convclass);
}

