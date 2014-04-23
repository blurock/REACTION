/*S RxnData=Object=
 */ 
/*F RxnData=Object=()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnData=Object=::RxnData=Object=()
{
  Identification = _ID;
  NameTag = _NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnData=Object=(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnData=Object=::RxnData=Object=(const RxnData=Object=& data)
  : RxnData=SubClass=(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnData=Object=
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnData=Object=::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnData=Object=
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnData=Object=::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = RxnData=SubClass=::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnData=Object=
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnData=Object=::print(ostream& out) const
{
  RxnData=SubClass=::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnData=Object=
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnData=Object=::Clone()
{
  RxnData=Object= *obj = new RxnData=Object=(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnData=Object=
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnData=Object=::CopyClone(Identify * obj)
{
  RxnData=Object= *objfull = (RxnData=Object= *) obj;
  *this = *objfull;
  //Parameter = (RxnData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnData=Object=
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnData=Object=::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnData=SubClass=::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  //result = result && Encode(buffer,---);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnData=Object=
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnData=Object=::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnData=SubClass=::DecodeThis(buffer);
  // The rest

  return result;
}
 
 
/*S Rxn=Object=Class
 */
/*F Rxn=Object=Class() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
Rxn=Object=Class::Rxn=Object=Class()
{
  Identification = _ID;
  NameTag = _NAME;
  SubClass = "=SubClass=";
  EncodeDecodeClass = NameTag;
} 
/*F Rxn=Object=Class(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
Rxn=Object=Class::Rxn=Object=Class(const Rxn=Object=Class& data)
  : Data=SubClass=Class(data)
{
} 
 
/*F Rxn=Object=Class(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
Rxn=Object=Class::Rxn=Object=Class(const int id, 
				 const String& name,
				 const String& descr)
  : Data=SubClass=Class(id,name,descr)
{
  SubClass = "=SubClass=";
  EncodeDecodeClass = "=Object=";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . Rxn=Object=Class
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& Rxn=Object=Class::print(ostream& out) const
{
  Data=SubClass=Class::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . Rxn=Object=Class
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base Rxn=Object=Class, there is no further information.
**
**  REMARKS
**
*/
bool Rxn=Object=Class::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = Data=SubClass=Class::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . Rxn=Object=Class
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void Rxn=Object=Class::CopyClone(Identify *  objc)
{
  Rxn=Object=Class *objcfull = (Rxn=Object=Class *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . Rxn=Object=Class
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * Rxn=Object=Class::Clone()
    {
      Rxn=Object=Class* id = new Rxn=Object=Class(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . Rxn=Object=Class
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool Rxn=Object=Class::EncodeThis(CommBuffer& buffer)
{
  bool result = Data=SubClass=Class::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . Rxn=Object=Class
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool Rxn=Object=Class::DecodeThis(CommBuffer& buffer)
{
  bool result = Data=SubClass=Class::DecodeThis(buffer);
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
BaseDataObject * Rxn=Object=Class::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnData=Object=();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . Rxn=Object=Class
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, Rxn=Object=Class*& obj)
     {
     obj = new Rxn=Object=Class;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnData=Object=
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnData=Object=*& obj)
     {
     obj = new RxnData=Object=;
     return obj->DecodeThis(buffer);
     }
//EncodeDecodeRegisterClass(Rxn=Object=Class,RxnData=Object=,_NAME);
//String descr("The Class");
//Rxn=Object=Class class(_ID,_NAME,descr);
//set.AddObjectClass(class);
