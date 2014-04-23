/*  FILE     Dbase.cc
**  PACKAGE  Dbase
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "Dbase" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "CoreDataObjects.hh"
#include "Dbase.hh"

/*S BaseDataDataBaseInformation
 */ 
/*F BaseDataDataBaseInformation()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataDataBaseInformation::BaseDataDataBaseInformation()
  : DataType(0)
{
  Identification = DBASE_INFO_ID;
  NameTag = DBASE_INFO_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F BaseDataDataBaseInformation(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
BaseDataDataBaseInformation::BaseDataDataBaseInformation(const BaseDataDataBaseInformation& data)
  : BaseDataObject(data),
    RootName(data.RootName),
    DataType(data.DataType)
{
}
 
/*F BaseDataDataBaseInformation(datatype,name)  . BaseDataDataBaseInformation
**
**  DESCRIPTION
**    datatype: The data type of the database element
**    name: The root name of the database file
**
**  REMARKS
**
*/
BaseDataDataBaseInformation::BaseDataDataBaseInformation(unsigned int datatype, String& name)
  : RootName(name),
    DataType(datatype)
{  
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in BaseDataDataBaseInformation
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in BaseDataDataBaseInformation
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::Read(istream& in, DataObjectClass* objc, const String& name)
{
  DataDataBaseInformationClass *databaseclass = (DataDataBaseInformationClass *) objc;
  DataSetOfObjectsClass *classes = databaseclass->PointerToAllowedClasses();

  bool result = BaseDataObject::Read(in,objc,name);
  StreamObjectInput str(in,' ');
  RootName = str.ReadNext();
  String datatypeS = str.ReadNext();
  if(classes->IsInList(datatypeS))
    {
      DataObjectClass *objclass = classes->GetObjectClass(datatypeS);
      DataType = objclass->Identification;
    }
  else
    {
      cerr << "Data type not found in list of classes: '" << datatypeS << "'" << endl;
      result = false;
    }

  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  BaseDataDataBaseInformation
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& BaseDataDataBaseInformation::print(ostream& out) const
{
  BaseDataObject::print(out);
  out << "File root: " << RootName;
  out << "for data of type: " << DataType << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .BaseDataDataBaseInformation
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * BaseDataDataBaseInformation::Clone()
{
  BaseDataDataBaseInformation *obj = new BaseDataDataBaseInformation(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .BaseDataDataBaseInformation
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void BaseDataDataBaseInformation::CopyClone(Identify * obj)
{
  BaseDataDataBaseInformation *objfull = (BaseDataDataBaseInformation *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataDataBaseInformation
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::EncodeThis(buffer);
  result = result && Encode(buffer,RootName);
  result = result && Encode(buffer,DataType);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataDataBaseInformation
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::DecodeThis(buffer);
  result = result && Decode(buffer,RootName);
  result = result && Decode(buffer,DataType);
  return result;
}
/*F OpenUpDataBase()  . . . . . . . . . . . . . . . . . . . . . . . . .  open
**
**  DESCRIPTION
**    Use gdbm_open to open the database
**
**  REMARKS
**
*/
void BaseDataDataBaseInformation::OpenUpDataBase(DataDataBaseInformationClass *dbaseclass)
{
  FileName = new DataBaseFileName(dbaseclass->getDirectory(),
				  RootName);

  //unsigned int size = strlen(FileName->FullName.c_str());
  unsigned int size = FileName->FullName.size() - 1;
  char *str = (char *) malloc(size+1);
  memcpy(str,FileName->FullName.c_str(),size);
  *(str + size) = '\0';
  
  DataBase = gdbm_open(str,
                       DBASE_DEFAULT_BLOCKSIZE,
                       dbaseclass->getMode(),
                       dbaseclass->getPermissions(),
                       0);
  if(DataBase == 0)
    {
      cout << "Error in opening DataBase: ";
      cout << str;
      cout << "\n";
    }
} 
/*F ans = StoreElement(ekey,eelement,flag)  . . . . . . . . . from CommBuffer
**
**  DESCRIPTION
**    ekey: The key
**    eelement: The element to be written
**    flag: The GDBM store mode (GDBM_REPLACE, GDBM_INSERT)
**    ans: true if successful
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::StoreElement(CommBuffer& ekey,
					       CommBuffer& eelement,
					       const int flag)
{
  if(DataBase != 0)
    {
      datum *keydatum = MakeDatumElement(ekey);
      datum *elementdatum = MakeDatumElement(eelement);
          
      int ret = gdbm_store(DataBase,*keydatum,*elementdatum,flag);
      
      FreeDatumElement(keydatum);
      FreeDatumElement(elementdatum);
          
      if(ret)
	{
	  cerr << "Error in gdbm_store" << endl;
	  return false;
	}
      else
        return true;
    }
  else
    return false;
}
/*F ans = StoreElement(element,flag)  . . . . . . . . . . . . . . . . . store
**
**  DESCRIPTION
**    key: Standard key for database
**    element: The database element
**    flag: The GDBM store mode
**    ans: true if successful
**
**    The GDBM store modes are:
**    - GDBM_REPLACE
**    - GDBM_INSERT
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::StoreElement(BaseDataObject *element,
					       const int flag)
{
  bool result = true;
  CommBuffer ekey(COMM_BUFF_ENCODE);
  CommBuffer eelement(COMM_BUFF_ENCODE);

  if(DataType == element->GetType())
    {
      Encode(ekey,element->NameTag);
      element->EncodeThis(eelement);
      result = StoreElement(ekey,eelement,flag);
    }
  else
    {
      cerr << "Element: '" << element->NameTag << "' is of type: ";
      cerr << element->GetType() << " and not of expected type for database: ";
      cerr << DataType << endl;
      result = false;
    }
  return result;
}
/*F ans = StoreElement(key,element) . . . . . . . . . . . . . . . . . . store
**
**  DESCRIPTION
**    key: Standard key for database
**    element: The database element
**    ans: true if successful
**
**    The GDBM store modes is GDBM_REPLACE by default
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::StoreElement(BaseDataObject *element)
{
  return StoreElement(element,GDBM_REPLACE);
}
/*F ans = FetchElement(key,element) . . . . . . . . . . . . . . . . . . fetch
**
**  DESCRIPTION
**    key: The keyword
**    element: where the element will be put
**    ans: true if successful
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::FetchElement(String& key,
					       DataDataBaseInformationClass *databaseclass,
					       BaseDataObject *& element)
{
  bool ret;
  if(DataBase != 0)
    {
      DataObjectClass *objclass = databaseclass->PointerToAllowedClasses()->GetObjectClass(DataType);
      element = objclass->BaseDataObjectExample();
      CommBuffer ekey(COMM_BUFF_ENCODE);
      Encode(ekey,key);
      datum *keydatum = MakeDatumElement(ekey);
      datum delement = gdbm_fetch(DataBase,*keydatum);
      if(delement.dptr != 0)
        {
          CommBuffer eelement(delement.dptr,delement.dsize);
          element->DecodeThis(eelement);
          free(delement.dptr);
          ret = true;
        }
      else
        ret = false;
          
      FreeDatumElement(keydatum);
    }
  else 
    ret = false;
     
  return ret;
}
/*F ans = FetchElement(key,element) . . . . . . . . . . . . . . . . . . fetch
**
**  DESCRIPTION
**    key: The keyword
**    element: where the element will be put
**    ans: true if successful
**
**  REMARKS
**
*/
bool BaseDataDataBaseInformation::ElementExists(String& key)
{
  bool ret = true;
  if(DataBase != 0)
    {
      CommBuffer ekey(COMM_BUFF_ENCODE);
      Encode(ekey,key);
      datum *keydatum = MakeDatumElement(ekey);
      int exists = gdbm_exists(DataBase,*keydatum);
      if(exists == 0)
	ret = false;
      else
	ret = true;
      FreeDatumElement(keydatum);
    }
  else 
    ret = false;
     
  return ret;
}
/*F dat = MakeDatumElement(buffer)  . . . . . . . . . . . . .  datum for gdbm
**
**  DESCRIPTION
**    buffer: The buffer with the encoded bytes
**    dat: The gdbm element used
** 
**  REMARKS
**
*/
datum* BaseDataDataBaseInformation::MakeDatumElement(CommBuffer& buffer)
{
  datum* dat = (datum *) malloc(sizeof(datum));
  unsigned int size = buffer.GetBufferSize();
     
  dat->dsize = size;
  dat->dptr = (char *) malloc(size);
  memcpy(dat->dptr,buffer.GetBuffer(),size);
     
  return dat;
}
/*F FreeDatumElement(element) . . . . . . . . . . . . . . . . . . . . .  free
**
**  DESCRIPTION
**    element: Element to be freed
**
**  REMARKS
**
*/
void BaseDataDataBaseInformation::FreeDatumElement(datum *element)
{
  free(element->dptr);
  free(element);
}
 
/*F elementclass = getDataElementClass(cls) . . . BaseDataDataBaseInformation
**
**  DESCRIPTION
**    cls: The class associated with the database
**    elementclass: The class of the element stored in the database
**
**  REMARKS
**
*/
DataObjectClass *BaseDataDataBaseInformation::getDataElementClass(DataDataBaseInformationClass *cls)
{
  return cls->PointerToAllowedClasses()->GetObjectClass(DataType);
}
/*S DataDataBaseInformationClass
 */
/*F DataDataBaseInformationClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
DataDataBaseInformationClass::DataDataBaseInformationClass()
{
  Identification = DBASE_INFO_ID;
  NameTag = DBASE_INFO_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
} 
/*F DataDataBaseInformationClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
DataDataBaseInformationClass::DataDataBaseInformationClass(const DataDataBaseInformationClass& data)
  : DataObjectClass(data),
    Permissions(data.Permissions),
    Mode(data.Mode),
    Directory(data.Directory)
{
}
/*F DataDataBaseInformationClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
DataDataBaseInformationClass::DataDataBaseInformationClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataObjectClass(id,name,descr)
{
  SubClass = "Object";
  EncodeDecodeClass = DBASE_INFO_NAME;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . DataDataBaseInformationClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& DataDataBaseInformationClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  out << "File Directory: " << Directory << "  ";
  out << "(" << Permissions << "," << Mode << ")" << endl;
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . DataDataBaseInformationClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base DataDataBaseInformationClass, there is no further information.
**
**  REMARKS
**
*/
bool DataDataBaseInformationClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataObjectClass::Read(in,set);

  StreamObjectInput str(in,' ');
  Directory = str.ReadNext();
  String PermissionsS = str.ReadNext();
  String ModeS = str.ReadNext();

  Permissions = PermissionsS.ToInteger();
  Mode = ModeS.ToInteger();

  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . DataDataBaseInformationClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void DataDataBaseInformationClass::CopyClone(Identify *  objc)
{
  DataDataBaseInformationClass *objcfull = (DataDataBaseInformationClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . DataDataBaseInformationClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * DataDataBaseInformationClass::Clone()
{
  DataDataBaseInformationClass* id = new DataDataBaseInformationClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . DataDataBaseInformationClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataDataBaseInformationClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  result = result && Encode(buffer,Permissions);
  result = result && Encode(buffer,Mode);
  result = result && Encode(buffer,Directory);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . DataDataBaseInformationClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataDataBaseInformationClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
  result = result && Decode(buffer,Permissions);
  result = result && Decode(buffer,Mode);
  result = result && Decode(buffer,Directory);
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
BaseDataObject * DataDataBaseInformationClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new BaseDataDataBaseInformation();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . DataDataBaseInformationClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, DataDataBaseInformationClass*& obj)
     {
     obj = new DataDataBaseInformationClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . BaseDataDataBaseInformation
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, BaseDataDataBaseInformation*& obj)
     {
     obj = new BaseDataDataBaseInformation;
     return obj->DecodeThis(buffer);
     }
 
/*F dir = getDirectory()  . . . . . . . . . . .  DataDataBaseInformationClass
**
**  DESCRIPTION
**    dir: The directory of the database
**
**  REMARKS
**
*/
String& DataDataBaseInformationClass::getDirectory()
{
  return Directory;
}
/*F permissions = getPermissions()
**
**  DESCRIPTION
**    permissions: The permssions on the database file
**
**  REMARKS
**
*/
unsigned int DataDataBaseInformationClass::getPermissions()
{
  return Permissions;
}
 
/*F mode = getMode()
**
**  DESCRIPTION
**    mode: The database mode
**
**  REMARKS
**
*/
unsigned int DataDataBaseInformationClass::getMode()
{
  return Mode;
}
 
/*F classes = PointerToAllowedClasses() DataDataBaseInformationClass
**
**  DESCRIPTION
**    classes: The allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *DataDataBaseInformationClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}
/*S
 */
/*S Utilities
 */
/*F InitialSetOfDBaseEncodeDecodeRoutines()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void InitialSetOfDBaseEncodeDecodeRoutines()
{
  EncodeDecodeRegisterClass(DataDataBaseInformationClass,BaseDataDataBaseInformation,DBASE_INFO_NAME);
}
 
/*F AddDBaseClasses(set)
**
**  DESCRIPTION
**    set: The set of classes
**
**  REMARKS
**
*/
extern void AddDBaseClasses(DataSetOfObjectsClass& set)
{
  String dbdescr("The Class");
  DataDataBaseInformationClass dbclass(DBASE_INFO_ID,DBASE_INFO_NAME,dbdescr);
  set.AddObjectClass(dbclass);
}

