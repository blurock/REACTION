/*  FILE     DbaseType.hh
**  PACKAGE  Dbase
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "Dbase" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 CoreObjects Project, RISC Linz
*/
 
#ifndef CoreObjects_DBASETYPE_HH
#define CoreObjects_DBASETYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
class DataDataBaseInformationClass;
/*C BaseDataDataBaseInformation  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the DataBaseInformation class definitions
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class BaseDataDataBaseInformation : public BaseDataObject
{
  String RootName;
  DataBaseFileName *FileName;
  GDBM_FILE DataBase;
  unsigned int DataType;
public:
  BaseDataDataBaseInformation();
  BaseDataDataBaseInformation(const BaseDataDataBaseInformation& data);
  BaseDataDataBaseInformation(unsigned int datatype, String& name);
  STANDARD_VIRTUAL_METHODS_OBJECT
  bool StoreElement(CommBuffer& ekey,
		    CommBuffer& eelement,
		    const int flag);
  bool StoreElement(BaseDataObject *element,
		    const int flag);
  bool StoreElement(BaseDataObject *element);
  bool FetchElement(String& key,
		    DataDataBaseInformationClass *databaseclass,
		    BaseDataObject *& element);
  bool ElementExists(String& key);
  void OpenUpDataBase(DataDataBaseInformationClass *dbaseclass);
  void FormDataBaseFileName();
  DataObjectClass *getDataElementClass(DataDataBaseInformationClass *cls);
  datum* MakeDatumElement(CommBuffer& buffer);
  void FreeDatumElement(datum *element);
};
/*C DataDataBaseInformationClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class DataDataBaseInformationClass : public DataObjectClass
{
  unsigned int Permissions;
  unsigned int Mode;
  String Directory;
public:
  DataDataBaseInformationClass();
  DataDataBaseInformationClass(const DataDataBaseInformationClass& data);
  DataDataBaseInformationClass(const int id, 
		    const String& name,
		    const String& descr);
  String& getDirectory();
  unsigned int getPermissions();
  unsigned int getMode();
  DataSetOfObjectsClass *PointerToAllowedClasses();
  STANDARD_VIRTUAL_METHODS_CLASS
};


#endif
