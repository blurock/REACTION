/*  FILE     UtilitiesType.hh
**  PACKAGE  Utilities
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "Utilities" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_UTILITIESTYPE_HH
#define Reaction_UTILITIESTYPE_HH

/*C RxnDataMoveData  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MoveData class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataMoveData : public BaseDataAlgorithmOperation
{
  String instanceNameListS;
  BaseDataKeyWords *instanceNameList;
  String toMoveS;
  BaseDataKeyWords *toMove;

  ObjectList<String> attributes;
  bool movetoinstance;
  BaseDataOperation *moveit;
  String objectname;
public:
  RxnDataMoveData();
  RxnDataMoveData(const RxnDataMoveData& data);
  bool StoreAttributes(BaseDataInstance *instance,
		       DataSetOfObjectsClass *classes,
		       BaseDataObject *obj);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C RxnMoveDataClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnMoveDataClass : public DataAlgorithmOperationClass
{
public:
  RxnMoveDataClass();
  RxnMoveDataClass(const RxnMoveDataClass& data);
  RxnMoveDataClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataStoreSetOfObjectsProperty  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the StoreSetOfObjectsProperty class definitions
**
**  REMARKS
**    Inheirits BaseDataOperation
*/
class RxnDataStoreSetOfObjectsProperty : public BaseDataOperation
{
public:
  RxnDataStoreSetOfObjectsProperty();
  RxnDataStoreSetOfObjectsProperty(const RxnDataStoreSetOfObjectsProperty& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_OPERATION_METHODS;
};
/*C RxnStoreSetOfObjectsPropertyClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataOperationClass
*/
class RxnStoreSetOfObjectsPropertyClass : public DataOperationClass
{
public:
  RxnStoreSetOfObjectsPropertyClass();
  RxnStoreSetOfObjectsPropertyClass(const RxnStoreSetOfObjectsPropertyClass& data);
  RxnStoreSetOfObjectsPropertyClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  DataSetOfObjectsClass *PointerToAllowedClasses();
};
/*C RxnDataRetrieveSetOfObjectsProperty  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the RetrieveSetOfObjectsProperty class definitions
**
**  REMARKS
**    Inheirits BaseDataOperation
*/
class RxnDataRetrieveSetOfObjectsProperty : public BaseDataOperation
{
public:
  RxnDataRetrieveSetOfObjectsProperty();
  RxnDataRetrieveSetOfObjectsProperty(const RxnDataRetrieveSetOfObjectsProperty& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_OPERATION_METHODS;
};
/*C RxnRetrieveSetOfObjectsPropertyClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataOperationClass
*/
class RxnRetrieveSetOfObjectsPropertyClass : public DataOperationClass
{
public:
  RxnRetrieveSetOfObjectsPropertyClass();
  RxnRetrieveSetOfObjectsPropertyClass(const RxnRetrieveSetOfObjectsPropertyClass& data);
  RxnRetrieveSetOfObjectsPropertyClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  DataSetOfObjectsClass *PointerToAllowedClasses();
};

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
