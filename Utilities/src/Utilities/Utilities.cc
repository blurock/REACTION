/*  FILE     Utilities.cc
**  PACKAGE  Utilities
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "Utilities" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "CoreDataObjects.hh"
#include "Vector.hh"
#include "NumericObjects.hh"
#include "LogicalObjects.hh"
#include "DataObjects.hh"
#include "FunctionReal1DObjects.hh"
#include "OperationObjects.hh"
#include "LogicalOperations.hh"
#include "DataObjects.hh"
#include "InstanceObjects.hh"
#include "MenuObjects.hh"
#include "DirectedTreeObjects.hh"
#include "SelectObjects.hh"
#include "AlgorithmObjects.hh"
#include "Utilities.hh"

/*S RxnDataMoveData
 */ 
/*F RxnDataMoveData()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoveData::RxnDataMoveData()
  : instanceNameListS("InstanceNameList"),
    toMoveS("ToMove")
{
  Identification = RXNUTIL_MOVE_ID;
  NameTag = RXNUTIL_MOVE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMoveData(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMoveData::RxnDataMoveData(const RxnDataMoveData& data)
  : BaseDataAlgorithmOperation(data),
    instanceNameListS(data.instanceNameListS),
    toMoveS(data.toMoveS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMoveData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMoveData::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMoveData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMoveData::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMoveData
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMoveData::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  cout << "Inputs: " << instanceNameListS << " and " << toMoveS << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMoveData
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMoveData::Clone()
{
  RxnDataMoveData *obj = new RxnDataMoveData(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMoveData
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMoveData::CopyClone(Identify * obj)
{
  RxnDataMoveData *objfull = (RxnDataMoveData *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoveData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoveData::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,instanceNameListS);
  result = result && Encode(buffer,toMoveS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoveData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoveData::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,instanceNameListS);
  result = result && Decode(buffer,toMoveS);
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
bool RxnDataMoveData::SetUpAlgorithms(BaseDataSetOfInstances*,
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
bool RxnDataMoveData::CheckInput(BaseDataSetOfInstances *instances,
				 DataSetOfInstancesClass *instancesclass,
				 BaseDataAlgorithmRun *run,
				 DataAlgorithmRunClass *runclass)
{
  bool result = true;

  if(run->ParameterInList(instanceNameListS))
    {
      if(run->ParameterInList(toMoveS))
	{
	}
      else
	{
	  cerr << "The list of attributes to move '" << toMoveS << "' was not in the list of parameters";
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
bool RxnDataMoveData::SetUpInput(BaseDataSetOfInstances* instances,
				 DataSetOfInstancesClass* instancesclass,
				 BaseDataAlgorithmRun *run,
				 DataAlgorithmRunClass* rclass)
{
  bool result = true;

  instanceNameList = (BaseDataKeyWords *) run->ParameterValue(instanceNameListS);
  toMove = (BaseDataKeyWords *) run->ParameterValue(toMoveS);
  attributes = toMove->GetKeyWords();
  if(attributes.size() >= 3)
    {
      String move = attributes.front();
      if(move == "ToInstance")
	{
	  movetoinstance = true;
	  cout << "Move objects to instance" << endl;
	}
      else if(move == "FromInstance")
	{
	  movetoinstance = false;
	  cout << "Move objects from instance " << endl;
	}
      else
	{
	  cerr << "Parameter: 'ToInstance' or 'FromInstance' expected, got " << move << endl;
	  result = false;
	}
      attributes.pop_front();
      String opS = attributes.front();
      if(instances->IsInList(opS))
	moveit = (BaseDataOperation *) instances->GetObject(opS);
      else
	{
	  cerr << "Operation not found: '" << opS << "'" << endl;
	  result = false;
	}
      attributes.pop_front();
      objectname = attributes.front();
      attributes.pop_front();
    }
  else
    {
      cerr << "Not enough attributes specified" << endl;
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
bool RxnDataMoveData::Calculate(BaseDataSetOfInstances *instances,
				DataSetOfInstancesClass *instancesclass,
				BaseDataAlgorithmRun *run,
				DataAlgorithmRunClass *runclass)
{
  bool result = true;
  
  ObjectList<String> inputList = instanceNameList->GetKeyWords();
  DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
  for(ObjectList<String>::iterator name = inputList.begin(); 
      name != inputList.end() && result;
      name++)
    {
      if(instances->InstanceInSet(*name))
	{
	  BaseDataInstance *instance = instances->GetInstance(*name);
	  if(instance->IsInList(objectname))
	    {
	      BaseDataObject *obj = instance->GetObject(objectname);
	      StoreAttributes(instance,classes,obj);
	    }
	  else
	    {
	      cerr << "Object: '" << objectname << "' not found in instance: '" << *name << "'" << endl;
	      result = false;
	    }
	}
      else
	{
	  cerr << "Instance: '" << *name << "' not found" << endl;
	  result = false;
	}
    }  
  return result;
}

/*F ans = StoreAttributes(moveit,instance,object,objectclass)
**
**  DESCRIPTION
**    moveit: The moving operation
**    instance: The instance
**    object: Where to be stored
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoveData::StoreAttributes(BaseDataInstance *instance, 
				      DataSetOfObjectsClass *classes,      
				      BaseDataObject *obj)
{
  bool result = true;
  DataObjectClass *objclass = classes->GetObjectClass(obj->GetType());
  DataStringClass *stringclass = new DataStringClass();
  BaseDataString *name = new BaseDataString();	      
  ObjectList<String>::iterator attr;
  attributes.print(cout);
  cout << endl;
  attr = attributes.begin();
  while(attr != attributes.end() && result)
    {
      String source = (*attr);
      attr++;
      if(attr != attributes.end())
	{
	  String destination = (*attr);
	  attr++;
	  if(movetoinstance)
	    {
	      name->setString(source);
	      BaseDataObject *object = moveit->operator()(name,obj,stringclass,objclass);
	      object->NameTag = destination;
	      instance->AddObject(object);
	      delete object;
	    }
	  else
	    {
	      cout << "Store: '" << source << "' in instance '" << instance->NameTag << "'" << endl;
	      if(instance->IsInList(source))
		{
		  BaseDataObject *object = instance->GetObject(source);
		  object->NameTag = destination;
		  DataObjectClass *objectclass = classes->GetObjectClass(object->GetType());
		  BaseDataObject *res = moveit->operator()(object,obj,objectclass,objclass);
		  object->NameTag = destination;
		  delete res;
		}
	      else
		{
		  cerr << "Object: '" << source << "' not in instance: '" << instance->NameTag << "'" << endl;
		  result = false;
		}
	    }
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
bool RxnDataMoveData::WriteOutputValues(BaseDataSetOfInstances*,
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
bool RxnDataMoveData::ConcludeRun(BaseDataSetOfInstances*,
				  DataSetOfInstancesClass*,
				  BaseDataAlgorithmRun*,
				  DataAlgorithmRunClass*)
{
  bool result = true;
//  delete something
  return result;
}
/*S RxnMoveDataClass
 */
/*F RxnMoveDataClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMoveDataClass::RxnMoveDataClass()
{
  Identification = RXNUTIL_MOVE_ID;
  NameTag = RXNUTIL_MOVE_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMoveDataClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMoveDataClass::RxnMoveDataClass(const RxnMoveDataClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnMoveDataClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMoveDataClass::RxnMoveDataClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "MoveData";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMoveDataClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMoveDataClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMoveDataClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMoveDataClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMoveDataClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMoveDataClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMoveDataClass::CopyClone(Identify *  objc)
{
  RxnMoveDataClass *objcfull = (RxnMoveDataClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMoveDataClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMoveDataClass::Clone()
    {
      RxnMoveDataClass* id = new RxnMoveDataClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoveDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoveDataClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoveDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoveDataClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnMoveDataClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMoveData();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMoveDataClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMoveDataClass*& obj)
     {
     obj = new RxnMoveDataClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMoveData
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMoveData*& obj)
     {
     obj = new RxnDataMoveData;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataStoreSetOfObjectsProperty
 */ 
/*F RxnDataStoreSetOfObjectsProperty()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataStoreSetOfObjectsProperty::RxnDataStoreSetOfObjectsProperty()
{
  Identification = RXNUTIL_STORE_ID;
  NameTag = RXNUTIL_STORE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataStoreSetOfObjectsProperty(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataStoreSetOfObjectsProperty::RxnDataStoreSetOfObjectsProperty(const RxnDataStoreSetOfObjectsProperty& data)
  : BaseDataOperation(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataStoreSetOfObjectsProperty::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataStoreSetOfObjectsProperty::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataOperation::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataStoreSetOfObjectsProperty::print(ostream& out) const
{
  BaseDataOperation::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataStoreSetOfObjectsProperty::Clone()
{
  RxnDataStoreSetOfObjectsProperty *obj = new RxnDataStoreSetOfObjectsProperty(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataStoreSetOfObjectsProperty::CopyClone(Identify * obj)
{
  RxnDataStoreSetOfObjectsProperty *objfull = (RxnDataStoreSetOfObjectsProperty *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataStoreSetOfObjectsProperty::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataStoreSetOfObjectsProperty::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::DecodeThis(buffer);
  return result;
}
/*F obj = operator()(x,y,xclass,yclass) . . . . . . . . . . BaseDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    x,y: The objects to be operated on
**    xclass,yclass: The object classes
**    obj: The result
**
**    This is a two-dimensional call.  An object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataStoreSetOfObjectsProperty::operator()(BaseDataObject *x, BaseDataObject *y,
							     DataObjectClass *xc, DataObjectClass *yc)
{
  BaseDataSetOfObjects *set = (BaseDataSetOfObjects *) y;
  set->AddObject(x);
  return (BaseDataObject *) x->Clone();
}
/*F obj = operator()(x,xclass)  . . . . . . . . . . . . . . RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    x: The object to be operated on
**    xclass: The class of the object
**    obj: The result
**
**    This is a one-dimensional call.  An object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataStoreSetOfObjectsProperty::operator()(BaseDataObject *x,
							     DataObjectClass *xc)
{
  return (BaseDataObject *) x->Clone();
}
/*F obj = operator()(cls,x,y,xclass,yclass) . . . . . . . . . . RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    x,y: The objects to be operated on
**    xclass,yclass: The object classes
**    obj: The result
**
**    This is a two-dimensional call.  An object is created.
**    The cls is used (among other things) to check the input types
**
**  REMARKS
**
*/
BaseDataObject *RxnDataStoreSetOfObjectsProperty::operator()(DataObjectClass *cls,
						  BaseDataObject *x, BaseDataObject *y,
						  DataObjectClass *xc, DataObjectClass *yc)
                                              
{
  RxnStoreSetOfObjectsPropertyClass *storeclass = (RxnStoreSetOfObjectsPropertyClass *) cls;
  DataSetOfObjectsClass *set = (DataSetOfObjectsClass *) storeclass->PointerToAllowedClasses();
  bool result = true;
  result = result && set->IsOfClass(*yc,COREOBJECTS_SET_NAME);
  result = result && (y->GetType() == yc->GetType());

  return operator()(x,y,xc,yc);
}
/*F obj = operator()(cls,x,xclass)  . . . . . . . . . . . . . . RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    cls: The class of the operation
**    x: The object to be operated on
**    xclass: The class of the object
**    obj: The result
**
**    This is a dummy one-dimensional call.  An empty object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataStoreSetOfObjectsProperty::operator()(DataObjectClass *cls,
							     BaseDataObject *x,
							     DataObjectClass *xclass)
{
   return operator()(x,xclass);
}
/*S RxnStoreSetOfObjectsPropertyClass
 */
/*F RxnStoreSetOfObjectsPropertyClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnStoreSetOfObjectsPropertyClass::RxnStoreSetOfObjectsPropertyClass()
{
  Identification = RXNUTIL_STORE_ID;
  NameTag = RXNUTIL_STORE_NAME;
  SubClass = "Operation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnStoreSetOfObjectsPropertyClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnStoreSetOfObjectsPropertyClass::RxnStoreSetOfObjectsPropertyClass(const RxnStoreSetOfObjectsPropertyClass& data)
  : DataOperationClass(data)
{
} 
 
/*F RxnStoreSetOfObjectsPropertyClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnStoreSetOfObjectsPropertyClass::RxnStoreSetOfObjectsPropertyClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataOperationClass(id,name,descr)
{
  SubClass = "Operation";
  EncodeDecodeClass = "StoreSetOfObjectsProperty";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnStoreSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnStoreSetOfObjectsPropertyClass::print(ostream& out) const
{
  DataOperationClass::print(out);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnStoreSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnStoreSetOfObjectsPropertyClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnStoreSetOfObjectsPropertyClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataOperationClass::Read(in,set);
  return result;
} 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnStoreSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnStoreSetOfObjectsPropertyClass::CopyClone(Identify *  objc)
{
  RxnStoreSetOfObjectsPropertyClass *objcfull = (RxnStoreSetOfObjectsPropertyClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnStoreSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnStoreSetOfObjectsPropertyClass::Clone()
    {
      RxnStoreSetOfObjectsPropertyClass* id = new RxnStoreSetOfObjectsPropertyClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnStoreSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnStoreSetOfObjectsPropertyClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnStoreSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnStoreSetOfObjectsPropertyClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::DecodeThis(buffer);
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
BaseDataObject * RxnStoreSetOfObjectsPropertyClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataStoreSetOfObjectsProperty();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnStoreSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnStoreSetOfObjectsPropertyClass*& obj)
     {
     obj = new RxnStoreSetOfObjectsPropertyClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataStoreSetOfObjectsProperty
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataStoreSetOfObjectsProperty*& obj)
     {
     obj = new RxnDataStoreSetOfObjectsProperty;
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
DataSetOfObjectsClass *RxnStoreSetOfObjectsPropertyClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S RxnDataRetrieveSetOfObjectsProperty
 */ 
/*F RxnDataRetrieveSetOfObjectsProperty()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataRetrieveSetOfObjectsProperty::RxnDataRetrieveSetOfObjectsProperty()
{
  Identification = RXNUTIL_RETRIEVE_ID;
  NameTag = RXNUTIL_RETRIEVE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataRetrieveSetOfObjectsProperty(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataRetrieveSetOfObjectsProperty::RxnDataRetrieveSetOfObjectsProperty(const RxnDataRetrieveSetOfObjectsProperty& data)
  : BaseDataOperation(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataRetrieveSetOfObjectsProperty::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataRetrieveSetOfObjectsProperty::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataOperation::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataRetrieveSetOfObjectsProperty::print(ostream& out) const
{
  BaseDataOperation::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataRetrieveSetOfObjectsProperty::Clone()
{
  RxnDataRetrieveSetOfObjectsProperty *obj = new RxnDataRetrieveSetOfObjectsProperty(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataRetrieveSetOfObjectsProperty::CopyClone(Identify * obj)
{
  RxnDataRetrieveSetOfObjectsProperty *objfull = (RxnDataRetrieveSetOfObjectsProperty *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataRetrieveSetOfObjectsProperty::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataRetrieveSetOfObjectsProperty::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::DecodeThis(buffer);
  return result;
}
/*F obj = operator()(x,y,xclass,yclass) . . . . . . . . . . BaseDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    x,y: The objects to be operated on
**    xclass,yclass: The object classes
**    obj: The result
**
**    This is a two-dimensional call.  An object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataRetrieveSetOfObjectsProperty::operator()(BaseDataObject *x, BaseDataObject *y,
					     DataObjectClass *xc, DataObjectClass *yc)
{
  BaseDataString *str = (BaseDataString *) x;
  BaseDataSetOfObjects *set = (BaseDataSetOfObjects *) y;
  BaseDataObject *obj;
  if(set->IsInList(str->getString()))
    obj = set->GetObject( str->getString() );
  else
    obj = x;
  return (BaseDataObject *) obj->Clone();
}
/*F obj = operator()(x,xclass)  . . . . . . . . . . . . . . RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    x: The object to be operated on
**    xclass: The class of the object
**    obj: The result
**
**    This is a one-dimensional call.  An object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataRetrieveSetOfObjectsProperty::operator()(BaseDataObject *x,
					     DataObjectClass *xc)
{
  return (BaseDataObject *) x->Clone();
}
/*F obj = operator()(cls,x,y,xclass,yclass) . . . . . . . . . . RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    x,y: The objects to be operated on
**    xclass,yclass: The object classes
**    obj: The result
**
**    This is a two-dimensional call.  An object is created.
**    The cls is used (among other things) to check the input types
**
**  REMARKS
**
*/
BaseDataObject *RxnDataRetrieveSetOfObjectsProperty::operator()(DataObjectClass *cls,
						  BaseDataObject *x, BaseDataObject *y,
						  DataObjectClass *xc, DataObjectClass *yc)
                                              
{
  RxnRetrieveSetOfObjectsPropertyClass *retrieveclass = (RxnRetrieveSetOfObjectsPropertyClass *) cls;
  DataSetOfObjectsClass *set = (DataSetOfObjectsClass *) retrieveclass->PointerToAllowedClasses();
  bool result = true;
  result = result && set->IsOfClass(*xc,DATAOBJ_STRING_NAME);
  result = result && set->IsOfClass(*yc,COREOBJECTS_SET_NAME);
  result = result && (x->GetType() == xc->GetType());
  result = result && (y->GetType() == yc->GetType());
  return operator()(x,y,xc,yc);
}
/*F obj = operator()(cls,x,xclass)  . . . . . . . . . . . . . . RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    cls: The class of the operation
**    x: The object to be operated on
**    xclass: The class of the object
**    obj: The result
**
**    This is a dummy one-dimensional call.  An empty object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataRetrieveSetOfObjectsProperty::operator()(DataObjectClass *cls,
						  BaseDataObject *x,
						  DataObjectClass *xclass)
{
  return operator()(x,xclass);
}

 
/*S RxnRetrieveSetOfObjectsPropertyClass
 */
/*F RxnRetrieveSetOfObjectsPropertyClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnRetrieveSetOfObjectsPropertyClass::RxnRetrieveSetOfObjectsPropertyClass()
{
  Identification = RXNUTIL_RETRIEVE_ID;
  NameTag = RXNUTIL_RETRIEVE_NAME;
  SubClass = "Operation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnRetrieveSetOfObjectsPropertyClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnRetrieveSetOfObjectsPropertyClass::RxnRetrieveSetOfObjectsPropertyClass(const RxnRetrieveSetOfObjectsPropertyClass& data)
  : DataOperationClass(data)
{
} 
 
/*F RxnRetrieveSetOfObjectsPropertyClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnRetrieveSetOfObjectsPropertyClass::RxnRetrieveSetOfObjectsPropertyClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataOperationClass(id,name,descr)
{
  SubClass = "Operation";
  EncodeDecodeClass = "RetrieveSetOfObjectsProperty";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnRetrieveSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnRetrieveSetOfObjectsPropertyClass::print(ostream& out) const
{
  DataOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnRetrieveSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnRetrieveSetOfObjectsPropertyClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnRetrieveSetOfObjectsPropertyClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnRetrieveSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnRetrieveSetOfObjectsPropertyClass::CopyClone(Identify *  objc)
{
  RxnRetrieveSetOfObjectsPropertyClass *objcfull = (RxnRetrieveSetOfObjectsPropertyClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnRetrieveSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnRetrieveSetOfObjectsPropertyClass::Clone()
    {
      RxnRetrieveSetOfObjectsPropertyClass* id = new RxnRetrieveSetOfObjectsPropertyClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnRetrieveSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnRetrieveSetOfObjectsPropertyClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnRetrieveSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnRetrieveSetOfObjectsPropertyClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::DecodeThis(buffer);
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
BaseDataObject * RxnRetrieveSetOfObjectsPropertyClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataRetrieveSetOfObjectsProperty();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnRetrieveSetOfObjectsPropertyClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnRetrieveSetOfObjectsPropertyClass*& obj)
     {
     obj = new RxnRetrieveSetOfObjectsPropertyClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataRetrieveSetOfObjectsProperty
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataRetrieveSetOfObjectsProperty*& obj)
     {
     obj = new RxnDataRetrieveSetOfObjectsProperty;
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
DataSetOfObjectsClass *RxnRetrieveSetOfObjectsPropertyClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S Utilities
 */
/*F Initialize=Package=DecodeFunctions()  . . . . . . . . . . . . Reactions
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialRxnUtilitiesDecodeFunctions()
{
  EncodeDecodeRegisterClass(RxnMoveDataClass,RxnDataMoveData,RXNUTIL_MOVE_NAME);
  EncodeDecodeRegisterClass(RxnStoreSetOfObjectsPropertyClass,RxnDataStoreSetOfObjectsProperty,RXNUTIL_STORE_NAME);
  EncodeDecodeRegisterClass(RxnRetrieveSetOfObjectsPropertyClass,RxnDataRetrieveSetOfObjectsProperty,RXNUTIL_RETRIEVE_NAME);
}
/*F Add=Package=Classes(set) . . . . . . . . . . . .  EquilibriumConst
**
**  DESCRIPTION
**    set: The set of classes to add them to
**
**  REMARKS
**
*/
void AddRxnUtilitiesClasses(DataSetOfObjectsClass& set)
{
  String moveitdescr("The Move to/from Instance from/to Reaction Object Class");
  RxnMoveDataClass moveitclass(RXNUTIL_MOVE_ID,RXNUTIL_MOVE_NAME,moveitdescr);
  set.AddObjectClass(moveitclass);

  String storedescr("The Store into set of objects operations Class");
  RxnStoreSetOfObjectsPropertyClass storeclass(RXNUTIL_STORE_ID,RXNUTIL_STORE_NAME,storedescr);
  set.AddObjectClass(storeclass);

  String retrievedescr("The Class");
  RxnRetrieveSetOfObjectsPropertyClass retrieveclass(RXNUTIL_RETRIEVE_ID,RXNUTIL_RETRIEVE_NAME,retrievedescr);
  set.AddObjectClass(retrieveclass);
}
