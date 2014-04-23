/*  FILE     Flame.cc
**  PACKAGE  Flame
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "Flame" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "CoreDataObjects.hh"
#include "NumericObjects.hh"
#include "LogicalObjects.hh"
#include "FunctionReal1DObjects.hh"
#include "OperationObjects.hh"
#include "LogicalOperations.hh"
#include "DataObjects.hh"
#include "InstanceObjects.hh"
#include "NumericOperations.hh"
#include "Flame.hh"

/*S BaseDataFlameSensitivityData
 */ 
/*F BaseDataFlameSensitivityData()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataFlameSensitivityData::BaseDataFlameSensitivityData()
{
  Identification = FLAME_DATA_ID;
  NameTag = FLAME_DATA_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F BaseDataFlameSensitivityData(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
BaseDataFlameSensitivityData::BaseDataFlameSensitivityData(const BaseDataFlameSensitivityData& data)
  : BaseDataInstanceDoubleMatrix(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in BaseDataFlameSensitivityData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool BaseDataFlameSensitivityData::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in BaseDataFlameSensitivityData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool BaseDataFlameSensitivityData::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = true;
  DataFlameSensitivityDataClass *flameclass = (DataFlameSensitivityDataClass *) objc;
  String nameline;
  nameline.ReadFullLine(in);
  cout << nameline << endl;
  String baseName(name);
  String paren("]");
  String beginParen("[");
  char delimitor = '\t';
  /*
  String sensname;
  nameline.IsolateNextWord(sensname,delimitor);
  cout << "sensname: " << sensname << endl;
  nameline.EliminateLeadingBlanks();
  sensname.EliminateBlanks();
  parameters.AddKeyWord(sensname);
  */
  unsigned int nameindex = 0;
  unsigned int count = 0;
  BaseDataKeySet &parameters = getParameterNames();
  while(!nameline.IsEmpty())
    {
      String param;
      nameline.IsolateNextWord(param,delimitor);
      param.EliminateBlanks();
      cout << "Read:  '" << param << "' (" << param.size() << ") - (" << nameline.size() << ")" << endl;
      if(param == flameclass->ProgressVariable) {
	nameindex = count;
	cout << "Progress Variable: " << nameindex;
      }
      parameters.AddKeyWord(param);
      nameline.EliminateLeadingBlanks();
      count++;
    }
  unsigned int dim2 = parameters.GetKeyWords().size();
  cout << "Read in Data for " << dim2 << " variables" << endl;

  StreamObjectInput file(in, delimitor);
  String oneset;
  oneset.ReadFullLine(in);
  oneset.EliminateLeadingBlanks();
  double currentVal;
  MatrixNumeric &mat = CurrentMatrix();
  count = 0;
  BaseDataKeyWords &instances = getInstanceNames();
  String word;
  //*** stringstream change    int siz = 100;
  //*** stringstream change    char* ch = new char[siz];

  while(!oneset.IsEmpty())
    {
      VectorNumeric line;
      //line.AddObject(timeval);

      oneset.EliminateLeadingBlanks();
      for (unsigned int j=0; j<dim2; j++)
	{
	  oneset.IsolateNextWord(word,delimitor);
	  oneset.EliminateLeadingBlanks();
	  currentVal = word.ToFloat();
	  line.AddObject(currentVal);
	}

      String name(baseName);
      name.AppendToEnd(beginParen);
      oneset.IsolateNextWord(word,delimitor);
      word.EliminateBlanks();
      double timeval = line[nameindex];
      stringstream ostrstr;
      //ostrstr.setf(ios::fixed,ios::floatfield);
      ostrstr.setf(ios::fixed);
      
      ostrstr << setprecision(6) << timeval << '\0';
      name.AppendToEnd(ostrstr.str().c_str());
      name.AppendToEnd(paren);

      instances.AddKeyWord(name);

      mat.AddObject(line);
      oneset.ReadFullLine(in);
      oneset.EliminateLeadingBlanks();
      count++;
    }

  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  BaseDataFlameSensitivityData
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& BaseDataFlameSensitivityData::print(ostream& out) const
{
  BaseDataInstanceDoubleMatrix::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .BaseDataFlameSensitivityData
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * BaseDataFlameSensitivityData::Clone()
{
  BaseDataFlameSensitivityData *obj = new BaseDataFlameSensitivityData(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .BaseDataFlameSensitivityData
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void BaseDataFlameSensitivityData::CopyClone(Identify * obj)
{
  BaseDataFlameSensitivityData *objfull = (BaseDataFlameSensitivityData *) obj;
  *this = *objfull;
  //Parameter = (BaseData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataFlameSensitivityData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataFlameSensitivityData::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataInstanceDoubleMatrix::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  //result = result && Encode(buffer,---);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataFlameSensitivityData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataFlameSensitivityData::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataInstanceDoubleMatrix::DecodeThis(buffer);
  // The rest

  return result;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool BaseDataFlameSensitivityData::AddAsInstances(BaseDataSetOfInstances &instances,
						  DataSetOfInstancesClass &instclass)
{
  bool result = true;
  return result;
}
/*S DataFlameSensitivityDataClass
 */
/*F DataFlameSensitivityDataClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
DataFlameSensitivityDataClass::DataFlameSensitivityDataClass()
{
  Identification = FLAME_DATA_ID;
  NameTag = FLAME_DATA_NAME;
  SubClass = "InstanceDoubleMatrix";
  EncodeDecodeClass = NameTag;
  Initialize();
} 
/*F DataFlameSensitivityDataClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
DataFlameSensitivityDataClass::DataFlameSensitivityDataClass(const DataFlameSensitivityDataClass& data)
  : DataInstanceDoubleMatrixClass(data)
{
} 
 
/*F DataFlameSensitivityDataClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
DataFlameSensitivityDataClass::DataFlameSensitivityDataClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataInstanceDoubleMatrixClass(id,name,descr)
{
  SubClass = "InstanceDoubleMatrix";
  EncodeDecodeClass = FLAME_DATA_NAME;
}
/*F Initialize()  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
void DataFlameSensitivityDataClass::Initialize() {
  InstanceBaseName = "Pos";
  ProgressVariable = "t[msec]";
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . DataFlameSensitivityDataClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& DataFlameSensitivityDataClass::print(ostream& out) const
{
  DataInstanceDoubleMatrixClass::print(out);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . DataFlameSensitivityDataClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base DataFlameSensitivityDataClass, there is no further information.
**
**  REMARKS
**
*/
bool DataFlameSensitivityDataClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataInstanceDoubleMatrixClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . DataFlameSensitivityDataClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void DataFlameSensitivityDataClass::CopyClone(Identify *  objc)
{
  DataFlameSensitivityDataClass *objcfull = (DataFlameSensitivityDataClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . DataFlameSensitivityDataClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * DataFlameSensitivityDataClass::Clone()
    {
      DataFlameSensitivityDataClass* id = new DataFlameSensitivityDataClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . DataFlameSensitivityDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataFlameSensitivityDataClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataInstanceDoubleMatrixClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  result = result && Encode(buffer,InstanceBaseName);
  result = result && Encode(buffer,ProgressVariable);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . DataFlameSensitivityDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataFlameSensitivityDataClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataInstanceDoubleMatrixClass::DecodeThis(buffer);
  result = result && Decode(buffer,InstanceBaseName);
  result = result && Decode(buffer,ProgressVariable);

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
BaseDataObject * DataFlameSensitivityDataClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new BaseDataFlameSensitivityData();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . DataFlameSensitivityDataClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, DataFlameSensitivityDataClass*& obj)
     {
     obj = new DataFlameSensitivityDataClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . BaseDataFlameSensitivityData
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, BaseDataFlameSensitivityData*& obj)
     {
     obj = new BaseDataFlameSensitivityData;
     return obj->DecodeThis(buffer);
     }

/*S Utilities
 */
 
/*F FlameEncodeDecodeRoutines()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void FlameEncodeDecodeRoutines()
{
  EncodeDecodeRegisterClass(DataFlameSensitivityDataClass,BaseDataFlameSensitivityData,FLAME_DATA_NAME);
}
 
/*F AddCobwebAlgorithmClasses(set)
**
**  DESCRIPTION
**    set: The set of object classes
**
**  REMARKS
**
*/
void AddFlameClasses(DataSetOfObjectsClass& set)
{
  String flamedescr("The Class");
  DataFlameSensitivityDataClass flameclass(FLAME_DATA_ID,FLAME_DATA_NAME,flamedescr);
  set.AddObjectClass(flameclass);
}
