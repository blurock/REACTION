/*  FILE     EquilibriumConst.cc
**  PACKAGE  EquilibriumConst
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "EquilibriumConst" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "FullSystem.hh"
#include "Dbase.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "ThermoTables.hh"
#include "ThermoProps.hh"
#include "Rxn.hh"
#include "EquilibriumConst.hh"
#include "MatrixUtilities.hh"

/*S RxnDataEquilibriumConstant
 */ 
/*F RxnDataEquilibriumConstant()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataEquilibriumConstant::RxnDataEquilibriumConstant()
{
  Identification = EQUIL_CONSTANT_ID;
  NameTag = EQUIL_CONSTANT_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataEquilibriumConstant(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataEquilibriumConstant::RxnDataEquilibriumConstant(const RxnDataEquilibriumConstant& data)
  : RxnDataReactionRates(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataEquilibriumConstant
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataEquilibriumConstant::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataEquilibriumConstant
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataEquilibriumConstant::Read(istream& in, DataObjectClass* objc, const String& name)
{
  //bool result = RxnDataReactionRates::Read(in,objc,name);
  bool result = ReadAsMolFile(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataEquilibriumConstant
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataEquilibriumConstant::print(ostream& out) const
{
  RxnDataReactionRates::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataEquilibriumConstant
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataEquilibriumConstant::Clone()
{
  RxnDataEquilibriumConstant *obj = new RxnDataEquilibriumConstant(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataEquilibriumConstant
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataEquilibriumConstant::CopyClone(Identify * obj)
{
  RxnDataEquilibriumConstant *objfull = (RxnDataEquilibriumConstant *) obj;
  *this = *objfull;
  RxnDataReactionRates::CopyClone(obj);
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataEquilibriumConstant
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataEquilibriumConstant::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionRates::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataEquilibriumConstant
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataEquilibriumConstant::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionRates::DecodeThis(buffer);
  return result;
}
/*S RxnEquilibriumConstantClass
 */
/*F RxnEquilibriumConstantClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnEquilibriumConstantClass::RxnEquilibriumConstantClass()
{
  Identification = EQUIL_CONSTANT_ID;
  NameTag = EQUIL_CONSTANT_NAME;
  SubClass = "ReactionRates";
  EncodeDecodeClass = NameTag;
} 
/*F RxnEquilibriumConstantClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnEquilibriumConstantClass::RxnEquilibriumConstantClass(const RxnEquilibriumConstantClass& data)
  : RxnReactionRatesClass(data)
{
} 
 
/*F RxnEquilibriumConstantClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnEquilibriumConstantClass::RxnEquilibriumConstantClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnReactionRatesClass(id,name,descr)
{
  SubClass = "ReactionRates";
  EncodeDecodeClass = "EquilibriumConstant";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnEquilibriumConstantClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnEquilibriumConstantClass::print(ostream& out) const
{
  RxnReactionRatesClass::print(out);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnEquilibriumConstantClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnEquilibriumConstantClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnEquilibriumConstantClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnReactionRatesClass::Read(in,set);
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnEquilibriumConstantClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnEquilibriumConstantClass::CopyClone(Identify *  objc)
{
  RxnEquilibriumConstantClass *objcfull = (RxnEquilibriumConstantClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnEquilibriumConstantClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnEquilibriumConstantClass::Clone()
{
  RxnEquilibriumConstantClass* id = new RxnEquilibriumConstantClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnEquilibriumConstantClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnEquilibriumConstantClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionRatesClass::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnEquilibriumConstantClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnEquilibriumConstantClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionRatesClass::DecodeThis(buffer);
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
BaseDataObject * RxnEquilibriumConstantClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataEquilibriumConstant();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnEquilibriumConstantClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnEquilibriumConstantClass*& obj)
     {
     obj = new RxnEquilibriumConstantClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataEquilibriumConstant
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataEquilibriumConstant*& obj)
     {
     obj = new RxnDataEquilibriumConstant;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataCalculateReverseRate
 */ 
/*F RxnDataCalculateReverseRate()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataCalculateReverseRate::RxnDataCalculateReverseRate()
  : Unity(NULL),
    MolThermoName("EquilibriumConstant"),
    EquilibriumUnits("EquilibriumUnits"),
    RateUnits("RateUnits")
{
  Identification = EQUIL_OPERATION_ID;
  NameTag = EQUIL_OPERATION_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataCalculateReverseRate(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataCalculateReverseRate::RxnDataCalculateReverseRate(const RxnDataCalculateReverseRate& data)
  : BaseDataOperation(data),
    MolThermoName(data.MolThermoName),
    EquilibriumUnits(data.EquilibriumUnits),
    RateUnits(data.RateUnits)
{
  Unity = (RxnDataEquilibriumConstant *) PointerClone(data.Unity);
}
/*F RxnDataCalculateReverseRate()  . . . . . . . . . . . . . . . . . . . destructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataCalculateReverseRate::~RxnDataCalculateReverseRate()
{
  if(Unity != NULL)
    delete Unity;
  Unity = NULL;
}

/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataCalculateReverseRate
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataCalculateReverseRate::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataCalculateReverseRate
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataCalculateReverseRate::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataOperation::Read(in,objc,name);
  RxnCalculateReverseRateClass *objclass = (RxnCalculateReverseRateClass *) objc;
  StreamObjectInput str(in,' ');
  String rev("CalculateReverseRate:");
  result = result && CheckReadKeyWord(in,rev);
  if(result)
    {
      MolThermoName = str.ReadNext();
      String notdefined("Not Defined");
      result = result && PointerObjectRead(in, (BaseDataObject *&) Unity,
					   objclass->getEquilibriumClass(),notdefined);
    }
  MolThermoName = str.ReadNext();
  EquilibriumUnits = str.ReadNext();
  RateUnits = str.ReadNext();
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataCalculateReverseRate
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataCalculateReverseRate::print(ostream& out) const
{
  BaseDataOperation::print(out);
  out << "Molecule Equilibrium Source: '" << MolThermoName << "'" << endl;
  PointerPrint(out,"The Equilibrium Unity Constant: ","None",Unity);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataCalculateReverseRate
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataCalculateReverseRate::Clone()
{
  RxnDataCalculateReverseRate *obj = new RxnDataCalculateReverseRate(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataCalculateReverseRate
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataCalculateReverseRate::CopyClone(Identify * obj)
{
  RxnDataCalculateReverseRate *objfull = (RxnDataCalculateReverseRate *) obj;
  *this = *objfull;
  Unity = (RxnDataEquilibriumConstant *) PointerClone(objfull->Unity);
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataCalculateReverseRate
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataCalculateReverseRate::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::EncodeThis(buffer);
  result = result && PointerEncode(buffer,Unity);
  result = result && Encode(buffer,MolThermoName);
  result = result && Encode(buffer,EquilibriumUnits);
  result = result && Encode(buffer,RateUnits);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataCalculateReverseRate
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataCalculateReverseRate::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) Unity);
  result = result && Decode(buffer,MolThermoName);
  result = result && Decode(buffer,EquilibriumUnits);
  result = result && Decode(buffer,RateUnits);
  return result;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataEquilibriumConstant *RxnDataCalculateReverseRate::EquilibriumMultMolecule(String& property,
										 RxnDataEquilibriumConstant *reactequ,
										 RxnEquilibriumConstantClass *equclass,
										 RxnDataSimpleMolecule *mol)
{
  RxnDataEquilibriumConstant *newequ;
  RxnDataEquilibriumConstant *equ = (RxnDataEquilibriumConstant *) mol->RetrieveProperty(MolThermoName);
  if(equ != NULL)
    {
      newequ = (RxnDataEquilibriumConstant *) equclass->ConvertRateConstants(equ,1,property);
      newequ = (RxnDataEquilibriumConstant *) reactequ->Mult(newequ,equclass);
      delete reactequ;
    }
  else
    {
      cerr << "Equilibrium Constant: '" << MolThermoName << "' not found in '" << mol->NameTag << "'" << endl;
      newequ = reactequ;
    }
  return newequ;
}
/*F obj = operator()(x,y,xclass,yclass) . . . . . . . . . . BaseDataRetrieveMoleculeProperty
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
BaseDataObject *RxnDataCalculateReverseRate::operator()(BaseDataObject *x, BaseDataObject *y,
							   DataObjectClass *xc, DataObjectClass *yc)
{
  return (BaseDataObject *) x->Clone();
}
/*F obj = operator()(x,xclass)  . . . . . . . . . . . . . . BaseDataRetrieveMoleculeProperty
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
BaseDataObject *RxnDataCalculateReverseRate::operator()(BaseDataObject *x,
							DataObjectClass *xc)
{
  cout << "RxnDataCalculateReverseRate::operator() The Begining" << endl;
  RxnDataReaction   *rxn = (RxnDataReaction *) x;
  RxnReactionClass  *rxnclass = (RxnReactionClass *) xc;
  DataSetOfObjectsClass *classes = rxnclass->PointerToAllowedClasses();
  
  ObjectList<String>::iterator name;
  RxnDataMoleculeSet *forwardmols = rxn->getReactants();
  
  RxnDataEquilibriumConstant *reactequ = (RxnDataEquilibriumConstant *) Unity->Clone();
  RxnEquilibriumConstantClass *equclass = (RxnEquilibriumConstantClass *) classes->GetObjectClass(Unity->GetType());

  BaseDataKeySet forwardset = rxn->getInternalReactantNames();
  ObjectList<String> names = forwardset.GetKeyWords();
  names.print(cout);
  for(name = names.begin(); name != names.end(); name++)
    {
      String reactant = rxn->getReactantName(*name);
      RxnDataSimpleMolecule *mol = (RxnDataSimpleMolecule *) forwardmols->GetObject(reactant);
      reactequ = EquilibriumMultMolecule(EquilibriumUnits,reactequ,equclass,mol);
    }

  RxnDataMoleculeSet *reversemols = rxn->getProducts();
  RxnDataEquilibriumConstant *prodequ = (RxnDataEquilibriumConstant *) Unity->Clone();
  BaseDataKeySet reverseset = rxn->getInternalProductNames();
  ObjectList<String> revnames = reverseset.GetKeyWords();
  for(name = revnames.begin(); name != revnames.end(); name++)
    {
      String product = rxn->getProductName(*name);
      if(reversemols->IsInList(product))
	{
	  RxnDataSimpleMolecule *mol = (RxnDataSimpleMolecule *) reversemols->GetObject(product);
	  prodequ = EquilibriumMultMolecule(EquilibriumUnits,prodequ,equclass,mol);
	}
      else
	{
	  cerr << "Product molecule not found" << endl;
	}
    }

  cout << "RxnDataCalculateReverseRate::operator() 1" << endl;
  cout << "Reactants:" << endl;
  reactequ->print(cout);
  cout << endl;
  cout << "Products:" << endl;
  prodequ->print(cout);
  cout << endl;
  RxnDataEquilibriumConstant *equ = (RxnDataEquilibriumConstant *) reactequ->Div(prodequ,equclass);
  cout << "Equilibrium Constants" << endl;
  equ->print(cout);
  cout << endl;

  RxnDataReactionRates *reverse;
  if(rxn->IsInList(rxnclass->getStandardForwardName()))
    {
      BaseDataObject *rxnrate = rxn->GetObject(rxnclass->getStandardForwardName());
      RxnReactionRatesClass *rateclass = (RxnReactionRatesClass *) classes->GetObjectClass(rxnrate->GetType());
      RxnDataReactionRates *forrate = rxn->GetRateConstants(rxnclass->getStandardForwardName(),
							    true,RateUnits,rateclass);
      cout << "Rate Constant" << endl;
      forrate->print(cout);
      cout << endl;
      reverse = forrate->Mult(equ,equclass);
      cout << "Reverse" << endl;
      reverse->print(cout);
      cout << endl;

      delete equ;
    }
  else
    {
      cerr << "Forward Rate: '" << rxnclass->getStandardForwardName() << "' not specified: Unity used" << endl;
      reverse = equ;
    }
  delete reactequ;
  delete prodequ;

  cout << "RxnDataCalculateReverseRate::operator() The End" << endl;
  return (BaseDataObject *) reverse;
}
/*F obj = operator()(cls,x,y,xclass,yclass) . . . . . . . . . . BaseDataRetrieveMoleculeProperty
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
BaseDataObject *RxnDataCalculateReverseRate::operator()(DataObjectClass *cls,
							BaseDataObject *x, BaseDataObject *y,
							DataObjectClass *xc, DataObjectClass *yc)
                                              
{
  return (BaseDataObject *) x->Clone();
}
/*F obj = operator()(cls,x,xclass)  . . . . . . . . . . . . . . BaseDataRetrieveMoleculeProperty
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
BaseDataObject *RxnDataCalculateReverseRate::operator()(DataObjectClass *cls,
						  BaseDataObject *x,
						  DataObjectClass *xclass)
{
  RxnRetrieveMoleculePropertyClass *rxncls = (RxnRetrieveMoleculePropertyClass *) cls;
  DataSetOfObjectsClass *set = rxncls->PointerToAllowedClasses();

  bool result = true;
  result = result && set->IsOfClass(*xclass,REACTION_RATES_NAME);
  result = result && (x->GetType() == xclass->GetType());
  return operator()(x,xclass);
}
/*S RxnCalculateReverseRateClass
 */
/*F RxnCalculateReverseRateClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnCalculateReverseRateClass::RxnCalculateReverseRateClass()
  : EquilibriumClass(NULL)
{
  Identification = EQUIL_OPERATION_ID;
  NameTag = EQUIL_OPERATION_NAME;
  SubClass = "Operation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnCalculateReverseRateClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnCalculateReverseRateClass::RxnCalculateReverseRateClass(const RxnCalculateReverseRateClass& data)
  : DataOperationClass(data)
{
  EquilibriumClass = (RxnEquilibriumConstantClass *) PointerClone(data.EquilibriumClass);
}
/*F RxnCalculateReverseRateClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnCalculateReverseRateClass::RxnCalculateReverseRateClass(const int id, 
							   const String& name,
							   const String& descr)
  : DataOperationClass(id,name,descr),
    EquilibriumClass(NULL)
{
  SubClass = "Operation";
  EncodeDecodeClass = "CalculateReverseRate";
}
/*F RxnCalculateReverseRateClass() . . . . . . . . . . . . . . . . . . . destructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnCalculateReverseRateClass::~RxnCalculateReverseRateClass()
{
  if(EquilibriumClass != NULL)
    delete EquilibriumClass;
  EquilibriumClass = NULL;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnCalculateReverseRateClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnCalculateReverseRateClass::print(ostream& out) const
{
  DataOperationClass::print(out);
  PointerPrint(out,"  The Equilibrium Constant Class: "," No Class Defined ",EquilibriumClass);
  return out;
}
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnCalculateReverseRateClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnCalculateReverseRateClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnCalculateReverseRateClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataOperationClass::Read(in,set);
  result = result && PointerClassRead(in,(DataObjectClass *&) EquilibriumClass,
				      EQUIL_CONSTANT_NAME,
				      set," No Class ");
  return result;
} 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnCalculateReverseRateClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnCalculateReverseRateClass::CopyClone(Identify *  objc)
{
  RxnCalculateReverseRateClass *objcfull = (RxnCalculateReverseRateClass *) objc;
  *this = *objcfull;
  EquilibriumClass = (RxnEquilibriumConstantClass *) PointerClone(objcfull->EquilibriumClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnCalculateReverseRateClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnCalculateReverseRateClass::Clone()
{
  RxnCalculateReverseRateClass* id = new RxnCalculateReverseRateClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnCalculateReverseRateClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnCalculateReverseRateClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,EquilibriumClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnCalculateReverseRateClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnCalculateReverseRateClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) EquilibriumClass);
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
BaseDataObject * RxnCalculateReverseRateClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataCalculateReverseRate();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnCalculateReverseRateClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnCalculateReverseRateClass*& obj)
     {
     obj = new RxnCalculateReverseRateClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataCalculateReverseRate
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataCalculateReverseRate*& obj)
     {
     obj = new RxnDataCalculateReverseRate;
     return obj->DecodeThis(buffer);
     }
/*F equ = getEquilibriumClass() . . . . . . . .  RxnCalculateReverseRateClass
**
**  DESCRIPTION
**    equ: The equilibrium class
**
**  REMARKS
**
*/
RxnEquilibriumConstantClass *RxnCalculateReverseRateClass::getEquilibriumClass()
{
  return EquilibriumClass;
}
/*S RxnDataOperationMoleculeEquilibrium
 */ 
/*F RxnDataOperationMoleculeEquilibrium()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataOperationMoleculeEquilibrium::RxnDataOperationMoleculeEquilibrium()
  : ThermodynamicConstantsS("ThermodynamicConstant"),
    EquilibriumTypeS("ThermodynamicClass"),
    GCUnits("GCUnits")
{
  Identification = EQUIL_EQUILIBRIUM_ID;
  NameTag = EQUIL_EQUILIBRIUM_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataOperationMoleculeEquilibrium(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataOperationMoleculeEquilibrium::RxnDataOperationMoleculeEquilibrium(const RxnDataOperationMoleculeEquilibrium& data)
  : BaseDataOperation(data),
    ThermodynamicConstantsS(data.ThermodynamicConstantsS),
    EquilibriumTypeS(data.EquilibriumTypeS),
    GCUnits(data.GCUnits),
    Temperatures(data.Temperatures)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataOperationMoleculeEquilibrium
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataOperationMoleculeEquilibrium::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataOperationMoleculeEquilibrium
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataOperationMoleculeEquilibrium::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataOperation::Read(in,objc,name);

  String therm("ThermoConstant:");
  result = result && CheckReadKeyWord(in,therm);

  StreamObjectInput str(in,' ');
  ThermodynamicConstantsS = str.ReadNext();
  EquilibriumTypeS = str.ReadNext();
  GCUnits = str.ReadNext();

  Temperatures.Read(in);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataOperationMoleculeEquilibrium
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataOperationMoleculeEquilibrium::print(ostream& out) const
{
  BaseDataOperation::print(out);
  out << "Constants: '" << ThermodynamicConstantsS;
  out << "' '" << EquilibriumTypeS;
  out << "' '" << GCUnits << endl;
  out << "Temperatures to Calculate:" << endl;
  Temperatures.print(cout);

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataOperationMoleculeEquilibrium
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataOperationMoleculeEquilibrium::Clone()
{
  RxnDataOperationMoleculeEquilibrium *obj = new RxnDataOperationMoleculeEquilibrium(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataOperationMoleculeEquilibrium
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataOperationMoleculeEquilibrium::CopyClone(Identify * obj)
{
  RxnDataOperationMoleculeEquilibrium *objfull = (RxnDataOperationMoleculeEquilibrium *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataOperationMoleculeEquilibrium
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataOperationMoleculeEquilibrium::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::EncodeThis(buffer);
  result = result && Encode(buffer,ThermodynamicConstantsS);
  result = result && Encode(buffer,EquilibriumTypeS);
  result = result && Encode(buffer,GCUnits);
  result = result && Temperatures.EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataOperationMoleculeEquilibrium
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataOperationMoleculeEquilibrium::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::DecodeThis(buffer);
  result = result && Decode(buffer,ThermodynamicConstantsS);
  result = result && Decode(buffer,EquilibriumTypeS);
  result = result && Decode(buffer,GCUnits);
  result = result && Temperatures.DecodeThis(buffer);
  return result;
}
/*F obj = operator()(x,y,xclass,yclass) . . . . . . . . . . RxnDataOperationMoleculeEquilibrium
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
BaseDataObject *RxnDataOperationMoleculeEquilibrium::operator()(BaseDataObject *x, BaseDataObject *y,
								DataObjectClass *xc, DataObjectClass *yc)
{
  return operator()(x,xc);
}
/*F obj = operator()(x,xclass)  . . . . . . . . . . . . . . RxnDataOperationMoleculeEquilibrium
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
BaseDataObject *RxnDataOperationMoleculeEquilibrium::operator()(BaseDataObject *x,
								DataObjectClass *xc)
{
  RxnDataSimpleMolecule *molecule = (RxnDataSimpleMolecule *) x;
  RxnSimpleMoleculeClass *moleculeclass = (RxnSimpleMoleculeClass *) xc;
  DataSetOfObjectsClass *classes = moleculeclass->PointerToAllowedClasses();
  RxnDataEquilibriumConstant *equil;
  unsigned int n = Temperatures.size();
  MatrixNumeric mat(n,3);
  VectorNumeric ans(n);
  RxnDataThermoProperty *thermo = (RxnDataThermoProperty *) molecule->RetrieveProperty(ThermodynamicConstantsS);
  if(thermo != NULL)
    {
      RxnThermoPropertyClass *thermoclass = (RxnThermoPropertyClass *) classes->GetObjectClass(thermo->GetType());
      for(unsigned int i=0;i<n;i++)
	{
	  double temp = Temperatures[i];
	  mat[i][0] = 1.0;
	  mat[i][1] = log(temp);
	  double r = thermoclass->getGasConstant();
	  double rconverted = thermoclass->Convert(true,GCUnits,r);
	  mat[i][2] = -1.0/(rconverted*temp);
	  double equvalue = thermo->CalculateEquilibrium(thermoclass,temp);
	  ans[i] = log(equvalue);
	}
      VectorNumeric coeffs = SolveLinearSystemOfEquations(mat,ans);
      MatrixNumeric coeffmat(3,1);
      coeffmat[0][0] = coeffs[0];
      coeffmat[1][0] = coeffs[1];
      coeffmat[2][0] = coeffs[2];

      cout << endl << "Matrix: " << endl;
      mat.print(cout);
      cout << endl << "Coefficients: " << endl;
      coeffmat.print(cout);
      cout << exp(coeffs[0]) << "* T^(" << coeffs[1] << ") * exp( ";
      cout << coeffs[2]/(thermoclass->getGasConstant()) << ")/T" << endl;
      cout << endl << "Thermo: " << endl;
      MatrixNumeric matthermo = mat * coeffmat;
      matthermo.print(cout);
      cout << endl << "Expected: " << endl;
      ans.print(cout);
      cout << endl;

      RxnEquilibriumConstantClass *equilclass = (RxnEquilibriumConstantClass *) 
	classes->GetObjectClass(EquilibriumTypeS);
      equil = (RxnDataEquilibriumConstant *) equilclass->BaseDataObjectExample();
      BaseDataReal *Arr = new BaseDataReal();
      BaseDataReal *Energy = new BaseDataReal();
      BaseDataReal *TCoeff = new BaseDataReal();
      Arr->SetValue(exp(coeffs[0]));
      TCoeff->SetValue(coeffs[1]);
      Energy->SetValue(coeffs[2]);
      cout << "------------------------------------------------------------" << endl;
      cout << "Calculated Constants:" << endl;
      Arr->print(cout);
      cout << endl;
      TCoeff->print(cout);
      cout << endl;
      Energy->print(cout);
      cout << endl;
      cout << "------------------------------------------------------------" << endl;
      equilclass->print(cout);
      cout << endl;
      cout << "------------------------------------------------------------" << endl;
      equil->setArrhenius(Arr,equilclass);
      equil->setActivationEnergy(Energy,equilclass);
      equil->setTemperatureCoefficient(TCoeff,equilclass);
      delete Arr;
      delete TCoeff;
      delete Energy;
    }
  return (BaseDataObject *) equil;
}
/*F obj = operator()(cls,x,y,xclass,yclass) . . . . . . . . . . RxnDataOperationMoleculeEquilibrium
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
BaseDataObject *RxnDataOperationMoleculeEquilibrium::operator()(DataObjectClass *cls,
								BaseDataObject *x, BaseDataObject *y,
								DataObjectClass *xc, DataObjectClass *yc)
                                              
{
  /*
    DataSetOfObjectsClass *set = (BaseDataSetOfObjects *) cls->PointerToAllowedClasses();
    bool result = true;
    result = result && set->IsOfClass(xc,_NAME);
    result = result && set->IsOfClass(yc,_NAME);
    result = result && (x->GetType() == xc->GetType());
    result = result && (y->GetType() == yc->GetType());
  */
  return operator()(x,y,xc,yc);
}
/*F obj = operator()(cls,x,xclass)  . . . . . . . . . . . . . . RxnDataOperationMoleculeEquilibrium
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
BaseDataObject *RxnDataOperationMoleculeEquilibrium::operator()(DataObjectClass *cls,
						  BaseDataObject *x,
						  DataObjectClass *xclass)
{
  /*
    DataSetOfObjectsClass *set = (BaseDataSetOfObjects *) cls->PointerToAllowedClasses();
    bool result = true;
    result = result && set->IsOfClass(xc,_NAME);
    result = result && (x->GetType() == xc->GetType());
  */
  return operator()(x,xclass);
}
 
 
/*S RxnOperationMoleculeEquilibriumClass
 */
/*F RxnOperationMoleculeEquilibriumClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnOperationMoleculeEquilibriumClass::RxnOperationMoleculeEquilibriumClass()
{
  Identification = EQUIL_EQUILIBRIUM_ID;
  NameTag = EQUIL_EQUILIBRIUM_NAME;
  SubClass = "Operation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnOperationMoleculeEquilibriumClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnOperationMoleculeEquilibriumClass::RxnOperationMoleculeEquilibriumClass(const RxnOperationMoleculeEquilibriumClass& data)
  : DataOperationClass(data)
{
} 
 
/*F RxnOperationMoleculeEquilibriumClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnOperationMoleculeEquilibriumClass::RxnOperationMoleculeEquilibriumClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataOperationClass(id,name,descr)
{
  SubClass = "Operation";
  EncodeDecodeClass = "OperationMoleculeEquilibrium";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnOperationMoleculeEquilibriumClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnOperationMoleculeEquilibriumClass::print(ostream& out) const
{
  DataOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnOperationMoleculeEquilibriumClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnOperationMoleculeEquilibriumClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnOperationMoleculeEquilibriumClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnOperationMoleculeEquilibriumClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnOperationMoleculeEquilibriumClass::CopyClone(Identify *  objc)
{
  RxnOperationMoleculeEquilibriumClass *objcfull = (RxnOperationMoleculeEquilibriumClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnOperationMoleculeEquilibriumClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnOperationMoleculeEquilibriumClass::Clone()
    {
      RxnOperationMoleculeEquilibriumClass* id = new RxnOperationMoleculeEquilibriumClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnOperationMoleculeEquilibriumClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnOperationMoleculeEquilibriumClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnOperationMoleculeEquilibriumClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnOperationMoleculeEquilibriumClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnOperationMoleculeEquilibriumClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataOperationMoleculeEquilibrium();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnOperationMoleculeEquilibriumClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnOperationMoleculeEquilibriumClass*& obj)
     {
     obj = new RxnOperationMoleculeEquilibriumClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataOperationMoleculeEquilibrium
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataOperationMoleculeEquilibrium*& obj)
     {
     obj = new RxnDataOperationMoleculeEquilibrium;
     return obj->DecodeThis(buffer);
     }


/*S Utilities
 */

/*F AddEquilibriumConstClasses(set) . . . . . . . . . . . .  EquilibriumConst
**
**  DESCRIPTION
**    set: The set of classes to add them to
**
**  REMARKS
**
*/
void AddEquilibriumConstClasses(DataSetOfObjectsClass& set)
{
  String equdescr("The Molecule Equilibrium Constant Class");
  RxnEquilibriumConstantClass equclass(EQUIL_CONSTANT_ID,EQUIL_CONSTANT_NAME,equdescr);
  set.AddObjectClass(equclass);

  String opdescr("The Reverse Rate Calculation Class");
  RxnCalculateReverseRateClass opclass(EQUIL_OPERATION_ID,EQUIL_OPERATION_NAME,opdescr);
  set.AddObjectClass(opclass);

  String equildescr("The Calculate Equilibrium Constant Class");
  RxnOperationMoleculeEquilibriumClass equilclass(EQUIL_EQUILIBRIUM_ID,EQUIL_EQUILIBRIUM_NAME,equildescr);
  set.AddObjectClass(equilclass);
}
/*F InitializeRxnPropertyDecodeFunctions()  . . . . . . . . . . . . Reactions
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialSetOfEquilibriumConstDecodeFunctions()
{
  EncodeDecodeRegisterClass(RxnCalculateReverseRateClass,RxnDataCalculateReverseRate,EQUIL_OPERATION_NAME);
  EncodeDecodeRegisterClass(RxnEquilibriumConstantClass,RxnDataEquilibriumConstant,EQUIL_CONSTANT_NAME);
  EncodeDecodeRegisterClass(RxnOperationMoleculeEquilibriumClass,RxnDataOperationMoleculeEquilibrium,EQUIL_EQUILIBRIUM_NAME);
}
