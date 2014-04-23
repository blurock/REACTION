/*  FILE     Rxn.cc
**  PACKAGE  Rxn
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "Rxn" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
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
#include "ThermoProps.hh"
#include "Utilities.hh"
#include "Rxn.hh"
#include "EquilibriumConst.hh"

String GeneratePairPrefix("P");
int InputReaction(ReactionSystemBase* sys);

/*S RxnDataReactionRates
 */ 
/*F RxnDataReactionRates()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionRates::RxnDataReactionRates()
{
  Identification = REACTION_RATES_ID;
  NameTag = REACTION_RATES_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataReactionRates(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataReactionRates::RxnDataReactionRates(const RxnDataReactionRates& data)
  : RxnDataRealBasedProperty(data)
{
}
/*F RxnDataReactionRates()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionRates::~RxnDataReactionRates()
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataReactionRates
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataReactionRates::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataReactionRates
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataReactionRates::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = RxnDataRealBasedProperty::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataReactionRates::print(ostream& out) const
{
  RxnDataRealBasedProperty::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionRates
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataReactionRates::Clone()
{
  RxnDataReactionRates *obj = new RxnDataReactionRates(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionRates
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataReactionRates::CopyClone(Identify * obj)
{
  RxnDataReactionRates *objfull = (RxnDataReactionRates *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionRates::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataRealBasedProperty::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionRates::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataRealBasedProperty::DecodeThis(buffer);
  return result;
}
/*F val = getArrheniusValue() . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The Arrhenius value
**
**  REMARKS
**
*/
double RxnDataReactionRates::getArrheniusValue(RxnReactionRatesClass *rateclass)
{
  double value = 0.0;
  if(IsInList(rateclass->ArrheniusName))
    {
      BaseDataNumeric *num = (BaseDataNumeric *) GetObject(rateclass->ArrheniusName);
      value = num->Distance();
    }
  return value;
}
/*F val = getActivationEnergyValue() . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The ActivationEnergy value
**
**  REMARKS
**
*/
double RxnDataReactionRates::getActivationEnergyValue(RxnReactionRatesClass *rateclass)
{
  double value = 0.0;
  if(IsInList(rateclass->ActivationEnergyName))
    {
      BaseDataNumeric *num = (BaseDataNumeric *) GetObject(rateclass->ActivationEnergyName);
      value = num->Distance();
    }
  return value;
}

/*F val = getTemperatureCoefficientValue() . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The TemperatureCoefficient value
**
**  REMARKS
**
*/
double RxnDataReactionRates::getTemperatureCoefficientValue(RxnReactionRatesClass *rateclass)
{
  double value = 0.0;
  if(IsInList(rateclass->TemperatureCoefficientName))
    {
      BaseDataNumeric *num = (BaseDataNumeric *) GetObject(rateclass->TemperatureCoefficientName);
      value = num->Distance();
    }
  return value;
}
 
/*F val = getArrhenius()  . . . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The Arrhenius structure
**
**  REMARKS
**
*/
BaseDataNumeric *RxnDataReactionRates::getArrhenius(RxnReactionRatesClass *rateclass)
{
  BaseDataNumeric *num = NULL;
  if(IsInList(rateclass->ArrheniusName))
    num = (BaseDataNumeric *) GetObject(rateclass->ArrheniusName);
  return num;
}
/*F val = getActivationEnergy() . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The ActivationEnergy structure
**
**  REMARKS
**
*/
BaseDataNumeric *RxnDataReactionRates::getActivationEnergy(RxnReactionRatesClass *rateclass)
{
  BaseDataNumeric *num = NULL;
  if(IsInList(rateclass->ActivationEnergyName))
    num = (BaseDataNumeric *) GetObject(rateclass->ActivationEnergyName);
  return num;
}
/*F val = getTemperatureCoefficient() . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The TemperatureCoefficient structure
**
**  REMARKS
**
*/
BaseDataNumeric *RxnDataReactionRates::getTemperatureCoefficient(RxnReactionRatesClass *rateclass)
{
  BaseDataNumeric *num = NULL;
  if(IsInList(rateclass->TemperatureCoefficientName))
    num = (BaseDataNumeric *) GetObject(rateclass->TemperatureCoefficientName);
  return num;
}
/*F setArrhenius(val)  . . . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The Arrhenius structure
**
**  REMARKS
**
*/
void RxnDataReactionRates::setArrhenius(BaseDataNumeric *A, RxnReactionRatesClass *rateclass)
{
  BaseDataObject *num = (BaseDataObject *) A->Clone();
  num->NameTag = rateclass->ArrheniusName;
  AddObject(num);
  delete num;
}
/*F setActivationEnergy(val) . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The ActivationEnergy structure
**
**  REMARKS
**
*/
void RxnDataReactionRates::setActivationEnergy(BaseDataNumeric *E,RxnReactionRatesClass *rateclass)
{
  BaseDataObject *num = (BaseDataObject *) E->Clone();
  num->NameTag = rateclass->ActivationEnergyName;
  AddObject(num);
  delete num;
}
/*F setTemperatureCoefficient(val) . . . . . . . . . . . . .  RxnDataReactionRates
**
**  DESCRIPTION
**    val: The TemperatureCoefficient structure
**
**  REMARKS
**
*/
void RxnDataReactionRates::setTemperatureCoefficient(BaseDataNumeric *n, RxnReactionRatesClass *rateclass)
{
  BaseDataObject *num = (BaseDataObject *) n->Clone();
  num->NameTag = rateclass->TemperatureCoefficientName;
  AddObject(num);
  delete num;
}
/*F m = operator*(rate)  . . . . . . . . . . . . StandardRateConstants
**
**  DESCRIPTION
**    rate1,rate2: The rate constants
**    m = Their product
**
**  REMARKS
**
*/
RxnDataReactionRates *RxnDataReactionRates::Mult(RxnDataReactionRates *rate, RxnReactionRatesClass *rateclass)
{
  BaseDataNumeric *Arrhenius = getArrhenius(rateclass);
  BaseDataNumeric *TemperatureCoefficient = getTemperatureCoefficient(rateclass);
  BaseDataNumeric *ActivationEnergy = getActivationEnergy(rateclass);
  BaseDataNumeric *A = (BaseDataNumeric *) Arrhenius->operator*(rate->getArrhenius(rateclass));
  BaseDataNumeric *n = (BaseDataNumeric *) TemperatureCoefficient->operator+(rate->getTemperatureCoefficient(rateclass));
  BaseDataNumeric *E = (BaseDataNumeric *) ActivationEnergy->operator+(rate->getActivationEnergy(rateclass));

  RxnDataReactionRates *r = (RxnDataReactionRates *) Clone();
  r->setArrhenius(A,rateclass);
  r->setTemperatureCoefficient(n,rateclass);
  r->setActivationEnergy(E,rateclass);

  delete A;
  delete n;
  delete E;

  return r;
}
/*F m = operator/(rate1,rate2)  . . . . . . . . . . . . StandardRateConstants
**
**  DESCRIPTION
**    rate1,rate2: The rate constants
**    m = Their ratio
**
**  REMARKS
**
*/
RxnDataReactionRates* RxnDataReactionRates::Div(RxnDataReactionRates* rate, RxnReactionRatesClass *rateclass)
{
  BaseDataNumeric *Arrhenius = getArrhenius(rateclass);
  BaseDataNumeric *TemperatureCoefficient = getTemperatureCoefficient(rateclass);
  BaseDataNumeric *ActivationEnergy = getActivationEnergy(rateclass);
  BaseDataNumeric *A = (BaseDataNumeric *) Arrhenius->operator/(rate->getArrhenius(rateclass));
  BaseDataNumeric *n = (BaseDataNumeric *) TemperatureCoefficient->operator-(rate->getTemperatureCoefficient(rateclass));
  BaseDataNumeric *E = (BaseDataNumeric *) ActivationEnergy->operator-(rate->getActivationEnergy(rateclass));
  
  RxnDataReactionRates *r = (RxnDataReactionRates *) Clone();
  r->setArrhenius(A,rateclass);
  r->setTemperatureCoefficient(n,rateclass);
  r->setActivationEnergy(E,rateclass);

  delete A;
  delete n;
  delete E;

  return r;
}
/*F ReadAsMolFile(in,objc,name)  . . . . . . .  . . . . .  Read in RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataReactionRates::ReadAsMolFile(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = RxnDataReactionRates::Read(in,objc,name);
  result = result && ReadConstants(in,objc);
  return result;
}
 
/*F ans = ReadConstants(in,objc)
**
**  DESCRIPTION
**    in: The input stream
**    objc: The rate class
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionRates::ReadConstants(istream& in, DataObjectClass* objc)
{
  bool result = true;
  RxnReactionRatesRxnFileClass *objcfull = (RxnReactionRatesRxnFileClass *) objc;
  String Type,ArrS,TempS,ExpS;

  StreamObjectInput str(in,' ');
  ArrS = str.ReadNext();
  TempS = str.ReadNext();
  ExpS = str.ReadNext();

  double arr = ArrS.ToFloat();
  double t   = TempS.ToFloat();
  double e   = ExpS.ToFloat();

  DataRealClass *rc = (DataRealClass *) objcfull->getRateType();
  BaseDataReal *Arr = (BaseDataReal *) rc->BaseDataObjectExample();
  Arr->SetValue(arr);
  Arr->NameTag = "Arrhenius";
  BaseDataReal *ActE = (BaseDataReal *) rc->BaseDataObjectExample();
  ActE->SetValue(e);
  ActE->NameTag = "ActivationEnergy";
  BaseDataReal *TempC = (BaseDataReal *) rc->BaseDataObjectExample();
  TempC->SetValue(t);
  TempC->NameTag = "TemperatureCoefficient";

  
  setArrhenius(Arr,objcfull);
  setActivationEnergy(ActE,objcfull);
  setTemperatureCoefficient(TempC,objcfull);
  return result;
}
 
/*F ans = WriteOutRates(out,n,rateclass,property,delimitor)
**
**  DESCRIPTION
**    out: The output stream
**    n: The number of reactants
**    rateclass: The rate class
**    property: The property units to convert to
**    delimitor: The delimitor between the constants (and at end)
**
**  REMARKS
**
*/
bool RxnDataReactionRates::WriteOutRates(ostream& out,
					 unsigned int n, 
					 RxnReactionRatesClass *rateclass, 
					 String property,
					 String delimitor,
					 bool chemkinreverse)
{
  bool result = true;
  double arr = rateclass->ConvertArrhenius(true,property,n,this);
  double tn  = getTemperatureCoefficientValue(rateclass);
  double e   = rateclass->ConvertActivationEnergy(true,property,this);
  out.setf(ios::scientific, ios::floatfield);
  out.precision(8);
  out.setf(ios::showpoint);

  if(chemkinreverse) 
    out << "\n                   REV /   ";
  out << arr << delimitor << tn << delimitor << e << delimitor;
  if(chemkinreverse) 
    out << " /";

  return result;
}

/*S RxnReactionRatesClass
 */
/*F RxnReactionRatesClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionRatesClass::RxnReactionRatesClass()
  : ArrheniusName("Arrhenius"),
    ActivationEnergyName("ActivationEnergy"),
    TemperatureCoefficientName("TemperatureCoefficient"),
    RateType(NULL),
    GasConstant(1.9872)
{
  Identification = REACTION_RATES_ID;
  NameTag = REACTION_RATES_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnReactionRatesClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnReactionRatesClass::RxnReactionRatesClass(const RxnReactionRatesClass& data)
  : RxnRealBasedPropertyClass(data),
    ArrheniusName(data.ArrheniusName),
    ActivationEnergyName(data.ActivationEnergyName),
    TemperatureCoefficientName(data.TemperatureCoefficientName),
    GasConstant(data.GasConstant),
    ArrheniusFilter(data.ArrheniusFilter),
    ActivationEnergyFilter(data.ActivationEnergyFilter)
{
  RateType = (DataNumericClass *) PointerClone(data.RateType);
} 
/*F RxnReactionRatesClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnReactionRatesClass::RxnReactionRatesClass(const int id, 
					     const String& name,
					     const String& descr)
  : RxnRealBasedPropertyClass(id,name,descr),
    ArrheniusName("Arrhenius"),
    ActivationEnergyName("ActivationEnergy"),
    TemperatureCoefficientName("TemperatureCoefficient"),
    RateType(NULL),
    GasConstant(1.9872)
{
  SubClass = "Object";
  EncodeDecodeClass = REACTION_RATES_NAME;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
/*F RxnReactionRatesClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionRatesClass::~RxnReactionRatesClass()
{
  if(RateType != NULL)
    delete RateType;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnReactionRatesClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnReactionRatesClass::print(ostream& out) const
{
  RxnRealBasedPropertyClass::print(out);
  out << "Names: '" << ArrheniusName << "' '";
  out << TemperatureCoefficientName << "' '";
  out << ActivationEnergyName << "'" << endl;
  out << "Gas Constant: " << GasConstant << endl;

  PointerPrint(out,"  The Rate Constant Class: "," No Class Defined ",RateType);
  out << endl;
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnReactionRatesClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnReactionRatesClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnReactionRatesClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnRealBasedPropertyClass::Read(in,set);

  result = result && PointerClassRead(in,(DataObjectClass *&) RateType,
				      NUMERIC_BASE_NAME,
				      set," No Class ");
  StreamObjectInput str(in,' ');
  ArrheniusName              = str.ReadNext();
  TemperatureCoefficientName = str.ReadNext();
  ActivationEnergyName       = str.ReadNext();
  String GasConstantS        = str.ReadNext();
  GasConstant = GasConstantS.ToFloat();

  String ename("ArrheniusFilter");
  String aname("ActivationEnergyFilter");
  String gname("GasConstantFilter");
  DataKeyWordsClass keyclass;

  GasConstantFilter.Read(in,&keyclass,gname);
  ArrheniusFilter.Read(in,&keyclass,ename);
  ActivationEnergyFilter.Read(in,&keyclass,aname);

  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnReactionRatesClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnReactionRatesClass::CopyClone(Identify *  objc)
{
  RxnReactionRatesClass *objcfull = (RxnReactionRatesClass *) objc;
  *this = *objcfull;
  RateType = (DataNumericClass *) PointerClone(objcfull->RateType);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnReactionRatesClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnReactionRatesClass::Clone()
{
  RxnReactionRatesClass* id = new RxnReactionRatesClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionRatesClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionRatesClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnRealBasedPropertyClass::EncodeThis(buffer);

  result = result && Encode(buffer,ArrheniusName);
  result = result && Encode(buffer,ActivationEnergyName);
  result = result && Encode(buffer,TemperatureCoefficientName);
  result = result && Encode(buffer,GasConstant);
  result = result && PointerEncode(buffer,RateType);
  result = result && Encode(buffer,ArrheniusFilter);
  result = result && Encode(buffer,ActivationEnergyFilter);
  result = result && Encode(buffer,GasConstantFilter);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionRatesClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionRatesClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnRealBasedPropertyClass::DecodeThis(buffer);

  result = result && Decode(buffer,ArrheniusName);
  result = result && Decode(buffer,ActivationEnergyName);
  result = result && Decode(buffer,TemperatureCoefficientName);
  result = result && Decode(buffer,GasConstant);
  result = result && PointerDecode(buffer,(BaseDataObject *&) RateType);
  result = result && Decode(buffer,ArrheniusFilter);
  result = result && Decode(buffer,ActivationEnergyFilter);
  result = result && Decode(buffer,GasConstantFilter);
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
BaseDataObject * RxnReactionRatesClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataReactionRates();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnReactionRatesClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnReactionRatesClass*& obj)
     {
     obj = new RxnReactionRatesClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataReactionRates
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataReactionRates*& obj)
{
  obj = new RxnDataReactionRates;
  return obj->DecodeThis(buffer);
}
 
/*F numclass = getRateType() 
**
**  DESCRIPTION
**    numclass: The numeric class of the rate constants
**
**  REMARKS
**
*/
DataNumericClass *RxnReactionRatesClass::getRateType()
{
  return RateType;
}
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnReactionRatesClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}
 
/*F newvalue = ConvertActivationEnergy(convertto,property,rates)
**
**  DESCRIPTION
**    convertto: true if convert to 
**    property: The conversion property name
**    rates:    The rates 
**    newvalue: converted value
**    
**  REMARKS
**
*/
double RxnReactionRatesClass::ConvertArrhenius(bool convertto,String& property,
					       unsigned int n,
					       RxnDataReactionRates *rates)
{
  double value  = rates->getArrheniusValue(this);
  double convertfactor = Convert(convertto,property,ArrheniusFilter,1.0);
  while(n > 1)
    {
      n--;
      value *= convertfactor;
    }
  return value;
} 
/*F newvalue = ConvertActivationEnergy(convertto,property,rates)
**
**  DESCRIPTION
**    convertto: true if convert to 
**    property: The conversion property name
**    rates:    The rates 
**    newvalue: converted value
**
**  REMARKS
**
*/
double RxnReactionRatesClass::ConvertActivationEnergy(bool convertto,String& property,
						      RxnDataReactionRates *rates)
{
  double value  = rates->getActivationEnergyValue(this);
  double conversion = Convert(convertto,property,ActivationEnergyFilter,1.0);
  return value*conversion;
}
/*F newvalue = ConvertGasConstant(convertto,property,rates)
**
**  DESCRIPTION
**    convertto: true if convert to 
**    property: The conversion property name
**    rates:    The rates 
**    newvalue: converted value
**
**  REMARKS
**
*/
double RxnReactionRatesClass::ConvertGasConstant(bool convertto,String& property,
						 RxnDataReactionRates *rates)
{
  double value  = getGasConstant();
  return Convert(convertto,property,GasConstantFilter,value);
}
 
/*F constant = getGasConstant() . . . . . . . . . . . . RxnReactionRatesClass
**
**  DESCRIPTION
**    constant: The gas constant
**
**  REMARKS
**
*/
double RxnReactionRatesClass::getGasConstant()
{
  return GasConstant;
}

/*S RxnDataReactionRatesRxnFile
 */ 
/*F RxnDataReactionRatesRxnFile()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionRatesRxnFile::RxnDataReactionRatesRxnFile()
{
  Identification = REACTION_RXNFILE_ID;
  NameTag = REACTION_RXNFILE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataReactionRatesRxnFile(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataReactionRatesRxnFile::RxnDataReactionRatesRxnFile(const RxnDataReactionRatesRxnFile& data)
  : RxnDataReactionRates(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataReactionRatesRxnFile::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataReactionRatesRxnFile::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = ReadAsMolFile(in,objc,name);
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataReactionRatesRxnFile::print(ostream& out) const
{
  RxnDataReactionRates::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataReactionRatesRxnFile::Clone()
{
  RxnDataReactionRatesRxnFile *obj = new RxnDataReactionRatesRxnFile(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataReactionRatesRxnFile::CopyClone(Identify * obj)
{
  RxnDataReactionRatesRxnFile *objfull = (RxnDataReactionRatesRxnFile *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionRatesRxnFile::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionRates::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionRatesRxnFile::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionRates::DecodeThis(buffer);
  return result;
}
 
 
/*S RxnReactionRatesRxnFileClass
 */
/*F RxnReactionRatesRxnFileClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionRatesRxnFileClass::RxnReactionRatesRxnFileClass()
{
  Identification = REACTION_RXNFILE_ID;
  NameTag = REACTION_RXNFILE_NAME;
  SubClass = "ReactionRates";
  EncodeDecodeClass = NameTag;
} 
/*F RxnReactionRatesRxnFileClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnReactionRatesRxnFileClass::RxnReactionRatesRxnFileClass(const RxnReactionRatesRxnFileClass& data)
  : RxnReactionRatesClass(data)
{
} 
 
/*F RxnReactionRatesRxnFileClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnReactionRatesRxnFileClass::RxnReactionRatesRxnFileClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnReactionRatesClass(id,name,descr)
{
  SubClass = "ReactionRates";
  EncodeDecodeClass = REACTION_RXNFILE_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnReactionRatesRxnFileClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnReactionRatesRxnFileClass::print(ostream& out) const
{
  RxnReactionRatesClass::print(out);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnReactionRatesRxnFileClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnReactionRatesRxnFileClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnReactionRatesRxnFileClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnReactionRatesClass::Read(in,set);
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnReactionRatesRxnFileClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnReactionRatesRxnFileClass::CopyClone(Identify *  objc)
{
  RxnReactionRatesRxnFileClass *objcfull = (RxnReactionRatesRxnFileClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnReactionRatesRxnFileClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnReactionRatesRxnFileClass::Clone()
    {
      RxnReactionRatesRxnFileClass* id = new RxnReactionRatesRxnFileClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionRatesRxnFileClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionRatesRxnFileClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionRatesClass::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionRatesRxnFileClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionRatesRxnFileClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnReactionRatesRxnFileClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataReactionRatesRxnFile();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnReactionRatesRxnFileClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnReactionRatesRxnFileClass*& obj)
     {
     obj = new RxnReactionRatesRxnFileClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataReactionRatesRxnFile
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataReactionRatesRxnFile*& obj)
     {
     obj = new RxnDataReactionRatesRxnFile;
     return obj->DecodeThis(buffer);
     }

/*S RxnDataThirdBody
 */ 
/*F RxnDataThirdBody()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataThirdBody::RxnDataThirdBody()
{
  Identification = REACTION_THIRDBODY_ID;
  NameTag = REACTION_THIRDBODY_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataThirdBody(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataThirdBody::RxnDataThirdBody(const RxnDataThirdBody& data)
  : RxnDataReactionRates(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataThirdBody
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataThirdBody::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataThirdBody
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataThirdBody::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = ReadAsMolFile(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataThirdBody
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataThirdBody::print(ostream& out) const
{
  RxnDataReactionRates::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataThirdBody
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataThirdBody::Clone()
{
  RxnDataThirdBody *obj = new RxnDataThirdBody(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataThirdBody
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataThirdBody::CopyClone(Identify * obj)
{
  RxnDataThirdBody *objfull = (RxnDataThirdBody *) obj;
  *this = *objfull;
  //Parameter = (RxnData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataThirdBody
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataThirdBody::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionRates::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  //result = result && Encode(buffer,---);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataThirdBody
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataThirdBody::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionRates::DecodeThis(buffer);
  // The rest

  return result;
}
/*F ans = WriteOutRates(out,n,rateclass,property,delimitor)
**
**  DESCRIPTION
**    out: The output stream
**    n: The number of reactants
**    rateclass: The rate class
**    property: The property units to convert to
**    delimitor: The delimitor between the constants (and at end)
**
**  REMARKS
**
*/
bool RxnDataThirdBody::WriteOutRates(ostream& out,
				     unsigned int n, 
				     RxnReactionRatesClass *rateclass, 
				     String property,
				     String delimitor,
				     bool chemkinreverse) {
  bool result = true;

  double arr = rateclass->ConvertArrhenius(true,property,n,this);
  double tn  = getTemperatureCoefficientValue(rateclass);
  double e   = rateclass->ConvertActivationEnergy(true,property,this);
  if(chemkinreverse) 
    out << "\n                   REV /   ";
  out.setf(ios::scientific, ios::floatfield);
  out.precision(8);
  out.setf(ios::showpoint);
  out << arr << delimitor << tn << delimitor << e << delimitor;
  if(chemkinreverse) 
    out << " / ";
  out << endl;
  out.setf(ios::floatfield);
  out.precision(3);
  out.setf(ios::showpoint);
  if(IsInList(REACTION_THIRDBODY_NAME)) {
    BaseDataKeyWords *species = (BaseDataKeyWords *) GetObject(REACTION_THIRDBODY_NAME);
    ObjectListString names = species->GetKeyWords();
    ObjectListString::iterator name = names.begin();
    for(;name != names.end();name++) {
      BaseDataReal *f = (BaseDataReal *) GetObject(*name);
      out << *name << " /" << f->GetValue() << "/ ";
    }
  }
  return result;
}

 
 
/*S RxnThirdBodyClass
 */
/*F RxnThirdBodyClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnThirdBodyClass::RxnThirdBodyClass()
{
  Identification = REACTION_THIRDBODY_ID;
  NameTag = REACTION_THIRDBODY_NAME;
  SubClass = "ReactionRates";
  EncodeDecodeClass = NameTag;
} 
/*F RxnThirdBodyClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnThirdBodyClass::RxnThirdBodyClass(const RxnThirdBodyClass& data)
  : RxnReactionRatesClass(data)
{
} 
 
/*F RxnThirdBodyClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnThirdBodyClass::RxnThirdBodyClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnReactionRatesClass(id,name,descr)
{
  SubClass = "ReactionRates";
  EncodeDecodeClass = REACTION_THIRDBODY_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnThirdBodyClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnThirdBodyClass::print(ostream& out) const
{
  RxnReactionRatesClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnThirdBodyClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnThirdBodyClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnThirdBodyClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnReactionRatesClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnThirdBodyClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnThirdBodyClass::CopyClone(Identify *  objc)
{
  RxnThirdBodyClass *objcfull = (RxnThirdBodyClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnThirdBodyClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnThirdBodyClass::Clone()
    {
      RxnThirdBodyClass* id = new RxnThirdBodyClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnThirdBodyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnThirdBodyClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionRatesClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnThirdBodyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnThirdBodyClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionRatesClass::DecodeThis(buffer);
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
BaseDataObject * RxnThirdBodyClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataThirdBody();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnThirdBodyClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnThirdBodyClass*& obj)
     {
     obj = new RxnThirdBodyClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataThirdBody
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataThirdBody*& obj)
     {
     obj = new RxnDataThirdBody;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataReactionRateHiLow
 */ 
/*F RxnDataReactionRateHiLow()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionRateHiLow::RxnDataReactionRateHiLow()
  : HiRate(NULL),
    Parameters(NULL)
{
  Identification = REACTION_HILOW_ID;
  NameTag = REACTION_HILOW_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataReactionRateHiLow(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataReactionRateHiLow::RxnDataReactionRateHiLow(const RxnDataReactionRateHiLow& data)
  : RxnDataReactionRates(data)
{
  HiRate = (RxnDataReactionRates *) PointerClone(data.HiRate);
  Parameters = (BaseDataDoubleVector *) PointerClone(data.Parameters);
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataReactionRateHiLow::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataReactionRateHiLow::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = RxnDataReactionRates::Read(in,objc,name);
  result = result && ReadConstants(in,objc);
  RxnReactionRateHiLowClass *rxnclass = (RxnReactionRateHiLowClass *) objc;

  VectorNumeric *vec = new VectorNumeric(rxnclass->getParameterSize());

  StreamObjectInput str(in,' ');
  for(unsigned int i=0; i < rxnclass->getParameterSize(); i++)
    {
      String numS = str.ReadNext();
      (*vec)[i] = numS.ToFloat();
    }
  Parameters = new BaseDataDoubleVector(*vec);
  delete vec;
  return result;
} 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataReactionRateHiLow::print(ostream& out) const
{
  out << "k zero Rate Constants" << endl;
  RxnDataReactionRates::print(out);  
  out << endl;
  PointerPrint(out,"K infinity Rate Constants ","No Rate Specified",HiRate);
  out << endl;
  PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  out << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataReactionRateHiLow::Clone()
{
  RxnDataReactionRateHiLow *obj = new RxnDataReactionRateHiLow(*this);
  return obj;
}
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataReactionRateHiLow::CopyClone(Identify * obj)
{
  RxnDataReactionRateHiLow *objfull = (RxnDataReactionRateHiLow *) obj;
  *this = *objfull;
  HiRate = (RxnDataReactionRates *) PointerClone(objfull->HiRate);
  Parameters = (BaseDataDoubleVector *) PointerClone(objfull->Parameters);
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionRateHiLow::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionRates::EncodeThis(buffer);
  result = result && PointerEncode(buffer,HiRate);
  result = result && PointerEncode(buffer,Parameters);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionRateHiLow::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReactionRates::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) HiRate);
  result = result && PointerDecode(buffer,(BaseDataObject *&) Parameters);
  return result;
}
/*F rxn = getHiRate() . . . . . . . . . . . . . . .  RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    rxn: The rate of reaction of the high pressure region
**
**  REMARKS
**
*/
RxnDataReactionRates * RxnDataReactionRateHiLow::getHiRate()
{
  return HiRate;
}
/*F rxn = getLowRate() . . . . . . . . . . . . . . . .  RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    rxn: The rate of reaction of the low pressure region
**
**  REMARKS
**
*/
RxnDataReactionRates * RxnDataReactionRateHiLow::getLowRate()
{
  return this;
}
/*F vec = getParameters() . . . . . . . . . . . . .  RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    vec: The vector of parameters
**
**  REMARKS
**
*/
BaseDataDoubleVector * RxnDataReactionRateHiLow::getParameters()
{
  return Parameters;
}
/*F ans = WriteOutRates(out,n,rateclass,property,delimitor) RxnDataReactionRates::
**
**  DESCRIPTION
**    out: The output stream
**    n: The number of reactants
**    rateclass: The rate class
**    property: The property units to convert to
**    delimitor: The delimitor between the constants (and at end)
**
**  REMARKS
**
*/
bool RxnDataReactionRateHiLow::WriteOutRates(ostream& out,
					     unsigned int n, 
					     RxnReactionRatesClass *rateclass, 
					     String property,
					     String delimitor,
					     bool chemkinreverse)
{
  bool result = true;
  /*
  double arr = rateclass->ConvertArrhenius(true,property,n,this);
  double tn  = getTemperatureCoefficientValue(rateclass);
  double e   = rateclass->ConvertActivationEnergy(true,property,this);

  out.setf(ios::scientific, ios::floatfield);
  out.precision(8);
  out.setf(ios::showpoint);

  out << delimitor << arr << "  " << tn << "  "  << e << delimitor;
  */
  RxnDataReactionRates *hi = getHiRate();
  RxnDataReactionRates *low = getLowRate();
  String delim("/");
  result = result && hi->RxnDataReactionRates::WriteOutRates(out,n,rateclass,property,delimitor,false);
  out << endl << "     LOW /";
  result = result && low->RxnDataReactionRates::WriteOutRates(out,n,rateclass,property,delimitor,false);
  out << "/" << endl;

  // Output Troe parameters
  BaseDataDoubleVector *vecd = getParameters();
  VectorNumeric vec =  vecd->CurrentVector();
  if(vec.size() > 0) {
    out << "     TROE /";
    for(unsigned int i=0;i<vec.size();i++) {
      out << " " << vec[i];
    }
    out << "/" << endl;
  }
  out.setf(ios::floatfield);
  out.precision(3);
  out.setf(ios::showpoint);
  if(IsInList(REACTION_THIRDBODY_NAME)) {
    BaseDataKeyWords *species = (BaseDataKeyWords *) GetObject(REACTION_THIRDBODY_NAME);
    ObjectListString names = species->GetKeyWords();
    ObjectListString::iterator name = names.begin();
    for(;name != names.end();name++) {
      BaseDataReal *f = (BaseDataReal *) GetObject(*name);
      out << *name << " /" << f->GetValue() << "/ ";
    }
  }
  return result;
}
/*S RxnReactionRateHiLowClass
 */
/*F RxnReactionRateHiLowClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionRateHiLowClass::RxnReactionRateHiLowClass()
{
  Identification = REACTION_HILOW_ID;
  NameTag = REACTION_HILOW_NAME;
  SubClass = "ReactionRates";
  EncodeDecodeClass = NameTag;
} 
/*F RxnReactionRateHiLowClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnReactionRateHiLowClass::RxnReactionRateHiLowClass(const RxnReactionRateHiLowClass& data)
  : RxnReactionRatesClass(data),
    ParameterSize(data.ParameterSize)
{
}
/*F RxnReactionRateHiLowClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnReactionRateHiLowClass::RxnReactionRateHiLowClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnReactionRatesClass(id,name,descr)
{
  SubClass = "ReactionRates";
  EncodeDecodeClass = "ReactionRateHiLow";
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnReactionRateHiLowClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnReactionRateHiLowClass::print(ostream& out) const
{
  RxnReactionRatesClass::print(out);
  out << "Number of Parameters: " << ParameterSize << endl;
  return out;
}
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnReactionRateHiLowClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnReactionRateHiLowClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnReactionRateHiLowClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnReactionRatesClass::Read(in,set);
  StreamObjectInput str(in,' ');
  String parS = str.ReadNext();
  ParameterSize = parS.ToInteger();
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnReactionRateHiLowClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnReactionRateHiLowClass::CopyClone(Identify *  objc)
{
  RxnReactionRateHiLowClass *objcfull = (RxnReactionRateHiLowClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnReactionRateHiLowClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnReactionRateHiLowClass::Clone()
{
  RxnReactionRateHiLowClass* id = new RxnReactionRateHiLowClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionRateHiLowClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionRateHiLowClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionRatesClass::EncodeThis(buffer);
  result = result && Encode(buffer,ParameterSize);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionRateHiLowClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionRateHiLowClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionRatesClass::DecodeThis(buffer);
  result = result && Decode(buffer,ParameterSize);
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
BaseDataObject * RxnReactionRateHiLowClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataReactionRateHiLow();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnReactionRateHiLowClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnReactionRateHiLowClass*& obj)
     {
     obj = new RxnReactionRateHiLowClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataReactionRateHiLow
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataReactionRateHiLow*& obj)
     {
     obj = new RxnDataReactionRateHiLow;
     return obj->DecodeThis(buffer);
     }
 
/*F parsize = getParameterSize()  . . . . . . . . . RxnReactionRateHiLowClass
**
**  DESCRIPTION
**    parsize: The number of parameters of the parameter vector
**
**  REMARKS
**
*/
unsigned int RxnReactionRateHiLowClass::getParameterSize()
{
  return ParameterSize;
}
/*S RxnDataReaction
 */ 
/*F RxnDataReaction()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReaction::RxnDataReaction()
  : Reactants(NULL),
    Products(NULL)
{
  Identification = REACTION_REACTION_ID;
  NameTag = REACTION_REACTION_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataReaction(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataReaction::RxnDataReaction(const RxnDataReaction& data)
  : BaseDataSetOfObjects(data)
{
  Reactants = (RxnDataMoleculeSet *) PointerClone(data.Reactants);
  Products = (RxnDataMoleculeSet *) PointerClone(data.Products);
}
/*F RxnDataReaction()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReaction::~RxnDataReaction()
{
  if(Reactants != NULL)
    delete Reactants;
  if(Products != NULL)
    delete Products;
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataReaction
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataReaction::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataReaction
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataReaction::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataSetOfObjects::Read(in,objc,name);
  RxnReactionClass *objclass = (RxnReactionClass *) objc;
  String notdefined("Not Defined");
  result = result && PointerObjectRead(in, (BaseDataObject *&) Reactants,
				       objclass->getMoleculeSetClass(),notdefined);
  result = result && PointerObjectRead(in, (BaseDataObject *&) Products,
				       objclass->getMoleculeSetClass(),notdefined);
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataReaction
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataReaction::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  PointerPrint(out,"The Reactants: ","None",Reactants);
  PointerPrint(out,"The Products: ","None",Products);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataReaction
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataReaction::Clone()
{
  RxnDataReaction *obj = new RxnDataReaction(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataReaction
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataReaction::CopyClone(Identify * obj)
{
  RxnDataReaction *objfull = (RxnDataReaction *) obj;
  *this = *objfull;
  Reactants = (RxnDataMoleculeSet *) PointerClone(objfull->Reactants);
  Products  = (RxnDataMoleculeSet *) PointerClone(objfull->Products);
} 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReaction
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReaction::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  result = result && PointerEncode(buffer,Reactants);
  result = result && PointerEncode(buffer,Products);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReaction
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReaction::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) Reactants);
  result = result && PointerDecode(buffer,(BaseDataObject *&) Products);
  return result;
}
/*F reactants = getReactantNames()  . . . . . . . . . . . . . . . RxnDataReaction
**
**  DESCRIPTION
**    reactants: The list of reactant names
**
**  REMARKS
**
*/
BaseDataKeySet& RxnDataReaction::getReactantNames()
{
  return Reactants->getMoleculeNames();
}
 
/*F products = getProductNames()  . . . . . . . . . . . . . . RxnDataReaction
**
**  DESCRIPTION
**    products: The list of products names
**
**  REMARKS
**
*/
BaseDataKeySet& RxnDataReaction::getProductNames()
{
  return Products->getMoleculeNames();
}
/*F reactants = getInternalReactantNames() . . . . . . . . . . RxnDataReaction
**
**  DESCRIPTION
**    reactants: The list of reactant names
**
**  REMARKS
**
*/
BaseDataKeySet& RxnDataReaction::getInternalReactantNames()
{
  return Reactants->getInternalMoleculeNames();
}
 
/*F products = getInternalProductNames()  . . . . . . . . . . . . . . RxnDataReaction
**
**  DESCRIPTION
**    products: The list of products names
**
**  REMARKS
**
*/
BaseDataKeySet& RxnDataReaction::getInternalProductNames()
{
  return Products->getInternalMoleculeNames();
}
/*F ans = setupReactantsMolecules(set,dbasetype,dbase,dbaseclass)
**
**  DESCRIPTION
**    set: The set to add molecules to
**    dbasetype: The name of the database
**    dbase: The data base source
**    dbaseclass: The source data base class
**
**  REMARKS
**
*/
bool RxnDataReaction::setupReactantMolecules(BaseDataSetOfObjects *set,
					     String& dbasetype,
					     RxnDataMolecularStructuresDataBase *dbase,
					     RxnMolecularStructuresDataBaseClass *dbaseclass)
{
  return Reactants->getMolecules(set,dbasetype,dbase,dbaseclass);
}
/*F ans = getDBaseProductMolecules(set,dbasetype,dbase,dbaseclass)
**
**  DESCRIPTION
**    set: The set to add molecules to
**    dbasetype: The name of the database
**    dbase: The data base source
**    dbaseclass: The source data base class
**
**  REMARKS
**
*/
bool RxnDataReaction::setupProductMolecules(BaseDataSetOfObjects *set,
					    String& dbasetype,
					    RxnDataMolecularStructuresDataBase *dbase,
					    RxnMolecularStructuresDataBaseClass *dbaseclass)
{
  return Products->getMolecules(set,dbasetype,dbase,dbaseclass);
}
/*F ans = setupReactionMolecules(set,dbasetype,dbase,dbaseclass)
**
**  DESCRIPTION
**    set: The set to add molecules to
**    dbasetype: The name of the database
**    dbase: The data base source
**    dbaseclass: The source data base class
**
**  REMARKS
**
*/
bool RxnDataReaction::setupReactionMolecules(BaseDataSetOfObjects *set,
					     String& dbasetype,
					     RxnDataMolecularStructuresDataBase *dbase,
					     RxnMolecularStructuresDataBaseClass *dbaseclass)
{
  bool result = setupProductMolecules(set,dbasetype,dbase,dbaseclass);
  result = result && setupReactantMolecules(set,dbasetype,dbase,dbaseclass);
  return result;
}
 
/*F realname = getReactantName(internalname)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
String RxnDataReaction::getReactantName(String& internalname)
{
  return Reactants->MoleculeNameFromInternalName(internalname);
}
/*F realname = getProductName(internalname)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
String RxnDataReaction::getProductName(String& internalname)
{
  return Products->MoleculeNameFromInternalName(internalname);
}
 
/*F ans = ReadRxnFileReaction(in,rxnclass)
**
**  DESCRIPTION
**    in: The input stream
**    rxnclass: The reaction class
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReaction::ReadRxnFileReaction(istream& in, String& dbasetype,
					  RxnReactionClass *rxnclass,
					  RxnDataMolecularStructuresDataBase *dbase,
					  RxnMolecularStructuresDataBaseClass *dbaseclass)
{
  bool result = true;
  RxnMoleculeSetClass *molsetclass = (RxnMoleculeSetClass *) rxnclass->getMoleculeSetClass();

  BaseDataDataBaseInformation *db = dbase->getDatabaseInfo(dbasetype);
  DataDataBaseInformationClass *dbclass = dbaseclass->getMoleculeDBClass();
  BaseDataObject *mol = db->getDataElementClass(dbclass);

  if(molsetclass != NULL)
    {
      Reactants = ( RxnDataMoleculeSet *) molsetclass->BaseDataObjectExample();
      Products = ( RxnDataMoleculeSet *) molsetclass->BaseDataObjectExample();
      String line;
      line.ReadFullLine(in);
      String blank(" ");
      line.AppendToEnd(blank);
      line.EliminateLeadingBlanks();
      if(!line.IsEmpty())
	{
	  String reactants,molname;
	  
	  line.IsolateNextWord(NameTag,' ');
	  line.IsolateNextWord(reactants,'=');
	  reactants.EliminateLeadingBlanks();
	  line.EliminateLeadingBlanks();
	  while(!reactants.IsEmpty() && result)
	    {
	      reactants.IsolateNextWord(molname,' ');
	      reactants.EliminateLeadingBlanks();
	      molname.EliminateBlanks();
	      String internalname = Reactants->AddMolecule(molname);
	      result = result && db->FetchElement(molname,dbclass,mol);
	      BaseDataString m;
	      m.NameTag = internalname;
	      m.setString(molname);
	      Reactants->AddObject(&m);
	    }
	  
	  while(!line.IsEmpty() && result)
	    {
	      line.IsolateNextWord(molname,' ');
	      molname.EliminateBlanks();
	      line.EliminateLeadingBlanks();
	      String internalname = Products->AddMolecule(molname);
	      result = result && db->FetchElement(molname,dbclass,mol);
	      BaseDataString m;
	      m.NameTag = internalname;
	      m.setString(molname);
	      Products->AddObject(&m);
	    }
	  if(!result)
	    {
	      cerr << "Molecule: '" << molname << "' not found in database" << endl;
	    }
	}
      else
	result = false;
    }
  else
    {
      cerr << "Molecule Set Class not defined" << endl;
      result = false;
    }
    return result;
}
/*F ans = FillReactionWithMolecules(dbasetype,dbase,dbaseclass) RxnDataReaction
**
**  DESCRIPTION
**    dbasetype: The database type
**    dbase:  The database
**    dbaseclass: The database class
**
**  REMARKS
**
*/
bool RxnDataReaction::FillReactionWithMolecules(String& dbasetype,
						RxnDataMolecularStructuresDataBase *dbase,
						RxnMolecularStructuresDataBaseClass *dbaseclass)
{
  bool result = true;
  result = result && setupReactantMolecules(getReactants(),
					    dbasetype,dbase,dbaseclass);
  result = result && setupProductMolecules(getProducts(),
					   dbasetype,dbase,dbaseclass);
  return result;
}
 
/*F set = getReactants()  . . . . . . . . . . . . . . . . . . RxnDataReaction
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeSet *RxnDataReaction::getReactants()
{
  return Reactants;
} 
/*F set = getProducts() . . . . . . . . . . . . . . . . . . . RxnDataReaction
**
**  DESCRIPTION
**    set: The set of product molecules
**
**  REMARKS
**
*/
RxnDataMoleculeSet *RxnDataReaction::getProducts()
{
  return Products;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionRates *RxnDataReaction::GetRateConstants(String& ratename, 
							bool forward,
							String& property, RxnReactionRatesClass *rxnclass)
{
  RxnDataReactionRates *rate;
  RxnDataReactionRates *newrate = NULL;
  
  if(IsInList(ratename))
    {
      rate = (RxnDataReactionRates *) GetObject(ratename);
      
      unsigned int n = 0;
      if(forward)
	{
	  ObjectList<String> names = getInternalReactantNames().GetKeyWords();
	  n = names.size();
	}
      else
	{
	  ObjectList<String> names = getInternalProductNames().GetKeyWords();
	  n = names.size();
	}
      newrate = rxnclass->ConvertRateConstants(rate,n,property);
      
    }
  else
    {
      cerr << "Rate '" << ratename << "' not found in reaction '" << NameTag << "'" << endl;
    }
  return newrate;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
unsigned int RxnDataReaction::getNumberOfReactants()
{
  unsigned int count = 0;
  if(Reactants != NULL)
    count = Reactants->getInternalMoleculeNames().SizeOf();
  return count;
}
/*F cnt = getNumberOfProducts() 
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
unsigned int RxnDataReaction::getNumberOfProducts()
{
  unsigned int count = 0;
  if(Products != NULL)
    count = Products->getInternalMoleculeNames().SizeOf();
  return count;
}

/*F newrate = GetRateConstants(rate,n,property,rxnclass)  . . RxnDataReaction
**
**  DESCRIPTION
**    rate: The rate to convert
**    n: The number of products or reactants (for Arrhenius conversion) 
**    property: The conversion property
**    newrate: The new converted rate
**
**  REMARKS
**
*/
RxnDataReactionRates *RxnReactionRatesClass::ConvertRateConstants(RxnDataReactionRates *rate,
								  unsigned int n,
								  String& property)
{
  double arr = ConvertArrhenius(true,property,n,rate);
  double energy = ConvertActivationEnergy(true,property,rate);
  double tempn = rate->getTemperatureCoefficientValue(this);
  
  RxnDataReactionRates *newrate = (RxnDataReactionRates *) BaseDataObjectExample();
  DataRealClass *typeclass = (DataRealClass *) getRateType();
  BaseDataReal *Arr = (BaseDataReal *) typeclass->BaseDataObjectExample();
  BaseDataReal *Energy = (BaseDataReal *) typeclass->BaseDataObjectExample();
  BaseDataReal *TempN = (BaseDataReal *) typeclass->BaseDataObjectExample();
  Arr->SetValue(arr);
  Energy->SetValue(energy);
  TempN->SetValue(tempn);
  
  newrate->setArrhenius(Arr,this);
  newrate->setActivationEnergy(Energy,this);
  newrate->setTemperatureCoefficient(TempN,this);
  
  delete Arr;
  delete Energy;
  delete TempN;

  return newrate;
}
 
/*F ans = WriteReactionEquation(out,ThirdBody,Eq,ThirdBodyS)
**
**  DESCRIPTION
**    out: The output stream
**    ThirdBody: true if a third body reaction
**    Eq: The equation sign ('=' reversible, '>' forward ...)
**    ThirdBodyS: The symbol for a third body
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataReaction::WriteReactionEquation(ostream& out, bool forward, bool chemkin, bool ThirdBody, String& Eq, 
					    String& ThirdBodyS, String& shortname)
{
  bool result = true;
  if(forward)
    result = result && WriteMoleculeSet(out,chemkin,true,getInternalReactantNames(),ThirdBody,ThirdBodyS,shortname);
  else
    result = result && WriteMoleculeSet(out,chemkin,false,getInternalProductNames(),ThirdBody,ThirdBodyS,shortname);
  out << " " << Eq << " ";
  if(!forward)
    result = result && WriteMoleculeSet(out,chemkin,true,getInternalReactantNames(),ThirdBody,ThirdBodyS,shortname);
  else
    result = result && WriteMoleculeSet(out,chemkin,false,getInternalProductNames(),ThirdBody,ThirdBodyS,shortname);
  
  return result;
}
 
/*F ans = WriteMoleculeSet(out,reactant,internal,ThirdBody,ThirdBodyS)    RxnDataReaction
**
**  DESCRIPTION
**    out: The output stream
**    reactant: true if set of reactants
**    internal: The internal molecule names
**    ThirdBody: true if a third body reaction
**    ThirdBodyS: The symbol for a third body
**    ans: true if successful
**    
**  REMARKS
**
*/
bool RxnDataReaction::WriteMoleculeSet(ostream& out, bool chemkin, bool reactant, 
				       BaseDataKeySet& internal, 
				       bool ThirdBody, String& ThirdBodyS, String& shortname)
{
  BaseDataKeySet *molecules = (BaseDataKeySet *) internal.Clone();
  bool notlast = true;
  while(notlast)
    {
      String name = molecules->NextKey();
	out << " " << CreateStandardName(chemkin,reactant,name,shortname) << " ";
      notlast = molecules->SizeOf() > 0;
      if(notlast)
	out << "+";
    }
  if(ThirdBody) {
    if(ThirdBodyS == "(M)")
      out << "(+M) ";
    else
      out << ThirdBodyS << " ";
  }
  return true;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
String RxnDataReaction::CreateStandardName(bool chemkin, bool reactant, String& name,String& shortname)
{
  String final;
  RxnDataMoleculeSet *mols;
  if(reactant)
    mols = getReactants();
  else 
    mols = getProducts();

  String molname;
  if(reactant)
    molname = getReactantName(name);
  else
    molname = getProductName(name);
  RxnDataSimpleMolecule *molecule = NULL;
  if(mols != NULL) {
      molecule = (RxnDataSimpleMolecule *) mols->GetObject(molname);
  } else {
    cerr << "Reactants or Products are NULL" << endl;
  }
  bool bfinal = false;
  if(chemkin && molecule != NULL) {
      BaseDataString *str = (BaseDataString *) molecule->RetrieveProperty(shortname);
      if(str != NULL) {
	  final = str->getString();
	  bfinal = true;
      } else {
	cerr << shortname << " not found in molecule: " << molname << endl;
      }
  } else {
    cerr << "chemkin not true or molecule is NULL" << endl;
  }
  if(!bfinal)
    {
      final = "{";
      final.AppendToEnd(molname);
      String endparen("}");
      final.AppendToEnd(endparen);
    }
  return final;
}
 
/*F setReactants(mols)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataReaction::setReactants(RxnDataMoleculeSet *mols)
{
  Reactants = mols;
}
 
/*F within = withinMoleculeSet(moleculelist)
**
**  DESCRIPTION
**    moleculelist: The list of candidate molecules
**    within: true if the reaction only contains molecules from the list
**
**  REMARKS
**
*/
bool RxnDataReaction::withinMoleculeSet(BaseDataKeyWords &moleculelist)
{
  bool ans1 = Reactants->ContainedIn(moleculelist);
  bool ans2 = Products->ContainedIn(moleculelist);
    return ans1 && ans2;
}
/*F setProducts(mols)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnDataReaction::setProducts(RxnDataMoleculeSet *mols)
{
  Products = mols;
}


/*S RxnReactionClass
 */
/*F RxnReactionClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionClass::RxnReactionClass()
  : MoleculeSetClass(NULL),
    RatesClass(NULL),
    NameInInstance("Reaction"),
    StandardForwardName("Forward"),
    StandardReverseName("Reverse")
{
  Identification = REACTION_REACTION_ID;
  NameTag = REACTION_REACTION_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnReactionClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnReactionClass::RxnReactionClass(const RxnReactionClass& data)
  : DataSetOfObjectsClass(data),
    NameInInstance(data.NameInInstance),
    StandardForwardName(data.StandardForwardName),
    StandardReverseName(data.StandardReverseName)
{
  MoleculeSetClass = (RxnMoleculeSetClass *) PointerClone(data.MoleculeSetClass);
  RatesClass = (RxnReactionRatesClass *) PointerClone(data.RatesClass);
} 
/*F RxnReactionClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionClass::~RxnReactionClass()
{
  if(MoleculeSetClass != NULL)
    delete MoleculeSetClass;
  if(RatesClass != NULL)
    delete RatesClass;
} 
/*F RxnReactionClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnReactionClass::RxnReactionClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr),
    MoleculeSetClass(NULL),
    RatesClass(NULL),
    NameInInstance("Reaction"),
    StandardForwardName("Forward"),
    StandardReverseName("Reverse")
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = REACTION_REACTION_NAME;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnReactionClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnReactionClass::print(ostream& out) const
{
  //DataSetOfObjectsClass::print(out);
  DataObjectClass::print(out);
  out << endl;
  out << "NameInInstance: '" << NameInInstance << "' ";
  out << "Forward(" << StandardForwardName << ") ";
  out << "Reverse(" << StandardReverseName << ") " << endl;
  PointerPrint(out,"  The Class: "," No Class Defined ",MoleculeSetClass);
  PointerPrint(out,"  The Class: "," No Class Defined ",RatesClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnReactionClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnReactionClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnReactionClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  //bool result = DataSetOfObjectsClass::Read(in,set);
  bool result = PointerClassRead(in,(DataObjectClass *&) MoleculeSetClass,
				      MOLECULE_MOLSET_NAME,
				      set," No Class ");
  result = result && PointerClassRead(in,(DataObjectClass *&) RatesClass,
				      REACTION_RATES_NAME,
				      set," No Class ");
  StreamObjectInput str(in,' ');
  String keyw("NameInInstance:");
  if(result && CheckReadKeyWord(in,keyw))
    {
      NameInInstance = str.ReadNext();
    }
  else
    result = false;
  
  String forw("Forward:");
  if(result && CheckReadKeyWord(in,forw))
    {
      StandardForwardName = str.ReadNext();
    }
  else
    result = false;
  
  String rev("Reverse:");
  if(result && CheckReadKeyWord(in,rev))
    {
      StandardReverseName = str.ReadNext();
    }
  else
    result = false;
  
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnReactionClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnReactionClass::CopyClone(Identify *  objc)
{
  RxnReactionClass *objcfull = (RxnReactionClass *) objc;
  *this = *objcfull;
  MoleculeSetClass = (RxnMoleculeSetClass *) PointerClone(objcfull->MoleculeSetClass);
  RatesClass = (RxnReactionRatesClass *) PointerClone(objcfull->RatesClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnReactionClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnReactionClass::Clone()
{
  RxnReactionClass* id = new RxnReactionClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,MoleculeSetClass);
  result = result && PointerEncode(buffer,RatesClass);
  result = result && Encode(buffer,NameInInstance);
  result = result && Encode(buffer,StandardForwardName);
  result = result && Encode(buffer,StandardReverseName);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) MoleculeSetClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) RatesClass);
  result = result && Decode(buffer,NameInInstance);
  result = result && Decode(buffer,StandardForwardName);
  result = result && Decode(buffer,StandardReverseName);
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
BaseDataObject * RxnReactionClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataReaction();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnReactionClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnReactionClass*& obj)
     {
     obj = new RxnReactionClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataReaction
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataReaction*& obj)
     {
     obj = new RxnDataReaction;
     return obj->DecodeThis(buffer);
     }
/*F molsetclass = getMoleculeSetClass() . . . . . . . . . .  RxnReactionClass
**
**  DESCRIPTION
**    molsetclass: The molecule set class
**
**  REMARKS
**
*/
RxnMoleculeSetClass *RxnReactionClass::getMoleculeSetClass()
{
  return MoleculeSetClass;
}
/*F ratesclass = getRatesClass() . . . . . . . . . .  RxnReactionClass
**
**  DESCRIPTION
**    ratesclass: The reaction rates class
**
**  REMARKS
**
*/
RxnReactionRatesClass *RxnReactionClass::getRatesClass()
{
  return RatesClass;
}
/*F setNameInInstance(name) . . . . . . . . . . . . .  RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    name: Set the name of the reaction in an instance to this name
**
**  REMARKS
**
*/
void RxnReactionClass::setNameInInstance(String& name)
{
  NameInInstance = name;
}
/*F setNameInInstance(name) . . . . . . . . . . . . .  RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    name: Set the name of the forward reaction name in reaction
**
**  REMARKS
**
*/
void RxnReactionClass::setStandardForwardName(String& name)
{
  StandardForwardName = name;
}
/*F setNameInInstance(name) . . . . . . . . . . . . .  RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    name: Set the name of the molecule in an instance to this name
**
**  REMARKS
**
*/
void RxnReactionClass::setStandardReverseName(String& name)
{
  StandardReverseName = name;
}
/*F name = getNameInInstance()  . . . . . . . . . . .  RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    name: Set the name of the molecule in an instance to this name
**
**  REMARKS
**
*/
String& RxnReactionClass::getNameInInstance()
{
  return NameInInstance;
}
/*F name = getStandardForwardName()  . . . . . . . . . . .  RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    name: get the name of the standard forward rate in reaction
**
**  REMARKS
**
*/
String& RxnReactionClass::getStandardForwardName()
{
  return StandardForwardName;
}
/*F name = getStandardReverseName()  . . . . . . . . . . .  RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    name: get the name of the reverse rate in reactions
**
**  REMARKS
**
*/
String& RxnReactionClass::getStandardReverseName()
{
  return StandardReverseName;
}
/*F classes = PointerToAllowedClasses()
**
**  DESCRIPTION
**    classes: The current set of allowed classes
**
**  REMARKS
**
*/
DataSetOfObjectsClass *RxnReactionClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S RxnDataReactionMoleculeCorrespondences
 */ 
/*F RxnDataReactionMoleculeCorrespondences()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionMoleculeCorrespondences::RxnDataReactionMoleculeCorrespondences()
  : ReactantsBondCorrespondenceVector(NULL),
    ProductBondCorespondenceVector(NULL)
{
  Identification = REACTION_CORRS_ID;
  NameTag = REACTION_CORRS_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataReactionMoleculeCorrespondences(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataReactionMoleculeCorrespondences::RxnDataReactionMoleculeCorrespondences(const RxnDataReactionMoleculeCorrespondences& data)
  : RxnDataReaction(data)
{
}
/*F RxnDataReactionMoleculeCorrespondences()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionMoleculeCorrespondences::~RxnDataReactionMoleculeCorrespondences()
{
  /*
  if(ReactantsBondCorrespondenceVector != NULL)
    delete ReactantsBondCorrespondenceVector;
  if(ProductBondCorespondenceVector != NULL)
    delete ProductBondCorespondenceVector;
  */
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataReactionMoleculeCorrespondences
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataReactionMoleculeCorrespondences
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = RxnDataReaction::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataReactionMoleculeCorrespondences
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataReactionMoleculeCorrespondences::print(ostream& out) const
{
  RxnDataReaction::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionMoleculeCorrespondences
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataReactionMoleculeCorrespondences::Clone()
{
  RxnDataReactionMoleculeCorrespondences *obj = new RxnDataReactionMoleculeCorrespondences(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionMoleculeCorrespondences
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataReactionMoleculeCorrespondences::CopyClone(Identify * obj)
{
  RxnDataReactionMoleculeCorrespondences *objfull = (RxnDataReactionMoleculeCorrespondences *) obj;
  *this = *objfull;
  //Parameter = (RxnData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionMoleculeCorrespondences
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataReaction::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  //result = result && Encode(buffer,---);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionMoleculeCorrespondences
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::DecodeThis(CommBuffer& buffer)
{
    bool result = RxnDataReaction::DecodeThis(buffer);
  // The rest

  return result;
} 
 
/*F ans = ReadCorrespondences(dbasetype,dbase,dbaseclass)
**
**  DESCRIPTION
**    dbasetype: The type of molecule set
**    dbase: The database source
**    dbaseclass: The source database class
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::ReadCorrespondences(istream& in,
								 String& dbasetype,
								 RxnDataMolecularStructuresDataBase *dbase,
								 RxnMolecularStructuresDataBaseClass *dbaseclass)
{
  BaseDataSetOfObjects *set = new BaseDataSetOfObjects();
  bool result = setupReactionMolecules(set,dbasetype,dbase,dbaseclass);
  
  ObjectList<String> rnames = getInternalReactantNames().GetKeyWords();
  StreamObjectInput str(in,' ');
  
  ReactantsBondCorrespondenceVector = new vector<int>();
  ObjectList<String>::iterator name;
  for(name = rnames.begin(); name != rnames.end(); name++)
    {
      String molname = getReactantName(*name);
      RxnDataSimpleMolecule *mol = (RxnDataSimpleMolecule *) set->GetObject(molname);
      result = result && ReadInReactantAtomCorrespondences(str,*name,mol);
      result = result && ReadInBondCorrespondences(ReactantsBondCorrespondenceVector,str,mol);
    }
  
  ProductBondCorespondenceVector = new vector<int>();
  ObjectList<String> pnames = getInternalProductNames().GetKeyWords();
  for(name = pnames.begin(); name != pnames.end(); name++)
    {
      String molname = getProductName(*name);
      RxnDataSimpleMolecule *mol = (RxnDataSimpleMolecule *) set->GetObject(molname);
      ReadInProductAtomCorrespondences(str,*name,mol);
      ReadInBondCorrespondences(ProductBondCorespondenceVector,str,mol);
    }
  return result;
}
 
/*F ReadInAtomCorrespondences(mol)
**
**  DESCRIPTION
**    mol: the molecule
**    ans: true if successful
**
**    For a given molecule line of the input, this reads in the reactant correspondences
**    and processes them (InitialReactantPair, AddCorrespondencePair and AddAtomCorrespondence).
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::ReadInReactantAtomCorrespondences(StreamObjectInput& str,
									       String& internalname,
									       RxnDataSimpleMolecule *mol)
{

  bool result = true;
  BaseDataSetOfObjects *atoms = mol->getNodes();
  ObjectList<String> anames = atoms->ListOfObjectNames();
  ObjectList<String>::iterator aname;
  for(aname = anames.begin(); result && aname != anames.end(); aname++)
    {
      String cS = str.ReadNext();
      unsigned int c = cS.ToInteger();

      OrderedReactantMolecules.push_back(internalname);
      OrderedReactantAtoms.push_back(*aname);
      
      BaseDataPair *p = InitialReactantPair(c,internalname,*aname);
      result = result && AddCorrespondencePair(p);
      result = result && AddAtomCorrespondence(internalname,*aname,p->NameTag);
    }
  return result;
}
 
/*F name = GeneratePairName(c)
**
**  DESCRIPTION
**    c: The code of the atom (from the input)
**    name: Name of the pair for code c
**
**    This generates, from the integer code of input,
**    the proper name of the pair.
**  REMARKS
**
*/
String RxnDataReactionMoleculeCorrespondences::GeneratePairName(unsigned int c)
{
  return PositveIntegerToString(c,GeneratePairPrefix,3);
}
/*F p = InitialReactantPair(c,molname,atomname)
**
**  DESCRIPTION
**    c: The code of the atom (from the input)
**    molname: The name of the molecule
**    atomname: The name of the atom
**    
**  REMARKS
**
*/
BaseDataPair *RxnDataReactionMoleculeCorrespondences::InitialReactantPair(unsigned int c, String& molname, String& atomname)
{
  BaseDataPair *pset = new BaseDataPair();
  pset->NameTag = GeneratePairName(c);

  BaseDataPair *p = new BaseDataPair();
  BaseDataString molS;
  BaseDataString atomS;
  molS.setString(molname);
  atomS.setString(atomname);
  p->setI(&molS);
  p->setJ(&atomS);
  pset->setI(p);
  delete p;
  return pset;
}
/*F AddCorrespondencePair(p)
**
**  DESCRIPTION
**    p: The correspondence pair to add
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::AddCorrespondencePair(BaseDataPair *p)
{
    return CorrespondencePairs.AddObject(p);
}

/*F AddAtomCorrespondence(molname,atomname,pairname)
**
**  DESCRIPTION
**    molname: The name of the molecule
**    atomname: The name of the atom in the molecule
**    pairname: The name of the corresponding pair
**    ans: true if successful
**
**    This creates the reactant molecule side of the correspondence
**    pair.  The pair will be called up again when the product
**    correspondences are read in and then will be filled in
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::AddAtomCorrespondence(String& molname,
								   String& atomname,
								   String& pairname)
{
  bool result = true;
  BaseDataSetOfObjects *mol,*atomlist;
  if(!AtomCorrespondences.IsInList(molname))
    {
      mol = (BaseDataSetOfObjects *) new BaseDataSetOfObjects();
      mol->NameTag = molname;
      AtomCorrespondences.AddObject(mol);
      delete mol;
    }
  mol = (BaseDataSetOfObjects *) AtomCorrespondences.GetObject(molname);
  
  if(!mol->IsInList(atomname))
      {
	  atomlist = (BaseDataSetOfObjects *) new BaseDataSetOfObjects();
	  atomlist->NameTag = atomname;
	  mol->AddObject(atomlist);
	  delete atomlist;
      }
  atomlist = (BaseDataSetOfObjects *) mol->GetObject(atomname);
  BaseDataString *str = new BaseDataString();
  str->setString(pairname);
  atomlist->AddObject(str);
  delete str;

  return result;
}
/*F p = GetCorrespondencePair(c)  . . . . . . . . . . . . . code to corr pair
**
**  DESCRIPTION
**   c: The code of the atom correspondence (from input)
**   p: The pair information of this correspondence atom
**    
**  REMARKS
**
*/
BaseDataPair *RxnDataReactionMoleculeCorrespondences::GetCorrespondencePair(unsigned int c)
{
  BaseDataPair *p = NULL;
  String name = GeneratePairName(c);
  if(CorrespondencePairs.IsInList(name))
    {
      p = (BaseDataPair *) CorrespondencePairs.GetObject(name);
    }
  else
    {
      cerr << "Correspondence pair not found: '" << name << "'" << endl;
    }
  return p;
}
/*F ans = ReadInProductAtomCorrespondences(mol) . . . . . . . .  for products
**
**  DESCRIPTION
**    mol: The molecule
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::ReadInProductAtomCorrespondences(StreamObjectInput& str,
									      String& internalname,
									      RxnDataSimpleMolecule *mol)
{
  bool result = true;
  BaseDataSetOfObjects *atoms = mol->getNodes();
  ObjectList<String> anames = atoms->ListOfObjectNames();
  ObjectList<String>::iterator aname;
  for(aname = anames.begin(); aname != anames.end(); aname++)
    {
      OrderedProductMolecules.push_back(internalname);
      OrderedProductAtoms.push_back(*aname);
      String cS = str.ReadNext();
      unsigned int c = cS.ToInteger();
      BaseDataPair *p = GetCorrespondencePair(c);
      result = result && StoreProductPair(p,internalname,*aname);
    }
  return result;
}
 
/*F ans = StoreProductPair(pset,molname,atomname) 
**
**  DESCRIPTION
**    pset: The correspondence pair
**    molname: The product molecule
**    atomname: The atom of the product
**
**    The sets up the product side of the correspondence pair
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::StoreProductPair(BaseDataPair *pset,String& molname,String atomname)
{
  BaseDataPair *p = new BaseDataPair();
  BaseDataString molS;
  BaseDataString atomS;
  molS.setString(molname);
  atomS.setString(atomname);
  p->setI(&molS);
  p->setJ(&atomS);
  pset->setJ(p);
  delete p;
  return true;
}

/*F ans = ReadInBondCorrespondences(vec,str,mol)  . .  2nd half of input line
**
**  DESCRIPTION
**    vec: The vector to store the correspondences in
**    str: The input object stream
**    mol: The molecule
**    ans: true if successful
**  
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::ReadInBondCorrespondences(vector<int> *vec,
								       StreamObjectInput& str,
								       RxnDataSimpleMolecule *mol)
{
  BaseDataSetOfObjects *atoms = mol->getEdges();
  ObjectList<String> anames = atoms->ListOfObjectNames();
  ObjectList<String>::iterator aname;
  for(aname = anames.begin(); aname != anames.end(); aname++)
    {
      String cS = str.ReadNext();
      unsigned int c = cS.ToInteger();
      vec->push_back(c);
    }
  return true;
}
/*F ans = ReadRxnFileReaction(in,rxnclass)
**
**  DESCRIPTION
**    in: The input stream
**    rxnclass: The reaction class
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::ReadRxnFileReaction(istream& in,
								 String& dbasetype,
								 RxnReactionClass *rxnclass,
								 RxnDataMolecularStructuresDataBase *dbase,
								 RxnMolecularStructuresDataBaseClass *dbaseclass)
{
  bool result = RxnDataReaction::ReadRxnFileReaction(in,dbasetype,rxnclass,dbase,dbaseclass);
  if(result)
    {
      String line;
      line.ReadFullLine(in);
      line.EliminateLeadingBlanks();
      String dash;
      line.IsolateNextWord(dash,' ');
      while(dash == "-")
	{
	  String idS;
	  line.EliminateLeadingBlanks();
	  line.IsolateNextWord(idS,' ');
	  if(idS == "ID") {
	      line.IsolateNextWord(idS,' ');
	    } else if(idS == "Correspondences") {
	      result = result && ReadCorrespondences(in,dbasetype,dbase,dbaseclass);
	    }
	  line.ReadFullLine(in);
	  line.EliminateLeadingBlanks();
	  line.IsolateNextWord(dash,' ');
	}
      String blank;
      dash.AppendToEnd(blank);
      dash.AppendToEnd(line);
      result = result && ReadRateConstantInfo(in,dash,rxnclass);
    }
  return result;
}
 
/*F ans = ReadRateConstantInfo(in,firstline,rxnclass) 
**
**  DESCRIPTION
**    in: The input stream
**    firstline: The first line of the information
**    rxnclass: The class information
**
**  REMARKS
**
*/
bool RxnDataReactionMoleculeCorrespondences::ReadRateConstantInfo(istream& in,String& firstline,
								  RxnReactionClass *rxnclass)
{
  bool result = true;
  RxnReactionRatesClass *rc = rxnclass->getRatesClass();
  firstline.EliminateLeadingBlanks();
  while(strncmp(firstline.c_str(),"$$$",3) && result)
    {
      if(firstline.c_str()[0] == '>')
	{
	  String dummy;
	  firstline.IsolateNextWord(dummy,'<');
	  firstline.EliminateBlanks();
	  String name = firstline.Isolate(0,firstline.size()-3);

	  RxnDataReactionRates *rate = (RxnDataReactionRates *) rc->BaseDataObjectExample();
	  rate->NameTag = name;
	  rate->Read(in,rc,rate->NameTag);
	  AddObject(rate);
	  delete rate;
	  /*
	  if(!strncmp(firstline.chars(),"ReverseRxn",10))
	    {
	      String name = firstline.Isolate(0,firstline.size()-3);
	      RxnDataReactionRates *rate = (RxnDataReactionRates *) rc->BaseDataObjectExample();
	      rate->NameTag = "ReverseRxn";
	      rate->Read(in,rc,rate->NameTag);
	      AddObject(rate);
	      delete rate;
	    }
	  */
	  firstline.ReadFullLine(in);
	  firstline.EliminateLeadingBlanks();
	  while(firstline.IsEmpty())
	    {
	      firstline.ReadFullLine(in);
	      firstline.EliminateLeadingBlanks();
	    }
	}
      else
	{
	  cerr << "Expected '<' at beginning of line: '" << firstline << "'" << endl;
	  result = false;
	}
    }
  return result;
}

/*S RxnReactionMoleculeCorrespondencesClass
 */
/*F RxnReactionMoleculeCorrespondencesClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionMoleculeCorrespondencesClass::RxnReactionMoleculeCorrespondencesClass()
{
  Identification = REACTION_CORRS_ID;
  NameTag = REACTION_CORRS_NAME;
  SubClass = "Reaction";
  EncodeDecodeClass = NameTag;
} 
/*F RxnReactionMoleculeCorrespondencesClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnReactionMoleculeCorrespondencesClass::RxnReactionMoleculeCorrespondencesClass(const RxnReactionMoleculeCorrespondencesClass& data)
  : RxnReactionClass(data)
{
  
} 
 
/*F RxnReactionMoleculeCorrespondencesClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnReactionMoleculeCorrespondencesClass::RxnReactionMoleculeCorrespondencesClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnReactionClass(id,name,descr)
{
  SubClass = "Reaction";
  EncodeDecodeClass = REACTION_CORRS_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnReactionMoleculeCorrespondencesClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnReactionMoleculeCorrespondencesClass::print(ostream& out) const
{
  RxnReactionClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnReactionMoleculeCorrespondencesClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnReactionMoleculeCorrespondencesClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnReactionMoleculeCorrespondencesClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnReactionClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnReactionMoleculeCorrespondencesClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnReactionMoleculeCorrespondencesClass::CopyClone(Identify *  objc)
{
  RxnReactionMoleculeCorrespondencesClass *objcfull = (RxnReactionMoleculeCorrespondencesClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnReactionMoleculeCorrespondencesClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnReactionMoleculeCorrespondencesClass::Clone()
    {
      RxnReactionMoleculeCorrespondencesClass* id = new RxnReactionMoleculeCorrespondencesClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionMoleculeCorrespondencesClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionMoleculeCorrespondencesClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionMoleculeCorrespondencesClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionMoleculeCorrespondencesClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnReactionClass::DecodeThis(buffer);
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
BaseDataObject * RxnReactionMoleculeCorrespondencesClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataReactionMoleculeCorrespondences();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnReactionMoleculeCorrespondencesClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnReactionMoleculeCorrespondencesClass*& obj)
     {
     obj = new RxnReactionMoleculeCorrespondencesClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataReactionMoleculeCorrespondences
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataReactionMoleculeCorrespondences*& obj)
     {
     obj = new RxnDataReactionMoleculeCorrespondences;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataReactionStructuresDataBase
 */ 
/*F RxnDataReactionStructuresDataBase()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataReactionStructuresDataBase::RxnDataReactionStructuresDataBase()
  : DBReaction(NULL),
    DBPatterns(NULL)
{
  Identification = REACTION_DBASE_ID;
  NameTag = REACTION_DBASE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataReactionStructuresDataBase(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataReactionStructuresDataBase::RxnDataReactionStructuresDataBase(const RxnDataReactionStructuresDataBase& data)
  : RxnDataMolecularStructuresDataBase(data),

    DBPatterns(data.DBPatterns)
{
    DBReaction = (BaseDataDataBaseInformation *) PointerClone(data.DBReaction);
    DBPatterns = (BaseDataDataBaseInformation *) PointerClone(data.DBPatterns);
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataReactionStructuresDataBase
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataReactionStructuresDataBase::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataReactionStructuresDataBase
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataReactionStructuresDataBase::Read(istream& in, DataObjectClass* objc, const String& name)
{
  RxnReactionStructuresDataBaseClass *dbclass = (RxnReactionStructuresDataBaseClass *) objc;
  bool result = RxnDataMolecularStructuresDataBase::Read(in,objc,name);
  String notdefined("Not Defined");
  result = result && PointerObjectRead(in,(BaseDataObject *&) DBReaction,dbclass->getMoleculeDBClass(),notdefined);
  result = result && PointerObjectRead(in,(BaseDataObject *&) DBPatterns,dbclass->getMoleculeDBClass(),notdefined);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataReactionStructuresDataBase
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataReactionStructuresDataBase::print(ostream& out) const
{
  RxnDataMolecularStructuresDataBase::print(out);
  PointerPrint(out,"The Reaction Database: ",
	       "No Database defined",DBReaction);
  PointerPrint(out,"The Reaction Pattern Database: ",
	       "No Database defined",DBPatterns);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionStructuresDataBase
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataReactionStructuresDataBase::Clone()
{
  RxnDataReactionStructuresDataBase *obj = new RxnDataReactionStructuresDataBase(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataReactionStructuresDataBase
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataReactionStructuresDataBase::CopyClone(Identify * obj)
{
  RxnDataReactionStructuresDataBase *objfull = (RxnDataReactionStructuresDataBase *) obj;
  *this = *objfull;
  DBReaction        = (BaseDataDataBaseInformation *) PointerClone(objfull->DBReaction);
  DBPatterns        = (BaseDataDataBaseInformation *) PointerClone(objfull->DBPatterns);
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionStructuresDataBase
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionStructuresDataBase::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataMolecularStructuresDataBase::EncodeThis(buffer);
  result = result && PointerEncode(buffer,DBReaction);
  result = result && PointerEncode(buffer,DBPatterns);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataReactionStructuresDataBase
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataReactionStructuresDataBase::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataMolecularStructuresDataBase::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) DBReaction);
  result = result && PointerDecode(buffer,(BaseDataObject *&) DBPatterns);
  return result;
}
/*F ans = Initialize(dbclass)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataReactionStructuresDataBase::Initialize(DataDataBaseInformationClass *dbclass)
{
  RxnDataMolecularStructuresDataBase::Initialize(dbclass);
  bool result = true;
  if( DBReaction != NULL &&
      DBPatterns != NULL)
    {
      DBReaction->OpenUpDataBase(dbclass);
      DBPatterns->OpenUpDataBase(dbclass);
    }
  else
    {
      cerr << "Databases not defined yet" << endl;
      result = false;
    }

  return result;
}
/*F getDatabaseInfo(type) . . . . . . . .  RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    type: The type of database
**
**  REMARKS
**
*/
BaseDataDataBaseInformation *RxnDataReactionStructuresDataBase::getDatabaseInfo(String& type)
{
  BaseDataDataBaseInformation *dbaseinfo = RxnDataMolecularStructuresDataBase::getDatabaseInfo(type);
  if(type == "Reaction")
    {
      dbaseinfo = DBReaction;
    }
  else if(type == "Patterns")
    dbaseinfo = DBPatterns;
  return dbaseinfo;
}
/*S RxnReactionStructuresDataBaseClass
 */
/*F RxnReactionStructuresDataBaseClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnReactionStructuresDataBaseClass::RxnReactionStructuresDataBaseClass()
{
  Identification = REACTION_DBASE_ID;
  NameTag = REACTION_DBASE_NAME;
  SubClass = "MolecularStructuresDataBase";
  EncodeDecodeClass = NameTag;
} 
/*F RxnReactionStructuresDataBaseClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnReactionStructuresDataBaseClass::RxnReactionStructuresDataBaseClass(const RxnReactionStructuresDataBaseClass& data)
  : RxnMolecularStructuresDataBaseClass(data)
{
} 
 
/*F RxnReactionStructuresDataBaseClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnReactionStructuresDataBaseClass::RxnReactionStructuresDataBaseClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnMolecularStructuresDataBaseClass(id,name,descr)
{
  SubClass = "MolecularStructuresDataBase";
  EncodeDecodeClass = "ReactionStructuresDataBase";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnReactionStructuresDataBaseClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnReactionStructuresDataBaseClass::print(ostream& out) const
{
  RxnMolecularStructuresDataBaseClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnReactionStructuresDataBaseClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnReactionStructuresDataBaseClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnReactionStructuresDataBaseClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnMolecularStructuresDataBaseClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnReactionStructuresDataBaseClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnReactionStructuresDataBaseClass::CopyClone(Identify *  objc)
{
  RxnReactionStructuresDataBaseClass *objcfull = (RxnReactionStructuresDataBaseClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnReactionStructuresDataBaseClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnReactionStructuresDataBaseClass::Clone()
    {
      RxnReactionStructuresDataBaseClass* id = new RxnReactionStructuresDataBaseClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionStructuresDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionStructuresDataBaseClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnMolecularStructuresDataBaseClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnReactionStructuresDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnReactionStructuresDataBaseClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnMolecularStructuresDataBaseClass::DecodeThis(buffer);
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
BaseDataObject * RxnReactionStructuresDataBaseClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataReactionStructuresDataBase();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnReactionStructuresDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnReactionStructuresDataBaseClass*& obj)
     {
     obj = new RxnReactionStructuresDataBaseClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataReactionStructuresDataBase
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataReactionStructuresDataBase*& obj)
     {
     obj = new RxnDataReactionStructuresDataBase;
     return obj->DecodeThis(buffer);
     }
/*S RxnSystemBase 
 */
/*F RxnSystemBase(argc,argv) . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    argc,argv: The input arguments
**
**  REMARKS
**
*/
RxnSystemBase::RxnSystemBase(int argc, char *argv[])
  : MoleculeSystemBase(argc,argv)
{
}
/*F EncodeDecodeObjectsSetUp()  . . . . . . . . . . . set up molecule objects
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnSystemBase::EncodeDecodeObjectsSetUp()
{
  MoleculeSystemBase::EncodeDecodeObjectsSetUp();
      InitialRxnUtilitiesDecodeFunctions();
      InitialSetOfRxnDecodeFunctions();
      InitialSetOfEquilibriumConstDecodeFunctions();
}
/*F StandardObjectsSetUp()  . . . . . . . . . . . . . set up molecule objects
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnSystemBase::StandardObjectsSetUp()
{
  MoleculeSystemBase::StandardObjectsSetUp();
      AddRxnUtilitiesClasses(getStandard());
      AddRxnClasses(getStandard());
      AddEquilibriumConstClasses(getStandard());
}
/*F CommandSetUp()  . . . . . . . . . . . . . . . . . . . . .  InstanceSystem
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnSystemBase::CommandSetUp()
{
  MoleculeSystemBase::CommandSetUp();

  SingleSystemCommand rxnread("ReadRxn",
			      "Read in a RxnFile",
			      &InputReaction);
  Commands.AddObject(rxnread.getName(),rxnread);

  SingleSystemCommand rxnfill("FillRxn",
			      "Fill in a Reaction with Current Molecule Information",
			      &FillReaction);
  Commands.AddObject(rxnfill.getName(),rxnfill);

}
/*F Initialization()  . . . . . . . . . . . . . . . . . .  RxnSystemBase
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void RxnSystemBase::Initialization()
{
  MoleculeSystemBase::Initialization();
}
/*F ans = InputReaction(sys)
**
**  DESCRIPTION
**    sys:

**  REMARKS
**
*/
int InputReaction(ReactionSystemBase* sys)
{
  RxnSystemBase *rxnsystem = (RxnSystemBase *) sys;
  int result = 1;

  if(rxnsystem->Inputs.size() != 4)
    {
      cerr << "Inputs:" << endl;
      cerr << "         Type: Reaction, Patterns" << endl;
      cerr << "         Type: Molecule, SubStructure" << endl;
      cerr << "         File: The RxnFile" << endl;
      cerr << "         Data: 'More': if more data, 'None': if none" << endl;
      exit(1);
    }
  String type = rxnsystem->GetNextInput();
  String mtype = rxnsystem->GetNextInput();
  String molfile = rxnsystem->GetNextInput();
  String moredataS = rxnsystem->GetNextInput();
  
  bool moredata = false;
  if(moredataS == "More")
    moredata = true;

  BaseDataDataBaseInformation *dbaseinfo = rxnsystem->MoleculeDataBase->getDatabaseInfo(type);
  DataDataBaseInformationClass *dbaseinfoclass = rxnsystem->MoleculeDataBaseClass->getMoleculeDBClass();
  if(dbaseinfo != NULL)
    {
      OpenInputFile file(molfile);
      RxnReactionClass* simpleclass = (RxnReactionClass *)
	dbaseinfo->getDataElementClass(dbaseinfoclass);
      bool success = true;
      while(success)
	{
	  RxnDataReaction *simple = (RxnDataReaction *) simpleclass->BaseDataObjectExample();
	  if(moredata)
	    success = simple->ReadRxnFileReaction(file.Stream,mtype,simpleclass,
						  rxnsystem->MoleculeDataBase,
						  rxnsystem->MoleculeDataBaseClass);
	  else
	    success = simple->RxnDataReaction::ReadRxnFileReaction(file.Stream,mtype,simpleclass,
								   rxnsystem->MoleculeDataBase,
								   rxnsystem->MoleculeDataBaseClass);
	  if(success)
	    {
	      BaseDataInstance *instance = new BaseDataInstance;
	      instance->NameTag = simple->NameTag;
	      rxnsystem->Instances.AddInstance(*instance);
	      delete instance;
	      instance = (BaseDataInstance *) rxnsystem->Instances.GetInstance(simple->NameTag);
	      simple->NameTag = simpleclass->getNameInInstance();
	      instance->AddObject(simple);
	      dbaseinfo->StoreElement(simple);
	    }
	  delete simple;
	}
    }
  else
    {
      cerr << "Database Information not found" << endl;
      result = false;
    }
  return result;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool FillReactions(BaseDataSetOfInstances& instances,
		   String& dbasetype,
		   RxnDataMolecularStructuresDataBase *dbase,
		   RxnMolecularStructuresDataBaseClass *dbaseclass,
		   RxnReactionClass *rxnclass,
		   BaseDataKeyWords *keynames)
{
  bool result = true;

  ObjectList<String> names = keynames->GetKeyWords();

  ObjectList<String>::iterator name;
  for(name = names.begin(); name != names.end() && result; name++)
    {
      if(instances.InstanceInSet(*name))
	{
	  BaseDataInstance *instance = (BaseDataInstance *) 
	    instances.GetInstance(*name);
	  if(instance->IsInList(rxnclass->getNameInInstance()))
	    {
	      RxnDataReaction *rxn = (RxnDataReaction *)
		instance->GetObject(rxnclass->getNameInInstance());
	      result = result && rxn->FillReactionWithMolecules(dbasetype,dbase,dbaseclass);
	    }
	  else
	    {
	      cerr << "Reaction: '" << rxnclass->getNameInInstance() << "'";
	      cerr << " not in instance: '" << instance->NameTag << "'" << endl;
	      result = false;
	    }
	}
      else
	{
	  cerr << "Reaction Instance '" << *name << "' not found" << endl;
	  result = false;
	}
    }
  return result;
}      
/*F ans = FillReaction(sys)
**
**  DESCRIPTION
**    sys:

**  REMARKS
**
*/
int FillReaction(ReactionSystemBase* sys)
{
  RxnSystemBase *rxnsystem = (RxnSystemBase *) sys;
  int result = 1;

  if(rxnsystem->Inputs.size() != 3)
    {
      cerr << "Inputs:" << endl;
      cerr << "         Type: Molecule, SubStructure" << endl;
      cerr << "         RxnType: The Reaction Class" << endl;
      cerr << "         Reactions: The keywords of reaction names" << endl;
      exit(1);
    }
  String mtype       = rxnsystem->GetNextInput();
  String rxnclassS   = rxnsystem->GetNextInput();
  String reactions   = rxnsystem->GetNextInput();
  
  bool success = true;

  if(rxnsystem->InstanceClasses.IsInList(rxnclassS))
    {
      RxnReactionClass *rxnclass = (RxnReactionClass *)
	rxnsystem->InstanceClasses.GetObjectClass(rxnclassS);

      if(rxnsystem->Instances.IsInList(reactions))
	{
	  BaseDataKeyWords *keynames = (BaseDataKeyWords *)
	    rxnsystem->Instances.GetObject(reactions);
	  success = FillReactions(rxnsystem->Instances,mtype,
				  rxnsystem->MoleculeDataBase,
				  rxnsystem->MoleculeDataBaseClass,
				  rxnclass,keynames);
	}
      else
	{
	  cerr << "The list of names not found in instances: '" << reactions << "'" << endl;
	  result = false;
	}
    }
  else
    {
      cerr << "The reaction class was not found: '" << rxnclassS << "'" << endl;
      result = false;
    }

  return result;
}

/*S Utilities
 */
/*F InitializeRxnPropertyDecodeFunctions()  . . . . . . . . . . . . Reactions
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialSetOfRxnDecodeFunctions()
{
  EncodeDecodeRegisterClass(RxnReactionStructuresDataBaseClass,RxnDataReactionStructuresDataBase,REACTION_DBASE_NAME);
  EncodeDecodeRegisterClass(RxnReactionMoleculeCorrespondencesClass,RxnDataReactionMoleculeCorrespondences,REACTION_CORRS_NAME);
  EncodeDecodeRegisterClass(RxnReactionClass,RxnDataReaction,REACTION_REACTION_NAME);
  EncodeDecodeRegisterClass(RxnThirdBodyClass,RxnDataThirdBody,REACTION_THIRDBODY_NAME);
  EncodeDecodeRegisterClass(RxnReactionRatesClass,RxnDataReactionRates,REACTION_RATES_NAME);
  EncodeDecodeRegisterClass(RxnReactionRatesRxnFileClass,RxnDataReactionRatesRxnFile,REACTION_RXNFILE_NAME);
  EncodeDecodeRegisterClass(RxnReactionRateHiLowClass,RxnDataReactionRateHiLow,REACTION_HILOW_NAME);
}
/*F AddRxnClasses(set)  . . . . . . . . . . . . . . . .  add reaction classes
**
**  DESCRIPTION
**    set: Set to add classes to
**
**  REMARKS
**
*/
void AddRxnClasses(DataSetOfObjectsClass& set)
{
  String dbasedescr("The Reaction Database Class");
  RxnReactionStructuresDataBaseClass dbaseclass(REACTION_DBASE_ID,REACTION_DBASE_NAME,dbasedescr);
  set.AddObjectClass(dbaseclass);
  
  String corrsdescr("The Reaction Correspondences Class");
  RxnReactionMoleculeCorrespondencesClass corrsclass(REACTION_CORRS_ID,REACTION_CORRS_NAME,corrsdescr);
  set.AddObjectClass(corrsclass);
  
  String rxndescr("The Reaction Class");
  RxnReactionClass rxnclass(REACTION_REACTION_ID,REACTION_REACTION_NAME,rxndescr);
  set.AddObjectClass(rxnclass);
  
  String thirddescr("The Third Body Reaction Rate Class");
  RxnThirdBodyClass thirdclass(REACTION_THIRDBODY_ID,REACTION_THIRDBODY_NAME,thirddescr);
  set.AddObjectClass(thirdclass);
  
  String ratesdescr("The Class");
  RxnReactionRatesClass ratesclass(REACTION_RATES_ID,REACTION_RATES_NAME,ratesdescr);
  set.AddObjectClass(ratesclass);
  
  String ratefiledescr("The Reaction Rate from RxnFile Class");
  RxnReactionRatesRxnFileClass ratefileclass(REACTION_RXNFILE_ID,REACTION_RXNFILE_NAME,ratefiledescr);
  set.AddObjectClass(ratefileclass);
  
  String hilowdescr("The Hi and Low Pressure Rate Parameterized Class");
  RxnReactionRateHiLowClass hilowclass(REACTION_HILOW_ID,REACTION_HILOW_NAME,hilowdescr);
  set.AddObjectClass(hilowclass);
}

