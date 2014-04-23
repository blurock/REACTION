/*  FILE     MechLumping.cc
**  PACKAGE  MechLumping
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "MechLumping" package.
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
#include "ThermoProps.hh"
#include "Rxn.hh"
#include "Mechanism.hh"
#include "InstanceAlgorithms.hh"
#include "DescriptionProbs.hh"
#include "EquivalentClasses.hh"
#include "MechLumping.hh"

/*S BaseDataRxnClassesInMolecules
 */ 
/*F BaseDataRxnClassesInMolecules()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataRxnClassesInMolecules::BaseDataRxnClassesInMolecules()
  : MechanismS(MECHANISM_PARAMETER),
    ReactionClassListS(REACTION_CLASS_LIST),
    ParameterNamesS(MOLRXN_PARAMETER_NAMES)
{
  Identification = LUMPING_MOLRXNCLASSALG_ID;
  NameTag = LUMPING_MOLRXNCLASSALG_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F BaseDataRxnClassesInMolecules(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
BaseDataRxnClassesInMolecules::BaseDataRxnClassesInMolecules(const BaseDataRxnClassesInMolecules& data)
  : BaseDataAlgorithmOperation(data),
    MechanismS(data.MechanismS),
    ReactionClassListS(data.ReactionClassListS),
    ParameterNamesS(data.ParameterNamesS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in BaseDataRxnClassesInMolecules
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool BaseDataRxnClassesInMolecules::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in BaseDataRxnClassesInMolecules
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool BaseDataRxnClassesInMolecules::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  BaseDataRxnClassesInMolecules
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& BaseDataRxnClassesInMolecules::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Find the reaction classes of the molecules in '" << MechanismS << "'";
  out << " to parameters in '" << ParameterNamesS << "'";
  out << " (for set of reaction classes: '" << ReactionClassListS << "')" << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .BaseDataRxnClassesInMolecules
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * BaseDataRxnClassesInMolecules::Clone()
{
  BaseDataRxnClassesInMolecules *obj = new BaseDataRxnClassesInMolecules(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .BaseDataRxnClassesInMolecules
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void BaseDataRxnClassesInMolecules::CopyClone(Identify * obj)
{
  BaseDataRxnClassesInMolecules *objfull = (BaseDataRxnClassesInMolecules *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataRxnClassesInMolecules
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataRxnClassesInMolecules::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,MechanismS);
  result = result && Encode(buffer,ParameterNamesS);
  result = result && Encode(buffer,ReactionClassListS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataRxnClassesInMolecules
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataRxnClassesInMolecules::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,MechanismS);
  result = result && Decode(buffer,ParameterNamesS);
  result = result && Decode(buffer,ReactionClassListS);
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
bool BaseDataRxnClassesInMolecules::SetUpAlgorithms(BaseDataSetOfInstances *instances,
				       DataSetOfInstancesClass *instancesclass,
				       BaseDataAlgorithmRun *run,
				       DataAlgorithmRunClass *runclass)
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
bool BaseDataRxnClassesInMolecules::CheckInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result && CheckInputVariable(MechanismS,"The mechanism",run);
  result = result && CheckInputVariable(ParameterNamesS,"The parameter names for the reaction class keywords",run);
  result = result && CheckInputVariable(ReactionClassListS,"The list of reaction classes to process",run);
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
bool BaseDataRxnClassesInMolecules::SetUpInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;

  ReactionClassList = (BaseDataKeyWords *) run->ParameterValue(ReactionClassListS)->Clone();
  ParameterNames = (BaseDataKeyWords *) run->ParameterValue(ParameterNamesS)->Clone();
  if(ParameterNames->SizeOf() == 3) {
    MechanismS = ParameterNames->NextKey();
    AsReactantS = ParameterNames->NextKey();
    AsProductS  = ParameterNames->NextKey();
  } else {
    cerr << "Expecting two parameter names: the parameter for reactants and the parameter for products" << endl;
    result = false;
  }
  if(instances->InstanceInSet(MechanismS)) {
    BaseDataInstance *instance = instances->GetInstance(MechanismS);
    if(instance->IsInList("Mechanism")) {
      Mechanism = (RxnDataMechanism *) instance->GetObject("Mechanism");
      DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
      
      MechanismClass = (RxnMechanismClass *) classes->GetObjectClass(Mechanism->GetType());
      RxnClass = (RxnReactionClass *) MechanismClass->getReactionClass();
      NameInInstance = "Reaction";
    } else {
      cerr << "In Mechanism '" << MechanismS << "', 'Mechanism' not found" << endl;
      result = false;
    }
  } else {
    cerr << "Mechanism '" << MechanismS << "' not found in instances" << endl;
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
bool BaseDataRxnClassesInMolecules::Calculate(BaseDataSetOfInstances *instances,
					      DataSetOfInstancesClass *instancesclass,
					      BaseDataAlgorithmRun *run,
					      DataAlgorithmRunClass *runclass)
{
  bool result = true;
  //if(run->AlgorithmSummary.KeyWordInList("")){}
  BaseDataKeyWords rxnnames = Mechanism->getReactionNames();
  while(ReactionClassList->SizeOf() > 0) {
    String name = ReactionClassList->NextKey();
    cout << "-------------- Reaction Class:" << name << endl;
    if(instances->InstanceInSet(name)) {
      BaseDataInstance *instance = instances->GetInstance(name);
      if(instance ->IsInList(NameInInstance)) {
	RxnDataReaction *rxncls = (RxnDataReaction *) instance->GetObject(NameInInstance);
	ObjectList<String> names = rxncls->ListOfObjectNames();
	ObjectList<String>::iterator rxnname;
	for(rxnname = names.begin();rxnname != names.end();rxnname++) {
	  if(rxnnames.KeyWordInList(*rxnname)) {
	    cout << "Reaction: " << *rxnname << endl;
	    RxnDataReaction *rxn = (RxnDataReaction *) rxncls->GetObject(*rxnname);
	    BaseDataKeySet *reactantnames = (BaseDataKeySet *) rxn->getReactantNames().Clone();
	    WriteMoleculeReactionClasses(instances,reactantnames,AsReactantS,name);
	    BaseDataKeySet *productnames = (BaseDataKeySet *) rxn->getProductNames().Clone();
	    WriteMoleculeReactionClasses(instances,productnames,AsProductS,name);
	  }
	}
      } else {
	cerr << "In reaction class '" << name << "' the reactions '" << NameInInstance << "' not found " << endl; 
      }
    } else {
      cerr << "Reaction Class not found in instances '" << name << "'" << endl;
    }
  }
  return result;
}
void  BaseDataRxnClassesInMolecules::WriteMoleculeReactionClasses(BaseDataSetOfInstances *instances,
								  BaseDataKeyWords *mols,
								  String& parametername,
								  String& rxnclass) {
  while(mols->SizeOf() > 0) {
    String name = mols->NextKey();
    if(instances->InstanceInSet(name)) {
      BaseDataInstance *mol = instances->GetInstance(name);
      if(mol->IsInList(parametername)) {
	BaseDataKeyWords *keys = (BaseDataKeyWords *) mol->GetObject(parametername);
	if(!keys->KeyWordInList(rxnclass))
	  keys->AddKeyWord(rxnclass);
      } else {
	BaseDataKeyWords *keys = new BaseDataKeyWords();
	keys->NameTag = parametername;
	keys->AddKeyWord(rxnclass);
	mol->AddObject(keys);
	delete keys;
      }
    }
  }
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
bool BaseDataRxnClassesInMolecules::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
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
bool BaseDataRxnClassesInMolecules::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 

/*S DataRxnClassesInMoleculesClass
 */
/*F DataRxnClassesInMoleculesClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
DataRxnClassesInMoleculesClass::DataRxnClassesInMoleculesClass()
{
  Identification = LUMPING_MOLRXNCLASSALG_ID;
  NameTag = LUMPING_MOLRXNCLASSALG_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F DataRxnClassesInMoleculesClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
DataRxnClassesInMoleculesClass::DataRxnClassesInMoleculesClass(const DataRxnClassesInMoleculesClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F DataRxnClassesInMoleculesClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
DataRxnClassesInMoleculesClass::DataRxnClassesInMoleculesClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "RxnClassesInMolecules";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . DataRxnClassesInMoleculesClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& DataRxnClassesInMoleculesClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . DataRxnClassesInMoleculesClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base DataRxnClassesInMoleculesClass, there is no further information.
**
**  REMARKS
**
*/
bool DataRxnClassesInMoleculesClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . DataRxnClassesInMoleculesClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void DataRxnClassesInMoleculesClass::CopyClone(Identify *  objc)
{
  DataRxnClassesInMoleculesClass *objcfull = (DataRxnClassesInMoleculesClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . DataRxnClassesInMoleculesClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * DataRxnClassesInMoleculesClass::Clone()
    {
      DataRxnClassesInMoleculesClass* id = new DataRxnClassesInMoleculesClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . DataRxnClassesInMoleculesClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataRxnClassesInMoleculesClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . DataRxnClassesInMoleculesClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataRxnClassesInMoleculesClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * DataRxnClassesInMoleculesClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new BaseDataRxnClassesInMolecules();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . DataRxnClassesInMoleculesClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, DataRxnClassesInMoleculesClass*& obj)
     {
     obj = new DataRxnClassesInMoleculesClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . BaseDataRxnClassesInMolecules
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, BaseDataRxnClassesInMolecules*& obj)
     {
     obj = new BaseDataRxnClassesInMolecules;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataLumpMechanism
 */ 
/*S RxnDataLumpMechanism
 */ 
/*F RxnDataLumpMechanism()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataLumpMechanism::RxnDataLumpMechanism()
  : MechanismNameS(MECHANISM_PARAMETER),
    LumpedMechanismNameS(LUMPED_MECHANISM_NAME),
    LumpedMoleculesS(LUMP_EQUIVALENT_SETS),
    SpeciesToEliminateS(SPECIES_TO_REMOVE)
{
  Identification = LUMPING_LUMPMECH_ID;
  NameTag = LUMPING_LUMPMECH_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataLumpMechanism(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataLumpMechanism::RxnDataLumpMechanism(const RxnDataLumpMechanism& data)
  : BaseDataAlgorithmOperation(data),
    MechanismNameS(data.MechanismNameS),
    LumpedMechanismNameS(data.LumpedMechanismNameS),
    LumpedMoleculesS(data.LumpedMoleculesS),
    SpeciesToEliminateS(data.SpeciesToEliminateS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataLumpMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataLumpMechanism::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataLumpMechanism
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataLumpMechanism::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataLumpMechanism
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataLumpMechanism::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Lump Mechanism, '" << MechanismNameS << "' ";
  out << "to '" << LumpedMechanismNameS << "' ";
  out << " using LumpedSpecies '" << LumpedMoleculesS << "' ";
  out << " and eliminating species '" << SpeciesToEliminateS << "'" << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataLumpMechanism
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataLumpMechanism::Clone()
{
  RxnDataLumpMechanism *obj = new RxnDataLumpMechanism(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataLumpMechanism
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataLumpMechanism::CopyClone(Identify * obj)
{
  RxnDataLumpMechanism *objfull = (RxnDataLumpMechanism *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataLumpMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataLumpMechanism::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,MechanismNameS);
  result = result && Encode(buffer,LumpedMechanismNameS);
  result = result && Encode(buffer,LumpedMoleculesS);
  result = result && Encode(buffer,SpeciesToEliminateS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataLumpMechanism
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataLumpMechanism::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,MechanismNameS);
  result = result && Decode(buffer,LumpedMechanismNameS);
  result = result && Decode(buffer,LumpedMoleculesS);
  result = result && Decode(buffer,SpeciesToEliminateS);
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
bool RxnDataLumpMechanism::SetUpAlgorithms(BaseDataSetOfInstances *instances,
				       DataSetOfInstancesClass *instancesclass,
				       BaseDataAlgorithmRun *run,
				       DataAlgorithmRunClass *runclass)
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
bool RxnDataLumpMechanism::CheckInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result && CheckInputVariable(MechanismNameS,"The original mechanism",run);
  result = result && CheckInputVariable(LumpedMechanismNameS,"The produced lumped mechanism",run);
  result = result && CheckInputVariable(LumpedMoleculesS,"The equivalent sets of lumped molecules",run);
  result = result && CheckInputVariable(SpeciesToEliminateS,"The list of species to eliminate",run);
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
bool RxnDataLumpMechanism::SetUpInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;

  MechanismName = (BaseDataString *) run->ParameterValue(MechanismNameS);
  if(instances->InstanceInSet(MechanismName->getString())) {
    BaseDataInstance *mechinstance = instances->GetInstance(MechanismName->getString());
    if(mechinstance->IsInList("Mechanism")) {
      Mechanism = (RxnDataMechanism *) mechinstance->GetObject("Mechanism");
    } else {
      cerr << "'Mechanism' in mechanism instance, '" << MechanismName->getString() << "' not found" << endl;
      result = false;
    }
  } else {
    cerr << "Mechanism instance '" << MechanismName->getString() << "' not found " << endl;
    result = false;
  }
  if(result) {
    BaseDataString *lumpedmolnames = (BaseDataString *) run->ParameterValue(LumpedMoleculesS);
    if(instances->IsInList(lumpedmolnames->getString())) {
      LumpedMolecules = (BaseDataSetOfEquivalentSets *) instances->GetObject(lumpedmolnames->getString());
      SpeciesToEliminate = (BaseDataKeyWords *) run->ParameterValue(SpeciesToEliminateS);
      
      int mtype = Mechanism->GetType();
      DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
      MechanismClass = (RxnMechanismClass *) classes->GetObjectClass(mtype);
      MoleculeClass = MechanismClass->getMoleculeClass();
      LumpedMechanism = (RxnDataMechanism *) MechanismClass->BaseDataObjectExample();
      ReactionClass = (RxnReactionClass *) MechanismClass->getReactionClass();
      
      BaseDataInstance finstance;
      finstance.NameTag = LumpedMechanismNameS;
      instances->AddInstance(finstance);
      BaseDataInstance *instance = instances->GetInstance(LumpedMechanismNameS);
      
      LumpedMechanism->NameTag = "Mechanism";
      instance->AddObject(LumpedMechanism);
      LumpedMechanism = (RxnDataMechanism *) instance->GetObject("Mechanism");
    } else {
      cerr << "Lumped molecules not in attributes: '" << lumpedmolnames->getString() << "'" << endl;
      result = false;
    }
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
bool RxnDataLumpMechanism::Calculate(BaseDataSetOfInstances *instances,
				 DataSetOfInstancesClass *instancesclass,
				 BaseDataAlgorithmRun *run,
				 DataAlgorithmRunClass *runclass)
{
 bool result = true;
 SetUpMolecules(instances,instancesclass);
 return result;
}
void RxnDataLumpMechanism::SetUpMolecules(BaseDataSetOfInstances *instances,
					  DataSetOfInstancesClass *instancesclass) {
  SetUpLumpedMoleculeCorrespondences(instances,instancesclass);
  BaseDataKeyWords *molnames = (BaseDataKeyWords *) Mechanism->getMoleculeNames().Clone();
  
  BaseDataKeyWords newmolnames = LumpedMechanism->getMoleculeNames();
  while(molnames->SizeOf() > 0)
    {
      String molname = molnames->NextKey();
      cout << "Molecule: " << molname << endl;
      if(MolCorrespondences->IsInList(molname)) {
	BaseDataString *lumpname = (BaseDataString *) MolCorrespondences->GetObject(molname);
	cout << "Molecule: " << molname << " is now " << lumpname->getString() << endl;
	if(!newmolnames.KeyWordInList(lumpname->getString())) {
	  cout << "Lumped Molecule '" << lumpname->getString() << "'" << endl;
	} else {
	  cout << "Lumped Molecule already in mechanism" << endl;
	}
      } else {
	cout << "Molecule: " << molname << " keeps its name" << endl;
	RxnDataSimpleMolecule *mol = Mechanism->getMolecule(molname,MoleculeClass,instances,instancesclass);
	RxnDataThermoProperty *thermo = Mechanism->getThermoValueForMolecule(molname,mol);
	RxnDataMoleculeSummary *summary = Mechanism->getMoleculeSummary(molname);
	LumpedMechanism->addMoleculeSummary(summary);
	LumpedMechanism->addMolecule(molname);
	LumpedMechanism->AddThermodynamic(molname,thermo);
      }
    }
}
void RxnDataLumpMechanism::SetUpLumpedMoleculeCorrespondences(BaseDataSetOfInstances *instances,
							      DataSetOfInstancesClass *instancesclass) {
  MolCorrespondences = new BaseDataSetOfObjects();
  ObjectList<String> names = LumpedMolecules->ListOfObjectNames();
  cout << "RxnDataLumpMechanism::SetUpLumpedMoleculeCorrespondences()" << endl;
  ObjectList<String>::iterator name;
  for(name = names.begin();name != names.end();name++) {
    BaseDataEquivalentSet *set = (BaseDataEquivalentSet *) LumpedMolecules->GetObject(*name);
    BaseDataKeyWords *mols = (BaseDataKeyWords *) set->getNames()->Clone();
    CreateAndAddMoleculeInfo(set,instances,instancesclass);
    while(mols->SizeOf() > 0) {
      BaseDataString corr;
      corr.NameTag = mols->NextKey();
      corr.setString(*name);
      MolCorrespondences->AddObject(&corr);
    }
  }
}
void RxnDataLumpMechanism::CreateAndAddMoleculeInfo(BaseDataEquivalentSet *set,
						    BaseDataSetOfInstances *instances,
						    DataSetOfInstancesClass *instancesclass) {
  BaseDataKeyWords *mols = (BaseDataKeyWords *) set->getNames()->Clone();
  String molname = mols->NextKey();
  RxnDataMoleculeSummary newsummary;
  RxnDataSimpleMolecule *mol = Mechanism->getMolecule(molname,MoleculeClass,instances,instancesclass);
  RxnDataThermoProperty *thermo = Mechanism->getThermoValueForMolecule(molname,mol);
  RxnDataMoleculeSummary *summary = (RxnDataMoleculeSummary *) 
    Mechanism->getMoleculeSummary(molname)->Clone();
  summary->ThermodynamicInfo = set->NameTag;
  summary->ShortName = set->NameTag;
  LumpedMechanism->AddThermodynamic(set->NameTag,thermo);
  LumpedMechanism->addMoleculeSummary(summary);
  delete summary;
}
/*
void RxnDataLumpMechanism::SetUpEquivalentReactions(BaseDataSetOfInstances *instances,
						    DataSetOfInstancesClass *instancesclass) {
  
  ObjectList<String> names = LumpedMolecules->ListOfObjectNames();
  ObjectList<String>::iterator name;
  for(name = names.begin();name != names.end();name++) {
    BaseDataEquivalentSet *set = (BaseDataEquivalentSet *) LumpedMolecules->GetObject(*name);
    BaseDataKeyWords *mols = (BaseDataKeyWords *) set->getNames();

    BaseDataKeyWords *equivrxns = FindReactionsWithLumpedMolecule(set);
    BaseDataEquivalentSet *equivrxnsset = SeparateReactionsIntoEquivalentSets(equivrxns);
    CombineEquivalentReactions(equivrxnsset);
    AddToLumpedReactions(equivrxns);
  }
}
void RxnDataLumpMechanism::SetUpReactionDescription(BaseDataSetOfInstances *instances,
					  DataSetOfInstancesClass *instancesclass) {
  bool result = true;

  

  RxnReactionClass *rxnclass = (RxnReactionClass *) MechanismClass->getReactionClass();
  ObjectList<String> names = Mechanism->getReactionNames().GetKeyWords();
  ObjectList<String>::iterator name;
  DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
  for(name = names.begin(); result && name != names.end(); name++) {
      RxnDataReactionSummary *summary = (RxnDataReactionSummary *) Mechanism->getReactionSummary(*name);
      RxnDataReaction *rxn = Mechanism->getReaction(summary->getReactionName(),
						    rxnclass,
						    instances,instancesclass);
      RxnDataReaction *newrxn = RewriteReaction(rxn);
      if(!AlreadyInSet(newrxn)) {
	Mechanism->addReaction(newrxn);
	AddReactionSummaryToMechanism(rxnname,forward,reverse);
      }
  }
}
*/
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
bool RxnDataLumpMechanism::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
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
bool RxnDataLumpMechanism::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 
 
 
/*S RxnLumpMechanismClass
 */
/*F RxnLumpMechanismClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnLumpMechanismClass::RxnLumpMechanismClass()
{
  Identification = LUMPING_LUMPMECH_ID;
  NameTag = LUMPING_LUMPMECH_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnLumpMechanismClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnLumpMechanismClass::RxnLumpMechanismClass(const RxnLumpMechanismClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnLumpMechanismClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnLumpMechanismClass::RxnLumpMechanismClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "LumpMechanism";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnLumpMechanismClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnLumpMechanismClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnLumpMechanismClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnLumpMechanismClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnLumpMechanismClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnLumpMechanismClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnLumpMechanismClass::CopyClone(Identify *  objc)
{
  RxnLumpMechanismClass *objcfull = (RxnLumpMechanismClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnLumpMechanismClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnLumpMechanismClass::Clone()
    {
      RxnLumpMechanismClass* id = new RxnLumpMechanismClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnLumpMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnLumpMechanismClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnLumpMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnLumpMechanismClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnLumpMechanismClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataLumpMechanism();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnLumpMechanismClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnLumpMechanismClass*& obj)
     {
     obj = new RxnLumpMechanismClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataLumpMechanism
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataLumpMechanism*& obj)
     {
     obj = new RxnDataLumpMechanism;
     return obj->DecodeThis(buffer);
     }
/*S BaseDataSimpleLumpedMolecules
 */ 
/*F BaseDataSimpleLumpedMolecules()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataSimpleLumpedMolecules::BaseDataSimpleLumpedMolecules()
  : EquivalentSetsS(EQUIVALENT_SET_NAME),
    MoleculeListS(MOLECULE_LIST),
    TakeFirstS(LUMPING_TAKE_FIRST)
{
  Identification = LUMPING_SIMPLEMOLECULE_ID;
  NameTag = LUMPING_SIMPLEMOLECULE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F BaseDataSimpleLumpedMolecules(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
BaseDataSimpleLumpedMolecules::BaseDataSimpleLumpedMolecules(const BaseDataSimpleLumpedMolecules& data)
  : BaseDataAlgorithmOperation(data),
    EquivalentSetsS(data.EquivalentSetsS),
    MoleculeListS(data.MoleculeListS),
    TakeFirstS(data.TakeFirstS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in BaseDataSimpleLumpedMolecules
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool BaseDataSimpleLumpedMolecules::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in BaseDataSimpleLumpedMolecules
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool BaseDataSimpleLumpedMolecules::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  BaseDataSimpleLumpedMolecules
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& BaseDataSimpleLumpedMolecules::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Create Lumped Molecules using '" << EquivalentSetsS << "'" << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .BaseDataSimpleLumpedMolecules
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * BaseDataSimpleLumpedMolecules::Clone()
{
  BaseDataSimpleLumpedMolecules *obj = new BaseDataSimpleLumpedMolecules(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .BaseDataSimpleLumpedMolecules
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void BaseDataSimpleLumpedMolecules::CopyClone(Identify * obj)
{
  BaseDataSimpleLumpedMolecules *objfull = (BaseDataSimpleLumpedMolecules *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataSimpleLumpedMolecules
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataSimpleLumpedMolecules::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,EquivalentSetsS);
  result = result && Encode(buffer,MoleculeListS);
  result = result && Encode(buffer,TakeFirstS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataSimpleLumpedMolecules
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataSimpleLumpedMolecules::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,EquivalentSetsS);
  result = result && Decode(buffer,MoleculeListS);
  result = result && Decode(buffer,TakeFirstS);
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
bool BaseDataSimpleLumpedMolecules::SetUpAlgorithms(BaseDataSetOfInstances *instances,
				       DataSetOfInstancesClass *instancesclass,
				       BaseDataAlgorithmRun *run,
				       DataAlgorithmRunClass *runclass)
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
bool BaseDataSimpleLumpedMolecules::CheckInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result && CheckInputVariable(EquivalentSetsS,"set of lumped molecules as sets of equivalent sets",run);
  result = result && CheckInputVariable(MoleculeListS,"original set of molecule names",run);
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
bool BaseDataSimpleLumpedMolecules::SetUpInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  cout << "Set up EquivalentSets '" << EquivalentSetsS << "'" << endl;
  EquivalentSets = (BaseDataSetOfEquivalentSets *) run->ParameterValue(EquivalentSetsS);
  EquivalentSets->print(cout);
  MoleculeList = (BaseDataKeyWords *) run->ParameterValue(MoleculeListS);
  TakeFirst = false;
  if(run->AlgorithmSummary.KeyWordInList(TakeFirstS)){
    TakeFirst = true;
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
bool BaseDataSimpleLumpedMolecules::Calculate(BaseDataSetOfInstances *instances,
				 DataSetOfInstancesClass *instancesclass,
				 BaseDataAlgorithmRun *run,
				 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  cout << "BaseDataSimpleLumpedMolecules::Calculate  " << endl;
  EquivalentSets->print(cout);
  BaseDataSetOfObjects *corrs = SetUpCorrespondences(EquivalentSets);
  BaseDataKeyWords *newmollist = TranslateKeyWords(MoleculeList,corrs);
  instances->AddObject(newmollist);
  if(TakeFirst) {
    cout << "BaseDataSimpleLumpedMolecules::Calculate  TakeFirst" << endl;
    SubstituteWithFirstElement(instances,instancesclass);
  }
  return result;
}
/*F SubstituteWithFirstElement(instances,instancesclass)  . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**      
**      Loop over equivalent sets of molecules:
**          name: name of the equivalent set
**          Loop over names of equivalent sets
**             until name of molecule is in instance
**             use this as name
**    
**  REMARKS
**
*/
void BaseDataSimpleLumpedMolecules::SubstituteWithFirstElement(BaseDataSetOfInstances *instances,
							       DataSetOfInstancesClass *instancesclass) {
  ObjectList<String> names = EquivalentSets->ListOfObjectNames();
  ObjectList<String>::iterator name;
  for(name=names.begin(); name != names.end(); name++) {
    cout << "Equivalent Set:" << *name << endl;
    BaseDataEquivalentSet *set = (BaseDataEquivalentSet *) EquivalentSets->GetObject(*name);
    set->getNames()->print(cout);
    cout << endl;
    BaseDataKeyWords *subs = (BaseDataKeyWords *) set->getNames()->Clone();
    bool found = false;
    String subname;
    while(subs->SizeOf() > 0 && !found) {
      subname = subs->NextKey();
      cout << subname << endl;
      if(instances->InstanceInSet(subname))
	found = true;
    }
    if(found) {
      String lump("L");
      BaseDataInstance *newinst = (BaseDataInstance *) instances->GetInstance(subname)->Clone();
      newinst->NameTag = *name;
      if(newinst->IsInList("ChemkinName")) {
	BaseDataString *chemkinname = (BaseDataString *) newinst->GetObject("ChemkinName");
	cout << "Chemkin Name: " << endl;
	chemkinname->print(cout);
	cout << endl;
	lump.AppendToEnd(chemkinname->getString());
	chemkinname->setString(lump);
	chemkinname->print(cout);

      }
      instances->AddInstance(*newinst);
      BaseDataInstance *inst = (BaseDataInstance *) instances->GetInstance(subname);
      BaseDataString *chemkinname = (BaseDataString *) inst->GetObject("ChemkinName");
      chemkinname->setString(lump);
    } else {
      cerr << "From lumped molecule '" << *name << "' molecules not found" << endl;
    }
  }
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
bool BaseDataSimpleLumpedMolecules::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
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
bool BaseDataSimpleLumpedMolecules::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 

/*S DataSimpleLumpedMoleculesClass
 */
/*F DataSimpleLumpedMoleculesClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
DataSimpleLumpedMoleculesClass::DataSimpleLumpedMoleculesClass()
{
  Identification = LUMPING_SIMPLEMOLECULE_ID;
  NameTag = LUMPING_SIMPLEMOLECULE_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F DataSimpleLumpedMoleculesClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
DataSimpleLumpedMoleculesClass::DataSimpleLumpedMoleculesClass(const DataSimpleLumpedMoleculesClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F DataSimpleLumpedMoleculesClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
DataSimpleLumpedMoleculesClass::DataSimpleLumpedMoleculesClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "SimpleLumpedMolecules";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . DataSimpleLumpedMoleculesClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& DataSimpleLumpedMoleculesClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . DataSimpleLumpedMoleculesClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base DataSimpleLumpedMoleculesClass, there is no further information.
**
**  REMARKS
**
*/
bool DataSimpleLumpedMoleculesClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . DataSimpleLumpedMoleculesClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void DataSimpleLumpedMoleculesClass::CopyClone(Identify *  objc)
{
  DataSimpleLumpedMoleculesClass *objcfull = (DataSimpleLumpedMoleculesClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . DataSimpleLumpedMoleculesClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * DataSimpleLumpedMoleculesClass::Clone()
    {
      DataSimpleLumpedMoleculesClass* id = new DataSimpleLumpedMoleculesClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . DataSimpleLumpedMoleculesClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataSimpleLumpedMoleculesClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . DataSimpleLumpedMoleculesClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataSimpleLumpedMoleculesClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * DataSimpleLumpedMoleculesClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new BaseDataSimpleLumpedMolecules();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . DataSimpleLumpedMoleculesClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, DataSimpleLumpedMoleculesClass*& obj)
     {
     obj = new DataSimpleLumpedMoleculesClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . BaseDataSimpleLumpedMolecules
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, BaseDataSimpleLumpedMolecules*& obj)
     {
     obj = new BaseDataSimpleLumpedMolecules;
     return obj->DecodeThis(buffer);
     }
/*S BaseDataSimpleEquivalentReactions
 */ 
/*F BaseDataSimpleEquivalentReactions()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataSimpleEquivalentReactions::BaseDataSimpleEquivalentReactions()
  : MoleculeEquivalentSetsS(LUMP_EQUIVALENT_SETS),
    EquivalentSetsS(EQUIVALENT_SET_NAME),
    ReactionListS(REACTION_LIST),
    TakeFirstS(LUMPING_TAKE_FIRST)
{
  Identification = LUMPING_SIMPLERXN_ID;
  NameTag = LUMPING_SIMPLERXN_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F BaseDataSimpleEquivalentReactions(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
BaseDataSimpleEquivalentReactions::BaseDataSimpleEquivalentReactions(const BaseDataSimpleEquivalentReactions& data)
  : BaseDataAlgorithmOperation(data),
    MoleculeEquivalentSetsS(data.MoleculeEquivalentSetsS),
    EquivalentSetsS(data.EquivalentSetsS),
    ReactionListS(data.ReactionListS),
    TakeFirstS(data.TakeFirstS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in BaseDataSimpleEquivalentReactions
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool BaseDataSimpleEquivalentReactions::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in BaseDataSimpleEquivalentReactions
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool BaseDataSimpleEquivalentReactions::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  BaseDataSimpleEquivalentReactions
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& BaseDataSimpleEquivalentReactions::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Create a set of single equivalent reactions using '" 
      << EquivalentSetsS << "'" << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .BaseDataSimpleEquivalentReactions
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * BaseDataSimpleEquivalentReactions::Clone()
{
  BaseDataSimpleEquivalentReactions *obj = new BaseDataSimpleEquivalentReactions(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .BaseDataSimpleEquivalentReactions
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void BaseDataSimpleEquivalentReactions::CopyClone(Identify * obj)
{
  BaseDataSimpleEquivalentReactions *objfull = (BaseDataSimpleEquivalentReactions *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataSimpleEquivalentReactions
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataSimpleEquivalentReactions::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,MoleculeEquivalentSetsS);
  result = result && Encode(buffer,EquivalentSetsS);
  result = result && Encode(buffer,TakeFirstS);
  result = result && Encode(buffer,ReactionListS);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataSimpleEquivalentReactions
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataSimpleEquivalentReactions::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,MoleculeEquivalentSetsS);
  result = result && Decode(buffer,EquivalentSetsS);
  result = result && Decode(buffer,TakeFirstS);
  result = result && Decode(buffer,ReactionListS);
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
bool BaseDataSimpleEquivalentReactions::SetUpAlgorithms(BaseDataSetOfInstances *instances,
				       DataSetOfInstancesClass *instancesclass,
				       BaseDataAlgorithmRun *run,
				       DataAlgorithmRunClass *runclass)
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
bool BaseDataSimpleEquivalentReactions::CheckInput(BaseDataSetOfInstances *instances,
						   DataSetOfInstancesClass *instancesclass,
						   BaseDataAlgorithmRun *run,
						   DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result &&  
    CheckInputVariable(MoleculeEquivalentSetsS,
		       "The set of lumped molecules as sets of equivalent sets",run);
  result = result && 
    CheckInputVariable(EquivalentSetsS,
		       "The set of lumped reactions as sets of equivalent sets",run);
  result = result && 
    CheckInputVariable(ReactionListS,
		       "The set of original reactions",run);
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
bool BaseDataSimpleEquivalentReactions::SetUpInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  MoleculeEquivalentSets = (BaseDataSetOfEquivalentSets *) 
    run->ParameterValue(MoleculeEquivalentSetsS);
  EquivalentSets = (BaseDataSetOfEquivalentSets *) run->ParameterValue(EquivalentSetsS);
  ReactionList = (BaseDataKeyWords *) run->ParameterValue(ReactionListS);
  TakeFirst = false;
  if(run->AlgorithmSummary.KeyWordInList(TakeFirstS)){
    TakeFirst = true;
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
bool BaseDataSimpleEquivalentReactions::Calculate(BaseDataSetOfInstances *instances,
				 DataSetOfInstancesClass *instancesclass,
				 BaseDataAlgorithmRun *run,
				 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  cout << "BaseDataSimpleEquivalentReactions Set up correspondence" << endl;
  BaseDataSetOfObjects *corrs = SetUpCorrespondences(EquivalentSets);
  cout << "BaseDataSimpleEquivalentReactions Translate the 'non-equivalent' reactions" << endl;
  TranslateRestReactions(ReactionList,instances,instancesclass,corrs);
  cout << "BaseDataSimpleEquivalentReactions Translate ReactionList" << endl;
  BaseDataKeyWords *newrxnlist = TranslateKeyWords(ReactionList,corrs);
  instances->AddObject(newrxnlist);

  cout << "BaseDataSimpleEquivalentReactions Create New Reactions" << endl;
  ObjectList<String> names = EquivalentSets->ListOfObjectNames();
  ObjectList<String>::iterator name;
  for(name=names.begin(); name != names.end(); name++) {
	cout << "Equivalent Set:" << *name << endl;
	BaseDataEquivalentSet *set = (BaseDataEquivalentSet *) EquivalentSets->GetObject(*name);
	set->getNames()->print(cout);
	cout << endl;
	BaseDataKeyWords *subs = (BaseDataKeyWords *) set->getNames()->Clone();
	unsigned int numrxns = subs->SizeOf();
	//double numrxnsD = (double) numrxns;
	String subname;
	bool found = false;
	String foundname = "";
	while(subs->SizeOf() > 0 && !found) {
	  subname = subs->NextKey();
	  cout << subname << endl;
	  if(instances->InstanceInSet(subname)) {
	    BaseDataInstance *instance = (BaseDataInstance *) instances->GetInstance(subname);
	    BaseDataInstance *newinst = UpdateReactionCoefficients(true,numrxns,subname,*name,instance,instancesclass);
	    if(newinst != NULL) {
	      found = true;
	      instances->AddInstance(*newinst);
	    } else {
	      cerr << "ERROR: Lumped Reaction not undated: '" << *name << "'" << endl;
	    }
	  }
	}
	
	if(!found) {
	  cerr << "From lumped reaction '" << *name << "' reactions not found" << endl;
	}
  }
  
  return result;
}

  BaseDataInstance *BaseDataSimpleEquivalentReactions::UpdateReactionCoefficients(bool newinstance,
										  unsigned int numrxns,
										String& subname,
										String& name,
										BaseDataInstance *instance,
										DataSetOfInstancesClass *instancesclass) {
  bool found = false;
  String NameInInstance = "Reaction";
  BaseDataInstance *newinst = NULL;
  if(instance->IsInList(NameInInstance)) {
    RxnDataReaction *rxn = (RxnDataReaction *) instance->GetObject(NameInInstance);
    DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
    //RxnReactionRatesClass *rxnclass = (RxnReactionRatesClass *) classes->GetObjectClass(rxn->GetType());
    if(instance->IsInList(REACTION_SUMMARY_NAME)) {
      double Af = 0.0;
      double nf = 0.0;
      double Ef = 0.0;
      double Ar = 0.0;
      double nr = 0.0;
      double Er = 0.0;
      found = true;
      RxnDataReactionSummary *summary = (RxnDataReactionSummary *) instance->GetObject(REACTION_SUMMARY_NAME);
      String forwardS = summary->getForwardRate();
      UpdateConstants(rxn,instancesclass,forwardS,&Af,&nf,&Ef);
      String reverseS = summary->getReverseRate();
      UpdateConstants(rxn,instancesclass,reverseS,&Ar,&nr,&Er);
      unsigned int forwardfactor = LumpingFactors("Reactants",instance);
      unsigned int reversefactor = LumpingFactors("Products",instance);
      double forwardfactorD = 1.0;
      double reversefactorD = 1.0;
      if(numrxns > 1) 
	forwardfactorD = (double) forwardfactor;
      if(numrxns > 1)
	reversefactorD = (double) reversefactor;
      cout << "Forward: (" << Af << ") * (" << reversefactorD << ")" << endl;
      cout << "Reverse: (" << Ar << ") * (" << forwardfactorD << ")" << endl;
      Ar = forwardfactorD * Ar;
      Af = reversefactorD * Af;
      if(newinstance)
	newinst = (BaseDataInstance *) instance->Clone();
      else
	newinst = instance;
      RxnDataReaction *rxn = (RxnDataReaction *) newinst->GetObject(NameInInstance);
      RxnDataReactionSummary *sum = (RxnDataReactionSummary *) newinst->GetObject(REACTION_SUMMARY_NAME);
      RxnDataReactionRates *forward = (RxnDataReactionRates *) rxn->GetObject(sum->getForwardRate());
      RxnReactionRatesClass *fclass = (RxnReactionRatesClass *) classes->GetObjectClass(forward->GetType());
      RxnDataReactionRates *reverse = (RxnDataReactionRates *) rxn->GetObject(sum->getReverseRate());
      RxnReactionRatesClass *rclass = (RxnReactionRatesClass *) classes->GetObjectClass(reverse->GetType());
      BaseDataReal AfReal;
      AfReal.SetValue(Af);
      forward->setArrhenius(&AfReal,fclass);
      BaseDataReal nfReal;
      nfReal.SetValue(nf);
      forward->setTemperatureCoefficient(&nfReal,fclass);
      BaseDataReal EfReal;
      EfReal.SetValue(Ef);
      forward->setActivationEnergy(&EfReal,fclass);
      BaseDataReal ArReal;
      ArReal.SetValue(Ar);
      reverse->setArrhenius(&ArReal,rclass);
      BaseDataReal nrReal;
      nrReal.SetValue(nr);
      reverse->setTemperatureCoefficient(&nrReal,rclass);
      BaseDataReal ErReal;
      ErReal.SetValue(Er);
      reverse->setActivationEnergy(&ErReal,rclass);
      newinst->NameTag = name;
      
      TranslateProductsAndReactantsInReaction(subname,newinst,rxn);
    }
  }
  return newinst;
}
void BaseDataSimpleEquivalentReactions::UpdateConstants(RxnDataReaction *rxn, DataSetOfInstancesClass *instancesclass, 
							String& rateS,double *A,double *n,double *E) {
  if(rxn->IsInList(rateS)) {
    RxnDataReactionRates *rate = (RxnDataReactionRates *) rxn->GetObject(rateS);
    RxnReactionRatesClass *rxnclass = (RxnReactionRatesClass *) instancesclass->PointerToAllowedClasses()->GetObjectClass(rate->GetType());
    *E = rate->getActivationEnergyValue(rxnclass);
    *n = rate->getTemperatureCoefficientValue(rxnclass);
    *A = rate->getArrheniusValue(rxnclass);
  }
}
void BaseDataSimpleEquivalentReactions::TranslateRestReactions(BaseDataKeyWords *rxnlist,
							       BaseDataSetOfInstances *instances,
							       DataSetOfInstancesClass *instancesclass,
							       BaseDataSetOfObjects *corrs) {
  String NameInInstance = "Reaction";
  BaseDataKeyWords *names = (BaseDataKeyWords *) rxnlist->Clone();
  while(names->SizeOf() > 0) {
    String name = names->NextKey();
    if(!corrs->IsInList(name)) {
      BaseDataInstance *instance = (BaseDataInstance *) instances->GetInstance(name);
      if(instance->IsInList(NameInInstance)) {
	RxnDataReaction *rxn = (RxnDataReaction *) instance->GetObject(NameInInstance);
	TranslateProductsAndReactantsInReaction(name,instance,rxn);
      } else {
	cerr << "ERROR: 'Reaction' not found in instance: '" << name << "'" << endl;
      }
      //UpdateReactionCoefficients(false,name,name,instance,instancesclass);
    }
  }
}
unsigned int BaseDataSimpleEquivalentReactions::LumpingFactors(const String& type, BaseDataInstance *inst) {  
  DataKeySetClass keyclass;
  unsigned int factor = 1;
  if(inst->IsInList(type)) {
    BaseDataKeyWords *names = (BaseDataKeyWords *) inst->GetObject(type)->Clone();
    cout << "'Lump Check, " << type <<  "':" << inst->NameTag << "':" << endl;
    names->WriteAsASCII(cout,&keyclass);
    cout << endl;
    while(names->SizeOf() > 0) {
      String name = names->NextKey();
      if( MoleculeEquivalentSets->IsInList(name)) {
	BaseDataEquivalentSet *lumped = (BaseDataEquivalentSet *) MoleculeEquivalentSets->GetObject(name);
	BaseDataKeyWords *orignames = lumped->getNames();
	unsigned int nlumped = orignames->SizeOf();
	cout << "Lump Factor: '" << name << "'    Count: " << nlumped << endl;
	factor *= nlumped;
      } else {
	cout << "Non lumped: " << name << endl;
      }
    }
  } else { 
    cerr << type << " not found in instance '" << inst->NameTag << "'" << endl;
  }
  return factor;
}
void BaseDataSimpleEquivalentReactions::TranslateProductsAndReactantsInReaction(String& foundname,
										BaseDataInstance *newinst, 
										RxnDataReaction *rxn) {
	  DataKeySetClass keyclass;
	  RxnDataMoleculeSet *Reactants = new RxnDataMoleculeSet();
	  RxnDataMoleculeSet *Products  = new RxnDataMoleculeSet();
	  rxn->setReactants(Reactants);
	  rxn->setProducts(Products);
	  if(newinst->IsInList("Reactants")) {
	    BaseDataKeyWords *names = (BaseDataKeyWords *) newinst->GetObject("Reactants")->Clone();
	    cout << "'Reactants: '" << foundname << "':" << endl;
	    names->WriteAsASCII(cout,&keyclass);
	    cout << endl;
	    while(names->SizeOf() > 0) {
	      String name = names->NextKey();
	      Reactants->AddMolecule(name);
	    }
	  } else { 
	    cerr << "'Reactants' not found in instance '" << foundname << "'" << endl;
	  }
	  
	  if(newinst->IsInList("Products")) {
	    BaseDataKeyWords *names = (BaseDataKeyWords *) newinst->GetObject("Products")->Clone();
	    cout << "'Products: '" << foundname << "':" << endl;
	    names->WriteAsASCII(cout,&keyclass);
	    cout << endl;
	    while(names->SizeOf() > 0) {
	      String name = names->NextKey();
	      Products->AddMolecule(name);
	    }
	  } else { 
	    cerr << "'Products' not found in instance '" << foundname << "'" << endl;
	  }
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
bool BaseDataSimpleEquivalentReactions::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
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
bool BaseDataSimpleEquivalentReactions::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 
 
 
/*S DataSimpleEquivalentReactionsClass
 */
/*F DataSimpleEquivalentReactionsClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
DataSimpleEquivalentReactionsClass::DataSimpleEquivalentReactionsClass()
{
  Identification = LUMPING_SIMPLERXN_ID;
  NameTag = LUMPING_SIMPLERXN_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F DataSimpleEquivalentReactionsClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
DataSimpleEquivalentReactionsClass::DataSimpleEquivalentReactionsClass(const DataSimpleEquivalentReactionsClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F DataSimpleEquivalentReactionsClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
DataSimpleEquivalentReactionsClass::DataSimpleEquivalentReactionsClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "SimpleEquivalentReactions";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . DataSimpleEquivalentReactionsClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& DataSimpleEquivalentReactionsClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . DataSimpleEquivalentReactionsClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base DataSimpleEquivalentReactionsClass, there is no further information.
**
**  REMARKS
**
*/
bool DataSimpleEquivalentReactionsClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . DataSimpleEquivalentReactionsClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void DataSimpleEquivalentReactionsClass::CopyClone(Identify *  objc)
{
  DataSimpleEquivalentReactionsClass *objcfull = (DataSimpleEquivalentReactionsClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . DataSimpleEquivalentReactionsClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * DataSimpleEquivalentReactionsClass::Clone()
    {
      DataSimpleEquivalentReactionsClass* id = new DataSimpleEquivalentReactionsClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . DataSimpleEquivalentReactionsClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataSimpleEquivalentReactionsClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . DataSimpleEquivalentReactionsClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataSimpleEquivalentReactionsClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * DataSimpleEquivalentReactionsClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new BaseDataSimpleEquivalentReactions();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . DataSimpleEquivalentReactionsClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, DataSimpleEquivalentReactionsClass*& obj)
     {
     obj = new DataSimpleEquivalentReactionsClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . BaseDataSimpleEquivalentReactions
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, BaseDataSimpleEquivalentReactions*& obj)
     {
     obj = new BaseDataSimpleEquivalentReactions;
     return obj->DecodeThis(buffer);
     }
/*S Utilities
 */
/*F InitializeMechLumpingDecodeFunctions()  . . . . . . . . . . . . Reactions
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialMechLumpingDecodeFunctions()
{
  EncodeDecodeRegisterClass(DataRxnClassesInMoleculesClass,BaseDataRxnClassesInMolecules,LUMPING_MOLRXNCLASSALG_NAME);
  EncodeDecodeRegisterClass(RxnLumpMechanismClass,RxnDataLumpMechanism,LUMPING_LUMPMECH_NAME);
  EncodeDecodeRegisterClass(DataSimpleLumpedMoleculesClass,BaseDataSimpleLumpedMolecules,LUMPING_SIMPLEMOLECULE_NAME);
  EncodeDecodeRegisterClass(DataSimpleEquivalentReactionsClass,BaseDataSimpleEquivalentReactions,LUMPING_SIMPLERXN_NAME);
}
/*F AddMechLumpingClasses(set) . . . . . . . . . . . .  EquilibriumConst
**
**  DESCRIPTION
**    set: The set of classes to add them to
**
**  REMARKS
**
*/
void AddMechLumpingClasses(DataSetOfObjectsClass& set)
{
  String molrxndescr("The Determine Reaction Classes of Molecules Algorithm Class");
  DataRxnClassesInMoleculesClass molrxnclass(LUMPING_MOLRXNCLASSALG_ID,LUMPING_MOLRXNCLASSALG_NAME,molrxndescr);
  set.AddObjectClass(molrxnclass);

  String lumpmechdescr("The Lump Molecules in Mechanism Algorithm Class");
  RxnLumpMechanismClass lumpmechclass(LUMPING_LUMPMECH_ID,LUMPING_LUMPMECH_NAME,lumpmechdescr);
  set.AddObjectClass(lumpmechclass);

  String simpmoldescr("The Simple Molecule Lumping Algorithm Class");
  DataSimpleLumpedMoleculesClass simpmolclass(LUMPING_SIMPLEMOLECULE_ID,LUMPING_SIMPLEMOLECULE_NAME,simpmoldescr);
  set.AddObjectClass(simpmolclass);

  String simprxndescr("The Simple Equivalent Reaction Creation Class");
  DataSimpleEquivalentReactionsClass simprxnclass(LUMPING_SIMPLERXN_ID,LUMPING_SIMPLERXN_NAME,simprxndescr);
  set.AddObjectClass(simprxnclass);

}
