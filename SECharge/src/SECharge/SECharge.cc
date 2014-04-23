/*  FILE     SECharge.cc
**  PACKAGE  SECharge
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "SECharge" package.
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
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "SECharge.hh"

#define DEBUG_CHARGE
#define SECHARGE_DEBUG

/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
MoleculeElectronic *TransferMoleculeToMolFileMolecule(RxnDataSimpleMolecule& molecule)
{
  BaseDataSetOfObjects *atoms = molecule.getNodes();
  BaseDataSetOfObjects *bonds = molecule.getEdges();

  ObjectList<String> atmnames = atoms->ListOfObjectNames();
  ObjectList<String> bndnames = bonds->ListOfObjectNames();
  ObjectList<String>::iterator name;

  unsigned int natm = atmnames.size();
  unsigned int nbnd = bndnames.size();

  double *atmvec = AllocArrayFLOAT(natm*6);
  double *atmvecstart = atmvec;
  for(name = atmnames.begin();name != atmnames.end();name++)
    {
      RxnDataMoleculeAtom *atom = (RxnDataMoleculeAtom *) atoms->GetObject(*name);
      RxnDataBasicAtomData *atmdata = (RxnDataBasicAtomData *) atom->getBasicAtomData();
      *(atmvec+0) = atmdata->X;
      *(atmvec+1) = atmdata->Y;
      *(atmvec+2) = atmdata->Z;
      *(atmvec+3) = atmdata->AtomicNumber;
      *(atmvec+4) = atmdata->Charge;
      *(atmvec+5) = atmdata->Radical;
      printf("Atom: %10f %10f %10f   %10f %10f %10f\n",
	     *(atmvec+0),*(atmvec+1),*(atmvec+2),*(atmvec+3),*(atmvec+4),*(atmvec+5));
      atmvec += 6;
    }

  int *bndvec = AllocArrayINT(nbnd*3);
  int *bndvecstart = bndvec;
  for(name = bndnames.begin();name != bndnames.end();name++)
    {
      RxnDataMoleculeBond *bond = (RxnDataMoleculeBond *) bonds->GetObject(*name);
      RxnDataMolFileBond *bnddata = (RxnDataMolFileBond *) bond->getBasicBondData();
      *(bndvec + 0) = bnddata->BondI-1;
      *(bndvec + 1) = bnddata->BondJ-1;
      *(bndvec + 2) = bnddata->BondOrder;

      bndvec += 3;
    }
  MolFileMolecule *mlfmolwithout = LoadMolFileMolecule(1,"molecule",
						       natm,nbnd,atmvecstart,bndvecstart);
  MolFileMolecule *mlfmol = AddHydrogens(mlfmolwithout);
  MoleculeElectronic *molele = ElectronicFromMolFile(mlfmol);

  FILE *file = fopen("test.txt","w");
  PrintPrettyMoleculeElectronic("->",file,molele);

  free(atmvecstart);
  free(bndvecstart);
  float *vec = UnLoadMolFileMolecule(molele);

  for(name = atmnames.begin();name != atmnames.end();name++)
    {
      RxnDataMoleculeAtom *atom = (RxnDataMoleculeAtom *) atoms->GetObject(*name);
      RxnDataBasicAtomData *atmdata = (RxnDataBasicAtomData *) atom->getBasicAtomData();
      atmdata->Charge                = *(vec+1);
      atmdata->NumberOfElectrons     = *(vec+0);
      atmdata->Screening             = *(vec+3);
      atmdata->ZEff                  = *(vec+4);
      atmdata->AtomElectronegativity = *(vec+2);

      fprintf(file,"Atom: Charge: %10f #Electrons: %10f  Screening: %10f\n      ZEff: %10f AtomElectronegativity: %10f\n",
	     atmdata->Charge,atmdata->NumberOfElectrons,atmdata->Screening,
	     atmdata->ZEff,atmdata->AtomElectronegativity);
      vec += 6;
    }
  fclose(file);

  return molele;
}
/*S RxnDataCalculateElectronegativity
 */ 
/*F RxnDataCalculateElectronegativity()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataCalculateElectronegativity::RxnDataCalculateElectronegativity()
{
  Identification = SECHARGE_OP_ID;
  NameTag = SECHARGE_OP_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataCalculateElectronegativity(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataCalculateElectronegativity::RxnDataCalculateElectronegativity(const RxnDataCalculateElectronegativity& data)
  : BaseDataOperation(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataCalculateElectronegativity
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataCalculateElectronegativity::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataCalculateElectronegativity
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataCalculateElectronegativity::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataOperation::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataCalculateElectronegativity
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataCalculateElectronegativity::print(ostream& out) const
{
  BaseDataOperation::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataCalculateElectronegativity
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataCalculateElectronegativity::Clone()
{
  RxnDataCalculateElectronegativity *obj = new RxnDataCalculateElectronegativity(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataCalculateElectronegativity
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataCalculateElectronegativity::CopyClone(Identify * obj)
{
  RxnDataCalculateElectronegativity *objfull = (RxnDataCalculateElectronegativity *) obj;
  *this = *objfull;
  //Parameter = (BaseData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataCalculateElectronegativity
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataCalculateElectronegativity::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  //result = result && Encode(buffer,---);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataCalculateElectronegativity
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataCalculateElectronegativity::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::DecodeThis(buffer);
  // The rest

  return result;
}
/*F obj = operator()(x,y,xclass,yclass) . . . . . . . . . . BaseDataOperation
**
**  DESCRIPTION
**    x,y: The objects to be operated on
**    xclass,yclass: The object classes
**    obj: The result
**
**    This is a dummy two-dimensional call.  An empty object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataCalculateElectronegativity::operator()(BaseDataObject*, BaseDataObject*,
							       DataObjectClass*, DataObjectClass*)
                                              
{
  return new BaseDataObject;
}
/*F obj = operator()(x,xclass)  . . . . . . . . . . . . . . BaseDataOperation
**
**  DESCRIPTION
**    x: The object to be operated on
**    xclass: The class of the object
**    obj: The result
**
**    This is a dummy one-dimensional call.  An empty object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataCalculateElectronegativity::operator()(BaseDataObject *obj,
							       DataObjectClass *objclass)
{
  RxnDataSimpleMolecule *molecule = (RxnDataSimpleMolecule *) obj;

  BaseDataSetOfObjects *atoms = molecule->getNodes();
  BaseDataSetOfObjects *bonds = molecule->getEdges();

  ObjectList<String> atmnames = atoms->ListOfObjectNames();
  ObjectList<String> bndnames = bonds->ListOfObjectNames();
  ObjectList<String>::iterator name;

  unsigned int natm = atmnames.size();
  unsigned int nbnd = bndnames.size();

  double *atmvec = AllocArrayFLOAT(natm*6);
  double *atmvecstart = atmvec;
  for(name = atmnames.begin();name != atmnames.end();name++)
    {
      RxnDataMoleculeAtom *atom = (RxnDataMoleculeAtom *) atoms->GetObject(*name);
      RxnDataBasicAtomData *atmdata = (RxnDataBasicAtomData *) atom->getBasicAtomData();
      *(atmvec+0) = atmdata->X;
      *(atmvec+1) = atmdata->Y;
      *(atmvec+2) = atmdata->Z;
      *(atmvec+3) = atmdata->AtomicNumber;
      *(atmvec+4) = atmdata->Charge;
      *(atmvec+5) = atmdata->Radical;
      printf("Atom: %10f %10f %10f   %10f %10f %10f\n",
	     *(atmvec+0),*(atmvec+1),*(atmvec+2),*(atmvec+3),*(atmvec+4),*(atmvec+5));
      atmvec += 6;
    }

  int *bndvec = AllocArrayINT(nbnd*3);
  int *bndvecstart = bndvec;
  for(name = bndnames.begin();name != bndnames.end();name++)
    {
      RxnDataMoleculeBond *bond = (RxnDataMoleculeBond *) bonds->GetObject(*name);
      RxnDataMolFileBond *bnddata = (RxnDataMolFileBond *) bond->getBasicBondData();
      *(bndvec + 0) = bnddata->BondI-1;
      *(bndvec + 1) = bnddata->BondJ-1;
      *(bndvec + 2) = bnddata->BondOrder;

      bndvec += 3;
    }
  MolFileMolecule *mlfmolwithout = LoadMolFileMolecule(1,"molecule",
						       natm,nbnd,atmvecstart,bndvecstart);
  MolFileMolecule *mlfmol = AddHydrogens(mlfmolwithout);
  MoleculeElectronic *molele = ElectronicFromMolFile(mlfmol);

  FILE *file = fopen("test.txt","w");
  PrintPrettyMoleculeElectronic("->",file,molele);

  free(atmvecstart);
  free(bndvecstart);
  float *vec = UnLoadMolFileMolecule(molele);

  VectorNumeric *vect = new VectorNumeric(natm);
  VectorNumeric::iterator vectele = vect->begin();
  for(name = atmnames.begin();name != atmnames.end();name++)
    {
      RxnDataMoleculeAtom *atom = (RxnDataMoleculeAtom *) atoms->GetObject(*name);
      RxnDataBasicAtomData *atmdata = (RxnDataBasicAtomData *) atom->getBasicAtomData();
      atmdata->Charge                = *(vec+1);
      atmdata->NumberOfElectrons     = *(vec+0);
      atmdata->Screening             = *(vec+3);
      atmdata->ZEff                  = *(vec+4);
      atmdata->AtomElectronegativity = *(vec+2);

      *vectele = atmdata->AtomElectronegativity;
      vectele++;
      fprintf(file,"Atom: Charge: %10f #Electrons: %10f  Screening: %10f\n      ZEff: %10f AtomElectronegativity: %10f\n",
	     atmdata->Charge,atmdata->NumberOfElectrons,atmdata->Screening,
	     atmdata->ZEff,atmdata->AtomElectronegativity);
      vec += 6;
    }
  BaseDataDoubleVector *dvec = new BaseDataDoubleVector(*vect);

  fclose(file);

  return dvec;
}
/*F obj = operator()(cls,x,y,xclass,yclass) . . . . . . . . . . BaseDataOperation
**
**  DESCRIPTION
**    x,y: The objects to be operated on
**    xclass,yclass: The object classes
**    obj: The result
**
**    This is a dummy two-dimensional call.  An empty object is created.
**
**  REMARKS
**
*/
BaseDataObject *RxnDataCalculateElectronegativity::operator()(DataObjectClass *cls,
							       BaseDataObject *x, BaseDataObject *y,
							       DataObjectClass *xclass, DataObjectClass *yclass)
                                              
{
  return operator()(x,y,xclass,yclass);
}
/*F obj = operator()(cls,x,xclass)  . . . . . . . . . . . . . . BaseDataOperation
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
BaseDataObject *RxnDataCalculateElectronegativity::operator()(DataObjectClass *cls,
							       BaseDataObject *x,
							       DataObjectClass *xclass)
{
  return operator()(x,xclass);
}
/*S RxnCalculateElectronegativityClass
 */
/*F RxnCalculateElectronegativityClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnCalculateElectronegativityClass::RxnCalculateElectronegativityClass()
{
  Identification = SECHARGE_OP_ID;
  NameTag = SECHARGE_OP_NAME;
  SubClass = "Operation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnCalculateElectronegativityClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnCalculateElectronegativityClass::RxnCalculateElectronegativityClass(const RxnCalculateElectronegativityClass& data)
  : DataOperationClass(data)
{
} 
 
/*F RxnCalculateElectronegativityClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnCalculateElectronegativityClass::RxnCalculateElectronegativityClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataOperationClass(id,name,descr)
{
  SubClass = "Operation";
  EncodeDecodeClass = SECHARGE_OP_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnCalculateElectronegativityClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnCalculateElectronegativityClass::print(ostream& out) const
{
  DataOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnCalculateElectronegativityClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnCalculateElectronegativityClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnCalculateElectronegativityClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnCalculateElectronegativityClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnCalculateElectronegativityClass::CopyClone(Identify *  objc)
{
  RxnCalculateElectronegativityClass *objcfull = (RxnCalculateElectronegativityClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnCalculateElectronegativityClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnCalculateElectronegativityClass::Clone()
    {
      RxnCalculateElectronegativityClass* id = new RxnCalculateElectronegativityClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnCalculateElectronegativityClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnCalculateElectronegativityClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnCalculateElectronegativityClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnCalculateElectronegativityClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnCalculateElectronegativityClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataCalculateElectronegativity();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnCalculateElectronegativityClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnCalculateElectronegativityClass*& obj)
     {
     obj = new RxnCalculateElectronegativityClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataCalculateElectronegativity
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataCalculateElectronegativity*& obj)
     {
     obj = new RxnDataCalculateElectronegativity;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataEletronegativityAlgorithm
 */ 
/*F RxnDataEletronegativityAlgorithm()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataEletronegativityAlgorithm::RxnDataEletronegativityAlgorithm()
  : moleculeNamesS("MoleculeNames")
{
  Identification = SECHARGE_ALG_ID;
  NameTag = SECHARGE_ALG_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataEletronegativityAlgorithm(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataEletronegativityAlgorithm::RxnDataEletronegativityAlgorithm(const RxnDataEletronegativityAlgorithm& data)
  : BaseDataAlgorithmOperation(data),
    moleculeNamesS(data.moleculeNamesS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataEletronegativityAlgorithm
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataEletronegativityAlgorithm::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataEletronegativityAlgorithm
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataEletronegativityAlgorithm::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataEletronegativityAlgorithm
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataEletronegativityAlgorithm::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataEletronegativityAlgorithm
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataEletronegativityAlgorithm::Clone()
{
  RxnDataEletronegativityAlgorithm *obj = new RxnDataEletronegativityAlgorithm(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataEletronegativityAlgorithm
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataEletronegativityAlgorithm::CopyClone(Identify * obj)
{
  RxnDataEletronegativityAlgorithm *objfull = (RxnDataEletronegativityAlgorithm *) obj;
  *this = *objfull;
  //Parameter = (RxnData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataEletronegativityAlgorithm
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataEletronegativityAlgorithm::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  //result = result && Encode(buffer,---);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataEletronegativityAlgorithm
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataEletronegativityAlgorithm::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  // The rest

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
bool RxnDataEletronegativityAlgorithm::SetUpAlgorithms(BaseDataSetOfInstances*,
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
bool RxnDataEletronegativityAlgorithm::CheckInput(BaseDataSetOfInstances*,
						   DataSetOfInstancesClass*,
						   BaseDataAlgorithmRun *run,
						   DataAlgorithmRunClass*)
{
  bool result = true;

  if(run->ParameterInList(moleculeNamesS))
    {
    }
  else
    {
      cerr << "The parameter '" << moleculeNamesS << "' was not in the list of parameters";
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
bool RxnDataEletronegativityAlgorithm::SetUpInput(BaseDataSetOfInstances* instances,
						 DataSetOfInstancesClass* instancesclass,
						 BaseDataAlgorithmRun *run,
						 DataAlgorithmRunClass* rclass)
{
  bool result = true;

  moleculeNames = (BaseDataKeyWords *) run->ParameterValue(moleculeNamesS);
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
bool RxnDataEletronegativityAlgorithm::Calculate(BaseDataSetOfInstances *instances,
						DataSetOfInstancesClass *instclass,
						BaseDataAlgorithmRun*,
						DataAlgorithmRunClass*)
{
  cout << "RxnDataEletronegativityAlgorithm::Calculate" << endl;
  bool result = true;
  RxnDataCalculateElectronegativity molele;

  ObjectList<String> names = moleculeNames->GetKeyWords();

  String molclassS  = names.front();
  names.pop_front();
  RxnSimpleMoleculeClass *molclass = (RxnSimpleMoleculeClass *) instclass->GetObjectClass(molclassS);
  String moleculename = molclass->GetNameInInstance();

  while(names.size() >= 2 && result)
    {
      String molname  = names.front();
      names.pop_front();
      String store    = names.front();
      names.pop_front();

      BaseDataInstance *instance = instances->GetInstance(molname);
      if(instance->NameTag == molname)
	{
	  if(instance->IsInList(moleculename))
	    {
	      RxnDataSimpleMolecule *molecule = (RxnDataSimpleMolecule *) instance->GetObject(moleculename);
	      BaseDataDoubleVector *vect = (BaseDataDoubleVector *) molele.operator()(molecule,molclass);
	      vect->NameTag = store;
	      instance->AddObject(vect);
	      delete vect;
	    }
	  else
	    {
	      cerr << "Object: '" << molname << "' in Instance: '" << molname << "' not found" << endl;
	      result = false;
	    }
	}
      else
	{
	  cerr << "Instance: '" << molname << "' not found" << endl;
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
bool RxnDataEletronegativityAlgorithm::WriteOutputValues(BaseDataSetOfInstances*,
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
bool RxnDataEletronegativityAlgorithm::ConcludeRun(BaseDataSetOfInstances*,
						  DataSetOfInstancesClass*,
						  BaseDataAlgorithmRun*,
						  DataAlgorithmRunClass*)
{
  bool result = true;
//  delete something
  return result;
}
/*S RxnEletronegativityAlgorithmClass
 */
/*F RxnEletronegativityAlgorithmClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnEletronegativityAlgorithmClass::RxnEletronegativityAlgorithmClass()
{
  Identification = SECHARGE_ALG_ID;
  NameTag = SECHARGE_ALG_NAME;
  SubClass = "Algorithm";
  EncodeDecodeClass = NameTag;
} 
/*F RxnEletronegativityAlgorithmClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnEletronegativityAlgorithmClass::RxnEletronegativityAlgorithmClass(const RxnEletronegativityAlgorithmClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnEletronegativityAlgorithmClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnEletronegativityAlgorithmClass::RxnEletronegativityAlgorithmClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "Algorithm";
  EncodeDecodeClass = SECHARGE_ALG_NAME;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnEletronegativityAlgorithmClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnEletronegativityAlgorithmClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnEletronegativityAlgorithmClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnEletronegativityAlgorithmClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnEletronegativityAlgorithmClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnEletronegativityAlgorithmClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnEletronegativityAlgorithmClass::CopyClone(Identify *  objc)
{
  RxnEletronegativityAlgorithmClass *objcfull = (RxnEletronegativityAlgorithmClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnEletronegativityAlgorithmClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnEletronegativityAlgorithmClass::Clone()
    {
      RxnEletronegativityAlgorithmClass* id = new RxnEletronegativityAlgorithmClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnEletronegativityAlgorithmClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnEletronegativityAlgorithmClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnEletronegativityAlgorithmClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnEletronegativityAlgorithmClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnEletronegativityAlgorithmClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataEletronegativityAlgorithm();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnEletronegativityAlgorithmClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnEletronegativityAlgorithmClass*& obj)
     {
     obj = new RxnEletronegativityAlgorithmClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataEletronegativityAlgorithm
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataEletronegativityAlgorithm*& obj)
     {
     obj = new RxnDataEletronegativityAlgorithm;
     return obj->DecodeThis(buffer);
     }
/*S Utilities
 */
/*F InitialSetOfSEChargeEncodeDecodeRoutines()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void InitialSetOfSEChargeEncodeDecodeRoutines()
{
  EncodeDecodeRegisterClass(RxnEletronegativityAlgorithmClass,RxnDataEletronegativityAlgorithm,SECHARGE_ALG_NAME);
  EncodeDecodeRegisterClass(RxnCalculateElectronegativityClass,RxnDataCalculateElectronegativity,SECHARGE_OP_NAME);
}
/*F AddSEChargeClasses(set) . . . . . . . . . . . . . . add to set of classes
**
**  DESCRIPTION
**    set: The set of classes to add to
**
**  REMARKS
**
*/
extern void AddSEChargeClasses(DataSetOfObjectsClass& set)
{
  String opdescr("The Electronegativity Operation Class");
  RxnEletronegativityAlgorithmClass opclass(SECHARGE_ALG_ID,SECHARGE_ALG_NAME,opdescr);
  set.AddObjectClass(opclass);

  String algdescr("The Electronegativity Algorithm Class");
  RxnCalculateElectronegativityClass algclass(SECHARGE_OP_ID,SECHARGE_OP_NAME,algdescr);
  set.AddObjectClass(algclass);
}
