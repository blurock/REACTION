/*  FILE     Molecule.cc
**  PACKAGE  Molecule
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "Molecule" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "FullSystem.hh"
#include "GeneralGraph.hh"
#include "Dbase.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "MolStats.hh"
#include "ThermoProps.hh"

int InputMolecule(ReactionSystemBase* sys);
int StoreMoleculeElement(ReactionSystemBase* sys);
int ReadMoleculeFromDatabase(ReactionSystemBase* sys);
int MoleculeElementExists(ReactionSystemBase* sys);
String MoleculeNamePrefix("M");

/*S RxnDataSimpleMolecule
 */ 
/*F RxnDataSimpleMolecule()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataSimpleMolecule::RxnDataSimpleMolecule()
{
  Identification = MOLECULE_SIMPLE_ID;
  NameTag = MOLECULE_SIMPLE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataSimpleMolecule(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataSimpleMolecule::RxnDataSimpleMolecule(const RxnDataSimpleMolecule& data)
  : BaseDataGraph(data),
    Properties(data.Properties),
    Atoms(data.Atoms)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataSimpleMolecule
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataSimpleMolecule
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataGraph::Read(in,objc,name);
  
  return result;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataSimpleMolecule
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataSimpleMolecule::print(ostream& out) const
{
  BaseDataGraph::print(out);
  Properties.print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataSimpleMolecule
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataSimpleMolecule::Clone()
{
  RxnDataSimpleMolecule *obj = new RxnDataSimpleMolecule(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataSimpleMolecule
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataSimpleMolecule::CopyClone(Identify * obj)
{
  RxnDataSimpleMolecule *objfull = (RxnDataSimpleMolecule *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataSimpleMolecule
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataGraph::EncodeThis(buffer);
  result = result && Properties.EncodeThis(buffer);
  int ss = Atoms.size();
  result = result && Encode(buffer,ss);
  for(vector<String>::iterator i=Atoms.begin();i<Atoms.end();i++)
    {
      result = result && Encode(buffer,*i);
    }
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataSimpleMolecule
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataGraph::DecodeThis(buffer);
  result = result && Properties.DecodeThis(buffer);
  int siz;
  result = result && Decode(buffer,siz);
  String name;
  for(int i=0;i < siz;i++)
    {
      result = result && Decode(buffer,name);
      Atoms.push_back(name);
    }
  return result;
}
/*F ReadMolFileMolecule(file,molclass,atominfo)  . . . . . . . . . . . . .  read from file
**
**  DESCRIPTION
**     file: The input stream
       molclass: The Simple Molecule class
**     atominfo: The atomic info for conversions
**
**     The MolFile has the following form:
**    - line 1        : ID number
**    - line 2        : ignored
**    - line 3        : Molecule Name
**    - line 4        : Number of Atoms and Number of Bonds (%3d%3d)
**    - line 4-nnn    : Atoms (See routine ReadMFAtom)
**    - line nnn-mmm  : Bonds (See routine ReadMFBond)
**    
**  REMARKS
**
**  REFERENCES
**
**  SEE ALSO
**
**  HEADERFILE
**
*/
bool RxnDataSimpleMolecule::ReadMolFileMolecule(istream& file,
						RxnSimpleMoleculeClass *molclass)
{
  bool result = true;
  RxnMoleculeAtomClass *atomclass = 
    (RxnMoleculeAtomClass *) molclass->NodeClass;
  RxnBasicAtomDataClass *atomdataclass = (RxnBasicAtomDataClass *)
    atomclass->getBasicAtomDataClass();

  RxnMoleculeBondClass *bondclass = 
    (RxnMoleculeBondClass *) molclass->EdgeClass;
  RxnBasicBondDataClass *bonddataclass = (RxnBasicBondDataClass *) 
    bondclass->getBasicBondDataClass();

  String line,name;
  int numatoms,numbonds;

  line.ReadFullLine(file);
  
  if(line.size() > 0 && *(line.c_str()) != 0)
    {
      Identification = line.ToInteger();
      
      line.ReadFullLine(file);
      name.ReadFullLine(file);
      
      name.EliminateLeadingBlanks();
      NameTag = name;
      
      //cout << "(" << Identification << "," << NameTag << ")" << endl;
	  
      line.ReadFullLine(file);
	  
      numatoms = line.ToInteger(0,2);
      numbonds = line.ToInteger(3,5);

      ReadInAtoms(file,numatoms,atomdataclass);
      ReadInBonds(file,numbonds,bonddataclass);
      ReadInProperties(file);
      CalculateInitialAtomProperties();
    }
  else
    {
      cout << "Done Reading" << endl;
      result = false;
    }
  return result;
}
 
/*F ans = ReadInAtoms(numatoms,atomdataclass) . . . . RxnDataSimpleMolecule:
**
**  DESCRIPTION
**    numatoms: The number of atoms to read in
**    atomdataclass: The atom class information
**    ans: true if sucessful 
**
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::ReadInAtoms(istream& file,
					int numatoms,
					RxnBasicAtomDataClass *atomdataclass)
{
  bool result = true;
  for(int count=0; count < numatoms ; count++)
    {
      RxnDataMolFileAtom *atom = (RxnDataMolFileAtom *) 
	atomdataclass->BaseDataObjectExample();
      String name("A");
      String cntS = Int2String(count);
      name.AppendToEnd(cntS);
      Atoms.push_back(name);
      atom->Read(file,atomdataclass,name);
      atom->NameTag = name;
      AddAtomToMolecule(atom);
      delete atom;
    }
  return result;
}  
 
/*F ans = ReadInBonds(numbonds,bonddataclass)
**
**  DESCRIPTION
**    numbonds: number of bonds to read in
**    bonddataclass: The bond class data
**    ans: true if successful
**
**    This reads in the bonds as specified by the bond class.
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::ReadInBonds(istream& file,
					int numbonds,
					RxnBasicBondDataClass *bonddataclass)
{
  bool result = true;
  String name("B");
  for(int count=0; count != numbonds ; count++)
    {
      RxnDataMolFileBond *bond = (RxnDataMolFileBond *) 
	bonddataclass->BaseDataObjectExample();
      bond->Read(file,bonddataclass,name);
      AddBondToMolecule(bond);
      delete bond;
    }
  return result;
}
 
/*F ans = ReadInProperties()
**
**  DESCRIPTION
**    ans: true if sucessfull
**
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::ReadInProperties(istream& file)
{
  bool result = true;
  String line;
  line.ReadFullLine(file);
  bool notdone = true;
  while(notdone) {
    if(line.IsEmpty()) {
      notdone = false;
      result = false;
    } else if(!strncmp(line.c_str(),"M  END",6)) {
      notdone = false;
    } else {
      line.ReadFullLine(file);
    }
  }
  return result;
}
 
/*F ans = CalculateInitialAtomProperties()  . . . . . . . . . RxnDataSimpleMolecule
**
**  DESCRIPTION
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::CalculateInitialAtomProperties()
{
  bool result = true;
  BaseDataSetOfObjects *atoms = getNodes();
  ObjectList<String> names = atoms->ListOfObjectNames();
  BaseDataInteger val;
  val.NameTag = STANDARD_VALENCE;
  ObjectList<String>::iterator name;
  for(name = names.begin(); name != names.end(); name++)
    {
      RxnDataMoleculeAtom *atom = (RxnDataMoleculeAtom *) getNode(*name);
      RxnDataBasicAtomData *data = atom->getBasicAtomData();
      data->CalculateSimpleElectronic();
      val.SetValue(data->Valence);
      atom->AddObject(&val);
    }
  return result;
}
/*F AddAtomToMolecule(atom)
**
**  DESCRIPTION
**    atom: The atom data to make an atom to add to graph
**
**  REMARKS
**
*/
void RxnDataSimpleMolecule::AddAtomToMolecule(RxnDataMolFileAtom *atom)
{
  RxnDataMoleculeAtom *molatom = new RxnDataMoleculeAtom(atom);
  addNode(molatom);
  delete molatom;
}
 
/*F AddAtomToMolecule(bond)
**
**  DESCRIPTION
**    bond: The bond data
**
**  REMARKS
**
*/
void RxnDataSimpleMolecule::AddBondToMolecule(RxnDataMolFileBond *bond)
{
  RxnDataMoleculeBond *molbond = new RxnDataMoleculeBond(bond);
  unsigned int bI = bond->BondI - 1;
  unsigned int bJ = bond->BondJ - 1;
  if(bI >= 0 &&
     bJ >= 0 &&
     bI < Atoms.size() &&
     bJ < Atoms.size() )
    {
      String nameI = Atoms[bI];
      String nameJ = Atoms[bJ];
      RxnDataMoleculeAtom *molatomI = (RxnDataMoleculeAtom *) 
	getNode(nameI);
      RxnDataMoleculeAtom *molatomJ = (RxnDataMoleculeAtom *) 
	getNode(nameJ);
      RxnDataBasicAtomData *dataI = molatomI->getBasicAtomData();
      RxnDataBasicAtomData *dataJ = molatomJ->getBasicAtomData();
      
      dataI->IncrementBondOrder(bond->BondOrder);
      dataJ->IncrementBondOrder(bond->BondOrder);
      
      String name(nameI);
      String delim("#");
      name.AppendToEnd(delim);
      name.AppendToEnd(nameJ);
      molbond->NameTag = name;
      molbond->setNodes(nameI,nameJ);
      
      addEdge(molbond);
      delete molbond;
    }
  else
    {
      cerr << "Illegal Atom specification for bond [";
      cerr << bond->BondI << "," << bond->BondJ << "]" << endl;
      cerr << "Bond not added" << endl;
    }
}
bool RxnDataSimpleMolecule::WriteAsLine(ostream& out, DataObjectClass *objc)
{
  out << "Molecule: " << NameTag << " of type " << objc->NameTag << endl;
  return true;
}
bool RxnDataSimpleMolecule::WriteAsASCII(ostream& out, DataObjectClass *objc)
{
  BaseDataGraph::WriteAsASCII(out,objc);
  Properties.WriteAsASCII(out,objc);
  return true;
}
bool RxnDataSimpleMolecule::WriteAsLatex(ostream& out, DataObjectClass* objc)
{
  bool result = true;
  RxnSimpleMoleculeClass *molclass = (RxnSimpleMoleculeClass *) objc;
  DataSetOfObjectsClass *objclass = molclass->PointerToAllowedClasses();


  BaseDataObject *obj;


  out << "\\begin{center}" << endl;
  out << "\{\\large \bf " << NameTag << "}" << endl;
  out << "\\end{center}" << endl;
  
  out << "The set of Atoms:" << endl;

  BaseDataSetOfObjects *nodes = getNodes();
  ObjectList<String> names = nodes->ListOfObjectNames();
  ObjectList<String>::iterator name = names.begin();
  obj = nodes->GetObject(*name);
  RxnBasicAtomDataClass *cls = (RxnBasicAtomDataClass *)objclass->GetObjectClass(obj->GetType());
  cls->PrintLaTeXTablePrefix(out);
  nodes->WriteAsLatex(out,objclass);
  cls->PrintLaTeXTablePostfix(out);

  out << endl << "The set of Edges" << endl;

  BaseDataSetOfObjects *edges = getEdges();
  ObjectList<String> enames = edges->ListOfObjectNames();
  ObjectList<String>::iterator ename = enames.begin();
  obj = nodes->GetObject(*ename);
  RxnBasicBondDataClass *ecls = (RxnBasicBondDataClass *) objclass->GetObjectClass(obj->GetType());
  ecls->PrintLaTeXTablePrefix(out);
  edges->WriteAsLatex(out,objclass);
  ecls->PrintLaTeXTablePostfix(out);

  /*
  RxnBasicAtomDataClass *cls;
  BaseDataSetOfObjects *nodes = getNodes();
  for(name=names.begin(); name != names.end() && result ; name++)
    {
      obj = nodes->GetObject(*name);
      cls = (RxnBasicAtomDataClass *)objclass->GetObjectClass(obj->GetType());
      if(name == names.begin())
	cls->PrintLaTeXTablePrefix(out);
      result = result && obj->WriteAsLatex(out,cls);
      out << " \\\\" << endl;
    }
  if(names.begin() != names.end())
    cls->PrintLaTeXTablePostfix(out);
  */
  return result;
}
 
/*F ans = StoreProperty(obj)
**
**  DESCRIPTION
**    obj: The property value to store
**    ans: True if successfull
**
**  REMARKS
**
*/
bool RxnDataSimpleMolecule::StoreProperty(BaseDataObject *obj)
{
  return Properties.AddObject(obj);
}
 
/*F obj = RetrieveProperty(name)
**
**  DESCRIPTION
**    name: The name of the property to retrieve
**    obj: The object (NULL if not found)
**
**  REMARKS
**
*/
BaseDataObject *RxnDataSimpleMolecule::RetrieveProperty(String& name)
{
  BaseDataObject *obj = NULL;
  if(Properties.IsInList(name))
    {
      obj = Properties.GetObject(name);
    }
  return obj;
}
/*S RxnSimpleMoleculeClass
 */
/*F RxnSimpleMoleculeClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnSimpleMoleculeClass::RxnSimpleMoleculeClass()
  : NameInInstance(NAME_IN_INSTANCE)
{
  Identification = MOLECULE_SIMPLE_ID;
  NameTag = MOLECULE_SIMPLE_NAME;
  SubClass = "Graph";
  EncodeDecodeClass = NameTag;
} 
/*F RxnSimpleMoleculeClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnSimpleMoleculeClass::RxnSimpleMoleculeClass(const RxnSimpleMoleculeClass& data)
  : DataGraphClass(data),
    NameInInstance(data.NameInInstance)
{
} 
 
/*F RxnSimpleMoleculeClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnSimpleMoleculeClass::RxnSimpleMoleculeClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataGraphClass(id,name,descr),
    NameInInstance(NAME_IN_INSTANCE)
{
  SubClass = "Graph";
  EncodeDecodeClass = "SimpleMolecule";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnSimpleMoleculeClass::print(ostream& out) const
{
  DataGraphClass::print(out);
  out << endl;
  out << "NameInInstance: " << NameInInstance << endl;
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnSimpleMoleculeClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnSimpleMoleculeClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataGraphClass::Read(in,set);
  
  String keyw("NameInInstance:");
  if(result && CheckReadKeyWord(in,keyw))
    {
      StreamObjectInput str(in,' ');
      NameInInstance = str.ReadNext();
    }
  else
    result = false;
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnSimpleMoleculeClass::CopyClone(Identify *  objc)
{
  RxnSimpleMoleculeClass *objcfull = (RxnSimpleMoleculeClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnSimpleMoleculeClass::Clone()
    {
      RxnSimpleMoleculeClass* id = new RxnSimpleMoleculeClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnSimpleMoleculeClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataGraphClass::EncodeThis(buffer);
  result = result && Encode(buffer,NameInInstance);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnSimpleMoleculeClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataGraphClass::DecodeThis(buffer);
  result = result && Decode(buffer,NameInInstance);
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
BaseDataObject * RxnSimpleMoleculeClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataSimpleMolecule();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnSimpleMoleculeClass*& obj)
     {
     obj = new RxnSimpleMoleculeClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataSimpleMolecule
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataSimpleMolecule*& obj)
     {
     obj = new RxnDataSimpleMolecule;
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
DataSetOfObjectsClass *RxnSimpleMoleculeClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}
 
/*F SetNameInInstance(name) . . . . . . . . . . . . .  RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    name: Set the name of the molecule in an instance to this name
**
**  REMARKS
**
*/
void RxnSimpleMoleculeClass::SetNameInInstance(String& name)
{
  NameInInstance = name;
}
 
/*F name = GetNameInInstance()  . . . . . . . . . . .  RxnSimpleMoleculeClass
**
**  DESCRIPTION
**    name: The name of the molecule in the instance
**
**  REMARKS
**
*/
String& RxnSimpleMoleculeClass::GetNameInInstance()
{
  return NameInInstance;
}

/*S RxnDataMoleculeSet
 */ 
/*F RxnDataMoleculeSet()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeSet::RxnDataMoleculeSet()
  : MolCount(0)
{
  Identification = MOLECULE_MOLSET_ID;
  NameTag = MOLECULE_MOLSET_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMoleculeSet(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMoleculeSet::RxnDataMoleculeSet(const RxnDataMoleculeSet& data)
  : BaseDataSetOfObjects(data),
    MoleculeNames(data.MoleculeNames),
    InternalMoleculeNames(data.InternalMoleculeNames),
    MolCount(data.MolCount)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMoleculeSet
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMoleculeSet::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMoleculeSet
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMoleculeSet::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataSetOfObjects::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMoleculeSet
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMoleculeSet::print(ostream& out) const
{
  BaseDataSetOfObjects::print(out);
  out << MolCount << " Molecules" << endl;
  out << "Molecule Names: ";
  MoleculeNames.print(out);
  out << "Internal Names: ";
  InternalMoleculeNames.print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeSet
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMoleculeSet::Clone()
{
  RxnDataMoleculeSet *obj = new RxnDataMoleculeSet(*this);
  return obj;
}
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeSet
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMoleculeSet::CopyClone(Identify * obj)
{
  RxnDataMoleculeSet *objfull = (RxnDataMoleculeSet *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeSet
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeSet::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::EncodeThis(buffer);
  result = result && MoleculeNames.EncodeThis(buffer);
  result = result && InternalMoleculeNames.EncodeThis(buffer);
  result = result && Encode(buffer,MolCount);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeSet
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeSet::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataSetOfObjects::DecodeThis(buffer);
  result = result && MoleculeNames.DecodeThis(buffer);
  result = result && InternalMoleculeNames.DecodeThis(buffer);
  result = result && Decode(buffer,MolCount);
  return result;
}
 
/*F names = getMoleculeNames()  . . . . . . . . . . . . .  RxnDataMoleculeSet
**
**  DESCRIPTION
**    names: Real names of molecules
**
**  REMARKS
**
*/
BaseDataKeySet& RxnDataMoleculeSet::getMoleculeNames()
{
  return MoleculeNames;
}
 
/*F names = getInternalMoleculeNames()  . . . . . . . . .  RxnDataMoleculeSet
**
**  DESCRIPTION
**    names: The internal names of the molecules (This is for
**           use when the molecules are going to have
**           additional information in other structures.
**           The name could also refer to a temporary molecule.
**           Is used for molecules in reactions.
**
**  REMARKS
**
*/
BaseDataKeySet& RxnDataMoleculeSet::getInternalMoleculeNames()
{
  return InternalMoleculeNames;
}
 
/*F name MoleculeNameFromInternalName(searchname) . . . .  RxnDataMoleculeSet
**
**  DESCRIPTION
**    searchname: The name of the internal name
**    name: The name of the 'real' molecule in the set
**
**  REMARKS
**
*/
String RxnDataMoleculeSet::MoleculeNameFromInternalName(String& searchname)
{
  ObjectList<String> molnames = MoleculeNames.GetKeyWords();
  ObjectList<String> internalnames = InternalMoleculeNames.GetKeyWords();
  ObjectList<String>::iterator molname = molnames.begin();
  String name("");
  bool notdone = true;
  for(ObjectList<String>::iterator intname = internalnames.begin();
      notdone && intname != internalnames.end();
      intname++)
    {
      if(searchname == *intname)
	{
	  name = *molname;
	  notdone = false;
	}
      molname++;
    }
  return name;
}
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
String RxnDataMoleculeSet::GenerateNextName(unsigned int places)
{
  return PositveIntegerToString(MolCount,MoleculeNamePrefix,places);
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
bool RxnDataMoleculeSet::getMolecules(BaseDataSetOfObjects *set,
				      String& dbasetype,
				      RxnDataMolecularStructuresDataBase *dbase,
				      RxnMolecularStructuresDataBaseClass *dbaseclass)
{
  bool result = true;
  BaseDataDataBaseInformation *db = dbase->getDatabaseInfo(dbasetype);
  DataDataBaseInformationClass *dbclass = dbaseclass->getMoleculeDBClass();
  if(db != NULL && dbclass != NULL)
    {
      BaseDataObject *obj = db->getDataElementClass(dbclass);
      ObjectList<String> names = MoleculeNames.GetKeyWords();
      ObjectList<String>::iterator name;
      for(name = names.begin(); name != names.end(); name++)
	{
	  result = result && db->FetchElement(*name,dbclass,obj);
	  set->AddObject(obj);
	}
    }
  else
    {
      cerr << "Molecule Database Type not found '" << dbasetype << endl;
      result = false;
    }
  return result;
}
 
/*F ans = AddMolecule(name)
**
**  DESCRIPTION
**    name: The molecule name
**    ans: true if successful
**
**  REMARKS
**
*/
String RxnDataMoleculeSet::AddMolecule(String& name)
{
  MoleculeNames.AddKeyWord(name);
  String internalname = GenerateNextName(3);
  InternalMoleculeNames.AddKeyWord(internalname);
  MolCount++;
  return internalname;
}
 
/*F count = getMoleculeCount()  . . . . . . . . . . . . . . RxnDataMoleculeSe
**
**  DESCRIPTION
**    count: The number of molecules in the set (including duplicates)
**
**  REMARKS
**
*/
unsigned int RxnDataMoleculeSet::getMoleculeCount()
{
  return MolCount;
}
/*F contained = ContainedIn(moleculelist)  . . . . . . . . . . . . . . RxnDataMoleculeSe
**
**  DESCRIPTION
**    moleculelist: The list of molelcule names
**    contained: true if all the molecules are in the molecule list
**
**    This is respect to full molecule names (MoleculeNames)
**
**  REMARKS
**
*/
bool RxnDataMoleculeSet::ContainedIn(BaseDataKeyWords& moleculelist) {
  return MoleculeNames.ContainedIn(moleculelist);
}
/*S RxnMoleculeSetClass
 */
/*F RxnMoleculeSetClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMoleculeSetClass::RxnMoleculeSetClass()
{
  Identification = MOLECULE_MOLSET_ID;
  NameTag = MOLECULE_MOLSET_NAME;
  SubClass = "SetOfObjects";
  EncodeDecodeClass = NameTag;
  ReadClassPairs = false;
  ReadAllowedClasses = false;
} 
/*F RxnMoleculeSetClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMoleculeSetClass::RxnMoleculeSetClass(const RxnMoleculeSetClass& data)
  : DataSetOfObjectsClass(data)
{
} 
 
/*F RxnMoleculeSetClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMoleculeSetClass::RxnMoleculeSetClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataSetOfObjectsClass(id,name,descr)
{
  SubClass = "SetOfObjects";
  EncodeDecodeClass = "MoleculeSet";
  ReadClassPairs = false;
  ReadAllowedClasses = false;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMoleculeSetClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMoleculeSetClass::print(ostream& out) const
{
  DataSetOfObjectsClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSetClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMoleculeSetClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMoleculeSetClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataSetOfObjectsClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSetClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMoleculeSetClass::CopyClone(Identify *  objc)
{
  RxnMoleculeSetClass *objcfull = (RxnMoleculeSetClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeSetClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMoleculeSetClass::Clone()
    {
      RxnMoleculeSetClass* id = new RxnMoleculeSetClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeSetClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeSetClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeSetClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeSetClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataSetOfObjectsClass::DecodeThis(buffer);
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
BaseDataObject * RxnMoleculeSetClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMoleculeSet();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMoleculeSetClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMoleculeSetClass*& obj)
     {
     obj = new RxnMoleculeSetClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMoleculeSet
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMoleculeSet*& obj)
     {
     obj = new RxnDataMoleculeSet;
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
DataSetOfObjectsClass *RxnMoleculeSetClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}
/*S BaseDataMoleculeEqualValuePredicate
 */ 
/*F BaseDataMoleculeEqualValuePredicate()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataMoleculeEqualValuePredicate::BaseDataMoleculeEqualValuePredicate()
{
  Identification = MOLECULE_PREDICATE_ID;
  NameTag = MOLECULE_PREDICATE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F BaseDataMoleculeEqualValuePredicate(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
BaseDataMoleculeEqualValuePredicate::BaseDataMoleculeEqualValuePredicate(const BaseDataMoleculeEqualValuePredicate& data)
  : BaseDataExactlyEqualPredicate(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in BaseDataMoleculeEqualValuePredicate
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool BaseDataMoleculeEqualValuePredicate::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in BaseDataMoleculeEqualValuePredicate
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool BaseDataMoleculeEqualValuePredicate::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataExactlyEqualPredicate::Read(in,objc,name);  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  BaseDataMoleculeEqualValuePredicate
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& BaseDataMoleculeEqualValuePredicate::print(ostream& out) const
{
  BaseDataExactlyEqualPredicate::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .BaseDataMoleculeEqualValuePredicate
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * BaseDataMoleculeEqualValuePredicate::Clone()
{
  BaseDataMoleculeEqualValuePredicate *obj = new BaseDataMoleculeEqualValuePredicate(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .BaseDataMoleculeEqualValuePredicate
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void BaseDataMoleculeEqualValuePredicate::CopyClone(Identify * obj)
{
  BaseDataMoleculeEqualValuePredicate *objfull = (BaseDataMoleculeEqualValuePredicate *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataMoleculeEqualValuePredicate
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataMoleculeEqualValuePredicate::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataExactlyEqualPredicate::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataMoleculeEqualValuePredicate
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataMoleculeEqualValuePredicate::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataExactlyEqualPredicate::DecodeThis(buffer);
  return result;
}
/*F obj = operator()(x,y,xclass,yclass) . . . . . . . . . . BaseDataMoleculeEqualValuePredicate
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
BaseDataObject *BaseDataMoleculeEqualValuePredicate::operator()(BaseDataObject *x, BaseDataObject *y,
						      DataObjectClass *xc, DataObjectClass *yc)
{
  return operator()(x,xc);
}
/*F obj = operator()(x,xclass)  . . . . . . . . . . . . . . BaseDataMoleculeEqualValuePredicate
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
BaseDataObject *BaseDataMoleculeEqualValuePredicate::operator()(BaseDataObject *x,
						      DataObjectClass *xc)
{
  RxnDataSimpleMolecule *molecule = (RxnDataSimpleMolecule *) x;
  RxnSimpleMoleculeClass *moleculeclass = (RxnSimpleMoleculeClass *) xc;
  BaseDataObject *obj = molecule->RetrieveProperty(getParameterName());
  DataObjectClass *objclass = moleculeclass->PointerToAllowedClasses()->GetObjectClass(obj->GetType());
  return BaseDataExactlyEqualPredicate::operator()(obj,objclass);
}
/*F obj = operator()(cls,x,y,xclass,yclass) . . . . . . . . . . BaseDataMoleculeEqualValuePredicate
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
BaseDataObject *BaseDataMoleculeEqualValuePredicate::operator()(DataObjectClass *cls,
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
/*F obj = operator()(cls,x,xclass)  . . . . . . . . . . . . . . BaseDataMoleculeEqualValuePredicate
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
BaseDataObject *BaseDataMoleculeEqualValuePredicate::operator()(DataObjectClass *cls,
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
/*S DataMoleculeEqualValuePredicateClass
 */
/*F DataMoleculeEqualValuePredicateClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
DataMoleculeEqualValuePredicateClass::DataMoleculeEqualValuePredicateClass()
{
  Identification = MOLECULE_PREDICATE_ID;
  NameTag = MOLECULE_PREDICATE_NAME;
  SubClass = "ExactlyEqualPredicate";
  EncodeDecodeClass = NameTag;
} 
/*F DataMoleculeEqualValuePredicateClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
DataMoleculeEqualValuePredicateClass::DataMoleculeEqualValuePredicateClass(const DataMoleculeEqualValuePredicateClass& data)
  : DataExactlyEqualPredicateClass(data)
{
}  
/*F DataMoleculeEqualValuePredicateClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
DataMoleculeEqualValuePredicateClass::DataMoleculeEqualValuePredicateClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataExactlyEqualPredicateClass(id,name,descr)
{
  SubClass = "ExactlyEqualPredicate";
  EncodeDecodeClass = "MoleculeEqualValuePredicate";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . DataMoleculeEqualValuePredicateClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& DataMoleculeEqualValuePredicateClass::print(ostream& out) const
{
  DataExactlyEqualPredicateClass::print(out);
  return out;
}
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . DataMoleculeEqualValuePredicateClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base DataMoleculeEqualValuePredicateClass, there is no further information.
**
**  REMARKS
**
*/
bool DataMoleculeEqualValuePredicateClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataExactlyEqualPredicateClass::Read(in,set);
  return result;
} 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . DataMoleculeEqualValuePredicateClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void DataMoleculeEqualValuePredicateClass::CopyClone(Identify *  objc)
{
  DataMoleculeEqualValuePredicateClass *objcfull = (DataMoleculeEqualValuePredicateClass *) objc;
  *this = *objcfull;
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . DataMoleculeEqualValuePredicateClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * DataMoleculeEqualValuePredicateClass::Clone()
    {
      DataMoleculeEqualValuePredicateClass* id = new DataMoleculeEqualValuePredicateClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . DataMoleculeEqualValuePredicateClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataMoleculeEqualValuePredicateClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataExactlyEqualPredicateClass::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . DataMoleculeEqualValuePredicateClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataMoleculeEqualValuePredicateClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataExactlyEqualPredicateClass::DecodeThis(buffer);
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
BaseDataObject * DataMoleculeEqualValuePredicateClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new BaseDataMoleculeEqualValuePredicate();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . DataMoleculeEqualValuePredicateClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, DataMoleculeEqualValuePredicateClass*& obj)
     {
     obj = new DataMoleculeEqualValuePredicateClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . BaseDataMoleculeEqualValuePredicate
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, BaseDataMoleculeEqualValuePredicate*& obj)
     {
     obj = new BaseDataMoleculeEqualValuePredicate;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataRetrieveMoleculeProperty
 */ 
/*F RxnDataRetrieveMoleculeProperty()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataRetrieveMoleculeProperty::RxnDataRetrieveMoleculeProperty()
{
  Identification = MOLECULE_RETRIEVE_ID;
  NameTag = MOLECULE_RETRIEVE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataRetrieveMoleculeProperty(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataRetrieveMoleculeProperty::RxnDataRetrieveMoleculeProperty(const RxnDataRetrieveMoleculeProperty& data)
  : BaseDataOperation(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataRetrieveMoleculeProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataRetrieveMoleculeProperty::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataRetrieveMoleculeProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataRetrieveMoleculeProperty::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataOperation::Read(in,objc,name);  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataRetrieveMoleculeProperty
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataRetrieveMoleculeProperty::print(ostream& out) const
{
  BaseDataOperation::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataRetrieveMoleculeProperty
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataRetrieveMoleculeProperty::Clone()
{
  RxnDataRetrieveMoleculeProperty *obj = new RxnDataRetrieveMoleculeProperty(*this);
  return obj;
} 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataRetrieveMoleculeProperty
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataRetrieveMoleculeProperty::CopyClone(Identify * obj)
{
  RxnDataRetrieveMoleculeProperty *objfull = (RxnDataRetrieveMoleculeProperty *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataRetrieveMoleculeProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataRetrieveMoleculeProperty::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataRetrieveMoleculeProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataRetrieveMoleculeProperty::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::DecodeThis(buffer);
  return result;
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
BaseDataObject *RxnDataRetrieveMoleculeProperty::operator()(BaseDataObject *x, BaseDataObject *y,
							   DataObjectClass *xc, DataObjectClass *yc)
{
  BaseDataString *str = (BaseDataString *) x;
  RxnDataSimpleMolecule *mol = (RxnDataSimpleMolecule *) y;
  BaseDataObject *obj;

  obj = mol->RetrieveProperty(str->getString());
  if(obj == NULL)
    obj = x;
  
  return (BaseDataObject *) obj->Clone();
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
BaseDataObject *RxnDataRetrieveMoleculeProperty::operator()(BaseDataObject *x,
							   DataObjectClass *xc)
{
  return (BaseDataObject *) x->Clone();
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
BaseDataObject *RxnDataRetrieveMoleculeProperty::operator()(DataObjectClass *cls,
						  BaseDataObject *x, BaseDataObject *y,
						  DataObjectClass *xc, DataObjectClass *yc)
                                              
{
  RxnRetrieveMoleculePropertyClass *rxncls = (RxnRetrieveMoleculePropertyClass *) cls;
  DataSetOfObjectsClass *set = rxncls->PointerToAllowedClasses();
  bool result = true;
  result = result && set->IsOfClass(*xc,DATAOBJ_STRING_NAME);
  result = result && set->IsOfClass(*yc,MOLECULE_SIMPLE_NAME);
  result = result && (x->GetType() == xc->GetType());
  result = result && (y->GetType() == yc->GetType());
  return operator()(x,y,xc,yc);
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
BaseDataObject *RxnDataRetrieveMoleculeProperty::operator()(DataObjectClass *cls,
						  BaseDataObject *x,
						  DataObjectClass *xclass)
{
  RxnRetrieveMoleculePropertyClass *rxncls = (RxnRetrieveMoleculePropertyClass *) cls;
  DataSetOfObjectsClass *set = rxncls->PointerToAllowedClasses();

  bool result = true;
  result = result && set->IsOfClass(*xclass,DATAOBJ_STRING_NAME);
  result = result && (x->GetType() == xclass->GetType());
  return operator()(x,xclass);
}
 
 
/*S RxnRetrieveMoleculePropertyClass
 */
/*F RxnRetrieveMoleculePropertyClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnRetrieveMoleculePropertyClass::RxnRetrieveMoleculePropertyClass()
{
  Identification = MOLECULE_RETRIEVE_ID;
  NameTag = MOLECULE_RETRIEVE_NAME;
  SubClass = "Operation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnRetrieveMoleculePropertyClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnRetrieveMoleculePropertyClass::RxnRetrieveMoleculePropertyClass(const RxnRetrieveMoleculePropertyClass& data)
  : DataOperationClass(data)
{
} 
 
/*F RxnRetrieveMoleculePropertyClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnRetrieveMoleculePropertyClass::RxnRetrieveMoleculePropertyClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataOperationClass(id,name,descr)
{
  SubClass = "Operation";
  EncodeDecodeClass = "RetrieveMoleculeProperty";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnRetrieveMoleculePropertyClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnRetrieveMoleculePropertyClass::print(ostream& out) const
{
  DataOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnRetrieveMoleculePropertyClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnRetrieveMoleculePropertyClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnRetrieveMoleculePropertyClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnRetrieveMoleculePropertyClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnRetrieveMoleculePropertyClass::CopyClone(Identify *  objc)
{
  RxnRetrieveMoleculePropertyClass *objcfull = (RxnRetrieveMoleculePropertyClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnRetrieveMoleculePropertyClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnRetrieveMoleculePropertyClass::Clone()
    {
      RxnRetrieveMoleculePropertyClass* id = new RxnRetrieveMoleculePropertyClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnRetrieveMoleculePropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnRetrieveMoleculePropertyClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnRetrieveMoleculePropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnRetrieveMoleculePropertyClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnRetrieveMoleculePropertyClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataRetrieveMoleculeProperty();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnRetrieveMoleculePropertyClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnRetrieveMoleculePropertyClass*& obj)
     {
     obj = new RxnRetrieveMoleculePropertyClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataRetrieveMoleculeProperty
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataRetrieveMoleculeProperty*& obj)
     {
     obj = new RxnDataRetrieveMoleculeProperty;
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
DataSetOfObjectsClass *RxnRetrieveMoleculePropertyClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}
/*S RxnDataStoreMoleculeProperty
 */ 
/*F RxnDataStoreMoleculeProperty()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataStoreMoleculeProperty::RxnDataStoreMoleculeProperty()
{
  Identification = MOLECULE_STORE_ID;
  NameTag = MOLECULE_STORE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataStoreMoleculeProperty(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataStoreMoleculeProperty::RxnDataStoreMoleculeProperty(const RxnDataStoreMoleculeProperty& data)
  : BaseDataOperation(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataStoreMoleculeProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataStoreMoleculeProperty::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataStoreMoleculeProperty
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataStoreMoleculeProperty::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataOperation::Read(in,objc,name);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataStoreMoleculeProperty
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataStoreMoleculeProperty::print(ostream& out) const
{
  BaseDataOperation::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataStoreMoleculeProperty
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataStoreMoleculeProperty::Clone()
{
  RxnDataStoreMoleculeProperty *obj = new RxnDataStoreMoleculeProperty(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataStoreMoleculeProperty
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataStoreMoleculeProperty::CopyClone(Identify * obj)
{
  RxnDataStoreMoleculeProperty *objfull = (RxnDataStoreMoleculeProperty *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataStoreMoleculeProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataStoreMoleculeProperty::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataStoreMoleculeProperty
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataStoreMoleculeProperty::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataOperation::DecodeThis(buffer);
  return result;
}
/*F obj = operator()(x,y,xclass,yclass) . . . . . . . . . . BaseDataStoreMoleculeProperty
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
BaseDataObject *RxnDataStoreMoleculeProperty::operator()(BaseDataObject *x, BaseDataObject *y,
							 DataObjectClass *xc, DataObjectClass *yc)
{
  RxnDataSimpleMolecule *mol = (RxnDataSimpleMolecule *) y;
  mol->StoreProperty(x);
  return (BaseDataObject *) x->Clone();
}
/*F obj = operator()(x,xclass)  . . . . . . . . . . . . . . BaseDataStoreMoleculeProperty
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
BaseDataObject *RxnDataStoreMoleculeProperty::operator()(BaseDataObject *x,
							 DataObjectClass *xc)
{
  return (BaseDataObject *) x->Clone();
}
/*F obj = operator()(cls,x,y,xclass,yclass) . . . . . . . . . . BaseDataStoreMoleculeProperty
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
BaseDataObject *RxnDataStoreMoleculeProperty::operator()(DataObjectClass *cls,
							 BaseDataObject *x, BaseDataObject *y,
							 DataObjectClass *xc, DataObjectClass *yc)
                                              
{
  RxnRetrieveMoleculePropertyClass *rxncls = (RxnRetrieveMoleculePropertyClass *) cls;
  DataSetOfObjectsClass *set = rxncls->PointerToAllowedClasses();
  bool result = true;
  result = result && set->IsOfClass(*yc,MOLECULE_SIMPLE_NAME);
  result = result && (x->GetType() == xc->GetType());
  result = result && (y->GetType() == yc->GetType());
  return operator()(x,y,xc,yc);
}
/*F obj = operator()(cls,x,xclass)  . . . . . . . . . . . . . . BaseDataStoreMoleculeProperty
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
BaseDataObject *RxnDataStoreMoleculeProperty::operator()(DataObjectClass *cls,
						  BaseDataObject *x,
						  DataObjectClass *xclass)
{
  return operator()(x,xclass);
}
/*S RxnStoreMoleculePropertyClass
 */
/*F RxnStoreMoleculePropertyClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnStoreMoleculePropertyClass::RxnStoreMoleculePropertyClass()
{
  Identification = MOLECULE_STORE_ID;
  NameTag = MOLECULE_STORE_NAME;
  SubClass = "Operation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnStoreMoleculePropertyClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnStoreMoleculePropertyClass::RxnStoreMoleculePropertyClass(const RxnStoreMoleculePropertyClass& data)
  : DataOperationClass(data)
{
} 
 
/*F RxnStoreMoleculePropertyClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnStoreMoleculePropertyClass::RxnStoreMoleculePropertyClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataOperationClass(id,name,descr)
{
  SubClass = "Operation";
  EncodeDecodeClass = "StoreMoleculeProperty";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnStoreMoleculePropertyClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnStoreMoleculePropertyClass::print(ostream& out) const
{
  DataOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnStoreMoleculePropertyClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnStoreMoleculePropertyClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnStoreMoleculePropertyClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnStoreMoleculePropertyClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnStoreMoleculePropertyClass::CopyClone(Identify *  objc)
{
  RxnStoreMoleculePropertyClass *objcfull = (RxnStoreMoleculePropertyClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnStoreMoleculePropertyClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnStoreMoleculePropertyClass::Clone()
    {
      RxnStoreMoleculePropertyClass* id = new RxnStoreMoleculePropertyClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnStoreMoleculePropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnStoreMoleculePropertyClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnStoreMoleculePropertyClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnStoreMoleculePropertyClass::DecodeThis(CommBuffer& buffer)
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
BaseDataObject * RxnStoreMoleculePropertyClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataStoreMoleculeProperty();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnStoreMoleculePropertyClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnStoreMoleculePropertyClass*& obj)
     {
     obj = new RxnStoreMoleculePropertyClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataStoreMoleculeProperty
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataStoreMoleculeProperty*& obj)
     {
     obj = new RxnDataStoreMoleculeProperty;
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
DataSetOfObjectsClass *RxnStoreMoleculePropertyClass::PointerToAllowedClasses()
{
  return StandardAllowedClasses;
}

/*S RxnDataMolecularStructuresDataBase
 */ 
/*F RxnDataMolecularStructuresDataBase()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMolecularStructuresDataBase::RxnDataMolecularStructuresDataBase()
  : DBMolecule(NULL),
    DBSubStructures(NULL),
    DBMoleculeClasses(NULL)
{
  Identification = MOLECULE_DBASE_ID;
  NameTag = MOLECULE_DBASE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMolecularStructuresDataBase(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMolecularStructuresDataBase::RxnDataMolecularStructuresDataBase(const RxnDataMolecularStructuresDataBase& data)
  : BaseDataObject(data)
{
  DBMolecule        = (BaseDataDataBaseInformation *) PointerClone(data.DBMolecule);
  DBSubStructures   = (BaseDataDataBaseInformation *) PointerClone(data.DBSubStructures);
  DBMoleculeClasses = (BaseDataDataBaseInformation *) PointerClone(data.DBMoleculeClasses);
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMolecularStructuresDataBase::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMolecularStructuresDataBase::Read(istream& in, DataObjectClass* objc, const String& name)
{
  RxnMolecularStructuresDataBaseClass *dbclass = (RxnMolecularStructuresDataBaseClass *) objc;
  bool result = BaseDataObject::Read(in,objc,name);
  String notdefined("Not Defined");
  result = result && PointerObjectRead(in,(BaseDataObject *&) DBMolecule,dbclass->getMoleculeDBClass(),notdefined);
  result = result && PointerObjectRead(in,(BaseDataObject *&) DBSubStructures,dbclass->getMoleculeDBClass(),notdefined);
  result = result && PointerObjectRead(in,(BaseDataObject *&) DBMoleculeClasses,dbclass->getMoleculeDBClass(),notdefined);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMolecularStructuresDataBase::print(ostream& out) const
{
  BaseDataObject::print(out);
  PointerPrint(out,"The Molecule Database: ",
	       "No Database defined",DBMolecule);
  PointerPrint(out,"The SubStructures Database: ",
	       "No Database defined",DBSubStructures);
  PointerPrint(out,"The MoleculeClasses Database: ",
	       "No Database defined",DBMoleculeClasses);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMolecularStructuresDataBase::Clone()
{
  RxnDataMolecularStructuresDataBase *obj = new RxnDataMolecularStructuresDataBase(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMolecularStructuresDataBase::CopyClone(Identify * obj)
{
  RxnDataMolecularStructuresDataBase *objfull = (RxnDataMolecularStructuresDataBase *) obj;
  *this = *objfull;
  DBMolecule        = (BaseDataDataBaseInformation *) PointerClone(objfull->DBMolecule);
  DBSubStructures   = (BaseDataDataBaseInformation *) PointerClone(objfull->DBSubStructures);
  DBMoleculeClasses = (BaseDataDataBaseInformation *) PointerClone(objfull->DBMoleculeClasses);
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMolecularStructuresDataBase::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::EncodeThis(buffer);
  result = result && PointerEncode(buffer,DBMolecule);
  result = result && PointerEncode(buffer,DBSubStructures);
  result = result && PointerEncode(buffer,DBMoleculeClasses);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMolecularStructuresDataBase::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) DBMolecule);
  result = result && PointerDecode(buffer,(BaseDataObject *&) DBSubStructures);
  result = result && PointerDecode(buffer,(BaseDataObject *&) DBMoleculeClasses);
  return result;
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataMolecularStructuresDataBase::Initialize(DataDataBaseInformationClass *dbclass)
{
  bool result = true;
  if( DBMolecule != NULL &&
      DBSubStructures != NULL &&
      DBMoleculeClasses != NULL)
    {
      DBMolecule->OpenUpDataBase(dbclass);
      DBSubStructures->OpenUpDataBase(dbclass);
      DBMoleculeClasses->OpenUpDataBase(dbclass);
    }
  else
    {
      cerr << "Databases not defined yet" << endl;
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
BaseDataDataBaseInformation *RxnDataMolecularStructuresDataBase::getDatabaseInfo(String& moltype)
{
  BaseDataDataBaseInformation *dbaseinfo = NULL;
  if(moltype == "Molecule")
    dbaseinfo = DBMolecule;
  else if(moltype == "Substructure")
    dbaseinfo = DBSubStructures;
  else if(moltype == "MoleculeClass")
    dbaseinfo = DBMoleculeClasses;
  return dbaseinfo;
}

/*S RxnMolecularStructuresDataBaseClass
 */
/*F RxnMolecularStructuresDataBaseClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMolecularStructuresDataBaseClass::RxnMolecularStructuresDataBaseClass()
  : MoleculeDBClass(NULL)
{
  Identification = MOLECULE_DBASE_ID;
  NameTag = MOLECULE_DBASE_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMolecularStructuresDataBaseClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMolecularStructuresDataBaseClass::RxnMolecularStructuresDataBaseClass(const RxnMolecularStructuresDataBaseClass& data)
  : DataObjectClass(data)
{
  MoleculeDBClass = (DataDataBaseInformationClass *) PointerClone(data.MoleculeDBClass);
} 
/*F RxnMolecularStructuresDataBaseClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
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
RxnMolecularStructuresDataBaseClass::RxnMolecularStructuresDataBaseClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataObjectClass(id,name,descr),
    MoleculeDBClass(NULL)
{
  SubClass = "Object";
  EncodeDecodeClass = "MolecularStructuresDataBase";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMolecularStructuresDataBaseClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMolecularStructuresDataBaseClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  PointerPrint(out,"  The Database Class: "," No Class Defined ",MoleculeDBClass);
  return out;
}
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMolecularStructuresDataBaseClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMolecularStructuresDataBaseClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMolecularStructuresDataBaseClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataObjectClass::Read(in,set);
  result = result && PointerClassRead(in,(DataObjectClass *&) MoleculeDBClass,
				      DBASE_INFO_NAME,
				      set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMolecularStructuresDataBaseClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMolecularStructuresDataBaseClass::CopyClone(Identify *  objc)
{
  RxnMolecularStructuresDataBaseClass *objcfull = (RxnMolecularStructuresDataBaseClass *) objc;
  *this = *objcfull;
  MoleculeDBClass = (DataDataBaseInformationClass *) PointerClone(objcfull->MoleculeDBClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMolecularStructuresDataBaseClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMolecularStructuresDataBaseClass::Clone()
{
  RxnMolecularStructuresDataBaseClass* id = new RxnMolecularStructuresDataBaseClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMolecularStructuresDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMolecularStructuresDataBaseClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,MoleculeDBClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMolecularStructuresDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMolecularStructuresDataBaseClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) MoleculeDBClass);
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
BaseDataObject * RxnMolecularStructuresDataBaseClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMolecularStructuresDataBase();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMolecularStructuresDataBaseClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMolecularStructuresDataBaseClass*& obj)
     {
     obj = new RxnMolecularStructuresDataBaseClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMolecularStructuresDataBase
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMolecularStructuresDataBase*& obj)
     {
     obj = new RxnDataMolecularStructuresDataBase;
     return obj->DecodeThis(buffer);
     }
 
/*F dbclass = getMoleculeDBClass()
**
**  DESCRIPTION
**    dbclass: The database information class
**
**  REMARKS
**
*/
DataDataBaseInformationClass *RxnMolecularStructuresDataBaseClass::getMoleculeDBClass()
{
  return MoleculeDBClass;
}
/*S MoleculeSystemBase
 */
 
/*F MoleculeSystemBase(argc,argv) . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    argc,argv: The input arguments
**
**  REMARKS
**
*/
MoleculeSystemBase::MoleculeSystemBase(int argc, char *argv[])
  : AnalysisSystemSave(argc,argv),
    MoleculeDataBaseS("MolecularStructureDataBase"),
    MoleculeDataBaseClassS("MolecularStructureDataBase")
{
  String dbaseclassS = GetEnvironmentVariable(MOLECULE_DATABASE_NAME);
  String dbaseS      = GetEnvironmentVariable(MOLECULE_DATABASE_NAME);
  if(!dbaseclassS.IsEmpty())
    {
      MoleculeDataBaseClassS = dbaseclassS;
    }
  if(!dbaseS.IsEmpty())
    {
      MoleculeDataBaseS = dbaseS;
    }
  MoleculeDataBase = NULL;
  MoleculeDataBaseClass = NULL;
}
MoleculeSystemBase::~MoleculeSystemBase() {

  if(MoleculeDataBase != NULL)
    delete MoleculeDataBase;
  if(MoleculeDataBaseClass != NULL)
    delete MoleculeDataBaseClass;
}
/*F EncodeDecodeObjectsSetUp()  . . . . . . . . . . . set up molecule objects
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MoleculeSystemBase::EncodeDecodeObjectsSetUp()
{
  AnalysisSystemSave::EncodeDecodeObjectsSetUp();
      InitialSetOfMolStatsDecodeFunctions();
      InitialSetOfStaticAtomEncodeDecodeRoutines();
      InitialSetOfMoleculeEncodeDecodeRoutines();
      InitialSetOfMolAtomEncodeDecodeRoutines();
      InitialSetOfMolBondEncodeDecodeRoutines();
      InitialSetOfDBaseEncodeDecodeRoutines();
      InitialSetOfThermoPropsDecodeFunctions();
}
/*F StandardObjectsSetUp()  . . . . . . . . . . . . . set up molecule objects
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MoleculeSystemBase::StandardObjectsSetUp()
    {
      AnalysisSystemSave::StandardObjectsSetUp();
      AddMolStatsClasses(getStandard());
      AddStaticAtomClasses(getStandard());
      AddMoleculeClasses(getStandard());
      AddMolAtomClasses(getStandard());
      AddMolBondClasses(getStandard());
      AddDBaseClasses(getStandard());
      AddThermPropClasses(getStandard());
    }
/*F CommandSetUp()  . . . . . . . . . . . . . . . . . . . . .  InstanceSystem
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MoleculeSystemBase::CommandSetUp()
{
  AnalysisSystemSave::CommandSetUp();
  SingleSystemCommand molread("ReadMolecule",
			      "Read in a MolFile",
			      &InputMolecule);
  Commands.AddObject(molread.getName(),molread);
  SingleSystemCommand fetch("FetchMolecule",
			    "Read in an Element",
			    &ReadMoleculeFromDatabase);
  Commands.AddObject(fetch.getName(),fetch);
  SingleSystemCommand sto("StoreMolecule",
			    "Store an Element",
			    &StoreMoleculeElement);
  Commands.AddObject(sto.getName(),sto);
  String ex("Exists");
  SingleSystemCommand exists(ex,
			     "Element Exists",
			     &MoleculeElementExists);
  Commands.AddObject(ex,exists);
}
/*F Initialization()  . . . . . . . . . . . . . . . . . .  MoleculeSystemBase
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MoleculeSystemBase::Initialization()
{
  AnalysisSystemSave::Initialization();
  
  if(InstanceClasses.IsInList(MoleculeDataBaseClassS))
    {
      MoleculeDataBaseClass = (RxnMolecularStructuresDataBaseClass *)
	InstanceClasses.GetObjectClass(MoleculeDataBaseClassS);
      if(Instances.IsInList(MoleculeDataBaseS))
	{
	  MoleculeDataBase = (RxnDataMolecularStructuresDataBase *)
	    Instances.GetObject(MoleculeDataBaseS);
	}
      MoleculeDataBase->Initialize(MoleculeDataBaseClass->getMoleculeDBClass());
    }
}
/*S MoleculeSystemCommands
 */
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int InputMolecule(ReactionSystemBase* sys)
{
  MoleculeSystemBase *molsystem = (MoleculeSystemBase *) sys;
  int result = 1;

  if(molsystem->Inputs.size() < 2)
    {
      cerr << "Inputs:" << endl;
      cerr << "         Type: Molecule, Substructure, MoleculeClass" << endl;
      cerr << "         File: The MolFile" << endl;
      cerr << "         [NoStore]  if present, don't store in database" << endl;
      exit(1);
    }
  String moltype = molsystem->GetNextInput();
  String molfile = molsystem->GetNextInput();
  bool dbasestore = true;
  if(molsystem->Inputs.size() > 0)
    {
      String flag = molsystem->GetNextInput();
      if(flag == "NoStore")
	dbasestore = false;
    }
  RxnDataMolecularStructuresDataBase *structures = molsystem->MoleculeDataBase;
  BaseDataDataBaseInformation *dbaseinfo = structures->getDatabaseInfo(moltype);
  DataDataBaseInformationClass *dbaseinfoclass = molsystem->MoleculeDataBaseClass->getMoleculeDBClass();
  if(dbaseinfo != NULL)
    {
      OpenInputFile file(molfile);
      RxnSimpleMoleculeClass* simpleclass = (RxnSimpleMoleculeClass *)
	dbaseinfo->getDataElementClass(dbaseinfoclass);
      bool success = true;
      while(success)
	{
	  RxnDataSimpleMolecule *simple = (RxnDataSimpleMolecule *) simpleclass->BaseDataObjectExample();
	  success = simple->ReadMolFileMolecule(file.Stream,simpleclass);
	  if(success)
	    {
	      BaseDataInstance *instance = new BaseDataInstance;
	      String name(simpleclass->GetNameInInstance());
	      instance->NameTag = simple->NameTag;
	      molsystem->Instances.AddInstance(*instance);
	      delete instance;
	      instance = (BaseDataInstance *) molsystem->Instances.GetInstance(simple->NameTag);
	      if(dbasestore)
		dbaseinfo->StoreElement(simple);
	      simple->NameTag = name;
	      instance->AddObject(simple);
	    }
	  delete simple;
	}
    }
  else
    {
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
int StoreMoleculeElement(ReactionSystemBase* sys)
{
  MoleculeSystemBase *molsystem = (MoleculeSystemBase *) sys;
  int result = 1;

  if(molsystem->Inputs.size() != 3)
    {
      cerr << "Inputs:" << endl;
      cerr << "         Type: Type of the database" << endl;
      cerr << "         Element: The name of the element in the instance" << endl;
      cerr << "         Names: The name of the instances of the elements" << endl;
    }
  String elementtype = molsystem->GetNextInput();
  String element = molsystem->GetNextInput();
  String namesS = molsystem->GetNextInput();

  BaseDataDataBaseInformation *dbaseinfo = molsystem->MoleculeDataBase->getDatabaseInfo(elementtype);
  //DataDataBaseInformationClass *dbaseinfoclass = molsystem->MoleculeDataBaseClass->getMoleculeDBClass();
  if(dbaseinfo != NULL)
    {
      //DataObjectClass *elementclass = dbaseinfo->getDataElementClass(dbaseinfoclass);
      if(molsystem->Instances.IsInList(namesS))
	{
	  BaseDataKeyWords *keys = (BaseDataKeyWords *) molsystem->Instances.GetObject(namesS);
	  ObjectList<String> names = keys->GetKeyWords();
	  while(names.size() > 0 && result)
	    {
	      String name = names.front();
	      names.pop_front();
	      if(molsystem->Instances.InstanceInSet(name))
		{
		  BaseDataInstance *instance = molsystem->Instances.GetInstance(name);
		  if(instance->IsInList(element))
		    {
		      BaseDataObject *obj = instance->GetObject(element);
		      String old = obj->NameTag;
		      obj->NameTag = name;
		      //if(obj->GetType() == elementclass->GetType())
		      dbaseinfo->StoreElement(obj);
		      obj->NameTag = old;
			//else
		      //{
		      //cerr << "Element: '" << element << "' in instace: '" << name << "'" << endl;
		      //cerr << "     not of proper type, expecting: '" << elementclass->NameTag << "'" << endl;
		      //result = false;
		      //}
		    }
		  else
		    {
		      cerr << "Element: '" << element << "' not found in instance: '" << name << "'" << endl;
		      result = false;
		    }
		}
	      else
		{
		  cerr << "Instance: '" << name << "' not found" << endl;
		  result = false;
		}
	    }
	}
    }
  else
    {
      cerr << "Database for element type not opened: '" << elementtype << "'" << endl;
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
int ReadMoleculeFromDatabase(ReactionSystemBase* sys)
{
  MoleculeSystemBase *molsystem = (MoleculeSystemBase *) sys;
  int result = 1;

  if(molsystem->Inputs.size() < 2)
    {
      cerr << "Inputs:" << endl;
      cerr << "         Type: Molecule, Substructure, MoleculeClass" << endl;
      cerr << "         Names:  KeyWord with list of instance/element pairs" << endl;
      return 0;
    }
  String moltype = molsystem->GetNextInput();
  String namesS = molsystem->GetNextInput();
      
  BaseDataDataBaseInformation *dbaseinfo = molsystem->MoleculeDataBase->getDatabaseInfo(moltype);
  DataDataBaseInformationClass *dbaseinfoclass = molsystem->MoleculeDataBaseClass->getMoleculeDBClass();
  if(dbaseinfo != NULL)
    {
      if(molsystem->Instances.IsInList(namesS))
	{
	  BaseDataKeyWords *keys = (BaseDataKeyWords *) molsystem->Instances.GetObject(namesS);
	  ObjectList<String> names = keys->GetKeyWords();
	  bool success = true;
	  BaseDataInstance *instance;
	  RxnSimpleMoleculeClass *simpleclass = (RxnSimpleMoleculeClass *) dbaseinfo->getDataElementClass(dbaseinfoclass);
	  String instancename = simpleclass->GetNameInInstance();
	  while(names.size() > 1 && success)
	    {
	      String name = names.front();
	      names.pop_front();
	      cout << "Fetch '" << name << "' and store in instance '" << instancename << "'" << endl;
	      BaseDataObject *element = simpleclass->BaseDataObjectExample();
	      success = dbaseinfo->FetchElement(name,dbaseinfoclass,element);
	      if(success)
		{
		  cout << "Element: '" << element->NameTag << "'" << endl;
		  if(!molsystem->Instances.InstanceInSet(instancename))
		    {
		      BaseDataInstance *newinstance = new BaseDataInstance;
		      newinstance->NameTag = instancename;
		      molsystem->Instances.AddInstance(*newinstance);
		      delete newinstance;
		    }
		  instance = (BaseDataInstance *) molsystem->Instances.GetInstance(instancename);
		  instance->AddObject(element);
		  delete element;
		}
	      else
		{
		  cerr << "Failed to Fetch Element: " << name << endl;
		}
	    }
	}
      else
	{
	  cerr << "KeyWords '" << namesS << "' not found in attributes" << endl;
	  result = 0;
	}
    }
  else
    {
      result = 0;
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
int MoleculeElementExists(ReactionSystemBase* sys)
{
  MoleculeSystemBase *molsystem = (MoleculeSystemBase *) sys;
  int result = 1;

  if(molsystem->Inputs.size() < 2)
    {
      cerr << "Inputs:" << endl;
      cerr << "         Type: Molecule, Substructure, MoleculeClass" << endl;
      cerr << "         Names:  KeyWord with list of instance/element pairs" << endl;
      return 0;
    }
  String moltype = molsystem->GetNextInput();
  String namesS = molsystem->GetNextInput();
      
  BaseDataDataBaseInformation *dbaseinfo = molsystem->MoleculeDataBase->getDatabaseInfo(moltype);
  if(dbaseinfo != NULL)
    {
      if(molsystem->Instances.IsInList(namesS))
	{
	  BaseDataKeyWords *keys = (BaseDataKeyWords *) molsystem->Instances.GetObject(namesS);
	  ObjectList<String> names = keys->GetKeyWords();
	  while(names.size() > 0)
	    {
	      String name = names.front();
	      names.pop_front();
	      cout << "Exists?: '" << name << "'" << endl;
	      bool success = dbaseinfo->ElementExists(name);
	      if(success)
		cout << "Element exists" << endl;
	      else
		cout << "Element does not exist" << endl;
	    }
	}
      else
	{
	  cerr << "KeyWords '" << namesS << "' not found in attributes" << endl;
	  result = 0;
	}
    }
  else
    {
      result = 0;
    }

  return result;
}



/*S Utilities
 */
/*F InitialSetOfMoleculeEncodeDecodeRoutines()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void InitialSetOfMoleculeEncodeDecodeRoutines()
{
  EncodeDecodeRegisterClass(RxnSimpleMoleculeClass,RxnDataSimpleMolecule,MOLECULE_SIMPLE_NAME);
  EncodeDecodeRegisterClass(RxnMolecularStructuresDataBaseClass,RxnDataMolecularStructuresDataBase,MOLECULE_DBASE_NAME);
  EncodeDecodeRegisterClass(RxnMoleculeSetClass,RxnDataMoleculeSet,MOLECULE_MOLSET_NAME);
  EncodeDecodeRegisterClass(RxnRetrieveMoleculePropertyClass,RxnDataRetrieveMoleculeProperty,MOLECULE_RETRIEVE_NAME);
  EncodeDecodeRegisterClass(RxnStoreMoleculePropertyClass,RxnDataStoreMoleculeProperty,MOLECULE_STORE_NAME);
  EncodeDecodeRegisterClass(DataMoleculeEqualValuePredicateClass,BaseDataMoleculeEqualValuePredicate,MOLECULE_PREDICATE_NAME);
}
/*F AddMoleculeClasses(set) . . . . . . . . . . . . . . add to set of classes
**
**  DESCRIPTION
**    set: The set of classes to add to
**
**  REMARKS
**
*/
extern void AddMoleculeClasses(DataSetOfObjectsClass& set)
{
  String moldescr("The Simple molecule Class");
  RxnSimpleMoleculeClass molclass(MOLECULE_SIMPLE_ID,MOLECULE_SIMPLE_NAME,moldescr);
  set.AddObjectClass(molclass);

  String dbdescr("The Molecule Structure Database Class");
  RxnMolecularStructuresDataBaseClass dbclass(MOLECULE_DBASE_ID,MOLECULE_DBASE_NAME,dbdescr);
  set.AddObjectClass(dbclass);

  String setdescr("The Molecule Set Class");
  RxnMoleculeSetClass setclass(MOLECULE_MOLSET_ID,MOLECULE_MOLSET_NAME,setdescr);
  set.AddObjectClass(setclass);

  String transtodescr("The Transfer Property to Instance Operation Class");
  RxnRetrieveMoleculePropertyClass transtoclass(MOLECULE_RETRIEVE_ID,MOLECULE_RETRIEVE_NAME,transtodescr);
  set.AddObjectClass(transtoclass);

  String storedescr("The Store Molecule in Molecule Operation Class");
  RxnStoreMoleculePropertyClass storeclass(MOLECULE_STORE_ID,MOLECULE_STORE_NAME,storedescr);
  set.AddObjectClass(storeclass);

  String molpreddescr("The Molecule Predicate Class");
  DataMoleculeEqualValuePredicateClass molpredclass(MOLECULE_PREDICATE_ID,MOLECULE_PREDICATE_NAME,molpreddescr);
  set.AddObjectClass(molpredclass);
}

