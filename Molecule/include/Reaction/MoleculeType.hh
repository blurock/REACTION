/*  FILE     MoleculeType.hh
**  PACKAGE  Molecule
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "Molecule" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_MOLECULETYPE_HH
#define Reaction_MOLECULETYPE_HH

class RxnSimpleMoleculeClass;
class RxnMolecularStructuresDataBaseClass;
class RxnDataMolecularStructuresDataBase;
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
/*C BaseDataSimpleMolecule  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the SimpleMolecule class definitions
**
**  REMARKS
**    Inheirits BaseDataGraph
*/
class RxnDataSimpleMolecule : public BaseDataGraph
{
  BaseDataSetOfObjects Properties;
  vector<String> Atoms;
public:
  RxnDataSimpleMolecule();
  RxnDataSimpleMolecule(const RxnDataSimpleMolecule& data);
  void AddAtomToMolecule(RxnDataMolFileAtom *atom);
  void AddBondToMolecule(RxnDataMolFileBond *bond);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  bool ReadMolFileMolecule(istream& file,
			   RxnSimpleMoleculeClass *molclass);
  bool ReadInAtoms(istream& file,
		   int numatoms,
		   RxnBasicAtomDataClass *atomdataclass);
  bool ReadInBonds(istream& file,
		   int numbonds,
		   RxnBasicBondDataClass *bonddataclass);
  bool ReadInProperties(istream& file);
  bool CalculateInitialAtomProperties();

  bool StoreProperty(BaseDataObject *);
  BaseDataObject *RetrieveProperty(String& name);

  friend class RxnDataBasicAtomData;
  friend class RxnDataBasicBondData;
  virtual bool WriteAsLine(ostream& out, DataObjectClass *objc);
  virtual bool WriteAsASCII(ostream&, DataObjectClass*);
  virtual bool WriteAsLatex(ostream& out, DataObjectClass* objc);
};
/*C DataSimpleMoleculeClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataGraphClass
*/
class RxnSimpleMoleculeClass : public DataGraphClass
{
  String NameInInstance;
public:
  RxnSimpleMoleculeClass();
  RxnSimpleMoleculeClass(const RxnSimpleMoleculeClass& data);
  RxnSimpleMoleculeClass(const int id, 
		    const String& name,
		    const String& descr);
  DataSetOfObjectsClass *PointerToAllowedClasses();
  void SetNameInInstance(String& name);
  String& GetNameInInstance();
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataMoleculeSet  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MoleculeSet class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataMoleculeSet : public BaseDataSetOfObjects
{
  BaseDataKeySet MoleculeNames;
  BaseDataKeySet InternalMoleculeNames;
  unsigned int MolCount;
public:
  RxnDataMoleculeSet();
  RxnDataMoleculeSet(const RxnDataMoleculeSet& data);
  BaseDataKeySet& getMoleculeNames();
  BaseDataKeySet& getInternalMoleculeNames();
  String MoleculeNameFromInternalName(String& searchname);
  String GenerateNextName(unsigned int places);
  String AddMolecule(String& name);
  bool getMolecules(BaseDataSetOfObjects *set,
		    String& dbasetype,
		    RxnDataMolecularStructuresDataBase *dbase,
		    RxnMolecularStructuresDataBaseClass *dbaseclass);
  unsigned int getMoleculeCount();
  bool ContainedIn(BaseDataKeyWords& moleculelist);
  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnMoleculeSetClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnMoleculeSetClass : public DataSetOfObjectsClass
{
  unsigned int NamePlaces;
public:
  RxnMoleculeSetClass();
  RxnMoleculeSetClass(const RxnMoleculeSetClass& data);
  RxnMoleculeSetClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  DataSetOfObjectsClass *PointerToAllowedClasses();
};
/*C BaseDataMoleculePredicate  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MoleculePredicate class definitions
**
**  REMARKS
**    Inheirits BaseDataLogicalOperation
*/
class BaseDataMoleculeEqualValuePredicate : public BaseDataExactlyEqualPredicate
{
public:
  BaseDataMoleculeEqualValuePredicate();
  BaseDataMoleculeEqualValuePredicate(const BaseDataMoleculeEqualValuePredicate& data);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_OPERATION_METHODS;
};
/*C DataMoleculePredicateClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataLogicalOperationClass
*/
class DataMoleculeEqualValuePredicateClass : public DataExactlyEqualPredicateClass
{
public:
  DataMoleculeEqualValuePredicateClass();
  DataMoleculeEqualValuePredicateClass(const DataMoleculeEqualValuePredicateClass& data);
  DataMoleculeEqualValuePredicateClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};


/*C RxnDataRetrieveMoleculeProperty  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MoleculeInfoToInstance class definitions
**
**  REMARKS
**    Inheirits BaseDataOperation
*/
class RxnDataRetrieveMoleculeProperty : public BaseDataOperation
{
public:
  RxnDataRetrieveMoleculeProperty();
  RxnDataRetrieveMoleculeProperty(const RxnDataRetrieveMoleculeProperty& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_OPERATION_METHODS;
};
/*C RxnMoleculeInfoToInstanceClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataOperationClass
*/
class RxnRetrieveMoleculePropertyClass : public DataOperationClass
{
public:
  RxnRetrieveMoleculePropertyClass();
  RxnRetrieveMoleculePropertyClass(const RxnRetrieveMoleculePropertyClass& data);
  RxnRetrieveMoleculePropertyClass(const int id, 
				   const String& name,
				   const String& descr);
  DataSetOfObjectsClass *PointerToAllowedClasses();
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataStoreMoleculeProperty  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the StoreMoleculeProperty class definitions
**
**  REMARKS
**    Inheirits BaseDataOperation
*/
class RxnDataStoreMoleculeProperty : public BaseDataOperation
{
public:
  RxnDataStoreMoleculeProperty();
  RxnDataStoreMoleculeProperty(const RxnDataStoreMoleculeProperty& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_OPERATION_METHODS;
};
/*C RxnStoreMoleculePropertyClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataOperationClass
*/
class RxnStoreMoleculePropertyClass : public DataOperationClass
{
public:
  RxnStoreMoleculePropertyClass();
  RxnStoreMoleculePropertyClass(const RxnStoreMoleculePropertyClass& data);
  RxnStoreMoleculePropertyClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  DataSetOfObjectsClass *PointerToAllowedClasses();
};
/*C RxnDataMolecularStructuresDataBase  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MolecularStructuresDataBase class definitions
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataMolecularStructuresDataBase : public BaseDataObject
{
  
  BaseDataDataBaseInformation *DBMolecule;
  BaseDataDataBaseInformation *DBSubStructures;
  BaseDataDataBaseInformation *DBMoleculeClasses;

public:
  RxnDataMolecularStructuresDataBase();
  RxnDataMolecularStructuresDataBase(const RxnDataMolecularStructuresDataBase& data);
  virtual bool Initialize(DataDataBaseInformationClass *dbclass);
  virtual BaseDataDataBaseInformation *getDatabaseInfo(String& dbtype);
  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnMolecularStructuresDataBaseClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnMolecularStructuresDataBaseClass : public DataObjectClass
{
  DataDataBaseInformationClass *MoleculeDBClass;
public:
  RxnMolecularStructuresDataBaseClass();
  RxnMolecularStructuresDataBaseClass(const RxnMolecularStructuresDataBaseClass& data);
  RxnMolecularStructuresDataBaseClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  virtual DataDataBaseInformationClass *getMoleculeDBClass();
};
/*S Molecule Classes
*/
/*C MoleculeSystemBase  . . . . . . . . . . . . . . . . . . .  Molecule Basis
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class MoleculeSystemBase : public AnalysisSystemSave
{
public:
  String MoleculeDataBaseS;
  RxnDataMolecularStructuresDataBase *MoleculeDataBase;
  String MoleculeDataBaseClassS;
  RxnMolecularStructuresDataBaseClass *MoleculeDataBaseClass;

  MoleculeSystemBase(int argc, char *argv[]);
  virtual ~MoleculeSystemBase();
  virtual void EncodeDecodeObjectsSetUp();
  virtual void StandardObjectsSetUp();
  virtual void Initialization();
  virtual void CommandSetUp();
};

#endif
