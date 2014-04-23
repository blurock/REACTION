/*  FILE     MechanismType.hh
**  PACKAGE  Mechanism
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "Mechanism" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
#ifndef Reaction_MECHANISMTYPE_HH
#define Reaction_MECHANISMTYPE_HH


class RxnMechanismClass;
/*C RxnDataMoleculeSummary  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MoleculeSummary class definitions
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataMoleculeSummary : public BaseDataObject
{
public:
  String ThermodynamicInfo;
  String ShortName;

  RxnDataMoleculeSummary();
  RxnDataMoleculeSummary(const RxnDataMoleculeSummary& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnMoleculeSummaryClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnMoleculeSummaryClass : public DataObjectClass
{
public:
  RxnMoleculeSummaryClass();
  RxnMoleculeSummaryClass(const RxnMoleculeSummaryClass& data);
  RxnMoleculeSummaryClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataMoleculeSummarySet  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MoleculeSummarySet class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataMoleculeSummarySet : public BaseDataSetOfObjects
{
public:
  RxnDataMoleculeSummarySet();
  RxnDataMoleculeSummarySet(const RxnDataMoleculeSummarySet& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
};
/*C RxnMoleculeSummarySetClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnMoleculeSummarySetClass : public DataSetOfObjectsClass
{
  RxnMoleculeSummaryClass *MoleculeSummaryClass;
public:
  RxnMoleculeSummarySetClass();
  RxnMoleculeSummarySetClass(const RxnMoleculeSummarySetClass& data);
  RxnMoleculeSummarySetClass(const int id, 
		    const String& name,
		    const String& descr);
  ~RxnMoleculeSummarySetClass() { if(MoleculeSummaryClass != NULL) delete MoleculeSummaryClass; }
  STANDARD_VIRTUAL_METHODS_CLASS
  DataSetOfObjectsClass *PointerToAllowedClasses();
  RxnMoleculeSummaryClass *getMoleculeSummaryClass();
};



/*C RxnDataReactionSummary  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ReactionSummary class definitions
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataReactionSummary : public BaseDataObject
{
  String ReactionName;
  String ForwardRate;
  String ReverseRate;
public:
  RxnDataReactionSummary();
  RxnDataReactionSummary(const RxnDataReactionSummary& data);
  RxnDataReactionSummary(const String& name,
			 const String& forward,
			 const String& reverse);

  STANDARD_VIRTUAL_METHODS_OBJECT
  String& getReactionName();
  String& getForwardRate();
  String& getReverseRate();
  void setReactionName(const String& name);
  void setForwardRate(const String& name);
  void setReverseRate(const String& name);
};
/*C RxnReactionSummaryClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnReactionSummaryClass : public DataObjectClass
{
public:
  RxnReactionSummaryClass();
  RxnReactionSummaryClass(const RxnReactionSummaryClass& data);
  RxnReactionSummaryClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataMechanism  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the Mechanism class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataMechanism : public BaseDataSetOfObjects
{
  BaseDataKeyWords MoleculeNames;
  BaseDataKeyWords ReactionNames;
  BaseDataSetOfObjects MoleculeSummaries;
  BaseDataSetOfObjects ReactionSummaries;
  BaseDataSetOfObjects Thermodynamics;
public:
  RxnDataMechanism();
  RxnDataMechanism(const RxnDataMechanism& data);
  bool addToMechanism(const String& name,
		      const String& forward,
		      const String& reverse,
		      RxnMechanismClass *mechclass);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  bool addMolecule(String& name);
  bool addMolecule(RxnDataSimpleMolecule *molecule);
  bool addReaction(RxnDataReaction *reaction);
  bool addReaction(String& name);
  bool addReactionSummary(RxnDataReactionSummary *summary);
  bool addMoleculeSummary(RxnDataMoleculeSummary *summary);
  bool AddThermodynamic(String &molecule, RxnDataThermoProperty *thermo);
  RxnDataSimpleMolecule *getMolecule(String& name,
				     RxnSimpleMoleculeClass *molclass,
				      BaseDataSetOfInstances *instances,
				      DataSetOfInstancesClass *instancesclass);
  RxnDataReaction *getReaction(String& name,
			       RxnReactionClass *molclass,
			       BaseDataSetOfInstances *instances,
			       DataSetOfInstancesClass *instancesclass);
  BaseDataKeyWords& getMoleculeNames();
  BaseDataKeyWords& getReactionNames();
  RxnDataReactionSummary *getReactionSummary(String& name);
  RxnDataMoleculeSummary *getMoleculeSummary(String& name);
  RxnDataThermoProperty *getThermoValueForMolecule(String& molecule);
  RxnDataThermoProperty *getThermoValueForMolecule(String& molname,RxnDataSimpleMolecule *molecule);
  RxnDataThermoProperty *getThermoValueForMolecule(String& molname,
						   RxnSimpleMoleculeClass *molclass,
						   BaseDataSetOfInstances *instances,
						   DataSetOfInstancesClass *instancesclass);
  bool ReadInMoleculeInformation(istream& in, RxnMechanismClass *mechclass);
  void setUpMoleculeSummaries(BaseDataSetOfObjects *molsum);
  BaseDataKeyWords *IdentifyIdenticalReactions(RxnReactionClass *rxnclass,
						    BaseDataSetOfInstances *instances,
						    DataSetOfInstancesClass *instancesclass);
};
/*C RxnMechanismClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnMechanismClass : public DataSetOfObjectsClass
{
  String NameInInstance;
  BaseDataSetOfObjects MoleculeData;
  RxnReactionSummaryClass *SummaryClass;
  RxnSimpleMoleculeClass *MoleculeClass;
  RxnReactionClass *ReactionClass;
  RxnMoleculeSummaryClass *MoleculeSummaryClass;
public:
  BaseDataKeyWords  MolDatVariables;
  RxnMechanismClass();
  RxnMechanismClass(const RxnMechanismClass& data);
  RxnMechanismClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  bool ReadMolDat(istream& in);
  BaseDataObject *GetMolDat(String& name);

  RxnReactionSummaryClass *getSummaryClass();
  DataSetOfObjectsClass *PointerToAllowedClasses();
  String getNameInInstance();
  RxnReactionClass *getReactionClass();
  RxnMoleculeSummaryClass *getMoleculeSummaryClass();
  RxnSimpleMoleculeClass *getMoleculeClass();
};
/*C RxnDataPrintOutMechanism  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the PrintOutMechanism class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataPrintOutMechanism : public BaseDataAlgorithmOperation
{
  String PrintParametersS;
  String MechanismS;
  String FileNameS;
  String MechanismClassS;
  String SubMoleculesS;
  BaseDataKeyWords *SubMolecules;
  String Property;
  String EnthalpyProperty;
  String EntropyProperty;
  String CpProperty;

  RxnDataMechanism *Mechanism;
  RxnMechanismClass *MechanismClass;
  OpenOutputFile *Output;
  OpenOutputFile *OutputBase;
  OpenOutputFile *OutputThm;
  OpenOutputFile *OutputCorr;
  OpenOutputFile *MolDat;
  OpenOutputFile *SpeciesList;

  BaseDataSetOfObjects References;

  bool oneFile;
public:
  RxnDataPrintOutMechanism();
  RxnDataPrintOutMechanism(const RxnDataPrintOutMechanism& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
  bool PrintMoleculeInformation(BaseDataSetOfInstances *instances,
				DataSetOfInstancesClass *instancesclass,
				BaseDataAlgorithmRun *run,
				DataAlgorithmRunClass *runclass);
  bool PrintMechanismPrefix(ostream& out,
			    BaseDataAlgorithmRun* run,
			    DataAlgorithmRunClass *runclass);
  bool PrintMechanismPostfix(ostream& out,
			     BaseDataAlgorithmRun* run,
			     DataAlgorithmRunClass *runclass);
  bool DecideIfRateListed(RxnDataReactionRates *rate,RxnReactionRatesClass *rateclass);
  bool PrintMechanismRates(ostream& out,
			   String& name,
			   String property,
			   RxnDataReaction *rxn,
			   DataSetOfObjectsClass *classes,
			   RxnDataReactionRates *forward,
			   RxnDataReactionRates *reverse,
			   BaseDataAlgorithmRun* run,
			   DataAlgorithmRunClass *runclass);
  bool PrintMechanismPrefixLaTeX(ostream& out,
			    BaseDataAlgorithmRun* run,
			    DataAlgorithmRunClass *runclass);
  bool PrintMechanismPostfixLaTeX(ostream& out,
			     BaseDataAlgorithmRun* run,
			     DataAlgorithmRunClass *runclass);
  bool PrintMechanismRatesLaTeX(ostream& out,
				String& name,
				String property,
				RxnDataReaction *rxn,
				DataSetOfObjectsClass *classes,
				RxnDataReactionRates *forward,
				RxnDataReactionRates *reverse,
				BaseDataAlgorithmRun* run,
				DataAlgorithmRunClass *runclass);
  bool PrintMechanismRatesStandard(ostream& out,
				   String& name,
				   String property,
				   RxnDataReaction *rxn,
				   DataSetOfObjectsClass *classes,
				   RxnDataReactionRates *forward,
				   RxnDataReactionRates *reverse,
				   BaseDataAlgorithmRun* run,
				   DataAlgorithmRunClass *runclass);
  bool PrintRateLaTeX(ostream& out,
		      unsigned int n, 
		      RxnDataReactionRates *rate, RxnReactionRatesClass *rateclass, 
		      DataSetOfObjectsClass *classes, String property);
  bool PrintThermodynamics(BaseDataSetOfInstances *instances,
			   DataSetOfInstancesClass *instancesclass,
			   BaseDataAlgorithmRun *run,
			   DataAlgorithmRunClass *runclass);
  bool PrintReactions(BaseDataSetOfInstances *instances,
		      DataSetOfInstancesClass *instancesclass,
		      BaseDataAlgorithmRun *run,
		      DataAlgorithmRunClass *runclass);
  bool PrintOutMechanismThermoSeries(ostream& out,
				     VectorNumeric& temperatures,
				     BaseDataAlgorithmRun* run,
				     DataAlgorithmRunClass *runclass,
				     BaseDataSetOfInstances *instances,
				     DataSetOfInstancesClass *instancesclass);
  bool ThermoSeriesTable(ostream& out,
			 String& molname,
			 RxnDataSimpleMolecule *molecule,
			 VectorNumeric& temperatures,
			 RxnDataThermoProperty *thermo,
			 RxnThermoPropertyClass *thermoclass,
			 BaseDataAlgorithmRun* run,
			 DataAlgorithmRunClass *runclass,
			 bool ischemkin);
  bool ThermoSeriesPrefix(ostream& out,
			  VectorNumeric& temperatures,
			  BaseDataAlgorithmRun* run,
			  DataAlgorithmRunClass *runclass);
  bool ThermoSeriesPostfix(ostream& out,
			   BaseDataAlgorithmRun* run,
			   DataAlgorithmRunClass *runclass);
  bool PrintMolDatInformation(String& name,
			      RxnDataSimpleMolecule *molecule,
			      DataSetOfObjectsClass *instancesclasses);
  void FillMoleculesInReaction(RxnDataMechanism *mech,
			       RxnDataReaction *rxn,
			       RxnSimpleMoleculeClass *molclass,
			       BaseDataSetOfInstances *instances,
			       DataSetOfInstancesClass *instancesclass);
  bool BaseReaction(RxnDataReaction *rxn);
  void UpdateShortName(BaseDataSetOfInstances *instances,
		       DataSetOfInstancesClass *instancesclass);

};
/*C RxnPrintOutMechanismClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnPrintOutMechanismClass : public DataAlgorithmOperationClass
{
public:
  RxnPrintOutMechanismClass();
  RxnPrintOutMechanismClass(const RxnPrintOutMechanismClass& data);
  RxnPrintOutMechanismClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataGetGeneratedMechanism  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the GetGeneratedMechanism class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataGetGeneratedMechanism : public BaseDataAlgorithmOperation
{
  String ParametersS;
  BaseDataKeyWords *Parameters;
  String RootName;

  BaseDataSetOfObjects Chemkin;
  BaseDataSetOfObjects Molecules;
  BaseDataSetOfObjects Reactions;

  BaseDataKeyWords MoleculeNames;

  String MoleculeDBaseS;
  String ReactionDBaseS;
  String MechanismDBaseS;
  String ChemkinClassS;
  String ChemkinThermoName;
  String ChemkinName;
  String ReferenceS;
  String DBaseName;
  String MoleculeSummaryS;

  RxnDataMolecularStructuresDataBase *DataBase;
  RxnMolecularStructuresDataBaseClass *DataBaseClass;
  BaseDataDataBaseInformation  *MoleculeDBase;
  BaseDataDataBaseInformation  *ReactionDBase;
  BaseDataDataBaseInformation  *MechanismDBase;
  DataDataBaseInformationClass *DBaseClass;
  
  RxnSimpleMoleculeClass *MoleculeClass;
  RxnReactionClass *ReactionClass;
  RxnMechanismClass *MechanismClass;

  RxnDataMechanism *Mechanism;
  RxnDataLiteratureReference *Reference;

  OpenInputFile *ChemkinFile;
  OpenInputFile *MoleculeFile;
  OpenInputFile *MechanismFile;

  BaseDataKeyWords *ReactionList;
public:
  RxnDataGetGeneratedMechanism();
  RxnDataGetGeneratedMechanism(const RxnDataGetGeneratedMechanism& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
  String& ReadInReaction(String& key,
			 double multiplicity,
			 istream& in,
			 RxnDataReaction *rxn);
  bool ReadInMolecules(BaseDataSetOfInstances *instances,
		       DataSetOfInstancesClass *instancesclass);
  bool ReadInMolecules(BaseDataSetOfInstances *instances);
  bool ReadReactionClasses(istream& in,
			   BaseDataSetOfInstances *instances);
  bool ReadInReactionSet(istream& in,
			 BaseDataSetOfInstances *instances);
  String StripParensFromName(String& name);
  bool ReadInClassCoefficients(istream& in,
			     BaseDataSetOfInstances *instances);
  String ReactionName(String& rxnclassS, unsigned int count);
  void AddReactionSummaryToMechanism(String& rxnname,
				     BaseDataInstance *instance,
				     RxnDataReactionRates *forward,
				     RxnDataReactionRates *reverse);
  RxnDataReaction *MakeReaction(String& rxnname,
				RxnDataReactionRates *forward,
				RxnDataReactionRates *reverse);
  void  SetUpMechanismInInstance(BaseDataSetOfInstances *instances);
  BaseDataInstance *InstanceInInstances(String& name, 
					BaseDataSetOfInstances *instances,
					DataSetOfInstancesClass *instancesclass);
  bool StoreChemkinNameInMolecule(String& name,
				  RxnDataSimpleMolecule *molecule,
				  BaseDataInstance *instance);
  bool StoreThermoInMolecule(RxnDataChemkinThermo *chemkin,
			     RxnDataSimpleMolecule *molecule,
			     BaseDataInstance *instance);
};
/*C RxnGetGeneratedMechanismClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnGetGeneratedMechanismClass : public DataAlgorithmOperationClass
{
public:
  RxnGetGeneratedMechanismClass();
  RxnGetGeneratedMechanismClass(const RxnGetGeneratedMechanismClass& data);
  RxnGetGeneratedMechanismClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataFillMechanism  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the FillMechanism class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataFillMechanism : public BaseDataAlgorithmOperation
{
  String parametersS;
  BaseDataKeyWords *parameters;
  String  rxnTypeS;
  String  mechClassS;
  String  mechanismS;
  String  dataBaseS;

  RxnMechanismClass *mechClass;
  RxnDataMechanism  *mechanism;
  RxnDataMolecularStructuresDataBase  *dataBase;
  RxnMolecularStructuresDataBaseClass  *dataBaseClass;
  RxnReactionClass* rxnClass;

  BaseDataDataBaseInformation *rxndbaseinfo;
  DataDataBaseInformationClass *dbaseinfoclass;

public:
  RxnDataFillMechanism();
  RxnDataFillMechanism(const RxnDataFillMechanism& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
  void MakeNewInstance(String name, BaseDataSetOfInstances *instances);
};
/*C RxnFillMechanismClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnFillMechanismClass : public DataAlgorithmOperationClass
{
public:
  RxnFillMechanismClass();
  RxnFillMechanismClass(const RxnFillMechanismClass& data);
  RxnFillMechanismClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataMechanismDataBase  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MechanismDataBase class definitions
**
**  REMARKS
**    Inheirits BaseDataReactionStructuresDataBase
*/
class RxnDataMechanismDataBase : public RxnDataReactionStructuresDataBase
{
  BaseDataDataBaseInformation *DBMechanisms;
  BaseDataDataBaseInformation *DBMechanismPatterns;
public:
  RxnDataMechanismDataBase();
  RxnDataMechanismDataBase(const RxnDataMechanismDataBase& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  virtual bool Initialize(DataDataBaseInformationClass *dbclass);
  virtual BaseDataDataBaseInformation *getDatabaseInfo(String& type);
};
/*C RxnMechanismDataBaseClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataReactionStructuresDataBaseClass
*/
class RxnMechanismDataBaseClass : public RxnReactionStructuresDataBaseClass
{
public:
  RxnMechanismDataBaseClass();
  RxnMechanismDataBaseClass(const RxnMechanismDataBaseClass& data);
  RxnMechanismDataBaseClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C MechanismSystemBase  . . . . . . . . . . . . . . . . . . .  Mechanism Base
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class MechanismSystemBase : public RxnSystemBase
{
 public:
  MechanismSystemBase(int argc, char *argv[]);
  virtual ~MechanismSystemBase() {};
  virtual void EncodeDecodeObjectsSetUp();
  virtual void StandardObjectsSetUp();
  virtual void Initialization();
  virtual void CommandSetUp();
};
/*C RxnDataBuildMechanism  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the BuildMechanism class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataBuildMechanism : public BaseDataAlgorithmOperation
{
  String ReactionListS;
  BaseDataKeyWords *ReactionList;
  String MoleculeListS;
  BaseDataKeyWords *MoleculeList;
  String MechanismNameS;
  BaseDataString *MechanismName;
  String NewMechanismName;
  RxnDataMechanism *Mechanism;

  RxnMechanismClass *MechanismClass;

  String UseReactionClassesS;
  bool UseReactionClasses;

  BaseDataKeyWords *ChemkinMoleculeList;
  
public:
  RxnDataBuildMechanism();
  RxnDataBuildMechanism(const RxnDataBuildMechanism& data);

  void BuildMoleculeData(BaseDataSetOfInstances *instances,
			 DataSetOfInstancesClass *instancesclass);
  void BuildReactionData(BaseDataSetOfInstances *instances,
			 DataSetOfInstancesClass *instancesclass);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C RxnBuildMechanismClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnBuildMechanismClass : public DataAlgorithmOperationClass
{
public:
  RxnBuildMechanismClass();
  RxnBuildMechanismClass(const RxnBuildMechanismClass& data);
  RxnBuildMechanismClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};


/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
