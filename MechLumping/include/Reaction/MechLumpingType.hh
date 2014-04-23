/*  FILE     MechLumpingType.hh
**  PACKAGE  MechLumping
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "MechLumping" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
#ifndef Reaction_MECHLUMPINGTYPE_HH
#define Reaction_MECHLUMPINGTYPE_HH

/*C BaseDataRxnClassesInMolecules  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the RxnClassesInMolecules class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class BaseDataRxnClassesInMolecules : public BaseDataAlgorithmOperation
{
  String MechanismS;
  RxnDataMechanism  *Mechanism;
  String ReactionClassListS;
  BaseDataKeyWords *ReactionClassList;
  String ParameterNamesS;
  BaseDataKeyWords *ParameterNames;

  String AsReactantS;
  String AsProductS;

  RxnMechanismClass *MechanismClass;
  RxnReactionClass *RxnClass;
  String NameInInstance;
public:
  BaseDataRxnClassesInMolecules();
  BaseDataRxnClassesInMolecules(const BaseDataRxnClassesInMolecules& data);

  void WriteMoleculeReactionClasses(BaseDataSetOfInstances *instances,
				    BaseDataKeyWords *mols,
				    String& parametername,
				    String& rxnclass);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C DataRxnClassesInMoleculesClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class DataRxnClassesInMoleculesClass : public DataAlgorithmOperationClass
{
public:
  DataRxnClassesInMoleculesClass();
  DataRxnClassesInMoleculesClass(const DataRxnClassesInMoleculesClass& data);
  DataRxnClassesInMoleculesClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataLumpMechanism  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the LumpMechanism class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataLumpMechanism : public BaseDataAlgorithmOperation
{
  String MechanismNameS;
  BaseDataString *MechanismName;
  RxnDataMechanism *Mechanism;
  RxnMechanismClass *MechanismClass;

  String LumpedMechanismNameS;
  RxnDataMechanism *LumpedMechanism;

  String LumpedMoleculesS;
  BaseDataSetOfEquivalentSets *LumpedMolecules;

  String SpeciesToEliminateS;
  BaseDataKeyWords *SpeciesToEliminate;

  BaseDataSetOfObjects *MolCorrespondences;
  RxnSimpleMoleculeClass *MoleculeClass;
  RxnReactionClass *ReactionClass;
public:
  RxnDataLumpMechanism();
  RxnDataLumpMechanism(const RxnDataLumpMechanism& data);
  void SetUpMolecules(BaseDataSetOfInstances *instances,
		 DataSetOfInstancesClass *instancesclass);
  void SetUpLumpedMoleculeCorrespondences(BaseDataSetOfInstances *instances,
					  DataSetOfInstancesClass *instancesclass);
  void CreateAndAddMoleculeInfo(BaseDataEquivalentSet *set,
				BaseDataSetOfInstances *instances,
				DataSetOfInstancesClass *instancesclass);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C RxnLumpMechanismClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnLumpMechanismClass : public DataAlgorithmOperationClass
{
public:
  RxnLumpMechanismClass();
  RxnLumpMechanismClass(const RxnLumpMechanismClass& data);
  RxnLumpMechanismClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C BaseDataSimpleLumpedMolecules  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the SimpleLumpedMolecules class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class BaseDataSimpleLumpedMolecules : public BaseDataAlgorithmOperation
{
  String EquivalentSetsS;
  BaseDataSetOfEquivalentSets *EquivalentSets;
  String MoleculeListS;
  BaseDataKeyWords *MoleculeList;

  String TakeFirstS;
  bool TakeFirst;
public:
  BaseDataSimpleLumpedMolecules();
  BaseDataSimpleLumpedMolecules(const BaseDataSimpleLumpedMolecules& data);
  void SubstituteWithFirstElement(BaseDataSetOfInstances *instances,
				 DataSetOfInstancesClass *instancesclass);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C DataSimpleLumpedMoleculesClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class DataSimpleLumpedMoleculesClass : public DataAlgorithmOperationClass
{
public:
  DataSimpleLumpedMoleculesClass();
  DataSimpleLumpedMoleculesClass(const DataSimpleLumpedMoleculesClass& data);
  DataSimpleLumpedMoleculesClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C BaseDataSimpleEquivalentReactions  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the SimpleEquivalentReactions class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class BaseDataSimpleEquivalentReactions : public BaseDataAlgorithmOperation
{
  String MoleculeEquivalentSetsS;
  BaseDataSetOfEquivalentSets *MoleculeEquivalentSets;
  String EquivalentSetsS;
  BaseDataSetOfEquivalentSets *EquivalentSets;
  String ReactionListS;
  BaseDataKeyWords *ReactionList;

  String TakeFirstS;
  bool TakeFirst;
public:
  BaseDataSimpleEquivalentReactions();
  BaseDataSimpleEquivalentReactions(const BaseDataSimpleEquivalentReactions& data);
  void UpdateConstants(RxnDataReaction *rxn,DataSetOfInstancesClass *instancesclass,String& rateS,double *A,double *n,double *E);
  void TranslateProductsAndReactantsInReaction(String& foundname, BaseDataInstance *newinst, RxnDataReaction *rxn);
  void TranslateRestReactions(BaseDataKeyWords *rxnlist, 
			      BaseDataSetOfInstances *instances, 
			      DataSetOfInstancesClass *instancesclass,
			      BaseDataSetOfObjects *corrs);
  BaseDataInstance *UpdateReactionCoefficients(bool newinstance,
					       unsigned int numrxns,
				  String& subname,String& name,
				  BaseDataInstance *instance,
				  DataSetOfInstancesClass *instancesclass);

  unsigned int LumpingFactors(const String& type, BaseDataInstance *inst);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C DataSimpleEquivalentReactionsClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class DataSimpleEquivalentReactionsClass : public DataAlgorithmOperationClass
{
public:
  DataSimpleEquivalentReactionsClass();
  DataSimpleEquivalentReactionsClass(const DataSimpleEquivalentReactionsClass& data);
  DataSimpleEquivalentReactionsClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};






/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
