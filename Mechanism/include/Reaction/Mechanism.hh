/*  FILE     Mechanism.hh
**  PACKAGE  Mechanism
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "Mechanism" package in the Reaction environment
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
#ifndef Reaction_MECHANISM_HH
#define Reaction_MECHANISM_HH

#define MECHANISM_BASE             990050
#define MECHANISM_SUMMARY_NAME     "ReactionSummary"
#define MECHANISM_MECHANISM_NAME   "Mechanism"
#define MECHANISM_DBASE_NAME       "MechanismDataBase"
#define MECHANISM_PRINT_NAME       "PrintOutMechanism"
#define MECHANISM_GENERATED_NAME   "GetGeneratedMechanism"
#define MECHANISM_FILLALG_NAME     "FillMechanism"
#define MECHANISM_MOLECULE_NAME    "MoleculeSummary"
#define MECHANISM_MOLSUMSET_NAME   "MoleculeSummarySet"
#define MECHANISM_BUILD_NAME       "BuildMechanism"

#define MECHANISM_SUMMARY_ID       MECHANISM_BASE + 1
#define MECHANISM_MECHANISM_ID     MECHANISM_BASE + 2
#define MECHANISM_DBASE_ID         MECHANISM_BASE + 3
#define MECHANISM_PRINT_ID         MECHANISM_BASE + 4
#define MECHANISM_GENERATED_ID     MECHANISM_BASE + 5
#define MECHANISM_FILLALG_ID       MECHANISM_BASE + 6
#define MECHANISM_MOLECULE_ID      MECHANISM_BASE + 7
#define MECHANISM_MOLSUMSET_ID     MECHANISM_BASE + 8
#define MECHANISM_BUILD_ID         MECHANISM_BASE + 9


#define MECHANISM_PARAMETER  "Mechanism"
#define REACTION_CLASS_LIST  "ReactionClassList"
#define MOLECULE_LIST        "MoleculeList"
#define REACTION_LIST        "ReactionList"
#define REACTION_SUMMARY_NAME         "Summary"
#define MOLECULE_SUMMARY_NAME         "Summary"
#define MECHANISM_USE_REACTION_CLASSES  "UseReactionClasses"

#define MOLECULE_DATABASE_PARAMETER   "MoleculeDatabase"
#define REACTION_DATABASE_PARAMETER   "ReactionDatabase"
#define MECHANISM_DATABASE_PARAMETER  "MechanismDatabase"
#define MOLECULE_CLASS_PARAMETER      "MoleculeClass"
#define REACTION_CLASS_PARAMETER      "ReactionClass"
#define MECHANISM_CLASS_PARAMETER     "MechanismClass"
#define CHEMKIN_CLASS_PARAMETER       "ChemkinClass"
#define REFERENCE_PARAMETER           "LiteratureReference"

/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "MechanismType.hh"


/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
void AddMechanismClasses(DataSetOfObjectsClass& set);
void InitialSetOfMechanismDecodeFunctions();
int MechanismReactionsAsInstances(ReactionSystemBase* sys);


#endif

