/*  FILE     MechLumping.hh
**  PACKAGE  MechLumping
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "MechLumping" package in the Reaction environment
**
**  COPYRIGHT (C) 2000 Edward S. Blurock
*/
 
#ifndef Reaction_MECHLUMPING_HH
#define Reaction_MECHLUMPING_HH

#define LUMPING_BASE      50160
#define LUMPING_MOLRXNCLASSALG_NAME      "RxnClassesInMolecules" 
#define LUMPING_MOLRXNCLASSALG_ID         LUMPING_BASE + 1
#define LUMPING_LUMPMECH_NAME            "LumpMechanism" 
#define LUMPING_LUMPMECH_ID              LUMPING_BASE + 2
#define LUMPING_SIMPLEMOLECULE_NAME            "SimpleLumpedMolecules" 
#define LUMPING_SIMPLEMOLECULE_ID              LUMPING_BASE + 3
#define LUMPING_SIMPLERXN_NAME                 "SimpleEquivalentReactions" 
#define LUMPING_SIMPLERXN_ID                   LUMPING_BASE + 4

#define MOLRXN_PARAMETER_NAMES    "MolRxnClassesParameterNames"
#define LUMPED_MECHANISM_NAME     "LumpedMechanismName"
#define LUMP_EQUIVALENT_SETS      "LumpedMoleculesEquivalentSet"
#define SPECIES_TO_REMOVE         "ListOfSpeciesToRemove"
#define LUMPING_TAKE_FIRST        "TakeFirst"

#define MOLECULE_EQUIVALENT_SET_NAME "LumpedEquivalentSets"
#define REACTION_EQUIVALENT_SET_NAME "ReactionEquivalentSets"

/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "MechLumpingType.hh"

/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
void InitialMechLumpingDecodeFunctions();
void AddMechLumpingClasses(DataSetOfObjectsClass& set);

#endif
