/*  FILE     Molecule.hh
**  PACKAGE  Molecule
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "Molecule" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_MOLECULE_HH
#define REACTION_MOLECULE_HH
 
#define MOLECULE_BASE   50030
#define MOLECULE_SIMPLE_NAME   "SimpleMolecule"
#define MOLECULE_SIMPLE_ID     MOLECULE_BASE + 1
#define MOLECULE_DBASE_NAME    "MolecularStructuresDataBase"
#define MOLECULE_DBASE_ID      MOLECULE_BASE + 2
#define MOLECULE_MOLSET_NAME   "MoleculeSet"
#define MOLECULE_MOLSET_ID     MOLECULE_BASE + 3
#define MOLECULE_RETRIEVE_NAME "RetrieveMoleculeProperty"
#define MOLECULE_RETRIEVE_ID   MOLECULE_BASE + 4
#define MOLECULE_STORE_NAME    "StoreMoleculeProperty"
#define MOLECULE_STORE_ID      MOLECULE_BASE + 5
#define MOLECULE_PREDICATE_NAME    "MoleculeEqualValuePredicate"
#define MOLECULE_PREDICATE_ID      MOLECULE_BASE + 6

#define MOLECULE_DATABASE_NAME "MOLECULE_DATABASE_NAME"
#define NAME_IN_INSTANCE "Molecule"

#define STANDARD_VALENCE  "Valence"

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "MoleculeType.hh"


/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
extern void InitialSetOfMoleculeEncodeDecodeRoutines();
extern void AddMoleculeClasses(DataSetOfObjectsClass& set);
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif



