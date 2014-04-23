/*  FILE     MolBond.hh
**  PACKAGE  MolBond
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "MolBond" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_MOLBOND_HH
#define REACTION_MOLBOND_HH

#define MOLBOND_BASE    50020
#define MOLBOND_BASIC_NAME     "BasicBondData"
#define MOLBOND_MOLFILE_NAME   "MolFileBond"
#define MOLBOND_MOLBOND_NAME   "MoleculeBond"

#define MOLBOND_BASIC_ID     MOLBOND_BASE + 1
#define MOLBOND_MOLFILE_ID   MOLBOND_BASE + 2
#define MOLBOND_MOLBOND_ID   MOLBOND_BASE + 3


#include "MolBondType.hh" 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
extern void InitialSetOfMolBondEncodeDecodeRoutines();
extern void AddMolBondClasses(DataSetOfObjectsClass& set);
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif





