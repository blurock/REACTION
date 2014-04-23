/*  FILE     StaticAtoms.hh
**  PACKAGE  StaticAtoms
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "StaticAtoms" package in the Reaction environment
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
 
#ifndef Reaction_STATICATOMS_HH
#define Reaction_STATICATOMS_HH

#define STATICATOM_BASE         50000
#define STATICATOM_ATOMINFO_NAME      "StaticAtomInfo"
#define STATICATOM_ATOMINFOSET_NAME   "AtomInformation"
#define STATICATOM_METADATA_NAME      "MetaAtomData"
#define STATICATOM_METAATOM_NAME      "CompleteMetaAtom"
#define STATICATOM_ATOMINFO_ID        STATICATOM_BASE + 1
#define STATICATOM_ATOMINFOSET_ID     STATICATOM_BASE + 2
#define STATICATOM_METADATA_ID        STATICATOM_BASE + 3
#define STATICATOM_METAATOM_ID        STATICATOM_BASE + 4


#define META_ATOM_OFFSET      200
#define MAX_POSSIBLE_ATOMIC_NUMBER   150
  
/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "StaticAtomType.hh"


/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
extern void InitialSetOfStaticAtomEncodeDecodeRoutines();
extern void AddStaticAtomClasses(DataSetOfObjectsClass& set);
#endif
