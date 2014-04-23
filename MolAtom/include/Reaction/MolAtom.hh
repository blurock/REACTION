/*  FILE     MolAtom.hh
**  PACKAGE  MolAtom
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "MolAtom" package in the Reaction environment
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
 
#ifndef Reaction_MOLATOM_HH
#define Reaction_MOLATOM_HH

#define MOLATOM_BASE    50010
#define MOLATOM_BASIC_NAME     "BasicAtomData"
#define MOLATOM_MOLFILE_NAME   "MolFileAtom"
#define MOLATOM_MOLATOM_NAME   "MoleculeAtom"
//#define MOLATOM_BASIC_NAME   ""

#define MOLATOM_BASIC_ID     MOLATOM_BASE + 1
#define MOLATOM_MOLFILE_ID   MOLATOM_BASE + 2
#define MOLATOM_MOLATOM_ID   MOLATOM_BASE + 3

#define VAL_SINGLE_BOND_COUNT  1     
#define VAL_DOUBLE_BOND_COUNT  10     
#define VAL_TRIPLE_BOND_COUNT  100     
#define VAL_CHARGE_SPEC        1000
#define VAL_AROMATIC_SPEC      10000
#define VAL_LONE_PAIR_COUNT    20000
#define VAL_HYDROGEN_COUNT     100000
#define VAL_ATOMIC_NUMBER      1000000

#define META_ATOM_OFFSET      200
#define MAX_NUMBER_OF_META_ATOMS  20


/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "MolAtomType.hh"


/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
extern void InitialSetOfMolAtomEncodeDecodeRoutines();
extern void AddMolAtomClasses(DataSetOfObjectsClass& set);

/*I Includes  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/

#endif
