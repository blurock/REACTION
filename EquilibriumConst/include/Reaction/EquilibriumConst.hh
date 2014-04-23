/*  FILE     EquilibriumConst.hh
**  PACKAGE  EquilibriumConst
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "EquilibriumConst" package in the Reaction environment
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
 
#ifndef Reaction_EQUILIBRIUMCONST_HH
#define Reaction_EQUILIBRIUMCONST_HH

#define EQUILIBRIUM_BASE         50100
#define EQUIL_CONSTANT_NAME      "EquilibriumConstant"
#define EQUIL_OPERATION_NAME     "CalculateReverseRate"
#define EQUIL_EQUILIBRIUM_NAME   "OperationMoleculeEquilibrium"

#define EQUIL_CONSTANT_ID        EQUILIBRIUM_BASE + 1
#define EQUIL_OPERATION_ID       EQUILIBRIUM_BASE + 2
#define EQUIL_EQUILIBRIUM_ID     EQUILIBRIUM_BASE + 3

/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "EquilibriumConstType.hh"


/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/

void AddEquilibriumConstClasses(DataSetOfObjectsClass& set);
void InitialSetOfEquilibriumConstDecodeFunctions();

#endif
