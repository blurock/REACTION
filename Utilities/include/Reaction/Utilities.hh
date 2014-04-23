/*  FILE     Utilities.hh
**  PACKAGE  Utilities
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "Utilities" package in the Reaction environment
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
 
#ifndef Reaction_UTILITIES_HH
#define Reaction_UTILITIES_HH

#define RXNUTIL_BASE           50120
#define RXNUTIL_MOVE_ID        RXNUTIL_BASE + 1
#define RXNUTIL_STORE_ID       RXNUTIL_BASE + 2
#define RXNUTIL_RETRIEVE_ID    RXNUTIL_BASE + 3

#define RXNUTIL_MOVE_NAME      "MoveData"
#define RXNUTIL_STORE_NAME     "StoreSetOfObjectsProperty"
#define RXNUTIL_RETRIEVE_NAME  "RetrieveSetOfObjectsProperty"


/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "UtilitiesType.hh"


/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
void InitialRxnUtilitiesDecodeFunctions();
void AddRxnUtilitiesClasses(DataSetOfObjectsClass& set);



#endif
