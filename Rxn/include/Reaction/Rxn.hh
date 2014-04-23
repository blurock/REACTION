/*  FILE     Rxn.hh
**  PACKAGE  Rxn
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "Rxn" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_RXN_HH
#define REACTION_RXN_HH
 
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "RxnType.hh"
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define REACTION_BASE   50040
#define REACTION_RATES_NAME        "ReactionRates"
#define REACTION_THIRDBODY_NAME    "ThirdBody"
#define REACTION_REACTION_NAME     "Reaction"
#define REACTION_CORRS_NAME        "ReactionMoleculeCorrespondences"
#define REACTION_DBASE_NAME        "ReactionStructuresDataBase"
#define REACTION_RXNFILE_NAME      "ReactionRatesRxnFile"
#define REACTION_HILOW_NAME        "ReactionRateHiLow"

#define REACTION_RATES_ID          REACTION_BASE + 1
#define REACTION_THIRDBODY_ID      REACTION_BASE + 2
#define REACTION_REACTION_ID       REACTION_BASE + 3
#define REACTION_CORRS_ID          REACTION_BASE + 4
#define REACTION_DBASE_ID          REACTION_BASE + 5
#define REACTION_RXNFILE_ID        REACTION_BASE + 6
#define REACTION_HILOW_ID          REACTION_BASE + 7


#define RATE_KEYWORD_LOG10     "logA"
#define RATE_KEYWORD_LOGE      "elogA"

void InitialSetOfRxnDecodeFunctions();
void AddRxnClasses(DataSetOfObjectsClass& set);
int FillReaction(ReactionSystemBase* sys);
bool FillReactions(BaseDataSetOfInstances& instances,
		   String& dbasetype,
		   RxnDataMolecularStructuresDataBase *dbase,
		   RxnMolecularStructuresDataBaseClass *dbaseclass,
		   RxnReactionClass *rxnclass,
		   BaseDataKeyWords *keynames);

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#endif


