/*  FILE     FullReaction.hh
**  PACKAGE  FullReaction
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "FullReaction" package in the Reaction environment
**
**  COPYRIGHT (C) 2000 Edward S. Blurock
*/
 
#ifndef Reaction_FULLREACTION_HH
#define Reaction_FULLREACTION_HH

/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "FullSystem.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Dbase.hh"
#include "Molecule.hh"
//#include "SECharge.hh"
#include "ThermoProps.hh"
#include "Utilities.hh"
#include "Rxn.hh"
#include "EquilibriumConst.hh"
#include "Mechanism.hh"
#include "MechanismGraph.hh"
#include "MolStats.hh"
#include "MechLumping.hh"
#include "Flame.hh"
#include "FullReactionType.hh"

/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
void InitialFullReactionDecodeFunctions();
void AddFullReactionClasses(DataSetOfObjectsClass& set);

#endif
