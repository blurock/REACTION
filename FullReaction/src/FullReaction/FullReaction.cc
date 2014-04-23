/*  FILE     FullReaction.cc
**  PACKAGE  FullReaction
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "FullReaction" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
using namespace std;
#define TEMPLATE_INSTANTIATION
#include "FullReaction.hh"

void ReactionSystemSave::StandardObjectsSetUp()
    {
      AnalysisSystemSave::StandardObjectsSetUp();

      AddRxnUtilitiesClasses(getStandard());
      AddMechanismGraphClasses(getStandard());
      AddMolStatsClasses(getStandard());
      AddStaticAtomClasses(getStandard());
      AddMoleculeClasses(getStandard());
      AddMolAtomClasses(getStandard());
      AddMolBondClasses(getStandard());
      AddDBaseClasses(getStandard());
      AddThermPropClasses(getStandard());
      AddMechanismClasses(getStandard());
      AddRxnClasses(getStandard());
      AddEquilibriumConstClasses(getStandard());
      AddMechLumpingClasses(getStandard());
    }
void ReactionSystemSave::EncodeDecodeObjectsSetUp()
    {
      AnalysisSystemSave::EncodeDecodeObjectsSetUp();
      InitialRxnUtilitiesDecodeFunctions();
      InitialMechanismGraphDecodeFunctions();
      InitialSetOfMolStatsDecodeFunctions();
      InitialSetOfStaticAtomEncodeDecodeRoutines();
      InitialSetOfMoleculeEncodeDecodeRoutines();
      InitialSetOfMolAtomEncodeDecodeRoutines();
      InitialSetOfMolBondEncodeDecodeRoutines();
      InitialSetOfDBaseEncodeDecodeRoutines();
      InitialSetOfThermoPropsDecodeFunctions();
      InitialSetOfMechanismDecodeFunctions();
      InitialSetOfRxnDecodeFunctions();
      InitialSetOfEquilibriumConstDecodeFunctions();
      InitialMechLumpingDecodeFunctions();
    }
/*F CommandSetUp()  . . . . . . . . . . . . . . . . . . . . .  InstanceSystem
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void ReactionSystemSave::CommandSetUp()
{
  AlgorithmSystemSave::CommandSetUp();


}
/*F Initialization()  . . . . . . . . . . . . . . . . . . . . .  InstanceSystem
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void ReactionSystemSave::Initialization()
{
  AlgorithmSystemSave::Initialization();
}

/*S Utilities
 */
/*F InitializeFullReactionDecodeFunctions()  . . . . . . . . . . . . Reactions
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialFullReactionDecodeFunctions()
{
}
/*F AddFullReactionClasses(set) . . . . . . . . . . . .  EquilibriumConst
**
**  DESCRIPTION
**    set: The set of classes to add them to
**
**  REMARKS
**
*/
void AddFullReactionClasses(DataSetOfObjectsClass& set)
{
}
