/*  FILE     Senkin.hh
**  PACKAGE  Senkin
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "Senkin" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_SENKIN_HH
#define REACTION_SENKIN_HH
 
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#include "SenkinType.hh"

/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
void FormAnalysisTimeMatrix(ObjectList<DbaseReaction>& rxns,
			    const MechanismSenkinDataPoints points,
			    const String outroot,
			    const int skip);
MatrixNumeric FormRxnVersusTimeMatrix(ObjectList<DbaseReaction>& rxns,
				      const MechanismSenkinDataPoints points,
				      const int skip);
ChemkinBaseTableObject GetChemkinTableObject(SimpleMolecule& molecule,
					     AtomInformation& atominfo);
String ChemkinMoleculeName(SimpleMolecule& mol);



/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/


#endif


