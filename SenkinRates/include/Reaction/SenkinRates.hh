/*  FILE     SenkinRates.hh
**  PACKAGE  SenkinRates
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "SenkinRates" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_SENKINRATES_HH
#define REACTION_SENKINRATES_HH
 
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "SenkinRatesType.hh"
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
void CalculateValuesFromReactions(ObjectList<DbaseReaction>& rxns,
				  FindReactionValues& values,
				  bool compfi,
				  bool takelog);
int SkipMatrixSize(int fullsize, int skip);
void FormAndAddReactionNames(ObjectList<DbaseReaction>& rxns,
			     ObjectList<String>& names,
			     const bool includerev = true);
bool FillTimeTemperatureMatrix(MatrixNumeric& mat,
			       const ObjectList<double>& timeValues,
			       const ObjectList<double>& temps,
			       const int skip);
MatrixNumeric& ReduceMatrixByRowSpecification(MatrixNumeric& mat,
					       const unsigned int initial,
					       const unsigned int ifinal);
MatrixNumeric ReduceMatToTimeInterval(MatrixNumeric& mat,
				       const MechanismSenkinDataPoints points,
				       double initialtime,
				       double finaltime,
				       const bool includerev);

#endif


