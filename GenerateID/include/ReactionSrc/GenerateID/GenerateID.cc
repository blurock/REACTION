/*  FILE     GenerateID.cc
**  PACKAGE  GenerateID
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "GenerateID" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "Reaction/Molecule.hh"
#include "Reaction/MolProps.hh"
#include "Reaction/MolValence.hh"
#include "Reaction/MolStats.hh"
#include  "Reaction/GenerateID.hh"

/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
template list<Identify>::iterator find_if(list<Identify>::iterator, list<Identify>::iterator,
					  IdentifyNameOfObject);


 

 
