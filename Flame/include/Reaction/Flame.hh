/*  FILE     Flame.hh
**  PACKAGE  Flame
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "Flame" package in the CoreObjects environment
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
 
#ifndef CoreObjects_FLAME_HH
#define CoreObjects_FLAME_HH

#define FLAME_BASE            40000
#define FLAME_DATA_ID         FLAME_BASE + 1
#define FLAME_DATA_NAME       "FlameSensitivityData"

/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "FlameType.hh"

/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/

extern void FlameEncodeDecodeRoutines();
void AddFlameClasses(DataSetOfObjectsClass& set);

#endif
