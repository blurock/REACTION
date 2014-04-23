/*  FILE     Dbase.hh
**  PACKAGE  Dbase
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "Dbase" package in the CoreObjects environment
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
 
#ifndef CoreObjects_DBASE_HH
#define CoreObjects_DBASE_HH

#define DBASE_BASE    50005

#define DBASE_INFO_ID    DBASE_BASE + 1
#define DBASE_INFO_NAME      "DataBaseInformation"
 
#define DBASE_DEFAULT_PERMISSIONS 438
#define DBASE_DEFAULT_BLOCKSIZE   0

/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "gdbm.h"

#include "DbaseType.hh"


/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
extern void InitialSetOfDBaseEncodeDecodeRoutines();
extern void AddDBaseClasses(DataSetOfObjectsClass& set);

#endif
