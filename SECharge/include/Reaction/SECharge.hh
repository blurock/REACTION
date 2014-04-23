/*  FILE     SECharge.hh
**  PACKAGE  SECharge
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "SECharge" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_SECHARGE_HH
#define REACTION_SECHARGE_HH
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define SECHARGE_BASE   50080
#define SECHARGE_OP_NAME   "CalculateElectronegativity"
#define SECHARGE_OP_ID     SECHARGE_BASE + 1
#define SECHARGE_ALG_NAME  "EletronegativityAlgorithm"
#define SECHARGE_ALG_ID    SECHARGE_BASE + 2

#define LOG int

#ifdef __cplusplus
extern "C" {
#endif
#define INT int
#define FLOAT double
#define CHAR char
#define STRING char
#define LINELENGTH 150
typedef void *VOID;

#include "simple.h"
#include "xdrrpc.h"
#include "dblink.h"

#include "mat.h"
#include "property/asciiprop.h"
#include "molecules/bnd.h"
#include "molecules/mlf.h"
#include "molecules/chrg.h"
#include "bondmat.h"

#include <math.h>
#include <stdio.h>
 
  extern MolFileMolecule *LoadMolFileMolecule(int id,char *name,
					      int natm,int nbnd,
					      double *atmvec,int *bndvec);
  extern float *UnLoadMolFileMolecule(MoleculeElectronic *mlfmol);

  extern MolFileMolecule *AddHydrogens(MolFileMolecule *mlfmol);

  extern MoleculeElectronic *ElectronicFromMolFile(MolFileMolecule *molecule);
  extern void PrintPrettyMoleculeElectronic(CHAR *prefix, FILE *file,
					    MoleculeElectronic *molecule);
  extern void PrintPrettyMolFile(CHAR *prefix, FILE *file,
				 MolFileMolecule *molecule );

  extern void *Malloc(int size);
  extern void *Calloc(int nelem, int elsize);

#ifdef __cplusplus
}
#endif

/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

extern LOG HeteroAtomTest(ConjAtomInfo *atom);
#include "Reaction/SEChargeType.hh"
extern void InitialSetOfSEChargeEncodeDecodeRoutines();
extern void AddSEChargeClasses(DataSetOfObjectsClass& set);
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
