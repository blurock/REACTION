/*  FILE     ThermoProps.hh
**  PACKAGE  ThermoProps
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Prototypes for the "ThermoProps" package in the Reaction environment
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/
 
#ifndef Reaction_THERMOPROPS_HH
#define Reaction_THERMOPROPS_HH

#define THERMO_BASE     50060
#define THERMO_PROPERTY_ID    THERMO_BASE + 1
#define THERMO_CHEMKIN_ID     THERMO_BASE + 2
#define THERMO_BENSON_ID      THERMO_BASE + 3
#define THERMO_CONVERSION_ID  THERMO_BASE + 4
#define THERMO_CONVSET_ID     THERMO_BASE + 5
#define THERMO_SINGLE_ID      THERMO_BASE + 6
#define THERMO_LIT_ID         THERMO_BASE + 7
#define THERMO_REAL_ID        THERMO_BASE + 8
#define THERMO_POLY_ID        THERMO_BASE + 9
#define THERMO_THERMVALS_ID   THERMO_BASE + 10
#define THERMO_CHEMKINCONV_ID THERMO_BASE + 11

#define THERMO_CONVERSION_NAME  "ConversionFactors"
#define THERMO_CONVSET_NAME     "ConversionSet"
#define THERMO_PROPERTY_NAME    "ThermoProperty"
#define THERMO_CHEMKIN_NAME     "ChemkinThermo"
#define THERMO_BENSON_NAME      "BensonThermo"
#define THERMO_SINGLE_NAME      "SingleConversionSet"
#define THERMO_LIT_NAME         "LiteratureReference"
#define THERMO_REAL_NAME        "RealBasedProperty"
#define THERMO_POLY_NAME        "PolynomialCp"
#define THERMO_THERMVALS_NAME   "ThermoValuesAlgorithm"
#define THERMO_CHEMKINCONV_NAME "ConvertToChemkin"


/*I  . . . INCLUDES . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include "ThermoPropsType.hh"


/*P  . . . PROTOTYPES . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/

void InitialSetOfThermoPropsDecodeFunctions();
void AddThermPropClasses(DataSetOfObjectsClass& set);
String& FillStringToLength(const unsigned int length, const String& original);

#endif
