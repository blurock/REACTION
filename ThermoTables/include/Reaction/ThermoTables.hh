/*  FILE     ThermoTables.hh
**  PACKAGE  ThermoTables
**  AUTHOR   Yuri Tsybukh, Edward S. Blurock
**
**  CONTENT
**    Definitions for the "ThermoTables" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_THERMOTABLES_HH
#define REACTION_THERMOTABLES_HH
 
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define COMMON_BENSON_T 1000.00
#define IS_GAP 0
#define NO_GAP 1
#define GAP_FILL_CHAR '*'
#define LOWER_TEMPS { 300.00, 500.00, 800.00, 1000.00 }
#define UPPER_TEMPS { 1300.00, 1500.00, 1700.00, 1000.00 }
#define COMMON_TEMP 1000.00     
#define STANDARD_TEMP 298.00

#define THERMO_TEX      1
#define THERMO_ASCII    2
#define THERMO_HTML     3

#define GAS_CONSTANT 1.98717

class ChemkinBaseTableObject;
class CalculateChemkinHeatCapacity;
class CalculateBensonHeatCapacity;

#include "ThermoTablesType.hh"

/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
double CalculateEntropy(ChemkinBaseTableObject& chemkin, double temperature);
double CalculateEnthalpy(ChemkinBaseTableObject& chemkin, double temperature);
double CalculateEntropy(BensonBaseTableObject& benson, double temperature);
double CalculateEnthalpy(BensonBaseTableObject& benson, double temperature);

template <class ThermoObject>
double CalculateFreeEnergyChange(ThermoObject& thermo, double temperature);
template <class ThermoObject>
double CalculateEquilibrium(ThermoObject& therm, double temperature);
template <class ThermoObject>
ObjectList<double> CalculateEquilibrium(ThermoObject& therm,
				       ObjectList<double> temperatures);
MatrixNumeric KTemperatureMatrix(ObjectList<double> temperatures);
template <class ThermObject>
VectorNumeric LogEquilibrium(ThermObject& therm, ObjectList<double> temperatures);
template <class ThermObject>
istream& operator>>(istream &stream, BensonBaseTableObject& ThermoObj);
istream& operator>>(istream &stream, ChemkinBaseTableObject& ThermoObj);

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#endif
