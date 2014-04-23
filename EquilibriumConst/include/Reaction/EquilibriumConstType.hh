/*  FILE     EquilibriumConstType.hh
**  PACKAGE  EquilibriumConst
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "EquilibriumConst" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_EQUILIBRIUMCONSTTYPE_HH
#define Reaction_EQUILIBRIUMCONSTTYPE_HH

/*C RxnDataEquilibriumConstant  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the EquilibriumConstant class definitions
**
**  REMARKS
**    Inheirits BaseDataReactionRates
*/
class RxnDataEquilibriumConstant : public RxnDataReactionRates
{
public:
  RxnDataEquilibriumConstant();
  RxnDataEquilibriumConstant(const RxnDataEquilibriumConstant& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnEquilibriumConstantClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataReactionRatesClass
*/
class RxnEquilibriumConstantClass : public RxnReactionRatesClass
{
public:
  RxnEquilibriumConstantClass();
  RxnEquilibriumConstantClass(const RxnEquilibriumConstantClass& data);
  RxnEquilibriumConstantClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataCalculateReverseRate  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the CalculateReverseRate class definitions
**
**  REMARKS
**    Inheirits BaseDataOperation
*/
class RxnDataCalculateReverseRate : public BaseDataOperation
{
  RxnDataEquilibriumConstant *Unity;
  String MolThermoName;
  String EquilibriumUnits;
  String RateUnits;

public:
  RxnDataCalculateReverseRate();
  RxnDataCalculateReverseRate(const RxnDataCalculateReverseRate& data);
  ~RxnDataCalculateReverseRate();
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_OPERATION_METHODS;
  RxnDataEquilibriumConstant *EquilibriumMultMolecule(String& property,
						      RxnDataEquilibriumConstant *reactequ,
						      RxnEquilibriumConstantClass *equclass,
						      RxnDataSimpleMolecule *mol);
};
/*C RxnCalculateReverseRateClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataOperationClass
*/
class RxnCalculateReverseRateClass : public DataOperationClass
{
  RxnEquilibriumConstantClass *EquilibriumClass;
public:
  RxnCalculateReverseRateClass();
  RxnCalculateReverseRateClass(const RxnCalculateReverseRateClass& data);
  RxnCalculateReverseRateClass(const int id, 
		    const String& name,
		    const String& descr);
  ~RxnCalculateReverseRateClass();
  STANDARD_VIRTUAL_METHODS_CLASS;
  RxnEquilibriumConstantClass *getEquilibriumClass();
};
/*C RxnDataOperationMoleculeEquilibrium  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the OperationMoleculeEquilibrium class definitions
**
**  REMARKS
**    Inheirits BaseDataOperation
*/
class RxnDataOperationMoleculeEquilibrium : public BaseDataOperation
{
  String ThermodynamicConstantsS;
  String EquilibriumTypeS;
  String GCUnits;
  VectorNumeric Temperatures;
public:
  RxnDataOperationMoleculeEquilibrium();
  RxnDataOperationMoleculeEquilibrium(const RxnDataOperationMoleculeEquilibrium& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_OPERATION_METHODS;
};
/*C RxnOperationMoleculeEquilibriumClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataOperationClass
*/
class RxnOperationMoleculeEquilibriumClass : public DataOperationClass
{
  BaseDataDoubleVector Temperatures;
public:
  RxnOperationMoleculeEquilibriumClass();
  RxnOperationMoleculeEquilibriumClass(const RxnOperationMoleculeEquilibriumClass& data);
  RxnOperationMoleculeEquilibriumClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};




/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
