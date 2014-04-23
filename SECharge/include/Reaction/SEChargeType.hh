/*  FILE     SEChargeType.hh
**  PACKAGE  SECharge
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "SECharge" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_SECHARGETYPE_HH
#define Reaction_SECHARGETYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
/*C RxnDataCalculateElectronegativity  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the CalculateElectronegativity class definitions
**
**  REMARKS
**    Inheirits BaseDataOperation
*/
class RxnDataCalculateElectronegativity : public BaseDataOperation
{
public:
  RxnDataCalculateElectronegativity();
  RxnDataCalculateElectronegativity(const RxnDataCalculateElectronegativity& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_OPERATION_METHODS;
};
/*C RxnCalculateElectronegativityClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataOperationClass
*/
class RxnCalculateElectronegativityClass : public DataOperationClass
{
public:
  RxnCalculateElectronegativityClass();
  RxnCalculateElectronegativityClass(const RxnCalculateElectronegativityClass& data);
  RxnCalculateElectronegativityClass(const int id, 
				     const String& name,
				     const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataEletronegativityAlgorithm  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the EletronegativityAlgorithm class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithm
*/
class RxnDataEletronegativityAlgorithm : public BaseDataAlgorithmOperation
{
  String moleculeNamesS;
  BaseDataKeyWords *moleculeNames;

public:
  RxnDataEletronegativityAlgorithm();
  RxnDataEletronegativityAlgorithm(const RxnDataEletronegativityAlgorithm& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C RxnEletronegativityAlgorithmClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmClass
*/
class RxnEletronegativityAlgorithmClass : public DataAlgorithmOperationClass
{
public:
  RxnEletronegativityAlgorithmClass();
  RxnEletronegativityAlgorithmClass(const RxnEletronegativityAlgorithmClass& data);
  RxnEletronegativityAlgorithmClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
#endif
