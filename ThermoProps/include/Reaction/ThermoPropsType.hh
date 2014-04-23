/*  FILE     ThermoPropsType.hh
**  PACKAGE  ThermoProps
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "ThermoProps" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_THERMOPROPSTYPE_HH
#define Reaction_THERMOPROPSTYPE_HH

class RxnThermoPropertyClass;
class RxnSingleConversionSetClass;
class RxnConversionSetClass;
class RxnConversionFactorsClass;
class RxnDataBensonThermo;
class RxnChemkinThermoClass;
/*C RxnDataLiteratureReference  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the LiteratureReference class definitions
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataLiteratureReference : public BaseDataObject
{
  String Source;
  String Author;
  String Title;
public:
  RxnDataLiteratureReference();
  RxnDataLiteratureReference(const RxnDataLiteratureReference& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  String& getSource();
  String& getAuthor();
  String& getTitle();
  void setReference(RxnDataLiteratureReference *);
};
/*C RxnLiteratureReferenceClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnLiteratureReferenceClass : public DataObjectClass
{
public:
  RxnLiteratureReferenceClass();
  RxnLiteratureReferenceClass(const RxnLiteratureReferenceClass& data);
  RxnLiteratureReferenceClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataConversionFactors  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ConversionFactors class definitions
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataConversionFactors : public BaseDataObject
{
  double AdditiveFactor;
  double MultiplicativeFactor;
public:
  RxnDataConversionFactors();
  RxnDataConversionFactors(const RxnDataConversionFactors& data);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  double Convert(bool normal, double value);
  RxnDataConversionFactors *ComputeInverseFactor(RxnConversionFactorsClass *factorclass);
};
/*C RxnConversionFactorsClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnConversionFactorsClass : public DataObjectClass
{
  String InversePrefix;
public:
  RxnConversionFactorsClass();
  RxnConversionFactorsClass(const RxnConversionFactorsClass& data);
  RxnConversionFactorsClass(const int id, 
			    const String& name,
			    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  String& getInversePrefix();  
};
/*C RxnDataSingleConversionSet  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the SingleConversionSet class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataSingleConversionSet : public BaseDataSetOfObjects
{
public:
  RxnDataSingleConversionSet();
  RxnDataSingleConversionSet(const RxnDataSingleConversionSet& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  bool AddConversionFactor(RxnDataConversionFactors *factors,
			   RxnConversionFactorsClass *factorclass);
  bool AddInverseFactor(RxnDataConversionFactors *factor,
		       RxnConversionFactorsClass *factorclass);
  double Convert(bool normal,String& name,
		 RxnSingleConversionSetClass *convclass, double value);
  double Convert(bool normal,BaseDataKeyWords& keys,
		 RxnSingleConversionSetClass *convclass, double value);
  double Convert(bool normal,
		 BaseDataKeyWords& keys,BaseDataKeySet& filter,
		 RxnSingleConversionSetClass *convclass, double value);
};
/*C RxnSingleConversionSetClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnSingleConversionSetClass : public DataSetOfObjectsClass
{
  RxnConversionFactorsClass *ConversionClass;  
public:
  RxnSingleConversionSetClass();
  RxnSingleConversionSetClass(const RxnSingleConversionSetClass& data);
  RxnSingleConversionSetClass(const int id, 
			      const String& name,
			      const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  double FunctionalConversions(bool normal,String& func, double value);
  RxnConversionFactorsClass *getConversionClass();
  DataSetOfObjectsClass *PointerToAllowedClasses();
};
/*C RxnDataConversionSet  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ConversionSet class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataConversionSet : public BaseDataSetOfObjects
{
  BaseDataSetOfObjects Objects;
  BaseDataKeyWords StandardUnits;
public:
  RxnDataConversionSet();
  RxnDataConversionSet(const RxnDataConversionSet& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  double ReadRealProperty(istream& in,String& propertyname,RxnConversionSetClass *factorclass);
  double ConvertPropertyToUnits(bool tounits,
				String& propertyname,
				double value,
				RxnConversionSetClass *factorclass);
  bool AddPropertyUnits(BaseDataKeySet *keys);
  bool AddConversion(RxnDataSingleConversionSet *factorset);
  double Convert(bool convertto, 
		 BaseDataKeyWords &units, 
		 RxnConversionSetClass *convclass,
		 double value);
  double Convert(bool convertto, 
		 BaseDataKeyWords &units,BaseDataKeySet& filter,
		 RxnConversionSetClass *convclass,double value);
  double ConvertPropertyToUnits(bool tounits,
				String& propertyname,BaseDataKeySet& filter,
				double value,RxnConversionSetClass *factorclass);
};
/*C RxnConversionSetClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnConversionSetClass : public DataSetOfObjectsClass
{
  RxnSingleConversionSetClass *ConversionUnitClass;
public:
  RxnConversionSetClass();
  RxnConversionSetClass(const RxnConversionSetClass& data);
  RxnConversionSetClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  RxnSingleConversionSetClass *getConversionUnitClass();
  DataSetOfObjectsClass *PointerToAllowedClasses();
};
/*C RxnDataRealBasedProperty  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the RealBasedProperty class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataRealBasedProperty : public BaseDataSetOfObjects
{
  RxnDataLiteratureReference *Reference;
public:
  RxnDataRealBasedProperty();
  RxnDataRealBasedProperty(const RxnDataRealBasedProperty& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  RxnDataLiteratureReference *getReference();
  void setReference(RxnDataLiteratureReference *ref);
};
/*C RxnRealBasedPropertyClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnRealBasedPropertyClass : public DataSetOfObjectsClass
{
  RxnDataConversionSet *Conversions;
  RxnConversionSetClass *ConversionsClass;
  RxnLiteratureReferenceClass *ReferenceClass;
public:
  RxnRealBasedPropertyClass();
  RxnRealBasedPropertyClass(const RxnRealBasedPropertyClass& data);
  RxnRealBasedPropertyClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  bool addConversion(RxnDataSingleConversionSet *set);
  double Convert(bool converto,BaseDataKeyWords& units, double value);
  double Convert(bool convertto,String& property, double value);
  double Convert(bool convertto,String& property, BaseDataKeySet& filter, double value);
  RxnLiteratureReferenceClass *getReferenceClass();
  DataSetOfObjectsClass *PointerToAllowedClasses();
};


/*C RxnDataThermoProperty  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ThermoProperty class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataThermoProperty : public RxnDataRealBasedProperty
{
  double StandardEntropy;
  double StandardEnthalpy;
public:
  RxnDataThermoProperty();
  RxnDataThermoProperty(const RxnDataThermoProperty& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  virtual double CalculateHeatCapacityDerivative(RxnThermoPropertyClass *classinfo,
						 const double& Temper);
  virtual double CalculateHeatCapacity(RxnThermoPropertyClass *classinfo,
				       const double& Temper);
  virtual double CalculateEntropy(RxnThermoPropertyClass *classinfo,
				  const double& Temper);
  virtual double CalculateEnthalpy(RxnThermoPropertyClass *classinfo,
				  const double& Temper);
  virtual double CalculateFreeEnergy(RxnThermoPropertyClass *classinfo,
				     const double& Temper);
  virtual double CalculateEquilibrium(RxnThermoPropertyClass *classinfo,
				      const double& Temper);
  virtual double getStandardEnthalpy(RxnThermoPropertyClass *classinfo);
  virtual double getStandardEntropy(RxnThermoPropertyClass *classinfo);
  virtual void setStandardEnthalpy(double enthalpy, 
				   RxnThermoPropertyClass *classinfo);
  virtual void setStandardEntropy(double entropy,
				  RxnThermoPropertyClass *classinfo);
  virtual BaseDataDoubleMatrix *CalculateThermoUnderUnits(BaseDataKeyWords *propertyunits,
							  RxnThermoPropertyClass *classinfo,
							  BaseDataDoubleVector *temperatures);
};
/*C RxnThermoPropertyClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnThermoPropertyClass : public RxnRealBasedPropertyClass
{
  double StandardTemperature;
  double GasConstant;
public:
  RxnThermoPropertyClass();
  RxnThermoPropertyClass(const RxnThermoPropertyClass& data);
  RxnThermoPropertyClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  double getStandardTemperature();
  double getGasConstant();
};
/*C RxnDataChemkinThermo  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ChemkinThermo class definitions
**
**  REMARKS
**    Inheirits BaseDataThermoProperty
*/
class RxnDataChemkinThermo : public RxnDataThermoProperty
{
public:
     String SpeciesName;
     String Date;
     String AtSymbAndFormula;
     String PhaseDescriptor;
     String FormulaDescriptor;
     
     double LowerBoundTemp;
     double UpperBoundTemp;
     double CommonTemp;
     
     int SimplyOne;
     int SimplyTwo;
     int SimplyThree;
     int SimplyFour;
     
     double y;
     
     VectorNumeric UpperCoeff;
     VectorNumeric LowerCoeff;
  RxnDataChemkinThermo();
  RxnDataChemkinThermo(const RxnDataChemkinThermo& data);
  RxnDataChemkinThermo(String& cpunits,
		       String& enthalpyunits,
		       String& entropyunits,
		       RxnDataSimpleMolecule *molecule,
		       RxnSimpleMoleculeClass *molclass,
		       RxnDataThermoProperty *data,
		       RxnThermoPropertyClass *thermoclass);
  RxnDataChemkinThermo(String& name,
		       String& cpunits,
		       String& enthalpyunits,
		       String& entropyunits,
		       RxnDataSimpleMolecule *molecule,
		       RxnSimpleMoleculeClass *molclass,
		       RxnDataThermoProperty *data,
		       RxnThermoPropertyClass *thermoclass);
  RxnDataChemkinThermo(String& cpunits,
		       String& enthalpyunits,
		       String& entropyunits,
		       RxnDataThermoProperty *data,
		       RxnThermoPropertyClass *thermoclass);
  void FillInChemkinInfo(String& cpunits,
			 String& enthalpyunits,
			 String& entropyunits,
			 RxnDataThermoProperty *data,
			 RxnThermoPropertyClass *thermoclass);
  void CalculateAtSymbAndFormula(RxnDataSimpleMolecule *molecule,
				 RxnSimpleMoleculeClass *molclass);
  void CalculateLowerA6(RxnDataThermoProperty *data,RxnThermoPropertyClass *thermoclass, String& enthalpyunits);
  void CalculateUpperA6(RxnDataThermoProperty *data,RxnThermoPropertyClass *thermoclass, String& enthalpyunits);
  void CalculateLowerA7(RxnDataThermoProperty *data,
			RxnThermoPropertyClass *thermoclass,
			String& entropyunits);
  void CalculateUpperA7(RxnDataThermoProperty *data,
			RxnThermoPropertyClass *thermoclass,
			String& entropyunits);
		   
  STANDARD_VIRTUAL_METHODS_OBJECT;
  virtual double CalculateHeatCapacity(RxnThermoPropertyClass *classinfo,
				       const double& Temper);
  virtual double CalculateEntropy(RxnThermoPropertyClass *classinfo,
				  const double& Temper);
  virtual double CalculateEnthalpy(RxnThermoPropertyClass *classinfo,
				  const double& Temper);
  void ComputeCoeffs(VectorNumeric &ChemkinTemps,
		     String& cpunits,
		     RxnDataThermoProperty *data,
		     RxnThermoPropertyClass *thermoclass,
		     VectorNumeric &Coeffs);
  bool ReadBasicChemkinElement(istream& in,
			       RxnChemkinThermoClass *chemclass);
  String& getSpeciesName();
  ostream& WriteAsNASAPolynomial(ostream& out) const;
};
/*C RxnChemkinThermoClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataThermoPropertyClass
*/
class RxnChemkinThermoClass : public RxnThermoPropertyClass
{
public:
  RxnChemkinThermoClass();
  RxnChemkinThermoClass(const RxnChemkinThermoClass& data);
  RxnChemkinThermoClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataBensonThermo  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the BensonThermo class definitions
**
**  REMARKS
**    Inheirits BaseDataThermoProperty
*/
class RxnDataBensonThermo : public RxnDataThermoProperty
{
  String BensonFormulaDescriptor;
  VectorNumeric CpS;
  vector<int> GapTrack;             
  int PolyFlag;     
  
  double CommonBensonTemp;
 public:
  RxnDataBensonThermo();
  RxnDataBensonThermo(const RxnDataBensonThermo& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  virtual double CalculateHeatCapacityDerivative(RxnThermoPropertyClass *bensonclass,
						 const double& Temper);
  virtual double CalculateHeatCapacity(RxnThermoPropertyClass *bensonclass,
				       const double& Temper);
  virtual double CalculateEntropy(RxnThermoPropertyClass *classinfo,
				  const double& Temper);
  virtual double CalculateEnthalpy(RxnThermoPropertyClass *classinfo,
				  const double& Temper);
  void FillHeatCapacityGaps(RxnThermoPropertyClass *bensonclass,
			    const VectorNumeric& TempsForExistData,
			    const VectorNumeric& ExistData);
};
/*C RxnBensonThermoClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataThermoPropertyClass
*/
class RxnBensonThermoClass : public RxnThermoPropertyClass
{
  VectorNumeric Temps;
public:
  RxnBensonThermoClass();
  RxnBensonThermoClass(const RxnBensonThermoClass& data);
  RxnBensonThermoClass(const int id, 
		    const String& name,
		    const String& descr);
  VectorNumeric& getTemperatures();
  double getTemperature(unsigned int i);
  unsigned int getNumberOfTemperatures();
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataPolynomialCp  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the PolynomialCp class definitions
**
**  REMARKS
**    Inheirits BaseDataThermoProperty
*/
class RxnDataPolynomialCp : public RxnDataThermoProperty
{
  VectorNumeric Cps;
 public:
  RxnDataPolynomialCp();
  RxnDataPolynomialCp(const RxnDataPolynomialCp& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  virtual double CalculateHeatCapacity(RxnThermoPropertyClass *classinfo,
				       const double& Temper);
  virtual double CalculateEntropy(RxnThermoPropertyClass *classinfo,
				  const double& Temper);
  virtual double CalculateEnthalpy(RxnThermoPropertyClass *classinfo,
				  const double& Temper);
};
/*C RxnPolynomialCpClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataThermoPropertyClass
*/
class RxnPolynomialCpClass : public RxnThermoPropertyClass
{
public:
  RxnPolynomialCpClass();
  RxnPolynomialCpClass(const RxnPolynomialCpClass& data);
  RxnPolynomialCpClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataThermoValuesAlgorithm  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ThermoValuesAlgorithm class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataThermoValuesAlgorithm : public BaseDataAlgorithmOperation
{
  String ValueUnitsS;
  BaseDataKeyWords *ValueUnits;
  String instanceNameListS;
  BaseDataKeyWords *instanceNameList;
  String temperaturesS;
  BaseDataDoubleVector *temperatures;

  String matrixOutS;
public:
  RxnDataThermoValuesAlgorithm();
  RxnDataThermoValuesAlgorithm(const RxnDataThermoValuesAlgorithm& data);
  
  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C RxnThermoValuesAlgorithmClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnThermoValuesAlgorithmClass : public DataAlgorithmOperationClass
{
public:
  RxnThermoValuesAlgorithmClass();
  RxnThermoValuesAlgorithmClass(const RxnThermoValuesAlgorithmClass& data);
  RxnThermoValuesAlgorithmClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataConvertToChemkin  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ConvertToChemkin class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmObject
*/
class RxnDataConvertToChemkin : public BaseDataAlgorithmOperation
{
  String instanceNameListS;
  BaseDataKeyWords *instanceNameList;
  String thermoInfoS;
  BaseDataKeyWords *thermoInfo;
  String unitsS;
  BaseDataKeyWords *unitkeys;

  String chemkinS;
  String chemkinNameS;
  String thermoS;
  String cpunits;
  String enthalpyunits;
  String entropyunits;
  String moleculeS;

public:
  RxnDataConvertToChemkin();
  RxnDataConvertToChemkin(const RxnDataConvertToChemkin& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C RxnConvertToChemkinClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmObjectClass
*/
class RxnConvertToChemkinClass : public DataAlgorithmOperationClass
{
public:
  RxnConvertToChemkinClass();
  RxnConvertToChemkinClass(const RxnConvertToChemkinClass& data);
  RxnConvertToChemkinClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
