/*  FILE     ThermoTablesType.hh
**  PACKAGE  ThermoTables
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "ThermoTables" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_THERMOTABLESTYPE_HH
#define Reaction_THERMOTABLESTYPE_HH

/*C BensonTable . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Class BensonTable is a auxiliary class providing
**  container and initializing facilities for the class
**  BensonBaseTableObject.
**
**  REMARKS
**
*/
class BensonTable
     {
     
 public:
     
     VectorNumeric Temperatures;

    
     BensonTable();
     
     void ReadTemperatures(istream &stream);
     

     };
/*C BensonBaseTableObject . . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Class BensonBaseTableObject represents a line within Benson
**  standard data table and provides some methods for constructing
**  data lacking in the input table object(line) and also for calculating
**  heat capacities and its derivatives for some particular temperature.
**  Provides conversion facilities from and to Chemkin thermodynamic data 
**  representation. 
**  Flag PolyFlag is used to keep track of the interpolation method for 
**  constructing data was used.  
**
**  REMARKS
**
*/
class BensonBaseTableObject 
     {   

 public:
     
     String BensonFormulaDescriptor;
     VectorNumeric Temps;
     VectorNumeric CpS;
     vector<int> GapTrack;             
     int PolyFlag;     

     double CommonBensonTemp;
     
     double StandardEntropy;
     double StandardEnthalpy;
     
     BensonBaseTableObject();
     BensonBaseTableObject(istream &stream, BensonTable& Table); 
     BensonBaseTableObject(ChemkinBaseTableObject& ThermoObj); 
     BensonBaseTableObject(ChemkinBaseTableObject& ThermoObj, 
			   const VectorNumeric& vec);
       virtual ~BensonBaseTableObject(){};
     virtual void CopyClone(BensonBaseTableObject *benson);
     virtual BensonBaseTableObject *Clone();
     virtual ostream& print(ostream& out) const;
     bool EncodeThis(CommBuffer& buffer);
     bool DecodeThis(CommBuffer& buffer);
     double CalculateHeatCapacity(const double& Temper);
     double CalculateHeatCapacityDerivative(const double& Temper);
     
     void FillHeatCapacityGaps(const VectorNumeric& TempsForExistData,
			       const VectorNumeric& ExistData);     

     };
/*C ChemkinBaseTableObject  . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Class ChemkinBaseTableObject represent Chemkin data table object.
**  Provides facilities for calculating all necessary values of this object.
**  Provides methods for calculating thermodynamic functions basing on the 
**  Chemkin coefficients.   
**
**  REMARKS
**
*/
class ChemkinBaseTableObject 
     {
 public:   

     String SpeciesName;
     String Date;
// to be corrected
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
     
     ChemkinBaseTableObject();
     ChemkinBaseTableObject(ChemkinBaseTableObject& ThermoObj);
     ChemkinBaseTableObject(BensonBaseTableObject& ThermoObj);
     ostream& print(ostream& stream) const;

     bool EncodeThis(CommBuffer& buffer);
     bool DecodeThis(CommBuffer& buffer);
     void NormalizeValue(double& x, const double& factor);
     void NormalizeVector(VectorNumeric& x, const double& factor);
     void ChemkinDenormalize(VectorNumeric& Coeffs, const double& factor);
     
     MatrixNumeric BuildChemkinMatrix(const VectorNumeric& vec);
 
     double CalculateChemkinEnthalpy(const double& Temper);
     double CalculatePartialEnthalpy(const double& Temper);
     double CalculateChemkinEntropy(const double& Temper);
     double CalculatePartialEntropy(const double& Temper);
     double CalculateHeatCapacity(const double& Temper);
     };
 
/*C CpPowerSeries . . . . . . . . . . . . . . Temperature Power series for Cp
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class CpPowerSeries
     {
 public:
     ObjectList<double> Coefficients;
     double UnitsFactor;
     double TempNorm;
     String Source;
     
     CpPowerSeries()
	  {
	  }

     CpPowerSeries(const String Source, const double tnorm, const double units)
	  : UnitsFactor(units),
	  TempNorm(tnorm),
	  Source(Source)
	       {
	       }
     void CopyClone(CpPowerSeries *series)
	  {
	  *this = *series;
	  }
     CpPowerSeries *Clone()
	  {
	  CpPowerSeries *series;
	  series->CopyClone(this);
	  return series;
	  }
     ostream& print(ostream& out) const
	  {
	  out << " [";
	  out << TempNorm;
	  out << UnitsFactor;
	  out << "]: ";
	  Coefficients.print(out);
	  out << ": ";
	  out << Source;
	  return out;
	  }
     bool EncodeThis(CommBuffer& buffer)
	  {
	  bool result = Encode(buffer,Coefficients);
	  result = result && Encode(buffer,UnitsFactor);
	  result = result && Encode(buffer,TempNorm);
	  result = result && Encode(buffer,Source);
	  return result;
	  }
     bool DecodeThis(CommBuffer& buffer)
	  {
	  bool result = Decode(buffer,Coefficients);
	  result = result && Decode(buffer,UnitsFactor);
	  result = result && Decode(buffer,TempNorm);
	  result = result && Decode(buffer,Source);
	  return result;
	  }
     
     };
/*C CalculateChemkinHeatCapacity  . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Class CalculateChemkinHeatCapacity provides functional interface
**  for calculating heat capacities for a set (vector) of temperatures
**  basing on the Chemkin object data. 
** 
**  REMARKS
**
*/
class CalculateChemkinHeatCapacity
{
     public:
	  
	  ChemkinBaseTableObject ThermoObj;  
	  
	  CalculateChemkinHeatCapacity();

	  CalculateChemkinHeatCapacity(ChemkinBaseTableObject& _ThermoObj);
	            
	  double operator()(const double& Temper);
	  
};
/*C CalculateBensonHeatCapacity . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Class CalculateBensonHeatCapacity provides functional interface
**  for calculating heat capacities for a set (vector) of temperatures
**  basing on the Benson object data. 
** 
**  REMARKS
**
*/
class CalculateBensonHeatCapacity
{
     public:
	  BensonBaseTableObject ThermoObj;  
	  CalculateBensonHeatCapacity();
	  CalculateBensonHeatCapacity(BensonBaseTableObject& _ThermoObj);
	  double operator()(const double& Temper);
};
/*C
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class OutputThermoTableStyle
{
  unsigned int Style;
  unsigned int Precision;
  unsigned int Width;
  unsigned int HeadLength;
  
  ObjectList<double> Temperatures;
  
  String BensonProperty;
  
public:
  OutputThermoTableStyle();
  OutputThermoTableStyle(const unsigned int style);
  
  ostream& OutputThermoHeader(ostream& out,
			   const String& title);
  ostream& OutputThermoHeaderASCII(ostream& out,
				const String& title);
  ostream& OutputThermoHeaderLaTeX(ostream& out,
				const String& title);
  ostream& OutputThermoTail(ostream& out);
  ostream& OutputThermoTailASCII(ostream& out);
  ostream& OutputThermoTailLaTeX(ostream& out);
  //  ostream& OutputThermoElement(ostream& out,
  //SimpleMolecule& mol);
  ostream& OutputThermoElementASCII(ostream& out,
				    BensonBaseTableObject& benson,
				    const String& title);
  ostream& OutputThermoElementLaTeX(ostream& out,
				    BensonBaseTableObject& benson,
				    const String& title);
  ostream& OutputThermoEnthalpy(ostream& out,
				BensonBaseTableObject& benson);
  ostream& OutputThermoEntropy(ostream& out,
			       BensonBaseTableObject& benson);
  ostream& OutputThermoHeatCapacity(ostream& out,
				    BensonBaseTableObject& benson,
				    double temperature);

};
 
/*C
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class ThermoTableStyle
{
  OutputThermoTableStyle Style;
  unsigned int LinesPerPage;
  String Title;
public:
  ThermoTableStyle(const OutputThermoTableStyle& style,
		   const String& title);
  ThermoTableStyle(const unsigned int style,
		   const String& title);
  //  ostream& OutputThermoTable(ostream& out,
  //		     ObjectList<SimpleMolecule>& molecules);
};



/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
