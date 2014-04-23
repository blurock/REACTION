/*  FILE     SenkinRatesType.hh
**  PACKAGE  SenkinRates
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "SenkinRates" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_SENKINRATESTYPE_HH
#define Reaction_SENKINRATESTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
/*C FindSenkinMoleculeDataPoints  . . . . . . . . . . .  find a set of points
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class FindSenkinMoleculeDataPoints
     {
     Identify ID;
 public:
     FindSenkinMoleculeDataPoints(const Identify& id)
	  : ID(id)
	  {
	  }
     bool operator()(const SenkinMoleculeDataPoints& points)
	  {
	  return points.Molecule == ID;
	  }
     };
/*C FindReactionValuesFromMol . . . . . . . . . rate constants from molecules
**
**  DESCRIPTION
**     This class is for calculating the rate constants for a given reaction
**     using the temperatures and times given by the
**     MechanismSenkinDataPoints class.  The Initialize() function
**     calculates the base rate constant.  The operator() multiplies in 
**     the concentration of a given molecule (Identify).
**     
**  REMARKS
**
*/
class FindReactionValuesFromMol
{
  DbaseReaction& Rxn;
  bool Forward;
  const MechanismSenkinDataPoints& MolPoints;
  String Constants;
  
public:
  vector<double> Values;
  
  FindReactionValuesFromMol(DbaseReaction& rxn,
			    vector<double> values,
			    const MechanismSenkinDataPoints& molpoints,
			    const String& constants);
  void operator()(const Identify& id);
  void Initialize();
  friend class MechanismSenkinDataPoints;
};

	       
/*C FindReactionValues  . . . . . . . . . . . . . . .  Compute Rate Constants
**
**  DESCRIPTION
**    For each reaction, the operator() function will compute the
**    vector of rate constants from the vector of temperatures (and
**    times) of the Senkin run.  
**
**  REMARKS
**
*/
class FindReactionValues
     {
     bool Forward;
     int Length;
     int Cnt;
     int SkipFactor;
     
     public:
     const MechanismSenkinDataPoints& MolPoints;
     MatrixNumeric& Mat;
     
     FindReactionValues(MatrixNumeric& mat,
			const MechanismSenkinDataPoints& molpoints,
			const bool forward,
			const int length,
			const int skip);
     vector<double> KineticValues(DbaseReaction& rxn,
				 ObjectList<Identify> mlist,
				 const String& constant);
     void operator()(DbaseReaction& rxn);
     };
/*C ComputePressureFromConcentrations . . . . . . . . . . . . . .  operator()
**
**  DESCRIPTION
**    The operator() adds the list of concentrations into the total pressure
**
**  REMARKS
**
*/
class ComputePressureFromConcentrations
{
  ObjectList<double>& Pressure;
public:
  ComputePressureFromConcentrations(unsigned int n,ObjectList<double>& p);
  ObjectList<double>& TotalPressure();
  void operator()(const SenkinMoleculeDataPoints& concs);
};
/*C ThirdBodyRateCalc . . . . . . . . . . . . . . . . .  correct with weights
**
**  DESCRIPTION
**     The elements of the class are:
**     - Mol: The molecule ID
**     - A: The weight
**
**     This is used to adjust the third body 
**     rate constants.  There are two operators
**     - Match the molecule ID
**     - Correct the pressure with the concentration
**
**     The correction, (A-1.0), has the following elements:
**     - The Arrhenius factor gives the contribution weight of the molecule
**     - The factor minus one is because all molecules were already included in the total pressure
**
**  REMARKS
**
*/
class ThirdBodyRateCalc
{
  unsigned int Mol;
  double A;
public:
  ThirdBodyRateCalc(StandardThirdBody& third);
  bool operator()(const SenkinMoleculeDataPoints& points);
  double operator()(const double p,
		   const double conc);
};
/*C CalculateTotalPressureVersusTime  . . . . . . . . . . . . .  add up concs
**
**  DESCRIPTION
**     The elements:
**     - Points: The data points
**     - TotalPressure: The calculated pressure
**     - Calc: Helping class to calculate
**
**     When the class is created, the total pressure
**     is calculated.  The following operators are 
**     available:
**     - TotalPressure: returns the total pressure
**     - ThirdBody: the weights of the third body molecules
**
**  REMARKS
**
*/
class CalculateTotalPressureVersusTime
{
  const MechanismSenkinDataPoints& Points;
  ObjectList<double> TotalPressure;
  ComputePressureFromConcentrations Calc;
  
public:
  CalculateTotalPressureVersusTime(const MechanismSenkinDataPoints& points);
  ObjectList<double>& ReturnTotalPressure();
  ObjectList<double>& ThirdBody(ListOfThirdBodyMolecules& third);
};
/*C MolRateSumCalculate . . . . . . . . . . . . . Calculate Molecule Rate Sum
**
**  DESCRIPTION
**
**      The following routine are called after each operator loop
**      - Increment(): Increment to next reaction
**      - IncrementTime(): Increment to next time
**      - ResetTime(): Reset the time count to zero
**      - ForwardSet(): Next will be forward reaction
**      - ReverseSet(): Next will be reverse reaction
**
**      The operator(id) loops through the set of times
**	and for the molecule specified by id, adds in the 
**	reaction rate.
**
**  REMARKS
**
*/
class MolRateSumCalculate
{
  SearchableObjectList<Identify, vector<double> >& Fi;
  MatrixNumeric& Mat;

  unsigned int Rxn;
  unsigned int Itime;
  
  bool Forward;
public:
  MolRateSumCalculate(SearchableObjectList<Identify, vector<double> >& fi,
		      MatrixNumeric& mat);
  void Increment()
    {
      Rxn++;
    }
  void IncrementTime()
    {
      Itime++;
    }
  void ResetTime()
    {
      Itime = 0;
    }
  void ForwardSet()
    {
      Forward = true;
    }
  void ReverseSet()
    {
      Forward = false;
    }
  void operator()(const Identify& id);
  double operator()(double rate, const Identify& id);
};
/*C MolRateSum  . . . . . . . . . . . .  Calculate molecule for all reactions
**
**  DESCRIPTION
**     The class has the following values:
**     - Fi: The set of Fi to be constructed
**     - Mat: The reaction rate versus time matrix
**
**     This class is used to create the set of Fi's.
**
**  REMARKS
**
*/
class MolRateSum
{
  SearchableObjectList<Identify,vector<double> > Fi;
  MatrixNumeric& Mat;

  void CalculateFi(ObjectList<DbaseReaction>& rxns);
  void CalculateNormalizedRate(ObjectList<DbaseReaction>& rxns);
  
public:
  MolRateSum(MechanismSenkinDataPoints points,
	     ObjectList<DbaseReaction>& rxns,
	     MatrixNumeric& mat);
  SearchableObjectList<Identify,vector<double> > GetFi();
  
};

#endif
