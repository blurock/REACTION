/*  FILE     SenkinRates.cc
**  PACKAGE  SenkinRates
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "SenkinRates" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "Basis/System.hh"
#include "Basis/Pairs.hh"
#include "Basis/Graph.hh"
#include "Basis/GraphCycle.hh"
#include "Basis/BasicLaTeXTable.hh"
#include "Basis/Vector.hh"
#include "Basis/MatrixNumeric.hh"
#include "Basis/MatrixUtilities.hh"
#include "Basis/MatrixOut.hh"

#include "Reaction/Molecule.hh"
#include "Reaction/MolProps.hh"
#include "Reaction/MolValence.hh"
#include "Reaction/MolStats.hh"
#include "Reaction/Rxn.hh"
#include "Reaction/DbaseRxn.hh"
#include "Reaction/DbaseMolRxn.hh"
#include "Reaction/RxnMech.hh"
#include "Reaction/ThermoTables.hh"
#include "Reaction/Senkin.hh"
#include "Reaction/SenkinRates.hh"

#include "strstream.h"
#include "iomanip.h"

#define SENKintOOLS "/home/reaction/Reaction/tools/Senkin/"
#define START_SENKIN_COMMAND "/home/reaction/Reaction/tools/Senkin/scripts/SenkinJobRun"
#define DEFAULT_JOB_TIMEOUT 300
#define BENSON_PROP_TAG "Benson"
#define CHEMKIN_PROP_TAG "ChemkinThermo"

 
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
void MatrixGnuPlotOut(MatrixNumeric& mat,
		      const ObjectList<String>& names,
		      const int part,
		      const int fsize,
		      const String& root,
		      const ObjectList<double>& xcoord,
		      const int skip);

/*S Interpret
 */
/*F ans = operator==(molpoints,id)  .  SenkinMoleculeDataPoints with Identify
**
**  DESCRIPTION
**    molpoints: The data for a given molecule
**    id: The identity of the molecule
**    ans: True if the Identify structures match
**
**  REMARKS
**
*/
bool operator==(const SenkinMoleculeDataPoints& molpoints, const Identify& id)
     {
     return id == molpoints.Molecule;
     }

 
 

/*S ComputePressureFromConcentrations
 */
 
/*F ComputePressureFromConcentrations(n,p)  . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    n: 
**    p: 
**
**  REMARKS
**
*/
ComputePressureFromConcentrations::ComputePressureFromConcentrations(unsigned int n,ObjectList<double>& p)
    : Pressure(p)
    {
      for(unsigned int count; count < n; count++)
	Pressure.AddObject(0.0);
    }
 
/*F pressure = TotalPressure()  . . . . . . . . . . . . . . retrieve pressure
**
**  DESCRIPTION
**    pressure: The calculated pressure
**
**  REMARKS
**
*/
ObjectList<double>& ComputePressureFromConcentrations::TotalPressure()
{
  return Pressure;
}
 
/*F operator()(concs) . . . . . . . . . . . . . . . add mol concs to pressure
**
**  DESCRIPTION
**    conc: The molecule concentrations
**
**    Loop through the concentrations over time and add to 
**    the total pressure
**
**  REMARKS
**
*/
void ComputePressureFromConcentrations::operator()(const SenkinMoleculeDataPoints& concs)
    {
      transform(concs.Concentrations.begin(),
		concs.Concentrations.end(),
		Pressure.begin(),
		Pressure.begin(),
		plus<double>());
    }
/*S ThirdBodyRateCalc
 */
 
/*F ThirdBodyRateCalc(third)  . . . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    third: The third body information
**
**    Just the Arrhenius constant (the multiplication factor) and the 
**    molecule identifier (an integer) is transfered .
**
**  REMARKS
**
*/
ThirdBodyRateCalc::ThirdBodyRateCalc(StandardThirdBody& third)
  : Mol(third.ThirdBody),
    A(third.Arrhenius)
{
}
 
/*F ans = operator()(points)  . . . . . . . . . . . . . . . .  same molecule?
**
**  DESCRIPTION
**    points: The molecule information
**    ans: True if the third body molecule matches the molecule info
**
**    This is used in a find_if to locate the molecule information
**    for the third body molecule.
**
**  REMARKS
**
*/
bool ThirdBodyRateCalc::operator()(const SenkinMoleculeDataPoints& points)
{
  return Mol == points.Molecule.Identification;
}
 
/*F flt = operator()(p,conc)  . . . . . . .  third body factor from time conc
**
**  DESCRIPTION
**    p: The current total pressure for this time
**    conc: The concentration for this time
**
**    The pressure is updated with this third body weight.  Since
**    the total pressure already has the third body normally included,
**    the mult factor is (A-1.0).  The 1.0 is compensation for the part
**    already in the pressure.
**
**  REMARKS
**
*/
double ThirdBodyRateCalc::operator()(const double p,
				     const double conc)	  
{
  return p + (A-1.0)*conc;
}
/*S CalculateTotalPressureVersusTime
 */
 
/*F CalculateTotalPressureVersusTime(points)  . . . . . . . . . . constructor
**
**  DESCRIPTION
**    points: The concentration data
**
**    In this constructor the pressure is calculated.
**
**    The concentrations for each molecule (for_each) are added up for each
**    time (ComputePressureFromConcentrations).
**
**  REMARKS
**
*/
CalculateTotalPressureVersusTime::CalculateTotalPressureVersusTime(const MechanismSenkinDataPoints& points)
  : Points(points),
    TotalPressure(),
    Calc(points.Times.size(),TotalPressure)
{
  for_each(Points.MoleculePoints.begin(),
	     Points.MoleculePoints.end(),
	     Calc);
}
 
/*F p = ReturnTotalPressure() . . . . . . . . .  total pressure for each time
**
**  DESCRIPTION
**    p: The pressure versus time
**
**  REMARKS
**
*/
ObjectList<double>& CalculateTotalPressureVersusTime::ReturnTotalPressure()
{
  return TotalPressure;
}
 
/*F t = ThirdBody(third)  . . . . . . . . .  third body corrections to M conc
**
**  DESCRIPTION
**    third: The list of third bodies
**
**    The third bodies are looped through and the correction added to the pressure.
**    If the third body identifier is zero, it is assumed that there is not factor
**    involved.
**
**  REMARKS
**
*/
ObjectList<double>& CalculateTotalPressureVersusTime::ThirdBody(ListOfThirdBodyMolecules& third)
{
  ListOfThirdBodyMolecules::iterator iter;
  ObjectList<double> *p = new ObjectList<double>;
  copy(TotalPressure.begin(),
       TotalPressure.end(),
       back_insert_iterator< ObjectList<double> >(*p));
  
  for(iter=third.begin();
      iter != third.end();
      iter++)
    {
      if((*iter).ThirdBody != 0)
	{
	  ThirdBodyRateCalc calc(*iter);
	  
	  ObjectList<SenkinMoleculeDataPoints>::const_iterator concs 
	    = find_if(Points.MoleculePoints.begin(),
		      Points.MoleculePoints.end(),
		      calc);
	  transform((*concs).Concentrations.begin(),
		    (*concs).Concentrations.end(),
		    (*p).begin(),
		    (*p).begin(),
		    calc);
	}
    }
  return *p;
}
/*S NormalisedRateCalculation
 */
/*F MolRateSumCalculate(fi,mat) . . . . . . . .  For a molecule in a reaction
**
**  DESCRIPTION
**    fi: The collected Fi values
**    Mat: The reaction rate versus time matrix Mat[rxn][time]
**
**
**  REMARKS
**
*/
MolRateSumCalculate::MolRateSumCalculate(SearchableObjectList<Identify, vector<double> >& fi,
					 MatrixNumeric& mat)
  : Fi(fi),
    Mat(mat),
    Rxn(0),
    Forward(true)
{
}
 
/*C MolRateReactionCalculate  . . . . . . . . . . . . . . . .  add rate to fi
**
**  DESCRIPTION
**     The rate is added to the fi.  The flags determine how:
**     - LeastSquares: The square of the rate is added
**     - Forward: The negative of the rate is added
**
**  REMARKS
**
*/
class MolRateReactionCalculate
{
  bool Forward;
  bool LeastSquares;
  
public:
  MolRateReactionCalculate(const bool forward,
			   const bool leastsquares)
    : Forward(forward),
    LeastSquares(leastsquares)
    {
    }
  
  double operator()(double fi, double rate)
    {
      double sum,modrate;
      if(Forward)
	modrate = -rate;
      else
	modrate = rate;
      if(LeastSquares)
	sum = fi + rate;
      else
	sum = fi + modrate;
      
      return sum;
    }
};

  
/*F operator()(id)  . . . . . . . . . . . . . . . add in the rate of reaction
**
**  DESCRIPTION
**    id: The reaction identifier
**
**    The current reaction is given by the 'Rxn' value.
**    For each time, the rates (Mat[Rxn]) are subtracted (if forward)
**    or added (if reverse) to the current Fi:
**    - Minus for Forward: reactant used up
**    - Plus for Reverse: product created
**    If LeastSquares is used, then the square of the rate is added.
**
**  REMARKS
**
*/
void MolRateSumCalculate::operator()(const Identify& id)
{      
  //cout << "MolRateSumCalculate:";
  //cout << id.NameTag << " ";
  //cout << Fi[id][0];
  MolRateReactionCalculate molcalc(Forward,true);
  
  transform(Fi[id].begin(),
	    Fi[id].end(),
	    Mat[Rxn].begin(),
	    Fi[id].begin(),
	    molcalc);
  //cout << " " << Mat[Rxn][0] << " ";
  //cout << Fi[id][0];
  //cout << "\n";
  
}
/*F operator()(rate,id) . . . . . . . . . . . . .  add in 1/fi for a molecule
**
**  DESCRIPTION
**    rate: The rate to be created
**    id: The molecule 
**
**    The inverse of Fi[molecule][time] is subtracted (forward)
**    or added (reverse) to the rate. If the rate is smaller 
**    than 1e-10 then nothing will be added or subtracted.
**
**  REMARKS
**
*/
double MolRateSumCalculate::operator()(double rate, const Identify& id)
{
  double ri;
  double r = Fi[id][Itime];
  bool standardFi = true;
  
  if(standardFi)
    {
      if(r < 1e-10 && r > -1e-0)
	ri = 0;
      else
	ri = 1.0/r;
    }
  else
    {
      ri = r;
    }
  rate += ri;
  return rate;
}
/*F GnuPlotFi(points,fi)  . . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void GnuPlotFi(MechanismSenkinDataPoints points,
	       SearchableObjectList<Identify, vector<double> >& fi,
	       const String& title)
{
  MatrixNumeric mat(points.MoleculePoints.size(),
		    points.Times.size());
  
  ObjectList<String> names;
  ObjectList<SenkinMoleculeDataPoints>::iterator iter;
  unsigned int i = 0;
  
  for(iter = points.MoleculePoints.begin();iter != points.MoleculePoints.end();iter++)
    {
      names.AddObject((*iter).Molecule.NameTag);
      for(unsigned int j=0;j<points.Times.size();j++)
	{
	  mat[i][j] = fi[(*iter).Molecule][j];
	}
      i++;
    }
  
  MatrixNumeric trans = mat.transpose();
  MatrixGnuPlotOut(trans,names,
		   6,20,title,
		   points.Times,1);
}
/*F MolRateSum(mols,rxns,mat) . . . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    points: The set of molecule concentrations (used only for the list of molecules)
**    rxn: The list of reactions
**    mat: rates[reaction][time]
**
**    The constructor sets up the Fi with zero values and then
**    calculates the normalized rate (CalculateNormalizedRate).
**
**  REMARKS
**
*/
MolRateSum::MolRateSum(MechanismSenkinDataPoints points,
		       ObjectList<DbaseReaction>& rxns,
		       MatrixNumeric& mat)
  : Fi(),
    Mat(mat)
{
  for(ObjectList<SenkinMoleculeDataPoints>::iterator iter=points.MoleculePoints.begin();
      iter != points.MoleculePoints.end();
      iter++)
    {
      vector<double> *vec = new vector<double>(Mat[0].size(),0.0);
      Fi[(*iter).Molecule] = *vec;
      
      vector<double>::iterator time 
	= Fi[(*iter).Molecule].begin();
      for(unsigned int i=0;i<Mat[0].size();i++)
	{
	*time = 0.0;
	time++;
	}
    }
  CalculateNormalizedRate(rxns);
}
 
/*F CalculateFi(rxns) . . . . . . . . . .  loop through reactions and form fi
**
**  DESCRIPTION
**    rxn: The set of reactions
**
**    For each reaction use MolRateSumCalculate to add up the elements of the fi's.
**
**    Each reaction is considered as a forward and as a reverse reaction, meaning 
**    the products and the reactants will be looped over two times.  The calls to 
**    MolRateSumCalculate determine whether the Fi is to be added or subtracted:
**    - Forward:
**      - Reactant: Subtracted (Forward)
**      - Product: Added (Reverse)
**    - Reverse: 
**      - Reactant: Added (Reverse)
**      - Product: Subtracted (Forward)
**
**  REMARKS
**
*/
void MolRateSum::CalculateFi(ObjectList<DbaseReaction>& rxns)
{
  ObjectList<DbaseReaction>::iterator rxn;
  MolRateSumCalculate calc(Fi,Mat);
  for(rxn =rxns.begin();
      rxn != rxns.end();
      rxn++)
    {
      calc.ForwardSet();
      for_each((*rxn).Reactants.begin(),
		 (*rxn).Reactants.end(),
		 calc);
      calc.ReverseSet();
      for_each((*rxn).Products.begin(),
		 (*rxn).Products.end(),
		 calc);
      calc.Increment();
      calc.ReverseSet();
      for_each((*rxn).Reactants.begin(),
		 (*rxn).Reactants.end(),
		 calc);
      calc.ForwardSet();
      for_each((*rxn).Products.begin(),
		 (*rxn).Products.end(),
		 calc);
      calc.Increment();
    }
}
/*F CalculateNormalizedRate(rxns) . . . . . . . . . . . . . . normalized rate
**
**  DESCRIPTION
**    rxns: The list of reactions
**
**    For each reaction and for each time calculate the Fi.  
**    For the set of products and reactants sum up the
**    f(mol).
**
**    First use 'CalculateFi' to calculate the completed
**    Fi.
**
**    Loop over all reactions and over all time:
**    - frate: subtract the inverse fi's for each reaction
**    - rrate: add the inverse fi's for each reaction
**
**    The sum is multiplied by the rate, positively for
**    forward and negaively for the reverse reaction.
**    
**  REMARKS
**
*/
void MolRateSum::CalculateNormalizedRate(ObjectList<DbaseReaction>& rxns)
{
  CalculateFi(rxns);
  ObjectList<DbaseReaction>::iterator rxn;
  MolRateSumCalculate calc(Fi,Mat);
  unsigned int n = Mat[0].size();
  unsigned int irxn = 0;
  bool standardFi = true;

  for(rxn =rxns.begin();
      rxn != rxns.end();
      rxn++)
    {
      /*
	cout << "Reaction: ";
	cout << (*rxn).NameTag;
	cout << "\n";
	*/
      calc.ResetTime();
      
      for(unsigned int itime=0;itime < n;itime++)
	{
	  calc.ForwardSet();	  
	  
	  double frate = accumulate((*rxn).Reactants.begin(),
				   (*rxn).Reactants.end(),
				   0.0,
				   calc);
	  calc.ReverseSet();	
	  
	  double rrate = accumulate((*rxn).Products.begin(),
				   (*rxn).Products.end(),
				   0.0,
				   calc);
	  double rsize = (double) (*rxn).Reactants.size() + (*rxn).Products.size();
	  
	  /*
	  if(itime == 100)
	    {
	      cout << "Rates: " << setw(10) << itime << " ";
	      cout << setw(10) << Mat[irxn][itime] << " ";
	      cout << setw(10) << Mat[irxn+1][itime] << " ";
	      cout << setw(10) << frate << " " << setw(10) << rrate << "\n";
	    }
	    */

	  if(standardFi)
	    {
	      Mat[irxn][itime] *= (rrate+frate)/rsize;
	      Mat[irxn+1][itime] *= (rrate+frate)/rsize;
	    }
	  else
	    {
	      double f = Mat[irxn][itime];
	      double r = Mat[irxn+1][itime];
	      double f2 = f*f;
	      double r2 = r*r;

	      if(f2 > 1e-20)
		Mat[irxn][itime] = f2*(frate+rrate);
	      else
		Mat[irxn][itime] = 0.0;
	      if(r2 > 1e-20)
		Mat[irxn+1][itime] = r2*(frate+rrate);
	      else
		Mat[irxn+1][itime] = 0.0;
	    }
	  /*
	  if(itime == 100)
	    {
	      cout << setw(10) << Mat[irxn][itime] << " ";
	      cout << setw(10) << Mat[irxn+1][itime] << " ";
	      cout << "\n";
	    }
	    */
	  calc.IncrementTime();
	}
      irxn++;
      irxn++;
    }
}
/*F fi = GetFi()  . . . . . . . . . . . . . . . .  retrieve the calculated Fi
**
**  DESCRIPTION
**    fi: The fi for each molecule
**
**  REMARKS
**
*/
SearchableObjectList<Identify,vector<double> > MolRateSum::GetFi()
{
  return Fi;
}
/*S MatrixOps
 */
/*F lflt = TakeLog(flts)  . . . . . . . . . . . . . . . .  FindReactionValues
**
**  DESCRIPTION
**    flts: A vector of floats
**    lflts: Each number is the log
**
**  REMARKS
**
*/
vector<double>& TakeLog(vector<double>& vec)
{
  vector<double>::iterator pnt;
  for(pnt = vec.begin(); 
      pnt != vec.end(); pnt++)
    {
      if(*pnt < 0.0000000001)
	*pnt = -10.0;
      else
	*pnt = log10((double) *pnt);
    }
  return vec;
}
 
/*F TakeLogOfMatrixElements(mat)  . . . . . . . . . . . . log of each element
**
**  DESCRIPTION
**    mat: The matrix
**
**  REMARKS
**
*/
void TakeLogOfMatrixElements(MatrixNumeric& mat)
{
  unsigned int inum = mat.size();
  
  for(unsigned int i=0;i<inum;i++)
    {
      TakeLog(mat[i]);
    }
}
/*S RateConstants
 */
/*F size = SkipMatrixSize(fullsize,skip)  . . . . . .  size of reduced matrix
**
**  DESCRIPTION
**    fullsize: The size of the full matrix
**    skip: The skip factor (ex. 3 is every third member) starting from zero
**    size: The size of the reduced matrix
**
**    Sometimes the formed matrix is too big or redundent and should
**    be reduced.  It is meant for sequential data where it is enough
**    to take every nth element (starting from the 0th element).
**
**  REMARKS
**
*/
int SkipMatrixSize(int fullsize, int skip)
{
  int size = fullsize/skip;
  if(size * skip < fullsize)
    size++;
  
  return size;
}
  
/*F ans = FillTimeTemperatureMatrix(mat,times,temps)  . . . . . . fill matrix
**
**  DESCRIPTION
**    mat: The matrix to fill (already dimensioned nx2)
**    times: The list of times (dimension n)
**    temps: The list of temperatures (dimension n)
**    ans: True if successful
**
**  REMARKS
**
*/
bool FillTimeTemperatureMatrix(MatrixNumeric& mat,
			       const ObjectList<double>& timeValues,
			       const ObjectList<double>& temps,
			       const int skip)
{
  unsigned int length = SkipMatrixSize(timeValues.size(),skip);
  bool ret = true;
  if(mat.size() != length ||
     timeValues.size() != temps.size() ||
     mat[0].size() != 2)
    {
      cout << "\nMismatch of Dimensions\n";
      cout << "Matrix:       " << mat.size() << " , " << mat[0].size() << "\n";
      cout << "Times:        " << timeValues.size() << "\n";
      cout << "Temperatures: " << temps.size() << "\n";
      ret = false;
    }
  else
    {
      ObjectList<double>::const_iterator temp = temps.begin();
      ObjectList<double>::const_iterator time = timeValues.begin();
      
      int count = 0;
      for(unsigned int i=0;i!=temps.size();i++)
	{
	  if(i % skip == 0)
	    {
	      mat[count][0] = *temp;
	      mat[count][1] = *time;
	      count++;
	    }
	      *temp++;
	      *time++;	  
	}
    }
  return ret;
}
/*F FormAndAddReactionNames(rxns,names) . . . . . . . . .  add reaction names
**
**  DESCRIPTION
**    rxns: The list of reactions
**    names: The current set of names
**
**    The forward and reverse reaction names are created and added to the list
**    of names.  The forward and reverse names, given a reaction XXXXXX are:
**    - For:XXXXXX
**    - Rev:XXXXXX
**
**  REMARKS
**
*/
void FormAndAddReactionNames(ObjectList<DbaseReaction>& rxns,
			     ObjectList<String>& names,
			     const bool includerev = true)
{
  list<DbaseReaction>::iterator rxn;
  for(rxn = rxns.begin();
      rxn != rxns.end();
      rxn++)
    {
      String forward("For:");
      String reverse("Rev:");
      forward.AppendToEnd((*rxn).NameTag);
      reverse.AppendToEnd((*rxn).NameTag);
      names.AddObject(forward);
      if(includerev)
	names.AddObject(reverse);
    }
}
/*F CalculateValuesFromReactions(rxns,values,compfi,takelog)  calculate rates
**
**  DESCRIPTION
**    rxns: The list of reactions
**    values: The intialized FindReactionValues values
**    compfi: Compute the normalized rates
**    takelog: Take the log of the answers
**
**    The information for each reaction will be calculated.  The matrix
**    of reaction rates versus time is formed (in normalized form through
**    MolRateSum).
**
**  REMARKS
**
*/
void CalculateValuesFromReactions(ObjectList<DbaseReaction>& rxns,
				  FindReactionValues& values,
				  bool compfi,
				  bool takelog)
{
  list<DbaseReaction>::iterator rxn;
  for(rxn = rxns.begin();
      rxn != rxns.end();
      rxn++)
    {
      values.operator()(*rxn);
    }
  if(compfi)
    MolRateSum normal(values.MolPoints,rxns,values.Mat);
  if(takelog)
    TakeLogOfMatrixElements(values.Mat);
}
/*C RateConstantsVersusTime . . . . . . . . . . . . . . . . .  compute rate k
**
**  DESCRIPTION
**     The operatror() function computes the rate constant for a 
**     a given temperature
**
**  REMARKS
**
*/
class RateConstantsVersusTime
     {
     StandardRateConstants& Constants;
 public:
     RateConstantsVersusTime(StandardRateConstants& constants)
	  : Constants(constants)
	       {
	       }
     double operator()(double temp)
	  {
	    //	  cout << "RateConstantsVersusTime: ";
	    //	  cout << temp << " ";
	    //	  cout << Constants.TemperatureCoefficient << " ";
	    double E = ((double) Constants.ActivationEnergy);
	    double t = (double) temp;
	    
	    double e = -E/(t*1.98717);

	    //	  cout << e << " ";
	    double r = Constants.Arrhenius;
	    //	  cout << r << " ";
	    r = r * pow((double) temp,
			(double) Constants.TemperatureCoefficient);
	    //	  cout << r << " ";
	    r = r * exp(e);
	    //	  cout << r << " ";
	    //	  cout << "\n";
	    return r;
	  }
     };
/*S ReactionRatesForSingleReaction
 */
/*F FindReactionValuesFromMol . . . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    rxn: The reaction
**    values: The vector of values to be filled in (vector over time)
**    molpoints: The concentration versus time information
**    constants: 
**
**    The constructor just puts these structures in the class elements
**
**  REMARKS
**
*/
FindReactionValuesFromMol::FindReactionValuesFromMol(DbaseReaction& rxn,
						     vector<double> values,
						     const MechanismSenkinDataPoints& molpoints,
						     const String& constants) 
  : Rxn(rxn),
    MolPoints(molpoints),
    Constants(constants),
    Values(values)
{
}
 
/*F Initialize()  . . . . . . . . . . . . . . . . . FindReactionValuesFromMol
**
**  DESCRIPTION
**    This routine initialises the Values (vector over time):
**    - Retrieve kinetic constants (if not found set to zero)
**    - Set up a 'StandardRateConstants' with values
**    - Using 'RateConstantsVersusTime' loop through temperatures
**      (versus time) to set up the kinetic constants versus time
**
**    The 'Values' have now the kinetic constants values versus time
**    (i.e. for each temperature).
**
**  REMARKS
**
*/
void FindReactionValuesFromMol::Initialize()
{
  //	  cout << "FindReactionValuesFromMol::Initialize\n";
  //	  cout << Rxn.NameTag << "\n";
  
  PropertyTypeByName<ReactionConstants<double> > *prop
    = (PropertyTypeByName<ReactionConstants<double> > *)
    Rxn.Properties.GetPointerToProperty(Constants);
  double A = 0.0;
  double E = 0.0;
  double n = 0.0;
  String doc;
  
  if(prop->NameTag == Constants)
    {
      A= prop->Object.Arrhenius;
      E = prop->Object.ActivationEnergy;
      n = prop->Object.TemperatureCoefficient;
      doc = prop->Object.Documentation;
    }
  else
    {
      cout << "Reaction: ";
      cout << Rxn.NameTag;
      cout << " Rate: ";
      cout << Constants;
      cout << " not found\n";
    }
  
  StandardRateConstants compute(A,E,n,doc);

  RateConstantsVersusTime init(compute);
  transform(MolPoints.Temperatures.begin(),
	    MolPoints.Temperatures.end(),
	    Values.begin(),
	    init);
}
 
/*F operator()(id)  . . . . . . . . . . . . . . . . FindReactionValuesFromMol
**
**  DESCRIPTION
**    id: The identification of the molecule
**
**    This operator is to be used in the loop over molecules in a reaction.
**    - The 'SenkinMoleculeDataPoints' is found for the molecule.
**    - The set of concentrations (versus time) are multiplied in Values
**
**    After all the molecules have been called for the reactants, then the
**    reaction rate is complete for each time period.
**
**  REMARKS
**
*/
void FindReactionValuesFromMol::operator()(const Identify& id)
{
  //	  cout << "FindReactionValuesFromMol::Initialize\n";
  //	  cout << id << "\n";
  
  FindSenkinMoleculeDataPoints findit(id);
  
  ObjectList<SenkinMoleculeDataPoints>::const_iterator concs 
    = find_if(MolPoints.MoleculePoints.begin(),
	      MolPoints.MoleculePoints.end(),
	      findit);

  //cout << "--------------------------------------------------\n";
  //cout << (*concs).Concentrations.size() << " , " << Values.size() << "\n";
  //cout << "\nConcentrations\n";
  //cout << "Mol: " << (*concs).Concentrations.front();
  //cout << "\nCurrent Values\n";
  //cout << "  " << Values.front();
  //cout << "\n";
  
  transform((*concs).Concentrations.begin(),
	    (*concs).Concentrations.end(),
	    Values.begin(),
	    Values.begin(),
	    multiplies<double>());
  //cout << "  " << Values.front();
  //cout << "\n";
  
  /*
    cout << "\nAfter Update\n";
    cout << Values;
    cout << "--------------------------------------------------\n";
    */
}
 
/*F FindReactionValues(mat,molpoints,forward,length,skip)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
FindReactionValues::FindReactionValues(MatrixNumeric& mat,
				       const MechanismSenkinDataPoints& molpoints,
				       const bool forward,
				       const int length,
				       const int skip)
  : Forward(forward),
    Length(length),
    Cnt(0),
    SkipFactor(skip),
    MolPoints(molpoints),
    Mat(mat)
{
}
 
/*F flts = KineticValues(rxn,mlist,constant)  . . . . . .  FindReactionValues
**
**  DESCRIPTION
**    rxn: The reaction
**    mlist: The list of reactant molecules
**    constant: "Forward" or "Reverse" reaction constants
**
**    The reactant molecules are given explicitely because they are
**    the 'Reactants' if forward and 'Products' if reverse.
**
**    The routine does the following steps:
**    - Construct 'FindReactionValuesFromMol' setup values in the elements
**    - 'Initialize' FindReactionValues calculates the k for each temperature
**    - Loop over the reacting molecules to multiply in their concentrations
**    - Get ThirdBody coefficient
**    - If a third body multiply by total pressure
**
**
**  REMARKS
**    At the moment the total pressure is calculated for 
**    each third body.
**
*/
vector<double> FindReactionValues::KineticValues(DbaseReaction& rxn,
						ObjectList<Identify> mlist,
						const String& constant)
{
  vector<double> values(Length);
  FindReactionValuesFromMol rxnkinetic(rxn,values,MolPoints,constant);
  rxnkinetic.Initialize();
  
  /*  cout << "==============================================\n";
  cout << "FindReactionValues::operator()\n";
  cout << rxnkinetic.Values.front();
  cout << "\n";
  cout << mlist;
  cout << "\n==============================================\n";
  */
  ObjectList<Identify>::iterator mol;
  for(mol = mlist.begin(); mol != mlist.end(); mol++)
    {
      rxnkinetic.operator()(*mol);
    }
  /*
  PropertyTypeByName<ListOfThirdBodyMolecules> *prop 
    = (PropertyTypeByName<ListOfThirdBodyMolecules> *) 
    rxn.Properties.GetPointerToProperty("ThirdBody");
  ListOfThirdBodyMolecules third = prop->Object;
  if(third.size() > 0)
    {
      CalculateTotalPressureVersusTime pres(MolPoints);
      transform(m.begin(),
		m.end(),
		rxnkinetic.Values.begin(),
		rxnkinetic.Values.begin(),
		times<double>());
    }
    */  
  return rxnkinetic.Values;
} 
/*F operator(rxn) . . . . . . . . . . . . . . . . . . . .  FindReactionValues
**
**  DESCRIPTION
**    rxn: The reaction 
**
**  REMARKS
**
*/
void FindReactionValues::operator()(DbaseReaction& rxn) 
{
  vector<double> forward = KineticValues(rxn,rxn.Reactants,"Forward");
  vector<double> reverse = KineticValues(rxn,rxn.Products,"Reverse");
  
  int count = 0;
  for(int i=0;i<Length;i++)
    {
      if(i % SkipFactor == 0)
	{
	  Mat[Cnt][count] = forward[i];
	  Mat[Cnt+1][count] = reverse[i];		 
	  count++;
	}
    }
  Cnt += 2;
}
 
/*Utilities
 */
/*F time = FindTimePoint(points,mechtime) . . . . . . . . . find a time point
**
**  DESCRIPTION
**    points: The Senkin information
**    mechtime: The time to find
**
**  REMARKS
**
*/
static int FindTimePoint(const MechanismSenkinDataPoints points,
			 double mechtime)
{
  ObjectList<double>::const_iterator time = points.Times.begin();
  int itime = 0;  
  while(*time < mechtime)
    {
      itime++;
      time++;
    }
  return itime;
}

/*F reduced = ReduceMatrixByRowSpecification(mat,initial,ifinal)  . .  reduce
**
**  DESCRIPTION
**    mat: The original matrix
**    initial: The initial row to be included
**    ifinal: The final row to be included
**    reduced: The new reduced matrix
**
**  REMARKS
**
*/
MatrixNumeric& ReduceMatrixByRowSpecification(MatrixNumeric& mat,
						      const unsigned int initial,
						      const unsigned int ifinal)
{
  unsigned int isize = ifinal - initial + 1;
  MatrixNumeric *reduced = new MatrixNumeric(isize,mat[0].size());
  cout << "Size of Original Matrix: " << mat.size() << " by " << mat[0].size() << "\n";
  cout << "Size of Reduced Matrix: " << (*reduced).size() << " by " << (*reduced)[0].size() << "\n";
  
  unsigned int initialc = initial;
  
  MatrixNumeric::iterator initrow = mat.begin();
  while(initialc > 0)
    {
      initialc--;
      initrow++;
    }
  
  MatrixNumeric::iterator finrow = initrow;
  while(isize > 0)
    {
      finrow++;
      isize--;
    }
  cout << "Difference in Rows: ";
  cout << finrow - initrow;
  cout << "\n";
  
  copy(initrow,finrow,(*reduced).begin());

  return *reduced;
}
/*F reduced = ReduceMatToTimeInterval(mat,points,initialtime,finaltime) reduce
**
**  DESCRIPTION
**    mat: The original time versus concentration (or whatever) matrix
**    points: The concentration information (for the set of times)
**    initialtime: The initial time to be included
**    finaltime: The final time to be included
**    reduced: The reduced matrix
**
**    The rows are included which are represent times between the initial
**    time and the final time.
**    The next row that is greater than or equal to the given times is included
**    in the reduced matrix.
**
**  REMARKS
**
*/
MatrixNumeric ReduceMatToTimeInterval(MatrixNumeric& mat,
				       const MechanismSenkinDataPoints points,
				       double initialtime,
				       double finaltime,
				       const bool includerev)
{
  unsigned int initial = FindTimePoint(points,initialtime);
  unsigned int ifinal = FindTimePoint(points,finaltime);

  cout << "Initial and Final Rows: ";
  cout << initial << "(" << initialtime << ") ";
  cout << ifinal << "(" << finaltime << ")\n";

  MatrixNumeric tmat = mat.transpose();

  MatrixNumeric reduced = ReduceMatrixByRowSpecification(tmat,initial,ifinal);
  if(!includerev)
    {
      MatrixNumeric trev = reduced.transpose();
      MatrixNumeric norev(trev.size()/2,trev[0].size());
      MatrixNumeric::iterator redpos;
      MatrixNumeric::iterator norevpos = norev.begin();
      unsigned int count = 0;
      for(redpos=trev.begin();
	  redpos != trev.end();
	  redpos++)
	{
	  if((count % 2) == 0)
	    {
	      copy((*redpos).begin(),(*redpos).end(),(*norevpos).begin());
	      norevpos++;
	    }
	  count++;
	}
      return norev;
    }
  
  return reduced.transpose();
}
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/



