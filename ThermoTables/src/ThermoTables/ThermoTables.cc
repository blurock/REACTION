/*  FILE     
**  PACKAGE     REACTION    
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    
**
**  REFERENCES
**
**  COPYRIGHT (C) 1995  REACTION Project / Edward S. Blurock 
*/
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "System.hh"
#include "Vector.hh"
#include "MatrixNumeric.hh"
#include "MatrixUtilities.hh"
#include "PrimitiveStats.hh"
#include "ThermoTables.hh"

/*S EncodeDecode
*/
/*S EncodeDecode
*/
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . .  ChemkinBaseTableObject
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool ChemkinBaseTableObject::EncodeThis(CommBuffer& buffer)
     {
     bool result = Encode(buffer,SpeciesName);
     result = result && Encode(buffer,Date);
     result = result && Encode(buffer,AtSymbAndFormula);
     result = result && Encode(buffer,PhaseDescriptor);
     result = result && Encode(buffer,FormulaDescriptor);
     result = result && Encode(buffer,LowerBoundTemp);
     result = result && Encode(buffer,UpperBoundTemp);
     result = result && Encode(buffer,CommonTemp);
     result = result && Encode(buffer,SimplyOne);
     result = result && Encode(buffer,SimplyTwo);
     result = result && Encode(buffer,SimplyThree);
     result = result && Encode(buffer,SimplyFour);
     result = result && Encode(buffer,y);
     //* STL   double *f;
     VectorNumeric::iterator f;
     int nu = UpperCoeff.size();
     result = result && Encode(buffer,nu);
     for(f=UpperCoeff.begin();
	 f != UpperCoeff.end();
	 f++)
	  {
	  result = result && Encode(buffer,*f);
	  }
     
     int nl = LowerCoeff.size();
     Encode(buffer,nl);
     for(f=LowerCoeff.begin();
	 f != LowerCoeff.end();
	 f++)
	  {
	  result = result && Encode(buffer,*f);
	  }
     return result;
     }
 
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . .  ChemkinBaseTableObject
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**    
**  REMARKS
**
*/
bool ChemkinBaseTableObject::DecodeThis(CommBuffer& buffer)
     {
     bool result = Decode(buffer,SpeciesName);
     result = result && Decode(buffer,Date);
     result = result && Decode(buffer,AtSymbAndFormula);
     result = result && Decode(buffer,PhaseDescriptor);
     result = result && Decode(buffer,FormulaDescriptor);
     result = result && Decode(buffer,LowerBoundTemp);
     result = result && Decode(buffer,UpperBoundTemp);
     result = result && Decode(buffer,CommonTemp);
     result = result && Decode(buffer,SimplyOne);
     result = result && Decode(buffer,SimplyTwo);
     result = result && Decode(buffer,SimplyThree);
     result = result && Decode(buffer,SimplyFour);
     result = result && Decode(buffer,y);
     int nu;
     VectorNumeric::iterator f;
     result = result && Decode(buffer,nu);
     for(f=UpperCoeff.begin();f != UpperCoeff.end();f++)
	  {
	  double fd;
	  result = result && Decode(buffer,fd);
	  *f = fd;
	  }
     
     result = result && Decode(buffer,nu);
     for(f=LowerCoeff.begin();f != LowerCoeff.end();f++)
	  {
	  double fd;
	  result = result && Decode(buffer,fd);
	  *f = fd;
	  }
     return result;
     }     
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . BensonBaseTableObject
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successfull
**
**  REMARKS
**
*/
bool BensonBaseTableObject::EncodeThis(CommBuffer& buffer)
     {
     bool result = Encode(buffer,BensonFormulaDescriptor);
     result = result && Encode(buffer,Temps);
     result = result && Encode(buffer,CpS);
     result = result && Encode(buffer,CommonBensonTemp);
     result = result && Encode(buffer,StandardEntropy);
     result = result && Encode(buffer,StandardEnthalpy);
     return result;
     }
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . BensonBaseTableObject
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool BensonBaseTableObject::DecodeThis(CommBuffer& buffer)
     {
     bool result = Decode(buffer,BensonFormulaDescriptor);
     result = result && Decode(buffer,Temps);
     result = result && Decode(buffer,CpS);
     result = result && Decode(buffer,CommonBensonTemp);
     result = result && Decode(buffer,StandardEntropy);
     result = result && Decode(buffer,StandardEnthalpy);
     return result;
     }

 
/*S
 */
/*S Calculate
 */
/*F S = CalculateEntropy(benson,temperature)  . . . .  from BensonTableObject
**
**  DESCRIPTION
**    benson: The Benson information
**    temperature: The temperature needed
**    S: The entropy at the given temperature
**
**    The entropy is calculated by:  
**      - Tm = The mean temperature between 298 and T
**      - Cp(Tm) = The heat capacity at this temperature
**      - S0 = The entropy at 298 degrees
**      - S = S0 + Cp(Tm) ln(T/298)
**
**  REMARKS
**
*/
double CalculateEntropy(BensonBaseTableObject& benson, double temperature)
     {
     double Tm = (temperature + STANDARD_TEMP)/2.0;
     double rT = temperature/STANDARD_TEMP;
     double Cp = benson.CalculateHeatCapacity(Tm);
     double S0 = benson.StandardEntropy;
     
     double S = S0 + Cp * log(rT);
     
     return S;
     }
 
/*F  S = CalculateEntropy(chemkin,temperature)  . from ChemkinBaseTableObject
**
**  DESCRIPTION
**    chemkin: The CHEMKIN information
**    temperature: The temperature needed
**    S: The entropy at the given temperature
**
**    The entropy is calculated by calling the CalculateEntropy 
**    routine in the CHEMKIN structure
**
**  REMARKS
**
*/
double CalculateEntropy(ChemkinBaseTableObject& chemkin, double temperature)
     {
     return chemkin.CalculateChemkinEntropy(temperature);
     }
/*F H = CalculateEnthalpy(benson,temperature) . .  from BensonBaseTableObject
**
**  DESCRIPTION
**    benson: The Benson thermo information
**    temperature: The temperature needed
**    H: The calculated enthalpy
**
**    The Enthalpy is calculated by:
**      - Tm = The mean temperature between 298 and T
**      - Cp(Tm) = The heat capacity at this temperature
**      - H0 = The enthalpy at 298 degrees
**      - H = H0 + Cp(Tm)*(T-298)
**     
**  REMARKS
**
*/
double CalculateEnthalpy(BensonBaseTableObject& benson, double temperature)
     {
     double Tm = (temperature + STANDARD_TEMP)/2.0;
     double Cp = benson.CalculateHeatCapacity(Tm);
     double H0 = benson.StandardEnthalpy;
     
     double H = H0 + Cp * (temperature - STANDARD_TEMP);
     
     return H;
     }
/*F H = CalculateEnthalpy(chemkin,temperature)  . from ChemkinBaseTableObject
**
**  DESCRIPTION
**    chemkin: The chemkin thermo information
**    temperature: The temperature needed
**    H: The calculated enthalpy
**
**    The Enthalpy is calculated by calling the CalculateEnthalpy
**    of the Chemkin class
**     
**  REMARKS
**
*/
double CalculateEnthalpy(ChemkinBaseTableObject& chemkin, double temperature)
     {
     return chemkin.CalculateChemkinEnthalpy(temperature);
     }
/*F G = CalculateFreeEnergyChange(thermo,temperature) . . . . . . . . Delta G
**
**  DESCRIPTION
**    thermo: The thermodynamic information
**    temperature: The temperature needed
**    G: The Gibbs Free Energy Change
**
**    The G is calculated by: G = H - T * S.  The values of H and 
**    S are calculated using the CalculateEnthalpy and CalculateEntropy
**    routines, respectively.
**
**  REMARKS
**
*/
template <class ThermoObject>
double CalculateFreeEnergyChange(ThermoObject& thermo, double temperature)
     {
     double H = CalculateEnthalpy(thermo,temperature);
     double S = CalculateEntropy(thermo,temperature);
     double G = H - temperature * S;
     return G;
     }
 
/*F K = CalculateEquilibrium(therm,temperature) . .  Equilibrium for Molecule
**
**  DESCRIPTION
**    therm: The thermodynamic information
**    temperature: The temperature needed
**    K: The equilibrium constant needed
**
**    The K is calculated in standard SI units.  It is meant to 
**    be used in calculating the equilibrium constants of reactions
**    where it will become dimensionless.
**
**    The K is calculated using G = -RTln(K)
**
**  REMARKS
**
*/
template <class ThermoObject>
double CalculateEquilibrium(ThermoObject& therm, double temperature)
     {
     double G = CalculateFreeEnergyChange(therm,temperature);
     double e = - G/(GAS_CONSTANT * temperature);
     double K = (double) exp(e);
     return K;
     }

 
/*C EquilibriumForTemperature . . . . . . . . . .  K for list of temperatures
**
**  DESCRIPTION
**    The operator takes the temperature and converts it to the
**    equilibrium value by calling CalculateEqulibrium.
**
**  REMARKS
**
*/
template<class ThermObject>
class EquilibriumForTemperature
     {
     ThermObject& Therm;
 public:
     EquilibriumForTemperature(ThermObject& therm)
	  : Therm(therm)
	       {
	       }
     double operator()(const double temperature)
	  {
	  return CalculateEquilibrium(Therm,temperature);
	  }
     };
/*F klist = CalculateEquilibrium(therm,temperatures)  . . . . . . Equilibrium
**
**  DESCRIPTION
**    therm: The thermodynamic information
**    temperatures: The list of temperatures
**    klist: The corresponding list of equilibrium values
**
**  REMARKS
**
*/
template <class ThermoObject>
ObjectList<double> CalculateEquilibrium(ThermoObject& therm,
				       ObjectList<double> temperatures)
     {
     ObjectList<double> Ks;
     EquilibriumForTemperature<ThermoObject> trns(therm);
     
     transform(temperatures.begin(),
	       temperatures.end(),
	       back_insert_iterator< ObjectList<double> >(Ks),
	       trns);
     return Ks;
     }

 
/*F mat = KTemperatureMatrix(temperatures)  . . . . . . .  Coefficient matrix
**
**  DESCRIPTION
**    temperatures: The list of temperatures
**    mat: The coefficient matrix
**
**    The matrix is set up for the least squares analysis of the 
**    matrix for putting K in the form K = AT^n exp(E/T)
**
**    The following values are put in the matrix:
**    - mat[i][0] = T
**    - mat[i][1] = log(T)
**    - mat[i][2] = 1/T
**
**  REMARKS
**
*/
MatrixNumeric KTemperatureMatrix(ObjectList<double> temperatures)
     {
     int n = temperatures.size();
     MatrixNumeric mat(n,3);
     ObjectList<double>::iterator temp = temperatures.begin();
     for(int i=0;i!=n;i++)
	  {
	  mat[i][0] = 1.0;
	  mat[i][1] = log(*temp);
	  mat[i][2] = 1/(*temp);
	  temp++;
	  }
     return mat;
     }
/*F vec = LogEquilibrium(therm, temperaturees)   Log of the Equilibrium value
**
**  DESCRIPTION
**    therm: The thermodynamic values
**    temperatures: The list of temperatures to compute the K values
**    vec: The vector of log(K) values
**
**    This is the vector used in the least squares analysis
**
**  REMARKS
**
*/
template <class ThermObject>
VectorNumeric LogEquilibrium(ThermObject& therm, ObjectList<double> temperatures)
     {
     VectorNumeric vec(temperatures.size());
     
     ObjectList<double> ks = CalculateEquilibrium(therm,temperatures);
     copy(ks.begin(),
	  ks.end(),
	  vec.begin());
     VectorNumeric::iterator k;
     for(k = vec.begin(); k != vec.end() ; k++)
	  (*k) = log((double) (*k));
	  
     return vec;
     }
 
/*S Constructors
*/
/*F BensonBaseTableObject(ThermoObj)  . . BensonBaseTableObject constructor. 
**
**  DESCRIPTION
**    ThermoObj: param.
**
**    Takes ChemkinBaseTableObject and converts it to BensonBaseTableObject.
**     
**  REMARKS
**
*/
BensonBaseTableObject::BensonBaseTableObject(ChemkinBaseTableObject& ThermoObj)
     {
     PolyFlag=1;
     BensonFormulaDescriptor=ThermoObj.SpeciesName;
     CalculateChemkinHeatCapacity calc(ThermoObj);
     Temps.AddObject(200.0);
     Temps.AddObject(400.0);
     Temps.AddObject(600.0);
     Temps.AddObject(800.0);
     Temps.AddObject(1000.0);
     Temps.AddObject(1200.0);
     double array1[]=LOWER_TEMPS;
     double array2[]=UPPER_TEMPS;   
     
     VectorNumeric dummy_array1;
     VectorNumeric dummy_array2;
     for(unsigned int i=0;i<4;i++)
       {
	 dummy_array1.AddObject(array1[i]);
	 dummy_array2.AddObject(array2[i]);
       }

     VectorNumeric::iterator t1;
     for(t1=Temps.begin(); t1 != Temps.end();t1++)
       {
	 double cp = calc(*t1);
	 CpS.AddObject(cp);
       }
     StandardEnthalpy=ThermoObj.CalculateChemkinEnthalpy(STANDARD_TEMP);
     StandardEntropy=ThermoObj.CalculateChemkinEntropy(STANDARD_TEMP);
     };
/*F BensonBaseTableObject(ThermoObj, vec) . . . . . . . . Class constructor. 
**
**  DESCRIPTION
**    ThermoObj: param.
**    vec: param.
**
**    Takes ChemkinBaseTableObject and converts it to BensonBaseTableObject.
**    The second argument is a vector of temperatures for which CpS are to be calculated.
**     
**  REMARKS
**
*/
BensonBaseTableObject::BensonBaseTableObject(ChemkinBaseTableObject& ThermoObj, const VectorNumeric& vec)
     {
 
     PolyFlag=1;
     BensonFormulaDescriptor=ThermoObj.SpeciesName;
     CalculateChemkinHeatCapacity calc(ThermoObj);
     

     copy(vec.begin(), vec.end(), 
	  back_insert_iterator<VectorNumeric >(Temps));
     
     
     transform(Temps.begin(), Temps.end(),
	       back_insert_iterator<VectorNumeric >(CpS),
	       calc);
     
    
     
     StandardEnthalpy=ThermoObj.CalculateChemkinEnthalpy(STANDARD_TEMP);
     StandardEntropy=ThermoObj.CalculateChemkinEntropy(STANDARD_TEMP);
     
     }
/*F ChemkinBaseTableObject(ThermoObj) .  ChemkinBaseTableObject constructor. 
**
**  DESCRIPTION
**    ThermoObj: param.
**
**    Takes BensonBaseTableObject and converts it to ChemkinBaseTableObject.
**     
**  REMARKS
**
*/
ChemkinBaseTableObject::ChemkinBaseTableObject(ChemkinBaseTableObject& ThermoObj)
  : SpeciesName(ThermoObj.SpeciesName),
    Date(ThermoObj.Date),
    AtSymbAndFormula(ThermoObj.AtSymbAndFormula),
    PhaseDescriptor(ThermoObj.PhaseDescriptor),
    FormulaDescriptor(ThermoObj.FormulaDescriptor),
    LowerBoundTemp(ThermoObj.LowerBoundTemp),
    UpperBoundTemp(ThermoObj.UpperBoundTemp),
    CommonTemp(ThermoObj.CommonTemp),
    SimplyOne(ThermoObj.SimplyOne),
    SimplyTwo(ThermoObj.SimplyTwo),
    SimplyThree(ThermoObj.SimplyThree),
    SimplyFour(ThermoObj.SimplyFour),
    y(ThermoObj.y),
    UpperCoeff(ThermoObj.UpperCoeff),
    LowerCoeff(ThermoObj.LowerCoeff)
{
}

/*F BuildChemkinMatrix(VectorOfTemps) . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    VectorOfTemps: param.
**
**    Builds matrix for system of linear equations, which solution represents  Chemkin 
**    coefficients a1-a5. Continuity of heat capacity function at the common temperature
**    is taken into account and is reflected in the structure of matrix.
**    
**  REMARKS
**
*/
MatrixNumeric ChemkinBaseTableObject::BuildChemkinMatrix(const VectorNumeric& VectorOfTemps)
     {
     unsigned int i;
     unsigned int j;
     int count1;
     int count2;
     
     MatrixNumeric Mat1(VectorOfTemps.size()+1, VectorOfTemps.size()+1);
     for(i=0; i<Mat1.size()-1; i++)
	  for(j=0; j<Mat1[0].size(); j++)
	       Mat1[i][j]=pow(VectorOfTemps[i], (double) j);
//   Last row - derivative
     count1=Mat1.size()-1;
     count2=VectorOfTemps.size()-1;    
     Mat1[count1][0]=0.0;
     for(j=1; j<Mat1[0].size(); j++)
	  Mat1[count1][j]=j*pow(VectorOfTemps[count2], (double) j-1);
     return Mat1;
     
     }
 
/*F ChemkinBaseTableObject(ThermoObj) .  ChemkinBaseTableObject constructor. 
**
**  DESCRIPTION
**    ThermoObj: param.
**
**    Takes BensonBaseTableObject and converts it to ChemkinBaseTableObject.
**     
**  REMARKS
**
*/
ChemkinBaseTableObject::ChemkinBaseTableObject(BensonBaseTableObject& ThermoObj)
: UpperCoeff(7, 0.0), LowerCoeff(7, 0.0)
     {
     double array1[]=LOWER_TEMPS;
     double array2[]=UPPER_TEMPS;   
     
     VectorNumeric LowerChemkinTemps;
     VectorNumeric UpperChemkinTemps;
     for(unsigned int i=0;i<4;i++)
       {
	 LowerChemkinTemps.AddObject(array1[i]);
	 UpperChemkinTemps.AddObject(array2[i]);
       }
     SpeciesName=ThermoObj.BensonFormulaDescriptor;
     
     LowerBoundTemp=LowerChemkinTemps[0];
     UpperBoundTemp=UpperChemkinTemps[2];
     CommonTemp=LowerChemkinTemps[3];
     
     
     SimplyOne=1;
     SimplyTwo=2;
     SimplyThree=3;
     SimplyFour=4;

     VectorNumeric UpperChemkinCpS;
     
     VectorNumeric LowerChemkinCpS;
     

     CalculateBensonHeatCapacity calc(ThermoObj);
     
     
//   Calculating heat capacities for lower temperatures 
     
     transform(LowerChemkinTemps.begin(), LowerChemkinTemps.end(),
	       back_insert_iterator<VectorNumeric >(LowerChemkinCpS),
	       calc);

//  Calculating heat capacity derivative for T=Tcommon (1000.00 K)

     LowerChemkinCpS.push_back(ThermoObj.CalculateHeatCapacityDerivative(COMMON_TEMP)*COMMON_TEMP);

     			          
//   Calculating heat capacities for upper temperatures 
   
     transform(UpperChemkinTemps.begin(), UpperChemkinTemps.end(),
	       back_insert_iterator<VectorNumeric >(UpperChemkinCpS),
	       calc);
     
//  Calculating heat capacity derivative for T=Tcommon (1000.00 K)

     UpperChemkinCpS.push_back(ThermoObj.CalculateHeatCapacityDerivative(COMMON_TEMP)*COMMON_TEMP);     
 
     VectorNumeric norm_vector_lower=LowerChemkinTemps;
     NormalizeVector(norm_vector_lower, COMMON_TEMP);
     MatrixNumeric MatForFit1=BuildChemkinMatrix(norm_vector_lower);
//   After gauss_jordan LowerChemkinCpS represent solution: lower a1-a5
     MatrixUtilities matutil(MatForFit1);
     matutil.gauss_jordan(MatForFit1, LowerChemkinCpS, MatForFit1.size());

     copy(LowerChemkinCpS.begin(), LowerChemkinCpS.end(), LowerCoeff.begin());
     ChemkinDenormalize(LowerCoeff, COMMON_TEMP);
//   ***********************************************************************

     VectorNumeric norm_vector_upper=UpperChemkinTemps;
     NormalizeVector(norm_vector_upper, COMMON_TEMP);
     MatrixNumeric MatForFit2=BuildChemkinMatrix(norm_vector_upper);
//   After gauss_jordan UpperChemkinCpS represent solution: upper a1-a5
     
     matutil.gauss_jordan(MatForFit2, UpperChemkinCpS, MatForFit2.size());

     copy(UpperChemkinCpS.begin(), UpperChemkinCpS.end(), UpperCoeff.begin());
     ChemkinDenormalize(UpperCoeff, COMMON_TEMP);

//   Get lower a6 from enthalpy fit: a6=H0-(a1*T+a2/2*T^2+a3/3*T^3+a4/4*T^4+a5/5*T^5)
//   T=300.00 K

     double En,En1,PEn;
     
     double Tlower = COMMON_TEMP - 10.0;
     double Tupper = COMMON_TEMP + 10.0;

     En = CalculateEnthalpy(ThermoObj,Tlower);
     En1 = En/GAS_CONSTANT;
     PEn = CalculatePartialEnthalpy(Tlower);

     LowerCoeff[5]= En1 - PEn;
//    Get upper a6 from enthalpy fit at common temperature
     En = CalculateEnthalpy(ThermoObj,Tupper);
     En1 = En/GAS_CONSTANT;
     PEn = CalculatePartialEnthalpy(Tupper);

     UpperCoeff[5]= En1 - PEn;
//   Get lower a7 from entropy fit: a7=S0-(a1*lnT+a2*T+a3/2*T^2+a4/3*T^3+a5/4*T^4)
//   T=300.00 K

     LowerCoeff[6]=
	  CalculateEntropy(ThermoObj,Tlower)/GAS_CONSTANT
	       - CalculatePartialEntropy(Tlower);

//  Get upper a7 from entropy fit at common temperature
    
     UpperCoeff[6]= 
	  CalculateEntropy(ThermoObj,Tupper)/GAS_CONSTANT
	       - CalculatePartialEntropy(Tupper);

     }
/*S Calculate
 */
/*F  CalculateChemkinEnthalpy(Temper) . . . . . . . . . . . . . . .  enthalpy
**
**  DESCRIPTION
**    Temper: param.
**
**    Calculates entahlpy using Chemkin enthalpy function representation.
**    
**  REMARKS
**
*/
double ChemkinBaseTableObject::CalculateChemkinEnthalpy(const double& Temper)
     {
     int i;
     double summa;
     
     summa=0.0;     

     if(Temper<CommonTemp)
	  {	  
	  for(i=0; i<=4; i++)
	       summa+=LowerCoeff[i]*pow(Temper, i+1)/(i+1);
	  summa+=LowerCoeff[5];
          }
     else 
          {	  
	  for(i=0; i<=4; i++)
	       summa+=UpperCoeff[i]*pow(Temper, i+1)/(i+1);
	  summa+=UpperCoeff[5];
          };
     
     summa *= GAS_CONSTANT;
     
     return summa;
     
     }
/*  CalculatePartialEnthalpy(Temper)  . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.
**
**    Calculates part of entahlpy using Chemkin enthalpy function representation,
**    excluding the last term (Chemkin coefficient a6).
**    
**  REMARKS
**
*/
double ChemkinBaseTableObject::CalculatePartialEnthalpy(const double& Temper)
     {     
     int i;
     double summa;
         
     summa=0.0;
     
     if(Temper<CommonTemp)
	  {
	  for(i=0; i<=4; i++)
	       summa+=LowerCoeff[i]*pow(Temper, i+1)/(i+1);
          }
     else 
          {
	  for(i=0; i<=4; i++)
	       summa+=UpperCoeff[i]*pow(Temper, i+1)/(i+1);
          };
     
     
     return summa;
     
     }
/*  CalculateEntropy(Temper)  . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.
**
**    Calculates entropy using Chemkin entropy function representation.
**    
**  REMARKS
**
*/
double ChemkinBaseTableObject::CalculateChemkinEntropy(const double& Temper)
     {
     int i;
     double summa;
     
     summa=0.0;
     
     if(Temper<CommonTemp)
	  {
	  summa=LowerCoeff[0]*log(Temper);
	  for(i=1; i<=4; i++)
	       summa+=LowerCoeff[i]*pow(Temper, i)/i;
	  summa+=LowerCoeff[6];
	  }
     else 
          {
	  summa=UpperCoeff[0]*log(Temper);
	  for(i=1; i<=4; i++)
	       summa+=UpperCoeff[i]*pow(Temper, i)/i;
	  summa+=UpperCoeff[6];
	  };
     summa *= GAS_CONSTANT;
     
     return summa;
     
     }
/*  CalculatePartialEntropy(Temper) . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.
**
**    Calculates part of entropy using Chemkin entropy function representation,
**    excluding the last term (Chemkin coefficient a7).
**    
**  REMARKS
**
*/
double ChemkinBaseTableObject::CalculatePartialEntropy(const double& Temper)						 
     {
     int i;
     double summa;
     
     summa=0.0;
     
     if(Temper<CommonTemp)
	  {
	  summa=LowerCoeff[0]*log(Temper);
	  for(i=1; i<=4; i++)
	       summa+=LowerCoeff[i]*pow(Temper, i)/i;
	  }
     else 
	  {
	  summa=UpperCoeff[0]*log(Temper);
	  for(i=1; i<=4; i++)
	       summa+=UpperCoeff[i]*pow(Temper, i)/i;
	  };
     

     return summa;
     
     }
/*  CalculateHeatCapacity(Temper) . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.
**
**    Calculates heat capacity using Chemkin heat capacity function representation:
**    
**    cp = R * sum(i,1,5) [ a(i)*T^(i-1) ]
**
**
**  REMARKS
**
*/
double ChemkinBaseTableObject::CalculateHeatCapacity(const double& Temper)
						   
     {
     int i;
     double summa = 0.0;
     if(Temper<CommonTemp)
	  {
	    for(i=0; i<=4; i++)
		summa+=LowerCoeff[i]*pow(Temper, (double) i);
          }
     else
	  {
	    for(i=0; i<=4; i++)
		summa+=UpperCoeff[i]*pow(Temper, (double) i);
          };
     summa *= GAS_CONSTANT;
     return summa;     
     }
/*  NormalizeValue(x, factor) . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    x: param.
**    factor: param.
**
**    Divides value by a constant.
**    
**  REMARKS
**
*/
void ChemkinBaseTableObject::NormalizeValue(double& x, const double& factor)
     {
     x=x/factor;
     }

/*  NormalizeVector(x, factor)  . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    x: param.
**    factor: param.
**    
**  REMARKS
**
*/
void ChemkinBaseTableObject::NormalizeVector(VectorNumeric& x, const double& factor)
     {
     unsigned int i;
     
     for(i=0;i<x.size();i++)
	  x[i]=x[i]/factor;
     }
 
/*F ChemkinDenormalize(Coeffs,factor) . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Coeffs: param.
**    factor: Denormalization factor
**
**    Auxiliary routine used due to the fact that power products of temperature 
**    in the Chemkin polynomial expansion of heat capacity function are needed 
**    to be normalized in order that system of linear equations for finding Chemkin 
**    coefficients a1-a5 will be well-conditioned. Thus, coefficients a1-a5 have to be
**    scaled backwards.  
**
**    In addition, the coefficients for the heat capacity are defined 
**    for Cp/R, thus the gas constant is also divided out.
**
**    
**  REMARKS
**
*/
void ChemkinBaseTableObject::ChemkinDenormalize(VectorNumeric& Coeffs, const double& factor)
     {
     unsigned int i;
     
     for(i=0; i<Coeffs.size(); i++)
	  Coeffs[i]=Coeffs[i]/(GAS_CONSTANT * pow(factor,(double) i));
     }
/*F ReadTemperatures(stream)  . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    stream: param.
**
**    Reads vector of temperatures from BensonTable head line.
**    
**  REMARKS
**
*/   
void BensonTable::ReadTemperatures(istream &stream)
     {
     String TopLine;
     String word;
     double dummy;
     TopLine.ReadFullLine(stream);
//   Ignoring first three fields
     TopLine.IsolateNextWord(word, ' ');
     TopLine.EliminateLeadingBlanks();
     TopLine.IsolateNextWord(word, ' ');
     TopLine.EliminateLeadingBlanks();
     TopLine.IsolateNextWord(word, ' ');
     TopLine.EliminateLeadingBlanks();

     while(TopLine.size() > 1)
	  {
	  TopLine.IsolateNextWord(word, ' ');
	  TopLine.EliminateLeadingBlanks();
          dummy=word.ToFloat();
	  Temperatures.push_back(dummy);    
	  };
     }
/*  operator()(Temper)  . . . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.

**    Class CalculateChemkinHeatCapacity operator(). Returns heat capacity using
**    routine CalculateHeatCapacity of ChemkinBaseTableObject.
**    
**  REMARKS
**
*/
double CalculateChemkinHeatCapacity::operator()(const double& Temper)
	  {
	  return ThermoObj.CalculateHeatCapacity(Temper);
	  }
/*  operator()(Temper)  . . . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.

**    Class CalculateBensonHeatCapacity operator(). Returns heat capacity using
**    routine CalculateHeatCapacity of BensonBaseTableObject.
**    
**  REMARKS
**
*/
double CalculateBensonHeatCapacity::operator()(const double& Temper)
	  {
	  return ThermoObj.CalculateHeatCapacity(Temper);
	  }
/*S Clone
*/
/*F CopyClone(benson) . . . . . . . . . . . . . . . . . BensonBaseTableObject
**
**  DESCRIPTION
**    benson: The Table to be cloned
**
**  REMARKS
**
*/
void BensonBaseTableObject::CopyClone(BensonBaseTableObject *benson)
     {
     *this = *benson;
     }
 
/*F Clone() . . . . . . . . . . . . . . . . . . . . . . BensonBaseTableObject
**
**  DESCRIPTION
**    Create of clone of the table
**
**  REMARKS
**
*/
BensonBaseTableObject *BensonBaseTableObject::Clone()
     {
     BensonBaseTableObject *benson;
     benson->CopyClone(this);
     return benson;
     }
/*S Utility
*/
/*F FillHeatCapacityGaps(TempsForExistData, ExistData)  . . . . . . . . . .  
**
**  DESCRIPTION
**    TempsForExistData: param.
**    ExistData: param.

**    Calculates heat capacity for some particular temperature. Uses information
**    (namely value of PolyFlag) which interpolation method was used for constructing
**    CpS data.
**        
**  REMARKS
**
*/
void BensonBaseTableObject::FillHeatCapacityGaps(const VectorNumeric& TempsForExistData,
						 const VectorNumeric& ExistData)
     {
     int i;
     int j;
     
     if(ExistData.size()>3)
	  {	  
	  VectorNumeric sigmas(ExistData.size(), 0.0);
	  VectorNumeric params(ExistData.size(), 0.0);  
	  
	  PolyLeastSquares XXX(ExistData.size());
	  XXX.Calculate(TempsForExistData, ExistData, sigmas, params);
	  fill(CpS.begin(), CpS.end(), 0.0);
	  int cpssize = CpS.size();
	  int parsize = params.size();
	  for(i=0;i<cpssize;i++)
	       for(j=0;j<parsize;j++)
		    CpS[i]+=params[j]*pow(Temps[i],j);
          PolyFlag=1;
	  }          
     else
	  {
          VectorNumeric sigmas(3, 0.0);
	  VectorNumeric params(3, 0.0);
	  
	  LogLeastSquares YYY(ExistData.size());
	  YYY.Calculate(TempsForExistData, ExistData, sigmas, params);
	  fill(CpS.begin(), CpS.end(), 0.0);  
	  int cpssize = CpS.size();
	  for(i=0;i<cpssize;i++)
	       {
	       CpS[i]=params[0];
	       
	       CpS[i]+=params[1]*log(1+Temps[i]);
               CpS[i]+=params[2]*Temps[i];
               };	  
	            
          PolyFlag=2;
	  
	  };
     }
/*F CalculateHeatCapacityDerivative(Temper) . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.
**
**    Calculates heat capacity derivative for some particular temperature. 
**    Uses information (namely value of PolyFlag) which interpolation method
**    was used for constructing CpS data.
**      
**  REMARKS
**
*/
double BensonBaseTableObject::CalculateHeatCapacityDerivative(const double& Temper)
     {
     double summa;
     
          
     if(PolyFlag==1)
	  {	  
	  VectorNumeric sigmas(CpS.size(), 0.0);
	  VectorNumeric params(CpS.size(), 0.0);  
   
	  PolyLeastSquares XXX(CpS.size());
	  
	  XXX.Calculate(Temps, CpS, sigmas, params);
	  
          summa=0.0;
	  int parsize = params.size();
	  for(int i=1;i<parsize;i++)
	       summa+=i*params[i]*pow(Temper,i-1);
	  
	  }          
     else
	  {
          VectorNumeric sigmas(CpS.size(), 0.0);
	  VectorNumeric params(3, 0.0);
	  
	  LogLeastSquares YYY(CpS.size());
	  
	  YYY.Calculate(Temps, CpS, sigmas, params);
	
          summa=0.0;  	  
	  summa+=params[1]/(1+Temper);
	  summa+=params[2];
              	  	 	  
	  };
     
     return summa;
                         
     }
/*F CalculateHeatCapacity(Temper) . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    Temper: param.

**    Calculates heat capacity for some particular temperature.
**        
**  REMARKS
**
*/
double BensonBaseTableObject::CalculateHeatCapacity(const double& Temper)
     {
     int i;
     double summa;

          
     summa=0.0;
     
     
     if (PolyFlag==1)
	  {
	  VectorNumeric sigmas(Temps.size(),0.0);
          VectorNumeric params(Temps.size(),0.0);
          PolyLeastSquares XXX(Temps.size());
	  XXX.Calculate(Temps, CpS, sigmas, params);
	  int parsize = params.size();
	  for(i=0;i<parsize;i++)
	       summa+=params[i]*pow(Temper,i);	     
	  } 
     
     else 
	  {
	  			 
	  VectorNumeric sigmas(3,0.0);
          VectorNumeric params(3,0.0);
          LogLeastSquares YYY(Temps.size());
          YYY.Calculate(Temps, CpS, sigmas, params);
          summa=params[0];	       
	  summa+=params[1]*log(1+Temper);
          summa+=params[2]*Temper;       
 
	     
	  }; 
                 
     return summa;
     
     }
/*S IO
*/ 
/*F out = print(stream) . . . . . . . . . . . . . . . . BensonBaseTableObject
**
**  DESCRIPTION
**    stream: The output stream
**    out: The stream after print
**
**  REMARKS
**
*/
ostream& BensonBaseTableObject::print(ostream& stream) const
     {
     int i;     

     stream.width(22);
     stream << BensonFormulaDescriptor;
     
     
     stream.setf(ios::fixed, ios::floatfield);
     stream.setf(ios::showpos);
     stream.setf(ios::showpoint);
     stream.precision(2);
    
     stream.width(14);
     stream << StandardEnthalpy;
     stream.width(10);
     stream << StandardEntropy;

     int tmpsize = Temps.size();     
     for(i=0; i<tmpsize; i++)
	  {
	  stream.width(1);
          stream.width(10);
          stream << CpS[i];
          }
     
     stream << endl;
     
     return stream;
     }
 
/*F out = print(out)  . . . . . . . . . . . . . . . .  ChemkinBaseTableObject
**
**  DESCRIPTION
**    out: The output stream
**
**  REMARKS
**
*/
ostream& ChemkinBaseTableObject::print(ostream& stream) const
     {
       String name = FillStringToLength(18,SpeciesName);
       stream << setw(18) << setfill(' ') << name;
       String date = FillStringToLength(6,Date);
       stream << setw(6) << date;
       stream.width(20);
       stream << AtSymbAndFormula;
       stream.width(1);
       if(PhaseDescriptor.size() <= 1)
	 stream << setw(1) << "G";
       else
	 stream << setw(1) << PhaseDescriptor;
       stream.width(10);

       stream.setf(ios::fixed, ios::floatfield);
       stream.precision(2);
       
       stream << LowerBoundTemp;
       stream.width(1);
     stream.width(10);
     stream.setf(ios::fixed, ios::floatfield);
     stream.precision(2);
     stream << UpperBoundTemp;
     stream.width(1);
     stream.width(8);
     stream.setf(ios::fixed, ios::floatfield);
     stream.precision(2);
     stream << CommonTemp;
     stream.width(5);
     stream << "      ";


     stream.width(1);
     stream << "1";
     stream << endl;

//
     stream.width(15);
     stream.setf(ios::scientific, ios::floatfield);
     stream.precision(8);
     stream.setf(ios::showpos);
     stream.setf(ios::showpoint);

//  Coefficients a1-a5 for upper temperature region

     stream << UpperCoeff[0];
     stream.width(15);
     stream << setw(15) << UpperCoeff[1];
     stream << setw(15) << UpperCoeff[2];
     stream << setw(15) << UpperCoeff[3];
     stream << setw(15) << UpperCoeff[4];

// Four blanks     

     stream << "    " ;
     stream.width(1);
     stream.unsetf(ios::showpos);
     stream << "2";
     stream << endl;


//   Coefficients a6-a7 for upper temperature region
//   and a1, a2, a3 for lower temperature region

     stream.width(15);
     stream.setf(ios::showpos);
     stream << setw(15) << UpperCoeff[5];
     stream << setw(15) << UpperCoeff[6];
     stream << setw(15) << LowerCoeff[0];
     stream << setw(15) << LowerCoeff[1];
     stream << setw(15) << LowerCoeff[2];

//   Four blanks

     stream << "    " ;
     stream.width(1);
     stream.unsetf(ios::showpos);
     stream << "3";
     stream << endl;

//  Coefficients a4, a5, a6, a7 for lower temperature region

     stream.width(15);
     stream.setf(ios::showpos);
     stream << setw(15) << LowerCoeff[3];
     stream << setw(15) << LowerCoeff[4];
     stream << setw(15) << LowerCoeff[5];
     stream << setw(15) << LowerCoeff[6];

// Fifteen blanks 

     stream << "                   " ;
     stream.width(1);
     stream.unsetf(ios::showpos);
     stream << "4";
     stream << endl;

//

     return stream;     
     }
/*F out = operator>>(stream, ThermoObj) BensonBaseTableObject input operator.
**
**  DESCRIPTION
**    stream: param.
**    ThermoObj: param. 
**    out: stream after output
**
**    BensonBaseTableObject input operator. Second argument is BensonBaseTableObject type.
**    Reads Benson table object data from a stream (file). In case there are gaps for CpS
**    data fills them, using interpolation. If number of input CpS is less or equal to 3
**    uses logarithmic-polynomial fit to construct the missing ones.   
**     
**    If the read in line is blank, then the name of ThermObj is left blank.  The
**    rest of the information is left untouched.
**
**  REMARKS
**
*/
istream& operator>>(istream &stream, BensonBaseTableObject& ThermoObj)
     {
     
     String BensonLine;
     String word;
     unsigned int i;
     SYS_RETURN dummy;
     VectorNumeric ExistData;
     VectorNumeric TempsForExistData;
     
          
     BensonLine.ReadFullLine(stream);
     if(BensonLine.size() > 1)
	  {
	  dummy=BensonLine.EliminateLeadingBlanks();
     
	  BensonLine.IsolateNextWord(word, ' ');
	  ThermoObj.BensonFormulaDescriptor=word;
	  
	  dummy=BensonLine.EliminateLeadingBlanks();
	  BensonLine.IsolateNextWord(word, ' '); 
	  ThermoObj.StandardEnthalpy=word.ToFloat();
	  
	  dummy=BensonLine.EliminateLeadingBlanks();
	  BensonLine.IsolateNextWord(word, ' '); 
	  ThermoObj.StandardEntropy=word.ToFloat();     
	  
	  for(i=0; i<ThermoObj.Temps.size(); i++)
	       {
	       dummy=BensonLine.EliminateLeadingBlanks();
	       BensonLine.IsolateNextWord(word, ' ');
	       if(word[0]==GAP_FILL_CHAR)
		    {
		    ThermoObj.GapTrack.push_back(IS_GAP);
		    }
	       else
		    {
		    ThermoObj.GapTrack.push_back(NO_GAP);
		    ThermoObj.CpS[i]=word.ToFloat();
		    ExistData.push_back(word.ToFloat());
		    TempsForExistData.push_back(ThermoObj.Temps[i]);
		    
		    };        
	       
	       };
	  
	  
	  if(ExistData.size()!=ThermoObj.CpS.size())
	       {
	       ThermoObj.FillHeatCapacityGaps(TempsForExistData, ExistData);
	       }
	  else
	       {
	       ThermoObj.PolyFlag=1;
	       };
	  }
     else
	  ThermoObj.BensonFormulaDescriptor = BensonLine;
     
     return stream;
     }
/*F operator>>(stream, ThermoObj) . .  ChemkinBaseTableObject input operator.
**
**  DESCRIPTION
**    stream: param.
**    ThermoObj: param. 
**
**    ChemkinBaseTableObject input operator. Second argument is ChemkinBaseTableObject type.
**    Reads all Chemkin table object data from a stream (file).
**       
**  REMARKS
**
*/
istream& operator>>(istream &stream, ChemkinBaseTableObject& ThermoObj)
     {
     String word; String ChemkinLine;

     ChemkinLine.ReadFullLine(stream);
     if(ChemkinLine.size() > 1)
	  {
	  ThermoObj.SpeciesName=ChemkinLine.Isolate(0,17);
	  ThermoObj.Date=ChemkinLine.Isolate(18,23);
	  ThermoObj.AtSymbAndFormula=ChemkinLine.Isolate(24,43);
	  ThermoObj.PhaseDescriptor=ChemkinLine.Isolate(43,44);
	  ThermoObj.PhaseDescriptor.EliminateLeadingBlanks();
	  word=ChemkinLine.Isolate(45,55);
	  ThermoObj.LowerBoundTemp=word.ToFloat();
	  word=ChemkinLine.Isolate(55,65);
	  ThermoObj.UpperBoundTemp=word.ToFloat();
	  word=ChemkinLine.Isolate(65,73);
	  ThermoObj.CommonTemp=word.ToFloat();
	  ThermoObj.FormulaDescriptor=ChemkinLine.Isolate(73,78);
	       
	  ChemkinLine.ReadFullLine(stream);
	  word=ChemkinLine.Isolate(0,15);
	  ThermoObj.UpperCoeff[0]=word.ToFloat();
	  word=ChemkinLine.Isolate(15,30);
	  ThermoObj.UpperCoeff[1]=word.ToFloat(); 
	  word=ChemkinLine.Isolate(30,45);
	  ThermoObj.UpperCoeff[2]=word.ToFloat(); 
	  word=ChemkinLine.Isolate(45,60);
	  ThermoObj.UpperCoeff[3]=word.ToFloat(); 
	  word=ChemkinLine.Isolate(60,75);
	  ThermoObj.UpperCoeff[4]=word.ToFloat(); 
		    
	  ChemkinLine.ReadFullLine(stream);
	  word=ChemkinLine.Isolate(0,15);
	  ThermoObj.UpperCoeff[5]=word.ToFloat();
	  word=ChemkinLine.Isolate(15,30);
	  ThermoObj.UpperCoeff[6]=word.ToFloat(); 
	  word=ChemkinLine.Isolate(30,45);
	  ThermoObj.LowerCoeff[0]=word.ToFloat(); 
	  word=ChemkinLine.Isolate(45,60);
	  ThermoObj.LowerCoeff[1]=word.ToFloat(); 
	  word=ChemkinLine.Isolate(60,75);
	  ThermoObj.LowerCoeff[2]=word.ToFloat(); 
	  
	  ChemkinLine.ReadFullLine(stream);
	  word=ChemkinLine.Isolate(0,15);
	  ThermoObj.LowerCoeff[3]=word.ToFloat();
	  word=ChemkinLine.Isolate(15,30);
	  ThermoObj.LowerCoeff[4]=word.ToFloat(); 
	  word=ChemkinLine.Isolate(30,45);
	  ThermoObj.LowerCoeff[5]=word.ToFloat(); 
	  word=ChemkinLine.Isolate(45,60);
	  ThermoObj.LowerCoeff[6]=word.ToFloat(); 
	  }
	  
     return stream;
     }

/*S OutputTable
 */
  
/*F OutputThermoTableStyle()  . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
OutputThermoTableStyle::OutputThermoTableStyle()
  : Style(THERMO_ASCII),
    Precision(5),
    Width(15),
    HeadLength(20),
    BensonProperty("Benson")
{
  Temperatures.AddObject(500);
  Temperatures.AddObject(800);
  Temperatures.AddObject(1000);
  Temperatures.AddObject(1200);
}
/*F OutputThermoTableStyle(style) . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    style: The style of the table
**
**  REMARKS
**
*/
OutputThermoTableStyle::OutputThermoTableStyle(const unsigned int style)
  : Style(style),
    Precision(2),
    Width(10),
    HeadLength(20),
    BensonProperty("Benson")
{
  Temperatures.AddObject(500);
  Temperatures.AddObject(800);
  Temperatures.AddObject(1000);
  Temperatures.AddObject(1200);
}
/*F out = OutputThermoHeader(out,title) . output the header of reaction table
**
**  DESCRIPTION
**    out: The output stream
**    title: The title of the table
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoHeader(ostream& out,
						 const String& title)
{
  switch(OutputThermoTableStyle::Style)
    {
    case THERMO_TEX:
      OutputThermoHeaderLaTeX(out,title);
      break;
    case THERMO_ASCII:
      OutputThermoHeaderASCII(out,title);
      break;
    case THERMO_HTML:
      OutputThermoHeaderASCII(out,title);
      break;
    }
  return out;
}
/*F out = OutputThermoHeaderASCII(out,title)  . . . . . . .  ASCII the header
**
**  DESCRIPTION
**    out: The output stream
**    title: The title of the table
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoHeaderASCII(ostream& out,
						      const String& title)
{
  int rhead = 20;
  int temps = Temperatures.size()*Width;
  int linelength = rhead + 2*Width + temps;

  RepeatOutNTimes(out,'=',linelength);
  out << "\n";
  
  OutputStringInWidth(out,linelength,title);
  out << "\n";
  

  RepeatOutNTimes(out,' ',rhead);
  OutputStringInWidth(out,2*Width,"Standard");
  OutputStringInWidth(out,temps,"Temperatures");
  out << "\n";

  RepeatOutNTimes(out,' ',rhead);
  OutputStringInWidth(out,Width,"Enthalpy");
  OutputStringInWidth(out,Width,"Entropy");

  ObjectList<double>::iterator iter;
  for(iter=Temperatures.begin();
      iter!=Temperatures.end();
      iter++)
    {
      out << setw(Width) << *iter;
    }
  
  out << "\n";

  RepeatOutNTimes(out,'-',linelength);
  out << "\n";

  return out;
}
/*F out = OutputThermoHeaderLaTex(out,title)  . . . . . . .  LaTeX the header
**
**  DESCRIPTION
**    out: The output stream
**    title: The title of the table
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoHeaderLaTeX(ostream& out,
						      const String&)
{

  out << "\\begin{tabular}{|l|c|c|";
  for(unsigned int i=0;i != Temperatures.size();i++)
    {
      out << "c|";
    }
  
  out << "}\\hline\n";
  out << "&";
  out << "\\multicolumn{2}{|c|}{Standard}";
  out << "&";

  out << "\\multicolumn{";
  out << Temperatures.size();
  out << "}{|c|}{Temperatures}";
  out << "\\\\\n";

  out << " & Enthalpy & Entropy ";
  ObjectList<double>::iterator iter;
  for(iter=Temperatures.begin();
      iter!=Temperatures.end();
      iter++)
    {
      out << "&";
      out << setw(Width) << *iter;
    }
  out << "\\\\\\hline\n";

  return out;
}
/*F out = OutputThermoTail(out,title) . . . output the tail of reaction table
**
**  DESCRIPTION
**    out: The output stream
**    title: The title of the table
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoTail(ostream& out)
{
  switch(OutputThermoTableStyle::Style)
    {
    case THERMO_TEX:
      OutputThermoTailLaTeX(out);
      break;
    case THERMO_ASCII:
      OutputThermoTailASCII(out);
      break;
    case THERMO_HTML:
      OutputThermoTailASCII(out);
      break;
    }
  return out;
}
/*F out = OutputThermoTailASCII(out)  . . . . . . . .  ASCII end of the table
**
**  DESCRIPTION
**    out: The output stream
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoTailASCII(ostream& out)
{
  unsigned int temps = Temperatures.size()*Width;
  unsigned int linelength = HeadLength + 2*Width + temps;

  RepeatOutNTimes(out,'=',linelength);
  out << "\n";

  return out;
}
 
/*F out = OutputThermoTailLaTeX(out)  . . . . . . . .  LaTeX end of the table
**
**  DESCRIPTION
**    out: The output stream
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoTailLaTeX(ostream& out)
{
  out << "\\hline\n\\end{tabular}\n";

  return out;
}
 
/*F out = OutputThermoElementASCII(out,benson,title)  . . . . . . ASCII Table
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoElementASCII(ostream& out,
							  BensonBaseTableObject& benson,
							  const String& title)
{
  out << title;
  RepeatOutNTimes(out,' ',HeadLength-strlen(title.c_str()));
  
  OutputThermoEnthalpy(out,benson);
  OutputThermoEntropy(out,benson);
  ObjectList<double>::iterator temp;
  for(temp=Temperatures.begin();
      temp!=Temperatures.end();
      temp++)
    {
      OutputThermoHeatCapacity(out,benson,*temp);
    }
  out << "\n";
  return out;
}
/*F out = OutputThermoElementLaTeX(out,benson,title)  . . . . . . LaTeX Table
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoElementLaTeX(ostream& out,
							  BensonBaseTableObject& benson,
							  const String& title)
{
  out << title;
  out << " & ";
  
  OutputThermoEnthalpy(out,benson);
  out << " & ";
  OutputThermoEntropy(out,benson);
  ObjectList<double>::iterator temp;
  for(temp=Temperatures.begin();
      temp!=Temperatures.end();
      temp++)
    {
      out << " & ";
      OutputThermoHeatCapacity(out,benson,*temp);
    }
  out << "\\\\\n";
  return out;
}
/*F out = OutputThermoEnthalpy(out,benson)  . . . . . . . .  Standard Enthapy
**
**  DESCRIPTION
**    out: The output stream
**    benson: The thermodynamic constants in Benson form
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoEnthalpy(ostream& out,
						      BensonBaseTableObject& benson)
{
  out << setw(Width) 
      << setprecision(Precision) 
      << setiosflags(ios::scientific) 
      << benson.StandardEnthalpy;
  return out;
}
/*F out = OutputThermoEntropy(out,benson) . . . . . . . . .  Standard Entropy
**
**  DESCRIPTION
**    out: The output stream
**    benson: The thermodynamic constants in Benson form
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoEntropy(ostream& out,
						     BensonBaseTableObject& benson)
{
  out << setw(Width) 
      << setprecision(Precision) 
      << setiosflags(ios::scientific) 
      << benson.StandardEntropy;
  return out;
}
/*F out = OutputThermoHeatCapacity(out,benson,temp) . . . . . .  Calculate Cp
**
**  DESCRIPTION
**    out: The output stream
**    benson: The thermodynamic constants in Benson form
**
**  REMARKS
**
*/
ostream& OutputThermoTableStyle::OutputThermoHeatCapacity(ostream& out,
							  BensonBaseTableObject& benson,
							  double temperature)
{
  double cp = benson.CalculateHeatCapacity(temperature);
  out << setw(Width) 
      << setprecision(Precision) 
      << setiosflags(ios::scientific) 
      << cp;
  return out;
} 
/*F ThermoTableStyle(style,title) . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    style: The style
**    title: The title
**
**  REMARKS
**
*/
ThermoTableStyle::ThermoTableStyle(const unsigned int style,
				   const String& title)
  : Style(style),
    LinesPerPage(60),
    Title(title)
{
}
/*F ThermoTableStyle(style,title) . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    style: The style
**    title: The title
**
**  REMARKS
**
*/
ThermoTableStyle::ThermoTableStyle(const OutputThermoTableStyle& style,
				   const String& title)
  : Style(style),
    LinesPerPage(60),
    Title(title)
{
}


 
/*F BensonTable() . . . . . . . . . . .  Class BensonTable empty constructor.
**
**  DESCRIPTION
**    Empty constructor.
**    
**  REMARKS
**
*/
BensonTable::BensonTable()
     {
     }
/*F CalculateChemkinHeatCapacity()  . . . . . . . . . . .  Empty constructor.
**
**  DESCRIPTION
**    Empty constructor.
**    
**  REMARKS
**
*/
CalculateChemkinHeatCapacity::CalculateChemkinHeatCapacity()
     {
     }
/*F CalculateChemkinHeatCapacity(_ThermoObj)  . . . . Class copy constructor.
**
**  DESCRIPTION
**    _ThermoObj: param.
**
**    Class CalculateChemkinHeatCapacity copy constructor.
**    
**  REMARKS
**
*/
CalculateChemkinHeatCapacity::CalculateChemkinHeatCapacity(ChemkinBaseTableObject& _ThermoObj)
: ThermoObj(_ThermoObj)
     {
     }
/*F CalculateBensonHeatCapacity() . . . . . . . . . . . .  Empty constructor.
**
**  DESCRIPTION
**    Empty constructor.
**    
**  REMARKS
**
*/
CalculateBensonHeatCapacity::CalculateBensonHeatCapacity()
     {
     }

/*F CalculateBensonHeatCapacity(_ThermoObj) . . . . . Class copy constructor.
**
**  DESCRIPTION
**    _ThermoObj: param.
**
**    Class CalculateBensonHeatCapacity copy constructor.
**    
**  REMARKS
**
*/
CalculateBensonHeatCapacity::CalculateBensonHeatCapacity(BensonBaseTableObject& _ThermoObj)
: ThermoObj(_ThermoObj)
     {
     }

/*F ChemkinBaseTableObject()  . . . ChemkinBaseTableObject empty constructor.
**
**  DESCRIPTION
**    Empty constructor.
**    
**  REMARKS
**
*/
ChemkinBaseTableObject::ChemkinBaseTableObject()
: UpperCoeff(7, 0.0), LowerCoeff(7, 0.0)
     {
     }
/*F BensonBaseTableObject() . . . .  BensonBaseTableObject empty constructor.
**
**  DESCRIPTION
**    Empty constructor.
**    
**  REMARKS
**
*/
BensonBaseTableObject::BensonBaseTableObject()
     {
     }



/*F BensonBaseTableObject(stream, Table)  . . . . . . . .  Class constructor.
**
**  DESCRIPTION
**    stream: param.
**    Table: param.

**    BensonBaseTableObject constructor, which reads Benson table object data from
**    stream (file) and assigns values to the members of the class (i.e. elements
**    of Benson table object).
**    
**  REMARKS
**
*/
BensonBaseTableObject::BensonBaseTableObject(istream &stream, BensonTable& Table)
     {
     Table.ReadTemperatures(stream); 

     vector<double> dummy(Table.Temperatures.size(), 0.0);

     copy(Table.Temperatures.begin(), Table.Temperatures.end(), 
	  back_insert_iterator<vector<double> >(Temps));

     copy(dummy.begin(), dummy.end(), 
	  back_insert_iterator<vector<double> >(CpS));
                
     }
