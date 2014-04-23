/*  FILE     Chemkin.cc
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
#include "FullSystem.hh"
#include "Dbase.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "ThermoProps.hh"
#include "Rxn.hh"
#include "Mechanism.hh"
#include "MechanismGraph.hh"
#include "MolStats.hh"
#include "MechLumping.hh"
#include "Flame.hh"
#include "Reaction/ThermoTables.hh"
#include "Reaction/Chemkin.hh"

#include "Basis/LineGraph.hh"
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#define CHEMKIN_THERMO_DATA_EXT "thm"
#define CHEMKIN_NAME_CORRS_EXT "crs"

bool Encode(CommBuffer& buffer, ChemkinBaseTableObject& thm);
bool Decode(CommBuffer& buffer, ChemkinBaseTableObject& thm);
ostream& ThermoGraphDataOut(BensonBaseTableObject& benson,
			    ChemkinBaseTableObject& chemkin,
			    double beginT, 
			    double endT,
			    double incT,
			    ostream& out);
/*S Graph
 */     
/*F GraphThermodynamicData(g) . . . . . . . . . . . . . . output data command
**
**  DESCRIPTION
**    g: The system infromation
**
**    The command has 4 arguments:
**    - The molecule name
**    - The begining temperature
**    - The final temperature
**    - The temperature increment
**
**    This command outputs the thermodynamic data to be used by graphic
**    routines.  The thermodynamic data is derived from the Benson
**    and Chemkin forms.  The data output is Enthapy, Entropy, FreeEnergy,
**    and Equilibrium as a function of temperature.
**
**  REMARKS
**
*/
int GraphThermodynamicData(ReactionSystemBase *g)
     {
     ChemkinSystemBase *global = (ChemkinSystemBase *) g;
     int ret=0;
     
     if(global->Inputs.size() < 1)
	  {
	  cout << "Expecting 4 arguments:\n";
	  cout << "    Molecule name\n";
          cout << "    Begin Temperature\n";
	  cout << "    End Temperature\n";
	  cout << "    Temperature Increment\n";
	  ret = 1;
	  }
     else
	  {
	  String molecule = global->Inputs.front();
	  global->Inputs.pop_front();
	  String beginT = global->Inputs.front();
	  global->Inputs.pop_front();
	  String endT = global->Inputs.front();
	  global->Inputs.pop_front();
	  String incT = global->Inputs.front();
	  global->Inputs.pop_front();
	 
	  String molstring("Molecule");
	  BaseDataDataBaseInformation *moldb = global->getDatabaseInfo(molstring);

	  int id = moldb->Names[molecule];
	  if(id != 0)
	       {
	       double bT = beginT.ToFloat();
	       double eT = endT.ToFloat();
	       double iT = incT.ToFloat();

	       RxnDataSimpleMolecule *molecule = (RxnDataSimpleMolecule *) moldb->ReadDBObject(id);
	       
	       PropertyTypeByName<BensonBaseTableObject> *bensonprop
		    = (PropertyTypeByName<BensonBaseTableObject> *) 
			 molecule->Properties.GetPointerToProperty("Benson");
     
	       PropertyTypeByName<ChemkinBaseTableObject> *chemkinprop
		    = (PropertyTypeByName<ChemkinBaseTableObject> *) 
			 molecule->Properties.GetPointerToProperty("ChemkinThermo");
	       ThermoGraphDataOut(bensonprop->Object,
				  chemkinprop->Object,
				  bT,eT,iT,cout);
	       }
	  }
     return 1;
     }

 
/*C
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class ChemkinGraphData
     {
 public:
     
     GraphData<double> temperature;
     GraphData<double> BensonEnthalpy;
     GraphData<double> BensonEntropy;
     GraphData<double> BensonFreeEnergyChange;
     GraphData<double> BensonEquilibrium;
     GraphData<double> ChemkinEnthalpy;
     GraphData<double> ChemkinEntropy;
     GraphData<double> ChemkinFreeEnergyChange;
     GraphData<double> ChemkinEquilibrium;
     };


     

/*F out = ThermoGraphDataOut(benson,chemkin,beginT,endT,incT,out)
**
**  DESCRIPTION
**    benson: The Benson Thermodynamic data
**    chemkin: The Chemkin Thermodynamic data
**    beginT: The beginning temperature of graph
**    endT: The final temperature
**    incT: The increment of the temperatures
**
**  REMARKS
**
*/
ostream& ThermoGraphDataOut(BensonBaseTableObject& benson,
			    ChemkinBaseTableObject& chemkin,
			    double beginT, 
			    double endT,
			    double incT,
			    ostream& out)
     {
     ChemkinGraphData cdata;
     
     out << "-------- Temperature, Enthalpy, Entropy, FreeEnergy, Equilibrium\n";
     out << benson;
     out << chemkin;
     
     for(double tp=beginT; tp <= endT; tp+=incT)
	  {
	  double t = CalculateEnthalpy(benson,tp);
	  cdata.BensonEnthalpy.AddObject(t);
	  t = CalculateEnthalpy(chemkin,tp);
	  cdata.ChemkinEnthalpy.AddObject(t);
	  
	  t = CalculateEntropy(benson,tp);
	  cdata.BensonEntropy.AddObject(t);
	  t = CalculateEntropy(chemkin,tp);
	  cdata.ChemkinEntropy.AddObject(t);
	  
	  t = CalculateFreeEnergyChange(benson,tp);
	  cdata.BensonFreeEnergyChange.AddObject(t);
	  t = CalculateFreeEnergyChange(chemkin,tp);
	  cdata.ChemkinFreeEnergyChange.AddObject(t);
	  
	  t = CalculateEquilibrium(benson,tp);
	  cdata.BensonEquilibrium.AddObject(t);
	  t = CalculateEquilibrium(chemkin,tp);
	  cdata.ChemkinEquilibrium.AddObject(t);
	  }

     out << beginT << " " << endT << " " << incT << "\n";
     
     cdata.BensonEnthalpy.CalcAndShowGraphData(out);
     cdata.ChemkinEnthalpy.CalcAndShowGraphData(out);
     cdata.BensonEntropy.CalcAndShowGraphData(out);
     cdata.ChemkinEntropy.CalcAndShowGraphData(out);
     cdata.BensonFreeEnergyChange.CalcAndShowGraphData(out);
     cdata.ChemkinFreeEnergyChange.CalcAndShowGraphData(out);
     cdata.BensonEquilibrium.CalcAndShowGraphData(out);
     cdata.ChemkinEquilibrium.CalcAndShowGraphData(out);

     ObjectList<double>::iterator be = cdata.BensonEnthalpy.Original.begin();
     ObjectList<double>::iterator ce = cdata.ChemkinEnthalpy.Original.begin();     
     ObjectList<double>::iterator bs = cdata.BensonEntropy.Original.begin();     
     ObjectList<double>::iterator cs = cdata.ChemkinEntropy.Original.begin();
     ObjectList<double>::iterator bg = cdata.BensonFreeEnergyChange.Original.begin();
     ObjectList<double>::iterator cg = cdata.ChemkinFreeEnergyChange.Original.begin();
     ObjectList<double>::iterator bq = cdata.BensonEquilibrium.Original.begin();
     ObjectList<double>::iterator cq = cdata.ChemkinEquilibrium.Original.begin();

     for(double t=beginT; t<= endT; t+=incT)
	  {
	  out << t;
	  out << " ";

	  out << *be;
	  out << " ";
	  out << *ce;
	  out << " ";
	  out << *bs;
	  out << " ";
	  out << *cs;
	  out << " ";
	  out << *bg;
	  out << " ";
	  out << *cg;
	  out << " ";
	  out << *bq;
	  out << " ";
	  out << *cq;
	  out << "\n";
	  
	  be++;
	  ce++;
	  bs++;
	  cs++;
	  bg++;
	  cg++;
	  bq++;
	  cq++;
	  }
     

     return out;
     }
/*S ReadInChemkinThermo
*/
 
/*F ReadInChemkinThermo(fileroot) . . . . . . . . . . . . CHEMKIN thermo data
**
**  DESCRIPTION
**    fileroot: The root of the data file name.  
**
**    This routine calls ReadInThermo with the CHEMKIN flag set to 
**    read in the thermodynamic data (with the molecule correspondences).
**
**  REMARKS
**
*/
void ChemkinSystemBase::ReadInChemkinThermo(const String& fileroot)
     {
     ReadInThermo(fileroot,CHEMKIN_INPUT);
     }
 
/*F ReadInAsBensonThermo(fileroot)  . . . . . . . .  Benson in CHEMKIN stored
**
**  DESCRIPTION
**    fileroot: The root of the data file name.  
**
**    This routine calls ReadInThermo with the BENSON flag set to 
**    read in the thermodynamic data (with the molecule correspondences).
**    The input is a Benson line, but the CHEMKIN form is stored.
**
**  REMARKS
**
*/
void ChemkinSystemBase::ReadInAsBensonThermo(const String& fileroot)
     {
     ReadInThermo(fileroot,BENSON_INPUT);
     }
/*F ReadInChemkinTable(in,table)  . . . . . . .  read in entire chemkin table
**
**  DESCRIPTION
**    in: The input stream
**    table: The list of chemkin thermo objects
**
**    This reads the entire chemkin file and stores the individual objects
**    in a list.  A while loop reads the file (repeated calls to the 
**    standard read operator) until the name is blank.
**
**  REMARKS
**
*/
void ReadInChemkinTable(istream& in, SearchableObjectList<String,ChemkinBaseTableObject>& table)
     {
     bool done = false;
     while(!done)
	  {
	  ChemkinBaseTableObject chemkinobject;
	  in >> chemkinobject;
	  if(chemkinobject.SpeciesName.size() > 1)
	       {
	       String name = chemkinobject.SpeciesName;
	       name.EliminateBlanks();
	       String noblanks = name;
	       cout << "Read: >";
	       cout << name;
	       cout << "< \n";
	       table[noblanks] = chemkinobject;
	       }
	  else
	       done = true;
	  }
     }
 
/*F ReadInAsBensonTable(in,table)  . . . . . . read CHEMKIN from benson form
**
**  DESCRIPTION
**    in: The input stream
**    table: The table of CHEMKIN values
**
**    This reads a an entire file of thermodynamic data in the benson
**    line form.  The line is then converted to the CHEMKIN form and
**    put into the table.  A while loop reads the file (repeated calls to the 
**    standard read operator) until the name is blank.
**
**  REMARKS
**
*/
void ReadInAsBensonTable(istream& in,  
			  SearchableObjectList<String,ChemkinBaseTableObject>& table)
     {
     bool done=false;

     BensonTable temps;
     temps.ReadTemperatures(in);
     BensonBaseTableObject benson(in,temps);

     while(!done)
	  {
	    
	  in >> benson;
	  if(benson.BensonFormulaDescriptor[0] != '\000')
	       {
	       ChemkinBaseTableObject chemkin(benson);
	       String name = benson.BensonFormulaDescriptor;
	       name.EliminateBlanks();
	       String noblanks = name;
	       cout << "Read: >";
	       cout << name;
	       cout << "\n";
	       table[noblanks] = chemkin;
	       }
	  else
	       done = true;
	  }
     }
 
/*S InsertChemkinIntoMolecule
*/
/*F InsertChemkinIntoMolecule(chemkin, standard, Table) insert CHEMKIN thermo info into molecule
**
**  DESCRIPTION
**    chemkin: The CHEMKIN molecule name (for table access)
**    standard: The standard molecule name
**    Table: The table of read in Chemkin Thermodynamic values (searchable by CHEMKIN name)
**
**    The molecule is read in (by converting the name to an ID), the thermo data is
**    found with the chemkin name and the CHEMKIN value is inserted as a property.
**
**  REMARKS
**
*/
void ChemkinSystemBase::InsertChemkinIntoMolecule(const String& chemkin,
						  const String& standard,
						  SearchableObjectList<String,ChemkinBaseTableObject>& Table)
     {
     bool result;
     
     int id = DBMolecules.DBMolecule.Names[standard];
     if(id != 0)
	  {
	  SimpleMolecule *molecule 
	       = (SimpleMolecule *) DBMolecules.DBMolecule.ReadDBObject(id);
	  
	  ChemkinBaseTableObject object = Table[chemkin];
	  if(object.SpeciesName.size() > 1)
	       {
	       PropertyTypeByName<ChemkinBaseTableObject> *prop
		    = new PropertyTypeByName<ChemkinBaseTableObject>("ChemkinThermo",
								     object);
	       molecule->Properties.AddObject(prop,"ChemkinThermo");
	       result = DBMolecules.DBMolecule.WriteDBObject(*molecule);
	       cout << *molecule;
	       }
	  else
	       {
	       cout << "Error: ";
	       cout << chemkin;
	       cout << " not in Chemkin table\n";
	       result = false;
	       }
	  
	  }
     else
	  {
	  cout << "Error: ";
	  cout << standard;
	  cout << " not found in Database\n";
	  }
     if(result)
	  {
	  cout << "Molecule Updated\n";
	  }
     else
	  cout << "Molecule Not Updated\n";
     }
/*F StoreChemkinFromCorrs(corrsdata,table)  .  use corrs to store in molecule
**
**  DESCRIPTION
**    corrsdata: The input stream of the correspondences
**    table: The table of CHEMKIN values
**
**    The correspondences determine
**    which molecules of the thermo table are to be used.  The correspondence
**    pairs are read until the end of the file.
**
**    The correspondence pairs consist of the CHEMKIN name, a blank and
**    then the standard name.
**
**  REMARKS
**
*/
void ChemkinSystemBase::StoreChemkinFromCorrs(OpenInputFile& corrsdata,
					      SearchableObjectList<String,ChemkinBaseTableObject>& table)
     {
     String line;
     line.ReadFullLine(corrsdata.Stream);
     while(line.size() > 1)
	  {
	  String chemkinname;
	  String standardname;
	  
	  line.EliminateLeadingBlanks();
	  line.IsolateNextWord(chemkinname,' ');
	  chemkinname.EliminateBlanks();
	  line.EliminateLeadingBlanks();
	  line.IsolateNextWord(standardname,' ');
	  standardname.EliminateBlanks();
	       
	  if(chemkinname.size() > 1 && standardname.size() > 1)
	       {
	       InsertChemkinIntoMolecule(chemkinname,standardname,table);
	       }
	  
	  line.ReadFullLine(corrsdata.Stream);
	  }
     }
/*F ReadInThermo(fileroot,inputform)  . . . . . . Read and Store Chemkin data
**
**  DESCRIPTION
**    fileroot: The root name of the files
**    inputform: Read file in CHEMKIN format (CHEMKIN_INPUT) or in
**               Benson format (BENSON_intPUT).
**
**    Two files are expected:
**    - fileroot.thm: The CHEMKIN thermo data (standard 4 line format)
**    - fileroot.crs: The CHEMKIN and standard name correspondences.
**
**    The chemkin table values are read in (ReadInChemkinTable)
**    and then the correspondences.  The correspondences determine
**    which molecules of the thermo table are to be used.  The correspondence
**    pairs are read until the end of the file.
**
**    The correspondence pairs consist of the CHEMKIN name, a blank and
**    then the standard name.
**
**  REMARKS
**
*/
void ChemkinSystemBase::ReadInThermo(const String& fileroot,
				     const int inputform)
     {
     OpenInputFile chemdata(fileroot,CHEMKIN_THERMO_DATA_EXT);
     OpenInputFile corrsdata(fileroot,CHEMKIN_NAME_CORRS_EXT);
     if(chemdata.Stream && corrsdata.Stream)
	  {
	  SearchableObjectList<String,ChemkinBaseTableObject> table;
	  if(inputform == CHEMKIN_INPUT)
	       ReadInChemkinTable(chemdata.Stream,table);
	  else
	       ReadInAsBensonTable(chemdata.Stream,table);
	  
	  StoreChemkinFromCorrs(corrsdata,table);
	  }
     else
	  {
	  cout << "\nCommand Aborted\n";
	  }
     }
/*S Commands
*/ 
/*F SetUpLinePropsForMolecule(g)  . . . . . . . . . . . . . . . .  line props
**
**  DESCRIPTION
**    g: The Global information
**
**    A file is read for thermodynamic properties of molecules
**    The input expected is:
**    - The filename
**
**    Each molecule is modified by adding the property to its
**    Property list
**
**  REMARKS
**
*/
int SetUpLinePropsForChemkin(ReactionSystemBase *g)
     {
     ChemkinSystemBase *global = (ChemkinSystemBase *) g;
     String filename;
     int ret=0;
     
     if(global->Inputs.size() < 1)
	  {
	  cout << "Expecting one argument:\n";
	  cout << "     Filename\n";
	  ret = 1;
	  }
     else
	  {
	  String filename = global->Inputs.front();
	  global->Inputs.pop_front();
	  
	  cout << "Reading from " << filename;
	  cout << "\n";
	  
	  ret = global->ReadInMoleculeLineProperties(filename);
	  }
     return ret;
     }
/*F ReadInChemkinThermoInfo(g)  . . . . . . . . . . . . . . . . .  line props
**
**  DESCRIPTION
**    g: The global information
**
**    The CHEMKIN thermodynamic information is read in.
**    The input expected is:
**    - The RootName of the input files
**    - The form of the input:
**      - Benson 
**      - Chemkin
**
**    The files expected by this routine are:
**    - RootName.thm having the list of thermodynamic information
**    - RootName.crs having the list of standard names with those in
**      the thermodynamic info.  This list also determines which should
**      be stored
**
**  REMARKS
**
*/
int ReadInChemkinThermoInfo(ReactionSystemBase *g)
     {
     ChemkinSystemBase *global = (ChemkinSystemBase *) g;
     int ret=0;
     
     if(global->Inputs.size() < 1)
	  {
	  cout << "Expecting two arguments:\n";
	  cout << "     Rootname - Root name of input files\n";
          cout << "     Form     - Benson or Chemkin (input form of data)\n";
          cout << "\n";
	  cout << "        Expects: Rootname.thm - The CHEMKIN data\n";
	  cout << "                 Rootname.crs - The correspondences between standard and CHEMKIN names\n";
	  ret = 1;
	  }
     else
	  {
	  String rootname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String form = global->Inputs.front();
	  global->Inputs.pop_front();
	  
	  cout << "Rootname: " << rootname;
	  cout << "\n";
	  
	  ret = 0;
	  if(form == "Chemkin")
	       {
	       cout << "Reading CHEMKIN format\n";
	       global->ReadInChemkinThermo(rootname);
	       }
	  else if(form == "Benson")
	       {
	       cout << "Reading Benson format\n";
	       global->ReadInAsBensonThermo(rootname);
	       }
	  else
	       {
	       cout << "Unknown Format: Nothing Read\n";
	       ret = 1;
	       }
	  }
     return ret;
     }
/*F out = Run() . . . . . . . . . . . . . . . . . . . .  Run chemkin commands
**
**  DESCRIPTION
**    out:  if successful
**
**    The commands are executed
**
**  REMARKS
**
*/
int ChemkinSystemBase::Run()
     {
     cout << "\n==================================\n";
     return Commands.ExecuteCommand(0,0,this);
     }
/*F InitializeChemkinCommands() . . . . . . . . . . . . . initialize commands
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void ChemkinSystemBase::InitializeChemkinCommands()
     {
     SingleSystemCommand setup("LineProps",
			      "Read in Line Properties for molecule",
			      &SetUpLinePropsForChemkin);
     Commands.AddObject("LineProps",setup);
     SingleSystemCommand chemkin("Chemkin",
				 "Read in CHEMKIN molecule data",
				 &ReadInChemkinThermoInfo);
     Commands.AddObject("Chemkin",chemkin);
     SingleSystemCommand graphout("GraphOut",
				 "Print out Thermo molecule data",
				 &GraphThermodynamicData);
     Commands.AddObject("GraphOut",graphout);
     }
/*F Initialize()  . . . . . . . . . . . . . . . . .  initialize chemkin stuff
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void ChemkinSystemBase::Initialize()
{
  InitializeChemkinCommands();
  InitializePropertyFunctions();
  InitializePropertyDecodeFunctions();
  InitializeThermPropertyDecodeFunctions();
  InitializeONamePropertyDecodeFunctions();
}
/*S Auxilliary
*/
/*C CalculateBensonFromSeries . . . . . . . . . . . . . . .  operator()(temp)
**
**  DESCRIPTION
**     Given the power series (CpSeries) the operator() 
**     calculates a specific heat capacity for a given temperature.
**
**  REMARKS
**
*/
class CalculateBensonFromSeries
     {
     CpPowerSeries Series;
     
 public:
    CalculateBensonFromSeries(const CpPowerSeries& series)
	 : Series(series)
	      {
	      }
	      
     double operator()(const double temperature)
	  {
	  ObjectList<double>::iterator c;
	  double tfactor = Series.UnitsFactor;
	  double tnorm = temperature * Series.TempNorm;
	  double cp = 0.0;
	  for(c=Series.Coefficients.begin();
	      c != Series.Coefficients.end();
	      c++)
	       {
	       cp += (*c)*tfactor;
	       tfactor *= tnorm;
	       }
	  return cp;
	  }
     };
/*F FillInBenson(benson,enthapy,entropy,series,temperatures)  . . . . fill in
**
**  DESCRIPTION
**    benson: The class to fill in
**    enthalpy: The standard enthalpy
**    entropy: The standard entropy
**    series: The Cp power series in Temperature
**    temperatures: The temperatures to calculate benson Cps
**
**    The enthalpy and entropy are filled in and the set of 
**    heat capacity values at the given temperatures are calculated
**    and filled in.
**
**  REMARKS
**
*/
void FillInBenson(BensonBaseTableObject& benson,
		  const double enthalpy, 
		  const double entropy,
		  const CpPowerSeries& series,
		  ObjectList<double>& temperatures)
     {
     CalculateBensonFromSeries calc(series);
     copy(temperatures.begin(),temperatures.end(),
	  back_insert_iterator< vector<double> >(benson.Temps));
     

     ObjectList<double>::iterator tp;
     for(tp=temperatures.begin();
	 tp != temperatures.end();
	 tp++)
	  {
	  double temp = calc.operator()(*tp);
	  benson.CpS.push_back(temp);
	  }
     
/*     
     transform(temperatures.begin(),temperatures.end(),
	       benson.CpS.begin(),
	       calc);
*/
     benson.StandardEntropy = entropy;
     benson.StandardEnthalpy = enthalpy;
     }
     

/*S Properties
*/ 
/*F ans = EntropyFromLine(plist,line) . . . . . . . . . . . . Read in Entropy
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The rest of the line
**    ans: 0 if successful
**
**    Read in an Entropy value.
**
**    The line contains three items:
**       - The property name (stored under this name)
**       - The numeric value
**       - The units (converted to cals-K-mol):
**          - kcals-K-mol
**          - cals-K-mol
**          - kJ-K-mol
**          - J-K-mol
**    
**  REMARKS
**
*/
int EntropyFromLine(PropertyListByName* plist,
		    String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     propname.EliminateBlanks();
     
     String EntropyString;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(EntropyString,' ');

     String Aform;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Aform,' ');
     
     double temp = EntropyString.ToFloat();
     double Entropy;
     
     if(Aform == "kcals-K-mol")
	  Entropy = temp*1000.0;
     else if(Aform == "cals-K-mol")
	  Entropy = temp;
     else if(Aform == "kJ-K-mol")
	  Entropy = (temp/4.187)*1000.0;
     else if(Aform == "J-K-mol")
	  Entropy = temp/4.187;
     else
	  {
	  cout << "Unit: " << Aform << " not found, storing as cals-K-mol\n";
	  Entropy = temp;
	  }
     PropertyTypeByName<double> *prop =
	  new PropertyTypeByName<double>(propname,Entropy);
     bool result = plist->AddObject(prop,"ByName-Float");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }
/*F ans = EnthalpyFromLine(plist,line)  . . . . . . . . . .  Read in Enthalpy
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The rest of the line
**    ans: 0 if successfull
**
**    Read in an Enthalpy value.
**
**    The line contains three items:
**       - The property name (stored under this name)
**       - The numeric value
**       - The units (converted to cals-K-mol):
**          - kcals-mol
**          - cals-mol
**          - kJ-mol
**          - J-mol
**    
**  REMARKS
**
*/
int EnthalpyFromLine(PropertyListByName* plist,
		    String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     propname.EliminateBlanks();
     
     String EnthalpyString;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(EnthalpyString,' ');

     String Aform;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Aform,' ');
     
     double temp = EnthalpyString.ToFloat();
     double Enthalpy;
     
     if(Aform == "kcals-mol")
	  Enthalpy = temp*1000.0;
     else if(Aform == "cals-mol")
	  Enthalpy = temp;
     else if(Aform == "kJ-mol")
	  Enthalpy = (temp/4.187)*1000.0;
     else if(Aform == "J-mol")
	  Enthalpy = temp/4.187;
     else
	  {
	  cout << "Unit: " << Aform << " not found, storing in cals-mol units\n";
	  Enthalpy = temp;
	  }
     PropertyTypeByName<double> *prop =
	  new PropertyTypeByName<double>(propname,Enthalpy);
     bool result = plist->AddObject(prop,"ByName-Float");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }

 
/*F ans = ReadCpPowerSeries(plist,line) Read in Cp as Temperature power series
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The line to be interpreted
**    ans: 0 if successful
**
**    - Power Series Name (as it is to stored)
**    - Normalization of Temperature (usually either 1.0 or .001)
**    - Units of the Heat Capacity
**      - kcals-K-mol
**      - cals-K-mol
**      - kJ-K-mol
**      - J-K-mol
**    - Source: a string (with no blanks) of the values
**    - The set of coefficient values
**
**  REMARKS
**
*/
int ReadCpPowerSeries(PropertyListByName* plist,
		      String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     propname.EliminateBlanks();
     
     String Normalization,Units,Source;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Normalization,' ');
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Units,' ');
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Source,' ');
     line.EliminateLeadingBlanks();
     String endofline = "\n";
     Source.AppendToEnd(endofline);
     
     double TempNorm = Normalization.ToFloat();
     double factor;
     
     if(Units == "kcals-K-mol")
	  factor = 1000.0;
     else if(Units == "cals-K-mol")
	  factor = 1.0;
     else if(Units == "kJ-K-mol")
	  factor = (1.0/4.187)*1000.0;
     else if(Units == "J-K-mol")
	  factor = 1.0/4.187;
     else
	  {
	  cout << "Unit: " << Units << " not found, storing as cals-K-mol\n";
	  factor = 1.0;
	  }
     CpPowerSeries Coefficients(Source,TempNorm,factor);
    
     String Coeff;
     while(*(line.chars()) != '\000')
	  {
	  line.IsolateNextWord(Coeff,' ');
	  line.EliminateLeadingBlanks();
	  double coeff = Coeff.ToFloat();
	  Coefficients.Coefficients.AddObject(coeff);
	  }
     PropertyTypeByName< CpPowerSeries > *prop
	  = new PropertyTypeByName< CpPowerSeries >(propname,Coefficients);
     bool result = plist->AddObject(prop,"CpPowerSeries");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }
/*F ans = EquilibriumConstantsFromLine(plist,line)  . .  Equilibrium as ATexp
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The line to be interpreted
**    ans: 0 if successful
**
**    The reaction constants on a line have the following elements (these
**    are the quantities after it is recognized that it is a reaction constant):
**    - The specific name of the constants
**    - Arrhenius Constant
**    - Temperature Coefficient
**    - Activation Energy
**    - The form of the Arrhenius constant (logA, A)
**    - Source
**
**  REMARKS
**
*/
int EquilibriumConstantsFromLine(PropertyListByName* plist,
				 String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     propname.EliminateBlanks();
     
     String Aconstant,Tcoeff,Energy;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Aconstant,' ');
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Tcoeff,' ');
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Energy,' ');
     
     String Aform;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(Aform,' ');
     
     double A = Aconstant.ToFloat();
     double n = Tcoeff.ToFloat();
     double E = Energy.ToFloat();
     
     if(Aform == "logA")
	  A = pow(10.0,A);

     line.EliminateLeadingBlanks();
     ReactionConstants<double> rc(A,E,n,line);
     PropertyTypeByName<ReactionConstants<double> > *prop 
	  = new PropertyTypeByName<ReactionConstants<double> >(propname,rc);
     bool result = plist->AddObject(prop,"ReactionConstants");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }

 
/*F ans = CalculateSFromEqAndH(plist,line)  . . . . . . . . Calculate Entropy
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The line to be interpreted
**    ans: 0 if successful
**
**    This routine calculates the Entropy from a given Enthalpy and
**    the equilibrium.  
**
**    'Equilibrium = A*T^n*exp(E/T)'
**
**    'DeltaS = R * log(Equilibrium) + DeltaH/T'
**
**    The input expected is:
**    - The property name
**    - The temperature of the equilibrium
**    - The heat of formation name
**    - The equilibrium name
**
**  REMARKS
**
*/
int CalculateSFromEqAndH(PropertyListByName* plist,
			       String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     propname.EliminateBlanks();

     String EnthalpyName,EquilName,Temp;
     line.IsolateNextWord(Temp,' ');
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(EnthalpyName,' ');
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(EquilName,' ');
     line.EliminateLeadingBlanks();

     double temperature = Temp.ToFloat();
     
     PropertyTypeByName<double> *enthalpyprop
	  = (PropertyTypeByName<double> *) 
	       plist->GetPointerToProperty(EnthalpyName);
     PropertyTypeByName<ReactionConstants<double> > *equilibriumprop
	  = (PropertyTypeByName<ReactionConstants<double> > *)
	       plist->GetPointerToProperty(EquilName);
     
     double enthalpy = enthalpyprop->Object;
     ReactionConstants<double> equilibrium = equilibriumprop->Object;
     
     double e = -((double) equilibrium.ActivationEnergy)/temperature;
     
     double equilattemp = 
	  equilibrium.Arrhenius * 
	       pow((double) temperature,(double) equilibrium.TemperatureCoefficient) *
		    exp(e);
     double entropy = 
	  (1.98717)*log((double) equilattemp) + 
	       enthalpy / temperature;
     PropertyTypeByName<double> *prop 
	  = new PropertyTypeByName<double>(propname,entropy);
     bool result = plist->AddObject(prop,"ByName-Float");
     
     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }
 
/*F ans = BensonFromCpHS(plist,line)  . . . . . . . . . . . .  Compute benson
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The line to be interpreted
**    ans: 0 if successful
**
**    The BensonTableObject is calculated from the Cp power series,
**    the heat of formation and the entropy of formation.
**
**    The input is:
**    - The name of the property
**    - The heat of formation name
**    - The entropy name
**    - The Cp power series name
**    - The list of temperatures for Cp.  The are two possibilities:
**      - Standard, where 3900,400,500,600,800,1000,1500 are used
**      - A list of temperatures
**    
**  REMARKS
**
*/
int BensonFromCpHS(PropertyListByName* plist,
		   String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     line.EliminateLeadingBlanks();
     
     String EnthalpyName, EntropyName, CpName;
     line.IsolateNextWord(EnthalpyName,' ');
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(EntropyName,' ');
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(CpName,' ');
     line.EliminateLeadingBlanks();

     PropertyTypeByName<double> *enthalpyprop
	  = (PropertyTypeByName<double> *) 
	       plist->GetPointerToProperty(EnthalpyName);
     PropertyTypeByName<double> *entropyprop
	  = (PropertyTypeByName<double> *) 
	       plist->GetPointerToProperty(EntropyName);
     PropertyTypeByName< CpPowerSeries > *seriesprop
	  = (PropertyTypeByName< CpPowerSeries > *) 
	       plist->GetPointerToProperty(CpName);

     String Temp;
     line.IsolateNextWord(Temp,' ');
     line.EliminateLeadingBlanks();
     
     ObjectList<double> Temperatures;
     if(Temp == "Standard")
	  {
	  Temperatures.AddObject(300);
	  Temperatures.AddObject(400);
	  Temperatures.AddObject(500);
	  Temperatures.AddObject(600);
	  Temperatures.AddObject(800);
	  Temperatures.AddObject(1000);
	  Temperatures.AddObject(1500);
	  }
     else
	  {
	  double temp = Temp.ToFloat();
	  Temperatures.AddObject(temp);
	  while(line[0] != '\0')
	       {
	       line.IsolateNextWord(Temp,' ');
	       line.EliminateLeadingBlanks();
	       temp = Temp.ToFloat();
	       Temperatures.AddObject(temp);
	       }
	  }

     
     BensonBaseTableObject benson;
     benson.BensonFormulaDescriptor = propname;
     
     FillInBenson(benson,
		  enthalpyprop->Object,
		  entropyprop->Object,
		  seriesprop->Object,
		  Temperatures);
     
     PropertyTypeByName<BensonBaseTableObject> *prop 
	  = new PropertyTypeByName<BensonBaseTableObject>("Benson",benson);
     bool result = plist->AddObject(prop,"BensonBaseTableObject");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }
 
/*F ans = ChemkinFromBenson(plist,line) . . . . Calculate Chemkin from Benson
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The line to be interpreted
**    ans: 0 if successful
**
**    This routine reads in the BensonTableObject from the properties,
**    then converts it (without the Atom counts) to the ChemkinTableForm
**    and writes it to the property list.  The line expects:
**    - The name under which to store the CHEMKIN value
**    - The name of the benson constants
**
**  REMARKS
**
*/
int ChemkinFromBenson(PropertyListByName* plist,
			       String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     propname.EliminateBlanks();

     String Benson;
     line.IsolateNextWord(Benson,' ');
     line.EliminateLeadingBlanks();
     
     PropertyTypeByName<BensonBaseTableObject> *bensonprop
	  = (PropertyTypeByName<BensonBaseTableObject> *) 
	       plist->GetPointerToProperty(Benson);
     
     ChemkinBaseTableObject chemkin(bensonprop->Object);
     
     PropertyTypeByName<ChemkinBaseTableObject> *prop 
	  = new PropertyTypeByName<ChemkinBaseTableObject>(propname,chemkin);
     bool result = plist->AddObject(prop,"ChemkinThermo");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }
/*F ans = BensonFromChemkin(plist,line) . . . . Calculate Benson from Chemkin
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The line to be interpreted
**    ans: 0 if successful
**
**    This routine reads in the BensonTableObject from the properties,
**    then converts it (without the Atom counts) to the ChemkinTableForm
**    and writes it to the property list.  The line expects:
**    - The name under which to store the Benson value
**    - The name of the CHEMKIN constants
**
**  REMARKS
**
*/
int BensonFromChemkin(PropertyListByName* plist,
			       String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     propname.EliminateBlanks();

     String Chemkin;
     line.IsolateNextWord(Chemkin,' ');
     line.EliminateLeadingBlanks();
     
     PropertyTypeByName<ChemkinBaseTableObject> *chemkinprop
	  = (PropertyTypeByName<ChemkinBaseTableObject> *) 
	       plist->GetPointerToProperty(Chemkin);
     
     BensonBaseTableObject benson(chemkinprop->Object);
     
     PropertyTypeByName<BensonBaseTableObject> *prop 
	  = new PropertyTypeByName<BensonBaseTableObject>(propname,benson);
     bool result = plist->AddObject(prop,"BensonBaseTableObject");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }
/*F ans = EquilibriumInArrheniusForm(plist,line)  . . Calculate K in exp form
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The line to be interpreted
**    ans: 0 if successful
**
**    This reads in the thermodynamic information to be able to calculate
**    the enthalpy and entropy and then calculates the Equilibrium in 
**    Arrhenius form (for use in calculating the equilibrium of a reaction, 
**    which in turn is used to calculate the reverse reaction constants).
**    The input expected is as follows:
**    - The name under which to store the K expression
**    - Thermo type (Benson or Chemkin)
**    - The Thermo object
**
**
**  REMARKS
**
*/
int EquilibriumInArrheniusForm(PropertyListByName* plist,
			       String line)
     {
     String propname;
     line.EliminateLeadingBlanks();
     line.IsolateNextWord(propname,' ');
     propname.EliminateBlanks();

     String ThermoType;
     line.IsolateNextWord(ThermoType,' ');
     line.EliminateLeadingBlanks();
     
     String ThermoName;
     line.IsolateNextWord(ThermoName,' ');
     line.EliminateLeadingBlanks();
     
     bool result;
     
     if(ThermoType == "Chemkin")
	  {
	  PropertyTypeByName<ChemkinBaseTableObject> *chemkinprop
	       = (PropertyTypeByName<ChemkinBaseTableObject> *) 
		    plist->GetPointerToProperty(ThermoName);
	  
	  ReactionConstants<double> equil0 = 
	       CalculateExpEquilibrium(propname,chemkinprop->Object);
	  StandardRateConstants equil(equil0.Arrhenius,
				      equil0.ActivationEnergy,
				      equil0.TemperatureCoefficient,
				      equil0.Documentation);
	  
	  PropertyTypeByName<StandardRateConstants> *prop 
	       = new PropertyTypeByName<StandardRateConstants>(propname,equil);
	  result = plist->AddObject(prop,"ReactionConstants");
	  }
     else if(ThermoType == "Benson")
	  {
	  PropertyTypeByName<BensonBaseTableObject> *bensonprop
	       = (PropertyTypeByName<BensonBaseTableObject> *) 
		    plist->GetPointerToProperty(ThermoName);
	  ReactionConstants<double> equil0 = 
	       CalculateExpEquilibrium(propname,bensonprop->Object);
	  StandardRateConstants equil(equil0.Arrhenius,
				      equil0.ActivationEnergy,
				      equil0.TemperatureCoefficient,
				      equil0.Documentation);
	  PropertyTypeByName<StandardRateConstants> *prop 
	       = new PropertyTypeByName<StandardRateConstants>(propname,equil);
	  result = plist->AddObject(prop,"ReactionConstants");
	  }
     else
	  {
	  cout << ThermoType << " not a proper thermo type\n";
	  result = false;
	  }
     
     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }


/*F ans = ThermoFromChemkin(plist,line) . . . . Calculate Benson from Chemkin
**
**  DESCRIPTION
**    plist: The list of properties
**    line: The line to be interpreted
**    ans: 0 if successful
**
**    This routine reads in the ChemkinTableObject from the properties,
**    then uses it to calculate the entropy, ethalpy and equilbrium 
**    constants at the given temperature. It then writes them to the 
**    property list.  The line expects:
**    - The Chemkin data
**    - The Temperature at which to calculate
**    - The Entropy Name (for example StandardEnthalpy)
**    - The Entropy Name (for example StandardEntropy)
**    - The Equilibrium Name (for example Equilibrium)
**
**  REMARKS
**
*/
int ThermoFromChemkin(PropertyListByName* plist,
			       String line)
     {
     String Chemkin;
     line.IsolateNextWord(Chemkin,' ');
     line.EliminateLeadingBlanks();
     
     String Temp;
     line.IsolateNextWord(Temp,' ');
     line.EliminateLeadingBlanks();     
     double temperature = Temp.ToFloat();

     String enthalpy;
     line.IsolateNextWord(enthalpy,' ');
     line.EliminateLeadingBlanks();
     
     String entropy;
     line.IsolateNextWord(entropy,' ');
     line.EliminateLeadingBlanks();
     
     String equilibrium;
     line.IsolateNextWord(equilibrium,' ');
     line.EliminateLeadingBlanks();
     
     
     PropertyTypeByName<ChemkinBaseTableObject> *chemkinprop
	  = (PropertyTypeByName<ChemkinBaseTableObject> *) 
	       plist->GetPointerToProperty(Chemkin);

     double hvalue = CalculateEnthalpy(chemkinprop->Object,temperature);
     double svalue = CalculateEntropy(chemkinprop->Object,temperature);
     double gvalue = hvalue - svalue*temperature;
     double levalue = gvalue/((1.98717)*temperature);
     double evalue = exp(levalue);
     
     PropertyTypeByName<double> *prop1 
	  = new PropertyTypeByName<double>(enthalpy,hvalue);
     bool result = plist->AddObject(prop1,"ByName-Float");

     PropertyTypeByName<double> *prop2
	  = new PropertyTypeByName<double>(entropy,svalue);
     result = result && plist->AddObject(prop2,"ByName-Float");

     PropertyTypeByName<double> *prop3 
	  = new PropertyTypeByName<double>(equilibrium,evalue);
     result = result && plist->AddObject(prop3,"ByName-Float");

     int ret;     
     if(result)
	  ret = 0;
     else
	  ret = 1;
     return ret;
     }
/*F ans = ReadInMoleculeLineProperties(filename)  . . . . . read in molecules
**
**  DESCRIPTION
**    filename: The name of the file to read the molecule data
**    ans: 0 if successful
**
**  REMARKS
**
*/
int ChemkinSystemBase::ReadInMoleculeLineProperties(const String filename)
     {
     OpenInputFile in(filename);
     int ret = 0;
     
     String line;
     line.ReadFullLine(in.Stream);
     while(line.size() > 1)
	  {
	  String molname;
	  line.IsolateNextWord(molname,' ');
	  molname.EliminateBlanks();
	  
	  int id = DBMolecules.DBMolecule.Names[molname];
	  if(id != 0)
	       {
	       SimpleMolecule *molecule 
		    = (SimpleMolecule *) DBMolecules.DBMolecule.ReadDBObject(id);
	       cout << "Add properties to: ";
	       cout << molecule->NameTag << " -->";
	       
	       line = ReadChemkinProperties(in.Stream,*molecule,PropertyFunctions);
	       bool ans = DBMolecules.DBMolecule.WriteDBObject(*molecule);
	       SimpleMolecule *newmolecule 
		    = (SimpleMolecule *) DBMolecules.DBMolecule.ReadDBObject(id);
	       if(ans)
		    {
		    cout << "Updated\n";
		    cout << *newmolecule;
		    }
	       else
		    cout << "Error, not Updated\n";
	       
	       delete molecule;
	       }
	  else
	       {
	       cout << "Molecule: ";
	       cout << molname;
	       cout << " not found\n";
	       ret = 1;
	       }
	  }
     return ret;
     }
/*F next = ReadChemkinProperties(in,molecule,props) determines type and reads
**
**  DESCRIPTION
**    in: The input stream
**    molecule: The molecule itself
**    props: The list of possible properties
**    next: The next line (that was not a dash)
**
**    This determines whether the current line has a property on it
**    (detection of a dash ('-')).  If so, then the MoleculeConstantsFromLine
**    routine is called and the next line is read and returned.  If the
**    line is not a property line, then the line is returned.
**
**  REMARKS
**
*/
String ReadChemkinProperties(istream& in, 
			      SimpleMolecule& molecule,
			      StringPropertyFunctions& props)
     {
     String line;
     line.ReadFullLine(in);
     line.EliminateLeadingBlanks();
     while(line.front() == '-')
	  {
	  String dummy;
	  line.IsolateNextWord(dummy,' ');
	  props.FillInProperty(&(molecule.Properties),line);
	  line.ReadFullLine(in);
	  line.EliminateLeadingBlanks();
	  }
     return line;
     }
/*F InitializePropertyFunctions() . . . .  initialize property read functions
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void ChemkinSystemBase::InitializePropertyFunctions()
     {
     StringPropertyFunction entropy("Entropy",
			     "Read in Entropy Value",
			     &EntropyFromLine);
     PropertyFunctions.AddObject("Entropy",entropy);
     StringPropertyFunction enthalpy("Enthalpy",
				     "Read in Enthalpy Value",
				     &EnthalpyFromLine);
     PropertyFunctions.AddObject("Enthalpy",enthalpy);
     StringPropertyFunction cppower("CpPowerSeries",
			     "Read in Cp as Power Series in Temperature",
			     &ReadCpPowerSeries);
     PropertyFunctions.AddObject("CpPowerSeries",cppower);
     StringPropertyFunction equilibrium("Equilibrium",
			     "Read in Equilibrium as A*T^n*exp(E/T)",
			     &EquilibriumConstantsFromLine);
     PropertyFunctions.AddObject("Equilibrium",equilibrium);
     StringPropertyFunction calcs("CalculateS",
			     "Calculate S from H and Equilibrium",
			     &CalculateSFromEqAndH);
     PropertyFunctions.AddObject("CalculateS",calcs);
     StringPropertyFunction calcbenson("CalculateBenson",
				       "Calculate Benson form with H, S and Cp power series",
				       &BensonFromCpHS);
     PropertyFunctions.AddObject("CalculateBenson",calcbenson);
     StringPropertyFunction bfromc("BensonFromChemkin",
				       "Calculate Benson Thermo form from Chemkin form",
				       &BensonFromChemkin);
     PropertyFunctions.AddObject("BensonFromChemkin",bfromc);
     StringPropertyFunction cfromb("ChemkinFromBenson",
				       "Calculate Chemkin Thermo form from Benson",
				       &ChemkinFromBenson);
     PropertyFunctions.AddObject("ChemkinFromBenson",cfromb);
     StringPropertyFunction thermfromchemkin("ThermoFromChemkin",
				       "Calculate",
				       &ThermoFromChemkin);
     PropertyFunctions.AddObject("ThermoFromChemkin",thermfromchemkin);
     StringPropertyFunction expequil("ExpEquilibrium",
				       "Calculate Equilibrium as exponetial function",
				       &EquilibriumInArrheniusForm);
     PropertyFunctions.AddObject("ExpEquilibrium",expequil);
     }
 
/*F InitializePropertyDecodeFunctions() . . . . . . . . initialize properties
**
**  DESCRIPTION
**    Initialize
**
**  REMARKS
**
*/
void ChemkinSystemBase::InitializePropertyDecodeFunctions()
     {
     }

