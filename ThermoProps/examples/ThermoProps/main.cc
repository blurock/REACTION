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
#include "CoreDataObjects.hh"
#include "NumericObjects.hh"
#include "Vector.hh"
#include "NumericObjects.hh"
#include "LogicalObjects.hh"
#include "DataObjects.hh"
#include "FunctionReal1DObjects.hh"
#include "OperationObjects.hh"
#include "LogicalOperations.hh"
#include "DataObjects.hh"
#include "InstanceObjects.hh"
#include "MenuObjects.hh"
#include "DirectedTreeObjects.hh"
#include "SelectObjects.hh"
#include "AlgorithmObjects.hh"
#include "Dbase.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"

#include "MatrixNumeric.hh"
#include "MatrixUtilities.hh"
#include "PrimitiveStats.hh"
#include "ThermoTables.hh"
#include "ThermoProps.hh"
#include "Utilities.hh"

#include "Dbase.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"

//int ConvertToChemkin(ReactionSystemBase* sys); 
int ChemkinToChemkin(ReactionSystemBase* sys);
/*C
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class ThermoTest : public MoleculeSystemBase
{
 public:
  ThermoTest(int argc, char *argv[])
    : MoleculeSystemBase(argc,argv)
    {
    }
  void EncodeDecodeObjectsSetUp()
    {
      MoleculeSystemBase::EncodeDecodeObjectsSetUp();
      InitialSetOfThermoPropsDecodeFunctions();
      InitialRxnUtilitiesDecodeFunctions();
    }
  void StandardObjectsSetUp()
    {
      MoleculeSystemBase::StandardObjectsSetUp();
      AddThermPropClasses(getStandard());
      AddRxnUtilitiesClasses(getStandard());
    }
  void Initialization()
    {
      MoleculeSystemBase::Initialization();
    }
  void CommandSetUp()
    {
      MoleculeSystemBase::CommandSetUp();

      String thermoS("ChemkinToChemkin");
      SingleSystemCommand thermo(thermoS,
			     "ChemkinToChemkin",
			     &ChemkinToChemkin);
      Commands.AddObject(thermoS,thermo);

      /*
      SingleSystemCommand thermo("ConvertToChemkin",
				  "Convert to CHEMKIN thermodynamic constants",
				  &ConvertToChemkin);
      Commands.AddObject(thermo.getName(),thermo);
      */
    }
};
/*F main  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int main(int argc, char *argv[])
{
  ThermoTest test(argc,argv);
  
  test.Initialization();
  test.Run();
  test.Exit();

}
/*F ans = ConvertToChemkin(sys)
**
**  DESCRIPTION
**    sys:
**  REMARKS
**
*/
int ChemkinToChemkin(ReactionSystemBase* sys) {
  ThermoTest *rxnsystem = (ThermoTest *) sys;
  int result = 1;
  
  if(rxnsystem->Inputs.size() != 4)
    {
      cerr << "Inputs:   Thermdat file " << endl;
      cerr << "         :       The keywords of instance names" << endl;
      cerr << "         Chemkin:         The CHEMKIN class" << endl;
      cerr << "         Thermo Variable: The name of the thermodynamic property" << endl;
      cerr << "         Conversions:     CpUnits, EnthalpyUnits, EntropyUnits" << endl;
      exit(1);
    }
  String thermdatfileS   = rxnsystem->GetNextInput();
  String cpunits         = rxnsystem->GetNextInput();
  String enthalpyunits   = rxnsystem->GetNextInput();
  String entropyunits    = rxnsystem->GetNextInput();
  
  BaseDataDoubleVector *temperatures = new BaseDataDoubleVector(7);
  temperatures->CurrentVector()[0] = 300;
  temperatures->CurrentVector()[1] = 500;
  temperatures->CurrentVector()[2] = 800;
  temperatures->CurrentVector()[3] = 1000;
  temperatures->CurrentVector()[4] = 1300;
  temperatures->CurrentVector()[5] = 1500;
  temperatures->CurrentVector()[6] = 1700;
  
  BaseDataKeySet *keys = new BaseDataKeySet();
  keys->AddKeyWord(cpunits);
  keys->AddKeyWord(enthalpyunits);
  keys->AddKeyWord(entropyunits);
  
  RxnChemkinThermoClass *propclass = (RxnChemkinThermoClass *) 
    rxnsystem->InstanceClasses.GetObjectClass("StandardChemkin");
  String chemkinname("Chemkin");
  bool notdone = true;
  cout << "Open up THERMDAT file" << endl;
  OpenInputFile thermdatfile("THERMDAT.txt");
  cout << "Opened up THERMDAT file" << endl;

  OpenOutputFile outputfile(thermdatfileS);
  
  while(notdone) {
    cout << "Read in a molecule" << endl;
    RxnDataChemkinThermo prop;
    notdone = prop.ReadBasicChemkinElement(thermdatfile.Stream,propclass);
    if(notdone) {
      cout << "Original:" << endl;
      prop.WriteAsNASAPolynomial(cout);
      cout << endl;
      cout << "Calculate for Temperatures" << endl;
      BaseDataDoubleMatrix *orig = prop.CalculateThermoUnderUnits(keys,propclass,temperatures);
      cout << "Values" << endl;
      orig->print(cout);
      RxnDataChemkinThermo *thermo = new RxnDataChemkinThermo(cpunits,enthalpyunits,entropyunits,&prop,propclass);
      cout << "Cp at 1000 " << thermo->CalculateHeatCapacity(propclass,1000.0) << endl;
      thermo->NameTag = chemkinname;
      cout << "CHEMKIN:" << endl;
      thermo->WriteAsNASAPolynomial(outputfile.Stream);
      cout << endl;
      cout << "Out: " << thermo->GetType() << " " << thermo->CalculateEnthalpy(propclass,1000.0);
      cout << endl;
      BaseDataDoubleMatrix *chemkin = thermo->CalculateThermoUnderUnits(keys,propclass,temperatures);
      cout << "Original:" << endl;
      orig->print(cout);
      cout << "Chemkin:" << endl;
      chemkin->print(cout);
      delete orig;
    }
  }
  outputfile.Stream.close();
  return result;
}
