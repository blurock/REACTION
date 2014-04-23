/*  FILE     Chemkin.hh
**  PACKAGE  Chemkin
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "Chemkin" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_CHEMKIN_HH
#define REACTION_CHEMKIN_HH

#define  CHEMKIN_INPUT 100
#define  BENSON_INPUT  200
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
/*C InsertChemkinIntoMolecule . . . . . . . . . . Chemkin Thermo for Molecule
**
**  DESCRIPTION
**     This is the helping routine which, given the standard and  SENKIN name
**     pair, adds the CHEMKIN thermodynamic constants to the molecule property 
**     information.  Two references are used:
**     - Tables: The table of chemkin constants referenced by the CHEMKIN name
**     - DBMolecules: The molecule database information
**
**  REMARKS
**
class InsertChemkinIntoMolecule
     {
     MoleculeSystemBase& DBMolecules;
     SearchableObjectList<String,ChemkinBaseTableObject>& Table;
     
 public:
     InsertChemkinIntoMolecule(MoleculeSystemBase& dbmolecules,
			       SearchableObjectList<String,ChemkinBaseTableObject> 
			       table);
     void operator()(const SymmetricPair<String>& npair);
     };
*/
 
/*C ChemkinSystemBase . . . . . . . . . . . . . . . . . . .  chemkin run base
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class ChemkinSystemBase : public ReactionSystemLevel1
     {
     StringPropertyFunctions PropertyFunctions;
 public:
     MoleculeSystemBase DBMolecules;
     
     ChemkinSystemBase(int argc, char *argv[])
	  : ReactionSystemLevel1(argc,argv),
	  DBMolecules(argc,argv)
	       {
	       Initialize();
	       }
     int ReadInMoleculeLineProperties(const String filename);
     void ReadInChemkinThermo(const String& fileroot);
     void ReadInAsBensonThermo(const String& fileroot);
     void ReadInThermo(const String& fileroot,
		       const int inputform);
     void StoreChemkinFromCorrs(OpenInputFile& corrsdata,
				SearchableObjectList<String,ChemkinBaseTableObject>& table);
     
     void Initialize();
     void InitializeChemkinCommands();
     void InitializePropertyFunctions();
     void InitializePropertyDecodeFunctions();
     void InsertChemkinIntoMolecule(const String& chemkin,
				    const String& standard,
				    SearchableObjectList<String,ChemkinBaseTableObject>& Table);
     
     int Run();
     };
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
String ReadChemkinProperties(istream& in, 
			     RxnDataSimpleMolecule& molecule,
			     StringPropertyFunctions& props);
int SetUpLinePropsForChemkin(ReactionSystemBase *g);
void ReadInChemkinTable(istream& in, 
			SearchableObjectList<String,ChemkinBaseTableObject>& table);
void ReadInCorrespondenceList(istream& in, 
			      ObjectList< SymmetricPair<String> >& nlist);




int GraphThermodynamicData(ReactionSystemBase *g);




/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif


