/*  FILE     SenkinType.hh
**  PACKAGE  Senkin
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "Senkin" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_SENKINTYPE_HH
#define Reaction_SENKINTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

/*C FindMoleculeByID  . . . . . . . . . . . . . . . . help for PrintThirdBody
**
**  DESCRIPTION
**     
**  REMARKS
**
*/
class FindMoleculeByID
     {
     unsigned int ID;
 public:
     FindMoleculeByID(unsigned int id)
	  : ID(id)
	       {
	       }
     bool operator()(SimpleMolecule& mol)
	  {
	  return mol.Identification == ID;
	  }
     };
/*C ReactionFromIdentify  . . . . . . . . . . read in reactions with Identify
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class ReactionFromIdentify
     {
     DbaseReactionSystemBase& DBReactions;
     ObjectList<DbaseReaction>& Reactions;
 public:
     ReactionFromIdentify(DbaseReactionSystemBase& dbreactions,
			  ObjectList<DbaseReaction>& reactions);
     void operator()(const Identify& id);
     };
/*C DBMechanismInfo . . . . . . . . . . . . . . . . . . set up mechanism info
**
**  DESCRIPTION
**     For a given mechanism, the reactions and molecules are read in from the 
**     database.  This would be used as a base class for classes manipulating 
**     complete mechanisms.
**
**  REMARKS
**
*/
class DBMechanismInfo
     {
     void ReadInDBReactions(const DbaseMechanism& mechanism);
     void ReadInDBMoleculesFromMechanism();
 public:
     DbaseReactionSystemBase DBReactions;
     MoleculeSystemBase DBMolecules;
     DbaseMechanismSystemBase DBMechanisms;

     ObjectList<DbaseReaction> Reactions;
     ObjectList<SimpleMolecule> Molecules;
     DbaseMechanism Mechanism;
     
     DBMechanismInfo(int argc, char *argv[])
	  : DBReactions(argc,argv),
	  DBMolecules(argc,argv),
	  DBMechanisms(argc,argv)
	       {
	       }
     void SetUp(const int mechid);
     void AddCarrierMolecules(ObjectList<String>& mollist);
     };

 
/*C PrintThirdBody  . . . . . . . . . . . . . . . . . .  print out third body
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class PrintThirdBody
     {
     ObjectList<SimpleMolecule>& Molecules;
     ostream& Out;
     
 public:
     PrintThirdBody(ObjectList<SimpleMolecule>& dbmolecules,
		    ostream& out);
     void operator()(const StandardThirdBody& thirdbody);
     };
     
/*C PrintSenkinReaction . . . . . . . . . . . . . . . . . to print a reaction
**
**  DESCRIPTION
**    This is the base class to print the Senkin input reaction
**
**  REMARKS
**
*/
class PrintSenkinReaction
     {
     ostream& out;
     ObjectList<SimpleMolecule>& Molecules;
     
 public:
     PrintSenkinReaction(ObjectList<SimpleMolecule>& molecules,
			 ostream& output);
     void PrintMolecules(ostream& out,
			 const ObjectList<Identify>& mols,
			 const bool thirdbody);
     void PrintForwardConstants(ostream& out, 
				DbaseReaction& reaction);
     bool ThirdBody(DbaseReaction& reaction);
     void operator()(DbaseReaction& reaction);
     };
/*C PrintMoleculeNames  . . . . . . . . . . . . . . prints the molecule names
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class  PrintMoleculeNames
     {
     ostream& Out;
 public:
     PrintMoleculeNames(ostream& out);
     void operator()(SimpleMolecule& mol);
     };

 
/*C SenkinConstantPressureRun
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class SenkinConstantPressureRun
     {
     String Mechanism;
     String RootName;
     
     SenkinInitialConditions InitialConditions;
     
     void StartJob();
     void CreateConditions();
     void WaitForJobToFinish(const int timeout);
     void ReadSenkinOutput(const ObjectList<SimpleMolecule>& mols);
 public:
     MechanismSenkinDataPoints MechanismResults;
     bool Okay;
     void RunAndWaitForSenkinJob(const ObjectList<SimpleMolecule>& mols,
				 const int timeout);
     SenkinConstantPressureRun(const String mechanism,
			       const String rootname,
			       const String temp,
			       const String pressure,
			       const String time,
			       ObjectList<String> inputs,
			       ObjectList<SimpleMolecule>& mols);
     
     friend int RunSenkinJob(ReactionSystemBase *g);
     };
/*C SenkinRun . . . . . . . . . . . . . . . . . . . . .  setup and run Senkin
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class SenkinRun : public DBMechanismInfo
     {
     void PrintOutSenkinReactions(ostream& out);
     void PrintOutSenkinMolecules(ostream& out);
     void PrintOutChemkinInfo(ostream& out);
     
 public:
     String Elements;

     SenkinRun(int argc, char *argv[])
	  : DBMechanismInfo(argc,argv),
	  Elements("H O C N Ar")
	       {
	       }
     SenkinRun(int argc, char *argv[],const String& elements)
	  : DBMechanismInfo(argc,argv),
	  Elements(elements)
	       {
	       }
     void PrintOutSenkinInputFiles(const String& rootname);
     };
/*C SenkinSystemBase  . . . . . . . . . . . . . . . . . . .  The command base
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class SenkinSystemBase : public ReactionSystemLevel1
     {
 public:
     SenkinRun Runit;
     
     SenkinSystemBase(int argc, char *argv[]);
     int Run();

     void Initialize();
     void InitializeSenkinCommands();
     };


#endif
