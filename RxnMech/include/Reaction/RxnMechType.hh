/*  FILE     RxnMechType.hh
**  PACKAGE  RxnMech
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "RxnMech" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_RXNMECHTYPE_HH
#define Reaction_RXNMECHTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
/*C ReactionMechanism . . . . . . . . . . . . . . . . . . base mechanism type
**
**  DESCRIPTION
**     This is the base reaction mechanism class.  A mechanism 
**     is a list of reactions of type ReactionClass.  This
**     should not be confused with just a list of reactions,
**     other mechanism related functions are included.
**     
**  REMARKS
**
*/
template <class ReactionClass>
class ReactionMechanism 
: public Identify
{
public:
  ObjectList<ReactionClass> Reactions;
  PropertyListByName Properties;

  inline ReactionMechanism();
  inline ReactionMechanism(const ReactionMechanism<ReactionClass>& mech);
     
  virtual void CopyClone(ReactionMechanism<ReactionClass> *mech);
  virtual ReactionMechanism<ReactionClass> *Clone();
  virtual ostream& print(ostream& out) const;
  virtual bool EncodeThis(CommBuffer& buffer);
  virtual bool DecodeThis(CommBuffer& buffer);
};
 
/*C DbaseMechanism  . . . . . . . . . . . . . . . . . . .  standard mechansim
**
**  DESCRIPTION
**     This is the standard mechanism used in the database. 
**     All the molecules and reactions used (referenced by ID number
**     refer to those is the database.
**
**  REMARKS
**
*/
class DbaseMechanism 
: public ReactionMechanism<Identify>
{     
public:
     
  inline DbaseMechanism();
  DbaseMechanism(const DbaseMechanism& mech)
    : ReactionMechanism<Identify>(mech)
    {
    }
	       
  friend ostream& operator<<(ostream& out, DbaseMechanism& mech);
};
 
/*C DbaseSystemMechanism  . . . . . . . . . . . .  System class for Mechanism
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class DbaseMechanismSystemBase : public ReactionSystemLevel1
{
  String MechanismDatabaseName;
  
public:
  DataBaseFromIdentify<DbaseMechanism> DBMechanisms;
     
  DbaseMechanismSystemBase(int argc, char *argv[])
    : ReactionSystemLevel1(argc,argv),
    MechanismDatabaseName("Mechanisms"),
    DBMechanisms(getDataRootDirectory(),MechanismDatabaseName,MECHANISM_NAMES)
    {
      InitializeMechanismPropDecodeFunctions();
    }
};
/*C BasicMechanismGraph . . . . . . . . . . . . . . .  The mechanism as graph
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class BasicMechanismGraph : public BasicIdentifyGraph
{
public:
  BasicMechanismGraph()
    {
    }
  BasicMechanismGraph(const BasicMechanismGraph& graph)
    : BasicIdentifyGraph(graph)
    {
    }
  ostream& print(ostream& out) const;
     
};
/*C SenkinInitialConditions . . . . . . . . . . . . set up initial conditions
**
**  DESCRIPTION
**    This class has the initial conditions of the SENKIN job.  The
**    standard output routines printout the conditions in SENKIN format.
**
**  REMARKS
**
*/
class SenkinInitialConditions
{
  double Temperature;
  double Pressure;
  double FinalTime;
  double DeltaTime;
     
  ObjectList<String> InitialSpecies;
  ObjectList<String> InitialCHEMKINSpecies;
  ObjectList<double> InitialMolFractions;
public:
  SenkinInitialConditions(){}
  virtual ~SenkinInitialConditions(){}
  SenkinInitialConditions(const SenkinInitialConditions& conditions);
     
  SenkinInitialConditions(const String temp,
			  const String pressure,
			  const String time,
			  ObjectList<String> inputs);
  virtual void Clone(SenkinInitialConditions *conditions);
  virtual SenkinInitialConditions *CopyClone();
  virtual ostream& print(ostream& out) const;
  virtual bool EncodeThis(CommBuffer& buffer);
  virtual bool DecodeThis(CommBuffer& buffer);

  bool ConvertToChemkinNames(ObjectList<SimpleMolecule>& mols);
};

class MechanismSenkinDataPoints;     
class FindSenkinMoleculeDataPoints;
class FindReactionValuesFromMol;
/*C SenkinMoleculeDataPoints  . . . . . . . . . concentrations for a molecule
**
**  DESCRIPTION
**     For a given molecule in a Senkin Run, a list of concentrations
**     are given.  The corresponding temperatures and times are found
**     in the MechanismSenkinDataPoints class.
**
**  REMARKS
**
*/
class SenkinMoleculeDataPoints
{
  void AddConcentrationValue(double f)
    {
      Concentrations.AddObject(f);
    }
     
public:
  Identify Molecule;
  ObjectList<double> Concentrations;
  SenkinMoleculeDataPoints();
  SenkinMoleculeDataPoints(const SenkinMoleculeDataPoints& data);
  SenkinMoleculeDataPoints(const Identify& molecule);

  void Clone(SenkinMoleculeDataPoints *data);
  SenkinMoleculeDataPoints *CopyClone();
  ostream& print(ostream& out) const;
  bool EncodeThis(CommBuffer& buffer);
  bool DecodeThis(CommBuffer& buffer);

  bool operator==(const SenkinMoleculeDataPoints& data) const
    {
      return Molecule == data.Molecule;
    }
  bool operator<(const SenkinMoleculeDataPoints& data) const
    {
      return Molecule < data.Molecule;
    }
     
  friend bool Encode(CommBuffer& buffer, SenkinMoleculeDataPoints& data);
  friend bool Decode(CommBuffer& buffer, SenkinMoleculeDataPoints& data);
  friend ostream& operator<<(ostream& out, const SenkinMoleculeDataPoints& data);
  friend bool operator==(const SenkinMoleculeDataPoints& data1, const SenkinMoleculeDataPoints& data2);
  friend bool operator<(const SenkinMoleculeDataPoints& data1, const SenkinMoleculeDataPoints& data2);

  friend class MechanismSenkinDataPoints;
  friend class FindSenkinMoleculeDataPoints;
  friend class FindReactionValuesFromMol;
     
  friend bool operator==(const SenkinMoleculeDataPoints& molpoints, const Identify& id);
};
 
/*C MechanismSenkinDataPoints The set of concentrations for all molecules
**
**  DESCRIPTION
**     The results of the SENKIN run are in this class for manipulation.
**     The run consists of data at a series of times.  For each time
**     there is the temperature and the concentration of the individual 
**     molecules (in SenkinMoleculeDataPoints).  
**
**  REMARKS
**
*/
class MechanismSenkinDataPoints
{
  String Mechanism;
  SenkinInitialConditions Conditions;

public:
  ObjectList<SenkinMoleculeDataPoints> MoleculePoints;
  ObjectList<double> Times;
  ObjectList<double> Temperatures;

  MechanismSenkinDataPoints();
  MechanismSenkinDataPoints(const MechanismSenkinDataPoints& points);
  MechanismSenkinDataPoints(const String& mechanism,
			    const SenkinInitialConditions& conditions,
			    const ObjectList<SimpleMolecule>& mols,
			    istream& in);

  void Clone(MechanismSenkinDataPoints *data);
  MechanismSenkinDataPoints *CopyClone();
  ostream& MechanismSenkinDataPoints::print(ostream& out) const;
  
  bool EncodeThis(CommBuffer& buffer);
  bool DecodeThis(CommBuffer& buffer);
  friend bool Encode(CommBuffer& buffer, MechanismSenkinDataPoints& data);
  friend bool Decode(CommBuffer& buffer, MechanismSenkinDataPoints& data);
     
  friend class SenkinMoleculeDataPoints;
  friend class FindReactionValuesFromMol;
};
/*C DbaseMechPaths
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class DbaseMechPaths : public ObjectList<PathsSearchNode<Identify> > 
{
public:
  DbaseMechPaths()
    {
      ChangeDelimitor("\n");
    }
  DbaseMechPaths(const DbaseMechPaths& paths)
    : ObjectList<PathsSearchNode<Identify> >(paths)
    {
    }
  DbaseMechPaths(const ObjectList<PathsSearchNode<Identify> >& paths,
		 const String& title)
    : ObjectList<PathsSearchNode<Identify> >(paths)
    {
      ChangeTitle(title);
      ChangeDelimitor("\n");
    }
};

#endif
