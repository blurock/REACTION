/*  FILE     SenkinJobInput.cc
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
#include "Basis/System.hh"
#include "Basis/Pairs.hh"
#include "Basis/Graph.hh"
#include "Basis/GraphCycle.hh"
#include "Basis/BasicLaTeXTable.hh"
#include "Basis/Vector.hh"
#include "Basis/MatrixNumeric.hh"
#include "Basis/MatrixUtilities.hh"
#include "Basis/MatrixOut.hh"

#include "Basis/EigenValues.hh"
#include "Basis/PCAAnalysis.hh"

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

#define SENKINTOOLS "/home/reaction/Reaction/tools/Senkin/"
#define START_SENKIN_COMMAND "/home/reaction/Reaction/tools/Senkin/scripts/SenkinJobRun"
#define DEFAULT_JOB_TIMEOUT 300
#define BENSON_PROP_TAG "Benson"
#define CHEMKIN_PROP_TAG "ChemkinThermo"

 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
template class list<SenkinMoleculeDataPoints>;
template bool Encode(CommBuffer &, ObjectList<SenkinMoleculeDataPoints> &);
template bool Decode(CommBuffer &, ObjectList<SenkinMoleculeDataPoints> &);
template bool Encode(CommBuffer &, ObjectList<String> &);
template bool Decode(CommBuffer &, ObjectList<String> &);

template bool TopDecode(CommBuffer &, PropertyTypeByName<MechanismSenkinDataPoints> *&);
void MatrixGnuPlotOut(MatrixNumeric& mat,
		      const ObjectList<String>& names,
		      const int part,
		      const int fsize,
		      const String& root,
		      const ObjectList<double>& xcoord,
		      const int skip);
void MatrixLaTexOutSelect(MatrixNumeric& mat,
			  const ObjectList<String>& names,
			  const ObjectList<double>& select,
			  const ObjectList<double>& dependencies,
			  const unsigned int fsize,
			  const unsigned int prec,
			  ostream& out);
int OutputMatrixBlock(MatrixNumeric mat, 
		      int size1, 
		      int part,
		      int fsize,
		      int varcnt,
		      ostream& out,
		      const ObjectList<double>& xcoord,
		      const int skip);
int OutputMatrixBlockTranspose(MatrixNumeric mat, 
		      int size1, 
		      int part,
		      int fsize,
		      int varcnt,
		      ostream& out,
		      const ObjectList<double>& xcoord,
		      const int skip);
 
#define RIGHT_TABLE_JUSTIFICATION    1
#define LEFT_TABLE_JUSTIFICATION     2
#define CENTER_TABLE_JUSTIFICATION   3

/*C EigenValueLaTeXTable
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class EigenValueLaTeXTable : public BasicLaTeXTable
{
  const MatrixNumeric& Eigenvectors;
  const VectorNumeric& Eigenvalues;
  const ObjectList<String>& Names;
  
  unsigned int eigensize;
  unsigned int i;
  ObjectList<String>::const_iterator name;
       
public:
  //EigenValueLaTeXTable();
  EigenValueLaTeXTable(const MatrixNumeric& eigenvectors,
		       const VectorNumeric& eigenvalues,
		       const ObjectList<String>& names,
		       const unsigned int numcols,
		       const String& caption,
		       const String& label);
  ostream& PrintTitles(ostream& out);
  ostream& PrintTableLine(ostream& out);
};
/*
EigenValueLaTeXTable::EigenValueLaTeXTable()
  : BasicLaTeXTable(5,"Table","Table")
    {
    }  
*/
/*S Clone
*/ 

/*S ANALYSIS
 */ 
/*F MatrixAnalysisOut(mat,names,part,fsize,out) . . . . . . for ANALYSIS data
**
**  DESCRIPTION
**    mat: The matrix
**    names: The names of the rows
**    part: The number of columns in a data row
**    fsize: The size of the element
**    out: The output stream
**
**  REMARKS
**
*/
void MatrixAnalysisOut(MatrixNumeric& mat,
		       const ObjectList<String>& names,
		       const int part,
		       const int fsize,
		       ostream& out,
		       bool transpose=false)
     {
       int size1,size2;
       
       if(transpose)
	 {
	   size1 = mat[0].size();
	   size2 = mat.size();
	 }
       else
	 {
	   size1 = mat.size();
	   size2 = mat[0].size();
	 }
       
     int varcnt = 0;
     
     ObjectList<String>::const_iterator name = names.begin();
     
     while(size2 > 0)
	  {
	    int part1 = part;
	  if(part1 > size2)
	       part1 = size2;
	       
	  for(int i=0; i < part1; i++)
	       {
	       out << *name;
	       out << "\n";
	       name++;
	       }
	  
	  for(int pnt=0; pnt < size1; pnt++)
	       {
	       for( int ivar=0;
		   ivar != part1;
		   ivar++)
		    {
		    out.width(fsize);
		    out.setf(ios::scientific, ios::floatfield);
		    out.precision(7);
		    if(transpose)		      
		      out << setw(fsize) << mat[varcnt + ivar][pnt];
		    else
		      out << setw(fsize) << mat[pnt][varcnt + ivar];
		    }
	       out << "\n";
	       }
	  varcnt += part1;
	  size2 -= part1;
	  }
     }
 
/*F MatrixAnalysisOutControl(mat,part,fsize,out)  . . . ANALYSIS control file
**
**  DESCRIPTION
**    mat: The matrix
**    filename: The name of the data file
**    part: How many variables in one block
**    fsize: The width of each parameter
**    out: The output stream
**
**    The size of the matrix is used to determine the blocks of data and
**    output to the stream. The form of the output is
**
**    - 0 filename
**    - num ; The number of blocks
**    - filename
**    And for each block:
**    - filename (or SAME if not the first block)
**    - nvar npoints ; The number of variables and number of points
**
**
**  REMARKS
**
*/
void MatrixAnalysisOutControl(MatrixNumeric& mat,
			      const String& filename,
			      const int part,
			      const int fsize,
			      ostream& out,
			      bool transpose=false)
     {
       int size1,size2;
       
       if(transpose)
	 {
	   size1 = mat[0].size();
	   size2 = mat.size();
	 }
       else
	 {
	   size1 = mat.size();
	   size2 = mat[0].size();
	 }
     
       int num = size2/part;
       if(num*part < size2)
	 num += 1;
     
     bool first = true;
     
     while(size2 > 0)
	  {
	    if(first)
	      out << filename;
	    else
	      out << "SAME";
	    first = false;
	    int part1 = part;
	  if(part1 > size2)
	       part1 = size2;

	  out << "\n";
	  out << part1;
	  out << "  ";
	  out << size1;
	  out << " NAME SAME\n";
	  
	  int position = 0;
	  
	  for(int i=0;i < part1;i++)
	       {
	       out << position;
	       out << "  ";
	       out << fsize;
	       out << "  F\n";
	       position += fsize;
	       
	       }
	  size2 -= part1;
	  }
     }
 
/*F OutputAnalysisDS(names, predicates,out) . . . . . . . .  ANALYSIS ds file
**
**  DESCRIPTION
**    names: The set of variable names
**    predicates: The string containing the predicate types
**    out: The output stream
**
**    This outputs the set of variable names in the form for the analysis routines
**
**  REMARKS
**
*/
void OutputAnalysisVariables(ObjectList<String>& names,
			     String& predicates,
			     ostream& out)
     {
     if(predicates.size() == 1)
	  predicates = "Predicates(>=)";
     
     for(ObjectList<String>::iterator name = names.begin();
	 name != names.end();
	 name++)
	  {
	  out << *name;
	  out << " ";
	  out << predicates;
	  out << "\n";
	  }
     }
/*S GnuPlot
 */ 
/*F MatrixGnuPlotOut(mat,names,part,fsize,root) . . . . . . for GnuPlot data
**
**  DESCRIPTION
**    mat: The matrix
**    names: The names of the rows
**    part: The number of columns in a data row
**    fsize: The size of the element
**    root: Root of the GnuPlot files
**
**  REMARKS
**
*/
void MatrixGnuPlotOut(MatrixNumeric& mat,
		      const ObjectList<String>& names,
		      const int part,
		      const int fsize,
		      const String& root,
		      const ObjectList<double>& xcoord,
		      const int skip)
     {
     int size1 = mat.size();
     int size2 = mat[0].size();
     
     int varcnt = 0;
     int filecnt = 0;
     String suffix = "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789";
     OpenOutputFile gnu(root,"plt");
     OpenOutputFile tex(root,"tex");
     
     ObjectList<String>::const_iterator name = names.begin();

     gnu.Stream << "set terminal postscript portrait\n";
     gnu.Stream << "set data style lines\n";
     unsigned int count = 0;
     while(size2 > 0)
	  {
	    String datname = root;
	    String nm(suffix.Isolate(filecnt,filecnt+1));
	    datname.AppendToEnd(nm);
	    
	    OpenOutputFile dat(datname,"dat");
	    int part1 = part;
	    if(part1 > size2)
	      part1 = size2;
	       
	    for(int i=0; i < part1; i++)
	      {
		gnu.Stream << "set output \"";
		gnu.Stream << *name;
		gnu.Stream << ".ps\"\n";
		
		gnu.Stream << "plot '";
		gnu.Stream << dat.FullName;
		gnu.Stream << "' using ";
		gnu.Stream << "1:";
		gnu.Stream << i+2;
		gnu.Stream << " title \'";
		gnu.Stream << *name;
		gnu.Stream << "'\n";

		if(count % 3 == 0)
		  tex.Stream << "\\clearpage\n";
		count++;
		
		tex.Stream << "\\begin{figure}\n";
		tex.Stream << "\\begin{center}\n";
		tex.Stream << "\\epsfxsize=10cm\n";
		tex.Stream << "\\epsfysize=7cm\n";
		tex.Stream << "\\epsffile{";
		tex.Stream << *name;
		tex.Stream << ".ps}\n";
		tex.Stream << "\\end{center}\n";
		tex.Stream << "\\caption[";
		tex.Stream << *name;
		tex.Stream << "]\n{";
		tex.Stream << *name;
		tex.Stream << "\\label{";
		tex.Stream << *name;
		tex.Stream << "}\n}\n";
		tex.Stream << "\\end{figure}\n\n";
		
		
		name++;
	      }
	    varcnt = OutputMatrixBlock(mat,size1,part,fsize,varcnt,dat.Stream,xcoord,skip);
	    filecnt++;
	    
	    size2 -= part1;
	  }
     }
/*F newcnt = OutputMatrixBlock(mat,size1,part,varcnt) . . . . . block of data
**
**  DESCRIPTION
**    mat: The matrix
**    size1: The number of data points (rows)
**    part: The number of columns in partition
**    varcnt: The current variable count
**    out: The output stream
**    newcnt: The variable count after this partition
**
**  REMARKS
**
*/
int OutputMatrixBlock(MatrixNumeric mat, 
		      int size1, 
		      int part,
		      int fsize,
		      int varcnt,
		      ostream& out,
		      const ObjectList<double>& xcoord,
		      const int skip)
{
  ObjectList<double>::const_iterator xpoint = xcoord.begin();

  for(int pnt=0; pnt < size1; pnt++)
    {
      vector<double> vec = mat[pnt];
      
      vector<double>::iterator begvar = vec.begin()+varcnt;
      vector<double>::iterator endvar = vec.begin()+varcnt+part;
      vector<double>::iterator var;
      
      
      if(xcoord.size() > 0)
	{
	  out.setf(ios::scientific, ios::floatfield);
	  out.precision(5);
	  out << setw(fsize) << *xpoint;
	  for(int i=0;i<skip;i++)
	    xpoint++;
	}
      
      for(var=begvar;
	  var != endvar;
	  var++)
	{
	  out.setf(ios::scientific, ios::floatfield);
	  out.precision(5);
	  out << setw(fsize) << *var;
	}
      out << "\n";
    }
  varcnt += part;
  return varcnt;
}
/*S Constructors
*/
/*F PrintSenkinReaction(molecules,output) . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    molecules: The set of molecules in the reactions (used as pointer)
**    output: The output stream to print to
**
**  REMARKS
**
*/
PrintSenkinReaction::PrintSenkinReaction(ObjectList<SimpleMolecule>& molecules,
					 ostream& output)
: out(output),
Molecules(molecules)
     {
     }
 
/*F PrintMoleculeNames(out) . . . . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    out: The output stream
**
**  REMARKS
**
*/
PrintMoleculeNames::PrintMoleculeNames(ostream& out)
: Out(out)
     {
     }
/*F SenkinConstantPressureRun(mechanism,rootname,temp,pressure,time,inputs,mols) .  Senkin setup
**
**  DESCRIPTION
**    mechanism: The mechanism name
**    rootname: The rootname of the CHEMKIN files
**    temp: The initial temperature
**    pressure: The initial pressure
**    time: The final time
**    inputs: The set of remaining inputs
**    mols: The set of molecules in the mechanism
**    
**  REMARKS
**
*/
SenkinConstantPressureRun::SenkinConstantPressureRun(const String mechanism,
						     const String rootname,
						     const String temp,
						     const String pressure,
						     const String time,
						     ObjectList<String> inputs,
						     ObjectList<SimpleMolecule>& mols)
: Mechanism(mechanism),
  RootName(rootname),
  InitialConditions(temp,pressure,time,inputs)
     {
     Okay = InitialConditions.ConvertToChemkinNames(mols);
     }

/*S SenkinPrint
*/ 
/*F name = ChemkinMoleculeName(mol) . . . . . . . . use CHEMKIN name if there
**
**  DESCRIPTION
**    mol: The molecule
**    name: The CHEMKIN name if there, otherwise the standard name
**
**  REMARKS
**
*/
String ChemkinMoleculeName(SimpleMolecule& mol)
     {
     String name = GetAlternativeName("CHEMKIN",mol.Properties);
     if(name.size() > 1)
	  return name;
     else
	  return mol.NameTag;
     }
/*F PrintOutSenkinInputFiles(rootname)  . . . . . . .  print out Senkin files
**
**  DESCRIPTION
**    rootname: The rootname of the input file
**
**    The SENKIN input file is produced with "inp" as the extension
**
**  REMARKS
**
*/
void SenkinRun::PrintOutSenkinInputFiles(const String& rootname)
     {
     OpenOutputFile out(rootname,"inp");
     OpenOutputFile therm(rootname,"thm");
     
     out.Stream << "ELEMENTS\n";
     out.Stream << Elements;
     out.Stream << "\n";
     PrintOutSenkinMolecules(out.Stream);
     PrintOutSenkinReactions(out.Stream);
     PrintOutChemkinInfo(therm.Stream);
     }
/*F PrintOutSenkinReactions(out)  . . . . . . . . . . . . Print out reactions
**
**  DESCRIPTION
**    out: The output stream
**
**  REMARKS
**
*/
void SenkinRun::PrintOutSenkinReactions(ostream& out)
     {
     PrintSenkinReaction prtrxn(Molecules,out);

     out << "REACTIONS\n";     
     for_each(Reactions.begin(),
	      Reactions.end(),
	      prtrxn);
     out << "END\n";
     }
 
	       
/*F PrintOutSenkinMolecules(out)  . . . . . . . . . . print species of SENKIN
**
**  DESCRIPTION
**    out: The output buffer
**
**  REMARKS
**
*/
void SenkinRun::PrintOutSenkinMolecules(ostream& out)
     {
     PrintMoleculeNames molnames(out);
     
     out << "SPECIES\n";
     for_each(Molecules.begin(),
	      Molecules.end(),
	      molnames);
     out << "END\n";
     }


 
/*F PrintMolecules(out,mols,thirdbody)  . . . . . . . . . .  Mols of one side
**
**  DESCRIPTION
**    out: The output stream
**    mols: The molecule ids
**    thirdbody: True if a 3rd body in the reaction
**
**    This prints out one side of the reaction.  If there is a third body,
**    then an "M" is added.
**
**  REMARKS
**
*/
void PrintSenkinReaction::PrintMolecules(ostream& out,
					 const ObjectList<Identify>& mols,
					 const bool thirdbody)
     {
     ObjectList<Identify>::const_iterator idens;
     ObjectList<SimpleMolecule>::iterator molecule;
     
     for(idens=mols.begin();
	 idens != mols.end();
	 idens++)
	  {
	  FindMoleculeByID findid((*idens).Identification);
     
	  molecule = find_if(Molecules.begin(),
			     Molecules.end(),
			     findid);
	  if(molecule != Molecules.end())
	       {
	       if(idens != mols.begin())
		    out << " + ";
	       out << ChemkinMoleculeName(*molecule);
	       }
	  else
	       {
	       cout << (*idens).NameTag;
	       cout << " (not found)\n";
	       }
	  }
     if(thirdbody)
	  out << " + M ";
     }
 
/*F PrintForwardConstants(out,reaction) . . . . . . .  rate constants printed
**
**  DESCRIPTION
**    out: The output stream
**    reaction: The reaction
**
**    The "Forward" rate constants of the reaction are printed.
**
**  REMARKS
**
*/
void PrintSenkinReaction::PrintForwardConstants(ostream& out, 
						DbaseReaction& reaction)
     {
     Identify iden(0,"Forward");
     PropertyTypeByName<StandardRateConstants> *prop 
	  = (PropertyTypeByName<StandardRateConstants> *) reaction.Properties.GetPointerToObject(iden);
     StandardRateConstants forward = prop->Object;
     
     out << "          ";
     out << forward.Arrhenius;
     out << " ";
     out << forward.TemperatureCoefficient;
     out << " ";
     out << forward.ActivationEnergy;
     out << "\n";
//     delete prop;
     }
 
/*F ans = ThirdBody(reaction) . . . . . . . . Is there a third body influence
**
**  DESCRIPTION
**    reaction: The reaction
**    ans: true if third body influence
**
**    The existence of a third body is found by seeing if there
**    are any third body rate constants listed
** 
**  REMARKS
**
*/
bool PrintSenkinReaction::ThirdBody(DbaseReaction& reaction)
     {
     bool ans;
     
     Identify iden(0,"ThirdBody");
     PropertyTypeByName<ListOfThirdBodyMolecules> *prop =
	  (PropertyTypeByName<ListOfThirdBodyMolecules> *)
	       reaction.Properties.GetPointerToObject(iden);
     ListOfThirdBodyMolecules thirdbody = prop->Object;
     
     if(thirdbody.size() > 0)
	  ans = true;
     else
	  ans = false;
//     delete prop;
     return ans;
     }
 
/*F operator()(reaction)  . . . . . . . . . . . . . . . . PrintSenkinReaction
**
**  DESCRIPTION
**    reaction: The reaction
**
**    The reaction is printed as Senkin input
**
**  REMARKS
**
*/
void PrintSenkinReaction::operator()(DbaseReaction& reaction)
     {
     bool thirdbody = ThirdBody(reaction);
     
     PrintMolecules(out,reaction.Reactants,thirdbody);
     out << " = ";
     PrintMolecules(out,reaction.Products,thirdbody);
     PrintForwardConstants(out,reaction);

     Identify iden(0,"ThirdBody");
     PropertyTypeByName<ListOfThirdBodyMolecules> *prop =
	  (PropertyTypeByName<ListOfThirdBodyMolecules> *)
	       reaction.Properties.GetPointerToObject(iden);
     ListOfThirdBodyMolecules thirdbodylist = prop->Object;
     PrintThirdBody p3rd(Molecules,out);
     for_each(thirdbodylist.begin(),
	      thirdbodylist.end(),
	      p3rd);
     } 
/*F operator()(mol) . . . . . . . . . . . . . . . . . . .  PrintMoleculeNames
**
**  DESCRIPTION
**    mol: The molecule
**    
**    Calls ChemkinMoleculeName to find molecule name
**
**  REMARKS
**
*/
void PrintMoleculeNames::operator()(SimpleMolecule& mol)
     {
     Out << ChemkinMoleculeName(mol);
     Out << "\n";
     } 
 
/*F PrintThirdBody(molecules,out) . . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    molecules: The database molecules
**    out: The output stream
** 
**  REMARKS
**
*/
 PrintThirdBody::PrintThirdBody(ObjectList<SimpleMolecule>& molecules,
				ostream& out)
      : Molecules(molecules),
      Out(out)
     {
     } 
/*F operator()(thirdbody) . . . . . . . . . . . . . . . . . .  PrintThirdBody
**
**  DESCRIPTION
**    thirdbody: The rate constants and molecule
**
**  REMARKS
**
*/
void PrintThirdBody::operator()(const StandardThirdBody& thirdbody)
     {
     FindMoleculeByID findid(thirdbody.ThirdBody);
     
     ObjectList<SimpleMolecule>::iterator molecule = find_if(Molecules.begin(),
							     Molecules.end(),
							     findid);
     if(molecule != Molecules.end())
	  {
	  Out << ChemkinMoleculeName(*molecule);
	  Out << "/";
	  Out << thirdbody.Arrhenius;
//	  Out << " ";
//	  Out << thirdbody.TemperatureCoefficient;
//	  Out << " ";
//	  Out << thirdbody.ActivationEnergy;
	  Out << "/";
	  Out << "\n";
	  }
     }

/*S JobRunUtility
*/
/*F ans = ConvertToChemkinNames(mols) . . . . .  input names to CHEMKIN names
**
**  DESCRIPTION
**    mols: The list of molecules
**    ans: true if all names found
**
**  REMARKS
**
*/
bool SenkinInitialConditions::ConvertToChemkinNames(ObjectList<SimpleMolecule>& mols)
     {
     bool success = true;
     
     ObjectList<SimpleMolecule>::iterator molecule;
     ObjectList<String>::iterator name;
     for(name =InitialSpecies.begin();
	 name != InitialSpecies.end();
	 name++)
	  {
	  FindMoleculeByName findit(*name);
	  molecule = find_if(mols.begin(),
			     mols.end(),
			     findit);
	  if(molecule != mols.end())
	       {
	       InitialCHEMKINSpecies.AddObject(ChemkinMoleculeName(*molecule));
	       }
	  else
	       {
	       success = false;
	       InitialCHEMKINSpecies.AddObject(*name);
	       cout << *name;
	       cout << " (not found)\n";
	       }
	  }
     return success;
     }
/*F RunAndWaitForSenkinJob(mols,timeout)  . . . . . . . . . . .  Job Sequence
**
**  DESCRIPTION
**    mols: The list of molecules (for output correspondence)
**    timeout: The timeout in seconds for waiting for the job
**
**    The Senkin Job run sequence:
**    - Write out run Conditions (CreateConditions)
**    - Start Job (StartJob)
**    - Wait For Finish (WaitForJobToFinish)
**    - Read In Results (ReadSenkinOutput)
**
**  REMARKS
**
*/
void SenkinConstantPressureRun::RunAndWaitForSenkinJob(const ObjectList<SimpleMolecule>& mols,
						       const int timeout)
     {
     if(Okay)
	  {
	  cout << "Write Out Run Conditions\n";
	  CreateConditions();
	  if(Okay)
	       {
	       cout << "Start Job";
	       StartJob();
	       if(Okay)
		    {
		    cout << "Wait For Finish\n";
		    WaitForJobToFinish(timeout);
		    if(Okay)
			 {
			 cout << "Read In Results\n";
			 ReadSenkinOutput(mols);
			 }
		    }
	       }
	  }
     else
	  {
	  cout << "\nJob Aborted\n";
	  }
     }
/*F CreateConditions()  . . . . . . . . . . . Write out SENKIN job conditions
**
**  DESCRIPTION
**    The SENKIN starting conditions are sent to a file.  If the
**    file cannot be created (opened), Okay is set to false.
**
**  REMARKS
**
*/
void SenkinConstantPressureRun::CreateConditions()
     {
     if(Okay)
	  {
	  OpenOutputFile senk(RootName,"senk");
	  if(senk.Stream)
	       InitialConditions.print(senk.Stream);
	  else
	       Okay = false;
	  }
     }
     
/*F StartJob()  . . . . . . . . . . . . . . . . . . . .  Start the SENKIN job
**
**  DESCRIPTION
**    The SENKIN job is started by issuing the system command.  If
**    Okay is false, the job will not be started.
**
**  REMARKS
**
*/
void SenkinConstantPressureRun::StartJob()
     {
     if(Okay)
	  {
	  String job = START_SENKIN_COMMAND;
	  String space = " ";
	  job.AppendToEnd(space);
	  job.AppendToEnd(RootName);
     
	  system(job.chars());
	  }
     }
 
/*F WaitForJobToFinish(timeout) . . . . . . . . . . . . . Wait for SENKIN Job
**
**  DESCRIPTION
**    timeout: The number of seconds to wait for the job
**
**    The end of the SENKIN job is signaled by the appearance of the 
**    file "RootName.done".  If the 
**
**  REMARKS
**
*/
void SenkinConstantPressureRun::WaitForJobToFinish(const int timeout)
     {
     int max = timeout/5;
     if(Okay)
	  {
	  cout << "\n";
	  bool notdone = true;
	  while(notdone)
	       {
	       OpenInputFile message(RootName,"done");
	       if(message.Stream)
		    {
		    cout << "\nJob Finished\n";
		    notdone = false;
		    }
	       else
		    {
		    system("sleep 5");
		    cout << ".";
		    flush(cout);
		    max--;
		    if(max < 0)
			 {
			 cout << "\n Job Timed Out\n";
			 notdone = false;
			 Okay = false;
			 }
		    }
	       }
	  }
     }
 
/*F ReadSenkinOutput(mols)  . . . . . . . . . . .  Read in SENKIN output data
**
**  DESCRIPTION
**    mols: The set of molecules in the mechanism
**
**  REMARKS
**
*/
void SenkinConstantPressureRun::ReadSenkinOutput(const ObjectList<SimpleMolecule>& mols)
     {
     if(Okay)
	  {
	  OpenInputFile concs(RootName,"intr.out");
	  if(concs.Stream)
	       {
	       MechanismSenkinDataPoints results(Mechanism,
						 InitialConditions,
						 mols,
						 concs.Stream);
	       MechanismResults = results;
	       }
	  else
	       Okay = false;
	  }
     }


/*S OperationsOnRates
 */
/*F LatexRxnVersusTimeMatrix(rxns,points,skip) . . rxn rate versus time matrix
**
**  DESCRIPTION
**    rxns: The list of reactions
**    points: The times, temperatures and concentrations of the SENKIN run.
**    skip: Take only the (skip)th element starting from zero
**
**  REMARKS
**
*/
void LatexRxnVersusTimeMatrix(ObjectList<DbaseReaction>& rxns,
			     ObjectList<double>& pnts,
			     const MechanismSenkinDataPoints points,
			     String& rootname,
			     bool compfi,
			     bool takelog)
{
  int length = SkipMatrixSize(points.Times.size(),1);
  MatrixNumeric mat(2*rxns.size(),length);
  FindReactionValues values(mat,points,true,points.Times.size(),1);
  
  ObjectList<String> names;
  
  CalculateValuesFromReactions(rxns,values,compfi,takelog);
  FormAndAddReactionNames(rxns,names);

  OpenOutputFile matout(rootname,"tex");
  LaTeXMatrixOutSelect latexout(rootname,rootname,mat,names,pnts,points.Times,10,2);
  latexout.printTable(matout.Stream);
  
}
/*F FormRxnVersusTimePCA(rxns,points,skip)   Form rxn rate versus time matrix
**
**  DESCRIPTION
**    rxns: The list of reactions
**    points: The times, temperatures and concentrations of the SENKIN run.
**    skip: Take only the (skip)th element starting from zero
**
**  REMARKS
**
*/
void FormRxnVersusTimePCA(ObjectList<DbaseReaction>& rxns,
			  const MechanismSenkinDataPoints points,
			  String& rootname,
			  const double initialtime,
			  const double finaltime,
			  bool compfi,
			  bool takelog)
{
  int length = SkipMatrixSize(points.Times.size(),1);
  MatrixNumeric mat(2*rxns.size(),length);
  FindReactionValues values(mat,points,true,points.Times.size(),1);
  
  ObjectList<String> names;
  
  CalculateValuesFromReactions(rxns,values,compfi,takelog);
  FormAndAddReactionNames(rxns,names,false);

  MatrixNumeric reduced = ReduceMatToTimeInterval(mat,points,initialtime,finaltime,false);

  cout << "Dimension of Reduced Matrix: " << reduced.size() << " by " << reduced[0].size() << "\n";
  cout << "The reactions:\n";
  cout << names;
  cout << "\n";
  
  String pcaroot(rootname);
  String pcaS("pca");
  pcaroot.AppendToEnd(pcaS);
  OpenOutputFile matout(pcaroot,"tex");
  PCAAnalysis pca(reduced);
  pca.EigenvaluesAsLaTeX(matout.Stream,names);
  MatrixNumeric tfm = pca.GetTransformedCoordinates();

  String gnuroot(rootname);
  String gnu("gnu");
  gnuroot.AppendToEnd(gnu);

  char * p = new char[10];
  ObjectList<String> fnames;
  for(unsigned int i=0;i<tfm[0].size();i++)
    {
      ostrstream ost(p,3);
      ost << "F" << i;
      ost << "   ";
      
      String name(p);
      String numb;
      name.IsolateNextWord(numb,BLANK);
      fnames.AddObject(numb);
    }
  
      
  MatrixGnuPlotOut(tfm,fnames,6,15,gnuroot,points.Times,1);
}
/*F FormAnalysisTimeMatrix(rxns,points) . .  Form rxn rate versus time matrix
**
**  DESCRIPTION
**    rxns: The list of reactions
**    points: The times, temperatures and concentrations of the SENKIN
**    outroot: The root name of the ANALYSIS files
**
**  REMARKS
**
*/
void FormAnalysisTimeMatrix(ObjectList<DbaseReaction>& rxns,
			    const MechanismSenkinDataPoints points,
			    const String outroot,
			    const int skip,
			    const bool normalrate,
			    const bool lograte)
     {
       int length = SkipMatrixSize(points.Times.size(),skip);
       MatrixNumeric mat(2*rxns.size(),length);
       FindReactionValues values(mat,points,true,points.Times.size(),skip);
       unsigned int part = 6;
       ObjectList<String> names;
       
       CalculateValuesFromReactions(rxns,values,normalrate,lograte);
       FormAndAddReactionNames(rxns,names);

       OpenOutputFile outctl(outroot,"ctl");
       OpenOutputFile outres(outroot,"res");
       OpenOutputFile outds(outroot,"ds");
       OpenOutputFile outdat(outroot,"dat");
       
       unsigned int num = mat.size()/part;
       if(num*part < mat.size())
	 num += 1;
       num += 1;
       
       outctl.Stream << "0" << outres.FullName << "\n";
       outctl.Stream << num;
       outctl.Stream << "\n";
       
       MatrixAnalysisOutControl(mat,outdat.FullName,6,15,outctl.Stream,true);
       
       MatrixAnalysisOut(mat,names,part,15,outdat.Stream,true);
       
       String pred("Predicates(Interval)");
       OutputAnalysisVariables(names,pred,outds.Stream);
       
       MatrixNumeric ttmat(length,2);
       bool ans = FillTimeTemperatureMatrix(ttmat,
					    points.Times, 
					    points.Temperatures,
					    skip);
       if(ans)
	 {
	   ObjectList<String> ttname;
	   ttname.AddObject("Temperature");
	   ttname.AddObject("Time");
	   
	   MatrixAnalysisOut(ttmat,ttname,part,15,outres.Stream,false);

	   
	   outctl.Stream << "\n";
	   outctl.Stream << outres.FullName << "\n";
	   outctl.Stream << "2 ";
	   outctl.Stream << length;
	   outctl.Stream << " NAME SAME\n";
	   
	   outctl.Stream << "0   15  F\n";
	   outctl.Stream << "15  15  F\n";
	   outctl.Stream << "\n";
	 }
     }

/*S GnuPlot
 */ 
/*F MatrixGnuPlotOutTranspose(mat,names,part,fsize,root) . . . . . . for GnuPlot data
**
**  DESCRIPTION
**    mat: The matrix
**    names: The names of the rows
**    part: The number of columns in a data row
**    fsize: The size of the element
**    root: Root of the GnuPlot files
**
**  REMARKS
**
*/
void MatrixGnuPlotOutTranspose(MatrixNumeric& mat,
		      const ObjectList<String>& names,
		      const int part,
		      const int fsize,
		      const String& root,
		      const ObjectList<double>& xcoord,
		      const int skip)
     {
     int size2 = mat.size();
     int size1 = mat[0].size();
     
     int varcnt = 0;
     int filecnt = 0;
     String suffix = "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789";
     OpenOutputFile gnu(root,"plt");
     OpenOutputFile tex(root,"tex");
     
     ObjectList<String>::const_iterator name = names.begin();

     gnu.Stream << "set terminal postscript portrait\n";
     gnu.Stream << "set data style lines\n";
     unsigned int count = 0;
     while(size2 > 0)
	  {
	    String datname = root;
	    String nm(suffix.Isolate(filecnt,filecnt+1));
	    datname.AppendToEnd(nm);
	    
	    OpenOutputFile dat(datname,"dat");
	    int part1 = part;
	    if(part1 > size2)
	      part1 = size2;
	       
	    for(int i=0; i < part1; i++)
	      {
		gnu.Stream << "set output \"";
		gnu.Stream << *name;
		gnu.Stream << ".ps\"\n";
		
		gnu.Stream << "plot '";
		gnu.Stream << dat.FullName;
		gnu.Stream << "' using ";
		gnu.Stream << "1:";
		gnu.Stream << i+2;
		gnu.Stream << " title \'";
		gnu.Stream << *name;
		gnu.Stream << "'\n";

		if(count % 3 == 0)
		  tex.Stream << "\\clearpage\n";
		count++;
		
		tex.Stream << "\\begin{figure}\n";
		tex.Stream << "\\begin{center}\n";
		tex.Stream << "\\epsfxsize=10cm\n";
		tex.Stream << "\\epsfysize=7cm\n";
		tex.Stream << "\\epsffile{";
		tex.Stream << *name;
		tex.Stream << ".ps}\n";
		tex.Stream << "\\end{center}\n";
		tex.Stream << "\\caption[";
		tex.Stream << *name;
		tex.Stream << "]\n{";
		tex.Stream << *name;
		tex.Stream << "\\label{";
		tex.Stream << *name;
		tex.Stream << "}\n}\n";
		tex.Stream << "\\end{figure}\n\n";
		
		
		name++;
	      }
	    //cout << "Gnu Times\n";
	    //cout << xcoord;
	    //cout << "\n";
	    
	    varcnt = OutputMatrixBlockTranspose(mat,size1,part1,fsize,varcnt,dat.Stream,xcoord,skip);
	    filecnt++;
	    
	    size2 -= part1;
	  }
     }
/*F newcnt = OutputMatrixBlockTranspose(mat,size1,part,varcnt) . . . . . block of data
**
**  DESCRIPTION
**    mat: The matrix
**    size1: The number of data points (rows)
**    part: The number of columns in partition
**    varcnt: The current variable count
**    out: The output stream
**    newcnt: The variable count after this partition
**
**  REMARKS
**
*/
int OutputMatrixBlockTranspose(MatrixNumeric mat, 
		      int size1, 
		      int part,
		      int fsize,
		      int varcnt,
		      ostream& out,
		      const ObjectList<double>& xcoord,
		      const int skip)
{
  ObjectList<double>::const_iterator xpoint = xcoord.begin();

  for(int pnt=0; pnt < size1; pnt++)
    {
      if(xcoord.size() > 0)
	{
	  out.setf(ios::scientific, ios::floatfield);
	  out.precision(7);
	  out << setw(fsize) << *xpoint;
	  for(int i=0;i<skip;i++)
	    xpoint++;
	}
      
      for( int ivar=0;
	  ivar < part;
	  ivar++)
	{
	  out.setf(ios::scientific, ios::floatfield);
	  out.precision(7);
	  out << setw(fsize) << mat[varcnt+ivar][pnt];
	}
      out << "\n";
    }
  varcnt += part;
  return varcnt;
}
/*F FormGnuPlotTimeMatrix(rxns,points)  . . . . .  Form GnuPlots of reactions
**
**  DESCRIPTION
**    rxns: The list of reactions
**    points: The times, temperatures and concentrations of the SENKIN
**    outroot: The root name of the GnuPlot files files
**
**    The reaction matrix is formed (FindReactionValues) and 
**    the files for making gnuplots are created (MatrixGnuPlotOut).
**    The results are the following files:
**    - root.plt: The gnuplot commands for forming the plots
**    - rootXX.dat: The set of data files
**    - root.tex: The LaTeX file for a set of plot figures
**
**  REMARKS
**
*/
void FormGnuPlotTimeMatrix(ObjectList<DbaseReaction>& rxns,
			   const MechanismSenkinDataPoints points,
			   const String outroot,
			   const int skip,
			   const bool compfi,
			   const bool takelog)
     {
       int length = SkipMatrixSize(points.Times.size(),skip);
       MatrixNumeric mat(2*rxns.size(),length);
       FindReactionValues values(mat,points,true,points.Times.size(),skip);
       
       unsigned int part = 6;
       ObjectList<String> names;
       
       cout << "CalculateValuesFromReactions\n";
       CalculateValuesFromReactions(rxns,values,compfi,takelog);

       cout << "\nFormAndAddReactionNames\n";
       FormAndAddReactionNames(rxns,names);
       MatrixGnuPlotOutTranspose(values.Mat,names,part,15,outroot,points.Times,skip);
     }
/*F FormGnuPlotConcTimeMatrix(points,outroot,skip)  . . Form GnuPlots of mols
**
**  DESCRIPTION
**    points: The times, temperatures and concentrations of the SENKIN
**    outroot: The root name of the GnuPlot files files
**    skip: The skip factor
**
**    The reaction matrix is formed (FindReactionValues) and 
**    the files for making gnuplots are created (MatrixGnuPlotOut).
**    The results are the following files:
**    - root.plt: The gnuplot commands for forming the plots
**    - rootXX.dat: The set of data files
**    - root.tex: The LaTeX file for a set of plot figures
**
**  REMARKS
**
*/
void FormGnuPlotConcTimeMatrix(const MechanismSenkinDataPoints points,
			   const String outroot,
			   const int skip)
     {
       unsigned int tlength = SkipMatrixSize(points.Times.size(),skip);
       unsigned int nummols = points.MoleculePoints.size();

       ObjectList<SenkinMoleculeDataPoints>::const_iterator mpoints = points.MoleculePoints.begin();
       ObjectList<double>::const_iterator ptemp = points.Temperatures.begin();
       
       MatrixNumeric mat(tlength,nummols+1);
       ObjectList<String> names;
       names.AddObject("Temperature");

       for(unsigned int time = 0;
	   time < tlength;
	   time++)
	 {
	   mat[time][0] = *ptemp;
	   ptemp++;
	   mpoints++;
	 }
       
       mpoints = points.MoleculePoints.begin();
       for(unsigned int mol = 0;
	   mol < nummols;
	   mol++)
	 {
	   names.AddObject((*mpoints).Molecule.NameTag);
	   ObjectList<double>::const_iterator conc = (*mpoints).Concentrations.begin();
	   //cout << (*mpoints).Molecule.NameTag << ":  ";
	   //cout << *conc << "\n";
	   
	   for(unsigned int time = 0;
	       time < tlength;
	       time++)
	     {
	       mat[time][mol+1] = (*conc);
	       conc++;
	     }
	   mpoints++;
	 }
       unsigned int part = 6;
       mat.print(cout);
       
       MatrixGnuPlotOut(mat,names,part,15,outroot,points.Times,skip);
     }
 
 
 

/*S Commands
*/
/*F ans = SetUpFilesForSenkin(g)  . . . . . . . . . . . . Set up SENKIN files
**
**  DESCRIPTION
**    g: The system information
**    ans: 0 if successful
**
**    This is the command to set up the Senkin files.  It takes
**    two inputs:
**    - The root name of the run (used as the root of the filename)
**    - The mechanism name
**
**    The mechanism is read in and the input files are generated
**
**  REMARKS
**
*/
int SetUpFilesForSenkin(ReactionSystemBase *g)
     {
     SenkinSystemBase *global = (SenkinSystemBase *) g;
     
     String filename;
     int ret=0;
     
     if(global->Inputs.size() < 2)
	  {
	  cout << "Expecting two arguments:\n";
	  cout << "     Run (root) Name\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Carrier Molecules Names\n";
	  ret = 1;
	  }
     else
	  {
	  String rootname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String mechanism = global->Inputs.front();
	  global->Inputs.pop_front();
	  
	  cout << "Senkin Run: Files For ";
	  cout << mechanism;
	  cout << "\n";
	  cout << global->Runit.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  
	  int id = global->Runit.DBMechanisms.DBMechanisms.Names[mechanism];
	  cout << "(With ID= " << id << ")\n";
	  if(id != 0)
	       {
	       global->Runit.SetUp(id);
	       global->Runit.AddCarrierMolecules(global->Inputs);
	       global->Runit.PrintOutSenkinInputFiles(rootname);
	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       }
	  }
     return 0;
     }
/*F ans = RunSenkinJobConstPressure(g)  . . . . . . . . . .  Run a Senkin Job
**
**  DESCRIPTION
**    g: TheSenkin info
**    ans: 0 if successful
**
**    The command expects the following arguments:
**    - The root name of the run (used as the root of the filename)
**    - The mechanism name
**    - Temperature
**    - Pressure
**    - Time
**    - Initial Reactant Pairs (Molecule, Mol-Fraction)
**
**  REMARKS
**
*/
int RunSenkinJob(ReactionSystemBase *g)
     {
     SenkinSystemBase *global = (SenkinSystemBase *) g;
     
     String filename;
     int ret=0;
     
     if(global->Inputs.size() < 6)
	  {
          cout << "Constant Pressure SENKIN run\n";
	  cout << "Expecting two arguments:\n";
	  cout << "     Run (root) Name (of job already set up)\n";
	  cout << "     Mechanism Name\n";
          cout << "     Initial Temperature\n";
	  cout << "     Pressure\n";
	  cout << "     Final Time\n";
          cout << "     Molecule, Mol-Fraction pairs\n";
	  ret = 1;
	  }
     else
	  {
	  String rootname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String mechanism = global->Inputs.front();
	  global->Inputs.pop_front();
	  String temperature = global->Inputs.front();
	  global->Inputs.pop_front();
	  String pressure = global->Inputs.front();
	  global->Inputs.pop_front();
	  String time = global->Inputs.front();
	  global->Inputs.pop_front();

	  cout << "Senkin Run: Files For ";
	  cout << mechanism;
	  cout << "\n";
	  cout << global->Runit.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  
	  int id = global->Runit.DBMechanisms.DBMechanisms.Names[mechanism];
	  cout << "(With ID= " << id << ")\n";
	  bool result = true;
	  if(id != 0)
	       {
	       global->Runit.SetUp(id);
	       ObjectList<String> initmols;
	       ObjectList<String> initconditions;
	       while(global->Inputs.size() > 0)
		    {
		    String mol = global->Inputs.front();
		    global->Inputs.pop_front();
		    if(mol == "Carrier")
			 {
			 String carrmol = global->Inputs.front();
			 initmols.AddObject(carrmol);
			 }
		    else
			 {
			 initconditions.AddObject(mol);
			 String molfrac = global->Inputs.front();
			 initconditions.AddObject(molfrac);
			 }
		    global->Inputs.pop_front();
		    }
	       cout << "Carrier Molecule(s)\n";
	       cout << initmols;
	       cout << "\n";
	       
	       cout << "Initial Conditions\n";
	       cout << initconditions;
	       cout << "\n";

	       if(initmols.size() > 0)
		    global->Runit.AddCarrierMolecules(initmols);

	       SenkinConstantPressureRun therun(mechanism,
						rootname,
						temperature,
						pressure,
						time,
						initconditions,
						global->Runit.Molecules);
	       
	       therun.RunAndWaitForSenkinJob(global->Runit.Molecules,
					     DEFAULT_JOB_TIMEOUT);
	       if(therun.Okay)
		    {
		    cout << "Run Successful\n";
		    cout << therun.MechanismResults;

		    PropertyTypeByName<MechanismSenkinDataPoints> *prop
			 = new PropertyTypeByName<MechanismSenkinDataPoints>(therun.RootName,therun.MechanismResults);
		    result =
			 global->Runit.Mechanism.Properties.AddObject(prop,
								      "MechanismSenkinDataPoints");
		    global->Runit.DBMechanisms.DBMechanisms.WriteDBObject(global->Runit.Mechanism);

		    String molplt("Mol");
		    
		    FormGnuPlotConcTimeMatrix(therun.MechanismResults,molplt,1);
		    }
	       else
		    result = false;
	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       result = false;
	       }
	  if(result)
	       ret = 0;
	  else 
	       ret = 1;
	  }
     return ret;
     }
 
/*F ans = InterpretSenkinJob(g) . . . . . . . . . .  Interpret SENKIN Results
**
**  DESCRIPTION
**    g: TheSenkin info
**    ans: 0 if successful
**
**    The command expects the following arguments:
**    - The root name of the run (used as the root of the filename)
**    - The mechanism name
**
**  REMARKS
**
*/
int InterpretSenkinJob(ReactionSystemBase *g)
     {
     SenkinSystemBase *global = (SenkinSystemBase *) g;
     
     String filename;
     int ret=0;
     
     if(global->Inputs.size() < 2)
	  {
          cout << "SENKIN run Interpretation\n";
	  cout << "Expecting two arguments:\n";
	  cout << "     Run (root) Name (of job already set up)\n";
	  cout << "     Mechanism Name\n";
	  cout << "     RootName of LaTeX output\n";
	  cout << "     The list of times\n";
	  ret = 1;
	  }
     else
	  {
	  String rootname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String mechanism = global->Inputs.front();
	  global->Inputs.pop_front();
	  String datname = global->Inputs.front();
	  global->Inputs.pop_front();

	  cout << "Senkin Run: Files For ";
	  cout << mechanism;
	  cout << "\n";
	  cout << global->Runit.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  
	  int id = global->Runit.DBMechanisms.DBMechanisms.Names[mechanism];
	  cout << "(With ID= " << id << ")\n";
	  bool result = true;
	  
	  if(id != 0)
	       {
	       global->Runit.SetUp(id);
//	       global->Runit.AddCarrierMolecules(global->Inputs);
	       
	       PropertyTypeByName<MechanismSenkinDataPoints> *prop
			 = (PropertyTypeByName<MechanismSenkinDataPoints> *)
			      global->Runit.Mechanism.Properties.GetPointerToProperty(rootname);
	       cout << "\n The Properties\n";	       
	       MechanismSenkinDataPoints points = prop->Object;
	       cout << "\n The Properties\n";	       
	       
	       cout << points;

	       ObjectList<double> times;
	       while(global->Inputs.size() > 0)
		 {
		   String time = global->Inputs.front();
		   global->Inputs.pop_front();
		   double thetime = time.ToFloat();
		   cout << time << "(" << thetime << ")\n";
		   times.AddObject(thetime);
		 }
	       cout << times.size() << "\n";
	       cout << times;
	       cout << "\n";
	       
	       LatexRxnVersusTimeMatrix(global->Runit.Reactions,times,points,datname,false,true);
	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       result = false;
	       }
	  if(result)
	       ret = 0;
	  else 
	       ret = 1;
	  }
     return ret;
     }
 
/*F ans = SenkinPCAAnalysis(g)  . . . . . . . . . . . Interpret SENKIN as PCA
**
**  DESCRIPTION
**    g: TheSenkin info
**    ans: 0 if successful
**
**    The command expects the following arguments:
**    - The root name of the run (used as the root of the filename)
**    - The mechanism name
**
**  REMARKS
**
*/
int SenkinPCAAnalysis(ReactionSystemBase *g)
     {
     SenkinSystemBase *global = (SenkinSystemBase *) g;
     
     String filename;
     int ret=0;
     
     if(global->Inputs.size() < 5)
	  {
          cout << "SENKIN run Interpretation\n";
	  cout << "Expecting two arguments:\n";
	  cout << "     Run (root) Name (of job already set up)\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Output File Root\n";
	  cout << "     Initial Time for analysis\n";
	  cout << "     Final Time for analysis\n";
	  cout << "     Normalised Rate (T or F)\n";
	  cout << "     Log of Rate (T or F)\n";
	  ret = 1;
	  }
     else
	  {
	  String rootname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String mechanism = global->Inputs.front();
	  global->Inputs.pop_front();
	  String datout = global->Inputs.front();
	  global->Inputs.pop_front();
	  String sinitial = global->Inputs.front();
	  global->Inputs.pop_front();
	  String sfinal = global->Inputs.front();
	  global->Inputs.pop_front();
	  String snormal = global->Inputs.front();
	  global->Inputs.pop_front();
	  String slog = global->Inputs.front();
	  global->Inputs.pop_front();
	  bool normalrate = false;
	  bool lograte = false;
	  
	  if(snormal == "T")
	    normalrate = true;
	  if(slog == "T")
	    lograte = true;

	  double initialtime = sinitial.ToFloat();
	  double finaltime = sfinal.ToFloat();

	  cout << "Senkin Run: Files For ";
	  cout << mechanism;
	  cout << "\n";
	  cout << global->Runit.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  
	  int id = global->Runit.DBMechanisms.DBMechanisms.Names[mechanism];
	  cout << "(With ID= " << id << ")\n";
	  cout << "Analysis for time interval:";
	  cout << initialtime;
	  cout << " to ";
	  cout << finaltime;
	  cout << "\n";

	  bool result = true;
	  
	  if(id != 0)
	       {
	       global->Runit.SetUp(id);

	       PropertyTypeByName<MechanismSenkinDataPoints> *prop
			 = (PropertyTypeByName<MechanismSenkinDataPoints> *)
			      global->Runit.Mechanism.Properties.GetPointerToProperty(rootname);
	       cout << "\n The Properties\n";	       
	       MechanismSenkinDataPoints points = prop->Object;
	       cout << "\n The Properties\n";	       
	       
	       cout << points;

	       FormRxnVersusTimePCA(global->Runit.Reactions,points,datout,
				    initialtime,finaltime,
				    normalrate,lograte);

	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       result = false;
	       }
	  if(result)
	       ret = 0;
	  else 
	       ret = 1;
	  }
     return ret;
     }
 
/*F ans = AnalysisSenkinJob(g)  . . . . . . .  ANALYSIS output for SENKIN job
**
**  DESCRIPTION
**    g: TheSenkin info
**    ans: 0 if successful
**
**    The data matrix of reaction rates versus time will be 
**    outputted for use with the ANALYSIS program
**
**    The command expects the following arguments:
**    - The root name of the run (used as the root of the filename)
**    - The mechanism name
**    - The root of the ANALYSIS job
**
**  REMARKS
**
*/
int AnalysisSenkinJob(ReactionSystemBase *g)
     {
     SenkinSystemBase *global = (SenkinSystemBase *) g;
     
     String filename;
     int ret=0;
     
     if(global->Inputs.size() < 5)
	  {
          cout << "SENKIN run ANALYSIS output\n";
	  cout << "Expecting three arguments:\n";
	  cout << "     Run (root) Name (of job already set up)\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Output File Root\n";
	  cout << "     Normalised Rate (T or F)\n";
	  cout << "     Log of Rate (T or F)\n";
	  ret = 1;
	  }
     else
	  {
	  String rootname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String mechanism = global->Inputs.front();
	  global->Inputs.pop_front();
	  String outroot = global->Inputs.front();
	  global->Inputs.pop_front();
	  String snormal = global->Inputs.front();
	  global->Inputs.pop_front();
	  String slog = global->Inputs.front();
	  global->Inputs.pop_front();
	  bool normalrate = false;
	  bool lograte = false;
	  
	  if(snormal == "T")
	    normalrate = true;
	  if(slog == "T")
	    lograte = true;

	  cout << "Senkin Run: Files For ";
	  cout << mechanism;
	  cout << "\n";
	  cout << global->Runit.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  cout << "Output Root: " << outroot << "\n";
	  
	  int id = global->Runit.DBMechanisms.DBMechanisms.Names[mechanism];
	  cout << "(With ID= " << id << ")\n";
	  bool result = true;
	  
	  if(id != 0)
	       {
	       global->Runit.SetUp(id);
	       
	       PropertyTypeByName<MechanismSenkinDataPoints> *prop
			 = (PropertyTypeByName<MechanismSenkinDataPoints> *)
			      global->Runit.Mechanism.Properties.GetPointerToProperty(rootname);
	       cout << "\n The Properties\n";	       
	       MechanismSenkinDataPoints points = prop->Object;
	       cout << "\n The Properties\n";	       
	       
	       cout << points;

	       FormAnalysisTimeMatrix(global->Runit.Reactions,points,outroot,1,normalrate,lograte);
	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       result = false;
	       }
	  if(result)
	       ret = 0;
	  else 
	       ret = 1;
	  }
     return ret;
     }
 
void GetNamedProperty(PropertyListByName& plist,
		 double obj,
		 String& name)
{
  PropertyTypeByName<double> *prop
    = (PropertyTypeByName<double> *)
    plist.GetPointerToProperty(name);
  obj = prop->Object;
}
/*F ans = GnuPlotSenkinJob(g) . . . . . . .  Reaction GnuPlots for SENKIN run
**
**  DESCRIPTION
**    g: TheSenkin info
**    ans: 0 if successful
**
**    The data matrix of reaction rates versus time will be 
**    outputted for use with the ANALYSIS program
**
**    The command expects the following arguments:
**    - The root name of the run (used as the root of the filename)
**    - The mechanism name
**    - The root of the ANALYSIS job
**
**  REMARKS
**
*/
int GnuPlotSenkinJob(ReactionSystemBase *g)
     {
     SenkinSystemBase *global = (SenkinSystemBase *) g;
     
     String filename;
     int ret=0;
     
     if(global->Inputs.size() < 5)
	  {
          cout << "GnuPlot for Reactions (versus time)\n";
	  cout << "Expecting three arguments:\n";
	  cout << "     Run (root) Name (of job already set up)\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Ouput File Root\n";
	  cout << "     Compute Normalised Rates (T or F)\n";
	  cout << "     Take Log of Result (T or F)\n";
	  ret = 1;
	  }
     else
	  {
	  String rootname = global->Inputs.front();
	  global->Inputs.pop_front();
	  String mechanism = global->Inputs.front();
	  global->Inputs.pop_front();
	  String outroot = global->Inputs.front();
	  global->Inputs.pop_front();
	  String computeFi = global->Inputs.front();
	  global->Inputs.pop_front();
	  String takeLog = global->Inputs.front();
	  global->Inputs.pop_front();
	  bool compfi = false;
	  bool takelog = true;
	  
	  if(computeFi == "T")
	    compfi = true;
	  if(takeLog == "F")
	    takelog = false;
	  
	  cout << "Senkin Run: Files For ";
	  cout << mechanism;
	  cout << "\n";
	  cout << global->Runit.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  cout << "Output Root: " << outroot << "\n";
	  
	  int id = global->Runit.DBMechanisms.DBMechanisms.Names[mechanism];
	  cout << "(With ID= " << id << ")\n";
	  bool result = true;
	  
	  if(id != 0)
	       {
	       global->Runit.SetUp(id);

	       PropertyTypeByName<MechanismSenkinDataPoints> *prop
		 = (PropertyTypeByName<MechanismSenkinDataPoints> *)
		 global->Runit.Mechanism.Properties.GetPointerToProperty(rootname);
	       MechanismSenkinDataPoints points = prop->Object;

	       //GetNamedProperty(global->Runit.Mechanism.Properties,points,rootname);
	       cout << "\n The Properties\n";	       	       
	       cout << points;
	       
	       String rxnplt = outroot;
	       String molplt = outroot;
	       String rxn("Rxn");
	       String mol("Mol");
	       
	       rxnplt.AppendToEnd(rxn);
	       molplt.AppendToEnd(mol);
	       
	       //FormGnuPlotConcTimeMatrix(points,molplt,1);
	       FormGnuPlotTimeMatrix(global->Runit.Reactions,points,rxnplt,1,compfi,takelog);
	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       result = false;
	       }
	  if(result)
	       ret = 0;
	  else 
	       ret = 1;
	  }
     return ret;
     }
 
/*F ans = SenkinReactionOutput(g) . . . . . . . . . . Output Reaction Constants
**
**  DESCRIPTION
**    g: TheSenkin info
**    ans: 0 if successful
**
**    The reaction constants will be outputted in LaTeX form
**
**    The command expects the following arguments:
**    - The mechanism name
**    - The root of the generated files
**
**  REMARKS
**
*/
int SenkinReactionOutput(ReactionSystemBase *g)
     {
     SenkinSystemBase *global = (SenkinSystemBase *) g;
     
     String filename;
     int ret=0;
     
     if(global->Inputs.size() < 2)
	  {
          cout << "SENKIN run ANALYSIS output\n";
	  cout << "Expecting two arguments:\n";
	  cout << "     Mechanism Name\n";
	  cout << "     Ouput File Root\n";
	  ret = 1;
	  }
     else
	  {
	  String mechanism = global->Inputs.front();
	  global->Inputs.pop_front();
	  String outroot = global->Inputs.front();
	  global->Inputs.pop_front();
	  

	  cout << "Senkin Run: Files For ";
	  cout << mechanism;
	  cout << "\n";
	  cout << global->Runit.DBMechanisms.DBMechanisms.Names;
	  cout << "\n";
	  cout << "Output Root: " << outroot << "\n";
	  
	  int id = global->Runit.DBMechanisms.DBMechanisms.Names[mechanism];
	  cout << "(With ID= " << id << ")\n";
	  bool result = true;
	  
	  if(id != 0)
	       {
	       global->Runit.SetUp(id);
	       String rxnroot(outroot);
	       String rxn("Rxn");
	       rxnroot.AppendToEnd(rxn);
	       
	       OpenOutputFile rxnout(rxnroot,"tex");

	       rxnout.Stream << "\\begin{sidewaysfigure}\n";
	       ReactionInTableStyle fstyle(REACTION_TEX,4,15,true,true);
	       ReactionTableStyle tfstyle(fstyle,100,"Reaction Set");
	       tfstyle.OutputReactionTable(rxnout.Stream,global->Runit.Reactions);
	       rxnout.Stream << "\\end{sidewaysfigure}\n";

	       rxnout.Stream << "\n";	       
	       rxnout.Stream << "\\begin{sidewaysfigure}\n";
	       OutputThermoTableStyle bstyle(THERMO_TEX);
	       ThermoTableStyle style(bstyle,"Thermodynamic Constants");
	       style.OutputThermoTable(rxnout.Stream,global->Runit.Molecules);
	       rxnout.Stream << "\\end{sidewaysfigure}\n";
	       rxnout.Stream << "\n";	       
	       }
	  else
	       {
	       cout << "Illegal Mechanism Name\n";
	       result = false;
	       }
	  if(result)
	       ret = 0;
	  else 
	       ret = 1;
	  }
     return ret;
     }
 
/*F InitializeSenkinCommands()  . . . . . . . . . . . . . . . . command setup
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void SenkinSystemBase::InitializeSenkinCommands()
     {
     SingleSystemCommand setup("SetUp",
			      "Set up files for Senkin Run",
			      &SetUpFilesForSenkin);
     Commands.AddObject("SetUp",setup);
     SingleSystemCommand runjob("RunJob",
			      "Set up files for Senkin Run",
			      &RunSenkinJob);
     Commands.AddObject("RunJob",runjob);
     SingleSystemCommand intjob("Interpret",
			      "Interpret Senkin Run",
			      &InterpretSenkinJob);
     Commands.AddObject("Interpret",intjob);
     SingleSystemCommand anljob("Analysis",
			      "Mechanism Data for ANALYSIS program",
			      &AnalysisSenkinJob);
     Commands.AddObject("Analysis",anljob);
     SingleSystemCommand plotjob("GnuPlot",
			      "Output GnuPlots of Mechanism Reactions",
			      &GnuPlotSenkinJob);
     Commands.AddObject("GnuPlot",plotjob);
     SingleSystemCommand rxnout("RxnOutput",
				"Reaction List Output",
				&SenkinReactionOutput);
     Commands.AddObject("RxnOutput",rxnout);
     SingleSystemCommand pca("PCA",
				"Principle Component Analysis",
				&SenkinPCAAnalysis);
     Commands.AddObject("PCA",pca);
     }
/*F Initialize()  . . . . . . . . . . . . . . . . . . .  intialize Senkin Run
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void SenkinSystemBase::Initialize()
     {
     InitializeSenkinCommands();
     InitializeMechanismPropDecodeFunctions();
     }
/*F SenkinSystemBase(argc,argv) . . . . . . . . . . . . . . . .  constructors
**
**  DESCRIPTION
**    argc,argv: The input arguments
**
**  REMARKS
**
*/
SenkinSystemBase::SenkinSystemBase(int argc, char *argv[])
  : ReactionSystemLevel1(argc,argv),
    Runit(argc,argv)
{
  Initialize();
}
 
/*F ans = Run() . . . . . . . . . . . . . . . . . . . . .  Running the Senkin
**
**  DESCRIPTION
**    ans: 0 if successful
**
**  REMARKS
**
*/
int SenkinSystemBase::Run()
{
  cout << "\n==================================\n";
  return Commands.ExecuteCommand(0,0,this);
}

/*S MechanismInfo
*/
/*F ReactionFromIdentify(dbreactions,reactions) . . . . . . . . . constructor
**
**  DESCRIPTION
**    dbreactions: The reaction database (reference)
**    reactions: The list of reactions in the database (reference)
**
**  REMARKS
**
*/
ReactionFromIdentify::ReactionFromIdentify(DbaseReactionSystemBase& dbreactions,
					   ObjectList<DbaseReaction>& reactions)
: DBReactions(dbreactions),
Reactions(reactions)
     {
     }
/*F MoleculeFromReaction(dbmolecules,molecules) . . . . . . . . . constructor
**
**  DESCRIPTION
**    dbmolecules: The database of molecules
**    molecules: The molecules read in
**
**  REMARKS
**
*/
MoleculeFromReaction::MoleculeFromReaction(MoleculeSystemBase& dbmolecules,
			  ObjectList<SimpleMolecule>& molecules)
: DBMolecules(dbmolecules),
Molecules(molecules)
     {
     }
/*F ReadInDBMoleculesFromReaction(dbmolecules,molecules)  . . . . constructor
**
**  DESCRIPTION
**    dbmolecules: The database molecules (dereferenced)
**    molecules: The molecules read in (dereferenced)
**
**  REMARKS
**
*/
ReadInDBMoleculesFromReaction::ReadInDBMoleculesFromReaction(MoleculeSystemBase& dbmolecule,
							     ObjectList<SimpleMolecule>& molecules)
: TransferMols(dbmolecule,molecules)
     {
     }
/*F operator()(id)  . . . . . . . . . . . . . . . . . .  MoleculeFromReaction
**
**  DESCRIPTION
**    id: The molecule ID
**
**  REMARKS
**
*/
void MoleculeFromReaction::operator()(const Identify& id)
     {
     SimpleMolecule * molecule = DBMolecules.DBMolecule.ReadDBObject(id.Identification);
     Molecules.AddObject(*molecule);
//     delete molecule;
     }
/*F operator()(id)  . . . . . . . . . . . . . . . . . . . . . .  get reaction
**
**  DESCRIPTION
**    id: The reaction database id
**
**  REMARKS
**
*/
void ReactionFromIdentify::operator()(const Identify& id)
     {
     DbaseReaction *reaction = DBReactions.DBReactions.ReadDBObject(id.Identification);
     Reactions.AddObject(*reaction);
//     delete reaction;
     }
/*F operator()(reaction)  . . . . . . . . . . . . . . molecules from reaction
**
**  DESCRIPTION
**    reaction: The reaction
**
**    All the molecules from the reaction are read in and added to the list
**
**  REMARKS
**
*/
void ReadInDBMoleculesFromReaction::operator()(const DbaseReaction& reaction)
     {
     for_each(reaction.Reactants.begin(),
	      reaction.Reactants.end(),
	      TransferMols);
     for_each(reaction.Products.begin(),
	      reaction.Products.end(),
	      TransferMols);
     }
/*F ReadInDBReactions(mechanism)  . . . . . . . . . .  read reactions from DB
**
**  DESCRIPTION
**    mechanism: The mechanism 
**
**    This routine reads the set of reactions from the database amd fills
**    in the Reactions field.
**
**  REMARKS
**
*/
void DBMechanismInfo::ReadInDBReactions(const DbaseMechanism& mechanism)
     {
     ReactionFromIdentify trans(DBReactions,Reactions);
     
     for_each(mechanism.Reactions.begin(),
	      mechanism.Reactions.end(),
	      trans);
     }
/*F ReadInDBMoleculesFromMechanism()  . . . . . . . .  molecules in mechanism
**
**  DESCRIPTION
**    This fills in the set of molecules used in the mechanism
**
**  REMARKS
**
*/
void DBMechanismInfo::ReadInDBMoleculesFromMechanism()
     {
     ReadInDBMoleculesFromReaction transfer(DBMolecules,Molecules);
     
     for_each(Reactions.begin(),
	      Reactions.end(),
	      transfer);
     Molecules.Unique();
     }
 
/*F AddCarrierMolecules(mollist)  . .  add carrier molecules to molecule list
**
**  DESCRIPTION
**    mollist: list of molecule names
**
**    The molecules are found in the database and added to the list
**    of molecules in the mechanism
**
**  REMARKS
**
*/
void DBMechanismInfo::AddCarrierMolecules(ObjectList<String>& mollist)
     {
     while(mollist.size() > 0)
	  {
	  String mol = mollist.front();
	  mollist.pop_front();
	  
	  int id = DBMolecules.DBMolecule.Names[mol];
	  if(id != 0)
	       {
	       SimpleMolecule * molecule = DBMolecules.DBMolecule.ReadDBObject(id);
	       Molecules.AddObject(*molecule);
	       }
	  else
	       {
	       cout << "Carrier Molecule: " << mol << " not found\n";
	       }
	  }
     }
/*F SetUp(mechid) . . . . .  read in all molecules and reactions of mechanism
**
**  DESCRIPTION
**    mechid: The mechanism ID
**
**  REMARKS
**
*/
void DBMechanismInfo::SetUp(const int mechid)
     {
     Molecules.erase(Molecules.begin(),Molecules.end());
     Reactions.erase(Reactions.begin(),Reactions.end());
     
     Mechanism = *(DBMechanisms.DBMechanisms.ReadDBObject(mechid));
     
     ReadInDBReactions(Mechanism);
     ReadInDBMoleculesFromMechanism();
     }

/*S CHEMKIN Tables
*/ 
/*C PrintChemkinThermoOp  . . . . . . . . . . . . .  print table for molecule
**
**  DESCRIPTION
**    For a molecule, the CHEMKIN thermodynamic class is read (GetChemkinTableObject)
**    and then printed out
**
**  REMARKS
**
*/
class PrintChemkinThermoOp
     {
     AtomInformation& AtomInfo;
     ostream& Out;
     
 public:
     PrintChemkinThermoOp(AtomInformation &atominfo,
			  ostream& out)
	  : AtomInfo(atominfo),
	  Out(out)
	       {
	       }
     void operator()(SimpleMolecule& molecule)
	  {
	  ChemkinBaseTableObject chemkin = GetChemkinTableObject(molecule,
								 AtomInfo);
	  Out << chemkin;
	  }
     };
 
/*F PrintOutChemkinInfo(out)  . . . . . . . . . . .  print out CHEMKIN tables
**
**  DESCRIPTION
**    out: The output stream
**
**    For each molecule molecule within the mechanism the CHEMKIN data is 
**    printed
**
**  REMARKS
**
*/
void SenkinRun::PrintOutChemkinInfo(ostream& out)
     {
     PrintChemkinThermoOp thermout(DBMolecules.AtomInfo,out);
     out << "THERMO";
     out << "\n";
     out << "    300.00   5000.00 1000.00   ";
     out << "\n";
     
     for_each(Molecules.begin(),
	      Molecules.end(),
	      thermout);
     }
/*F chemkin = GetChemkinTableObject(molecule,atominfo)  . Return Chemkin data
**
**  DESCRIPTION
**    molecule: The molecule
**    atominfo: The static atom information
**    chemkin: The CHEMKIN data
**
**    This retrieves the chemkin data from the molecule properties, either
**    as Chemkin or Benson.  The FormulaDescriptor is filled in and
**    the class returned
**
**  REMARKS
**
*/
ChemkinBaseTableObject GetChemkinTableObject(SimpleMolecule& molecule,
					     AtomInformation& atominfo)
     {
     ChemkinBaseTableObject chemkin;
     bool success = true;
     
     cout << "Thermo Data: ";
     cout << molecule.NameTag;
     
     PropertyTypeByName<ChemkinBaseTableObject> *prop
	  = (PropertyTypeByName<ChemkinBaseTableObject> *) 
	       molecule.Properties.GetPointerToProperty(CHEMKIN_PROP_TAG);
     if(prop->NameTag == CHEMKIN_PROP_TAG)
	  {
	  cout << "   Chemkin \n";
	  chemkin = prop->Object;
	  }
     else 
	  {
	  PropertyTypeByName<BensonBaseTableObject> *bensonprop
	       = (PropertyTypeByName<BensonBaseTableObject> *) 
		    molecule.Properties.GetPointerToProperty(BENSON_PROP_TAG);
	  if(bensonprop->NameTag == BENSON_PROP_TAG)
	       {
	       ChemkinBaseTableObject *chemkinobject
		    = new ChemkinBaseTableObject(bensonprop->Object);
	       chemkin = *chemkinobject;
	       cout << "   Benson \n";
	       }
	  else
	       {
	       cout << "   Neither ";
	       cout << BENSON_PROP_TAG << " nor " << CHEMKIN_PROP_TAG;
	       cout << " present in molecule: ";
	       cout << molecule.NameTag;
	       cout << "\n";
	       success = false;
	       }
	  }
     AtomCountList<int> acounts(molecule,"Valence:Standard","ByName=Integer");
     cout << "Atom Counts: ";
     cout << acounts;
     
     char *buf = new char[80];
     ostrstream atoms(buf,80);
     ObjectList<int>::iterator atomicnumber;
     for(atomicnumber=acounts.AtomicNumbers.begin();
	 atomicnumber!=acounts.AtomicNumbers.end();
	 atomicnumber++)
	  {
	  int atomcount = acounts.ValStats[*atomicnumber].AtomCount;
	  String atomstr = atominfo.AtomNameFromAtomicNumber(*atomicnumber);
	  if(atomstr.size() == 2)
	       atoms << atomstr << " ";
	  else
	       atoms << atomstr;
          atoms << setw(3) << setfill('0') << atomcount;
	  }
     int blanks = 4 - acounts.AtomicNumbers.size();
     for(int i=0;i<blanks;i++)
	  atoms << "     ";
     atoms << '\0';
     
     chemkin.AtSymbAndFormula = buf;
     
     return chemkin;
     }
