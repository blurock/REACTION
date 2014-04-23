/*  FILE     main.cc
**  PACKAGE     ANALYSIS
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
using namespace std;
#define TEMPLATE_INSTANTIATION
#include "CoreDataObjects.hh"
#include "Vector.hh"
#include "NumericObjects.hh"
#include "LogicalObjects.hh"
#include "FunctionReal1DObjects.hh"
#include "OperationObjects.hh"
#include "NumericOperations.hh"
#include "LogicalOperations.hh"
#include "DataObjects.hh"
#include "InstanceObjects.hh"
#include "DirectedTreeObjects.hh"
#include "SelectObjects.hh"
#include "AlgorithmObjects.hh"
#include "InstanceAlgorithms.hh"
#include "PredicateObjects.hh"
#include "PredicateGen.hh"
#include "EntropyObjects.hh"
#include "EvaluationTree.hh"
#include "DecisionTreeAlgorithms.hh"
#include "GoalObjects.hh"
#include "PrimitiveStats.hh"
#include "ExpressionTree.hh"
#include "ParameterizedFunction.hh"
#include "DistributionAlgorithm.hh"
#include "MatrixUtilities.hh"
#include "BasicLaTeXTable.hh"
#include "EigenValuesOut.hh"
#include "EigenValues.hh"
#include "PCAAnalysis.hh"
#include "InstancePCA.hh"
#include "DescriptionProbs.hh"
#include "NumericProperties.hh"
#include "ClusterTree.hh"
#include "CobwebCluster.hh"

#include "RuleSystem.hh"
#include "Optimize.hh"
#include "PopulationBasedOptimization.hh"
#include "ParameterizedFunction.hh"
#include "GeneticOperations.hh"
#include "VectorOperations.hh"
#include "Utilities.hh"
//#include "VerifyEnvironment.hh"
//#include "SimulatedAnnealing.hh"
#include "Consecutive.hh"
#include "GeneralGraph.hh"
#include "GraphOperations.hh"
#include "FullSystem.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Dbase.hh"
#include "Molecule.hh"
//#include "SECharge.hh"
#include "Flame.hh"
#include "Molecule.hh"
#include "ThermoProps.hh"
int FlameTest(ReactionSystemBase *sys); 
int DistributionReport(ReactionSystemBase *sys);
int CriticalPointList(ReactionSystemBase *sys);
int InstanceFromMatrix(ReactionSystemBase *sys);
class SystemFlameTest : public GoalSystemSave
{
public:
  SystemFlameTest(int argc, char *argv[])
    : GoalSystemSave(argc,argv)
    {
    }
  virtual void StandardObjectsSetUp()
    {
      GoalSystemSave::StandardObjectsSetUp();

      AddBaseAlgorithmClasses(getStandard());
      AddSelectionTreeClasses(getStandard());
      AddDirectedTreeObjects(getStandard());


      AddInstanceAlgorithmsClasses(getStandard());
      AddEntropyAlgorithmClasses(getStandard());
      AddPredicateClasses(getStandard());
      AddPredicateGenAlgorithmClasses(getStandard());
      AddEvaluationTreeClasses(getStandard());
      AddDecisionTreeClasses(getStandard());
      AddCobwebAlgorithmClasses(getStandard());
      AddClusterTreeClasses(getStandard());
      AddBaseExpressionTreeClasses(getStandard());
      AddBaseDistributionClasses(getStandard());
      //AddBasePCAClasses(getStandard());
      AddDescriptionProbsClasses(getStandard());
      AddLogicalOperationClasses(getStandard());
      AddNumericOperationClasses(getStandard());
      AddParameterizedClasses(getStandard());
      AddConsecutiveClasses(getStandard());
      AddRxnUtilitiesClasses(getStandard());
      AddVectorOperationClasses(getStandard());

      AddGeneralGraphClasses(getStandard());
      AddGraphOperationClasses(getStandard());

      //AddRuleSystemClasses(getStandard());
      //AddSimAnnAlgorithmsClasses(getStandard());

      AddOptimizeClasses(getStandard());
      AddPopulationClasses(getStandard());
      AddGeneticClasses(getStandard());
      
      //AddBaseVerifyClasses(getStandard());

      AddNumericPropertiesClasses(getStandard());
      
      AddFlameClasses(getStandard());
    }
  virtual void EncodeDecodeObjectsSetUp()
    {
      GoalSystemSave::EncodeDecodeObjectsSetUp();
      InitialInstanceAlgorithmsEncodeDecodeRoutines();
      InitialEntropyAlgorithmEncodeDecodeRoutines();
      InitialSetOfPredicateEncodeDecodeRoutines();
      InitialPredicateGenEncodeDecodeRoutines();
      InitialEvaluationTreeEncodeDecodeRoutines();
      DecisionTreeEncodeDecodeRoutines();
      ClusterTreeEncodeDecodeRoutines();
      CobwebClusterEncodeDecodeRoutines();
      InitialSetOfExpressionTreeEncodeDecodeRoutines();
      InitialDistributionObjectsEncodeDecodeRoutines();
      InitialPCAEncodeDecodeRoutines();
      InitialDescriptionProbsEncodeDecodeRoutines();
      InitialSetOfNumericOperationEncodeDecodeRoutines();
      InitialSetOfLogicalOperationEncodeDecodeRoutines();
      InitialParameterizedEncodeDecodeRoutines();
      ConsecutiveEncodeDecodeRoutines();
      InitialSetOfGeneralGraphEncodeDecodeRoutines();
      InitialGraphOperationEncodeDecodeRoutines();
      InitialRxnUtilitiesDecodeFunctions();

      InitialSetOfOptimizeEncodeDecodeRoutines();
      InitialSetOfPopulationEncodeDecodeRoutines();
      InitialGeneticEncodeDecodeRoutines();

      InitialSetOfNumericPropertiesEncodeDecodeRoutines();
      //InitialSimAnnAlgorithmsEncodeDecodeRoutines();

      //InitialSetOfRuleSystemEncodeDecodeRoutines();
      //VerifyEncodeDecodeRoutines();
      InitialVectorOperationsEncodeDecodeRoutines();
      FlameEncodeDecodeRoutines();


    }
  void CommandSetUp()
    {
      GoalSystemSave::CommandSetUp();
      String srd("Flame");
      SingleSystemCommand rd(srd,
			     "Flame Test",
			     &FlameTest);
      Commands.AddObject(srd,rd);

      String distS("DistributionReport");
      SingleSystemCommand dist(distS,
			     "Create a Distribution Statistics Report",
			     &DistributionReport);
      Commands.AddObject(distS,dist);

      String critS("MakeCriticalPoints");
      SingleSystemCommand crit(critS,
			     "Create a Critical Points File",
			     &CriticalPointList);
      Commands.AddObject(critS,crit);

      String instS("MakeInstanceFromMatrix");
      SingleSystemCommand inst(instS,
			     "Create Instances from a Matrix",
			     &InstanceFromMatrix);
      Commands.AddObject(instS,inst);
    }
  int Run() {
    String done("END");
    if(Inputs.size() == 0) {
      istream& inputstream = cin;
      String inpline;
      bool notDone = true;
      while(notDone) {
	cout << "Reaction::" << endl;
	cerr << "Reaction::" << endl;
	cout.flush();
	cerr.flush();
	inpline.ReadFullLine(inputstream);
	cout << "Command Line: '" << inpline << "'" << endl;
	cerr << "Command Line: '" << inpline << "'" << endl;
	if(!strncmp(inpline.c_str(), done.c_str(),3))
	  notDone = false;
	else {
	  loadInput(inpline);
	  Commands.ExecuteCommand(0,0,this);
	}
      }
    } else {
      Commands.ExecuteCommand(0,0,this);
    }
    return 1;
  }
  void loadInput(String line) {
    String word;
    while(Inputs.size() > 0) {
      word = this->GetNextInput();
      cout << "Extra: '" << word << "'" << endl;
    }

    while(!line.IsEmpty()) {
      line.IsolateNextWord(word,' ');
      Inputs.AddObject(word);
    }
  }
};
/*F main(argc,argv)
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int main(int argc, char *argv[])
{
  SystemFlameTest test(argc,argv);
  
  test.Initialization();
  test.Run();
  return test.Exit();
}

int FlameTest(ReactionSystemBase *sys)
{
  SystemFlameTest *flametest = (SystemFlameTest *) sys;
  BaseDataFlameSensitivityData flame;
  DataFlameSensitivityDataClass flameclass;

  String filename = flametest->GetNextInput();
  String cutoffS = flametest->GetNextInput();
  String name("Flame");
  if(sys->Inputs.size() > 0)
    name = flametest->GetNextInput();

  OpenInputFile file(filename);
  if(file.IfOpen())
    {
      flame.Read(file.Stream,&flameclass,name);
      
      flame.NameTag = "MatrixObject";
      flametest->Instances.AddObject(&flame);
    }
  else
    {
      cerr << "Flame Matrix not created" << endl;
    }
  cout << "------------------------------------------" << endl;
  cout << "   Reaction Extent";
  cout << "------------------------------------------" << endl;

  BaseDataKeySet times = flame.getInstanceNames();
  times.NameTag = "TotalInstanceNameList";
  BaseDataKeySet molecules = flame.getParameterNames();
  molecules.NameTag = "AttributeNames";
  flametest->Instances.AddObject(&times);
  flametest->Instances.AddObject(&molecules);

  int numberoftimes = times.SizeOf();
  cout << "The number of Times: " << numberoftimes << endl;
  int numberofmolecules = molecules.SizeOf();
  MatrixNumeric mat = flame.CurrentMatrix();
  VectorNumeric vec = mat.getMatrixRow(numberoftimes - 1);

  double cutoff = cutoffS.ToFloat();
  for(int j=0;j<numberofmolecules;j++) {
    String name = molecules.NextKey();
    if(vec[j] > cutoff) 
      cout << setw(20) << name << setw(20) << vec[j] << endl;
  }

  return 0;
}

int DistributionReport(ReactionSystemBase *sys)
{
  SystemFlameTest *flametest = (SystemFlameTest *) sys;

  String reportname = flametest->GetNextInput();
  String dists      = flametest->GetNextInput();

  String tex("tex");
  OpenOutputDataFile reportout(reportname,tex);
  reportout.Stream << "\\section{Distribution of Data}" << endl;

  String gnu("gnu");
  OpenOutputDataFile gnuout(reportname,gnu);
  gnuout.Stream << "set grid" << endl;
  gnuout.Stream << "set data style boxes" << endl;
  gnuout.Stream << "set terminal postscript eps monochrome solid" << endl;

  String postfix("dat");

  BaseDataKeyWords *keys = (BaseDataKeyWords *) flametest->Instances.GetObject(dists);
  cout << "The keywords: " << endl;
  keys->print(cout);
  cout << endl;

  ObjectList<String> names = keys->GetKeyWords();
  ObjectList<String>::iterator name;
  for(name = names.begin(); name != names.end(); name++)
    {
      cout << "Distribution: " << *name << endl;
      if(flametest->Instances.IsInList(*name))
	{
	  OpenOutputDataFile distout(*name,postfix);

	  BaseDataAttributeDistribution *distribution = (BaseDataAttributeDistribution *) flametest->Instances.GetObject(*name);
	  EvenDistributionStats *stats = (EvenDistributionStats *) distribution->getStatistics();
	  double val = stats->Min;
	  for(VectorNumeric::const_iterator count = stats->Counts.begin() ; 
	      count != stats->Counts.end();
	      count++)
	    {
	      distout.Stream << setw(20) << val << "  ";
	      distout.Stream << setw(20) << *count << endl;
	      val += stats->IntervalSize;
	    }
	  
	  gnuout.Stream << "set output '" << *name << ".eps" << "'" << endl;
	  gnuout.Stream << "plot '" << *name << "." << postfix << "'" << endl;

	  reportout.Stream << "\\begin{figure}[htb]" << endl;
	  reportout.Stream << "    \\begin{center}" << endl;
	  reportout.Stream << "      \\standardeps{" << *name << "}" << endl;
	  reportout.Stream << "      \\caption{Distribution from " << *name << "}" << endl;
	  reportout.Stream << "      \\label{fig:" << *name << "}" << endl;
	  reportout.Stream << "    \\end{center}" << endl;
	  reportout.Stream << "\\end{figure}" << endl;
	  reportout.Stream << "\\clearpage" << endl;
	}
      else
	{
	  cerr << "Distribution: '" << *name << "' not found in Attributes" << endl;
	}
    }
  return 0;
}

int CriticalPointList(ReactionSystemBase *sys)
{
  SystemFlameTest *flametest = (SystemFlameTest *) sys;

  String critpointsname = flametest->GetNextInput();
  String dists      = flametest->GetNextInput();
  String listtype       = flametest->GetNextInput();

  String dat("dat");
  OpenOutputDataFile critpointout(critpointsname,dat);

  BaseDataKeyWords *keys = (BaseDataKeyWords *) flametest->Instances.GetObject(dists);

  ObjectList<String> names = keys->GetKeyWords();
  ObjectList<String>::iterator name;
  for(name = names.begin(); name != names.end(); name++)
    {
      cout << "Distribution: " << *name << endl;
      if(flametest->Instances.IsInList(*name))
	{
	  BaseDataAttributeDistribution *distribution = (BaseDataAttributeDistribution *) flametest->Instances.GetObject(*name);
	  EvenDistributionStats *stats = (EvenDistributionStats *) distribution->getStatistics();
	  double val = stats->Min;
	  critpointout.Stream << *name << endl;
	  critpointout.Stream << "CriticalPoints: " << stats->NameTag << "   ";
	  if(listtype == "DistributionPartitions")
	    {
	      for(VectorNumeric::const_iterator count = stats->Counts.begin() ; 
		  count != stats->Counts.end();
		  count++)
		{
		  critpointout.Stream << "[" << val << "," << stats->IntervalSize << "]  ";
		  val += stats->IntervalSize;
		}
	    }
	  else
	    {
	      critpointout.Stream << "[" << stats->Min + stats->IntervalSize << "," << stats->IntervalSize << "]  ";
	      critpointout.Stream << "[" << stats->Average << "," << stats->StdDev << "]  ";
	    }
	  critpointout.Stream << "END" << endl;
	}
      else
	{
	  cerr << "Distribution: '" << *name << "' not found in Attributes" << endl;
	}
    }
  critpointout.Stream << "END" << endl;

  return 0;
}

int InstanceFromMatrix(ReactionSystemBase *sys)
{
  SystemFlameTest *flametest = (SystemFlameTest *) sys;
  bool result = true;

  String matrixS = flametest->GetNextInput();
  
  if(flametest->Instances.IsInList(matrixS))
    {
      BaseDataInstanceDoubleMatrix *instmat = (BaseDataInstanceDoubleMatrix *) flametest->Instances.GetObject(matrixS);
      MatrixNumeric &mat = instmat->CurrentMatrix();
      BaseDataKeySet &namekeys = instmat->getParameterNames();
      BaseDataKeySet &instkeys = instmat->getInstanceNames();

      BaseDataKeySet namelist = instmat->getInstanceNames();
      namelist.NameTag = "TotalInstanceNameList";
      flametest->Instances.AddObject(&namelist);

      ObjectList<String> params = namekeys.GetKeyWords();
      ObjectList<String> insts  = instkeys.GetKeyWords();
      ObjectList<String>::iterator param,inst;
      unsigned int instcount = 0;
      BaseDataReal *number = new BaseDataReal();
      BaseDataInstance *instance;
      for(inst = insts.begin(); inst != insts.end();inst++)
	{
	  if(!flametest->Instances.InstanceInSet(*inst)) {
	    instance = new BaseDataInstance();
	    instance->NameTag = *inst;
	    flametest->Instances.AddInstance(*instance);
	    delete instance;
	  }
	  instance = flametest->Instances.GetInstance(*inst);
	  
	  unsigned int paramcount = 0;
	  for(param=params.begin();param != params.end();param++)
	    {
	      double val = mat[instcount][paramcount];
	      number->SetValue(val);
	      number->NameTag = *param;
	      instance->AddObject(number);
	      paramcount++;
	    }
	  instcount++;
	}
    }
  else
    {
      cerr << "Matrix Object: ':" << matrixS << "' not found in Attributes" << endl;
      result = false;
    }
  return 0;
}
