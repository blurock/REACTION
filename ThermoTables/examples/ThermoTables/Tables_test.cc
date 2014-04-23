#include "Reaction/System.hh"
#include "Reaction/Matrix.hh"
#include "Reaction/Statistics.hh"
#include "Reaction/ThermoTables.hh"

main (int argc, char *argv[])

     {
     int i,j,k;
     double x;
          
     ChemkinBaseTableObject ChemkinSample;
     
     
     ifstream myin("inchemkin");
     if(!myin) 
	  {
	  cout <<  "Cannot open file\n";
	  return 1;
	  };
     
     
     ofstream myout("outchemkin");
     if(!myout) 
	  {
	  cout <<  "Cannot open file\n";
	  return 1;
	  };
     cout << " this was pre-stage \n";

     
    ifstream myin_b("inbenson");
     if(!myin_b) 
	  {
	  cout <<  "Cannot open file\n";
	  return 1;
	  };
    
     

     ofstream myout_b("outbenson");
     if(!myout_b) 
	  {
	  cout <<  "Cannot open file\n";
	  return 1;
	  };
     cout << " this was pre-stage \n";

/*     
     myin >> ChemkinSample;
     cout << " ----------------------------" << endl;
     cout << " made chemkin input " << endl;
     cout << " ----------------------------" << endl; 

     cout << " SpeciesName " << endl;
     cout << ChemkinSample.SpeciesName << endl;
     cout << " Date " << endl;
     cout << ChemkinSample.Date << endl;
     cout << " AtSymbAndFormula " << endl;
     cout << ChemkinSample.AtSymbAndFormula << endl;
     cout << " PhaseDescriptor " << endl;
     cout << ChemkinSample.PhaseDescriptor << endl;
     cout << " UpperBoundTemp " << endl;
     cout << ChemkinSample.UpperBoundTemp << endl;
     cout << " LowerBoundTemp " << endl;
     cout << ChemkinSample.LowerBoundTemp << endl;
     cout << " CommonTemp " << endl;
     cout << ChemkinSample.CommonTemp << endl;
     cout << "FormulaDescriptor  " << endl;
     cout << ChemkinSample.FormulaDescriptor << endl;
     cout << " ----------------------------" << endl;
     myout << ChemkinSample;
     cout << " ----------------------------" << endl;
     cout << " made chemkin output " << endl;
     cout << " ----------------------------" << endl; 

*/

     BensonTable SampleBensonTable;
     BensonBaseTableObject BensonSample(myin_b, SampleBensonTable);
     

     myin_b >> BensonSample;
     
     cout << " ----------------------------" << endl;
     cout << " made Benson input " << endl;
     cout << " ----------------------------" << endl; 

/*     myout_b << BensonSample;
     
    
       cout << " ----------------------------" << endl;
       cout << " made Benson output " << endl;
       cout << " ----------------------------" << endl; 

       double T1=800.00;    
       double Cp1;




//     cout << " GapTrack vector " << endl;
//     cout <<  BensonSample.GapTrack << endl;
     

     cout << " ----------------------------" << endl;
     cout << " test for heat capacity calculations " << endl;
     cout << " ----------------------------" << endl;
     Cp1=BensonSample.GetHeatCapacity(T1);     
     cout << " Cp(T) = " << Cp1 << endl;
     cout << " ----------------------------" << endl;
*/



     cout << " test for benson-chemkin conversion " << endl;
     ChemkinBaseTableObject ChemkinSampleNew(BensonSample);
     
//     ChemkinSampleNew.SpeciesName="AL";
     ChemkinSampleNew.Date="62987";
// to be corrected

     ChemkinSampleNew.AtSymbAndFormula="AL  1";
     
     ChemkinSampleNew.PhaseDescriptor="G";
     ChemkinSampleNew.FormulaDescriptor="formu";

     myout << ChemkinSampleNew;
     
     cout << " ----------------------------" << endl;
     cout << " made Chemkin output" << endl;
     cout << " ----------------------------" << endl; 

     BensonBaseTableObject BensonSampleNew(ChemkinSampleNew);
       
        
     cout << " ----------------------------" << endl;
     cout << " made Benson input from chemkin" << endl;
     cout << " ----------------------------" << endl; 


     double T2=800.00;    
     double Cp2;

     cout << " ----------------------------" << endl;
     cout << " test for heat capacity calculations " << endl;
     cout << " ----------------------------" << endl;
     Cp2=BensonSampleNew.CalculateHeatCapacity(T2);     
     cout << " Cp(T) = " << Cp2 << endl;
     cout << " ----------------------------" << endl;

     myout_b << BensonSampleNew;
     
     cout << " ----------------------------" << endl;
     cout << " made Benson output " << endl;
     cout << " ----------------------------" << endl;


     myin.close();
     myout.close(); 
     myin_b.close();
     myout_b.close(); 
     
          
}








