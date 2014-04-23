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
#define TEMPLATE_INSTANTIATION
#include "Reaction/Molecule.hh"
#include "Reaction/MolProps.hh"
#include "Reaction/MolValence.hh"
#include "Reaction/MolStats.hh"

#include  "Reaction/GenerateID.hh"

template bool Encode(CommBuffer &, EquivalentIDs<SimpleMolecule> &);
template bool Decode(CommBuffer &, EquivalentIDs<SimpleMolecule> &);
template operator<<(ostream &, EquivalentIDs<SimpleMolecule> const &);
template class vector<int>;
template class list< BasicPair<int,int> >;
template class list<int>;

ostream& operator<<(ostream& out,
		    const PntrMixed<Identify,Identify>& p)
{
  return p.Pntr->print(out);
}

/*C CalculateStandardMoleculeID . . . . . . . . . . . . calculate molecule id
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class CalculateStandardMoleculeID 
     : public CalculateAndInsertObject<SimpleMolecule>
	  {
     public:
	  CalculateStandardMoleculeID()
	       {
	       }
	   SimpleMolecule& InsertIDIntoObject(SimpleMolecule& molecule, 
				  const int id,
				  const int num)
	       {
	       if(num < 100)
		    {
		    molecule.Identification = id+num;
		    }
	       return molecule;
	       }
	  int CalculateBaseID(SimpleMolecule& molecule)
	       {
	       AtomCountList<int> counts(molecule,"Valence:Standard","ByName-Integer");
	       AtomicNumberCount<int> carbons   = counts.ValStats[6];
	       AtomicNumberCount<int> hydrogens = counts.ValStats[1];
	       
	       int numhydrogens = hydrogens.AtomCount;
	       int numcarbons   = carbons.AtomCount;
	       int numatoms     = molecule.Atoms.size();
	       int numhetero    = numatoms - numcarbons - numhydrogens;
	       
	       int id = 
		    numcarbons * 1000000 +
			 numatoms   *   10000 +
			      numhetero  *     100;
	       return id;
	       }
	  };
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

//template class ostream_iterator<Identify>;
//template void destroy(Identify *);
//template void construct(Identify *, Identify const &);
main()
     {
     cout << "\n--------------------------------------------\n";
     cout << "AtomInformation Read\n";
     ifstream sfrom("/home/reaction/Reaction/data/stat-inf.dat");
     ifstream mfrom("/home/reaction/Reaction/data/StandardMeta.mta");
     AtomInformation atominfo(&sfrom,&mfrom);

     cout << "\n--------------------------------------------\n";
     cout << "MolFileMolecule\n";
     ifstream smolfrom("moleculetest.txt");
     MolFileMolecule mfmol1(smolfrom,atominfo);
     MolFileMolecule mfmol2(smolfrom,atominfo);
     MolFileMolecule mfmol3(smolfrom,atominfo);
     cout << "\n--------------------------------------------\n";
     cout << "SimpleMolecule\n";
     SimpleMolecule simp1(mfmol1,atominfo);
     SimpleMolecule simp2(mfmol2,atominfo);
     SimpleMolecule simp3(mfmol3,atominfo);
     cout << "\n--------------------------------------------\n";
     cout << "The Valence\n";
     ComputeNormalValenceSet normal1(simp1);     
     ComputeNormalValenceSet normal2(simp2);     
     ComputeNormalValenceSet normal3(simp3);     

     AtomCountList<int> counts(simp1,"Valence:Standard","ByName-Integer");
     
     cout << counts;
     cout << "\n=================================================\n";
     CalculateStandardMoleculeID calc;
     
     cout << "\n--------------------------------------------\n";
     int id = calc.CalculateBaseID(simp1);
     cout << id << "\n";
     cout << "\n--------------------------------------------\n";
     calc.InsertIDIntoObject(simp1,id,5);
     cout << simp1.Identification;
     cout << "\n--------------------------------------------\n";
     SetOfIDs<SimpleMolecule> molset(&calc);
     
     simp1 = molset.InsertNewElement(simp1);
     cout << simp1.Identification;
     cout << "\n--------------------------------------------\n";
     simp2 = molset.InsertNewElement(simp2);
     cout << simp2.Identification;
     cout << "\n--------------------------------------------\n";
     simp3 = molset.InsertNewElement(simp3);
     cout << simp3.Identification;
     cout << "\n--------------------------------------------\n";

     return(0);
     }

     

