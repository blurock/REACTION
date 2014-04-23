/*  FILE     SECharge.cc
**  PACKAGE  SECharge
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "SECharge" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "Reaction/Molecule.hh"
#include "Reaction/SECharge.hh"

/*S Utility
*/
/*F FillBondOrder(bondorder)  . . . . . BondOrderMatrix to MatrixOfBondOrders
**
**  DESCRIPTION
**    bondorder: The structure of bond orders
**
**    This simply loops through the C structure and puts it in the C++ 
**    MatrixOfBondOrders class.
**
**  REMARKS
**
*/
void BondMatricies::FillBondOrder(BondOrderMatrix *bondorder)
     {
     int *bndord,i,j;
     double bondord;
     
//     cout << "FillBondOrder\n";
     
     bndord = bondorder->Matrix;
     for(i=0;i<bondorder->Size;i++)
	  {
	  for(j=0;j<bondorder->Size;j++)
	       {
	       bondord = (double) *bndord;
	       MatrixOfBondOrders[i].push_back(bondord);
	       bndord++;
	       }
	  }
     }

/*S Boolean
*/ 
/*F operator==(x,y) . . . . . . . . . . . . . . . . . . . . . . SubBetaCalcIJ
**
**  DESCRIPTION
**    x,y: The I and J fields to be compared
**
**  REMARKS
**
*/
bool operator==(const SubBetaCalcIJ& x, 
		const SubBetaCalcIJ& y)
     {
     return (x.I == y.I) && (x.J == y.J);
     }
/*F operator==(x,y) . . . . . . . . . . . . . . . . . . . . . ShiftElectronic
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool operator==(const ShiftElectronic& x,
		const ShiftElectronic& y)
     {
     return x.BondOrderJJ == y.BondOrderJJ;
     }
/*S IO
*/ 
/*F out = operator<<(out,electronic)  . . . . . . . . . . .  ChargeElectronic
**
**  DESCRIPTION
**    out: The output stream
**    electronic: The charge information
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, ChargeElectronic& electronic)
     {
     PrtConjBndSet(electronic.Conjset);
     return(out);
     }
/*F out = operator<<(out,calc)  . . . . . . . . . . . . . . . . SubBetaCalcIJ
**
**  DESCRIPTION
**    out: The output stream
**    calc: Intermediate info
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SubBetaCalcIJ& calc)
	  {
	  out << "SubBetaCalcIJ(";
	  out << calc.AtomicNumber << ",(";
	  out << calc.I << ",";
	  out << calc.J << "),";
	  out << calc.CovalentRadius << ",";
	  out << calc.ZEff << ",";
	  out << calc.Beta << ",";
	  out << calc.Valence << ")";
	  return out;
	  }
/*F out = operator<<(out,calc)  . . . . . . . . . . . . . . . InitialBetaCalc
**
**  DESCRIPTION
**    out: The output stream
**    calc: The intermediate data
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, InitialBetaCalc& calc)
	  {
	  out << "InitialBetaCalc\n";
	  ostream_iterator<SubBetaCalcIJ> calciter(out,"\n");
	  copy(calc.Calc.begin(),calc.Calc.end(),calciter);
	  return out;
	  }
     
/*F out = operator<<(out,electrons) . . . . . . . . . . . . .  ShiftElectrons
**
**  DESCRIPTION
**    electrons: shift information
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const ShiftElectrons& electrons)
     {
#ifdef DEBUG_CHARGE
     out << "ShiftElectrons(";
     out << electrons.Delta << ",";
     out << electrons.StopLimit << ",";
     out << electrons.CountStop << ")";
#endif
     ostream_iterator<ShiftElectronic> shiftout(out,"\n");
     copy(electrons.ShiftE.begin(),electrons.ShiftE.end(),shiftout);
     return out;
     }



/*S Misc
*/ 
/*F ans = HeteroAtomTest(atom)  . . . . . . . . . . . . . is it a heteroatom?
**
**  DESCRIPTION
**    atom: atom from which the atomic number is determined
**    ans: 1 yes, 0 no
**
**    This determines whether the atom is capable of donating (e.g. lone
**    pairs) electrons for resonance
**
**  REMARKS
**
*/
extern LOG HeteroAtomTest(ConjAtomInfo *atom)
	  {                       
	  int an;
	  
	  an = atom->AtomicN;
	  
	  if ( (an > 6 && an < 10) ||
	      (an > 14 && an < 18 ) ||
	      (an > 32 && an < 36 ) ||
	      (an > 50 && an < 54 )
	      ) 
	       return(1);
	  
	  if ( atom->Charge > 0 && 
	      atom->Double == -1 )
	       return(1);
	  
	  return(0);
	  }
/*F bondorder = MFMol2BndOrdMat(molecule) . . . . molecule to BondOrderMatrix
**
**  DESCRIPTION
**  molecule: The molecule
**  bondorder: The bond order matrix
**
**  This routine converts the information of a molecule
**  to the Bond Order Matrix
**    
**  REMARKS
**
*/
BondOrderMatrix *MFMol2BndOrdMat(const SimpleMolecule& molecule )
	  {
	  TransferBndOrdMat mattransfer((int) molecule.Atoms.size());

	  for_each(molecule.Bonds.begin(),
		   molecule.Bonds.end(),
		   mattransfer);
	  
	  return(mattransfer.bndord);
	  }
/*F conj = MFMol2CnjAtmInf(molecule)  . . . . . . molecule to ConjAtomInfoVec
**
**  DESCRIPTION
**   molecule: The molecule
**   conj: Conjugation iteration information
**
**   This routine converts the information in the molecule
**   to that needed for the atom information in the conjugation iterations
**
**   Double and Hetero have to be computed
**    
**  REMARKS
**
*/
extern ConjAtomInfoVec *MFMol2CnjAtmInf(const SimpleMolecule& molecule )
     {
     ConjAtomInfoVec    *atominfo;
     ConjAtomInfo       *vect;
     int i,size;
     
     size = (int) molecule.Atoms.size();
     atominfo = AllocateCnjAtmInfVec(size);
     
     vect = atominfo->Vector;
     int molsize = molecule.Atoms.size();
     LOOPi(molsize)
	  {
	  vect->Double = -1;
	  vect++;
	  }
     TransferBondToConjAtomInfo trans(atominfo);
     for_each(molecule.Bonds.begin(),molecule.Bonds.end(),trans);
     for_each(molecule.Atoms.begin(),molecule.Atoms.end(),trans);
     
     return(atominfo);
     }
