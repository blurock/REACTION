/*  FILE     Molecule.cc
**  PACKAGE  Molecule
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "Molecule" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "Reaction/Molecule.hh"
#include "Basis/Vector.hh"
#include "Basis/MatrixNumeric.hh"
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

template class list<MoleculeAtom>;
template class list<MoleculeBond>;
template class list<MolFileAtom>;
template class list<MolFileBond>;
template class vector<SimpleBondAtom>;

template bool Decode(CommBuffer &, ObjectList<MoleculeAtom> &);
template bool Decode(CommBuffer &, ObjectList<MoleculeBond> &);

template ostream& operator<<(ostream &, ObjectList<MoleculeAtom> const &);
template ostream& operator<<(ostream &, ObjectList<MoleculeBond> const &);
/*S System
 */
/*F Initialize()  . . . . . . . . . . . . . . . .  Initialize molecule system
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void MoleculeSystemBase::Initialize()
     {
     }
/*F InitializeProperties()  . . . . . . . . . . . . . . initialize properties
**
**  DESCRIPTION
**    This initializes the property list for structures with accumulations 
**    of objects (for example OName classes).
**
**  REMARKS
**
*/
void SimpleMolecule::InitializeProperties()
     {
     InitONameForPropertyList(&Properties);
     }
 
/*F nout = operator<<(out,counts) . . . . . . . . . . . . .  SimpleBondCounts
**
**  DESCRIPTION
**    out: The output stream
**    counts: The structure
**    nout: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SimpleBondCounts& counts)
{
  ostream_iterator<SimpleBondAtom> atom (out,"\n");
  copy(counts.Bonding.begin(),counts.Bonding.end(),atom);
  return out;
}


    

/*S Utilities
*/ 
/*F MolFileMolecule::MolFileMolecule(file,atominfo) . . . . .  read from file
**
**  DESCRIPTION
**     file: The input stream
**     atominfo: The atomic info for conversions
**
**     The MolFile has the following form:
**    - line 1        : ID number
**    - line 2        : ignored
**    - line 3        : Molecule Name
**    - line 4        : Number of Atoms and Number of Bonds (%3d%3d)
**    - line 4-nnn    : Atoms (See routine ReadMFAtom)
**    - line nnn-mmm  : Bonds (See routine ReadMFBond)
**    
**
**  REMARKS
**
**  REFERENCES
**
**  SEE ALSO
**
**  HEADERFILE
**
*/
MolFileMolecule::MolFileMolecule(istream& file,
				 AtomInformation& atominfo)
     {
     String line,name;
     int numatoms,numbonds;
     
     Atoms.ChangeTitle("\n------ MolFile Atoms -----\n");     
     Atoms.ChangeDelimitor("\n");
     Bonds.ChangeTitle("\n------ MolFile Bonds -----\n");
     Bonds.ChangeDelimitor("\n");
     
     line.ReadFullLine(file);
     
     if(line.size() > 0 && *(line.chars()) != 0)
	  {
	  Identification = line.ToInteger();
	  
	  line.ReadFullLine(file);
	  name.ReadFullLine(file);
//	  remove(name.begin(),name.end(),'\n');
	  
	  name.EliminateLeadingBlanks();
//	  NameTag = *(new String(name));
	  NameTag = name;
	  
	  cout << "(" << Identification << "," << NameTag << ")" << endl;
	  
	  line.ReadFullLine(file);
	  
	  numatoms = line.ToInteger(0,2);
	  numbonds = line.ToInteger(3,5);
	  
//	  ReadMolFileAtom atomread(file,atominfo);
	  for(int count=0; count != numatoms ; count++)
	    {
	      MolFileAtom atom;
	      atom.ReadMFAtom(file,atominfo);
	      atom.Identification = count;
	      Atoms.AddObject(atom);
	    }
	  
//	  generate_n(back_insert_iterator< ObjectList<MolFileAtom> > (Atoms),
//		     numatoms,atomread);

	  ReadMolFileBond bondread(file);
	  generate_n(back_insert_iterator< ObjectList<MolFileBond> > (Bonds),
		     numbonds,bondread);
	       
	  line.ReadFullLine(file);
	  while(strncmp(line.chars(),"$$$",3))
	       line.ReadFullLine(file);
	  }
     else
	  {
	  Identification = NO_MORE_MOLECULES;
	  cout << "Done Reading" << Identification << "\n";
	  }
     }
 
/*f TransferCovalentBond(atom,info)  . . . . . . . . . . .  ReadCovalentRadius
**
**  DESCRIPTION
**    atom: The atom
**    info: General atomic information
**
**    This uses the ReadCovalentRadius routine to retrieve the covalent radius
**
**  REMARKS
**
*/
int TransferCovalentBond(SimpleElectronicAtom& atom,
				const AtomInformation& info)
     {
     atom.CovalentRadius = info.ReadCovalentRadius(atom.AtomicNumber);
     return 0;
     }
 
/*f counts = CountBondTypes(counts, bond) . . . . . . . .  update bond counts
**
**  DESCRIPTION
**    count: The current bond count
**    bond:  The bond
**
**    This routine is used in conjuction with an STL accumulate to 
**    count the number of bond types for an atom
**
**  REMARKS
**
*/
extern SimpleBondCounts&  CountBondTypes(SimpleBondCounts& counts, const SimpleBond& bond)
     {
     switch(bond.BondOrder)
	  {
     case 1:
	  counts.Bonding[bond.I].SingleBondCount += 1;
	  counts.Bonding[bond.J].SingleBondCount += 1;
	  break;
     case 2:
	  counts.Bonding[bond.I].DoubleBondCount += 1;
	  counts.Bonding[bond.J].DoubleBondCount += 1;
	  break;
     case 3:
	  counts.Bonding[bond.I].TripleBondCount += 1;
	  counts.Bonding[bond.J].TripleBondCount += 1;
	  break;
	  }
     return(counts);
     }

 
/*f molatom = TransferBonding(bondatom,molatom) . . . . . . . . transfer data
**
**  DESCRIPTION
**    bondatom: The simple bond info
**    molatom:  The molecule atom
**
**   This transfers the bond counts of the SimpleBondAtom to the molecule atom
**
**  REMARKS
**
*/
extern MoleculeAtom& TransferBonding(SimpleBondAtom& bondatom, 
				     MoleculeAtom& molatom)
     {
     molatom.SingleBondCount = bondatom.SingleBondCount;
     molatom.DoubleBondCount = bondatom.DoubleBondCount;
     molatom.TripleBondCount = bondatom.TripleBondCount;
     return molatom;
     }

 
/*F SimpleMolecule(molfile,info)  . . . . simple molecule from input molecule
**
**  DESCRIPTION
**    molfile: The MolFile molecule
**    info: The set of atomic information
**
**    This not only transfers the MolFileMolecule data to the 
**    SimpleMolecule, but it also calculates:
**    - (FindCovalentBondFromInfo) The covalent radius
**    - (CountBondTypes) Bond type counts for each atom
**    - (TransferBonding) Transfers bond counts to SimpleAtom
**    - (CalculateSimpleElectronic) Simple electronic calcs
**
**  REMARKS
**
*/
SimpleMolecule::SimpleMolecule(const MolFileMolecule& molfile,
			       const AtomInformation& info)
  : BasicMolecule(molfile),
    Bonds(molfile.Bonds.size()), 
    Atoms(molfile.Atoms.size())
     {
     SimpleBondCounts counts(molfile.Atoms.size());
     CalculateSimpleElectronic electronic;

     Bonds.ChangeDelimitor("\n");
     Bonds.ChangeTitle("\n----- SimpleMolecule Bonds -----\n");
     Atoms.ChangeDelimitor("\n");
     Atoms.ChangeTitle("\n----- SimpleMolecule Atoms -----\n");
     Properties.ChangeDelimitor("\n");
     Properties.ChangeTitle("\n----- SimpleMolecule Properties -----\n");
     
     InitializeProperties();
     

     copy(molfile.Bonds.begin(),
	  molfile.Bonds.end(),
	  Bonds.begin());
     copy(molfile.Atoms.begin(),
	  molfile.Atoms.end(),
	  Atoms.begin());
     for_each(Atoms.begin(),
	      Atoms.end(),
	      FindCovalentBondFromInfo(info));
     
     counts = accumulate(molfile.Bonds.begin(),
			 molfile.Bonds.end(),
			 counts,
			 CountBondTypes);
     transform(counts.Bonding.begin(),
	       counts.Bonding.end(),
	       Atoms.begin(),
	       Atoms.begin(),
	       TransferBonding);

     for_each(Atoms.begin(),
	      Atoms.end(),
	      electronic);
     }

 
 

/*S IO
*/
/*F out = operator<<(out,mol) . . . . . . . . . . . . . . . . . BasicMolecule
**
**  DESCRIPTION
**    out: The output stream
**    mol: The molecule
** 
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const BasicMolecule& mol)
     {
     return mol.print(out);
     } 
/*F out = operator<<(out,mol) . . . . . . . . . . . . . . . . MolFileMolecule
**
**  DESCRIPTION
**    out: The output stream
**    mol: The molecule
**    
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const MolFileMolecule& mol)
     {
     return mol.print(out);
     }
 
/*F out = operator<<(out,mol) . . . . . . . . . . . . . . . .  SimpleMolecule
**
**  DESCRIPTION
**    out: The output stream
**    mol: The molecule
**    
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SimpleMolecule& mol)
     {
     return mol.print(out);
     }

/*S Boolean
*/
 
/*F ans = operator==(x,y) . . . . . . . . . . . . . . . . . . . BasicMolecule
**
**  DESCRIPTION
**    x,y: The molecule
**    ans: the ID's are the same
**
**  REMARKS
**
*/
bool operator==(const BasicMolecule& x,
		const BasicMolecule& y)
     {
     return x.Identification == y.Identification;
     }
/*F ans = operator<(x,y)  . . . . . . . . . . . . . . . . . . . BasicMolecule
**
**  DESCRIPTION
**    x,y: The molecule
**    ans: the ID of x is less than that of y
**    
**  REMARKS
**
*/
bool operator<(const BasicMolecule& x,
	       const BasicMolecule& y)
     {
     return x.Identification < y.Identification;
     }

     
/*F ans = operator==(x,y) . . . . . . . . . . . . . . . . . . MolFileMolecule
**
**  DESCRIPTION
**    x,y: The molecule
**    ans: the ID's are the same
**
**  REMARKS
**
*/
bool operator==(const MolFileMolecule& x,
		const MolFileMolecule& y)
     {
     return x.Identification == y.Identification;
     }
/*F ans = operator<(x,y)  . . . . . . . . . . . . . . . . . . MolFileMolecule
**
**  DESCRIPTION
**    x,y: The molecule
**    ans: the ID of x is less than that of y
**    
**  REMARKS
**
*/
bool operator<(const MolFileMolecule& x,
	       const MolFileMolecule& y)
     {
     return x.Identification < y.Identification;
     }

     
/*F ans = operator==(x,y) . . . . . . . . . . . . . . . . . .  SimpleMolecule
**
**  DESCRIPTION
**    x,y: The molecule
**    ans: the ID's are the same
**
**  REMARKS
**
*/
bool operator==(const SimpleMolecule& x,
		const SimpleMolecule& y)
     {
     return x.Identification == y.Identification;
     }
/*F ans = operator<(x,y)  . . . . . . . . . . . . . . . . . .  SimpleMolecule
**
**  DESCRIPTION
**    x,y: The molecule
**    ans: the ID of x is less than that of y
**    
**  REMARKS
**
*/
bool operator<(const SimpleMolecule& x,
	       const SimpleMolecule& y)
     {
     return x.Identification < y.Identification;
     }

     

/*S EncodeDecode
 */
/*F ans = Encode(buffer,mol)  . . . . . . . . . . . . . . . . . BasicMolecule
**
**  DESCRIPTION
**    buffer: The buffer class (input)
**    mol: The molecule
**    ans: true if successful
**    
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, BasicMolecule& mol)
     {
     return mol.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,mol)  . . . . . . . . . . . . . . . . . BasicMolecule
**
**  DESCRIPTION
**    buffer: The buffer class 
**    mol: The molecule
**    ans: true if successful
**    
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, BasicMolecule& mol)
     {
     return mol.DecodeThis(buffer);
     }
/*F ans = Encode(buffer,mol)  . . . . . . . . . . . . . . . . MolFileMolecule
**
**  DESCRIPTION
**    buffer: The buffer class (input)
**    mol: The molecule
**    ans: true if successful
**    
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, MolFileMolecule& mol)
     {
     return mol.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,mol)  . . . . . . . . . . . . . . . . MolFileMolecule
**
**  DESCRIPTION
**    buffer: The buffer class 
**    mol: The molecule
**    ans: true if successful
**    
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, MolFileMolecule& mol)
     {
     return mol.DecodeThis(buffer);
     }
/*F ans = Encode(buffer,mol)  . . . . . . . . . . . . . . . .  SimpleMolecule
**
**  DESCRIPTION
**    buffer: The buffer class (input)
**    mol: The molecule
**    ans: true if successful
**    
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, SimpleMolecule& mol)
     {
     return mol.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,mol)  . . . . . . . . . . . . . . . .  SimpleMolecule
**
**  DESCRIPTION
**    buffer: The buffer class 
**    mol: The molecule
**    ans: true if successful
**    
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, SimpleMolecule& mol)
     {
     return mol.DecodeThis(buffer);
     }

 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  SimpleMolecule
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**    
**  REMARKS
**
*/
bool SimpleMolecule::EncodeThis(CommBuffer& buffer)
     {
     bool result = BasicMolecule::EncodeThis(buffer);
     result = result && Atoms.EncodeThis(buffer);
     result = result && Bonds.EncodeThis(buffer);
     result = result && Properties.EncodeThis(buffer);
     
     return result;
     }
 
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  SimpleMolecule
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool SimpleMolecule::DecodeThis(CommBuffer& buffer)
     {
     bool result = BasicMolecule::DecodeThis(buffer);
     result = result && Atoms.DecodeThis(buffer);
     result = result && Bonds.DecodeThis(buffer);
     Properties.erase(Properties.begin(),Properties.end());
     result = result && Properties.DecodeThis(buffer);
     return result;
     }
/*S Constructors
*/
/*F BasicMolecule() . . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BasicMolecule::BasicMolecule() 
: Identify()
     {
     }
  
/*F BasicMolecule(mol)  . . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    mol: to be copied
**
**  REMARKS
**
*/
BasicMolecule::BasicMolecule(const BasicMolecule& mol) 
: Identify(mol)
     {
     }
 
/*F MolFileMolecule() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
MolFileMolecule::MolFileMolecule()
	  {
	  }
 
/*F MolFileMolecule(mol)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    mol: The molecule
**
**  REMARKS
**
*/
MolFileMolecule::MolFileMolecule(const MolFileMolecule& mol) 
: BasicMolecule(mol),
Atoms(mol.Atoms),
Bonds(mol.Bonds)
     {
     }
/*F SimpleMolecule()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
SimpleMolecule::SimpleMolecule()
     {
     InitializeProperties();
     }
/*F SimpleMolecule(mol) . . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    mol: The molecule
**
**  REMARKS
**
*/     
SimpleMolecule::SimpleMolecule(const SimpleMolecule& mol)
  : BasicMolecule(mol),
    Bonds(mol.Bonds), 
    Atoms(mol.Atoms),
    Properties(mol.Properties)
     {       
     }
