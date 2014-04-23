/*  FILE     MolAtom.cc
**  PACKAGE  MolAtom
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "MolAtom" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIAION
#include "Basis/System.hh"
#include "Reaction/StaticAtom.hh"
#include "Reaction/MolAtom.hh"
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
//template void allocate(int, SimpleBondAtom *);

/*S MolFileAtom
 */
/*F CloneCopy(atom) . . . . . . . . . . . . . . . . . . . . . . . MolFileAtom
**
**  DESCRIPTION
**
**  REMARKS
**
*/
void MolFileAtom::CloneCopy(MolFileAtom *atom)
{
  *this = *atom;
}
/*F  atom = Clone() . . . . . . . . . . . . . . . . . . . . . . . MolFileAtom
**
**  DESCRIPTION
**    atom: 
**  REMARKS
**
*/
MolFileAtom *MolFileAtom::Clone()
{
  MolFileAtom *atom = new MolFileAtom;
  atom->CloneCopy(this);
  return atom;
}
/*F out = print(out)  . . . . . . . . . . . . . . . . . . . . . . MolFileAtom
**
**  DESCRIPTION
**    out: The output stream
**
**  REMARKS
**
*/
ostream& MolFileAtom::print(ostream& out) const 
{
  out << (SimpleAtom) *this;
  out << "  ";
  Coordinates.print(out);
  return out;
} 
/*F EncodeThis(buffer)  . . . . . . . . . . . . . . . . . . . . . MolFileAtom
**
**  DESCRIPTION
**     buffer: the buffer
**     ans: true if successful
**
**  REMARKS
**
*/
bool MolFileAtom::EncodeThis(CommBuffer& buffer)
{
  bool result = SimpleAtom::EncodeThis(buffer);
  result = result && Coordinates.EncodeThis(buffer);
  result = result && Parameters.EncodeThis(buffer);
  return result;
}
/*F DecodeThis(buffer)  . . . . . . . . . . . . . . . . . . . . . MolFileAtom
**
**  DESCRIPTION
**     buffer: the buffer
**     ans: true if successful
**
**  REMARKS
**
*/
bool MolFileAtom::DecodeThis(CommBuffer& buffer)
{
  bool result = SimpleAtom::DecodeThis(buffer);
  result = result && Coordinates.DecodeThis(buffer);
  result = result && Parameters.DecodeThis(buffer);
  return result;
}

/*S MolFileAtom
*/
/*F ReadMolFileAtom(file,atominfo)  . . . . . . . . . . . . . ReadMolFileAtom
**
**  DESCRIPTION
**     file: The input stream
**     atominfo: The atom information
**
**  REMARKS
**
*/
ReadMolFileAtom::ReadMolFileAtom(istream& file,
				 AtomInformation atominfo)
  : File(file), Atominfo(atominfo)
{
  count = 0;
}
/*F atom = operator()() . . . . . . . . . . . . . . . . . . . ReadMolFileAtom
**
**  DESCRIPTION
**
**  REMARKS
**
*/
MolFileAtom ReadMolFileAtom::operator() ()
{
  MolFileAtom atom;
  
  atom.ReadMFAtom(File,Atominfo);
  atom.Identification = count;
  count++;
  return(atom);
}


/*S SimpleBondAtom()
*/
/*S Calculations
*/
/*F FillInValenceAtom() . . . . . . . . . . . . calculate simple valence info
**
**  DESCRIPTION
**    From the Simple atom information and the bond counts,
**    fill in the simple valence information
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
void SimpleElectronicAtom::CalculateSimpleElectronic()
{
  CalcNumberOfBonds();
  CalcNumberOfElectrons();
  CalcAtomGroupFromElectrons();
  CountLonePairsOfAtom();
  SingleBondsNotSpecified();
     
  CalcShells();
  CalcScreening();
  CalcEffectiveCharge();
  CalcElectronegativity();

  Aromatic = 0;
}
/*F n = CalcNumberOfBonds() . . . . . . . . . . . . . . . . . number of bonds
**
**  DESCRIPTION
**    n: The number of bonds (NumberOfBonds)
**
**    Adds up the single, double and triple bonds. Double and Triple
**    bonds count as two and three bonds, respectively.
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
int SimpleElectronicAtom::CalcNumberOfBonds()
{
  int bnds;
     
  bnds = (SingleBondCount +
          2*DoubleBondCount +
          3*TripleBondCount);
  NumberOfBonds = bnds;
  return(bnds);
}
/*F electrons = CalcNumberOfElectrons(atomicnumber,charge)  . . . . electrons
**
**  DESCRIPTION
**    atomicnumber: Input or AtomicNumber from class
**    charge: Input or Charge from class
**    electrons: atomicnumber-charge;
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
double SimpleElectronicAtom::CalcNumberOfElectrons(int atomicnumber, int charge)
{
  int atmnum,chrg;
     
  atmnum  = atomicnumber ? atomicnumber : AtomicNumber;
  chrg    = charge ? charge : (int) Charge;
     
  NumberOfElectrons = atmnum - charge;
  return(NumberOfElectrons);
}
/*F group = CalcAtomGroupFromElectrons(electrons) . . . . . . . . . . . group
**
**  DESCRIPTION
**    electrons: The number of electrons of the atom
**    group: The group of the atom
**
**    The group number is found by finding which row in the
**    periodic table the element is and determining the
**    remaining electrons
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
int SimpleElectronicAtom::CalcAtomGroupFromElectrons(double eles)
{
  int group,nremaining;
  double felectrons;
  int electrons;
     
  felectrons = eles ? eles : NumberOfElectrons;
  electrons = (int) floor(felectrons + 0.5);
     
  if(electrons <= 2)
    {
      if(electrons == 1)
        group = 1;
      else
        group = 0;
    }
  else if(electrons <= 10)
    {
      group = electrons - 2;
    }
  else if(electrons <= 18)
    {
      group = electrons - 10;
    }
  else
    {
      if(electrons <= 36)
        nremaining = electrons - 18;
      else if(electrons <= 54)
        nremaining = electrons - 36;
      else
        nremaining = electrons - 54;
          
      if(nremaining <= 8)
        {
          group = nremaining;
        }
      else if(nremaining <= 10)
        {
          group = 8;
        }
      else 
        {
          group = nremaining - 10;
        }
    }
  if(group == 9)
    group = 0;
  Group = group;
  return(group);
}

 

 
/*F count = CountLonePairsOfAtom(group,numbonds)  . . . . . . . .  lone pairs
**
**  DESCRIPTION
**    group: The group in the periodic table (input or from Class)
**    numbonds: The number of bonds connected to the atom (input or from Class)
**    count: The number of lone pairs (LonePairs)
**
**   The result is the number of lone pairs. This routine
**   is guarenteed for the main group elements, but the 
**   transistion elements could present problems with
**   such a simple algorithm.
**
**   Basically, it determines the number of remaining electrons
**   (group - numbonds) and for the main group elements (group > 4)
**   The number of lone pairs is the number of pairs of electrons
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
int SimpleElectronicAtom::CountLonePairsOfAtom(int g,int bnds)
{
  int remaining,lonepair;
  int numbonds,group;
     
  group = g ? g : Group;
  numbonds = bnds ? bnds : NumberOfBonds;

  lonepair = 0;
  remaining = group - numbonds - Radical;
  if(remaining < 0)
    remaining += 10;
  switch(remaining)
    {
    case 0:
    case 1:
      lonepair = 0;
      break;
    case 2:
    case 3:
      if(group > 4)
        lonepair = 1;
      else 
        lonepair = 0;
      break;
    case 4:
    case 5:
      if(group > 4)
        lonepair = 2;
      else 
        lonepair = 0;
      break;
    case 6:
    case 7:
      if(group > 4)
        lonepair = 3;
      else 
        lonepair = 0;
      break;
    }
  LonePairs = lonepair;
     
  return(lonepair);
}
     
 
/*F r = SingleBondsNotSpecified(group,lonepairs,numbonds) . . . .  rest bonds
**
**  DESCRIPTION
**    group:       The group in the periodic table
**    lonepairs:   The number of lone pairs
**    numbonds:    The number of bonds
**    r:           The remaining bonds
**
**    The result is the number of bonds implicit in the description.
**    This is used to determine the number of hydrogens are to
**    be added to a molecule.
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
int SimpleElectronicAtom::SingleBondsNotSpecified(int g, int lp, int bnds, int rad)
{
  int count;
  int group,lonepair,numbonds,radical;
     
  group = g ? g : Group;
  lonepair = lp ? lp : LonePairs;
  numbonds = bnds ? bnds : CalcNumberOfBonds();
  radical  = rad ? rad : Radical;
     
  count = group - 2*lonepair - numbonds - radical;
  if(count < 0)
    count += 10;

  HydrogenCount = count;
     
  return(count);
}
 
/*F CalcShells(es)  . . . . . . . . . . . . . . . . calculate shell structure
**
**  DESCRIPTION
**    electrons: The number of electrons (input or NumberOfElectrons)
**
**    Given the number of electrons, the occupancy of the shells
**    is determined and put into Shells in the class
**
**    The output is a vector.  The size of the vector is the number
**    of shells occupied. Each INDEX in the vector is the occupancy of
**    that shell
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
void SimpleElectronicAtom::CalcShells(double es)
{
  double nremaining,electrons;

  electrons = es ? es : NumberOfElectrons;
     
  Shells.erase(Shells.begin(),Shells.end());
     
  if(electrons <= 2)
    {
      Shells.push_back(electrons);
    }
  else if(electrons <= 10)
    {
      Shells.push_back(2);
      Shells.push_back(electrons - 2);
    }
  else if(electrons <= 18)
    {
      Shells.push_back(2);
      Shells.push_back(8);
      Shells.push_back(electrons - 10);
    }
  else if(electrons <= 36)
    {
      Shells.push_back(2);
      Shells.push_back(8);
      Shells.push_back(8);
      nremaining = electrons - 18;
      if(nremaining <= 2)
        {
          Shells.push_back(0);
          Shells.push_back(nremaining);
        }
      else if(nremaining <= 5)
        {
          Shells.push_back(nremaining - 2);
          Shells.push_back(2);
        }
      else if(nremaining <= 7)
        {
          Shells.push_back(5);
          Shells.push_back(nremaining - 5);
        }
      else if(nremaining <= 10)
        {
          Shells.push_back(nremaining - 2);
          Shells.push_back(2);
        }           
      else 
        {
          Shells.push_back(10);
          Shells.push_back(nremaining - 10);
        }
    }
  else
    {
      Shells.push_back(2);
      Shells.push_back(8);
      Shells.push_back(8);
      Shells.push_back(10);
      Shells.push_back(8);
      nremaining = electrons - 36;
      if(nremaining <= 2)
        {
          Shells.push_back(0);
          Shells.push_back(nremaining);
        }
      else if(nremaining <= 4)
        {
          Shells.push_back(nremaining - 2);
          Shells.push_back(2);
        }
      else if(nremaining <= 9)
        {
          Shells.push_back(nremaining - 1);
          Shells.push_back(1);
        }
      else 
        {
          Shells.push_back(10);
          Shells.push_back(nremaining - 10);
        }
    }
}
 
/*F screening = CalcScreening() . . . . . . . . . . . . . .  screening factor
**
**  DESCRIPTION
**    screening: The electronic screening of the nucleus.
**
**    This is a simple calculation using the shell structure
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
double SimpleElectronicAtom::CalcScreening()
{
  int occupied,occupiedm1,occupiedm2,i,ip1;
  double screening,outer;
     
  occupied = Shells.size();
     
  occupiedm1 = occupied - 1;
  occupiedm2 = occupiedm1 - 1;
     
  screening = 0.0;

  if(occupied == 1)
    outer = 0.30;
  else 
    {
      outer = 0.35;
      for(i=0;i<occupiedm1;i++)
        {
          ip1 = i + 1;
          if( (ip1 == occupiedm1)           ||
              ( (ip1 == occupiedm2) &&
                (occupied >= 5) )
              )
            screening += Shells[i]*0.85;
          else 
            screening += Shells[i];
        }
    }
  Screening = screening + outer*(Shells[occupied-1] - 1);
  return(Screening);
}
 
/*F zeff = CalcEffectiveCharge(screening) . . . . . . . . . . . . . .  charge
**
**  DESCRIPTION
**    screening: The screening factor
**    zeff: The effective charge of the atom
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
double SimpleElectronicAtom::CalcEffectiveCharge(double screening)
{
  double screen,sigma,outer;
     
  screen = screening ? screening : Screening;

  if(Shells.size() == 1)
    outer = 0.30;
  else
    outer = 0.35;
     
  sigma      = screen + outer;
  ZEff      = AtomicNumber - sigma;
  return(ZEff);
}

     


 
/*F electroneg = CalcElectronegativity(atnum,cradius,scrn,eles) .  electroneg
**
**  DESCRIPTION
**    atnum: The atomic number 
**    cradius: The covalent radius
**    scrn: The screening factor
**    eles: The number of electrons
**    electroneg: The calculated electronegativity
**
**    Either all are taken from input or all from the SimpleElectronAtom class
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
double SimpleElectronicAtom::CalcElectronegativity(int atnum,
                                                   double cradius, 
                                                   double scrn,
                                                   double eles)
{
  int noccupied;
  double potential;
     
  if(!atnum)
    {
      atnum = AtomicNumber;
      cradius = CovalentRadius;
      scrn = Screening;
      eles = NumberOfElectrons;
    }
     
  noccupied = Shells.size();
     
  if(eles < 1)
    potential = atnum / cradius;
  else
    potential = (atnum - scrn)/cradius;

  if(noccupied == 1)
    AtomElectronegativity = .5 * potential + 0.64;
  else if(noccupied == 2)
    AtomElectronegativity = .478 * potential + 0.5;
  else if(noccupied == 3)
    AtomElectronegativity = .44 * potential + 0.28;
  else if(noccupied == 5)
    AtomElectronegativity = .42 * potential - 0.07;
  else
    AtomElectronegativity = .46 * potential - 0.12;
  return AtomElectronegativity;
}

/*S Boolean
*/
 
/*F ans = operator==(x,y) . . . . . . . . . . . . . . . . . . . .  SimpleAtom
**
**  DESCRIPTION
**    x,y: The atom
**    ans: true if AtomicNumber, Charge and Radical are the same
**
**  REMARKS
**
*/
bool operator==(const SimpleAtom& x,
                const SimpleAtom& y)
{
  return 
    (x.AtomicNumber == y.AtomicNumber) &&
    (x.Charge == y.Charge) &&
    (x.Radical == y.Radical);
}

 
/*F ans = operator!=(x,y) . . . . . . . . . . . . . . . . . . . .  SimpleAtom
**
**  DESCRIPTION
**    x,y: The atom
**    ans: true if not equal
**
**  REMARKS
**
*/
bool operator!=(const SimpleAtom& x,
                const SimpleAtom& y)
{
  return !(x == y);
}
 
/*F ans = operator<(x,y)  . . . . . . . . . . . . . . . . . . . .  SimpleAtom
**
**  DESCRIPTION
**    x,y: The atoms
**    ans: AtomicNumber, Charge and Radical are successively check to see if 
**         they are less.  One exception is the AtomicNumber 6 (Carbon) is 
**         considered less than all other atomic numbers (prefering hydrocarbons)
**
**  REMARKS
**
*/
bool operator<(const SimpleAtom& x,
               const SimpleAtom& y)
{
  bool ans;
  if(x.AtomicNumber == y.AtomicNumber)
    {
      if(x.Charge == y.Charge)
        ans = (x.Radical < y.Radical);
      else
        ans = fabs(x.Charge) < fabs(y.Charge);
    }
  else
    {
      if(x.AtomicNumber == 6)
        ans = true;
      else if(y.AtomicNumber == 6)
        ans = false;
      else 
        ans = x.AtomicNumber < y.AtomicNumber;
    }
  return ans;
}

/*S IO
*/
/*F newout = operator<<(out,atom) . . . . . . . . . . . . . . . .  SimpleAtom
**
**  DESCRIPTION
**    out: The output stream
**    atom: The SimpleAtom class
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SimpleAtom& atom)
{
  return atom.print(out);
}
/*F newout = operator<<(out,coord)  . . . . . . . . . . . . . . Coordinates3D
**
**  DESCRIPTION
**    out: The output stream
**    coord: The 3D coordinates
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const Coordinates3D& coord)
{
  return coord.print(out);
}
 
/*F newout = operator<<(out,atom) . . . . . . . . . . . . . . . . MolFileAtom
**
**  DESCRIPTION
**    out: The output stream
**    atom: The MolFile information for the atom
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const MolFileAtom& atom)
{
  return atom.print(out);
}
/*F newout = operator<<(out,atom) . . . . . . . . . . . . . .  SimpleBondAtom
**
**  DESCRIPTION
**    out: The output stream
**    atom: The single, double and triple counts
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SimpleBondAtom& atom)
{
  return atom.print(out);
}
         
/*F newout = operator<<(out,atom) . . . . . . . . . . .  SimpleElectronicAtom
**
**  DESCRIPTION
**    out: The output stream
**    atom: The atom information
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SimpleElectronicAtom& atom)
{
  return atom.print(out);
}
/*F newout = operator<<(out,atom) . . . . . . . . . . . . . .MoleculeAtom
**
**  DESCRIPTION
**    out: The output stream
**    atom: The atom information
**    newout: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const MoleculeAtom& atom)
{
  return atom.print(out);
}

/*S Read in atom
*/
/*F status = ReadMFAtom(file,atominfo)  . . . . . . . . . . . . read one line
**
**  DESCRIPTION
**    file: The input stream
**    atominfo: The additional information needed to interpret the atom info
**    status: The success of reading in the line
**
**    The Atom information is read in using the one line format of the
**    MolFile information.  The AtomInformation is used to convert the
**    string name of the atom to an atomic number
**
**  REMARKS
**    The input stream is given as a pointer (should be changed to 
**    dereferenced) because the information is modified
**
*/
int MolFileAtom::ReadMFAtom(istream& file, 
                            AtomInformation& atominfo)
{
  String line, sym;
  INDEX chargecode;
     
  line.ReadFullLine(file);

#ifdef DEBUG_ATOMBOND_ITC
  cout << "\nMolFileAtom   ";
#endif
     
  Coordinates.X = line.ToFloat(0, 9);
  Coordinates.Y = line.ToFloat(10,19);
  Coordinates.Z = line.ToFloat(20,29);
     
  sym = line.Isolate(31,32);
  NameTag = *(new String(sym));

#ifdef DEBUG_ATOMBOND_ITC
  cout << "Name Tag-" << NameTag << "-  ";
#endif
     
     
  chargecode = (INDEX) line.ToInteger(37,39);
  ChargeAndRadicalFromMolFileSpec(chargecode);

  AtomicNumber = atominfo.AtomicNumberFromSymbol(sym);
     
  if(AtomicNumber == -1)
    AtomicNumber = atominfo.FindMetaAtomSymbol(sym);

  if(AtomicNumber == -1)
    {
      cout << "Atomic Symbol Not Found: " << sym << endl;
      return(SYSTEM_ERROR_RETURN);
    }
     
  //     Parameters = ReadIntegerParameters(line+33,NUMBER_ATOM_PARAMETERS);

#ifdef DEBUG_ATOMBOND_ITC
  cout << "AtomicNumber=" << AtomicNumber << " Charge=" << Charge << "\n";
#endif
     
  return(SYSTEM_NORMAL_RETURN);
}                     


 
/*F ChargeAndRadicalFromMolFileSpec(charge) . . .  Fill in charge and radical
**
**  DESCRIPTION
**    charge:  The MolFile code for the charge and radical
**
**    The codes are:
**       - 0  Charge=0 Radical=0
**       - 1  Charge=3 Radical=0
**       - 2  Charge=2 Radical=0
**       - 3  Charge=1 Radical=0
**       - 4  Charge=0 Radical=1
**       - 5  Charge=-1 Radical=0
**       - 6  Charge=-2 Radical=0
**       - 7  Charge=-3 Radical=0
**
**  REMARKS
**
*/
void MolFileAtom::ChargeAndRadicalFromMolFileSpec(const INDEX charge)
{
  double chrg;
  int rad;
     
  switch(charge)
    {
    case 0:
      chrg = 0.0;
      rad = 0;
      break;
    case 1:
      chrg = 3.0;
      rad = 0;
      break;
    case 2:
      chrg = 2.0;
      rad = 0;
      break;
    case 3:
      chrg = 1.0;
      rad = 0;
      break;
    case 4:
      chrg = 0.0;
      rad = 1;
      break;
    case 5:
      chrg = -1.0;
      rad = 0;
      break;
    case 6:
      chrg = -2.0;
      rad = 0;
      break;
    case 7:
      chrg = -3.0;
      rad = 0;
      break;
    default:
      chrg = 0.0;
      rad = 0;
    }
  Charge = chrg;
  Radical = rad;
}

/*S EncodeDecode
 */
/*F ans = Encode(buffer,atom) . . . . . . . . . . . . . . . . . .  SimpleAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, SimpleAtom& atom)
{
  return atom.EncodeThis(buffer);
}
/*F ans = Decode(buffer,atom) . . . . . . . . . . . . . . . . . .  SimpleAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, SimpleAtom& atom)
{
  return atom.DecodeThis(buffer);
}
/*F ans = Encode(buffer,atom) . . . . . . . . . . . . . . . . . Coordinates3D
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, Coordinates3D& atom)
{
  return atom.EncodeThis(buffer);
}
/*F ans = Decode(buffer,atom) . . . . . . . . . . . . . . . . . Coordinates3D
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, Coordinates3D& atom)
{
  return atom.DecodeThis(buffer);
}
/*F ans = Encode(buffer,atom) . . . . . . . . . . . . . . . . . . MolFileAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, MolFileAtom& atom)
{
  return atom.EncodeThis(buffer);
}
/*F ans = Decode(buffer,atom) . . . . . . . . . . . . . . . . . . MolFileAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, MolFileAtom& atom)
{
  return atom.DecodeThis(buffer);
}
/*F ans = Encode(buffer,atom) . . . . . . . . . . . . . . . .  SimpleBondAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, SimpleBondAtom& atom)
{
  return atom.EncodeThis(buffer);
}
/*F ans = Decode(buffer,atom) . . . . . . . . . . . . . . . . .SimpleBondAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, SimpleBondAtom& atom)
{
  return atom.DecodeThis(buffer);
}
/*F ans = Encode(buffer,atom) . . . . . . . . . . . . .  SimpleElectronicAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, SimpleElectronicAtom& atom)
{
  return atom.EncodeThis(buffer);
}
/*F ans = Decode(buffer,atom) . . . . . . . . . . . . . .SimpleElectronicAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, SimpleElectronicAtom& atom)
{
  return atom.DecodeThis(buffer);
}
/*F ans = Encode(buffer,atom) . . . . . . . . . . . . . . . . .  MoleculeAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, MoleculeAtom& atom)
{
  return atom.EncodeThis(buffer);
}
/*F ans = Decode(buffer,atom) . . . . . . . . . . . . . . . . .  MoleculeAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    atom: The atom info
**    ans: true if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, MoleculeAtom& atom)
{
  return atom.DecodeThis(buffer);
}
