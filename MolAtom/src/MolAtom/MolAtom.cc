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
#include "CoreDataObjects.hh"
#include "Vector.hh"
#include "NumericObjects.hh"
#include "LogicalObjects.hh"
#include "FunctionReal1DObjects.hh"
#include "OperationObjects.hh"
#include "LogicalOperations.hh"
#include "DataObjects.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"

/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

/*S RxnDataBasicAtomData
 */ 
/*F RxnDataBasicAtomData()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataBasicAtomData::RxnDataBasicAtomData()
  : AtomicNumber(0),
    Charge(0.0),
    Radical(0),
    X(0.0),
    Y(0.0),
    Z(0.0),
    SingleBondCount(0),
    DoubleBondCount(0),
    TripleBondCount(0),
    Valence(0),
    NumberOfBonds(0),
    NumberOfElectrons(0.0),
    Group(0),
    LonePairs(0),
    HydrogenCount(0),
    CovalentRadius(0.0),
    Screening(0.0),
    ZEff(0.0),
    AtomElectronegativity(0.0),
    Aromatic(0)
{
  Identification = MOLATOM_BASIC_ID;
  NameTag = MOLATOM_BASIC_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataBasicAtomData(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataBasicAtomData::RxnDataBasicAtomData(const RxnDataBasicAtomData& data)
  : BaseDataObject(data),
    AtomicNumber(data.AtomicNumber),
    Charge(data.Charge),
    Radical(data.Radical),
    X(data.X),
    Y(data.Y),
    Z(data.Z),
    SingleBondCount(data.SingleBondCount),
    DoubleBondCount(data.DoubleBondCount),
    TripleBondCount(data.TripleBondCount),
    Valence(data.Valence),
    NumberOfBonds(data.NumberOfBonds),
    NumberOfElectrons(data.NumberOfElectrons),
    Group(data.Group),
    LonePairs(data.LonePairs),
    HydrogenCount(data.HydrogenCount),
    CovalentRadius(data.CovalentRadius),
    Screening(data.Screening),
    ZEff(data.ZEff),
    AtomElectronegativity(data.AtomElectronegativity),
    Aromatic(data.Aromatic)
{
  vector<double>::const_iterator i;
  for(i = data.Shells.begin();
      i != data.Shells.end();i++)
    {
      Shells.push_back(*i);
    }
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataBasicAtomData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataBasicAtomData::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataBasicAtomData
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataBasicAtomData::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataObject::Read(in,objc,name);
  StreamObjectInput str(in,' ');

  String AtomicNumberS = str.ReadNext();
  AtomicNumber = AtomicNumberS.ToInteger();
  String ChargeS = str.ReadNext();
  Charge = ChargeS.ToFloat();
  String RadicalS = str.ReadNext();
  Radical = RadicalS.ToInteger();
  String XS = str.ReadNext();
  X = XS.ToFloat();
  String YS = str.ReadNext();
  Y = YS.ToFloat();
  String ZS = str.ReadNext();
  Z = ZS.ToFloat();
  String SingleBondCountS = str.ReadNext();
  SingleBondCount = SingleBondCountS.ToInteger();
  String DoubleBondCountS = str.ReadNext();
  DoubleBondCount = DoubleBondCountS.ToInteger();
  String TripleBondCountS = str.ReadNext();
  TripleBondCount = TripleBondCountS.ToInteger();
  String NumberOfBondsS = str.ReadNext();
  NumberOfBonds = NumberOfBondsS.ToInteger();
  String NumberOfElectronsS = str.ReadNext();
  NumberOfElectrons = NumberOfElectronsS.ToFloat();
  String GroupS = str.ReadNext();
  Group = GroupS.ToInteger();
  String LonePairsS = str.ReadNext();
  LonePairs = LonePairsS.ToInteger();
  String HydrogenCountS = str.ReadNext();
  HydrogenCount = HydrogenCountS.ToInteger();
  String CovalentRadiusS = str.ReadNext();
  CovalentRadius = CovalentRadiusS.ToFloat();
  String ScreeningS = str.ReadNext();
  Screening = ScreeningS.ToFloat();
  String ZEffS = str.ReadNext();
  ZEff = ZEffS.ToFloat();
  String AtomElectronegativityS = str.ReadNext();
  AtomElectronegativity = AtomElectronegativityS.ToFloat();
  String AromaticS = str.ReadNext();
  Aromatic = AromaticS.ToInteger();

  String word; 
  unsigned int dim = word.ToInteger(); 
  double currentVal;
  for (unsigned int i=0; i<dim; i++) 
    { 
      word = str.ReadNext(); 
      currentVal = word.ToFloat(); 
      Shells.push_back(currentVal);
    } 
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataBasicAtomData
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataBasicAtomData::print(ostream& out) const
{
  BaseDataObject::print(out);

  out << "AtomicNumber: " << AtomicNumber << "  ";
  out << "Charge: " << Charge << "  ";
  out << "Radical: " << Radical << "  ";
  
  out << "[" << X << "," << Y << "," << Z << "]";
  out << endl;
  out << "Bonding[";
  if(SingleBondCount > 0)
    out << "Sing(" << SingleBondCount << ") ";
  if(DoubleBondCount > 0)
    out << "Doub(" << DoubleBondCount << ") ";
  if(TripleBondCount > 0)
    out << "Trip(" << TripleBondCount << ") ";
  out << "]";
  
  out << " H(" << HydrogenCount << "),";
  out << "G(" << Group << "),";
  out << "E(" << NumberOfElectrons << "),";
  out << "Bonds(" << NumberOfBonds << ")";
  
  if(LonePairs != 0)
    out << ",LP(" << LonePairs << ")";
  if(Aromatic != 0)
    out << ",Ar(" << Aromatic << ")";
  out << " CovalentRadius=" << CovalentRadius;
  out << " Screening=" << Screening;
  out << " Electro=" << AtomElectronegativity;
  out << " ZEff=" << ZEff;
  out << " Shells=[";
  ostream_iterator<double> shell (out," ");
  copy(Shells.begin(),Shells.end(),shell);
  out << "]";
  out << endl;
  
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataBasicAtomData
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataBasicAtomData::Clone()
{
  RxnDataBasicAtomData *obj = new RxnDataBasicAtomData(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataBasicAtomData
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataBasicAtomData::CopyClone(Identify * obj)
{
  RxnDataBasicAtomData *objfull = (RxnDataBasicAtomData *) obj;
  *this = *objfull;
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataBasicAtomData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBasicAtomData::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::EncodeThis(buffer);
  result = result && Encode(buffer,AtomicNumber);
  result = result && Encode(buffer,Charge);
  result = result && Encode(buffer,Radical);
  result = result && Encode(buffer,X);
  result = result && Encode(buffer,Y);
  result = result && Encode(buffer,Z);
  result = result && Encode(buffer,SingleBondCount);
  result = result && Encode(buffer,DoubleBondCount);
  result = result && Encode(buffer,TripleBondCount);
  result = result && Encode(buffer,Valence);
  result = result && Encode(buffer,NumberOfBonds);
  result = result && Encode(buffer,NumberOfElectrons);
  result = result && Encode(buffer,Group);
  result = result && Encode(buffer,LonePairs);
  result = result && Encode(buffer,HydrogenCount);
  result = result && Encode(buffer,CovalentRadius);
  result = result && Encode(buffer,Screening);
  result = result && Encode(buffer,ZEff);
  result = result && Encode(buffer,AtomElectronegativity);
  int ar = Aromatic;
  result = result && Encode(buffer,ar);
  int ss = Shells.size();
  result = result && Encode(buffer,ss);
  vector<double>::iterator i;
  for(i=Shells.begin();i<Shells.end();i++)
    {
      result = result && Encode(buffer,*i);
    }
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataBasicAtomData
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBasicAtomData::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataObject::DecodeThis(buffer);
  result = result && Decode(buffer,AtomicNumber);
  result = result && Decode(buffer,Charge);
  result = result && Decode(buffer,Radical);
  result = result && Decode(buffer,X);
  result = result && Decode(buffer,Y);
  result = result && Decode(buffer,Z);
  result = result && Decode(buffer,SingleBondCount);
  result = result && Decode(buffer,DoubleBondCount);
  result = result && Decode(buffer,TripleBondCount);
  result = result && Decode(buffer,Valence);
  result = result && Decode(buffer,NumberOfBonds);
  result = result && Decode(buffer,NumberOfElectrons);
  result = result && Decode(buffer,Group);
  result = result && Decode(buffer,LonePairs);
  result = result && Decode(buffer,HydrogenCount);
  result = result && Decode(buffer,CovalentRadius);
  result = result && Decode(buffer,Screening);
  result = result && Decode(buffer,ZEff);
  result = result && Decode(buffer,AtomElectronegativity);
  int aromatic;
  result = result && Decode(buffer, aromatic);
  Aromatic = aromatic;
  int siz;
  result = result && Decode(buffer,siz);
  double shell;
  for(int i=0;i < siz;i++)
    {
      result = result && Decode(buffer,shell);
      Shells.push_back(shell);
    }
  return result;
}
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
void RxnDataBasicAtomData::CalculateSimpleElectronic()
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

  CalculateStandardValence();

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
int RxnDataBasicAtomData::CalcNumberOfBonds()
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
double RxnDataBasicAtomData::CalcNumberOfElectrons(int atomicnumber, int charge)
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
int RxnDataBasicAtomData::CalcAtomGroupFromElectrons(double eles)
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
int RxnDataBasicAtomData::CountLonePairsOfAtom(int g,int bnds)
{
  int remaining,lonepair;
  int numbonds,group;
     
  group = g ? g : Group;
  numbonds = bnds ? bnds : NumberOfBonds;
  remaining = group - numbonds - LonePairs*2;

  lonepair = 0;
  if(Radical != 0)
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
  LonePairs += lonepair;
     
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
int RxnDataBasicAtomData::SingleBondsNotSpecified(int g, int lp, int bnds, int rad)
{
  int count;
  int group,lonepair,numbonds;
     
  group = g ? g : Group;
  lonepair = lp ? lp : LonePairs;
  numbonds = bnds ? bnds : CalcNumberOfBonds();

  int rad1 = 0;
  if(rad != 0 || Radical != 0) 
    rad1 = Radical;
     
  count = group - 2*lonepair - numbonds - rad1;
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
void RxnDataBasicAtomData::CalcShells(double es)
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
double RxnDataBasicAtomData::CalcScreening()
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
double RxnDataBasicAtomData::CalcEffectiveCharge(double screening)
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
double RxnDataBasicAtomData::CalcElectronegativity(int atnum,
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
 
/*F val = CalculateStandardValence() standard valence value
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
unsigned int RxnDataBasicAtomData::CalculateStandardValence()
{
  int atm = AtomicNumber;
  int singles = SingleBondCount;
  int doubles = DoubleBondCount;
  int triples = TripleBondCount;
  int charge = 0;
  if(Charge != 0)
    charge = ((int) Charge) + 4;
  else if(Radical != 0)
    charge = 4;
  int aromatic = (int) Aromatic;
  int lonepairs = LonePairs;
  int hydrogens = HydrogenCount;
  
  Valence = singles * VAL_SINGLE_BOND_COUNT
    + doubles * VAL_DOUBLE_BOND_COUNT
    + triples * VAL_TRIPLE_BOND_COUNT
    + charge * VAL_CHARGE_SPEC
    + aromatic * VAL_AROMATIC_SPEC
    + lonepairs * VAL_LONE_PAIR_COUNT
    + hydrogens * VAL_HYDROGEN_COUNT 
    + atm * VAL_ATOMIC_NUMBER;

  return Valence;
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
bool RxnDataBasicAtomData::operator<(const RxnDataBasicAtomData& x)
{
  bool ans;
  if(x.AtomicNumber == AtomicNumber)
    {
        ans = NameTag < x.NameTag;
    }
  else
    {
      if(AtomicNumber == 6)
        ans = false;
      else if(x.AtomicNumber == 6)
        ans = true;
      else 
        ans = AtomicNumber < x.AtomicNumber;
    }
  return ans;
}
 
/*F IncrementBondOrder(bondorder) . . . . . . . . . . .  RxnDataBasicAtomData
**
**  DESCRIPTION
**    bondorder: 1, 2 or 3 for Single, Double or Triple bonds
**
**    The SingleBondCount, DoubleBondCount and TripleBondCount are incremented
**    appropriately
**
**  REMARKS
**
*/
void RxnDataBasicAtomData::IncrementBondOrder(unsigned int bondorder)
{
  if(bondorder == 1)
    {
      SingleBondCount += 1;
    }
  else if(bondorder == 2)
    {
      DoubleBondCount += 1;
    }
  else if(bondorder == 3)
    {
      TripleBondCount += 1;
    }
}
/*F ans = WriteAsLine(out,objc) . . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object one one line
**
**  REMARKS
**
*/
bool RxnDataBasicAtomData::WriteAsLine(ostream& out, DataObjectClass *objc)
{
  out << "Atn: " << AtomicNumber << "  ";
  out << "Chrg: " << Charge << "  ";
  out << "Rad: " << Radical << "  ";
  out << "Bonding[";
  if(SingleBondCount > 0)
    out << "S(" << SingleBondCount << ") ";
  if(DoubleBondCount > 0)
    out << "D(" << DoubleBondCount << ") ";
  if(TripleBondCount > 0)
    out << "T(" << TripleBondCount << ") ";
  out << "] ";
  
  out << "H(" << HydrogenCount << "),";
  out << "G(" << Group << "),";
  out << "E(" << NumberOfElectrons << ")";
  out << " Shells=[";
  ostream_iterator<double> shell (out," ");
  copy(Shells.begin(),Shells.end(),shell);
  out << "]";

  return true;
}
/*F ans = WriteAsASCII(out,objc)  . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object as complement to input
**
**  REMARKS
**
*/
bool RxnDataBasicAtomData::WriteAsASCII(ostream &out, DataObjectClass*)
{
  out << " " << AtomicNumber;
  out << " " << Charge;
  out << " " << Radical;

  out << "      " << X;
  out << " " << Y;
  out << " " << Z;

  out << "      " << SingleBondCount;
  out << " " << DoubleBondCount;
  out << " " << TripleBondCount;
  out << endl;

  out << " " << Valence;
     
  out << " " << NumberOfBonds;
  out << " " << NumberOfElectrons;
  out << " " << Group;
  out << " " << LonePairs;
  out << " " << HydrogenCount;
  out << " " << CovalentRadius;
  out << endl;

  out << " " << Screening;
  out << " " << ZEff;
  out << " " << AtomElectronegativity;
  out << " " << Aromatic;
  out << endl;

  out << Shells.size() << " ";
  ostream_iterator<double> shell (out," ");
  copy(Shells.begin(),Shells.end(),shell);
  out << endl;

  return true;
}
/*F ans = WriteAsLatex(out,objc)  . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object prepared for latex
**
**  REMARKS
**
*/
bool RxnDataBasicAtomData::WriteAsLatex(ostream& out, DataObjectClass* objc)
{
  out << NameTag << " \\> ";
  out << AtomicNumber << " \\> ";
  out << Valence << "\\> ";
  out << Charge << " \\> ";
  if(Radical == 0)
    out << " - \\> ";
  else
    out << "R \\> ";
  out << "[";
  if(SingleBondCount > 0)
    out << "S(" << SingleBondCount << ") ";
  if(DoubleBondCount > 0)
    out << "D(" << DoubleBondCount << ") ";
  if(TripleBondCount > 0)
    out << "T(" << TripleBondCount << ") ";
  out << "] \\> ";
  
  out << HydrogenCount << "\\> ";
  out << Group << "\\> ";
  out << NumberOfElectrons << "\\> ";
  out << "[";
  ostream_iterator<double> shell (out," ");
  copy(Shells.begin(),Shells.end(),shell);
  out << "] \\>";
  out << " \\\\" << endl;

  return true;
}
 
/*F ans = StoreAsProperties(props)  . . . . . . . . . . . . .  store elements
**
**  DESCRIPTION
**    props: Where to store
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataBasicAtomData::StoreAsProperties(BaseDataSetOfObjects *props)
{
  BaseDataInteger *p1 = new BaseDataInteger();
  p1->NameTag = "AtomicNumber";
  p1->SetValue(AtomicNumber);
  props->AddObject(p1);
  delete p1;

  BaseDataReal *p2 = new BaseDataReal();
  p2->NameTag = "Charge";
  p2->SetValue(Charge);
  props->AddObject(p2);
  delete p2;

  BaseDataInteger *p3 = new BaseDataInteger();
  p3->NameTag = "Radical";
  p3->SetValue(Radical);
  props->AddObject(p3);
  delete p3;

  BaseDataReal *p4 = new BaseDataReal();
  p4->NameTag = "X";
  p4->SetValue(X);
  props->AddObject(p4);
  delete p4;

  BaseDataReal *p5 = new BaseDataReal();
  p5->NameTag = "Y";
  p5->SetValue(Y);
  props->AddObject(p5);
  delete p5;

  BaseDataReal *p6 = new BaseDataReal();
  p6->NameTag = "Z";
  p6->SetValue(Z);
  props->AddObject(p6);
  delete p6;

  BaseDataInteger *p7 = new BaseDataInteger();
  p7->NameTag = "SingleBondCount";
  p7->SetValue(SingleBondCount);
  props->AddObject(p7);
  delete p7;

  BaseDataInteger *p8 = new BaseDataInteger();
  p8->NameTag = "DoubleBondCount";
  p8->SetValue(DoubleBondCount);
  props->AddObject(p8);
  delete p8;

  BaseDataInteger *p9 = new BaseDataInteger();
  p9->NameTag = "TripleBondCount";
  p9->SetValue(TripleBondCount);
  props->AddObject(p9);
  delete p9;

  BaseDataInteger *p10 = new BaseDataInteger();
  p10->NameTag = "Valence";
  p10->SetValue(Valence);
  props->AddObject(p10);
  delete p10;

  BaseDataInteger *p11 = new BaseDataInteger();
  p11->NameTag = "NumberOfBonds";
  p11->SetValue(NumberOfBonds);
  props->AddObject(p11);
  delete p11;

  BaseDataReal *p12 = new BaseDataReal();
  p12->NameTag = "NumberOfElectrons";
  p12->SetValue(NumberOfElectrons);
  props->AddObject(p12);
  delete p12;

  BaseDataInteger *p13 = new BaseDataInteger();
  p13->NameTag = "Group";
  p13->SetValue(Group);
  props->AddObject(p13);
  delete p13;

  BaseDataInteger *p14 = new BaseDataInteger();
  p14->NameTag = "LonePairs";
  p14->SetValue(LonePairs);
  props->AddObject(p14);
  delete p14;

  BaseDataInteger *p15 = new BaseDataInteger();
  p15->NameTag = "HydrogenCount";
  p15->SetValue(HydrogenCount);
  props->AddObject(p15);
  delete p15;

  BaseDataReal *p16 = new BaseDataReal();
  p16->NameTag = "CovalentRadius";
  p16->SetValue(CovalentRadius);
  props->AddObject(p16);
  delete p16;

  BaseDataReal *p17 = new BaseDataReal();
  p17->NameTag = "Screening";
  p17->SetValue(Screening);
  props->AddObject(p17);
  delete p17;

  BaseDataReal *p18 = new BaseDataReal();
  p18->NameTag = "ZEff";
  p18->SetValue(ZEff);
  props->AddObject(p18);
  delete p18;

  BaseDataReal *p19 = new BaseDataReal();
  p19->NameTag = "AtomElectronegativity";
  p19->SetValue(AtomElectronegativity);
  props->AddObject(p19);
  delete p19;

  BaseDataInteger *p20 = new BaseDataInteger();
  p20->NameTag = "Aromatic";
  p20->SetValue(Aromatic);
  props->AddObject(p20);
  delete p20;

  BaseDataDoubleVector *vec = new BaseDataDoubleVector();
  Vector<double> v1(Shells);
  vec->CurrentVector().CopyClone(&v1);
  vec->NameTag = "Shells";
  props->AddObject(vec);
  delete vec;
  return true;
}
/*S RxnBasicAtomDataClass
 */
/*F RxnBasicAtomDataClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnBasicAtomDataClass::RxnBasicAtomDataClass()
  : AtomInformationClass(NULL),
    AtomInformation(NULL)
{
  Identification = MOLATOM_BASIC_ID;
  NameTag = MOLATOM_BASIC_NAME;
  SubClass = "Object";
  EncodeDecodeClass = NameTag;
} 
/*F RxnBasicAtomDataClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnBasicAtomDataClass::RxnBasicAtomDataClass(const RxnBasicAtomDataClass& data)
  : DataObjectClass(data)
{
  AtomInformationClass = (RxnAtomInformationClass *) PointerClone(data.AtomInformationClass);
  AtomInformation      = (RxnDataAtomInformation  *) PointerClone(data.AtomInformation);
} 
 
/*F RxnBasicAtomDataClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnBasicAtomDataClass::RxnBasicAtomDataClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataObjectClass(id,name,descr),
    AtomInformationClass(NULL),
    AtomInformation(NULL)
{
  SubClass = "Object";
  EncodeDecodeClass = MOLATOM_BASIC_NAME;
}
/*F ~RxnBasicAtomDataClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnBasicAtomDataClass::~RxnBasicAtomDataClass()
{
  if(AtomInformationClass != NULL)
    delete AtomInformationClass;
  AtomInformationClass = NULL;
  if(AtomInformation != NULL)
    delete AtomInformation;
  AtomInformation = NULL;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnBasicAtomDataClass::print(ostream& out) const
{
  DataObjectClass::print(out);
  PointerPrint(out,"  The Atom Information Class: "," No Class Defined ",AtomInformationClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnBasicAtomDataClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnBasicAtomDataClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataObjectClass::Read(in,set);
  result = result && PointerClassRead(in,(DataObjectClass *&) AtomInformationClass,
				      STATICATOM_ATOMINFOSET_NAME,
				      set," No AtomInformation Class ");
  String notdefined("Atom Information not defined");
  result = result && PointerObjectRead(in, (BaseDataObject *&) AtomInformation,
				       AtomInformationClass, notdefined);
  return result;
}
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnBasicAtomDataClass::CopyClone(Identify *  objc)
{
  RxnBasicAtomDataClass *objcfull = (RxnBasicAtomDataClass *) objc;
  *this = *objcfull;
  AtomInformationClass = (RxnAtomInformationClass *) PointerClone(objcfull->AtomInformationClass);
  AtomInformation      = (RxnDataAtomInformation  *) PointerClone(objcfull->AtomInformation);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnBasicAtomDataClass::Clone()
{
  RxnBasicAtomDataClass* id = new RxnBasicAtomDataClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBasicAtomDataClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,AtomInformationClass);
  result = result && PointerEncode(buffer,AtomInformation);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBasicAtomDataClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataObjectClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) AtomInformationClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) AtomInformation);
  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnBasicAtomDataClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataBasicAtomData();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnBasicAtomDataClass*& obj)
     {
     obj = new RxnBasicAtomDataClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataBasicAtomData
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataBasicAtomData*& obj)
     {
     obj = new RxnDataBasicAtomData;
     return obj->DecodeThis(buffer);
     }
 
/*F atominfoclass = getAtomInformationClass() . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    atominfoclass: The Atom Information Class 
**
**  REMARKS
**
*/
RxnAtomInformationClass *RxnBasicAtomDataClass::getAtomInformationClass()
{
  return AtomInformationClass;
}
 
/*Fatominfo = getAtomInformation()  . . . . . . . . . . RxnBasicAtomDataClass
**
**  DESCRIPTION
**    atominfo: The complete set of atomic information
**
**  REMARKS
**
*/
RxnDataAtomInformation  *RxnBasicAtomDataClass::getAtomInformation()
{
  return AtomInformation;
}
 
/*F ans = PrintLaTeXTablePrefix(out)
**
**  DESCRIPTION
**    out: The output stream
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBasicAtomDataClass::PrintLaTeXTablePrefix(ostream& out)
{
  bool result = true;
  out << "\\begin{tabbing}" << endl;
  out << "XXXX \\= ";
  out << "XXX \\= ";
  out << "XXXXXXXXX \\= ";
  out << "XXXXXX \\= ";
  out << "XXXX \\= ";
  out << "XXXXXXXXXX \\= ";
  out << "XX \\= ";
  out << "XX \\= ";
  out << "XXX \\= ";
  out << "XXXXXXXXX \\= \\kill " << endl;

  out << " \\>";
  out << "At \\# \\> ";
  out << "C \\> ";
  out << "R \\> ";
  out << "Bonds \\> ";
  out << "H \\> ";
  out << "G \\> ";
  out << "elec \\> ";
  out << "Shell \\\\ ";
  out << endl;
  return result;
}
/*F ans = PrintLaTeXTablePostfix(out)
**
**  DESCRIPTION
**    out: The output stream
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnBasicAtomDataClass::PrintLaTeXTablePostfix(ostream& out)
{
  bool result = true;
  out << "\\end{tabbing}" << endl;
  return result;
}

/*S RxnDataMolFileAtom
 */ 
/*F RxnDataMolFileAtom()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMolFileAtom::RxnDataMolFileAtom()
{
  Identification = MOLATOM_MOLFILE_ID;
  NameTag = MOLATOM_MOLFILE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMolFileAtom(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMolFileAtom::RxnDataMolFileAtom(const RxnDataMolFileAtom& data)
  : RxnDataBasicAtomData(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMolFileAtom
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMolFileAtom::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMolFileAtom
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMolFileAtom::Read(istream& in, DataObjectClass* objc, const String& name)
{
  RxnMolFileAtomClass *molatomclass = (RxnMolFileAtomClass *) objc;
  RxnDataAtomInformation *atominfo = molatomclass->getAtomInformation();

  bool result = true;
  
  String line, sym;
  INDEX chargecode;
     
  line.ReadFullLine(in);

#ifdef DEBUG_ATOMBOND_ITC
  cout << "\nMolFileAtom   ";
#endif
     
  X = line.ToFloat(0, 9);
  Y = line.ToFloat(10,19);
  Z = line.ToFloat(20,29);
     
  sym = line.Isolate(31,32);
  String atS(sym);
  atS.EliminateBlanks();

#ifdef DEBUG_ATOMBOND_ITC
  cout << "Name Tag-" << NameTag << "-  ";
#endif
     
     
  chargecode = (int) line.ToInteger(37,39);
  ChargeAndRadicalFromMolFileSpec(*molatomclass,chargecode);

  int atn = (int) atominfo->AtomicNumberFromSymbol(atS);
#ifdef DEBUG_ATOMBOND_ITC
  cout << "AtomicSymbol: '"  << atS << "'" << endl;
  cout << "Corresponding Atomic Number: " << atn << endl;
#endif
  if(atn == -1)
    {
      atn = (int) atominfo->FindMetaAtomSymbol(sym);
#ifdef DEBUG_ATOMBOND_ITC
      cout << "MetaAtom: " << atn << endl;
#endif
    }

  if(atn == -1)
    {
      cout << "Atomic Symbol Not Found: " << sym << endl;
      atn = 0;
      result = false;
    }
  else
    AtomicNumber = atn;

#ifdef DEBUG_ATOMBOND_ITC
  cout << "AtomicNumber=" << AtomicNumber << " Charge=" << Charge << "\n";
#endif
  CovalentRadius = atominfo->ReadCovalentRadius(AtomicNumber);
  //CalculateSimpleElectronic();
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMolFileAtom
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMolFileAtom::print(ostream& out) const
{
  RxnDataBasicAtomData::print(out);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMolFileAtom
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMolFileAtom::Clone()
{
  RxnDataMolFileAtom *obj = new RxnDataMolFileAtom(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMolFileAtom
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMolFileAtom::CopyClone(Identify * obj)
{
  RxnDataMolFileAtom *objfull = (RxnDataMolFileAtom *) obj;
  *this = *objfull;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMolFileAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMolFileAtom::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnDataBasicAtomData::EncodeThis(buffer);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMolFileAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMolFileAtom::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnDataBasicAtomData::DecodeThis(buffer);
  return result;
}
/*S RxnMolFileAtomClass
 */
/*F RxnMolFileAtomClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMolFileAtomClass::RxnMolFileAtomClass()
  : MinCode(0),
    VectorClass(NULL),
    Charges(NULL),
    Radical(NULL)
{
  Identification = MOLATOM_MOLFILE_ID;
  NameTag = MOLATOM_MOLFILE_NAME;
  SubClass = "BasicAtomData";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMolFileAtomClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMolFileAtomClass::RxnMolFileAtomClass(const RxnMolFileAtomClass& data)
  : RxnBasicAtomDataClass(data),
    MinCode(data.MinCode)
{
 VectorClass  = (DataDoubleVectorClass *) PointerClone(data.VectorClass);
 Charges      = (BaseDataDoubleVector *) PointerClone(data.Charges);
 Radical      = (BaseDataDoubleVector *) PointerClone(data.Radical);
} 
 
/*F RxnMolFileAtomClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnMolFileAtomClass::RxnMolFileAtomClass(const int id, 
				 const String& name,
				 const String& descr)
  : RxnBasicAtomDataClass(id,name,descr),
    MinCode(0),
    VectorClass(NULL),
    Charges(NULL),
    Radical(NULL)
{
  SubClass = "BasicAtomData";
  EncodeDecodeClass = MOLATOM_MOLFILE_NAME;
}
/*F RxnMolFileAtomClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMolFileAtomClass::~RxnMolFileAtomClass()
{
  if(VectorClass != NULL)
    delete VectorClass;
  VectorClass = NULL;
  if(Charges != NULL)
    delete Charges;
  Charges = NULL;
  if(Radical != NULL)
    delete Radical;
  Radical = NULL;
} 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMolFileAtomClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMolFileAtomClass::print(ostream& out) const
{
  RxnBasicAtomDataClass::print(out);
  out << "Minimum Code Number: " << MinCode << endl;
  PointerPrint(out,"  The Vector Class: "," No Class Defined ",VectorClass);
  PointerPrint(out,"\n  The Charge - Input Code Correspondence: "," No Class Defined ",Charges);
  PointerPrint(out,"\n  The Radical - Input Code Correspondence: "," No Class Defined ",Radical);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMolFileAtomClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMolFileAtomClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMolFileAtomClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = RxnBasicAtomDataClass::Read(in,set);
  result = result && PointerClassRead(in,(DataObjectClass *&) VectorClass,
				      DATAOBJ_VECTOR_NAME,
				      set," No Class ");
  if(VectorClass != NULL)
    {
      StreamObjectInput str(in,' ');
      String minS = str.ReadNext();
      MinCode = minS.ToInteger();
    }

  String notdefined("Not Defined");
  result = result && PointerObjectRead(in, (BaseDataObject *&) Charges,
				       VectorClass, notdefined);
  result = result && PointerObjectRead(in, (BaseDataObject *&) Radical,
				       VectorClass, notdefined);

  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMolFileAtomClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMolFileAtomClass::CopyClone(Identify *  objc)
{
  RxnMolFileAtomClass *objcfull = (RxnMolFileAtomClass *) objc;
  *this = *objcfull;
  VectorClass = (DataDoubleVectorClass *) PointerClone(objcfull->VectorClass);
  Charges = (BaseDataDoubleVector *) PointerClone(objcfull->Charges);
  Radical = (BaseDataDoubleVector *) PointerClone(objcfull->Radical);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMolFileAtomClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMolFileAtomClass::Clone()
    {
      RxnMolFileAtomClass* id = new RxnMolFileAtomClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMolFileAtomClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMolFileAtomClass::EncodeThis(CommBuffer& buffer)
{
  bool result = RxnBasicAtomDataClass::EncodeThis(buffer);
  result = result && Encode(buffer,MinCode);
  result = result && PointerEncode(buffer,VectorClass);
  result = result && PointerEncode(buffer,Charges);
  result = result && PointerEncode(buffer,Radical);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMolFileAtomClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMolFileAtomClass::DecodeThis(CommBuffer& buffer)
{
  bool result = RxnBasicAtomDataClass::DecodeThis(buffer);
  result = result && Decode(buffer,MinCode);
  result = result && PointerDecode(buffer,(BaseDataObject *&) VectorClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) Charges);
  result = result && PointerDecode(buffer,(BaseDataObject *&) Radical);
  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnMolFileAtomClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMolFileAtom();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMolFileAtomClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMolFileAtomClass*& obj)
     {
     obj = new RxnMolFileAtomClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMolFileAtom
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMolFileAtom*& obj)
     {
     obj = new RxnDataMolFileAtom;
     return obj->DecodeThis(buffer);
     }
/*F ChargeAndRadicalFromMolFileSpec(molfileclass,code)  .  RxnDataMolFileAtom
**
**  DESCRIPTION
**    molfileclass: The mol atom information
**    code: The input code to translate
**
**  REMARKS
**
*/
void RxnDataMolFileAtom::ChargeAndRadicalFromMolFileSpec(RxnMolFileAtomClass& molfileclass,
							 const unsigned int code)
{
  Charge = molfileclass.getCharge(code);
  Radical = (int) molfileclass.getRadical(code);
  LonePairs = (int) molfileclass.getLonePairs(code);
}
 
 
/*F chrg = getCharge(code)  . . . . . . . . . . . .  translate code to charge
**
**  DESCRIPTION
**    code: The input code
**    chrg: The corresponding charge
**
**  REMARKS
**
*/
double RxnMolFileAtomClass::getCharge(unsigned int code)
{
  VectorNumeric vec = Charges->CurrentVector();
  double chrg = 0;
  if(code >= MinCode &&
     code < MinCode + vec.size())
    {
      unsigned int c = code - MinCode;
      chrg = vec[c];
    }
  return chrg;
}
/*F rad = getRadical(code)  . . . . . . . . . . . .  translate code to radical type
**
**  DESCRIPTION
**    code: The input code
**    rad: The corresponding radical
**
**  REMARKS
**
*/
double RxnMolFileAtomClass::getRadical(unsigned int code)
{
  VectorNumeric vec = Radical->CurrentVector();
  double rad = 0;
  if(code >= MinCode &&
     code < MinCode + vec.size())
    {
      unsigned int c = code - MinCode;
      rad = vec[c];
    }
  int rad10 = (int) floor( rad / 10 );
  double rad1 = rad - rad10*10;
  return rad1;
}
/*F rad = getLonePairs(code)  . . . . . . . .  translate code to radical type
**
**  DESCRIPTION
**    code: The input code
**    rad: The corresponding radical
**
**  REMARKS
**
*/
double RxnMolFileAtomClass::getLonePairs(unsigned int code)
{
  VectorNumeric vec = Radical->CurrentVector();
  double rad = 0;
  if(code >= MinCode &&
     code < MinCode + vec.size())
    {
      unsigned int c = code - MinCode;
      rad = vec[c];
    }
  double rad10 = (int) floor( rad / 10 );
  return rad10;
}
/*S RxnDataMoleculeAtom
 */ 
/*F RxnDataMoleculeAtom()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeAtom::RxnDataMoleculeAtom()
  : BasicAtomData(NULL)
{
  Identification = MOLATOM_MOLATOM_ID;
  NameTag = MOLATOM_MOLATOM_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMoleculeAtom(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMoleculeAtom::RxnDataMoleculeAtom(const RxnDataMoleculeAtom& data)
  : BaseDataNode(data)
{
  BasicAtomData = (RxnDataBasicAtomData *) PointerClone(data.BasicAtomData);
}
 
/*F RxnDataMoleculeAtom(data) . . . . . . . . . . . . . . RxnDataMoleculeAtom
**
**  DESCRIPTION
**    data: The atom data
**
**  REMARKS
**
*/
RxnDataMoleculeAtom::RxnDataMoleculeAtom(RxnDataBasicAtomData *data)
{
  Identification = MOLATOM_MOLATOM_ID;
  NameTag = MOLATOM_MOLATOM_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
  BasicAtomData = (RxnDataBasicAtomData *) PointerClone(data);
  NameTag = data->NameTag;
}
/*F RxnDataMoleculeAtom()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMoleculeAtom::~RxnDataMoleculeAtom()
{
  if(BasicAtomData != NULL)
    delete BasicAtomData;
  BasicAtomData = NULL;
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMoleculeAtom
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMoleculeAtom::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMoleculeAtom
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMoleculeAtom::Read(istream& in, DataObjectClass* objc, const String& name)
{
  cout << "Start: RxnDataMoleculeAtom::Read" << endl;
  bool result = BaseDataNode::Read(in,objc,name);
  cout << "Data : RxnDataMoleculeAtom::Read" << endl;
  String notdefined("Basic Atom Data Class not defined");
  result = PointerObjectRead(in,(BaseDataObject *&) BasicAtomData,
			     objc,notdefined);
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMoleculeAtom
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMoleculeAtom::print(ostream& out) const
{
  BaseDataNode::print(out);
  PointerPrint(out,"The Basic Atom Data: \n","No Data",BasicAtomData);
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeAtom
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMoleculeAtom::Clone()
{
  RxnDataMoleculeAtom *obj = new RxnDataMoleculeAtom(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMoleculeAtom
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMoleculeAtom::CopyClone(Identify * obj)
{
  RxnDataMoleculeAtom *objfull = (RxnDataMoleculeAtom *) obj;
  *this = *objfull;
  BasicAtomData = (RxnDataBasicAtomData *) PointerClone(objfull->BasicAtomData);
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeAtom::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataNode::EncodeThis(buffer);
  result = result && PointerEncode(buffer,BasicAtomData);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMoleculeAtom
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMoleculeAtom::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataNode::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) BasicAtomData);
  return result;
}
 
/*F data = getBasicAtomData()  . . . . . . . . . . . . . RxnDataMoleculeAtom::
**
**  DESCRIPTION
**    data: The principle data from the atom
**
**  REMARKS
**
*/
RxnDataBasicAtomData *RxnDataMoleculeAtom::getBasicAtomData()
{
  return BasicAtomData;
}
/*F ans = WriteAsLine(out,objc) . . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object one one line
**
**  REMARKS
**
*/
bool RxnDataMoleculeAtom::WriteAsLine(ostream& out, DataObjectClass *objc)
{
  bool result = true;
  if(BasicAtomData != NULL)
    BasicAtomData->WriteAsLine(out,objc);
  return result;
}
/*F ans = WriteAsASCII(out,objc)  . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object as complement to input
**
**  REMARKS
**
*/
bool RxnDataMoleculeAtom::WriteAsASCII(ostream &out, DataObjectClass *objc)
{
  bool result = true;
  if(BasicAtomData != NULL)
    BasicAtomData->WriteAsASCII(out,objc);
  return result;
}
/*F ans = WriteAsLatex(out,objc)  . . . . . . . . . . . . . .  BaseDataObject
**
**  DESCRIPTION
**    out: The output stream
**    objc: The class of the object
**    ans: true if successful
**
**    This writes out the object prepared for latex
**
**  REMARKS
**
*/
bool RxnDataMoleculeAtom::WriteAsLatex(ostream& out, DataObjectClass* objc)
{
  bool result = true;
  if(BasicAtomData != NULL)
    BasicAtomData->WriteAsLatex(out,objc);
  return result;
}
 
/*F TransferBasicAtomDataToProperties() . . . . . . . . . RxnDataMoleculeAtom
**
**  DESCRIPTION
**    ans: true if sucessful
**
**  REMARKS
**
*/
bool RxnDataMoleculeAtom::TransferBasicAtomDataToProperties()
{
  bool result = true;
  if(BasicAtomData != NULL)
    {
      result = BasicAtomData->StoreAsProperties(this);
    }
  else
    result = false;

  return result;
}
/*S RxnMoleculeAtomClass
 */
/*F RxnMoleculeAtomClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMoleculeAtomClass::RxnMoleculeAtomClass()
  : BasicAtomDataClass(NULL)
{
  Identification = MOLATOM_MOLATOM_ID;
  NameTag = MOLATOM_MOLATOM_NAME;
  SubClass = "Node";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMoleculeAtomClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMoleculeAtomClass::RxnMoleculeAtomClass(const RxnMoleculeAtomClass& data)
  : DataNodeClass(data)
{
  BasicAtomDataClass = (RxnBasicAtomDataClass *) PointerClone(data.BasicAtomDataClass);
} 
 
/*F RxnMoleculeAtomClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnMoleculeAtomClass::RxnMoleculeAtomClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataNodeClass(id,name,descr),
    BasicAtomDataClass(NULL)
{
  SubClass = "Node";
  EncodeDecodeClass = MOLATOM_MOLATOM_NAME;
}
/*F ~RxnMoleculeAtomClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMoleculeAtomClass::~RxnMoleculeAtomClass()
{
  if(BasicAtomDataClass != NULL)
    delete BasicAtomDataClass;
  BasicAtomDataClass = NULL;
}
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMoleculeAtomClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMoleculeAtomClass::print(ostream& out) const
{
  DataNodeClass::print(out);
  PointerPrint(out,"  The Basic Atom Data Class: "," No Class Defined ",BasicAtomDataClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeAtomClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMoleculeAtomClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMoleculeAtomClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  ReadClassPairs = false;
  ReadAllowedClasses = false;
  //bool result = DataNodeClass::Read(in,set);
  //result = result && 
  bool result = PointerClassRead(in,(DataObjectClass *&) BasicAtomDataClass,
				      MOLATOM_BASIC_NAME,
				      set," No Class ");
  DetailedRead = false;
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMoleculeAtomClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMoleculeAtomClass::CopyClone(Identify *  objc)
{
  RxnMoleculeAtomClass *objcfull = (RxnMoleculeAtomClass *) objc;
  *this = *objcfull;
  BasicAtomDataClass = (RxnBasicAtomDataClass *) PointerClone(objcfull->BasicAtomDataClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMoleculeAtomClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMoleculeAtomClass::Clone()
{
  RxnMoleculeAtomClass* id = new RxnMoleculeAtomClass(*this);
  return (Identify *) id;
}
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeAtomClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeAtomClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataNodeClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,BasicAtomDataClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMoleculeAtomClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeAtomClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataNodeClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) BasicAtomDataClass);
  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnMoleculeAtomClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMoleculeAtom();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMoleculeAtomClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMoleculeAtomClass*& obj)
{
  obj = new RxnMoleculeAtomClass;
  return obj->DecodeThis(buffer);
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMoleculeAtom
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMoleculeAtom*& obj)
{
  obj = new RxnDataMoleculeAtom;
  return obj->DecodeThis(buffer);
}
/*F ans = PrintLaTeXTablePrefix(out)  . . . . . . . . . print begining of set
**
**  DESCRIPTION
**    out: The output stream
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMoleculeAtomClass::PrintLaTeXTablePrefix(ostream& out)
{
  bool result = true;

  if(BasicAtomDataClass != NULL)
    result = BasicAtomDataClass->PrintLaTeXTablePrefix(out);
  else
    result = false;
  return result;
}
/*F ans = PrintLaTeXTablePostfix(out)  . . . . . . . . . print end of set
**
**  DESCRIPTION
**    out: The output stream
**    ans: true if successful
**    
**  REMARKS
**
*/
bool RxnMoleculeAtomClass::PrintLaTeXTablePostfix(ostream& out)
{
  bool result = true;

  if(BasicAtomDataClass != NULL)
    result = BasicAtomDataClass->PrintLaTeXTablePostfix(out);
  else
    result = false;
  return result;
}
/*F data = getBasicAtomDataClass()  . . . . . . . . . . . . . RxnDataMoleculeAtom::
**
**  DESCRIPTION
**    data: The data class from the atom class
**
**  REMARKS
**
*/
RxnBasicAtomDataClass *RxnMoleculeAtomClass::getBasicAtomDataClass()
{
  return BasicAtomDataClass;
}

/*S Utilities
 */
/*F InitialSetOfMolAtomEncodeDecodeRoutines()
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void InitialSetOfMolAtomEncodeDecodeRoutines()
{
  EncodeDecodeRegisterClass(RxnBasicAtomDataClass,RxnDataBasicAtomData,MOLATOM_BASIC_NAME);
  EncodeDecodeRegisterClass(RxnMolFileAtomClass,RxnDataMolFileAtom,MOLATOM_MOLFILE_NAME);
  EncodeDecodeRegisterClass(RxnMoleculeAtomClass,RxnDataMoleculeAtom,MOLATOM_MOLATOM_NAME);
}
 
/*F
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
extern void AddMolAtomClasses(DataSetOfObjectsClass& set)
{
  String basicdescr("The Basic Atom Information Class");
  RxnBasicAtomDataClass basicclass(MOLATOM_BASIC_ID,MOLATOM_BASIC_NAME,basicdescr);
  set.AddObjectClass(basicclass);

  String mfdescr("The MolFile Atom Class");
  RxnMolFileAtomClass mfclass(MOLATOM_MOLFILE_ID,MOLATOM_MOLFILE_NAME,mfdescr);
  set.AddObjectClass(mfclass);

  String molatomdescr("The Molecule Atom Class");
  RxnMoleculeAtomClass molatomclass(MOLATOM_MOLATOM_ID,MOLATOM_MOLATOM_NAME,molatomdescr);
  set.AddObjectClass(molatomclass);
}
