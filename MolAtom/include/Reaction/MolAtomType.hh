/*  FILE     MolAtomType.hh
**  PACKAGE  MolAtom
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "MolAtom" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_MOLATOMTYPE_HH
#define Reaction_MOLATOMTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

/*S Atom Classes
*/
class RxnMolFileAtomClass;
/*C RxnDataBasicAtomData  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the BasicAtomData class definitions

**    The MolFileAtom has the information that is read in using
**    the MolFile format (adapted from Molecular Design).  The 
**    coordinates (3DCoordinates), AtomicNumber (SimpleAtom) Charge
**    (SimpleAtom) and Radical (SimpleAtom) information is read in
**    from the one line format.  The entire set of parameters
**    given on each line is also given
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataBasicAtomData : public BaseDataObject
{
public:
  unsigned int AtomicNumber;
  double Charge;
  int Radical;

  double X;
  double Y;
  double Z;

  unsigned int SingleBondCount;
  unsigned int DoubleBondCount;
  unsigned int TripleBondCount;

     
  unsigned int Valence;
  unsigned int NumberOfBonds;
  double NumberOfElectrons;
  unsigned int Group;
  unsigned int LonePairs;
  unsigned  int HydrogenCount;
     
  double CovalentRadius;
  double Screening;
  double ZEff;
  double AtomElectronegativity;

  vector<double> Shells;
  
  unsigned int Aromatic;

  RxnDataBasicAtomData();
  RxnDataBasicAtomData(const RxnDataBasicAtomData& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;

  void CalculateSimpleElectronic(void);
  
  int CalcNumberOfBonds(void);
  double CalcNumberOfElectrons(int atomicnumber =0, 
			       int charge =0);
  int CalcAtomGroupFromElectrons(double eles =0);
  int CountLonePairsOfAtom(int g =0, int bnds =0);
  int SingleBondsNotSpecified(int g =0, int lp =0, 
			      int bnds =0, int rad =0);
  void CalcShells(double electrons=0);
  double CalcScreening();
  double CalcEffectiveCharge(double screenin =0)     ;
  double CalcElectronegativity(int atomicnumber =0,
			       double cradius =0, 
			       double screening =0,
			       double electrons =0.0);
  unsigned int CalculateStandardValence();
  bool operator<(const RxnDataBasicAtomData& x);  
  void IncrementBondOrder(unsigned int bondorder);
  bool StoreAsProperties(BaseDataSetOfObjects *props);
  virtual bool WriteAsLine(ostream& out, DataObjectClass *objc);
  virtual bool WriteAsASCII(ostream& out, DataObjectClass *objc);
  virtual bool WriteAsLatex(ostream& out, DataObjectClass* objc);
};
/*C RxnBasicAtomDataClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnBasicAtomDataClass : public DataObjectClass
{
  RxnAtomInformationClass *AtomInformationClass;
  RxnDataAtomInformation  *AtomInformation;

public:
  RxnBasicAtomDataClass();
  RxnBasicAtomDataClass(const RxnBasicAtomDataClass& data);
  RxnBasicAtomDataClass(const int id, 
		    const String& name,
		    const String& descr);
  ~RxnBasicAtomDataClass();
  RxnAtomInformationClass *getAtomInformationClass();
  RxnDataAtomInformation  *getAtomInformation();
  STANDARD_VIRTUAL_METHODS_CLASS;
  bool PrintLaTeXTablePrefix(ostream& out);
  bool PrintLaTeXTablePostfix(ostream& out);
};
/*C RxnDataMolFileAtom  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MolFileAtom class definitions
**
**    The purpose of the MolFileAtom is to fill in the initial
**    atomic data that comes from MolFile input
**
**  REMARKS
**    Inheirits BaseDataBasicAtomData
*/
class RxnDataMolFileAtom : public RxnDataBasicAtomData
{
public:
  RxnDataMolFileAtom();
  RxnDataMolFileAtom(const RxnDataMolFileAtom& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  void ChargeAndRadicalFromMolFileSpec(RxnMolFileAtomClass& molfileclass,
				       const unsigned int code);
};
/*C RxnMolFileAtomClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataBasicAtomDataClass
*/
class RxnMolFileAtomClass : public RxnBasicAtomDataClass
{
  unsigned int MinCode;
  DataDoubleVectorClass *VectorClass;
  BaseDataDoubleVector *Charges;
  BaseDataDoubleVector *Radical;

public:
  RxnMolFileAtomClass();
  RxnMolFileAtomClass(const RxnMolFileAtomClass& data);
  RxnMolFileAtomClass(const int id, 
		    const String& name,
		    const String& descr);
  ~RxnMolFileAtomClass();
  double getCharge(unsigned int code);
  double getRadical(unsigned int code);
  double getLonePairs(unsigned int code);
 
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataMoleculeAtom  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MoleculeAtom class definitions
**
**  REMARKS
**    Inheirits BaseDataNode
*/
class RxnDataMoleculeAtom : public BaseDataNode
{
  RxnDataBasicAtomData *BasicAtomData;
public:
  RxnDataMoleculeAtom();
  RxnDataMoleculeAtom(const RxnDataMoleculeAtom& data);
  RxnDataMoleculeAtom(RxnDataBasicAtomData *data);
  ~RxnDataMoleculeAtom();

  STANDARD_VIRTUAL_METHODS_OBJECT;
  RxnDataBasicAtomData *getBasicAtomData();
  bool TransferBasicAtomDataToProperties();
  virtual bool WriteAsLine(ostream& out, DataObjectClass *objc);
  virtual bool WriteAsASCII(ostream&, DataObjectClass*);
  virtual bool WriteAsLatex(ostream& out, DataObjectClass* objc);
};
/*C RxnMoleculeAtomClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataNodeClass
*/
class RxnMoleculeAtomClass : public DataNodeClass
{
  RxnBasicAtomDataClass *BasicAtomDataClass;
public:
  RxnMoleculeAtomClass();
  RxnMoleculeAtomClass(const RxnMoleculeAtomClass& data);
  RxnMoleculeAtomClass(const int id, 
		    const String& name,
		    const String& descr);
  ~RxnMoleculeAtomClass();
  RxnBasicAtomDataClass *getBasicAtomDataClass();
  STANDARD_VIRTUAL_METHODS_CLASS;
  bool PrintLaTeXTablePrefix(ostream& out);
  bool PrintLaTeXTablePostfix(ostream& out);
};


#endif
