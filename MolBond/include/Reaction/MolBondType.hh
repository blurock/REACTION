/*  FILE     MolBondType.hh
**  PACKAGE  MolBond
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "MolBond" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_MOLBONDTYPE_HH
#define Reaction_MOLBONDTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
/*S Bond Classes
*/
/*C RxnDataBasicBondData  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the BasicBondData class definitions
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataBasicBondData : public BaseDataObject
{
public:
  unsigned int BondOrder;
  double DeltaCharge;
  double DeltaElectronegativity;

  RxnDataBasicBondData();
  RxnDataBasicBondData(const RxnDataBasicBondData& data);

  bool StoreAsProperties(BaseDataSetOfObjects *props);
  virtual bool WriteAsLine(ostream& out, DataObjectClass *objc);
  virtual bool WriteAsASCII(ostream& out, DataObjectClass *objc);
  virtual bool WriteAsLatex(ostream& out, DataObjectClass* objc);
  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnBasicBondDataClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnBasicBondDataClass : public DataObjectClass
{
public:
  RxnBasicBondDataClass();
  RxnBasicBondDataClass(const RxnBasicBondDataClass& data);
  RxnBasicBondDataClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  bool PrintLaTeXTablePrefix(ostream& out);
  bool PrintLaTeXTablePostfix(ostream& out);
};
/*C RxnDataMolFileBond  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MolFileBond class definitions
**
**  REMARKS
**    Inheirits BaseDataBasicBondData
*/
class RxnDataMolFileBond : public RxnDataBasicBondData
{
 public:
  unsigned int BondI;
  unsigned int BondJ;
  vector<int> BondData;

  RxnDataMolFileBond();
  RxnDataMolFileBond(const RxnDataMolFileBond& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnMolFileBondClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataBasicBondDataClass
*/
class RxnMolFileBondClass : public RxnBasicBondDataClass
{
public:
  unsigned int MaxBondData;

  RxnMolFileBondClass();
  RxnMolFileBondClass(const RxnMolFileBondClass& data);
  RxnMolFileBondClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
};
/*C RxnDataMoleculeBond  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MoleculeBond class definitions
**
**  REMARKS
**    Inheirits BaseDataEdge
*/
class RxnDataMoleculeBond : public BaseDataEdge
{
  RxnDataBasicBondData *BasicBondData;
public:
  RxnDataMoleculeBond();
  RxnDataMoleculeBond(const RxnDataMoleculeBond& data);
  RxnDataMoleculeBond(RxnDataBasicBondData *data);
  ~RxnDataMoleculeBond();
  STANDARD_VIRTUAL_METHODS_OBJECT;
  virtual bool WriteAsLine(ostream& out, DataObjectClass *objc);
  virtual bool WriteAsASCII(ostream& out, DataObjectClass *objc);
  virtual bool WriteAsLatex(ostream& out, DataObjectClass* objc);
  bool TransferBasicAtomDataToProperties();
  RxnDataBasicBondData *getBasicBondData();
};
/*C RxnMoleculeBondClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataEdgeClass
*/
class RxnMoleculeBondClass : public DataEdgeClass
{
  RxnBasicBondDataClass *BasicBondDataClass;
public:
  RxnMoleculeBondClass();
  RxnMoleculeBondClass(const RxnMoleculeBondClass& data);
  RxnMoleculeBondClass(const int id, 
		    const String& name,
		    const String& descr);
  ~RxnMoleculeBondClass();
  RxnBasicBondDataClass *getBasicBondDataClass();
  STANDARD_VIRTUAL_METHODS_CLASS;
  bool PrintLaTeXTablePrefix(ostream& out);
  bool PrintLaTeXTablePostfix(ostream& out);
};



 

#endif




