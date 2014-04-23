/*  FILE     StaticAtomsType.hh
**  PACKAGE  StaticAtoms
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "StaticAtoms" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_STATICATOMSTYPE_HH
#define Reaction_STATICATOMSTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
/*C BaseDataStaticAtomInfo  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**     The set of atomic constants associated that are needed for calculations:
**     - AtomName:     The string name (one or two characters) of the atom 
                       This is stored in the 'Identification' 
**     - AtomicNumber: The atomic number of the atom
**     - CovalentRadius: The electronic radius (used for various calculations)
**
**     The input of the line is done through the standard input operator.  The
**     output mimics the input.  All constants are on one line separated by 
**     spaces
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataStaticAtomInfo : public BaseDataSetOfObjects
{
  unsigned int AtomicNumber;

public:
  RxnDataStaticAtomInfo();
  RxnDataStaticAtomInfo(const RxnDataStaticAtomInfo& data);
  unsigned int getAtomicNumber();
  void setAtomicNumber(unsigned int atn);
  bool operator==(const RxnDataStaticAtomInfo& y);
  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C DataStaticAtomInfoClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnStaticAtomInfoClass : public DataSetOfObjectsClass
{
public:
  RxnStaticAtomInfoClass();
  RxnStaticAtomInfoClass(const RxnStaticAtomInfoClass& data);
  RxnStaticAtomInfoClass(const int id, 
		    const String& name,
		    const String& descr);
  DataSetOfObjectsClass *PointerToAllowedClasses();
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataMetaAtomData  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the MetaAtomData class definitions
**
**  REMARKS
**    Inheirits BaseDataNValued
*/
class RxnDataMetaAtomData : public BaseDataNValued
{
  String AtomName;
public:
  RxnDataMetaAtomData();
  RxnDataMetaAtomData(const RxnDataMetaAtomData& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnMetaAtomDataClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataNValuedClass
*/
class RxnMetaAtomDataClass : public DataNValuedClass
{
public:
  RxnMetaAtomDataClass();
  RxnMetaAtomDataClass(const RxnMetaAtomDataClass& data);
  RxnMetaAtomDataClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataCompleteMetaAtom  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the CompleteMetaAtom class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataCompleteMetaAtom : public BaseDataSetOfObjects
{
public:
  unsigned int Index;
  RxnDataCompleteMetaAtom();
  RxnDataCompleteMetaAtom(const RxnDataCompleteMetaAtom& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnCompleteMetaAtomClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnCompleteMetaAtomClass : public DataSetOfObjectsClass
{
  RxnMetaAtomDataClass *MetaAtomClass;
public:
  RxnCompleteMetaAtomClass();
  RxnCompleteMetaAtomClass(const RxnCompleteMetaAtomClass& data);
  RxnCompleteMetaAtomClass(const int id, 
		    const String& name,
		    const String& descr);
  RxnMetaAtomDataClass *getMetaAtomClass();
  DataSetOfObjectsClass *PointerToAllowedClasses();
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataAtomInformation  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the AtomInformation class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataAtomInformation : public BaseDataSetOfObjects
{
  VectorSimple<String> *AtomicNumberToName;
  BaseDataSetOfObjects MetaAtoms;

public:
  RxnDataAtomInformation();
  RxnDataAtomInformation(const RxnDataAtomInformation& data);

  STANDARD_VIRTUAL_METHODS_OBJECT;

  unsigned int AtomicNumberFromSymbol(String& symbol);
  String AtomNameFromAtomicNumber(unsigned int num);
  unsigned int FindMetaAtomSymbol(String& symbol);
  double ReadCovalentRadius(unsigned int atomicnumber);
};
/*C RxnAtomInformationClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnAtomInformationClass : public DataSetOfObjectsClass
{
  RxnStaticAtomInfoClass *StaticAtomInfoClass;
  RxnCompleteMetaAtomClass *CompleteMetaAtomClass;
public:
  RxnAtomInformationClass();
  RxnAtomInformationClass(const RxnAtomInformationClass& data);
  RxnAtomInformationClass(const int id, 
		    const String& name,
		    const String& descr);

  RxnStaticAtomInfoClass *getStaticAtomInfoClass();
  RxnCompleteMetaAtomClass *getCompleteMetaAtomClass();
  STANDARD_VIRTUAL_METHODS_CLASS;
  DataSetOfObjectsClass *PointerToAllowedClasses();
};



/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
