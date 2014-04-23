/*  FILE     MolStatsType.hh
**  PACKAGE  MolStats
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "MolStats" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_MOLSTATSTYPE_HH
#define Reaction_MOLSTATSTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
 
/*C AtomicNumberCount<ValenceType>  . . . . .  how many of atomic number type
**
**  DESCRIPTION
**    This class gives the counts (AtomCount) within a molecule of all the atoms
**    of the given atomic number (AtomicNumber).  In addition, 
**    a pair list is given (ValenceAndCounts) where each pair holds the 
**    valence (the first argument) and the number of atoms (of this 
**    atomic number) with this valence.  The fields are:
**    - AtomicNumber: The atomic number of the set of atoms in this class
**    - AtomCount: The number of atoms with this atomic number
**    - ValenceAndCounts: A PairList with the valence (first) and the
**      number of atoms in this set of that valence.
**
**  REMARKS
**
*/
template <class ValenceType> class AtomicNumberCount
{
public:
  int AtomicNumber;
  int AtomCount;
  PairList<ValenceType,int> ValenceAndCounts;

  AtomicNumberCount() : AtomCount(0) { }
 
  AtomicNumberCount(const AtomicNumberCount<ValenceType>& atmcounts) 
	  : AtomicNumber(atmcounts.AtomicNumber),
	  AtomCount(atmcounts.AtomCount),
	  ValenceAndCounts(atmcounts.ValenceAndCounts) { }

  AtomicNumberCount(const int atmnum, 
                           const int count,
                           const PairList<ValenceType,int>& vcounts)
	  : AtomicNumber(atmnum),
	  AtomCount(count),
	  ValenceAndCounts(vcounts) { }

  virtual ~AtomicNumberCount(){};
  virtual void CopyClone(AtomicNumberCount *cnt)
    {
      *this = *cnt;
    }
  virtual AtomicNumberCount * Clone()
    {
      AtomicNumberCount *cnt = new AtomicNumberCount();
      cnt->CopyClone(this);
      return cnt;
    }
  virtual ostream& print(ostream& out) const
    {
      out << AtomicNumber << ": (";
      out << AtomCount << ") ";
      out << ValenceAndCounts << "\n";
      return out;
    }
  virtual bool EncodeThis(CommBuffer& buffer)
    {
      bool result = Encode(buffer,AtomicNumber);
      result = result && Encode(buffer,AtomCount);
      result = result && ValenceAndCounts.EncodeThis(buffer);
      return result;
    }
  virtual bool DecodeThis(CommBuffer& buffer)
    {
      bool result = Decode(buffer,AtomicNumber);
      result = result && Decode(buffer,AtomCount);
      result = result && ValenceAndCounts.DecodeThis(buffer);
      return result;
    }
};
/*C
**
**  DESCRIPTION
**
**  REMARKS
**
*/
template <class VType> class CountConversion
{
protected:
    VType BaseType;
 public:
  CountConversion(VType base);
  int operator()(BaseDataObject *obj);
  void Convert(BaseDataObject *obj);
  VType GetValue() { return BaseType; }
};
class IntegerPropertyFromNumericOperation : public CountConversion<int>
{
 public:
  IntegerPropertyFromNumericOperation();
  void Convert(BaseDataObject *num);
};
/*C AtomCountList<ValenceType>  . .  list of valence and atomic number counts
**
**  DESCRIPTION
**
**  REMARKS
**
*/
template <class ValenceType> class AtomCountList 
{
public:
  SearchableObjectListSimple<int,AtomicNumberCount<ValenceType> > ValStats;
  ObjectList<ValenceType> Valences;
  ObjectList<int> AtomicNumbers;
     
  AtomCountList(RxnDataSimpleMolecule& molecule, 
		const String& property,
		CountConversion<ValenceType>* convert);
  AtomCountList(){}
  AtomCountList(const AtomCountList<ValenceType>& list)
    : ValStats(list.ValStats), 
      Valences(list.Valences),
      AtomicNumbers(list.AtomicNumbers)
    {
    }
  virtual ~AtomCountList(){}
  ostream& print(ostream& out) const;
     
  virtual void CopyClone(AtomCountList<ValenceType> *list)
    {
      *this = *list;
    }
  virtual AtomCountList<ValenceType> *Clone()
    {
      AtomCountList<ValenceType> *list = new AtomCountList<ValenceType>;
      list->CopyClone(this);
      return list;
    }
  virtual bool EncodeThis(CommBuffer& buffer)
    {
      bool result = ValStats.EncodeThis(buffer);
      result = result && Valences.EncodeThis(buffer);
      result = result && AtomicNumbers.EncodeThis(buffer);
      return result;
    }
  virtual bool DecodeThis(CommBuffer& buffer)
    {
      bool result = ValStats.DecodeThis(buffer);
      result = result && Valences.DecodeThis(buffer);
      result = result && AtomicNumbers.DecodeThis(buffer);
      return result;
    }
};
class IntegerPropertyFromNumericCount : public AtomCountList<int>
{
  
 public:
    IntegerPropertyFromNumericCount()
      : AtomCountList<int>() {};
    IntegerPropertyFromNumericCount(RxnDataSimpleMolecule& molecule, 
				    const String& property,
				    CountConversion<int> *convert)
	: AtomCountList<int>(molecule,property,convert)
	{}
};
/*C RxnDataAtomStatistics  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the AtomStatistics class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataAtomStatistics : public BaseDataSetOfObjects
{
  IntegerPropertyFromNumericCount AtomCounts;
public:
  RxnDataAtomStatistics();
  RxnDataAtomStatistics(const RxnDataAtomStatistics& data);
  RxnDataAtomStatistics(RxnDataSimpleMolecule& molecule,
			const String& property,
			RxnDataAtomInformation& atomsinfo,
			CountConversion<int> *convert);
  ObjectList<int>& getAtomicNumbers();
  AtomicNumberCount<int>& getAtomicNumberCount(int atnum);
  STANDARD_VIRTUAL_METHODS_OBJECT;
};
/*C RxnAtomStatisticsClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnAtomStatisticsClass : public DataSetOfObjectsClass
{
public:
  RxnAtomStatisticsClass();
  RxnAtomStatisticsClass(const RxnAtomStatisticsClass& data);
  RxnAtomStatisticsClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};

/*C RxnDataFormMoleculeSetStatistics  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the FormMoleculeSetStatistics class definitions
**
**  REMARKS
**    Inheirits BaseDataAlgorithmOperation
*/
class RxnDataFormMoleculeSetStatistics : public BaseDataAlgorithmOperation
{
  String MoleculeNameListS;
  String RootNameS;

  String ValencesInMoleculeKey;
  String AtomsInMoleculeKey;

  BaseDataKeyWords *CountNames;
  BaseDataKeyWords *VariablesInInstance;
  int NumberOfMolecules;
  String MoleculeNameList;
  String RootName;
  BaseDataKeyWords *MoleculeNames;
  BaseDataSetOfObjects *Distributions;

  bool ValencesInMolecule;
  bool AtomsInMolecule;

public:
  RxnDataFormMoleculeSetStatistics();
  RxnDataFormMoleculeSetStatistics(const RxnDataFormMoleculeSetStatistics& data);

  void AddAtomDistributions(BaseDataInstance *molecule,RxnDataAtomStatistics& atomstats, int cnt);
  void AddValenceDistributions(BaseDataInstance *molecule,RxnDataAtomStatistics& atomstats, int cnt);

  String *MakeCountName(String& name);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  STANDARD_VIRTUAL_ALGORITHM_OBJECT_METHODS;
};
/*C RxnFormMoleculeSetStatisticsClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataAlgorithmOperationClass
*/
class RxnFormMoleculeSetStatisticsClass : public DataAlgorithmOperationClass
{
public:
  RxnFormMoleculeSetStatisticsClass();
  RxnFormMoleculeSetStatisticsClass(const RxnFormMoleculeSetStatisticsClass& data);
  RxnFormMoleculeSetStatisticsClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};



#endif
