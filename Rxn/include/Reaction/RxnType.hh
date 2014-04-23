/*  FILE     RxnType.hh
**  PACKAGE  Rxn
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "Rxn" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_RXNTYPE_HH
#define Reaction_RXNTYPE_HH

class RxnReactionClass;
class RxnMolecularStructuresDataBaseClass;
class RxnDataReactionRatesRxnFile;
class RxnReactionRatesClass;
/*C RxnDataReactionRates  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ReactionRates class definitions
**
**  REMARKS
**    Inheirits BaseDataObject
*/
class RxnDataReactionRates : public RxnDataRealBasedProperty
{
public:
  RxnDataReactionRates();
  RxnDataReactionRates(const RxnDataReactionRates& data);
  ~RxnDataReactionRates();

  RxnDataReactionRates* Mult(RxnDataReactionRates* rate, RxnReactionRatesClass *rateclass);
  RxnDataReactionRates* Div(RxnDataReactionRates* rate, RxnReactionRatesClass *rateclass);

  STANDARD_VIRTUAL_METHODS_OBJECT;
  double getArrheniusValue(RxnReactionRatesClass *rateclass);
  double getActivationEnergyValue(RxnReactionRatesClass *rateclass);
  double getTemperatureCoefficientValue(RxnReactionRatesClass *rateclass);
  BaseDataNumeric *getArrhenius(RxnReactionRatesClass *rateclass);
  BaseDataNumeric *getActivationEnergy(RxnReactionRatesClass *rateclass);
  BaseDataNumeric *getTemperatureCoefficient(RxnReactionRatesClass *rateclass);

  void setArrhenius(BaseDataNumeric *A,RxnReactionRatesClass *rateclass);
  void setActivationEnergy(BaseDataNumeric *n,RxnReactionRatesClass *rateclass);
  void setTemperatureCoefficient(BaseDataNumeric *E,RxnReactionRatesClass *rateclass);
  bool ReadAsMolFile(istream& in, DataObjectClass* objc, const String& name);
  bool ReadConstants(istream& in, DataObjectClass* objc);
  virtual bool WriteOutRates(ostream& out,
			     unsigned int n, 
			     RxnReactionRatesClass *rateclass, 
			     String property,
			     String delimitor,
			     bool chemkinreverse);

  friend class RxnDataReactionRatesRxnFile;
  friend class RxnReactionRatesClass;
};
/*C RxnReactionRatesClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataObjectClass
*/
class RxnReactionRatesClass : public RxnRealBasedPropertyClass
{
 protected:
  String ArrheniusName;
  String ActivationEnergyName;
  String TemperatureCoefficientName;

  DataNumericClass *RateType;
  double GasConstant;

  BaseDataKeySet ArrheniusFilter;
  BaseDataKeySet ActivationEnergyFilter;
  BaseDataKeySet GasConstantFilter;
public:
  RxnReactionRatesClass();
  RxnReactionRatesClass(const RxnReactionRatesClass& data);
  RxnReactionRatesClass(const int id, 
		    const String& name,
		    const String& descr);
  ~RxnReactionRatesClass();
  STANDARD_VIRTUAL_METHODS_CLASS;
  DataNumericClass *getRateType();

  double ConvertArrhenius(bool convertto,String& property,
			  unsigned int n,
			  RxnDataReactionRates *rates);
  double ConvertActivationEnergy(bool convertto,String& property,
				 RxnDataReactionRates *rates);
  double ConvertGasConstant(bool convertto,String& property,
			    RxnDataReactionRates *rates);
  DataSetOfObjectsClass *PointerToAllowedClasses();
  double getGasConstant();
  friend class RxnDataReactionRates;
  RxnDataReactionRates *ConvertRateConstants(RxnDataReactionRates *rate,
					     unsigned int n,String& property);
};
/*C RxnDataReactionRatesRxnFile  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ReactionRatesRxnFile class definitions
**
**  REMARKS
**    Inheirits BaseDataReactionRates
*/
class RxnDataReactionRatesRxnFile : public RxnDataReactionRates
{
public:
  RxnDataReactionRatesRxnFile();
  RxnDataReactionRatesRxnFile(const RxnDataReactionRatesRxnFile& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnReactionRatesRxnFileClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataReactionRatesClass
*/
class RxnReactionRatesRxnFileClass : public RxnReactionRatesClass
{
public:
  RxnReactionRatesRxnFileClass();
  RxnReactionRatesRxnFileClass(const RxnReactionRatesRxnFileClass& data);
  RxnReactionRatesRxnFileClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataThirdBody  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ThirdBody class definitions
**
**  REMARKS
**    Inheirits BaseDataReactionRates
*/
class RxnDataThirdBody : public RxnDataReactionRates
{
public:
  RxnDataThirdBody();
  RxnDataThirdBody(const RxnDataThirdBody& data);

  virtual bool WriteOutRates(ostream& out,
			     unsigned int n, 
			     RxnReactionRatesClass *rateclass, 
			     String property,
			     String delimitor,
			     bool chemkinreverse);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnThirdBodyClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataReactionRatesClass
*/
class RxnThirdBodyClass : public RxnReactionRatesClass
{
public:
  RxnThirdBodyClass();
  RxnThirdBodyClass(const RxnThirdBodyClass& data);
  RxnThirdBodyClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C RxnDataReactionRateHiLow  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ReactionRateHiLow class definitions
**
**  REMARKS
**    Inheirits BaseDataReactionRates
*/
class RxnDataReactionRateHiLow : public RxnDataReactionRates
{
  RxnDataReactionRates *HiRate;
  BaseDataDoubleVector *Parameters;
public:
  RxnDataReactionRateHiLow();
  RxnDataReactionRateHiLow(const RxnDataReactionRateHiLow& data);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  RxnDataReactionRates *getHiRate();
  RxnDataReactionRates *getLowRate();
  BaseDataDoubleVector *getParameters();
  void setHiRate(RxnDataReactionRates *r) {HiRate = r;};
  void setParameters(BaseDataDoubleVector *p) {Parameters = p;};
  bool WriteOutRates(ostream& out,
		     unsigned int n, 
		     RxnReactionRatesClass *rateclass, 
		     String property,
		     String delimitor,
		     bool chemkinreverse);
};
/*C RxnReactionRateHiLowClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataReactionRatesClass
*/
class RxnReactionRateHiLowClass : public RxnReactionRatesClass
{
  unsigned int ParameterSize;
public:
  RxnReactionRateHiLowClass();
  RxnReactionRateHiLowClass(const RxnReactionRateHiLowClass& data);
  RxnReactionRateHiLowClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS;
  unsigned int getParameterSize();
};
/*C RxnDataReaction  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the Reaction class definitions
**
**  REMARKS
**    Inheirits BaseDataSetOfObjects
*/
class RxnDataReaction : public BaseDataSetOfObjects
{
  RxnDataMoleculeSet *Reactants;
  RxnDataMoleculeSet *Products;
public:
  RxnDataReaction();
  RxnDataReaction(const RxnDataReaction& data);
  ~RxnDataReaction();

  BaseDataKeySet& getReactantNames();
  BaseDataKeySet& getProductNames();
  BaseDataKeySet& getInternalReactantNames();
  BaseDataKeySet& getInternalProductNames();
  String getReactantName(String& internalname);
  String getProductName(String& internalname);
  virtual bool ReadRxnFileReaction(istream& in, String& dbasetype,
				   RxnReactionClass *rxnclass,
				   RxnDataMolecularStructuresDataBase *dbase,
				   RxnMolecularStructuresDataBaseClass *dbaseclass);
  bool WriteReactionEquation(ostream& out, bool forward, bool chemkin, bool ThirdBody, String& Eq, 
			     String& ThirdBodyS, String& shortname);
  bool WriteMoleculeSet(ostream& out, bool chemkin, bool reactant, BaseDataKeySet& internal, bool ThirdBody, 
			String& ThirdBodyS, String& shortname);
  String CreateStandardName(bool chemkin, bool reactant, String& name,String& shortname);
  STANDARD_VIRTUAL_METHODS_OBJECT;
  RxnDataReactionRates *GetRateConstants(String& ratename, bool forward,
					 String& property, RxnReactionRatesClass *rxnclass);
  bool setupReactantMolecules(BaseDataSetOfObjects *set,
			    String& dbasetype,
			    RxnDataMolecularStructuresDataBase *dbase,
			    RxnMolecularStructuresDataBaseClass *dbaseclass);
  bool setupProductMolecules(BaseDataSetOfObjects *set,
			   String& dbasetype,
			   RxnDataMolecularStructuresDataBase *dbase,
			   RxnMolecularStructuresDataBaseClass *dbaseclass);
  bool setupReactionMolecules(BaseDataSetOfObjects *set,
			      String& dbasetype,
			      RxnDataMolecularStructuresDataBase *dbase,
			      RxnMolecularStructuresDataBaseClass *dbaseclass);
  bool FillReactionWithMolecules(String& dbasetype,
				 RxnDataMolecularStructuresDataBase *dbase,
				 RxnMolecularStructuresDataBaseClass *dbaseclass);
  RxnDataMoleculeSet *getReactants();
  RxnDataMoleculeSet *getProducts();
  void setReactants(RxnDataMoleculeSet *mols);
  void setProducts(RxnDataMoleculeSet *mols);
  unsigned int getNumberOfReactants();
  unsigned int getNumberOfProducts();
  bool withinMoleculeSet(BaseDataKeyWords &moleculelist);
};
/*C RxnReactionClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataSetOfObjectsClass
*/
class RxnReactionClass : public DataSetOfObjectsClass
{
  RxnMoleculeSetClass *MoleculeSetClass;
  RxnReactionRatesClass *RatesClass;
  String NameInInstance;
  String StandardForwardName;
  String StandardReverseName;
public:
  RxnReactionClass();
  RxnReactionClass(const RxnReactionClass& data);
  RxnReactionClass(const int id, 
		    const String& name,
		    const String& descr);
  ~RxnReactionClass();
  STANDARD_VIRTUAL_METHODS_CLASS;
  RxnMoleculeSetClass *getMoleculeSetClass();
  RxnReactionRatesClass *getRatesClass();
  String& getNameInInstance();
  String& getStandardForwardName();
  String& getStandardReverseName();
  void setNameInInstance(String& name);
  void setStandardForwardName(String& name);
  void setStandardReverseName(String& name);
  DataSetOfObjectsClass *PointerToAllowedClasses();
};
/*C RxnDataReactionMoleculeCorrespondences  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ReactionMoleculeCorrespondences class definitions
**
**  REMARKS
**    Inheirits BaseDataReaction
*/
class RxnDataReactionMoleculeCorrespondences : public RxnDataReaction
{
  VectorSimple<String> OrderedReactantAtoms;
  VectorSimple<String> OrderedReactantMolecules;
  VectorSimple<String> OrderedProductAtoms;
  VectorSimple<String> OrderedProductMolecules;

  vector<int> *ReactantsBondCorrespondenceVector;
  VectorSimple<int> ReactantBondChange; 
  vector<int> *ProductBondCorespondenceVector;
  VectorSimple<int> ProductBondChange; 

  BaseDataSetOfObjects CorrespondencePairs;
  BaseDataSetOfObjects AtomCorrespondences;
public:
  RxnDataReactionMoleculeCorrespondences();
  RxnDataReactionMoleculeCorrespondences(const RxnDataReactionMoleculeCorrespondences& data);
  ~RxnDataReactionMoleculeCorrespondences();
  virtual bool ReadRxnFileReaction(istream& in, String& dbasetype,
				   RxnReactionClass *rxnclass,
				   RxnDataMolecularStructuresDataBase *dbase,
				   RxnMolecularStructuresDataBaseClass *dbaseclass);
  bool ReadCorrespondences(istream& in,
			   String& dbasetype,
			   RxnDataMolecularStructuresDataBase *dbase,
			   RxnMolecularStructuresDataBaseClass *dbaseclass);
  bool ReadInReactantAtomCorrespondences(StreamObjectInput &str,
					 String& internalname,
					 RxnDataSimpleMolecule *mol);
  String GeneratePairName(unsigned int c);
  BaseDataPair *InitialReactantPair(unsigned int c, String& molname, String& atomname);
  bool AddCorrespondencePair(BaseDataPair *p);
  bool AddAtomCorrespondence(String& molname,
			     String& atomname,
			     String& pairname);
  BaseDataPair *GetCorrespondencePair(unsigned int c);
  bool ReadInProductAtomCorrespondences(StreamObjectInput& str,
					String& internalname,
					RxnDataSimpleMolecule *mol);
  bool StoreProductPair(BaseDataPair *pset,String& molname,String atomname);
  bool ReadInBondCorrespondences(vector<int> *vec,
				 StreamObjectInput& str,
				 RxnDataSimpleMolecule *mol);
  bool ReadRateConstantInfo(istream& in,String& firstline,
			    RxnReactionClass *rxnclass);

  STANDARD_VIRTUAL_METHODS_OBJECT;
};
/*C RxnReactionMoleculeCorrespondencesClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataReactionClass
*/
class RxnReactionMoleculeCorrespondencesClass : public RxnReactionClass
{
public:
  RxnReactionMoleculeCorrespondencesClass();
  RxnReactionMoleculeCorrespondencesClass(const RxnReactionMoleculeCorrespondencesClass& data);
  RxnReactionMoleculeCorrespondencesClass(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};

/*C RxnDataReactionStructuresDataBase  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the ReactionStructuresDataBase class definitions
**
**  REMARKS
**    Inheirits BaseDataMolecularStructuresDataBase
*/
class RxnDataReactionStructuresDataBase : public RxnDataMolecularStructuresDataBase
{
  BaseDataDataBaseInformation *DBReaction;
  BaseDataDataBaseInformation *DBPatterns;
public:
  RxnDataReactionStructuresDataBase();
  RxnDataReactionStructuresDataBase(const RxnDataReactionStructuresDataBase& data);
  virtual bool Initialize(DataDataBaseInformationClass *dbclass);
  virtual BaseDataDataBaseInformation *getDatabaseInfo(String& dbtype);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C RxnReactionStructuresDataBaseClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataMolecularStructuresDataBaseClass
*/
class RxnReactionStructuresDataBaseClass : public RxnMolecularStructuresDataBaseClass
{
  DataDataBaseInformationClass *MoleculeDBClass;
public:
  RxnReactionStructuresDataBaseClass();
  RxnReactionStructuresDataBaseClass(const RxnReactionStructuresDataBaseClass& data);
  RxnReactionStructuresDataBaseClass(const int id, 
				     const String& name,
				     const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};
/*C ReactionSystemBase  . . . . . . . . . . . . . . . . . . .  Reaction Base
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class RxnSystemBase : public MoleculeSystemBase
{
 public:
  RxnSystemBase(int argc, char *argv[]);
  virtual ~RxnSystemBase() {};
  virtual void EncodeDecodeObjectsSetUp();
  virtual void StandardObjectsSetUp();
  virtual void Initialization();
  virtual void CommandSetUp();
};


/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/


#endif
