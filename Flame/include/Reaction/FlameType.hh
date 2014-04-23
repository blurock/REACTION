/*  FILE     FlameType.hh
**  PACKAGE  Flame
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "Flame" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 CoreObjects Project, RISC Linz
*/
 
#ifndef CoreObjects_FLAMETYPE_HH
#define CoreObjects_FLAMETYPE_HH

/*C BaseDataFlameSensitivityData  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the FlameSensitivityData class definitions
**
**  REMARKS
**    Inheirits BaseDataInstanceDoubleMatrix
*/
class BaseDataFlameSensitivityData : public BaseDataInstanceDoubleMatrix
{
public:
  BaseDataFlameSensitivityData();
  BaseDataFlameSensitivityData(const BaseDataFlameSensitivityData& data);

  bool AddAsInstances(BaseDataSetOfInstances &instances,
		      DataSetOfInstancesClass &instclass);
  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C DataFlameSensitivityDataClass . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits DataDoubleMatrixClass
*/
class DataFlameSensitivityDataClass : public DataInstanceDoubleMatrixClass
{
public:
  String InstanceBaseName;
  String ProgressVariable;


  DataFlameSensitivityDataClass();
  DataFlameSensitivityDataClass(const DataFlameSensitivityDataClass& data);
  DataFlameSensitivityDataClass(const int id, 
		    const String& name,
		    const String& descr);
  void Initialize();
  STANDARD_VIRTUAL_METHODS_CLASS
};


/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#endif
