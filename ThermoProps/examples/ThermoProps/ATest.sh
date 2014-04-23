#! /usr/bin/tcsh -f

set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts
set GENERIC         = $REACTION_BASE/data/generic

otest xxx Initial test 0 Read algs/algClass.inp algs/alg.inp 0
otest xxx Change test  1   SetAlgorithmClass TestAlgorithmRun

otest xxx Operate test 1 Read tests/ConversionsClass.inp tests/Conversions.inp 0
#otest xxx Experiment test 3 Print Attribute UnitConversions

otest xxx Change test     2 Read $MOLECULEINPUTS/MoleculeClass.inp $MOLECULEINPUTS/Molecule.inp 0
otest xxx Change test     2 Read $MOLECULEINPUTS/MolDbaseClass.inp $MOLECULEINPUTS/MolDbase.inp 0

otest xxx Change test    2 ReadMol Molecule $MOLECULEDATA/simple/C0HO.sdf

otest xxx Operate test 2 Read algs/PolyClass.inp $REACTION_BASE/data/mol/benson/Benson0.inp 0
#otest xxx Experiment test 3 Print Instance ethane PolyCp

otest xxx Operate test 3 Read algs/ThermoArgsClass.inp algs/ThermoArgs.inp 0

#otest xxx Operate test 4 RunAlgorithm ThermoMatrix 0
#otest xxx Experiment test 5 Print Instance Ethane MatrixObject

otest xxx Operate test 4 Read algs/ChemkinClass.inp algs/Chemkin.inp 0
otest xxx Change  test 5 RunAlgorithm ChemkinConversion 0


#otest xxx Operate test 5 Read algs/TransferClass.inp algs/Transfer.inp 0
#otest xxx Operate test 6 RunAlgorithm MoveIt 0


#otest xxx Experiment test 5 ConvertToChemkin InstanceNameList\
#                                             StandardChemkinThermo\
#                                             PolyCp\
#                                             CpCalorie EnthalpyCalorie EntropyCalorie
