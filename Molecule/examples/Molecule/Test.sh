#! /usr/bin/tcsh -f

set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts
set GENERIC         = $REACTION_BASE/data/generic


#otest xxx Initial test    -1 Read algClass.inp alg.inp 0
#otest xxx Change test     0   SetAlgorithmClass TestAlgorithmRun
#otest xxx Change test     0   Read $GENERIC/expClass.inp $GENERIC/exp.inp 0

#otest xxx Operate test    0 Read $MOLECULEINPUTS/MoleculeClass.inp $MOLECULEINPUTS/Molecule.inp 0
#otest xxx Change test     1 Read $MOLECULEINPUTS/MolDbaseClass.inp $MOLECULEINPUTS/MolDbase.inp 0
#
#otest xxx Operate test     1 ReadMol Molecule $MOLECULEDATA/simple/C0HO.sdf
#otest xxx Experiment test 2 Print Instance

#otest xxx Operate test 2 Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/C0HO.inp 0
#otest xxx Experiment test 3 Print Instance hydrogen Chemkin

#otest xxx Operate test 3 Read MolExpClass.inp MolExp.inp 0
#otest xxx Change  test 4 RunAlgorithm Operation 0
#otest xxx Experiment test 4 Print Instance hydrogen Molecule
#otest xxx Operate test 4 Read MoleculeReadClass.inp MoleculeRead.inp 0
otest xxx Experiment test 5 Exists Molecule ReadInNames
