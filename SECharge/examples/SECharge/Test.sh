#! /usr/bin/tcsh -f

set MOLECULEDATA     = $REACTION_BASE/data/mol
set MOLECULEINPUTS   = $MOLECULEDATA/inputs
set MOLECULESCRIPTS  = $MOLECULEDATA/scripts
set GENERIC          = $REACTION_BASE/data/generic

#  Set up a set of molecules (three carbon atom molecules)
otest xxx Initial    test 0 Read ChargeAlgDefClass.inp ChargeAlgDef.inp 0
otest xxx Change     test 1 SetAlgorithmClass TestAlgorithmRun
$MOLECULESCRIPTS/MoleculeSetup.sh test 1
otest xxx Operate    test 2  ReadMol Molecule $MOLECULEDATA/simple/C3HO.sdf
otest xxx Experiment test 3 Print Instance

# Calculate the Semi-Empirical values
$MOLECULESCRIPTS/SECharge.sh test 3 StandardMolecule propane 1-propene-3-ol 1-propyl-radical
otest xxx Experiment test 4 Print Instance propane
otest xxx Experiment test 4 Print Instance propane Molecule

