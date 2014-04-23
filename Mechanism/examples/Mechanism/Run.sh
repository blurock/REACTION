#! /usr/bin/tcsh -f

set MOLECULEDATA    = $REACTION_BASE/data/mol
set MOLECULEINPUTS  = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA    = $REACTION_BASE/data/rxn
set REACTIONINPUTS  = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

set MECHDATA        = $REACTION_BASE/data/mech
set MECHINPUTS      = $MECHDATA/inputs
set MECHSCRIPTS     = $MECHDATA/scripts

set GENERIC         = $REACTION_BASE/data/generic

rm *.dbf

otest xxx Initial test     -1 Read algClass.inp alg.inp 0
otest xxx Change  test     0 SetAlgorithmClass TestAlgorithmRun
otest xxx Operate test     0 Read    $MOLECULEINPUTS/MoleculeClass.inp $MOLECULEINPUTS/Molecule.inp 0
otest xxx Change test     1 Read    $MOLECULEINPUTS/MolDbaseClass.inp $MOLECULEINPUTS/MolDbase.inp 0

otest xxx Operate test     1 Read    $REACTIONINPUTS/ReactionClass.inp $REACTIONINPUTS/Reaction.inp 0
otest xxx Change test     2 Read    $REACTIONINPUTS/RxnDbaseClass.inp $REACTIONINPUTS/RxnDbase.inp 0

otest xxx Operate test     2 Read    $MECHINPUTS/MechanismClass.inp $MECHINPUTS/Mechanism.inp 0
otest xxx Change test     3 Read    $MECHINPUTS/MechDbaseClass.inp $MECHINPUTS/MechDbase.inp 0
otest xxx Change test     3 Read    $MOLECULEDATA/inputs/MoleculeChemkinClass.inp empty.inp 0
