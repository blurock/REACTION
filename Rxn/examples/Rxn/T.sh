#! /usr/bin/tcsh -f


set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA   = $REACTION_BASE/data/rxn
set REACTIONINPUTS = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

set GENERIC         = $REACTION_BASE/data/generic

rm *.dbf

otest xxx Initial test     0 Read $MOLECULEINPUTS/MoleculeClass.inp $MOLECULEINPUTS/Molecule.inp 0
otest xxx Change  test     1 Read $MOLECULEINPUTS/MolDbaseClass.inp $MOLECULEINPUTS/MolDbase.inp 0
otest xxx Operate test     1 Read $REACTIONINPUTS/ReactionClass.inp $REACTIONINPUTS/Reaction.inp 0
otest xxx Change  test     2 Read $REACTIONINPUTS/RxnDbaseClass.inp $REACTIONINPUTS/RxnDbase.inp 0

otest xxx Operate test     2 Read ReactionConstantsClass.inp $REACTIONDATA/baulch/Baulch.rxn 0
otest xxx Experiment test  3 Print Instance O+H2=OH+H
