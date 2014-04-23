#! /usr/bin/tcsh -f
set PROGRAM         = Reaction.exe

set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA   = $REACTION_BASE/data/rxn
set REACTIONINPUTS = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

set MECHANISMDATA    = $REACTION_BASE/data/mech
set MECHANISMINPUTS  = $MECHANISMDATA/inputs
set MECHANISMSCRIPTS = $MECHANISMDATA/scripts

set GENERIC         = $REACTION_BASE/data/generic

if( $#argv <  8) then
echo   " Usage: $0 SaveFileRoot  SaveFileCount [ProgramName Initialize]"
echo   "        SaveFileRoot:          The rootname of the save file"
echo   "        SaveFileCount:         The count of the current save file"
echo   "        MechanismName:         The name of the mechanism to print"
echo   "        Output Type:           PrintStandard, PrintLaTeX"
echo   "        ReactionUnits:         The units (for example calories-cm3)"
echo   "        EnthalpyUnits:         The units (for example GCKiloCalories-Mole)"
echo   "        EntropyUnits:          The units (for example GCKiloCalories-Mole)"
echo   "        CpUnits:               The units (for example Entropy)"
echo   "        ProgramName:           The executable to use (default Reaction.exe)"
echo   ""
echo   " This increments the SaveFileCount by 1"
exit(1)
endif
set PROGRAM = Reaction.exe
if($#argv >=  9) then
    set PROGRAM = $9
endif

set SAVE                 = $1
set SAVECOUNT            = $2
set MECHNAME             = $3
set TYPE                 = $4
set REACTIONUNITS        = $5
set CPUNITS              = $6
set ENTHALPYUNITS        = $7
set ENTROPYUNITS         = $8

cat <<EOF >! MechanismPrint.inp
Print Mechanism Print the mechanism
Attributes:
%% ------------------------------------
%% PrintMechanism   KeySet
%% ------------------------------------
$MECHNAME
StandardMechanism 
$REACTIONUNITS
$CPUNITS      
$ENTHALPYUNITS
$ENTROPYUNITS 
$MECHNAME
END
%% ------------------------------------
%% SubMolecules   KeyWords
%% ------------------------------------
EOF
if(-f SubMolecules.txt) then
    cat SubMolecules.txt >> MechanismPrint.inp
endif

cat <<EOF >> MechanismPrint.inp
END
%% ------------------------------------
%% No Instances
%% ------------------------------------
END

EOF

cat <<EOF > prog.inp
Read $MECHANISMINPUTS/MechPrintClass.inp MechanismPrint.inp 0
RunAlgorithm $TYPE 0
Print
EOF
$PROGRAM xxx Experiment $SAVE  $SAVECOUNT < prog.inp

#$PROGRAM xxx Operate $SAVE  $SAVECOUNT Read $MECHANISMINPUTS/MechPrintClass.inp MechanismPrint.inp 0
#@ SAVECOUNT++
#$PROGRAM xxx Experiment $SAVE  $SAVECOUNT RunAlgorithm $TYPE 0
