#! /bin/csh
# ---------------------------------------------------------------------------
#
#  This is a run for a given mechanism for a given temperature
#
# ---------------------------------------------------------------------------

#set verbose
set SENKINJOB    = $Reaction/bin/Senkin
set MAIN_TEX     = main.tex
set MAIN         = main
if( $#argv <  5) then
    echo "Usage: MechanismName JobName [options] 
    echo "      MechanismName: The name of the mechanism used"
    echo "      JobName:      The job name (root of the output files)"
    echo "Options:"
    echo "-Carrier add a carrier molecule"
    echo "-Pressure Pressure (default 1.0)
    echo "-T Add a temperature
    echo "-C Add a molecule pressure pair
    exit(1)
endif

# ---------------------------------------------------------------------------
# Initial Assignments for command line
# ---------------------------------------------------------------------------

set MECHANISM_NAME   = $1
set JOBNAME          = $2
shift
shift
# ---------------------------------------------------------------------------
# Interpret Command Line
#
# Each -Carrier    : adds a Carrier molecule
# Each -T          : adds a Temperature
#      -Pressure   : changes the default pressure
#      -C          : Molecules and initial concentrations
#      -Time       : final time
#      -Dir        : default directory
#
# Defaults:
#    final time = .001
#    pressure   = 1.0
#    directory  = ./
#
# ---------------------------------------------------------------------------
set FINALTIME    = .001
set PRESSURE  = 1.0
set DIRECTORY    = .
set CARRIER
set CONDITIONS
set TEMPERATURES
while ($#argv > 1)
    echo $1
    switch($1)
    case -Pressure:
    set PRESSURE = $2
    shift
    shift
    breaksw
    case -Carrier:
    set CARRIER = ($CARRIER $2)
    shift
    shift
    breaksw
    case -Dir:
    set DIRECTORY = $2
    shift
    shift
    breaksw
    case -Time:
    set FINALTIME = $2
    shift
    shift
    breaksw
    case -T:
    set TEMPERATURES = ($TEMPERATURES $2)
    shift
    shift
    breaksw
    case -C:
    set CONDITIONS = ($CONDITIONS $2 $3)
    shift
    shift
    shift
    breaksw
    default:
    break
    endsw
end

echo $CONDITIONS
echo $PRESSURE
echo $CARRIER
echo $TEMPERATURES
echo $CONDITIONS
# ---------------------------------------------------------------------------
# Change to default directory (create if not there)
# ---------------------------------------------------------------------------
if(! -d $DIRECTORY) then
    mkdir $DIRECTORY
endif
pushd $DIRECTORY
# ---------------------------------------------------------------------------
#
# Print out the reaction constants
#
#$SENKINJOB RxnOutput $MECHANISM_NAME $REACTIONS

# ---------------------------------------------------------------------------
# Loop through all the temperatures and run the jobs
# ---------------------------------------------------------------------------
foreach temp ($TEMPERATURES)
set TEMPJOBNAME = 'Temp='$temp

echo Command: $SENKINJOB SetUp $JOBNAME $MECHANISM_NAME $CARRIER
$SENKINJOB SetUp $TEMPJOBNAME $MECHANISM_NAME $CARRIER

echo Command: $SENKINJOB RunJob $JOBNAME $MECHANISM_NAME $temp $PRESSURE $FINALTIME $CONDITIONS
$SENKINJOB RunJob $TEMPJOBNAME $MECHANISM_NAME $temp $PRESSURE $FINALTIME $CONDITIONS
end

# ---------------------------------------------------------------------------
#
# Create the main tex file (to compile from)
#
cat << EOF > $MAIN_TEX
\documentclass{article}
\usepackage{epsf}
\usepackage{rotating}

\begin{document}

%%% The Reactions
\clearpage

cat << EOF >> $MAIN_TEX

\end{document}

# ---------------------------------------------------------------------------
#
# Compile the LaTeX file to Postscript file
#
latex $MAIN
dvips $MAIN

#rm $MAIN.dvi
#rm *.tex
#rm *.aux
#rm For*.ps
#rm Rev*.ps
#rm *.plt
