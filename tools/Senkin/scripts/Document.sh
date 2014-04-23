#! /bin/csh
# ---------------------------------------------------------------------------
#
#  This script documents the run in latex
#
# ---------------------------------------------------------------------------
set verbose

set SENKINJOB    = $Reaction/bin/Senkin
set GNUPLOTJOB   = gnuplot

if( $#argv <  6) then
    echo "Usage: Document RunName MechanismName"
    echo "      RunName:       The name of the particular run"
    echo "      MechanismName: The name of the mechanism used"
    echo "      Times: A list of at leat 4 times"
    exit(1)
endif

set MECHANISM_NAME   = $2
set RUN_NAME         = $1
if($#argv < 7) then
    set TIMES            = $3" "$4" "$5" "$6
else if($#argv < 8) then
    set TIMES            = $3" "$4" "$5" "$6" "$7
else if($#argv < 9) then
    set TIMES            = $3" "$4" "$5" "$6" "$7" "$8
else if($#argv < 10) then
    set TIMES            = $3" "$4" "$5" "$6" "$7" "$8" "$9
endif
echo The Mechansim Name:   $MECHANISM_NAME
echo The Run Name:         $RUN_NAME
echo The Times:            $TIMES

set OUTPUT_ROOT      = $RUN_NAME
echo MAIN

set MAIN             = $RUN_NAME
set MAIN_TEX         = $MAIN'.tex'

set INTERPRET        = $RUN_NAME'Intr'
set INTERPRET_TEX    = $INTERPRET'.tex'

set GNUPLOT          = $RUN_NAME'Gnu'
set GNUPLOT_TEX      = $GNUPLOT'.tex'
set GNUPLOT_PLOT     = $GNUPLOT'.plt'

set REACTIONS        = $OUTPUT_ROOT
set REACTIONS_TEX    = $REACTIONS'Rxn.tex'

set PCA              = $RUN_NAME
set PCA_TEX          = $PCA'pca.tex'
set PCAPLOT_TEX      = $PCA'gnu.tex'
set PCA_GNUPLOT      = $PCA'gnu.plt'

# ---------------------------------------------------------------------------
#
# Concentrations (in LaTeX and in GnuPlot Form)
# 1. Senkin with 'Interpret' is run for the conc. table
# 2. Senkin with 'GnuPlot' for the gnuplot tex and scripts
# 3. gnuplot to generate the eps files for each reaction
#
#
$SENKINJOB Interpret $RUN_NAME $MECHANISM_NAME\
$INTERPRET $TIMES

$SENKINJOB GnuPlot $RUN_NAME $MECHANISM_NAME $GNUPLOT
$GNUPLOTJOB $GNUPLOT_PLOT

# ---------------------------------------------------------------------------
# 
# Principle Component Analysis
#
$SENKINJOB PCA $RUN_NAME $MECHANISM_NAME $PCA
$GNUPLOTJOB $PCA_GNUPLOT
# ---------------------------------------------------------------------------
#
# Create the Chapter
#
cat << EOF > $MAIN_TEX
%%%==========================================================================
\chapter{$MAIN}
\section{Rates Of Reaction}
%%% Rates
\input{$INTERPRET_TEX}

%%% GnuPlots
\input{$GNUPLOT_TEX}

\section{Principle Component Analysis}
%%% PCA Table
\input{$PCA_TEX}

%%% PCA Plots
\input{$PCAPLOT_TEX}

EOF






