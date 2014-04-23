#! /usr/bin/tcsh -f

otest xxx Initial test 0 Read DbaseClass.inp Dbase.inp 0
#otest xxx Experiment test 1 Print Class
#otest xxx Experiment test 1 Print Attribute Dbase
#otest xxx Operate test 1 DbaseStart Dbase

otest xxx Experiment test 1 DbaseTest Dbase Write X
otest xxx Experiment test 1 DbaseTest Dbase Read X
