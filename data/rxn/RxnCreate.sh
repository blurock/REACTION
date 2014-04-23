#!/bin/csh
#
# FILE     RxnCreate
# PACKAGE  Reaction
# AUTHOR  Edward S. Blurock 
#------------------------------------------------------------------------------
set verbose on
$Reaction/bin/Rxn ReadFile Reaction SenkinExample.rxn
$Reaction/bin/Rxn ReadFile Reaction barbieri1.rxn
$Reaction/bin/Rxn ReadFile Reaction barbieri2.rxn
