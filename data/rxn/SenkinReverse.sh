#!/bin/csh
#
# FILE     RxnCreate
# PACKAGE  Reaction
# AUTHOR  Edward S. Blurock 
#------------------------------------------------------------------------------
# Calculate the reverse reaction constants 
#    for the SenkinExample Mechanism
#
Rxn CalcReverse 'SE-H2+O2=OH+OH' Reverse
Rxn CalcReverse 'SE-OH+H2=H2O+H' Reverse
Rxn CalcReverse 'SE-O+OH=O2+H' Reverse
Rxn CalcReverse 'SE-O+H2=OH+H' Reverse
Rxn CalcReverse 'SE-H+O2+M=HO2+M' Reverse
Rxn CalcReverse 'SE-OH+HO2=H2O+O2' Reverse
Rxn CalcReverse 'SE-H+HO2=OH+OH' Reverse
Rxn CalcReverse 'SE-O+HO2=O2+OH' Reverse
Rxn CalcReverse 'SE-OH+OH=O+H2O' Reverse
Rxn CalcReverse 'SE-H+H+M=H2+M' Reverse
Rxn CalcReverse 'SE-H+H+H2=H2+H2' Reverse
Rxn CalcReverse 'SE-H+H+H2O=H2+H2O' Reverse
Rxn CalcReverse 'SE-H+OH+M=H2O+M' Reverse
Rxn CalcReverse 'SE-H+O+M=OH+M' Reverse
Rxn CalcReverse 'SE-O+O=O2' Reverse
Rxn CalcReverse 'SE-H+HO2=H2+O2' Reverse
Rxn CalcReverse 'SE-HO2+HO2=H2O2+O2' Reverse
Rxn CalcReverse 'SE-H2O2+M=OH+OH+M' Reverse
Rxn CalcReverse 'SE-H2O2+H=HO2+H2' Reverse
Rxn CalcReverse 'SE-H2O2+OH=H2O+HO2' Reverse
Rxn CalcReverse 'SE-O+N2=NO+N' Reverse
Rxn CalcReverse 'SE-N+O2=NO+O' Reverse
Rxn CalcReverse 'SE-OH+N=NO+H' Reverse
