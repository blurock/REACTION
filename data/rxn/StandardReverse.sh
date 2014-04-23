#!/bin/csh
#
# FILE     StandardReverse.sh
# PACKAGE  Reaction
# AUTHOR  Edward S. Blurock 
#------------------------------------------------------------------------------
# Calculate the reverse reaction constants 
#    for the Dryer-CH2O-92 Mechanism
#
Rxn CalcReverse 'STD-HCHO+M=HCO+H+M' Reverse
Rxn CalcReverse 'STD-HCHO+M=CO+H2+M' Reverse
Rxn CalcReverse 'STD-HCHO+O2=HCO+HO2' Reverse
Rxn CalcReverse 'STD-HCHO+H=HCO+H2' Reverse
Rxn CalcReverse 'STD-HCHO+O=HCO+OH' Reverse
Rxn CalcReverse 'STD-HCHO+OH=HCO+H2O' Reverse
Rxn CalcReverse 'STD-HCHO+HO2=HCO+H2O2' Reverse
Rxn CalcReverse 'STD-HCO+M=CO+H+M' Reverse
Rxn CalcReverse 'STD-HCO+O2=CO+HO2' Reverse
Rxn CalcReverse 'STD-HCO+H=CO+H2' Reverse
Rxn CalcReverse 'STD-HCO+O=CO+OH' Reverse
Rxn CalcReverse 'STD-HCO+O=CO2+H' Reverse
Rxn CalcReverse 'STD-HCO+OH=CO+H2O' Reverse
Rxn CalcReverse 'STD-HCO+HO2=OH+H+CO2' Reverse
Rxn CalcReverse 'STD-CO+OH=CO2+H' Reverse
Rxn CalcReverse 'STD-CO+HO2=CO2+OH' Reverse
Rxn CalcReverse 'STD-CO+O+M=CO2+M' Reverse
Rxn CalcReverse 'STD-CO+O2=CO2+O' Reverse
Rxn CalcReverse 'STD-H+O2=OH+O' Reverse
Rxn CalcReverse 'STD-H2+O=OH+H' Reverse
Rxn CalcReverse 'STD-OH+OH=H2O+O' Reverse
Rxn CalcReverse 'STD-OH+H2=H2O+H' Reverse
Rxn CalcReverse 'STD-H2O2+OH=H2O+HO2' Reverse
Rxn CalcReverse 'STD-H2O2+H=H2O+OH' Reverse
Rxn CalcReverse 'STD-HO2+HO2=H2O2+O2' Reverse
Rxn CalcReverse 'STD-H+HO2=OH+OH' Reverse
Rxn CalcReverse 'STD-H+HO2=O2+H2' Reverse
Rxn CalcReverse 'STD-HO2+OH=O2+H2O' Reverse
Rxn CalcReverse 'STD-O+HO2=OH+O2' Reverse
Rxn CalcReverse 'STD-H+H2O2=H2+HO2' Reverse
Rxn CalcReverse 'STD-O2+M=O+O+M' Reverse
Rxn CalcReverse 'STD-H2+M=H+H+M' Reverse
Rxn CalcReverse 'STD-H+OH+M=H2O+M' Reverse
Rxn CalcReverse 'STD-H2O2+M=OH+OH+M' Reverse
Rxn CalcReverse 'STD-OH+M=O+H+M' Reverse
Rxn CalcReverse 'STD-H+O2+M=HO2+M' Reverse
