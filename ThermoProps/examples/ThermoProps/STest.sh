#! /usr/bin/tcsh -f

otest xxx Initial test 0 Read tests/LitClass.inp tests/Lit.inp 0
otest xxx Experiment test 1 Print Attribute Literature

otest xxx Operate test 1 Read tests/FactorsClass.inp tests/Factors.inp 0
otest xxx Experiment test 2 Print Attribute Factors
otest xxx Experiment test 2 Print Attribute Meter

otest xxx Operate test 2 Read tests/ConversionsClass.inp tests/Conversions.inp 0
otest xxx Experiment test 3 Print Attribute UnitConversions

otest xxx Operate test 3 Read tests/ThermoClass.inp tests/Thermo.inp 0
otest xxx Experiment test 4 Print Attribute AThermoProperty

otest xxx Operate test 4 Read tests/ChemkinClass.inp tests/Chemkin.inp 0
otest xxx Experiment test 5 Print Attribute ChemkinOH

otest xxx Experiment test 5 ChemkinToChemkin NEWDAT.txt KiloCalorie KiloCalorie KiloCalorie
#otest xxx Operate test 5 Read tests/PolyClass.inp tests/Poly.inp 0
#otest xxx Experiment test 6 Print Attribute PolyCpEthane 

#otest xxx Operate test 6 Read algs/ThermoArgsClass.inp algs/ThermoArgs.inp 0

#otest xxx Experiment test 6 ConvertToChemkin InstanceNameList PolyCp CpCalorie EnthalpyCalorie EntropyCalorie
