#! /usr/bin/tcsh -f

#InitializeData.sh test 0 otest
#otest xxx Operate test 0 Read /usr/local/Software/Interface/data/initialize/StandardAlgorithmsClass.inp\
#                              /usr/local/Software/Interface/data/initialize/StandardAlgorithms.inp 0
#otest xxx Operate test 1 Read /usr/local/Software/Interface/data/initialize/StandardLogicClass.inp\
#                              /usr/local/Software/Interface/data/initialize/StandardLogic.inp 0
#otest xxx Operate test 2 Read /usr/local/Software/Interface/data/initialize/StandardExpressionsClass.inp\
#                              /usr/local/Software/Interface/data/initialize/StandardExpressions.inp 0
#otest  xxx Operate test 3 Flame SpeciesSensitivity.txt 0.000001
#otest  xxx Experiment test 4 Print Attribute MatrixObject
#otest  xxx Operate    test 4 MakeInstanceFromMatrix MatrixObject

cat <<EOF > Start.inp
Read /usr/local/Software/Interface/data/initialize/StandardAlgorithmsClass.inp /usr/local/Software/Interface/data/initialize/StandardAlgorithms.inp 0
Read /usr/local/Software/Interface/data/initialize/StandardLogicClass.inp /usr/local/Software/Interface/data/initialize/StandardLogic.inp 0
Read /usr/local/Software/Interface/data/initialize/StandardExpressionsClass.inp     /usr/local/Software/Interface/data/initialize/StandardExpressions.inp 0
Flame SpeciesSensitivity.txt 0.000001
Print Attribute MatrixObject
MakeInstanceFromMatrix MatrixObject
END
EOF
otest xxx Operate test 0 < Start.inp

#otest   xxx Experiment test 2 Write Instance All \
#CO2 H O2 CH3     OH      H2      CH4     H2O     CO\
#CO2     3-CH2   C2H3    O       C2H2    HO2     C3H6 \
#C2H4    CH      CHCO    C3H7    C2H6    C2H     C2H5 \
#CH2O    CHO     C3H5    C3H4    C3H3    H2O2    IPRO    C3H8 
#otest  xxx Operate    test 5 Read DistInfoClass.inp DistInfo.inp 0
#otest  xxx Change     test 6 RunAlgorithm DistributionAlg 0

#otest xxx Experiment test 3 DistributionReport report Distributions
#otest xxx Experiment test 3 MakeCriticalPoints critpoints Distributions
#otest xxx Operate test 3 Read critClass.inp critpointsSetUp.dat 0
#otest xxx Change  test 4 RunGoal PartitionsFromCriticalPoints 0
#otest xxx Operate test 4 Read CobwebSetupClass.inp CobwebSetup.inp 0
#otest xxx Change  test 5 RunAlgorithm InstanceSetUpAlg 0
#otest xxx Change  test 5 RunAlgorithm CobwebClusterAlg 1
