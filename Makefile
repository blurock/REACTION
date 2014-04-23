REACTION=\
	StaticAtom MolAtom MolBond Molecule Rxn SECharge ThermoTables MolStats ThermoProps Utilities EquilibriumConst\
        Mechanism MechLumping Flame MechanismGraph FullReaction

#  Lstops      Reaction  Chemkin     Flame  Senkin  GenerateID MechGraph RxnMech SenkinRates

space:=$(empty) $(empty)
sREACTION:=$(space)$(REACTION)

compileprefix:=c
mergeprefix:=m

cREACTION=$(subst $(space),$(space)$(compileprefix),$(sREACTION))
mREACTION=$(subst $(space),$(space)$(mergeprefix),$(sREACTION))

all: mergeReaction compileReaction

compileReaction: $(cREACTION)

mergeReaction: $(mREACTION)

$(cREACTION):
	pushd $(subst $(space)$(compileprefix),$(empty),$(space)$@);\
	cd lib;\
	make cleaner;\
	make all;\
	mv lib*.a $(CodeBaseRoot)/lib;\
	make cleaner;\
	popd;

$(mREACTION):
	MergePackage $(subst $(space)$(mergeprefix),$(empty),$(space)$@) Reaction

.PHONY: $(cREACTION) $(mREACTION) compileReaction mergeReaction

