# FILE     Makefile
# PACKAGE  ThermoTables
# AUTHOR   Edward S. Blurock
#
# CONTENT
#   GNU Makefile for the package library "libThermoTables.a
#
# COPYRIGHT (C)  1997 Reaction Library

#V PackageRoot . . . . . . . . . . . . . . The root directory of this package.
PackageRoot:=$(shell cd ..; pwd)
export PackageRoot


#I
include ../etc/Reaction/MakeDef


#V Variable definitions.

objdirs:=$(filter-out %/Config,$(shell cd ../src; find * -type d ! -name seq ! -name par ! -name CVS -print))


#F Targets.

all: libThermoTables.a

libThermoTables.a:
	@echo Building sources ...
	for i in `find ../src/* -name Makefile -print`; do\
	  $(MAKE) -C `dirname $$i` all ;\
	done
	@echo Building objects ...
	@-mkdir ../obj; cd ../obj; mkdir $(objdirs) 2>/dev/null
	find ../obj/* -type d ! -name CVS -exec \
	  $(MAKE) -f $(PackageRoot)/etc/Reaction/Makefile.obj -C {} all \;
	@echo Building library ...
	$(RM) libThermoTables.a
	ar -vq libThermoTables.a `find ../obj -name '*.o' -print` 
	$(RANLIB) libThermoTables.a

clean::
	find ../obj/* -type d ! -name CVS -exec \
	  $(MAKE) -f $(PackageRoot)/etc/Reaction/Makefile.obj CLEAN=1 -C {} clean \;

cleaner::
	$(RM) lib*.a

#f Special targets.

.PHONY:	libThermoTables.a
