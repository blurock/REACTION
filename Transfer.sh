#!/bin/csh
#
# FILE     Transfer.sh
# PACKAGE  Maintenance
# AUTHORS  Edward S. Blurock
#
# CONTENT
#   Shell script for transfering files from ReactionOld to Reaction
#
#c COPYRIGHT (C) 1997 RISC Linz
set verbose

set NEWSOURCE                = /home/reaction/Reaction
set OLDSOURCE                = /home/reaction/ReactionOld

if( $#argv < 1 ) then
    echo "Transfer UNIX programs to NT System"
    echo "Usage:  $0 Module"
    echo "        Module: The name of the module to transfer"
    exit(1)
endif

set MODULE            = $1

set OLDMODULE         = $OLDSOURCE/$MODULE
set DEVMODULE         = $NEWSOURCE/$MODULE

set OLDINCLUDE         = $OLDMODULE/include/Reaction
set OLDSOURCE          = $OLDMODULE/src/$MODULE
set OLDEXAMPLE         = $OLDMODULE/examples/$MODULE

set DEVINCLUDE         = $DEVMODULE/include/Reaction
set DEVSOURCE          = $DEVMODULE/src/$MODULE
set DEVEXAMPLE         = $DEVMODULE/examples/$MODULE

echo "ReactionOld files"
ls -l $OLDINCLUDE
ls -l $OLDSOURCE
ls -l $OLDEXAMPLE

cp $OLDINCLUDE/* $DEVINCLUDE
cp $OLDSOURCE/* $DEVSOURCE
cp $OLDEXAMPLE/* $DEVEXAMPLE
rm $DEVEXAMPLE/Makefile
cp $NEWSOURCE/StaticAtom/examples/StaticAtom/Makefile $DEVEXAMPLE

ls -l $DEVINCLUDE
ls -l $DEVSOURCE
ls -l $DEVEXAMPLE
pushd $DEVSOURCE
foreach i ($DEVINCLUDE/* $DEVSOURCE/* $DEVEXAMPLE/*)
/usr/local/java/htmldemos/scripts/SubstituteString.sh INT int $i $i.tmp
/usr/local/java/htmldemos/scripts/SubstituteString.sh FLOAT double $i.tmp $i
rm *.tmp
end
popd

exit(0)
