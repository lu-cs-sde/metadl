#!/bin/bash

METADL=../../compiler.jar

java -jar $METADL -g spot-bugs-metadl.mdl
mv libSwigInterface.so libSpotBugs.so

java -jar $METADL -g error-prone-metadl.mdl
mv libSwigInterface.so libErrorProne.so
