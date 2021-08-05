#!/bin/bash

METADL=../../compiler.jar

java -jar $METADL -g spot-bugs-metadl.mdl -l libSpotBugs.so
mv libSwigInterface.so libSpotBugs.so

java -jar $METADL -g error-prone-metadl.mdl -l libErrorProne.so
mv libSwigInterface.so libErrorProne.so
