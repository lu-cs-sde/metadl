#!/bin/bash

CLOG_JAR=../compiler.jar

java -jar $CLOG_JAR -e metadl \
     --lang c \
     --sources src/loops.c,A:src/duff.c,A:src/loops.cpp,A \
     --profile profile.json \
     loop_depth.mdl
