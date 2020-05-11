SRCS=$(wildcard *.mdl)
TARGETS=$(patsubst %.mdl,%.run, $(SRCS))

METADL=../../compiler.jar

%.dl : %.mdl
	java -jar $(METADL) pretty::souffle -FACTS . -OUT . $<

%.run : %.dl
	souffle $< -o $@

all : $(TARGETS)
	echo $(SRCS)
	echo $(TARGETS)
