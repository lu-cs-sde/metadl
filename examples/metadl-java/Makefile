SRCS=$(wildcard *.mdl)
TARGETS=$(patsubst %.mdl,%.run, $(SRCS))

METADL=../../compiler.jar

.PRECIOUS: %.dl

all : $(TARGETS)

%.dl : %.mdl
	java -jar $(METADL) pretty::souffle -FACTS . -OUT . $<

%.run : %.dl
	souffle $< -o $@

clean :
	rm -f *.dl
	rm -f *.run
	rm -f *.cpp
