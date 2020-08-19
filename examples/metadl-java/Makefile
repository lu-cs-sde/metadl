SRCS=$(wildcard *.mdl)
TARGETS=$(patsubst %.mdl,%.run, $(SRCS))

METADL=../../compiler.jar

.PRECIOUS: %.dl

all : $(TARGETS)

%.dl : %.mdl
	java -jar $(METADL) -p souffle -F . -D . $<

%.run : %.dl
	souffle -p $<.prof $< -o $@

clean :
	rm -f *.dl
	rm -f *.run
	rm -f *.cpp
