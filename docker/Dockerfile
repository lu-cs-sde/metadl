FROM ubuntu:18.04

ARG SOUFFLE_GIT
ARG SOUFFLE_HASH

ARG JAVADL_GIT
ARG JAVADL_HASH

ARG JAVADL_INC_EVAL_GIT
ARG JAVADL_INC_EVAL_HASH

ARG JAVADL_EVAL_GIT
ARG JAVADL_EVAL_HASH


RUN useradd -m javadl && echo javadl:javadl | chpasswd && \
	usermod -aG sudo javadl

# General-purpose utilities
RUN apt-get update && apt-get -y install \
	bash \
	git \
	gdb \
	gcc \
	emacs \
	vim \
	nano \
	sudo \
	autoconf \
	automake \
	bison \
	build-essential \
	clang \
	doxygen \
	flex \
	g++ \
	git \
	libffi-dev \
	libncurses5-dev \
	libtool \
	libsqlite3-dev \
	make \
	mcpp \
	python \
	sqlite \
	zlib1g-dev

# Java; Java 8 is needed by Defects4J, Java 11 for everything else
RUN apt-get -y install openjdk-8-jdk \
	openjdk-11-jdk

# Swig, needed for JavaDL to be able to interface Souffle-generated program
RUN apt-get -y install swig

# Python
RUN apt-get -y install python3
RUN apt-get -y install python3-pip

RUN mkdir /work && chown javadl /work && cd /work

# Switch user
USER javadl

# Build Souffle
RUN cd /work && git clone $SOUFFLE_GIT souffle
RUN cd /work/souffle/ && git checkout $SOUFFLE_HASH
RUN cd /work/souffle && sh ./bootstrap && ./configure && make -j4
RUN echo "export PATH=$PATH:/work/souffle/src/" >> ~/.bash_aliases

# Build JavaDL
RUN git config --global url."https://git.cs.lth.se/".insteadOf "git://git.cs.lth.se:"
RUN git config --global url."https://git.cs.lth.se/".insteadOf "git@git.cs.lth.se:"

RUN cd /work && git clone $JAVADL_GIT
RUN cd /work/metadl && git checkout $JAVADL_HASH && git submodule update --init --recursive

RUN cd /work/metadl && ./gradlew jar

# Build the hybrid libraries
ENV PATH="${PATH}:/work/souffle/src"
RUN cd /work/metadl/examples/metadl-java/ && touch srcs.csv && ./gen-hybrid.sh

RUN cd /work && git clone $JAVADL_INC_EVAL_GIT javadl-inc-eval
RUN cd /work/javadl-inc-eval && git checkout $JAVADL_INC_EVAL_HASH

RUN cd /work && git clone $JAVADL_EVAL_GIT javadl-eval-1
RUN cd /work && git clone $JAVADL_EVAL_GIT javadl-eval-2


# Check out the evaluation framework
