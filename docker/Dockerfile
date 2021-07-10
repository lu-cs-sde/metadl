FROM ubuntu:18.04

ARG SOUFFLE_GIT
ARG SOUFFLE_HASH

ARG JAVADL_GIT
ARG JAVADL_HASH

ARG JAVADL_INC_EVAL_GIT
ARG JAVADL_INC_EVAL_HASH

ARG JAVADL_EVAL_GIT
ARG JAVADL_EVAL_HASH


# RUN useradd -m javadl && echo javadl:javadl | chpasswd && \
# 	usermod -aG sudo javadl

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


# Locales (use UTF-8 instead of ASCII)
RUN apt-get install -y locales locales-all
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

# Java; Java 8 is needed by Defects4J, Java 11 for everything else
RUN apt-get -y install openjdk-8-jdk \
	openjdk-11-jdk

# Swig, needed for JavaDL to be able to interface Souffle-generated program
RUN apt-get -y install swig

# Python
RUN apt-get -y install python3
RUN apt-get -y install python3-pip

RUN mkdir /work

# Switch user
# USER javadl

# Build Souffle
RUN cd /work && git clone $SOUFFLE_GIT souffle
RUN cd /work/souffle/ && git checkout $SOUFFLE_HASH
RUN cd /work/souffle && sh ./bootstrap && ./configure --enable-swig --enable-64bit-domain && make -j4
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

# Check out the evaluation framework
RUN cd /work && git clone $JAVADL_INC_EVAL_GIT javadl-inc-eval
RUN cd /work/javadl-inc-eval && git checkout $JAVADL_INC_EVAL_HASH
# Install the packages used by the evaluation framework
RUN pip3 install -r /work/javadl-inc-eval/python/requirements.txt

RUN cd /work && git clone $JAVADL_EVAL_GIT javadl-eval-1
RUN cd /work && git clone $JAVADL_EVAL_GIT javadl-eval-2
RUN cd /work && git clone $JAVADL_EVAL_GIT javadl-eval-3


# Change to Java 8
# USER root
RUN echo "2" | sudo update-alternatives --config java && java -version
RUN echo "2" | sudo update-alternatives --config javac && javac -version
RUN apt-get -y install curl unzip subversion
RUN perl -MCPAN -e 'install Bundle::DBI'

# USER javadl
RUN pip3 install joblib
ENV JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF8"
RUN cd /work/javadl-eval-1 && ./scripts/download_defects4j.sh
RUN cd /work/javadl-eval-2 && ./scripts/download_defects4j.sh
RUN cd /work/javadl-eval-3 && ./scripts/download_defects4j.sh


# Change back to Java 11
# USER root
RUN echo "0" | sudo update-alternatives --config java && java -version
RUN echo "0" | sudo update-alternatives --config javac && javac -version

RUN apt-get -y install wget

# Download Error Prone and SpotBugs
# USER javadl
RUN cd /work/javadl-eval-1 && ./scripts/download_static_checkers.sh ; echo "Success"
RUN cd /work/javadl-eval-2 && ./scripts/download_static_checkers.sh ; echo "Success"
RUN cd /work/javadl-eval-3 && ./scripts/download_static_checkers.sh ; echo "Success"
# Copy over JavaDL to the static checkers folder (TODO: symlink instead)
RUN cd /work/javadl-eval-1 && cp -r /work/metadl ./static-checkers

# Add the script for running the analysis quality part of the evaluation
COPY run_quality.bash /work
COPY run_performance.bash /work

# Point to rt.jar
ENV METADL_JAVA_RT=/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/rt.jar

# RUN cd /work/javadl-eval-1 && \
# 	echo "2" | sudo update-alternatives --config java && \
# 	echo "2" | sudo update-alternatives --config javac && \
# 	javac -version && \
# 	./scripts/download_defects4j.sh && \
# 	echo "0" | sudo update-alternatives --config java && \
# 	echo "0" | sudo update-alternatives --config javac && \
# 	javac -version
