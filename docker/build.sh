#! /bin/bash


docker build \
       --build-arg SOUFFLE_GIT=https://github.com/alexdura/souffle \
       --build-arg SOUFFLE_HASH=84826bb8b6ef739a762c992c06a42441d19b72da \
       --build-arg JAVADL_GIT=https://github.com/lu-cs-sde/metadl.git \
       --build-arg JAVADL_HASH=cf5a8ffde175b0d6a8bcaf4a5e9b199324f22859 \
       --build-arg JAVADL_INC_EVAL_GIT=https://github.com/alexdura/metadl-inc-eval.git \
       --build-arg JAVADL_INC_EVAL_HASH=780c6c746c4260d4c5758feafa3c0dc2c21b3ef3 \
       --build-arg JAVADL_EVAL_GIT=https://github.com/lu-cs-sde/metadl-eval.git \
       --build-arg JAVADL_EVAL_HASH=oospla21 \
       -t javadl:oopsla21 .
