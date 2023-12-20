#! /bin/bash

docker build \
       --build-arg SOUFFLE_GIT=https://github.com/alexdura/souffle \
       --build-arg SOUFFLE_HASH=84826bb8b6ef739a762c992c06a42441d19b72da \
       --build-arg JAVADL_GIT=https://github.com/lu-cs-sde/metadl.git \
       --build-arg JAVADL_HASH=8f119b7b2a6a23eeb1a48e9e168d22d2b26849e7 \
       --build-arg CLOG_EVAL_GIT=https://github.com/alexdura/clog-eval.git \
       --build-arg CLOG_EVAL_HASH=a4cedd636513e9dbe37ef2c02b317c2bc60d2d91 \
       --build-arg LLVM_GIT=https://github.com/alexdura/llvm-project.git \
       --build-arg LLVM_HASH=c9a1d5ea1edc7439f2213fd42a080828d0e7de22 \
       --build-arg JULIET_GIT=https://github.com/alexdura/juliet.git \
       --build-arg JULIET_HASH=dc4849eb6072f1fb1dec861aa2eb0e34ba379c1a \
       --build-arg MAGMA_GIT=https://github.com/alexdura/magma.git \
       --build-arg MAGMA_HASH=f0f88cb22bec4054e0a1241d77f300d70106e5b1 \
       -t clog23 .
