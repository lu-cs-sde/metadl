#! /bin/bash

docker build \
       --build-arg SOUFFLE_GIT=https://github.com/alexdura/souffle \
       --build-arg SOUFFLE_HASH=84826bb8b6ef739a762c992c06a42441d19b72da \
       --build-arg JAVADL_GIT=https://github.com/lu-cs-sde/metadl.git \
       --build-arg JAVADL_HASH=e586f9ad190daac73c4a7398ab03e8924819382a \
       --build-arg CLOG_EVAL_GIT=https://github.com/alexdura/clog-eval.git \
       --build-arg CLOG_EVAL_HASH=31894a3361e51b81c2a785e7b41770c386bf9a14 \
       --build-arg LLVM_GIT=https://github.com/alexdura/llvm-project.git \
       --build-arg LLVM_HASH=c9a1d5ea1edc7439f2213fd42a080828d0e7de22 \
       --build-arg JULIET_GIT=https://github.com/alexdura/juliet.git \
       --build-arg JULIET_HASH=dc4849eb6072f1fb1dec861aa2eb0e34ba379c1a \
       --build-arg MAGMA_GIT=https://github.com/alexdura/magma.git \
       --build-arg MAGMA_HASH=f0f88cb22bec4054e0a1241d77f300d70106e5b1 \
       -t clog23 .
