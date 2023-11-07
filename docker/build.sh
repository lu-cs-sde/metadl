#! /bin/bash

docker build \
       --build-arg SOUFFLE_GIT=https://github.com/alexdura/souffle \
       --build-arg SOUFFLE_HASH=84826bb8b6ef739a762c992c06a42441d19b72da \
       --build-arg JAVADL_GIT=https://github.com/lu-cs-sde/metadl.git \
       --build-arg JAVADL_HASH=751f63917b980e3ec652d83fb5d7de9995773540 \
       --build-arg CLOG_EVAL_GIT=https://github.com/alexdura/clog-eval.git \
       --build-arg CLOG_EVAL_HASH=79d86f6c6e58756980478790eb2242a3472f839e \
       --build-arg LLVM_GIT=https://github.com/alexdura/llvm-project.git \
       --build-arg LLVM_HASH=c9a1d5ea1edc7439f2213fd42a080828d0e7de22 \
       --build-arg JULIET_GIT=https://github.com/alexdura/juliet.git \
       --build-arg JULIET_HASH=dc4849eb6072f1fb1dec861aa2eb0e34ba379c1a \
       --build-arg MAGMA_GIT=https://github.com/alexdura/magma.git \
       --build-arg MAGMA_HASH=2b5dfb428ad9d8fef3c9e6c3d02a7695649b2c35 \
       -t clog23 .
