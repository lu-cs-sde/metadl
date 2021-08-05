#! /bin/bash

JAVADL_TOKEN=Td8iVr7GdD3Jv688hVBm
JAVADL_INC_EVAL_TOKEN=4iDr7qWs1JHncUDDyRZy
JAVADL_EVAL_TOKEN=jQ7vu8WkKzd3_EoAyDhL

docker build \
       --build-arg SOUFFLE_GIT=https://github.com/creichen/souffle \
       --build-arg SOUFFLE_HASH=970a41cfadcaf93ad549d0c82bcfe634532ca0fe \
       --build-arg JAVADL_GIT=https://oauth2:$JAVADL_TOKEN@git.cs.lth.se/al7330du/metadl \
       --build-arg JAVADL_HASH=588c7f392e956edc0260e00b3aee52cf70e3f2b5 \
       --build-arg JAVADL_INC_EVAL_GIT=https://oauth2:$JAVADL_INC_EVAL_TOKEN@git.cs.lth.se/al7330du/metadl-inc-eval \
       --build-arg JAVADL_INC_EVAL_HASH=oopsla21 \
       --build-arg JAVADL_EVAL_GIT=https://oauth2:$JAVADL_EVAL_TOKEN@git.cs.lth.se/al7330du/metadl-eval \
       --build-arg JAVADL_EVAL_HASH=oospla21 \
       -t javadl:oopsla21hfp .
