#! /bin/bash

JAVADL_TOKEN=Td8iVr7GdD3Jv688hVBm
JAVADL_INC_EVAL_TOKEN=4iDr7qWs1JHncUDDyRZy
JAVADL_EVAL_TOKEN=jQ7vu8WkKzd3_EoAyDhL

docker build \
       --build-arg SOUFFLE_GIT=https://github.com/creichen/souffle \
       --build-arg SOUFFLE_HASH=no-signal \
       --build-arg JAVADL_GIT=https://oauth2:$JAVADL_TOKEN@git.cs.lth.se/al7330du/metadl \
       --build-arg JAVADL_HASH=cd02b571aee3d7bfaef622d47114715778359e07 \
       --build-arg JAVADL_INC_EVAL_GIT=https://oauth2:$JAVADL_INC_EVAL_TOKEN@git.cs.lth.se/al7330du/metadl-inc-eval \
       --build-arg JAVADL_INC_EVAL_HASH=65b7e74ccfe3c995755703b117f274ad8cc5ce80 \
       --build-arg JAVADL_EVAL_GIT=https://oauth2:$JAVADL_EVAL_TOKEN@git.cs.lth.se/al7330du/metadl-eval \
       --build-arg JAVADL_EVAL_HASH=oospla21 \
       -t javadl:oopsla21hfp .
