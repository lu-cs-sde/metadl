#! /bin/bash

JAVADL_TOKEN=Td8iVr7GdD3Jv688hVBm
JAVADL_INC_EVAL_TOKEN=4iDr7qWs1JHncUDDyRZy
JAVADL_EVAL_TOKEN=jQ7vu8WkKzd3_EoAyDhL

docker build \
       --build-arg SOUFFLE_GIT=https://github.com/alexdura/souffle \
       --build-arg SOUFFLE_HASH=84826bb8b6ef739a762c992c06a42441d19b72da \
       --build-arg JAVADL_GIT=https://oauth2:$JAVADL_TOKEN@git.cs.lth.se/al7330du/metadl \
       --build-arg JAVADL_HASH=cf5a8ffde175b0d6a8bcaf4a5e9b199324f22859 \
       --build-arg JAVADL_INC_EVAL_GIT=https://oauth2:$JAVADL_INC_EVAL_TOKEN@git.cs.lth.se/al7330du/metadl-inc-eval \
       --build-arg JAVADL_INC_EVAL_HASH=780c6c746c4260d4c5758feafa3c0dc2c21b3ef3 \
       --build-arg JAVADL_EVAL_GIT=https://oauth2:$JAVADL_EVAL_TOKEN@git.cs.lth.se/al7330du/metadl-eval \
       --build-arg JAVADL_EVAL_HASH=oospla21 \
       -t javadl:oopsla21 .
