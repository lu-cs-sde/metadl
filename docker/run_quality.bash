#!/bin/bash

pushd javadl-eval-1
# Run the checkers on Defects4j
./scripts/time_checkers_on_defects4j.sh
# Process the results
python3 python/CompareMetaDL.py  study/output-fixed/mdl_parsed_hybrid.json \
        study/output-fixed/ep_parsed.json study/output-fixed/sb_parsed.json
popd
