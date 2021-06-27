#!/bin/bash

pushd javadl-inc-eval

PROJECTS_DIR=/work/javadl-eval-2/defects4j/projects/f/
PROJECT_REGEX="d4j"

JAVADL_DIR=/work/metadl
JAVADL_JAR=$JAVADL_DIR/compiler.jar
ERROR_PRONE_LIB=$JAVADL_DIR/examples/metadl-java/libErrorProne.so
SPOT_BUGS_LIB=$JAVADL_DIR/examples/metadl-java/libSpotBugs.so
EP_JAR=/work/javadl-eval-2/static-checkers/error_prone_core-2.4.0-with-dependencies.jar
SB_JAR=/work/javadl-eval-2/static-checkers/spotbugs-4.0.3/lib/spotbugs.jar


################################################################################
# Do the runs
################################################################################

OUT_EP="ep"
# Error Prone runs
python3 python/src/single.py  $PROJECTS_DIR  out_single_$OUT_EP \
        $JAVADL_JAR $JAVADL_DIR/examples/metadl-java/error-prone-metadl.mdl \
        $ERROR_PRONE_LIB $PROJECT_REGEX

python3 python/src/incremental.py  $PROJECTS_DIR  out_inc_$OUT_EP \
        $JAVADL_JAR $JAVADL_DIR/examples/metadl-java/error-prone-metadl.mdl $PROJECT_REGEX

# Error Prone baseline
python3 python/src/single_error_prone.py $PROJECTS_DIR \
	out_baseline_$OUT_EP \
	$EP_JAR $PROJECT_REGEX

OUT_SB="sb"
# SpotBugs runs
python3 python/src/single.py  $PROJECTS_DIR  out_single_$OUT_SB \
        $JAVADL_JAR $JAVADL_DIR/examples/metadl-java/spot-bugs-metadl.mdl \
        $SPOT_BUGS_LIB $PROJECT_REGEX


python3 python/src/incremental.py  $PROJECTS_DIR  out_inc_$OUT_SB \
        $JAVADL_JAR $JAVADL_DIR/examples/metadl-java/spot-bugs-metadl.mdl $PROJECT_REGEX


# Spot Bugs baseline
python3 python/src/single_spot_bugs.py $PROJECTS_DIR \
	out_baseline_$OUT_SB \
	$SB_JAR $PROJECT_REGEX


################################################################################
# Gather data
################################################################################

mkdir -p data

python3 python/src/gather_data.py \
	$PROJECTS_DIR out_inc_$OUT_EP $PROJECT_REGEX inc
mv all_data.csv data/all_data_ep_inc.csv

python3 python/src/gather_data.py \
	$PROJECTS_DIR out_single_$OUT_EP $PROJECT_REGEX exh
mv all_data.csv data/all_data_ep_exh.csv

python3 python/src/gather_data.py \
	$PROJECTS_DIR out_baseline_$OUT_EP $PROJECT_REGEX baseline
mv all_data.csv data/all_data_ep_baseline.csv

python3 python/src/gather_data.py \
	$PROJECTS_DIR out_inc_$OUT_SB $PROJECT_REGEX inc
mv all_data.csv data/all_data_sb_inc.csv

python3 python/src/gather_data.py \
	$PROJECTS_DIR out_single_$OUT_SB $PROJECT_REGEX exh
mv all_data.csv data/all_data_sb_exh.csv

python3 python/src/gather_data.py \
	$PROJECTS_DIR out_baseline_$OUT_SB $PROJECT_REGEX baseline
mv all_data.csv data/all_data_sb_baseline.csv


################################################################################
# Gather commit data
################################################################################
python3 python/src/provenance.py \
	$PROJECTS_DIR out_inc_$OUT_EP $PROJECT_REGEX

mv provenance.csv data/provenance.csv

################################################################################
# Do the plots
################################################################################
mkdir -p plots_ep
python3 python/src/plot_change_vs_analyzed.py \
	"{\"projects_dir\":\"$PROJECTS_DIR\", \"main_data_file\":\"data/all_data_ep_inc.csv\", \"project_filter\":\"$PROJECT_REGEX\", \"other_data_files\":[\"data/all_data_ep_exh.csv\", \"data/all_data_ep_baseline.csv\"], \"provenance_data_file\":\"data/provenance.csv\"}"
mv *.svg plots_ep

mkdir -p plots_sb
python3 python/src/plot_change_vs_analyzed.py \
	"{\"projects_dir\":\"$PROJECTS_DIR\", \"main_data_file\":\"data/all_data_sb_inc.csv\", \"project_filter\":\"$PROJECT_REGEX\", \"other_data_files\":[\"data/all_data_sb_exh.csv\", \"data/all_data_sb_baseline.csv\"], \"provenance_data_file\":\"data/provenance.csv\"}"
mv *.svg plots_sb

popd
