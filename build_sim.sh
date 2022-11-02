#!/bin/bash
# Build the simulator

BUILD_DIR=$(realpath ../boom_build/)
SIM_STORE_DIR=$(realpath ./sims/)
BUILD_LOG_DIR=$(realpath ./build_log/)
WORK_DIR=$(realpath ./)

mkdir -p $BUILD_DIR
mkdir -p $SIM_STORE_DIR
mkdir -p $BUILD_LOG_DIR

BOOM_SRC_DIR=./boom_scala
SIM_NAME='simulator-MediumBoomConfig'

cd $BOOM_SRC_DIR
./copy_to_boom.sh $BUILD_DIR/
cd $BUILD_DIR && make 2>&1 | tee $BUILD_LOG_DIR/current.log
if [ ${PIPESTATUS[0]} -ne 0 ]; then 
    echo "$COMMIT build failed"
else
    rm $BUILD_LOG_DIR/current.log
    cp $BUILD_DIR/build/$SIM_NAME $SIM_STORE_DIR/current
fi