#!/bin/bash

KEYWORD=$1
BUILD_DIR=$(realpath ../boom_build/)
SIM_STORE_DIR=$(realpath ./sims/)
BUILD_LOG_DIR=$(realpath ./build_log/)
WORK_DIR=$(realpath ./)
DEFAULT_BRANCH=liuhao

mkdir -p $BUILD_DIR
mkdir -p $SIM_STORE_DIR
mkdir -p $BUILD_LOG_DIR

BEGIN_DATE='2022-07-09'
BOOM_SRC_DIR=./boom_scala
SIM_NAME='simulator-MediumBoomConfig'
CUR_COMMIT=$(git log --pretty=oneline |awk '{print $1}'|head -1)
for COMMIT in $(git log --pretty=oneline --since="${BEGIN_DATE}"| grep ${KEYWORD} |awk '{print $1}')
do  
    if [ ! -f $SIM_STORE_DIR/$COMMIT ]; then
        git checkout $COMMIT
        cd $BOOM_SRC_DIR
        ./copy_to_boom.sh $BUILD_DIR/
        if [ $? -ne 0 ]; then
            exit 233
        fi
        
        cd $BUILD_DIR && make 2>&1 | tee $BUILD_LOG_DIR/$COMMIT.log
        if [ ${PIPESTATUS[0]} -ne 0 ]; then 
            echo "$COMMIT build failed"
        else
            rm $BUILD_LOG_DIR/$COMMIT.log
            cp $BUILD_DIR/build/$SIM_NAME $SIM_STORE_DIR/$COMMIT
        fi
    fi 
    
    cd $WORK_DIR
done
git checkout $DEFAULT_BRANCH