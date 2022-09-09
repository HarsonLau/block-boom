#!/bin/bash
KEYWORD=$1
MCS_STORE_DIR=$(realpath ./mcs/)
VIVADO_PROJ_DIR=$(realpath ../vivado-risc-v/)
VIVADO_BOOM_DEST_DIR=$VIVADO_PROJ_DIR/generators/riscv-boom
BOOM_SRC_DIR=$(realpath ./boom_scala/)
MCS_BUILD_LOG_DIR=$(realpath ./mcs_build_log/)
WORK_DIR=$(realpath ./)
DEFAULT_BRANCH=liuhao

mkdir -p $MCS_STORE_DIR
mkdir -p $MCS_BUILD_LOG_DIR

BEGIN_DATE='2022-07-09'
for COMMIT in $(git log --pretty=oneline --since="${BEGIN_DATE}"| grep ${KEYWORD} |awk '{print $1}')
do 
    MCS_PATH=$MCS_STORE_DIR/$COMMIT.mcs
    if [ ! -f $MCS_PATH ]; then
        rm -rf $VIVADO_PROJ_DIR/workspace/rocket64x1/
        git checkout $COMMIT
        cd $BOOM_SRC_DIR
        ./copy_to_boom.sh $VIVADO_BOOM_DEST_DIR/
        if [ $? -ne 0 ];then
            exit 233
        fi 
        
        cd $VIVADO_PROJ_DIR 
        ./build.sh 2>&1 | tee $MCS_BUILD_LOG_DIR/$COMMIT.log
        if [ ${PIPESTATUS[0]} -ne 0 ]; then
            echo "$COMMIT build failed"
            exit 233
        else
            rm $MCS_BUILD_LOG_DIR/$COMMIT.log
            cp $VIVADO_PROJ_DIR/workspace/rocket64x1/genesys2-riscv.mcs $MCS_PATH
        fi
        
    fi
    cd $WORK_DIR
done
git checkout $DEFAULT_BRANCH