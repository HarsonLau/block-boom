#!/bin/bash
VivadoDir="/mnt/disk/liuhao/vivado-risc-v/"
DestDir="${VivadoDir}/generators/riscv-boom/"
./update_scala.sh  $DestDir
# echo ${DestDir}
# SrcDir="liuhao@39.105.198.167:~/tage-eval/boom_scala/"
# rsync -a ${SrcDir} ${DestDir}
cd  ${VivadoDir}
rm workspace/rocket64x1/*.v
mkdir -p workspace/rocket64x1
make CONFIG=rocket64x1 BOARD=genesys2 bitstream 2>&1  > workspace/rocket64x1/build.log 
