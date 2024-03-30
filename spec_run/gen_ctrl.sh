#!/bin/bash
# take in two arguments , the first specifying the benchmark name ,the second specifying the config hash
benchname=$1
config=$2
maxperiod=500

# if there is a third argument ,set maxperiod
if [ $# -eq 3 ];then
    maxperiod=$3
fi

benchlist=('400.perlbench' '401.bzip2' '403.gcc' '429.mcf'  '445.gobmk' '458.sjeng'   '462.libquantum'   '464.h264ref'   '471.omnetpp'   '473.astar'   '483.xalancbmk'   '999.specrand')
for bench in ${benchlist[*]}
do
    if [[ $bench == *$benchname* ]];then 
        benchname=$bench
        break
    fi
done

ctrlfile=./samplectrl.txt
#get the log cnt
mkdir -p ./data
logcnt=`ls ./data | grep $benchname | grep $config | wc -l`
logname=$(realpath ./data/$benchname-$config-$logcnt.log)

echo "eventsel: 0" > $ctrlfile
echo "maxevent: 200000000">> $ctrlfile
echo "warmupinst: 0">>$ctrlfile
echo "maxperiod: $maxperiod">>$ctrlfile
echo "logname: $logname">>$ctrlfile
