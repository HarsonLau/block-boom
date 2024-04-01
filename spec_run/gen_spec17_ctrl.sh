#!/bin/bash
# take in two arguments , the first specifying the benchmark name ,the second specifying the config hash
benchname=$1
config=$2
maxperiod=300

# if there is a third argument ,set maxperiod
if [ $# -eq 3 ];then
    maxperiod=$3
fi

# benchlist=('400.perlbench' '401.bzip2' '403.gcc' '429.mcf'  '445.gobmk' '458.sjeng'   '462.libquantum'   '464.h264ref'   '471.omnetpp'   '473.astar'   '483.xalancbmk'   '999.specrand')

# list the spec17 int benchmarks
benchlist=('500.perlbench_r' '502.gcc_r' '505.mcf_r' '520.omnetpp_r' '523.xalancbmk_r' '525.x264_r' '531.deepsjeng_r' '541.leela_r' '548.exchange2_r' '557.xz_r')

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
