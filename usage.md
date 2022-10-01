## Evaluating TAGE with SPEC2006 using boom_stop on FPGA

## 零、代码库介绍

代码从[我fork的崔博的仓库](https://github.com/HarsonLau/boom_stop)的对应分支获取。崔博和我的仓库均为私有仓库，如需使用，请联系原作者崔博申请权限。我的仓库目前包含以下分支：

```txt
liuhao 相比上游main分支，添加了一些用于批量build的脚本
tage-signals 相比liuhao分支，添加了用于统计tage测评的信号
tage-eval 相比tage-signals, 修改了tage的各种配置
```

可以通过`git rebase`快速同步上游的修改。

## 一、编译 BOOMv3

### 1. 生成不同TAGE配置的BOOM 代码

通过以下脚本生成不同tagSize 和 组数的BOOM 配置，并进行git commit

```bash
#!/bin/bash
tagSzs=(7 8 9)
nSets=(128 256 512 1024 2048)
for tagSz in ${tagSzs[*]}
do
    # enumerate the nSets array
    for nset in ${nSets[*]}
    do
        config=tage-config-6-${nset}-${tagSz}
        echo $config
        command1='s/_NSET/'${nset}'/g'
        command2='s/_TAGSZ/'${tagSz}'/g'
        cat ./tage.template | sed ${command1}|sed ${command2} > ./tage.scala
        git diff
        git add ./tage.scala
        git commit -m $config       
    done
done
```

### 2.build mcs 

在21服务器上通过rsync 同步代码

```bash
rsync -a liuhao@39.105.198.167:~/boom_stop/ ./boom_stop
```
确保在正确的分支上，有我们需要build的commit

```bash
cd boom_stop && git log
```
build 过程可能很长，开一个screen 以防连接断开

```
screen 
./batch_build_mcs.sh config
```

## 二、  操作系统

总体流程还是使用之前的make sd image 和make sd card。需要做一个修改，将崔博仓库中修改后的内核代码，移到20服务器上合适的位置，构建内核。

使用这个脚本构建的内存卡，分区会很小，可用空间可能只有200MB左右。所以对分区进行扩容。

大致过程是

```bash
在一台本地的linux机器上 使用fdisk 修改分区表
删除原有的较小的分区
在原有分区的起始位置创建一个更大的分区
（以上修改暂时都只是在内存中）
将分区表写回到SD卡上。
利用resize2fs命令调整文件系统大小
```

## 三、benchmark

使用boom stop的ptrace 对benchmark 进行测评，不需要修改benchmark代码。

spec 2006的编译，见另一篇文档。

## 四、FPGA执行，生成log

### 0. FPGA 环境准备

​	将`boom_stop/example/ptrace/perf.riscv`可执行文件放到PATH上，方便在不同目录下使用。

​	我的大致做法是	

```shell
mkdir -p $HOME/.local/bin
ln -s /boom_stop/example/ptrace/perf.riscv $HOME/.local/bin/perf.riscv
export PATH=$PATH:$(realpath $HOME/.local/bin)
```

安装make

### 1. 设置取样的参数

- samplectrl.txt中的内容: 用于设置采样的参数
  - eventsel: num, 用于选择采样的事件
  - maxevent: num, 用于设置采样事件的间隔
  - maxperiod: num, 用于设置采样的次数
  - warmupinst: num, 用于设置第一次采样之前预热的指令数
  - logname: filename, 用于设置采样结果输出的文件名(`benchmark-hash-cnt.log`)

使用 spec_run/gen_ctrl.sh 即可设置samplectrl.txt

### 2. 执行benchmark

在安装了make的FPGA上
```bash
make config=<hash> <benchmark list> 
```


## 五，数据处理与分析

数据处理及绘图代码所需依赖在`log_process/requirements.txt` 中。创建一个虚拟环境，并安装依赖。

###  1. 把log转换为csv文件

1.1. `cd log_process/process`

1.2 在eventlist中定义事件名称（已定义好tage 测评相应信号）

1.3 process.py中，通过eventinfos.append()函数添加对原始数据的处理后的信号（比如IPC等），数据在1.4生成的csv文件中会追加在原始信号之后 （已写好，不需要改）

1.4 执行`process.py gcc_ckpt.log`,生成*.eventinfo_h.csv文件即可（此时可以打开看一下）


### 2. 对csv 进行后处理（截断，合并）
在`boom_stop_data` 目录下新建了一个后处理[脚本](https://gist.github.com/HarsonLau/cd89edec765a348929d333f7f02f3ff2)，用于截断，合并多个配置产生的数据。两个必要的参数，input file list 和benchmark name。

```bash
python .\postprocess.py --input 15efb0d2f501f7220af1fd89ab220bf2a5f0c15d-2_eventinfo_h.csv 74b62fec3c350cbb9a5c424037d4f5ddda7387c2-0_eventinfo_h.csv   --benchmark 403.gcc
```

### 3. 对csv进行可视化

TODO:

  -  平均值

2.1 `cd log_process/drawplot`

2.2 通过figs.append()添加需要的图即可，不详细介绍

2.3 执行 `python .\maxinst_draw.py ..\..\boom_stop_data\403.gcc_post.csv`，在boom_stop_data目录下生成403.gcc_post.pdf 文件（即画出的图）
