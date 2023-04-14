# Build & Pack Spec_2006 On mprc238 For FPGA

## 获取源代码

* 进入238服务器，将祖传的SPEC2006源码拷贝到$HOME 目录

    ```bash
    cd $HOME && cp /home/zhanglei/spec2006_compiled.zip .
    ```

* 解压

    ```bash
    unzip spec2006_compiled.zip
    ```

* 修改`runspec`脚本的解释器

    ```bash
    vim spec2006/bin/runspec
    ```

    将第一行改成

    ```bash
    #!/usr/bin/env specperl
    ```

## 编译

之前参考实验室同学写的编译spec的文档，遇到非常多的问题。后续，主要参考了一篇[博客](https://www.francisz.cn/2021/10/04/spec2006-usage/)

* 进入SPEC2006目录

    ```bash
    cd spec2006/
    ```

* 设置环境变量；**注意：每次登录都需重新设置！**

    ```bash
    source ./shrc
    ```

* 设置编译选项

  * 将`config/rv64.cfg` 内容设置为[这个](https://gist.github.com/HarsonLau/744e55354976b9b35dedd958c34dc3d3)

* 编译

    ```bash
    # 只编译base int用例
     runspec -c rv64 --action=build -T base int
    ```

## 打包

创建一个脚本`pack.sh`

```bash
# !/bin/bash

benchlist=('401.bzip2'   '429.mcf'   '458.sjeng'   '462.libquantum'   '464.h264ref'   '471.omnetpp'   '473.astar'   '483.xalancbmk'   '999.specrand')
for bench in ${benchlist[*]}
do
 echo $bench
 dir=$(realpath ./benchspec/CPU2006/$bench)
 if [ -d $dir ]; then
  dest_dir=$HOME/spec_run/$bench
  rm -rf $dest_dir
  mkdir -p $dest_dir
  cp -r $dir/run/run_base_ref_rv64.0001 $dest_dir
 fi
done
cd $HOME && tar czf spec_run.tar.gz spec_run 


```

```bash
chmod +x ./pack.sh
./pack.sh
```
