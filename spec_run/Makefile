config=?
perlbench:
	./gen_ctrl.sh 400.perlbench $(config) 75 &&\
	cd 400.perlbench &&\
	cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./perlbench_base.rv64 perlbench_base.rv64 -I./lib checkspam.pl 2500 5 25 11 150 1 1 1 1
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./perlbench_base.rv64 perlbench_base.rv64 -I./lib diffmail.pl 4 800 10 17 19 300
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./perlbench_base.rv64 perlbench_base.rv64 -I./lib splitmail.pl 1600 12 26 16 4500

perlbench_r:
	./gen_spec17_ctrl.sh 500.perlbench_r $(config)  &&\
	cd runenv17/perlbench_r &&\
	perf.riscv ../../samplectrl.txt ./perlbench_r.riscv perlbench_r.riscv -I./lib checkspam.pl 2500 5 25 11 150 1 1 1 1 

gcc:
	./gen_ctrl.sh 403.gcc $(config) 150 &&\
	cd 403.gcc &&\
	cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gcc_base.rv64 gcc_base.rv64 166.i -o 166.s
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gcc_base.rv64 gcc_base.rv64 200.i -o 200.s
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gcc_base.rv64 gcc_base.rv64 expr.i -o expr.s
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gcc_base.rv64 gcc_base.rv64 c-typeck.i -o c-typeck.s

gcc_r:
	./gen_spec17_ctrl.sh 502.gcc_r $(config)  &&\
	cd runenv17/gcc_r &&\
	perf.riscv ../../samplectrl.txt ./gcc_r.riscv gcc_r.riscv gcc-pp.c -O3 -finline-limit=0 -fif-conversion -fif-conversion2 -o gcc-pp.opts-O3_-finline-limit_0_-fif-conversion_-fif-conversion2.s 

gobmk:
	./gen_ctrl.sh 445.gobmk $(config) 60 &&\
	cd 445.gobmk &&\
	cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gobmk_base.rv64 gobmk_base.rv64 --quiet --mode gtp < 13x13.tst
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gobmk_base.rv64 gobmk_base.rv64 --quiet --mode gtp < score2.tst
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gobmk_base.rv64 gobmk_base.rv64 --quiet --mode gtp < nngs.tst
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gobmk_base.rv64 gobmk_base.rv64 --quiet --mode gtp < trevorc.tst
	#cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./gobmk_base.rv64 gobmk_base.rv64 --quiet --mode gtp < trevord.tst

hmmer:
	./gen_ctrl.sh 456.hmmer $(config) 125 &&\
	cd 456.hmmer &&\
	cd ./run_base_ref_rv64.0001 && perf.riscv ../../samplectrl.txt ./hmmer_base.rv64 hmmer_base.rv64 nph3.hmm swiss41

bzip2:
	./gen_ctrl.sh bzip2 $(config) &&\
	cd 401.bzip2/run_base_ref_rv64.0001i &&\
	perf.riscv ../../samplectrl.txt ./bzip2_base.rv64 bzip2_base.rv64 chicken.jpg 30

# mcf not enough memory
mcf:
	./gen_ctrl.sh mcf $(config) &&\
	cd 429.mcf/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./mcf_base.rv64 mcf_base.rv64 inp.in &&\

sjeng:
	./gen_ctrl.sh sjeng $(config) 60 &&\
	cd 458.sjeng/run_base_ref_rv64.0001  &&\
	perf.riscv ../../samplectrl.txt ./sjeng_base.rv64 sjeng_base.rv64 ref.txt

deepsjeng_r:
	./gen_spec17_ctrl.sh 531.deepsjeng_r $(config)  &&\
	cd runenv17/deepsjeng_r &&\
	perf.riscv ../../samplectrl.txt ./deepsjeng_r.riscv deepsjeng_r.riscv ref.txt

libquantum:
	./gen_ctrl.sh libquantum $(config) 50 &&\
	cd 462.libquantum/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./libquantum_base.rv64 libquantum_base.64 1397 8
h264ref:
	./gen_ctrl.sh h264ref $(config) &&\
	cd 464.h264ref/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./h264ref_base.rv64 h264ref_base.rv64 -d foreman_ref_encoder_baseline.cfg
omnetpp:
	./gen_ctrl.sh omnetpp $(config) 100 &&\
	cd 471.omnetpp/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./omnetpp_base.rv64 omnetpp_base.rv64 omnetpp.ini

omnetpp_r:
	./gen_spec17_ctrl.sh 520.omnetpp_r $(config)  &&\
	cd runenv17/omnetpp_r &&\
	perf.riscv ../../samplectrl.txt ./omnetpp_r.riscv omnetpp_r.riscv -c General -r 0 

astar:
	./gen_ctrl.sh astar $(config) 50 &&\
	cd 473.astar/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./astar_base.rv64 astar_base.rv64 rivers.cfg
xalancbmk:
	./gen_ctrl.sh xalancbmk $(config) 200 &&\
	cd 483.xalancbmk/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./Xalan_base.rv64 Xalan_base.rv64 -v t5.xml xalanc.xsl

leela_r:
	./gen_spec17_ctrl.sh 541.leela_r $(config)  &&\
	cd runenv17/leela_r &&\
	perf.riscv ../../samplectrl.txt ./leela_r.riscv leela_r.riscv ref.sgf 

exchange2_r:
	./gen_spec17_ctrl.sh 548.exchange2_r $(config)  &&\
	cd runenv17/exchange2_r &&\
	perf.riscv ../../samplectrl.txt ./exchange2_r.riscv exchange2_r.riscv 6 

# run spec2017 xz benchmark
xz_r:
	./gen_spec17_ctrl.sh 557.xz_r $(config)  &&\
	cd runenv17/xz_r &&\
	perf.riscv ../../samplectrl.txt ./xz_r.riscv xz_r.riscv cld.tar.xz 160 19cf30ae51eddcbefda78dd06014b4b96281456e078ca7c13e1c0c9e6aaea8dff3efb4ad6b0456697718cede6bd5454852652806a657bb56e07d61128434b474 59796407 61004416 6 

int: perlbench bzip2 gcc gobmk hmmer sjeng libquantum h264ref omnetpp astar xalancbmk
	echo “running all the int benchmarks”

int_r: perlbench_r gcc_r deepsjeng_r omnetpp_r leela_r exchange2_r xz_r

subset5: sjeng libquantum gcc hmmer xalancbmk
	echo "running the five subsets"

subset: perlbench gcc gobmk hmmer sjeng libquantum omnetpp astar xalancbmk
	echo "running the ten subsets"
