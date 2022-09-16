#set up a phony target for each of the benchmark 
config=?
.PHONY: bzip2 mcf sjeng libquantum h264ref omnetpp astar xalancbmk specrand 
bzip2:
	./gen_ctrl.sh bzip2 $(config)
	cd 401.bzip2/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./bzip2_base.rv64 bzip2_base.rv64 chicken.jpg 30
# mcf not enough memory
mcf:
	./gen_ctrl.sh mcf $(config)
	cd 429.mcf/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./mcf_base.rv64 mcf_base.rv64 inp.in
sjeng:
	./gen_ctrl.sh sjeng $(config)
	cd 458.sjeng/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./sjeng_base.rv64 sjeng_base.rv64 ref.txt
libquantum:
	./gen_ctrl.sh libquantum $(config)
	cd 462.libquantum/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./libquantum_base.rv64 libquantum_base.64 1397 8
h264ref:
	./gen_ctrl.sh h264ref $(config)
	cd 464.h264ref/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./h264ref_base.rv64 h264ref_base.rv64 -d foreman_ref_encoder_baseline.cfg
omnetpp:
	./gen_ctrl.sh omnetpp $(config)
	cd 471.omnetpp/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./omnetpp_base.rv64 omnetpp_base.rv64 omnetpp.ini
astar:
	./gen_ctrl.sh astar $(config)
	cd 473.astar/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./astar_base.rv64 astar_base.rv64 rivers.cfg
xalancbmk:
	./gen_ctrl.sh xalancbmk $(config)
	cd 483.xalancbmk/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./xalancbmk_base.rv64 xalancbmk_base.rv64 -v t5.xml xalanc.xsl
specrand:
	./gen_ctrl.sh specrand $(config)
	cd 998.specrand/run_base_ref_rv64.0001 &&\
	perf.riscv ../../samplectrl.txt ./specrand_base.rv64 specrand_base.rv64 1255432124 234923

