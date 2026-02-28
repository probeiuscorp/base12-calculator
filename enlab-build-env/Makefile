XVLOG_OPTIONS=
OUTFILTER=grep --color=always -e "ERROR.*" -e "CRIT.*" -e "WARN.*" 

export ROOT:=$(shell git rev-parse --show-toplevel)
export RSRC=$(ROOT)/resources

all: compile elaborate simulate

.PHONY: compile
.PHONY: elaborate
.PHONY: simulate


OUTFILTER=grep --color=always -e "ERROR.*" -e "CRIT.*" -e "WARN.*" 

compile: src/*.v 
	@ printf "xvlog $(XVLOG_OPTIONS) src/*.v\n"
	@! xvlog $(XVLOG_OPTIONS) src/*.v | $(OUTFILTER)

elaborate: compile
	@ printf "xelab -debug typical -s sim testbench\n"
	@! xelab -debug typical -s sim testbench | $(OUTFILTER)

simulate: elaborate
	@ printf "xsim --runall sim\n"
	@ xsim --runall sim


gui: elaborate
	@ printf "xsim --gui sim\n"
	@ xsim --gui sim 


implement:
	@ if [ -e build.tcl ]; then vivado -mode batch -source build.tcl; \
	else vivado -mode batch -source $(RSRC)/build.tcl; fi

check: 
	@ vivado -mode batch -source $(RSRC)/check_syntax.tcl 

