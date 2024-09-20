# Makefile

# defaults
SIM ?= ghdl
GHDL_ARGS += --std=08 -frelaxed
SIM_ARGS += --ieee-asserts=disable
SIM_ARGS += --wave=wave.ghw

TOPLEVEL_LANG ?= vhdl
COCOTB_HDL_TIMEUNIT = 1ps
COCOTB_HDL_TIMEPRECISION = 1fs

VHDL_SOURCES += ../src/constants.vhd 
VHDL_SOURCES += ../src/find_earliest_non_empty_fifo.vhd
VHDL_SOURCES += ../src/find_earliest_non_empty_fifo_1k.vhd
VHDL_SOURCES += ../src/simple_dual_one_clock.vhd
VHDL_SOURCES += ../src/ram_tdp_rf_rf.vhd
VHDL_SOURCES += ../src/shared_mem_fifo.vhd
VHDL_SOURCES += ../src/fin_time_arr_bhv.vhd
VHDL_SOURCES += ../src/metaflex_top.vhd
VHDL_SOURCES += ../src/dma_buf_bhv.vhd 
VHDL_SOURCES += ../src/metaflex_test_wrapper.vhd

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = metaflex_test_wrapper

# MODULE is the basename of the Python test file
MODULE = fixed, rand

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim
