import cocotb
import shr_mem_tb

@cocotb.test(timeout_time = 5, timeout_unit = 'us')
async def push_pop_test(dut):
    tb = shr_mem_tb.ShrMemTb(dut)
    await tb.run()

