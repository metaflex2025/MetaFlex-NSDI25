import cocotb
import mf_tb

@cocotb.test(timeout_time = 300, timeout_unit = 'us')
async def fixed290(dut):
    tb = mf_tb.MetaFlexTb(dut)
    await tb.run()

