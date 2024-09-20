import cocotb
from cocotb.handle import SimHandleBase
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, ClockCycles, Join
from cocotb.types import concat, LogicArray, Range

L2_NUM_FIFOS = 8
PKT_LEN_BIT_WIDTH = 11
FIN_TIME_BIT_WIDTH  = 20
FLOW_ID_BIT_WIDTH   = 10
PKT_ID_BIT_WIDTH  = 16
DESC_BIT_WIDTH = PKT_LEN_BIT_WIDTH + FIN_TIME_BIT_WIDTH + FLOW_ID_BIT_WIDTH + \
                    PKT_ID_BIT_WIDTH

class ShrMemTb():
    reg_dict = {"MM_SCRATCH_REG"   : 0,
                "MM_START"         : 1,
                "MM_ENQ_MAX_CNT"   : 2,
                "MM_ENQ_GAP"       : 3,
                "MM_DEQ_DELAY"     : 4,
                "MM_ENQ_COUNT"     : 5,
                "MM_DEQ_COUNT"     : 6,
                "MM_OVFL_COUNT"    : 7,
                "MM_ENQ_BUFFER"    : 1 * 2**15,
                "MM_ENQ_GAP_BUFFER": 2 * 2**15,
                "MM_DEQ_BUFFER"    : 3 * 2**15,
                "MM_DEQ_TS_BUFFER" : 4 * 2**15,
                "MM_OVFL_BUFFER"   : 5 * 2**15
               }
    flow_enq = {}
    flow_deq = {}
    flow_ovfl = {}

    def __init__(self, dut: SimHandleBase): 
        self.dut = dut
        self.clk_rst_init = self.ClkRstInit(self.dut)
        self.reg_init = self.RegInit(self.dut)
        self.enq_init = self.EnqInit(self.dut)
        self.deq_mon  = self.DeqMon(self.dut)
        self.ovfl_mon = self.OvflMon(self.dut)
        self.dut.wr_en.value = 0
        self.dut.rd_en.value = 0
        #self.enq_init_file = cocotb.regression_manager._test.name + ".enq"

    async def _wait_rd_data_valid(self, timeout):
        for _ in range(timeout):
            await RisingEdge(self.dut.clk)
            if self.dut.data_valid.value == 1:
                return
        raise RuntimeError("Timeout while waiting for _readdatavalid")

    async def reg_rw(self, addr, data, rw):
        result = True
        rdata = None
        
        """
        #await RisingEdge(self.dut.mm_master_clk)
        addr_la = LogicArray(value = addr, range = Range(len(self.dut.mm_master_address) - 1, "downto", 0))
        self.dut.mm_master_address.value = addr_la
        self.dut.mm_master_byteenable.binstr = "1111"
        if rw == 1:
            self.dut.mm_master_writedata.value = data
            self.dut.mm_master_write.value = 1 
        else:
            self.dut.mm_master_read.value = 1
        await RisingEdge(self.dut.mm_master_clk)
        self.dut.mm_master_write.value = 0
        self.dut.mm_master_read.value = 0
            
        if rw == 0:
            await SifterTb._wait_rd_data_valid(self, REG_RD_TIMEOUT)
            rdata = self.dut.mm_master_readdata.value
            if not rdata.binstr.isnumeric():
                self.dut._log.error("Register read error Address: {} Data: {}".format(hex(addr.integer - 4), rdata.value))
                result = False
            else:
                if data is not None and rdata.integer != data:
                    self.dut._log.error("Register read error Address: {} - Got: {}, Expecting: {}".format(hex(addr_la.integer - REG_OFFSET), hex(rdata.integer), hex(data)))
                    result = False
        await RisingEdge(self.dut.mm_master_clk)
        """
        
        return result, rdata

    async def run(self):
        await self.clk_rst_init.start()
        reg_init_rc = await self.reg_init.run()
        enq_init = cocotb.start_soon(self.enq_init.run())
        ShrMemTb.deq_mon_act = True
        #deq_mon  = cocotb.start_soon(self.deq_mon.run())
        #ovfl_mon  = cocotb.start_soon(self.ovfl_mon.run())
        enq_init_rc = await Join(enq_init)
        deq_mon  = cocotb.start_soon(self.deq_mon.run())
        ovfl_mon  = cocotb.start_soon(self.ovfl_mon.run())
        inversion_cnt = await Join(deq_mon)
        ShrMemTb.deq_mon_act = False
        ovfl_mon_rc  = await Join(ovfl_mon)
        """
        _, enq_cnt = await ShrMemTb.reg_rw(self, ShrMemTb.reg_dict["MM_ENQ_COUNT"], None, 0)
        _, deq_cnt = await ShrMemTb.reg_rw(self, ShrMemTb.reg_dict["MM_DEQ_COUNT"], None, 0)
        _, ovfl_cnt = await ShrMemTb.reg_rw(self, ShrMemTb.reg_dict["MM_OVFL_COUNT"], None, 0)
        """
        deq_cnt = ovfl_cnt = enq_cnt = 0
        num_desc_check = (deq_cnt + ovfl_cnt) == enq_cnt
        
        missing_cnt = 0
        duplicate_cnt = 0
        # Check for missing and duplicate packets
        for flow in ShrMemTb.flow_enq:
            for pkt in ShrMemTb.flow_enq[flow]:
                flow_in_deq = flow in ShrMemTb.flow_deq
                flow_in_ovfl = flow in ShrMemTb.flow_ovfl
                if not flow_in_deq and not flow_in_ovfl:
                    self.dut._log.error(f"Missing packet: flow id: {flow} pkt id: {pkt}")
                    missing_cnt += 1
                else:
                    deq_pkt_cnt = ShrMemTb.flow_deq[flow].count(pkt) if flow_in_deq else 0
                    ovfl_pkt_cnt = ShrMemTb.flow_ovfl[flow].count(pkt) if flow_in_ovfl else 0
                    if flow_in_deq:
                        if deq_pkt_cnt == 0:
                            if ovfl_pkt_cnt == 0:
                                self.dut._log.error(f"Missing packet: flow id: {flow} pkt id: {pkt}")
                                missing_cnt += 1
                            elif ovfl_pkt_cnt > 1:
                                self.dut._log.error(f"Duplicate ovfl packet ({ovfl_pkt_cnt - 1}): flow id: {flow} pkt id: {pkt}")
                                duplicate_cnt += ovfl_pkt_cnt - 1
                        elif deq_pkt_cnt > 1:
                            self.dut._log.error(f"Duplicate deq packet ({deq_pkt_cnt + ovfl_pkt_cnt - 1}): flow id: {flow} pkt id: {pkt}")
                            duplicate_cnt += deq_pkt_cnt + ovfl_pkt_cnt - 1
                        elif ovfl_pkt_cnt > 0: 
                            self.dut._log.error(f"Duplicate deq (1) & ovfl ({ovfl_pkt_cnt}) packets: flow id: {flow} pkt id: {pkt}")
                            duplicate_cnt += ovfl_pkt_cnt

        inversion_check = inversion_cnt == 0
        missing_check = missing_cnt == 0
        duplicate_check = duplicate_cnt == 0
        assert all([reg_init_rc, enq_init_rc, ovfl_mon_rc, inversion_check, missing_check, duplicate_check, num_desc_check])

    class ClkRstInit():
        def __init__(self, dut):
            self.dut = dut
            self.dut.clk.value = 0
            self.dut.rst.value = 1

        async def start(self):
            # Clock
            clk = Clock(self.dut.clk, 2.857, 'ns')
            await cocotb.start(clk.start())
            # Reset pulse
            await RisingEdge(self.dut.clk)
            self.dut.rst.value = 1
            await RisingEdge(self.dut.clk)
            self.dut.rst.value = 0
            print("started clock")

    class RegInit():
        def __init__(self, dut):
            self.dut = dut
            self.reg_init_file = cocotb.regression_manager._test.name + ".conf"

        async def run(self): 
            result = True
            with open(self.reg_init_file) as f_reg_init:
                for line in f_reg_init:
                    # skip lines with comments
                    if len(line.strip().split()) == 3 and line[0] != '#':
                        cmd, reg_name, data_str = line.split()
                        if reg_name in ShrMemTb.reg_dict:
                            addr = ShrMemTb.reg_dict[reg_name]
                        else:
                            self.dut._log.error("Invalid register name: {}".format(reg_name))
                            op_result = False
                        if data_str[0:2] == '0x':
                            data = int(data_str, 16)
                        else:
                            data = int(data_str)
                        if cmd == 'W':
                            op_result, rdata = await ShrMemTb.reg_rw(self, addr, data, 1)
                        elif cmd == 'R':
                            op_result, rdata = await ShrMemTb.reg_rw(self, addr, data, 0)
                        else:
                            self.dut._log.error("Invalid command: {}".format(cmd))
                            op_result = False

                        result = result and op_result
            return result             

    class EnqInit():
        def __init__(self, dut):
            self.dut = dut
            self.enq_init_file = cocotb.regression_manager._test.name + ".enq"

        async def push(self, chan, data):
            self.dut.wr_chan.value = chan
            self.dut.din.value = data
            self.dut.wr_en.value = 1 
            await RisingEdge(self.dut.clk)
            self.dut.wr_en.value = 0
            await RisingEdge(self.dut.clk)
            while self.dut.wr_ack.value == 0:
                await RisingEdge(self.dut.clk)
            return True

        async def run(self):
            result = True
            rd_result = True
            ShrMemTb.enq_cnt = 0
            while self.dut.init_done.value != 1:
                await RisingEdge(self.dut.clk)
            with open(self.enq_init_file) as f_enq_init:
                for line in f_enq_init:
                    # skip lines with comments
                    if len(line.strip().split()) == 6 and line[0] != '#':
                        cmd, gap, pkt_len, fin_time, flow_id, pkt_id = line.split()
                        print(cmd, gap, pkt_len, fin_time, flow_id, pkt_id)
                        # Form enq desc 
                        desc = (int(pkt_len)  << (FIN_TIME_BIT_WIDTH + FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH)) + \
                               (int(fin_time) << (FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH)) + \
                               (int(flow_id)  << (PKT_ID_BIT_WIDTH)) + \
                               (int(pkt_id))
                        if int(flow_id) not in ShrMemTb.flow_enq:
                            ShrMemTb.flow_enq[int(flow_id)] = [int(pkt_id)]
                        else:
                            ShrMemTb.flow_enq[int(flow_id)].append(int(pkt_id))
                        push_rc = await self.push(int(flow_id), desc)
                        """
                        desc_lo = desc & (2**32 - 1) 
                        desc_hi = desc >> 32
                        gap_addr = ShrMemTb.reg_dict["MM_ENQ_GAP_BUFFER"] + ShrMemTb.enq_cnt * 2
                        await ShrMemTb.reg_rw(self, gap_addr , int(gap), 1)
                        enq_addr = ShrMemTb.reg_dict["MM_ENQ_BUFFER"] + ShrMemTb.enq_cnt * 2
                        await ShrMemTb.reg_rw(self, enq_addr, desc_hi, 1)
                        await ShrMemTb.reg_rw(self, enq_addr + 1, desc_lo, 1)
                        """
                        ShrMemTb.enq_cnt += 1
            """
            # Write number of enq descriptors
            await ShrMemTb.reg_rw(self, ShrMemTb.reg_dict["MM_ENQ_MAX_CNT"], ShrMemTb.enq_cnt, 1)
            rd_result, _ = await ShrMemTb.reg_rw(self, ShrMemTb.reg_dict["MM_ENQ_MAX_CNT"], ShrMemTb.enq_cnt, 0)
            result = result and rd_result 
            # Start ShrMemTb
            await ShrMemTb.reg_rw(self, ShrMemTb.reg_dict["MM_START"], 1, 1)
            rd_result, _ = await ShrMemTb.reg_rw(self, ShrMemTb.reg_dict["MM_START"], 0, 0)
            """
            result = result and rd_result 
            return result
            
            
    class DeqMon():
        def __init__(self, dut):
            self.dut = dut
            self.deq_mon_file = cocotb.regression_manager._test.name + ".deq"

        async def pop(self, chan):
            self.dut.rd_chan.value = chan
            self.dut.rd_en.value = 1 
            await RisingEdge(self.dut.clk)
            self.dut.rd_en.value = 0
            await RisingEdge(self.dut.clk)
            await ShrMemTb._wait_rd_data_valid(self, 4)
            return self.dut.dout

        async def run(self):
            inversion_cnt = 0
            ShrMemTb.deq_cnt = 0
            ShrMemTb.deq_mon_act <= True
            print (self.dut.empty.value.buff)
            
            for i in range (2**L2_NUM_FIFOS):
                print('flow id: ', i)
                rev_i = 2**L2_NUM_FIFOS - 1 - i
                while self.dut.empty.value[rev_i] != 1:
                    pop_rslt = await self.pop(i)
                    deq_pkt_id = pop_rslt.value.integer & (2**PKT_ID_BIT_WIDTH - 1)
                    deq_flow_id = (pop_rslt.value.integer >> PKT_ID_BIT_WIDTH) & (2**FLOW_ID_BIT_WIDTH - 1)
                    deq_fin_time = (pop_rslt.value.integer >> (FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH)) & \
                                      (2**FIN_TIME_BIT_WIDTH - 1)
                    deq_pkt_len = (pop_rslt.value.integer >> (FIN_TIME_BIT_WIDTH + FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH)) & \
                                      (2**FIN_TIME_BIT_WIDTH - 1)
                    if i in ShrMemTb.flow_deq:
                        ShrMemTb.flow_deq[i].append(deq_pkt_id)
                    else:
                        ShrMemTb.flow_deq[i] = [deq_pkt_id]
                    print('deq pkt len: ', deq_pkt_len)
                    print('deq fin time: ', deq_fin_time)
                    print('deq flow id: ', deq_flow_id)
                    print('deq pkt id: ', deq_pkt_id)


            """
            # Wait for start bit to be set
            while (self.dut.start_reg.value != 1):
            
                await RisingEdge(self.dut.mm_master_clk)
            self.dut._log.info('Received Start command')

            # Wait for dequeue delay to elapse
            while (self.dut.deq_delay_cnt.value.integer < self.dut.deq_delay_reg.value.integer):
                await RisingEdge(self.dut.mm_master_clk)
            self.dut._log.info('Dequeue delay elapsed')

            # Wait until deq activity stops
            act_cnt = 0
            while act_cnt < 64:
                await RisingEdge(self.dut.mm_master_clk)
                act_cnt += 1

                if self.dut.deq_buff_we.value == 1:
                    ShrMemTb.deq_cnt += 1
                    act_cnt = 0;

                if (ShrMemTb.deq_cnt + ShrMemTb.ovfl_cnt) == ShrMemTb.enq_cnt:
                    break;

            self.dut._log.info('Enq Cnt: {}, Deq Cnt: {}, Ovfl Cnt: {}'.format(ShrMemTb.enq_cnt, ShrMemTb.deq_cnt, ShrMemTb.ovfl_cnt))

            # Transfer dequeued descriptors from deq_buf to file
            with open(self.deq_mon_file, 'w+') as f_deq_mon:
                deq_cnt = 0
                prev_fin_time = 0
                inversion_cnt = 0
                while deq_cnt < ShrMemTb.deq_cnt:
                    deq_addr = ShrMemTb.reg_dict["MM_DEQ_BUFFER"] + deq_cnt * 2
                    deq_ts_addr = ShrMemTb.reg_dict["MM_DEQ_TS_BUFFER"] + deq_cnt * 2
                    op_result_lo, rdata_hi = await ShrMemTb.reg_rw(self, deq_addr, None, 0)
                    op_result_hi, rdata_lo = await ShrMemTb.reg_rw(self, deq_addr + 1, None, 0)
                    op_result, deq_ts = await ShrMemTb.reg_rw(self, deq_ts_addr, None, 0)
                    rdata = (rdata_hi << 32) + rdata_lo
                    pkt_len =  (rdata >> (FIN_TIME_BIT_WIDTH + FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH))
                    fin_time = (rdata >> (FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH)) & (2**FIN_TIME_BIT_WIDTH - 1) 
                    inversion = fin_time < prev_fin_time
                    inversion_cnt += int(inversion)
                    flow_id = (rdata >> PKT_ID_BIT_WIDTH) & (2**FLOW_ID_BIT_WIDTH - 1)
                    pkt_id = rdata & (2**PKT_ID_BIT_WIDTH - 1)
                    if not inversion:
                        prev_fin_time = fin_time
                    else:
                        self.dut._log.error(f"Inversion: {deq_ts.integer} {pkt_len} {prev_fin_time} {fin_time} {flow_id} {pkt_id}")
                    if flow_id in SifterTb.flow_deq:
                        SifterTb.flow_deq[flow_id].append(pkt_id)
                    else:
                        SifterTb.flow_deq[flow_id] = [pkt_id]

                    f_deq_mon.write(f"{deq_ts.integer} {pkt_len} {fin_time} {flow_id} {pkt_id} \n")
                    deq_cnt += 1
            """
            return inversion_cnt

    class OvflMon():
        def __init__(self, dut):
            self.dut = dut
            self.ovfl_mon_file = cocotb.regression_manager._test.name + ".ovfl"

        async def run(self):
            result = True
            ShrMemTb.ovfl_cnt = 0
            """
            # Wait until deq activity stops
            while ShrMemTb.deq_mon_act:
                await RisingEdge(self.dut.mm_master_clk)

                if self.dut.ovfl_buff_we.value == 1:
                    ShrMemTb.ovfl_cnt += 1
                
            # Transfer overflow descriptors from ovfl_buf to file
            with open(self.ovfl_mon_file, 'w+') as f_ovfl_mon:
                ovfl_cnt = 0
                while ovfl_cnt < ShrMemTb.ovfl_cnt:
                    ovfl_addr = ShrMemTb.reg_dict["MM_OVFL_BUFFER"] + ovfl_cnt * 2
                    op_result_lo, rdata_hi = await ShrMemTb.reg_rw(self, ovfl_addr, None, 0)
                    op_result_hi, rdata_lo = await ShrMemTb.reg_rw(self, ovfl_addr + 1, None, 0)
                    rdata = (rdata_hi << 32) + rdata_lo
                    pkt_len =  (rdata >> (FIN_TIME_BIT_WIDTH + FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH))
                    fin_time = (rdata >> (FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH)) & (2**FIN_TIME_BIT_WIDTH - 1) 
                    flow_id = (rdata >> PKT_ID_BIT_WIDTH) & (2**FLOW_ID_BIT_WIDTH - 1)
                    pkt_id = rdata & (2**PKT_ID_BIT_WIDTH - 1)
                    if flow_id in ShrMemTb.flow_ovfl:
                        ShrMemTb.flow_ovfl[flow_id].append(pkt_id)
                    else:
                        ShrMemTb.flow_ovfl[flow_id] = [pkt_id]
                    f_ovfl_mon.write(f"{pkt_len} {fin_time} {flow_id} {pkt_id} \n")
                    ovfl_cnt += 1
            """
            return True
