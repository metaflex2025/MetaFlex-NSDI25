-- gearbox top level module


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.math_real.ceil;
use IEEE.math_real.log2;
use ieee.std_logic_misc.all;

--library xpm;
--use xpm.vcomponents.all;
use work.mf_package.all;

entity metaflex_top is
  generic (
    g_L2_NUM_FIFOS      : integer := 8;
    g_NUM_FIFOS         : integer := 256;
    g_L2_NUM_MEM_SEGS   : integer := 6;
    g_NUM_MEM_SEGS      : integer := 64;
    g_L2_MEM_SEG_SIZE   : integer := 6;
    g_DESC_BIT_WIDTH    : integer := 64;
    g_L2_NUM_FLOWS      : integer := 10;
    g_RANK_WIDTH        : integer := 20;
    g_VC_BIT_WIDTH      : integer := 20;
    g_PKT_LEN_BIT_WIDTH : integer := 11;
    g_PKT_CNT_WIDTH     : integer := 8
  );
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    
    init_done                        : out std_logic;
    
    -- enq i/f
    enq_rdy                          : out std_logic;
    enq_cmd                          : in  std_logic;
    enq_desc                         : in  std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
    enq_done                         : out std_logic;
        
    -- ovfl out i/f
    ovfl_out                         : out std_logic;
    ovfl_desc_out                    : out std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
    
    -- deq i/f
    deq_rdy                          : out std_logic;
    deq_cmd                          : in  std_logic;
    deq_desc                         : out std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
    deq_desc_valid                   : out std_logic;
    all_fifos_empty                  : out std_logic;
    
    -- level pkt count i/f
    level_pkt_cnt                    : out unsigned(g_L2_NUM_FIFOS+g_PKT_CNT_WIDTH-1 downto 0)

  );
end metaflex_top;

architecture metaflex_top_arch of metaflex_top is

----------------
-- COMPONENTS --
----------------

component shared_mem_fifo
  generic (
    L2_NUM_FIFOS    : integer;
    NUM_FIFOS       : integer;
    L2_NUM_MEM_SEGS : integer;
    NUM_MEM_SEGS    : integer;
    L2_MEM_SEG_SIZE : integer;
    DESC_DATA_WIDTH : integer
    );
  port (
    rst           : in  std_logic;
    clk           : in  std_logic;
    init_done     : out std_logic := '0';
    wr_en         : in  std_logic;
    wr_chan       : in  std_logic_vector(L2_NUM_FIFOS-1 downto 0);
    din           : in  std_logic_vector(DESC_DATA_WIDTH-1 downto 0);
    wr_ack        : out std_logic;
    rd_en         : in  std_logic;
    rd_chan       : in  std_logic_vector(L2_NUM_FIFOS-1 downto 0);
    dout          : out std_logic_vector(DESC_DATA_WIDTH-1 downto 0);
    data_valid    : out std_logic;
    almost_empty  : out std_logic_vector(NUM_FIFOS-1 downto 0);
    almost_full   : out std_logic_vector(NUM_FIFOS-1 downto 0);
    empty         : out std_logic_vector(NUM_FIFOS-1 downto 0);
    full          : out std_logic;
    underflow     : out std_logic_vector(NUM_FIFOS-1 downto 0);
    overflow      : out std_logic_vector(NUM_FIFOS-1 downto 0)
    );
end component;

component find_earliest_non_empty_fifo_1k
  generic (
    g_L2_FIFO_NUM       : integer;
    g_FIFO_NUM          : integer
    );    
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    find_earliest_non_empty_fifo_cmd : in  std_logic;
    current_fifo_index               : in  unsigned(g_L2_FIFO_NUM - 1 downto 0);
    empty                            : in  std_logic_vector(0 to g_NUM_FIFOS - 1);
    find_earliest_non_empty_fifo_rsp : out std_logic;
    earliest_fifo_index              : out unsigned(g_L2_FIFO_NUM - 1 downto 0);
    all_fifos_empty                  : out std_logic
  );
end component;

component fin_time_arr
  generic (
    g_L2_FLOW_NUM  : integer := 10;    -- log2 of number of flows
    g_VC_BIT_WIDTH : integer := 20     -- VC bit width
  );
  PORT (
    a    : IN STD_LOGIC_VECTOR(g_L2_FLOW_NUM-1 DOWNTO 0);
    d    : IN STD_LOGIC_VECTOR(g_VC_BIT_WIDTH-1 DOWNTO 0);
    dpra : IN STD_LOGIC_VECTOR(g_L2_FLOW_NUM-1 DOWNTO 0);
    clk  : IN STD_LOGIC;
    we   : IN STD_LOGIC;
    dpo  : OUT STD_LOGIC_VECTOR(g_VC_BIT_WIDTH-1 DOWNTO 0)
  );
end component;

---------------
-- CONSTANTS --
---------------
  constant c_L2_MIN_GRAN                  : integer := 0;
  constant c_MIN_GRAN                     : integer := 2 ** c_L2_MIN_GRAN;
  
-------------
-- SIGNALS --
-------------
  type   t_pkt_cnt_array           is array(0 to g_NUM_FIFOS-1)  of unsigned(g_PKT_CNT_WIDTH-1 downto 0);
  type   t_enq_state               is (IDLE, ENQ_OP);
  type   t_deq_state               is (IDLE, WAIT_EARLIEST_FIFO);

  signal find_earliest_non_empty_fifo_cmd : std_logic;
  signal find_earliest_non_empty_fifo_rsp : std_logic;
  signal earliest_fifo_index              : unsigned(g_L2_NUM_FIFOS-1 downto 0);
  signal ovfl_out_ft                      : std_logic;
  signal ovfl_desc_out_ft                 : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
  signal ovfl_out_sm                      : std_logic_vector(g_NUM_FIFOS-1 downto 0);

  signal fin_time_dpra                    : std_logic_vector(g_L2_NUM_FLOWS-1 downto 0);
  signal fin_time_we                      : std_logic;
  signal fin_time_dpo                     : std_logic_vector(g_RANK_WIDTH-1 downto 0);
  signal flow_id                          : unsigned(g_L2_NUM_FLOWS-1 downto 0);
  signal fin_time                         : unsigned(g_RANK_WIDTH-1 downto 0);
  signal ft_incr                          : unsigned(g_RANK_WIDTH-1 downto 0);
  signal vc                               : unsigned(g_VC_BIT_WIDTH-1 downto 0);
  signal vc_update                        : std_logic;

  signal empty                            : std_logic_vector(g_NUM_FIFOS-1 downto 0);
  signal empty_flip                       : std_logic_vector(0 to g_NUM_FIFOS-1);
  signal enq_state                        : t_enq_state;
  signal enq_desc_d1                      : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
  signal enq_desc_d2                      : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
  signal enq_fifo_pkt_cnt                 : t_pkt_cnt_array;
  signal enq_done_sm                      : std_logic;
  signal enq_done_of                      : std_logic;
  signal enq_done_of_d1                   : std_logic;
  signal enq_done_of_d2                   : std_logic;
    
  signal deq_state                        : t_deq_state;
  signal deq_fifo_pkt_cnt                 : t_pkt_cnt_array;
  signal deq_cmd_d1                       : std_logic;
  signal dout                             : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
  signal data_valid                       : std_logic;
  
  signal wr_en                            : std_logic;
  signal wr_chan                          : std_logic_vector(g_L2_NUM_FIFOS-1 downto 0);
  signal rd_en                            : std_logic;
  signal rd_chan                          : std_logic_vector(g_L2_NUM_FIFOS-1 downto 0);
 
begin
  
  i_shr_mem: shared_mem_fifo
  generic map (      
    L2_NUM_FIFOS        => g_L2_NUM_FIFOS,
    NUM_FIFOS           => g_NUM_FIFOS,
    L2_NUM_MEM_SEGS     => g_L2_NUM_MEM_SEGS,
    NUM_MEM_SEGS        => g_NUM_MEM_SEGS,
    L2_MEM_SEG_SIZE     => g_L2_MEM_SEG_SIZE,
    DESC_DATA_WIDTH     => g_DESC_BIT_WIDTH
  )
  port map (
    rst                 => rst,
    clk                 => clk,
    init_done           => init_done,
    wr_en               => wr_en,
    wr_chan             => wr_chan,
    din                 => enq_desc_d2,
    wr_ack              => enq_done_sm,
    rd_en               => rd_en,
    rd_chan             => rd_chan,
    dout                => dout,
    data_valid          => data_valid,
    almost_empty        => open,
    almost_full         => open,
    empty               => empty,
    full                => open,
    underflow           => open,
    overflow            => ovfl_out_sm
  );

  -- Flip empty vector
  flip_gen: for i in 0 to g_NUM_FIFOS - 1 generate
    empty_flip(i) <= empty(i);
  end generate;

  deq_desc       <= dout;
  deq_desc_valid <= data_valid;
  
  i_find_earliest: find_earliest_non_empty_fifo_1k
  generic map(
    g_L2_FIFO_NUM       => g_L2_NUM_FIFOS,
    g_FIFO_NUM          => g_NUM_FIFOS
    )  
  port map(
    rst                              => rst,
    clk                              => clk,
    find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd,
    current_fifo_index               => vc(g_L2_NUM_FIFOS - 1 downto 0),
    empty                            => empty_flip,
    find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp,
    earliest_fifo_index              => earliest_fifo_index,
    all_fifos_empty                  => all_fifos_empty
  );
  
  -- Finish time table
  fin_time_dpra <= enq_desc(g_DESC_BIT_WIDTH - g_PKT_LEN_BIT_WIDTH - g_RANK_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - g_PKT_LEN_BIT_WIDTH - g_RANK_WIDTH - g_L2_NUM_FLOWS);
 
  i_fin_time_arr: fin_time_arr
  generic map (
    g_L2_FLOW_NUM  => g_L2_NUM_FLOWS,    -- log2 of number of flows
    g_VC_BIT_WIDTH => g_VC_BIT_WIDTH     -- VC bit width
  )
  PORT MAP (
    a    => std_logic_vector(flow_id),
    d    => std_logic_vector(fin_time),
    dpra => fin_time_dpra,
    clk  => clk,
    we   => fin_time_we,
    dpo  => fin_time_dpo
  );

  -- Enqueue process
  p_enqueue: process(rst, clk)
  variable v_pkt_time : unsigned(g_RANK_WIDTH-1 downto 0);
  variable v_flow_id  : unsigned(g_L2_NUM_FLOWS-1 downto 0);
  begin
    if rst = '1' then
      enq_fifo_pkt_cnt   <= (others => (others => '0'));
      ovfl_out_ft        <= '0';
      enq_rdy            <= '1';
      enq_state          <= IDLE;
      fin_time_we        <= '0';
      enq_done_of        <= '0';
      
    elsif clk'event and clk = '1' then
      -- defaults
      ovfl_out_ft        <= '0';
      fin_time_we        <= '0';
      wr_en              <= '0';
      enq_done_of        <= '0';
                  
      -- Enqueue state machine
      case enq_state is
        when IDLE => 
          if enq_cmd = '1' then
            -- extract packet time and flow id from descriptor
            v_pkt_time := unsigned(enq_desc(g_DESC_BIT_WIDTH - g_PKT_LEN_BIT_WIDTH - 1 downto 
                                            g_DESC_BIT_WIDTH - g_PKT_LEN_BIT_WIDTH - g_RANK_WIDTH));
            v_flow_id  := unsigned(enq_desc(g_DESC_BIT_WIDTH - g_PKT_LEN_BIT_WIDTH - g_RANK_WIDTH - 1 downto 
                                            g_DESC_BIT_WIDTH - g_PKT_LEN_BIT_WIDTH - g_RANK_WIDTH - g_L2_NUM_FLOWS));
            flow_id    <= v_flow_id;
            -- calculate incremental and actual finish time
            if vc >= unsigned(fin_time_dpo) then
              ft_incr  <= v_pkt_time;
              fin_time <= v_pkt_time + vc;
            else
              ft_incr  <= v_pkt_time + unsigned(fin_time_dpo) - vc;
              fin_time <= v_pkt_time + unsigned(fin_time_dpo);
            end if;
            enq_desc_d1 <= enq_desc;
            enq_rdy     <= '0';
            enq_state   <= ENQ_OP;
          end if;
          
        when ENQ_OP =>
          -- Replace pkt time with finish time
          enq_desc_d2 <= enq_desc_d1(g_DESC_BIT_WIDTH-1 downto g_DESC_BIT_WIDTH-g_PKT_LEN_BIT_WIDTH) &
                         std_logic_vector(fin_time) & enq_desc_d1(g_L2_NUM_FLOWS+PKT_ID_BIT_WIDTH-1 downto 0);
          -- if incremental finish time greater than max level capacity, drop pkt (check MSB bits)
          if ft_incr > to_unsigned(g_NUM_FIFOS*c_MIN_GRAN, g_RANK_WIDTH) then
            ovfl_desc_out_ft <= enq_desc;
            ovfl_out_ft <= '1';
            enq_done_of <= '1';
          else
            -- write shared memory FIFO
            wr_en   <= '1';
            wr_chan <= std_logic_vector(fin_time(g_L2_NUM_FIFOS-1 downto 0));
            -- update fin_time_arr entry for flow_id
            fin_time_we <= '1';
          end if;
          
          -- Update enq packet counts
          enq_rdy      <= '1';
          enq_state    <= IDLE;

        when others =>
          enq_state   <= IDLE;
          
      end case;
      
      enq_done_of_d1 <= enq_done_of;
      enq_done_of_d2 <= enq_done_of_d1;
        
    end if;
  end process p_enqueue;
  enq_done <= enq_done_sm or enq_done_of_d2;
  ovfl_out <= or_reduce(ovfl_out_sm) or ovfl_out_ft;
  ovfl_desc_out <= ovfl_desc_out_ft when ovfl_out_ft = '1' else
                   enq_desc;

  -- dequeue process
  p_dequeue: process(rst, clk)  
  begin
    if rst = '1' then
      vc             <= (others => '0');
      vc_update      <= '0';
      find_earliest_non_empty_fifo_cmd <= '0';
      deq_rdy        <= '1';
    elsif clk'event and clk = '1' then
      vc_update      <= '0';
      find_earliest_non_empty_fifo_cmd <= '0';
      deq_cmd_d1     <= deq_cmd;
      -- Dequeue state machine
      case deq_state is
        when IDLE => 
          if deq_cmd = '1' then
            find_earliest_non_empty_fifo_cmd <= '1';
            deq_rdy <= '0';
            deq_state <= WAIT_EARLIEST_FIFO;
          end if;
          
        when WAIT_EARLIEST_FIFO =>
          if find_earliest_non_empty_fifo_rsp = '1' then
            deq_rdy <= '1';
            deq_state <= IDLE;
          end if;
          
        when others =>
          deq_state <= IDLE;
          
      end case;
    
      rd_en <= find_earliest_non_empty_fifo_rsp;
      rd_chan <= std_logic_vector(earliest_fifo_index);
      
      if data_valid = '1' then
        vc <= unsigned(dout(g_DESC_BIT_WIDTH - g_PKT_LEN_BIT_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - g_PKT_LEN_BIT_WIDTH - g_RANK_WIDTH));
        vc_update <= '1';
      end if;
    end if;
  end process p_dequeue;
  --deq_rdy <=  not deq_cmd and not deq_cmd_d1;
       
end metaflex_top_arch;
