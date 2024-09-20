-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity shared_mem_fifo is
  generic (
    L2_NUM_FIFOS    : integer := 8;
    NUM_FIFOS       : integer := 256;
    L2_NUM_MEM_SEGS : integer := 8;
    NUM_MEM_SEGS    : integer := 256;
    L2_MEM_SEG_SIZE : integer := 6;
    DESC_DATA_WIDTH : integer := 64
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
end shared_mem_fifo;
 
architecture rtl of shared_mem_fifo is

component simple_dual_one_clock
generic(
    ADDR_WIDTH : integer;
    DATA_WIDTH : integer
);
port(
    clk : in std_logic;
    ena : in std_logic;
    enb : in std_logic;
    wea : in std_logic;
    addra : in std_logic_vector(ADDR_WIDTH-1 downto 0);
    addrb : in std_logic_vector(ADDR_WIDTH-1 downto 0);
    dia : in std_logic_vector(DATA_WIDTH-1 downto 0);
    dob : out std_logic_vector(DATA_WIDTH-1 downto 0)
);
end component;

component rams_tdp_rf_rf
generic(
  ADDR_WIDTH : integer;
  DATA_WIDTH : integer
);
port(
  clka : in std_logic;
  clkb : in std_logic;
  ena : in std_logic;
  enb : in std_logic;
  wea : in std_logic;
  web : in std_logic;
  addra : in std_logic_vector(ADDR_WIDTH-1 downto 0);
  addrb : in std_logic_vector(ADDR_WIDTH-1 downto 0);
  dia : in std_logic_vector(DATA_WIDTH-1 downto 0);
  dib : in std_logic_vector(DATA_WIDTH-1 downto 0);
  doa : out std_logic_vector(DATA_WIDTH-1 downto 0);
  dob : out std_logic_vector(DATA_WIDTH-1 downto 0)
);
end component; 

constant MEM_SEG_SIZE : integer := 2**L2_MEM_SEG_SIZE;

signal rd_ptr_we     : std_logic;
signal rd_ptr_addra  : std_logic_vector(L2_NUM_FIFOS-1 downto 0);
signal init_rd_ptr_addrb  : std_logic_vector(L2_NUM_FIFOS-1 downto 0);
signal rd_ptr_di     : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE -1 downto 0);
signal init_rd_ptr_dib    : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE -1 downto 0);
signal rd_ptr_doa    : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE -1 downto 0);
signal wr_ptr_we     : std_logic;
signal init_rd_ptr_web    : std_logic;
signal wr_ptr_addra  : std_logic_vector(L2_NUM_FIFOS-1 downto 0);
signal wr_ptr_addrb  : std_logic_vector(L2_NUM_FIFOS-1 downto 0);
signal wr_ptr_di     : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE - 1 downto 0);
signal wr_ptr_doa    : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE - 1 downto 0);
signal wr_ptr_dob    : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE - 1 downto 0);
signal fll_mem_wea   : std_logic;
signal fll_mem_web   : std_logic;
signal fll_mem_addra : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal fll_mem_addrb : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal fll_mem_dia   : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal fll_mem_dib   : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal fll_mem_doa   : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal fll_mem_dob   : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal shr_mem_we    : std_logic;
signal shr_mem_waddr : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE - 1 downto 0);
signal shr_mem_di    : std_logic_vector(DESC_DATA_WIDTH - 1 downto 0);
signal shr_mem_re    : std_logic;
signal shr_mem_raddr : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE - 1 downto 0);
signal shr_mem_do    : std_logic_vector(DESC_DATA_WIDTH - 1 downto 0);
signal rd_ptr        : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE-1 downto 0);
signal wr_ptr        : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE-1 downto 0);
signal wr_ptr_b      : std_logic_vector(L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE-1 downto 0);
signal fl_head       : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal fl_tail       : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal prev_seg      : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal next_free_seg : std_logic_vector(L2_NUM_MEM_SEGS-1 downto 0);
signal wr_ptr_rd     : std_logic;
signal rd_ptr_rd     : std_logic;
signal link_seg      : std_logic;
signal wr_ptr_rdy    : std_logic;
signal rd_fll_rdy    : std_logic;
signal rd_ptr_rdy    : std_logic;
signal init_cnt      : unsigned(L2_NUM_MEM_SEGS - 1 downto 0);
signal empty_int     : std_logic_vector(NUM_FIFOS-1 downto 0); 
signal full_int      : std_logic;

type PUSH_STATE_TYPE is (INIT_FLL, IDLE, CHK_SEG, WRITE_DATA);
signal push_state : PUSH_STATE_TYPE;
type POP_STATE_TYPE is (IDLE, GET_RD_PTR, READ_DATA);
signal pop_state : POP_STATE_TYPE;

begin
 
  rd_ptr_mem: rams_tdp_rf_rf
  generic map (
    ADDR_WIDTH => L2_NUM_FIFOS,
    DATA_WIDTH => L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE
  )
  port map(
    clka  => clk,
    clkb  => clk,
    ena   => '1',
    enb   => '1',
    wea   => rd_ptr_we,
    web   => init_rd_ptr_web,
    addra => rd_ptr_addra,
    addrb => init_rd_ptr_addrb,
    dia   => rd_ptr_di,
    dib   => init_rd_ptr_dib,
    doa   => rd_ptr_doa,
    dob   => open
 );

  wr_ptr_mem: rams_tdp_rf_rf
  generic map (
    ADDR_WIDTH => L2_NUM_FIFOS,
    DATA_WIDTH => L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE
  )
  port map (
    clka  => clk,
    clkb  => clk,
    ena   => '1',
    enb   => '1',
    wea   => wr_ptr_we,
    web   => '0',
    addra => wr_ptr_addra,
    addrb => wr_ptr_addrb,
    dia   => wr_ptr_di,
    dib   => (others => '0'),
    doa   => wr_ptr_doa,
    dob   => wr_ptr_dob
  );

  fll_mem: rams_tdp_rf_rf
  generic map (
    ADDR_WIDTH => L2_NUM_MEM_SEGS,
    DATA_WIDTH => L2_NUM_MEM_SEGS
  )
  port map (
    clka  => clk,
    clkb  => clk,
    ena   => '1',
    enb   => '1',
    wea   => fll_mem_wea,
    web   => fll_mem_web,
    addra => fll_mem_addra,
    addrb => fll_mem_addrb,
    dia   => fll_mem_dia,
    dib   => fll_mem_dib,
    doa   => fll_mem_doa,
    dob   => fll_mem_dob
  );
  
  shr_mem: simple_dual_one_clock
  generic map (
    ADDR_WIDTH => L2_NUM_MEM_SEGS + L2_MEM_SEG_SIZE,
    DATA_WIDTH => DESC_DATA_WIDTH
  )
  port map (
    clk   => clk,
    ena   => shr_mem_we,
    enb   => shr_mem_re,
    wea   => shr_mem_we,
    addra => shr_mem_waddr,
    addrb => shr_mem_raddr,
    dia   => shr_mem_di,
    dob   => shr_mem_do    
  );

  p_smfifo : process (clk) is
  begin
    if rising_edge(clk) then
      if rst = '1' then
        wr_ptr_rd <= '0';
        wr_ptr_we <= '0';
        wr_ptr_rdy <= '0';
        rd_ptr_rd <= '0';
        rd_ptr_we <= '0';
        init_rd_ptr_web <= '0';
        rd_ptr_rdy <= '0';
        shr_mem_we <= '0';
        shr_mem_re <= '0';
        fll_mem_wea <= '0';
        fll_mem_web <= '0';
        empty_int <= (others => '1');
        full_int  <= '0';
        overflow <= (others => '0');
        wr_ack <= '0';
        data_valid <= '0';
        link_seg <= '0';
        push_state <= INIT_FLL;
        pop_State <= IDLE;
        init_cnt <= (others => '0');
        init_done <= '0';
        fl_head  <= (others => '0');
        fl_tail  <= std_logic_vector(to_unsigned(NUM_MEM_SEGS - 1, fl_tail'length));
        prev_seg <= (others => '0');
        
      else      
        wr_ptr_rd <= '0';
        wr_ptr_we <= '0';
        rd_ptr_rd <= '0';
        rd_ptr_we <= '0';
        init_rd_ptr_web <= '0';
        shr_mem_we <= '0';
        shr_mem_re <= '0';
        fll_mem_wea <= '0';
        fll_mem_web <= '0';
        wr_ack <= '0';
        data_valid <= '0';
        link_seg <= '0';
        overflow <= (others => '0');
        
        case push_state is
        when INIT_FLL =>
          fll_mem_addra <= std_logic_vector(init_cnt);
          fll_mem_dia <= std_logic_vector(init_cnt + 1);
          fll_mem_wea <= '1';
          init_cnt <= init_cnt + 1;
          if init_cnt = NUM_MEM_SEGS - 1 then
            init_done <= '1';
            push_state <= IDLE;
          end if;
          
        when IDLE =>
          if wr_en = '1' then
            -- Get write pointer
            wr_ptr_rd <= '1';
            wr_ptr_addra <= wr_chan;
            -- Prepare to write read ptr mem in case FIFO channel was previously empty
            init_rd_ptr_addrb <= wr_chan;
            -- Drive head address to FLL to get next free seg
            fll_mem_addra <= fl_head;
            -- Go to next state
            push_state <= CHK_SEG;
          end if;
          
        when CHK_SEG =>
          wr_ptr_rdy <= wr_ptr_rd;
          if wr_ptr_rdy = '1' then
            -- Get write pointer from Free List if segment is empty or full
            if unsigned(wr_ptr_doa(L2_MEM_SEG_SIZE - 1 downto 0)) = 0
                 or empty_int(to_integer(unsigned(wr_chan))) = '1' then
               wr_ptr <= fl_head & (L2_MEM_SEG_SIZE - 1 downto 0 => '0');
               -- Drive address to read next FL ptr.  Value is stored in IDLE state
               fll_mem_addra <= prev_seg;
               -- Set flag to link segment in next state
               link_seg <= '1';
               full_int <= '0';
            else
              wr_ptr <= wr_ptr_doa;
            end if;
            
            -- If chan's FIFO is empty, write new segment address in rd ptr mem
            if empty_int(to_integer(unsigned(wr_chan))) = '1' then
              init_rd_ptr_dib <= fl_head & (L2_MEM_SEG_SIZE - 1 downto 0 => '0'); 
              init_rd_ptr_web <= '1';
              -- If adding a segment to an empty FIFO, set prev ptr to current write ptr
              prev_seg <= fl_head;
            elsif unsigned(wr_ptr_doa(L2_MEM_SEG_SIZE - 1 downto 0)) = 0 then
              -- Seg was not empty, store prev_seg ptr
              prev_seg <= wr_ptr_doa(wr_ptr'LEFT downto L2_MEM_SEG_SIZE);
              
            else
              if fl_head = fl_tail and unsigned(wr_ptr) = L2_MEM_SEG_SIZE - 2 then
                full_int <= '1';
                overflow(to_integer(unsigned(wr_chan))) <= '1';
                push_state <= IDLE;
              else
                full_int <= '0';
              end if;
            end if;

            -- Save next free segment for later
            next_free_seg <= fll_mem_doa;
            
            -- Go to next state
            push_state <= WRITE_DATA;
          end if;

        when WRITE_DATA =>
          -- Write data in shared memory
          shr_mem_waddr <= wr_ptr;
          shr_mem_di <= din;
          shr_mem_we <= '1';
          -- Clear channel's empty flag
          empty_int(to_integer(unsigned(wr_ptr_addra))) <= '0';
          -- Update write pointer memory
          wr_ptr_di <= std_logic_vector(unsigned(wr_ptr) + 1);
          wr_ptr_we <= '1';
          -- Set full flag if last location of segment was written and 
          if fl_head = fl_tail and unsigned(wr_ptr) = L2_MEM_SEG_SIZE - 2 then
            full_int <= '1';
          else
            full_int <= '0';
          end if;
          -- If a new segment was linked, update the link information
          if link_seg = '1' then
            --fll_mem_addra <= prev_seg;
            fll_mem_dia <= fl_head;
            fll_mem_wea <= '1';
            -- Assign head to next free segment
            fl_head <= next_free_seg;
          end if;
          wr_ack <= '1';
          push_state <= IDLE;
            
        end case;     

        case pop_state is
        when IDLE =>
          if rd_en = '1' then
            rd_ptr_rd <= '1';
            rd_ptr_addra <= rd_chan;
            wr_ptr_addrb <= rd_chan;
            pop_state <= GET_RD_PTR;
          end if;
          
        when GET_RD_PTR =>
          rd_ptr_rdy <= rd_ptr_rd;
          if rd_ptr_rdy = '1' then
            rd_ptr <= rd_ptr_doa;
            shr_mem_raddr <= rd_ptr_doa;
            shr_mem_re <= '1';
            wr_ptr_b <= wr_ptr_dob;
            -- Read next ptr in READ_DATA state
            fll_mem_addrb <= rd_ptr_doa(wr_ptr'LEFT downto L2_MEM_SEG_SIZE);
            pop_state <= READ_DATA;
          end if;
          
        when READ_DATA =>
          if std_logic_vector(unsigned(rd_ptr) + 1) = wr_ptr_b or 
             unsigned(rd_ptr(L2_MEM_SEG_SIZE - 1 downto 0)) = to_unsigned(MEM_SEG_SIZE - 1, L2_MEM_SEG_SIZE) then
            -- link tail seg to freed segment
            fll_mem_addrb <= fl_tail;
            fll_mem_dib <= rd_ptr(rd_ptr'LEFT downto L2_MEM_SEG_SIZE);
            fll_mem_web <= '1';
            -- tail ptr = freed seg
            fl_tail <= rd_ptr(rd_ptr'LEFT downto L2_MEM_SEG_SIZE);
            -- Check for empty condition: next ptr = current ptr
            if std_logic_vector(unsigned(rd_ptr) + 1) = wr_ptr_b then
              empty_int(to_integer(unsigned(rd_ptr_addra))) <= '1';
            else
              -- read ptr from fll mem (addr set in GET_RD_PTR state)
              rd_ptr_di <= fll_mem_dob & (L2_MEM_SEG_SIZE - 1 downto 0 => '0');
            end if;
          else
            rd_ptr_di <= std_logic_vector(unsigned(rd_ptr) + 1);
          end if;
          rd_ptr_we <= '1';
          pop_state <= IDLE;
          data_valid <= '1';
 
        end case;     
      end if;                           -- sync reset
    end if;                             -- rising_edge(i_clk)
  end process p_smfifo;
    
  dout  <= shr_mem_do;
  empty <= empty_int;
  full  <= full_int;
  
  -- ASSERTION LOGIC - Not synthesized
  -- synthesis translate_off
 
  p_ASSERT : process (clk) is
  begin
    if rising_edge(clk) then
      underflow <= (others => '0');
      
      if wr_en = '1' and full_int = '1' then
        report "ASSERT FAILURE: FIFO " & integer'image(to_integer(unsigned(wr_chan))) & " IS FULL AND BEING WRITTEN " severity warning;
      end if;
 
      if rd_en = '1' and empty_int(to_integer(unsigned(rd_chan))) = '1' then
        underflow(to_integer(unsigned(rd_chan))) <= '1';
        report "ASSERT FAILURE: FIFO " & integer'image(to_integer(unsigned(rd_chan))) & " IS EMPTY AND BEING READ" severity warning;
      end if;
    end if;
  end process p_ASSERT;
 
  -- synthesis translate_on
end rtl;


