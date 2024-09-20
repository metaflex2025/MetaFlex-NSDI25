library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;
use IEEE.math_real.all;


entity find_earliest_non_empty_fifo is
  generic (
    g_L2_FIFO_NUM       : integer := 5;
    g_FIFO_NUM          : integer := 32;
    g_FIFO_ID           : integer := 0;
    g_LEVEL             : integer := 0
  );
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    find_earliest_non_empty_fifo_cmd : in  std_logic;
    current_fifo_index               : in  unsigned(2*g_L2_FIFO_NUM - 1 downto 0);
    empty                            : in  std_logic_vector(0 to g_FIFO_NUM - 1);
    find_earliest_non_empty_fifo_rsp : out std_logic;
    earliest_fifo_index              : out unsigned(g_L2_FIFO_NUM - 1 downto 0);
    earliest_fifo_right              : out std_logic;
    earliest_fifo_left               : out std_logic;
    all_fifos_empty                  : out std_logic
  );
end find_earliest_non_empty_fifo;

architecture find_earliest_non_empty_fifo_arch of find_earliest_non_empty_fifo is
begin

  p_find_first: process(rst, clk)
  variable v_earliest_found_right : boolean := false;
  variable v_earliest_found_left  : boolean := false;
  variable v_earliest_fifo_index  : integer range 0 to g_FIFO_NUM-1 := 0;
  begin
    if rst = '1' then
      v_earliest_found_right := false;  
      v_earliest_found_left  := false;
      v_earliest_fifo_index  := 0;
      find_earliest_non_empty_fifo_rsp <= '0';
      earliest_fifo_left               <= '0';
      earliest_fifo_right              <= '0';
      all_fifos_empty                  <= '1';
    elsif clk'event and clk = '1' then
      if find_earliest_non_empty_fifo_cmd = '1' then
        v_earliest_found_right := false;
        v_earliest_found_left  := false;
        for i in 0 to g_FIFO_NUM - 1 loop
          if g_FIFO_ID * g_FIFO_NUM * (1 - g_LEVEL) + i < 
              to_integer(current_fifo_index(2*g_L2_FIFO_NUM - 1 downto g_LEVEL * g_L2_FIFO_NUM)) then
            if empty(i) = '0' and not v_earliest_found_left then
              v_earliest_fifo_index := i;
              v_earliest_found_left := true;
            end if;
          else
            if empty(i) = '0' and not v_earliest_found_right then
              v_earliest_fifo_index  := i;
              v_earliest_found_right := true;
            end if;
          end if;
        end loop;
        if v_earliest_found_right then
            earliest_fifo_right    <= '1';
        else
            earliest_fifo_right    <= '0';
        end if;
        if v_earliest_found_left then
            earliest_fifo_left     <= '1';
        else
            earliest_fifo_left     <= '0';
        end if;
        if (not v_earliest_found_left and not v_earliest_found_right) then
            all_fifos_empty        <= '1';
        else
            all_fifos_empty        <= '0';
        end if;
      end if;
      find_earliest_non_empty_fifo_rsp <= find_earliest_non_empty_fifo_cmd;
      -- v_earliest_fifo_index holds the first "right" side value if any non-empty FIFOs are after
      -- current_fifo_index.  Otherwise it holds the first "left" side value.
      earliest_fifo_index        <= to_unsigned(v_earliest_fifo_index, g_L2_FIFO_NUM);
    end if;
  end process p_find_first;  

end find_earliest_non_empty_fifo_arch; 
