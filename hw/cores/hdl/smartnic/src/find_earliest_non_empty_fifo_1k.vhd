
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;
use IEEE.math_real.all;

entity find_earliest_non_empty_fifo_1k is
  generic (
    g_L2_FIFO_NUM       : integer := 10;
    g_FIFO_NUM          : integer := 2**10
    );    
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    find_earliest_non_empty_fifo_cmd : in  std_logic;
    current_fifo_index               : in  unsigned(g_L2_FIFO_NUM - 1 downto 0);
    empty                            : in  std_logic_vector(0 to g_FIFO_NUM - 1);
    find_earliest_non_empty_fifo_rsp : out std_logic;
    earliest_fifo_index              : out unsigned(g_L2_FIFO_NUM - 1 downto 0);
    all_fifos_empty                  : out std_logic
  );
end find_earliest_non_empty_fifo_1k;

architecture find_earliest_non_empty_fifo_1k_arch of find_earliest_non_empty_fifo_1k is
component find_earliest_non_empty_fifo
  generic (
    g_L2_FIFO_NUM       : integer;
    g_FIFO_NUM          : integer;
    g_FIFO_ID           : integer;
    g_LEVEL             : integer
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
end component find_earliest_non_empty_fifo;

constant g_L2_LEAF_FIFO_NUM : integer := 5;
constant g_LEAF_FIFO_NUM    : integer := 2**g_L2_LEAF_FIFO_NUM;
constant L2_ROOT_FIFO_NUM   : integer := g_L2_FIFO_NUM - g_L2_LEAF_FIFO_NUM;
constant ROOT_FIFO_NUM      : integer := 2**L2_ROOT_FIFO_NUM;

signal prev_find_earliest_non_empty_fifo_rsp : std_logic_vector(ROOT_FIFO_NUM - 1 downto 0);
signal prev_earliest_fifo_index          : unsigned(ROOT_FIFO_NUM * g_L2_LEAF_FIFO_NUM - 1 downto 0);
signal prev_earliest_fifo_right          : std_logic_vector(0 to ROOT_FIFO_NUM - 1);
signal prev_earliest_fifo_right_d1       : std_logic;
signal prev_earliest_fifo_left           : std_logic_vector(0 to ROOT_FIFO_NUM - 1);
signal prev_earliest_fifo_left_d1        : std_logic;
signal prev_all_fifos_empty              : std_logic_vector(0 to ROOT_FIFO_NUM - 1);
signal prev_all_fifos_empty_right        : std_logic_vector(0 to ROOT_FIFO_NUM - 1);
signal prev_all_fifos_empty_left         : std_logic_vector(0 to ROOT_FIFO_NUM - 1);
signal earliest_fifo_index_right         : unsigned(L2_ROOT_FIFO_NUM - 1 downto 0);
signal earliest_fifo_right_right         : std_logic;
signal earliest_fifo_left_right          : std_logic;
signal prev_earliest_fifo_index_right_d1 : unsigned(g_L2_LEAF_FIFO_NUM - 1 downto 0);
signal earliest_fifo_index_left          : unsigned(L2_ROOT_FIFO_NUM - 1 downto 0);
signal prev_earliest_fifo_index_left_d1  : unsigned(g_L2_LEAF_FIFO_NUM - 1 downto 0);
signal earliest_fifo_right_left          : std_logic;
signal earliest_fifo_left_left           : std_logic;
signal all_fifos_empty_right             : std_logic;
signal all_fifos_empty_left              : std_logic;

begin

    tree_gen: for i in 0 to ROOT_FIFO_NUM - 1 generate
        ff_i: find_earliest_non_empty_fifo
              generic map (
                  g_L2_FIFO_NUM      => g_L2_LEAF_FIFO_NUM,
                  g_FIFO_NUM         => g_LEAF_FIFO_NUM,
                  g_FIFO_ID          => i,
                  g_LEVEL            => 0
             )
              port map (
                  rst                              => rst,
                  clk                              => clk,
                  find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd,
                  current_fifo_index               => current_fifo_index(2*g_L2_LEAF_FIFO_NUM - 1 downto 0),
                  empty                            => empty(i*g_LEAF_FIFO_NUM to (i+1)*g_LEAF_FIFO_NUM - 1),
                  find_earliest_non_empty_fifo_rsp => prev_find_earliest_non_empty_fifo_rsp(i),
                  earliest_fifo_index              => prev_earliest_fifo_index((i+1)*g_L2_LEAF_FIFO_NUM - 1 downto i*g_L2_LEAF_FIFO_NUM),
                  earliest_fifo_right              => prev_earliest_fifo_right(i),
                  earliest_fifo_left               => prev_earliest_fifo_left(i),
                  all_fifos_empty                  => prev_all_fifos_empty(i)
              );
    end generate tree_gen;
    
    --prev_all_fifos_empty_right <= not(not prev_all_fifos_empty and prev_earliest_fifo_right);
    --prev_all_fifos_empty_left  <= not(not prev_all_fifos_empty and prev_earliest_fifo_left);
    
    p_rclk_prev_efi: process(rst, clk)
    begin
        if rst = '1' then
            prev_earliest_fifo_right_d1 <= '0';
            prev_earliest_fifo_left_d1  <= '0';
        elsif clk'event and clk = '1' then
            prev_efi_loop: for i in ROOT_FIFO_NUM - 1 downto 0 loop
                if i < to_integer(current_fifo_index(g_L2_FIFO_NUM - 1 downto g_L2_LEAF_FIFO_NUM)) then
                    if prev_earliest_fifo_left(i) = '1' and prev_find_earliest_non_empty_fifo_rsp(i) = '1' then
                        prev_earliest_fifo_index_left_d1 <= prev_earliest_fifo_index((i+1)*g_L2_LEAF_FIFO_NUM - 1 downto i*g_L2_LEAF_FIFO_NUM);
                        prev_earliest_fifo_right_d1 <= '0';
                        prev_earliest_fifo_left_d1  <= '1';
                    end if;
                else
                    if prev_earliest_fifo_right(i) = '1' and prev_find_earliest_non_empty_fifo_rsp(i) = '1' then
                        prev_earliest_fifo_index_right_d1 <= prev_earliest_fifo_index((i+1)*g_L2_LEAF_FIFO_NUM - 1 downto i*g_L2_LEAF_FIFO_NUM);
                        prev_earliest_fifo_right_d1 <= '1';
                        prev_earliest_fifo_left_d1  <= '0';
                    end if;
                end if;
            end loop prev_efi_loop;
        end if;
    end process p_rclk_prev_efi;
    
    last_ff_right_i: find_earliest_non_empty_fifo
        generic map (
            g_L2_FIFO_NUM      => L2_ROOT_FIFO_NUM,
            g_FIFO_NUM         => ROOT_FIFO_NUM,
            g_FIFO_ID          => 0,
            g_LEVEL            => 1
            )
        port map (
            rst                              => rst,
            clk                              => clk,
            find_earliest_non_empty_fifo_cmd => prev_find_earliest_non_empty_fifo_rsp(0),
            current_fifo_index               => current_fifo_index(g_L2_FIFO_NUM - 1 downto 0),
            empty                            => prev_all_fifos_empty, --_right,
            find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp,
            earliest_fifo_index              => earliest_fifo_index_right,
            earliest_fifo_right              => earliest_fifo_right_right,
            earliest_fifo_left               => earliest_fifo_left_right,
            all_fifos_empty                  => all_fifos_empty_right
        );

    last_ff_left_i: find_earliest_non_empty_fifo
        generic map (
            g_L2_FIFO_NUM      => L2_ROOT_FIFO_NUM,
            g_FIFO_NUM         => ROOT_FIFO_NUM,
            g_FIFO_ID          => 0,
            g_LEVEL            => 1
        )
        port map (
            rst                              => rst,
            clk                              => clk,
            find_earliest_non_empty_fifo_cmd => prev_find_earliest_non_empty_fifo_rsp(0),
            current_fifo_index               => current_fifo_index(g_L2_FIFO_NUM - 1 downto 0),
            empty                            => prev_all_fifos_empty, --_left,
            find_earliest_non_empty_fifo_rsp => open,
            earliest_fifo_index              => earliest_fifo_index_left,
            earliest_fifo_right              => earliest_fifo_right_left,
            earliest_fifo_left               => earliest_fifo_left_left,
            all_fifos_empty                  => all_fifos_empty_left
    );
    earliest_fifo_index <= (earliest_fifo_index_right & prev_earliest_fifo_index_right_d1) 
                               when earliest_fifo_right_right =  '1' and all_fifos_empty_right = '0' else
                           (earliest_fifo_index_left  & prev_earliest_fifo_index_left_d1);
    all_fifos_empty <= all_fifos_empty_right and all_fifos_empty_left;
end find_earliest_non_empty_fifo_1k_arch;
