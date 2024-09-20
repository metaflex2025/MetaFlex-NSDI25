library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity simple_dual_one_clock is
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
end simple_dual_one_clock;
architecture syn of simple_dual_one_clock is
    type ram_type is array (2**ADDR_WIDTH-1 downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);
    shared variable RAM : ram_type;
begin
    process(clk)
    begin
        if clk'event and clk = '1' then
	    if ena = '1' then
		if wea = '1' then
		    RAM(to_integer(unsigned(addra))) := dia;
		end if;
	    end if;
	end if;
    end process;

    process(clk)
    begin
        if clk'event and clk = '1' then
            if enb = '1' then
    	        dob <= RAM(to_integer(unsigned(addrb)));
            end if;
        end if;
    end process;
end syn;
