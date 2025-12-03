library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Store is
    port(
        clk       : in  std_logic;
        rst       : in  std_logic;
        addr      : in  integer;
        writeEn   : in  std_logic;
        writeData : in  integer;
        readData  : out integer
    );
end entity Store;

architecture rtl of Store is
    type ram_t is array(0 to 255) of integer;
    signal RAM : ram_t := (others => 0);
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                RAM <= (others => 0);
            else
                if writeEn = '1' then
                    RAM(addr) <= writeData;
                end if;
            end if;
            readData <= RAM(addr);
        end if;
    end process;
end architecture rtl;
