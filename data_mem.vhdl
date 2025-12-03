library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity DataMemory is
    port(
        clk       : in  std_logic;
        addr      : in  std_logic_vector(7 downto 0); -- byte address
        writeEn   : in  std_logic;
        writeData : in  std_logic_vector(31 downto 0);
        storeType : in  std_logic_vector(1 downto 0); -- "00"=sb, "01"=sh, "10"=sw
        readData  : out std_logic_vector(31 downto 0)
    );
end entity;

architecture Behavioral of DataMemory is
    type ram_byte_t is array(0 to 255) of std_logic_vector(7 downto 0);
    signal RAM : ram_byte_t := (
        0 => x"11", 1 => x"22", 2 => x"33", 3 => x"44",
        4 => x"55", 5 => x"66", 6 => x"77", 7 => x"88",
        8 => x"AA", 9 => x"BB", 10 => x"CC", 11 => x"DD",
        12 => x"FF", 13 => x"EE", 14 => x"DD", 15 => x"CC",
        16 => x"12", 17 => x"34", 18 => x"56", 19 => x"78",
        others => (others => '0')
    );
begin
    process(clk)
        variable byte_addr    : integer;
        variable word         : std_logic_vector(31 downto 0);
        variable aligned_addr : integer;
    begin
        if rising_edge(clk) then
            byte_addr := to_integer(unsigned(addr));

            -- ====== WRITE ======
            if writeEn = '1' then
                case storeType is
                    -- Word store (big-endian)
                    when "10" =>
                        RAM(byte_addr)     <= writeData(31 downto 24);
                        RAM(byte_addr + 1) <= writeData(23 downto 16);
                        RAM(byte_addr + 2) <= writeData(15 downto 8);
                        RAM(byte_addr + 3) <= writeData(7 downto 0);
                        report "WRITE (sw) addr=" & integer'image(byte_addr) &
                               " data=" & integer'image(to_integer(unsigned(writeData)));

                    -- Halfword store (big-endian)
                    when "01" =>
                        RAM(byte_addr)     <= writeData(15 downto 8);
                        RAM(byte_addr + 1) <= writeData(7 downto 0);
                        report "WRITE (sh) addr=" & integer'image(byte_addr) &
                               " data=" & integer'image(to_integer(unsigned(writeData(15 downto 0))));

                    -- Byte store
                    when "00" =>
                        RAM(byte_addr) <= writeData(7 downto 0);
                        report "WRITE (sb) addr=" & integer'image(byte_addr) &
                               " data=" & integer'image(to_integer(unsigned(writeData(7 downto 0))));

                    when others =>
                        null;
                end case;
            end if;

            -- ====== READ ======
            -- Align to 4-byte boundary for word read
            aligned_addr := (byte_addr / 4) * 4;

            -- Big-endian word read
            word := RAM(aligned_addr) & RAM(aligned_addr + 1) & RAM(aligned_addr + 2) & RAM(aligned_addr + 3);
            readData <= word;

            report "READ addr=" & integer'image(byte_addr) &
                   " aligned=" & integer'image(aligned_addr) &
                   " data=" & integer'image(to_integer(unsigned(word)));
        end if;
    end process;
end architecture;
