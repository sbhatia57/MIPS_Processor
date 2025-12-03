library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_store is
end tb_store;

architecture sim of tb_store is

    -- Signals
    signal clk       : std_logic := '0';
    signal addr      : std_logic_vector(7 downto 0) := (others => '0');  
    signal writeEn   : std_logic := '0';
    signal writeData : std_logic_vector(31 downto 0) := (others => '0');
    signal storeType : std_logic_vector(1 downto 0) := (others => '0');  
    signal readData  : std_logic_vector(31 downto 0);

    -- DUT
    component DataMemory is
        port(
            clk       : in  std_logic;
            addr      : in  std_logic_vector(7 downto 0);
            writeEn   : in  std_logic;
            writeData : in  std_logic_vector(31 downto 0);
            storeType : in  std_logic_vector(1 downto 0);
            readData  : out std_logic_vector(31 downto 0)
        );
    end component;

    -- Extract byte (big-endian)
    function get_byte_be(w : std_logic_vector(31 downto 0); byte_index : integer) return std_logic_vector is
    begin
        case byte_index mod 4 is
            when 0 => return w(31 downto 24);
            when 1 => return w(23 downto 16);
            when 2 => return w(15 downto 8);
            when 3 => return w(7 downto 0);
            when others => return (7 downto 0 => '0');
        end case;
    end function;

    -- Extract halfword (big-endian)
    function get_halfword_be(w : std_logic_vector(31 downto 0); half_index : integer) return std_logic_vector is
    begin
        case half_index mod 2 is
            when 0 => return w(31 downto 16);
            when 1 => return w(15 downto 0);
            when others => return (15 downto 0 => '0');
        end case;
    end function;

begin

    -- Instantiate DUT
    UUT: DataMemory
        port map(
            clk       => clk,
            addr      => addr,
            writeEn   => writeEn,
            writeData => writeData,
            storeType => storeType,
            readData  => readData
        );

    -- Clock generation: 10 ns period
    clk_process : process
    begin
        while true loop
            clk <= '0'; wait for 5 ns;
            clk <= '1'; wait for 5 ns;
        end loop;
    end process;

    -- Test process
    test_process: process
        variable mem_val : integer;
    begin
        --------------------------------------------------------------------
        -- TEST 1: Store Word (sw)
        --------------------------------------------------------------------
        report "Starting TEST 1: Store Word (sw)";
        addr      <= x"00"; 
        writeData <= x"00000042";  -- 66 decimal
        storeType <= "10";
        writeEn   <= '1';
        wait until rising_edge(clk);
        writeEn   <= '0';
        wait until rising_edge(clk);  -- wait one cycle for write to complete
        wait for 1 ns;  -- allow combinational logic to settle

        -- Read full word
        mem_val := to_integer(unsigned(readData));
        if mem_val /= 66 then
            report "SW Test FAILED: Memory[0] = " & integer'image(mem_val) severity error;
        else
            report "SW Test PASSED: Memory[0] = " & integer'image(mem_val);
        end if;

        --------------------------------------------------------------------
        -- TEST 2: Store Halfword (sh)
        --------------------------------------------------------------------
        report "Starting TEST 2: Store Halfword (sh)";
        addr      <= x"04"; 
        writeData <= x"00001234";  -- 4660 decimal
        storeType <= "01";
        writeEn   <= '1';
        wait until rising_edge(clk);
        writeEn   <= '0';
        wait until rising_edge(clk);  -- wait one cycle for write to complete
        wait for 1 ns;  -- allow combinational logic to settle

        mem_val := to_integer(unsigned(readData(31 downto 16)));
        if mem_val /= 4660 then
            report "SH Test FAILED: Memory[4] = " & integer'image(mem_val) severity error;
        else
            report "SH Test PASSED: Memory[4] = " & integer'image(mem_val);
        end if;

        --------------------------------------------------------------------
        -- TEST 3: Store Byte (sb)
        --------------------------------------------------------------------
        report "Starting TEST 3: Store Byte (sb)";
        addr      <= x"08"; 
        writeData <= x"000000AB";  -- 171 decimal
        storeType <= "00";
        writeEn   <= '1';
        wait until rising_edge(clk);
        writeEn   <= '0';
        wait until rising_edge(clk);  -- wait one cycle for write to complete
        wait for 1 ns;  -- allow combinational logic to settle

        mem_val := to_integer(unsigned(readData(31 downto 24)));
        if mem_val /= 171 then
            report "SB Test FAILED: Memory[8] = " & integer'image(mem_val) severity error;
        else
            report "SB Test PASSED: Memory[8] = " & integer'image(mem_val);
        end if;

        report "========================================";
        report "STORE TEST SIMULATION FINISHED";
        report "========================================";
        wait;
    end process;

end sim;