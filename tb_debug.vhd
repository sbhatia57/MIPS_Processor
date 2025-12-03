library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util_pkg.all;

entity tb_debug is
end entity;

architecture sim of tb_debug is
    -- Component under test

    -- Signals for simulation
    signal CLOCK_50 : std_logic := '0';
    signal KEY      : std_logic_vector(1 downto 0) := (others => '1');
    signal SW       : std_logic_vector(9 downto 0) := (others => '0');
    signal LEDR     : std_logic_vector(9 downto 0);
    signal HEX0, HEX1, HEX2, HEX3, HEX4, HEX5 : std_logic_vector(6 downto 0);
	 signal SIM_DUMP_sig : std_logic := '0';
	 
begin
    -- Instantiate your design
    uut: entity work.debug
        port map (
            CLOCK_50 => CLOCK_50,
            KEY      => KEY,
            SW       => SW,
            LEDR     => LEDR,
            HEX0     => HEX0,
            HEX1     => HEX1,
            HEX2     => HEX2,
            HEX3     => HEX3,
            HEX4     => HEX4,
            HEX5     => HEX5,
				SIM_DUMP => SIM_DUMP_sig
        );

    -- Clock generation: 50 MHz = 20 ns period
    CLOCK_PROCESS: process
    begin
        while true loop
            CLOCK_50 <= '0';
            wait for 10 ns;
            CLOCK_50 <= '1';
            wait for 10 ns;
        end loop;
    end process;

STIM_PROC: process
    constant NUM_STEPS : integer := 30;
    constant PULSE_HOLD : time := 20 ns;
    constant STEP_WAIT  : time := 240 ns;
begin
    report "Simulation starting...";
    SW <= (others => '0');
    KEY <= (others => '1');
    wait for 100 ns;

    for i in 0 to NUM_STEPS-1 loop
        KEY(0) <= '0';
        wait for PULSE_HOLD;
        KEY(0) <= '1';
        wait for STEP_WAIT;
    end loop;

    -- Request DUT to print final registers
    SIM_DUMP_sig <= '1';
    wait for 100 ns; -- give DUT time to print
    report "Simulation finished (TB)." severity note;
    wait;
end process;

end architecture;
