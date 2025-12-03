library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util_pkg is
    function nibble_to_char(n : std_logic_vector(3 downto 0)) return character;
    function to_hex_string32(vec : std_logic_vector(31 downto 0)) return string;
    function slv_to_string(vec : std_logic_vector) return string;
end package;

package body util_pkg is

    function nibble_to_char(n : std_logic_vector(3 downto 0)) return character is
        variable v : integer := to_integer(unsigned(n));
    begin
        case v is
            when 0  => return '0';
            when 1  => return '1';
            when 2  => return '2';
            when 3  => return '3';
            when 4  => return '4';
            when 5  => return '5';
            when 6  => return '6';
            when 7  => return '7';
            when 8  => return '8';
            when 9  => return '9';
            when 10 => return 'A';
            when 11 => return 'B';
            when 12 => return 'C';
            when 13 => return 'D';
            when 14 => return 'E';
            when 15 => return 'F';
            when others => return '0';
        end case;
    end function;

    function to_hex_string32(vec : std_logic_vector(31 downto 0)) return string is
        constant NIBBLES : integer := 8;
        variable out_str : string(1 to NIBBLES);
        variable nib     : std_logic_vector(3 downto 0);
    begin
        for j in 0 to NIBBLES-1 loop
            nib := vec(31 - j*4 downto 31 - j*4 - 3);
            out_str(j+1) := nibble_to_char(nib);
        end loop;
        return out_str;
    end function;

    function slv_to_string(vec : std_logic_vector) return string is
        variable out_str : string(1 to vec'length);
        variable pos     : integer := 1;
    begin
        for i in vec'range loop
            if vec(i) = '1' then
                out_str(pos) := '1';
            elsif vec(i) = '0' then
                out_str(pos) := '0';
            else
                out_str(pos) := 'X';
            end if;
            pos := pos + 1;
        end loop;
        return out_str;
    end function;

end package body;