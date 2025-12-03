library ieee;
use ieee.std_logic_1164.all;

entity ALU1Bit is
    port(
        A       : in  std_logic;
        B       : in  std_logic;
        Cin     : in  std_logic;
        Op      : in  std_logic_vector(5 downto 0); -- ALU function code
		  Opcode  : in  std_logic_vector(5 downto 0); -- ALU Op code
        Result  : out std_logic;
        Cout    : out std_logic                      -- carry out (for add/sub)
    );
end entity ALU1Bit;

architecture rtl of ALU1Bit is
begin
    process(A, B, Cin, Op)
        variable tmp_result : std_logic;
        variable carry      : std_logic;
        variable B_inv      : std_logic;
        variable Cin_sub    : std_logic;
    begin
        -- Default values
        tmp_result := '0';
        carry := '0';
        B_inv := '0';
        Cin_sub := '0';
		  
		  -- NOTE!: Op is actually the function code
		  
		  -- ADD
			-- ADD / ADDI / ADDIU / LW / LB / LBU / LH / LHU
			if ( (Op = "100000" and Opcode = "000000")  -- R-type ADD
				  or Opcode = "001000"                    -- ADDI
				  or Opcode = "001001"                    -- ADDIU
				  or Opcode = "100011"                    -- LW
				  or Opcode = "100000"                    -- LB
				  or Opcode = "100001"                    -- LH
				  or Opcode = "100100"                    -- LBU
				  or Opcode = "100101") then              -- LHU
				 tmp_result := A xor B xor Cin;
				 carry := (A and B) or (B and Cin) or (A and Cin);

		  -- SUB: A - B
		  elsif (Op = "100010" and Opcode = "000000") then
				B_inv := not B;
            tmp_result := A xor B_inv xor Cin;
            carry := (A and B_inv) or (B_inv and Cin) or (A and Cin);
		  --- ADDu
		  elsif (Op = "100001" and Opcode = "000000") then
				tmp_result := A xor B xor Cin;
            carry := (A and B) or (B and Cin) or (A and Cin);
		  --- SUBu
		  elsif (Op = "100011" and Opcode = "000000") then
				B_inv := not B;
            tmp_result := A xor B_inv xor Cin;
            carry := (A and B_inv) or (B_inv and Cin) or (A and Cin);
					  -- ANDI
			elsif (Opcode = "001100") then
				 tmp_result := A and B;
				 carry := '0';

			-- ORI
			elsif (Opcode = "001101") then
				 tmp_result := A or B;
				 carry := '0';

			-- XORI
			elsif (Opcode = "001110") then
				 tmp_result := A xor B;
				 carry := '0';

		  -- AND
		  elsif (Op = "100100" and Opcode = "000000") then
				tmp_result := A and B;
            carry := '0';
		
		  -- OR
		  elsif (Op = "100101" and Opcode = "000000") then
				tmp_result := A or B;
            carry := '0';
		
		  -- XOR
		  elsif (Op = "100110" and Opcode = "000000") then
				tmp_result := A xor B;
            carry := '0';
		  
		  -- NOR
		  elsif (Op = "100111" and Opcode = "000000") then
			   tmp_result := not (A or B);
            carry := '0';
		
		  else 
				tmp_result := '0';
            carry := '0';
		
		  end if;

        Result <= tmp_result;
        Cout <= carry;
    end process;
end architecture rtl;
