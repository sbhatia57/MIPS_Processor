library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ALU32 is
    port(
        val1_alu   : in  std_logic_vector(31 downto 0);
        val2_alu   : in  std_logic_vector(31 downto 0);
        I_aluop    : in  std_logic_vector(5 downto 0); --- actually function code
		  I_shamt 	 : in  std_logic_vector(4 downto 0);
		  I_aluopcode : in std_logic_vector(5 downto 0);
        result_alu : out std_logic_vector(31 downto 0);
		  I_PC_plus_1 : in std_logic_vector(31 downto 0);
		  I_LinkReg   : in std_logic
    );
end entity ALU32;

architecture rtl of ALU32 is
    signal carry      : std_logic_vector(31 downto 0);
    signal tmp_result : std_logic_vector(31 downto 0);
    signal cin        : std_logic_vector(31 downto 0);
	 signal shamt 		 : std_logic_vector(4 downto 0);
	 signal val1 		 : std_logic_vector(31 downto 0);
	 signal val2 		 : std_logic_vector(31 downto 0);
begin

	 ---- NOTE!!!!: I_aluop is the function code and I_aluopcode is the opcode!

    -- Compute carry-in for each bit
	 cin(0) <= '1' when (I_aluop = "100010" and I_aluopcode = "000000") or (I_aluop = "100011" and I_aluopcode = "000000") else '0';  -- first bit carry-in
	 
		-- Connect register values to ALU
	 val1 <= val1_alu;
	 val2 <= val2_alu;

    gen_cin: for i in 1 to 31 generate
        cin(i) <= carry(i-1);
    end generate;

    -- Instantiate 32 ALU1Bit slices
    gen_alu: for i in 0 to 31 generate
        alu_bit: entity work.ALU1Bit
            port map(
                A      => val1(i),
                B      => val2(i),
                Cin    => cin(i),
                Op     => I_aluop,
                Result => tmp_result(i),
                Cout   => carry(i),
					 Opcode => I_aluopcode
            );
    end generate;

    process(val1_alu, val2_alu, I_aluop, tmp_result, I_LinkReg)
    begin
	     if I_LinkReg = '1' then
            result_alu <= I_PC_plus_1; 
		  -- Signed comparison (SLT)
		  elsif (I_aluop = "101010" and I_aluopcode = "000000") then
		     if signed(val1_alu) < signed(val2_alu) then
			     result_alu <= (0 => '1', others => '0');
            else
              result_alu <= (others => '0');
            end if;
		  -- Unsigned comparison (SLTU)
		  elsif (I_aluop = "101001" and I_aluopcode = "000000") then
				if unsigned(val1_alu) < unsigned(val2_alu) then
              result_alu <= (0 => '1', others => '0');
            else
              result_alu <= (others => '0');
            end if;
		  
		 elsif (I_aluopcode = "001010") then -- SLTI
			  if signed(val1_alu) < signed(val2_alu) then
					result_alu <= (0 => '1', others => '0');
			  else
					result_alu <= (others => '0');
			  end if;
		elsif I_aluopcode = "001100" then -- andi
			 result_alu <= val1_alu and val2;

		elsif I_aluopcode = "001101" then -- ori
			 result_alu <= val1_alu or val2;

		elsif I_aluopcode = "001110" then -- xori
			 result_alu <= val1_alu xor val2;

		 elsif (I_aluopcode = "001011") then -- SLTIU
			  if unsigned(val1_alu) < unsigned(val2_alu) then
					result_alu <= (0 => '1', others => '0');
			  else
					result_alu <= (others => '0');
			  end if;
		 elsif (I_aluopcode = "001111") then -- LUI
			  result_alu <= val2_alu; -- val2_alu is already shifted <<16 in imm32 logic

		  --- shift left logical
		  elsif (I_aluop = "000000" and I_aluopcode = "000000") then
				result_alu <= std_logic_vector(shift_left(unsigned(val2_alu), to_integer(unsigned(I_shamt))));
		  
		  --- shift right logical
		  elsif (I_aluop = "000010" and I_aluopcode = "000000") then
				result_alu <= std_logic_vector(shift_right(unsigned(val2_alu), to_integer(unsigned(I_shamt))));
		  
		  --- shift right arithmetic
		  elsif (I_aluop = "000011" and I_aluopcode = "000000") then
				result_alu <= std_logic_vector(shift_right(signed(val2_alu), to_integer(unsigned(I_shamt))));
		  
		  --- shift logical left value
		  elsif (I_aluop = "000100" and I_aluopcode = "000000") then
				result_alu <= std_logic_vector(shift_left(unsigned(val1_alu), to_integer(unsigned(val2_alu))));
		  
		  --- shift logical right value
		  elsif (I_aluop = "000110" and I_aluopcode = "000000") then
				result_alu <= std_logic_vector(shift_right(unsigned(val1_alu), to_integer(unsigned(val2_alu))));
		
		  --- shift right arithmetic value
		  elsif (I_aluop = "000111" and I_aluopcode = "000000") then
				result_alu <= std_logic_vector(shift_right(signed(val1_alu), to_integer(unsigned(val2_alu))));
		  
		  else 
				result_alu <= tmp_result;
				
   end if;
	
	report "ALU32: val1=" & integer'image(to_integer(unsigned(val1_alu))) &
       " val2=" & integer'image(to_integer(unsigned(val2_alu))) &
       " Result=" & integer'image(to_integer(unsigned(tmp_result)));

	
   end process;
	
end architecture rtl;
