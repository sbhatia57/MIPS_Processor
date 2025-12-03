library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util_pkg.all;

entity debug is
    port (
        CLOCK_50 : in  std_logic;
        KEY      : in  std_logic_vector(1 downto 0);
        SW       : in  std_logic_vector(9 downto 0);
        LEDR     : out std_logic_vector(9 downto 0);
        HEX0     : out std_logic_vector(6 downto 0);
        HEX1     : out std_logic_vector(6 downto 0);
        HEX2     : out std_logic_vector(6 downto 0);
        HEX3     : out std_logic_vector(6 downto 0);
        HEX4     : out std_logic_vector(6 downto 0);
        HEX5     : out std_logic_vector(6 downto 0);
		  SIM_DUMP : in std_logic

    );
end entity debug;

architecture rtl of debug is

    -- =========================
    -- Register File
    -- =========================
    type register_type is array(0 to 31) of std_logic_vector(31 downto 0);
    signal Registers : register_type := (
        0 => x"00000000", 
        1 => x"00000001", 
        2 => x"00000002", 
        3 => x"00000003", 
        4 => x"00000004",
        5 => x"00000084",
        6 => x"0000008C",
        7 => x"000000AA",
        others => x"00000000"
    );

    -- =========================
    -- ALU Signals
    -- =========================
    signal val1   : std_logic_vector(31 downto 0);
    signal val2   : std_logic_vector(31 downto 0);
    signal result : std_logic_vector(31 downto 0);
    signal aluOp  : std_logic_vector(5 downto 0);

    -- Register indices from decoder
    signal regI1  : std_logic_vector(4 downto 0);
    signal regI2  : std_logic_vector(4 downto 0);
    signal regD   : std_logic_vector(4 downto 0);

    -- Program Counter / instruction
    signal PC          : unsigned(31 downto 0) := (others => '0');
    signal instr_from_ram : std_logic_vector(31 downto 0);

    -- =========================
    -- Clock divider for slow stepping
    -- =========================
    signal slow_clk     : std_logic := '0';
    signal clk_counter  : unsigned(27 downto 0) := (others => '0');  
    constant DIVISOR    : unsigned(27 downto 0) := to_unsigned(50_000_000 / 2, 28);

    signal step_clk     : std_logic;  -- final clock used for PC increment
	 
	 signal displayReg : std_logic_vector(31 downto 0);
	 
	 signal shamt  		: std_logic_vector(4 downto 0);
	 signal opcode 		: std_logic_vector(5 downto 0);
	 
	 signal memWrite : std_logic := '0'; -- Only 1 during sw instructions
	 signal memAddr : std_logic_vector(31 downto 0);
	 signal memReadData : std_logic_vector(31 downto 0);
	 signal RegWriteEn : std_logic;
	 signal MemRead : std_logic;
	 signal ALUSrc : std_logic;
	 signal imm32 : std_logic_vector(31 downto 0);
	 signal storeType     : std_logic_vector(1 downto 0) := (others => '0'); 
    
	 -- Stage encoding
	 signal Branch        : std_logic;
	 signal Jump          : std_logic;
	 signal JumpReg       : std_logic;
	 signal BranchType    : std_logic_vector(2 downto 0);
	 signal LinkReg       : std_logic;
	 signal branch_taken  : std_logic;
	 signal jump_target   : unsigned(31 downto 0);
	 signal branch_target : unsigned(31 downto 0);
	 
	 signal IF_ID_instr : std_logic_vector(31 downto 0) := (others => '0');
	signal IF_ID_PC    : unsigned(31 downto 0) := (others => '0');

	-- ID/EX Pipeline Register
	signal ID_EX_instr    : std_logic_vector(31 downto 0) := (others => '0');
	signal ID_EX_PC       : unsigned(31 downto 0) := (others => '0');
	signal ID_EX_val1     : std_logic_vector(31 downto 0) := (others => '0');
	signal ID_EX_val2     : std_logic_vector(31 downto 0) := (others => '0');
	signal ID_EX_regI1    : std_logic_vector(4 downto 0) := (others => '0');
	signal ID_EX_regI2    : std_logic_vector(4 downto 0) := (others => '0');
	signal ID_EX_regD     : std_logic_vector(4 downto 0) := (others => '0');
	signal ID_EX_imm32    : std_logic_vector(31 downto 0) := (others => '0');
	-- Control signals
	signal ID_EX_RegWrite : std_logic := '0';
	signal ID_EX_MemWrite : std_logic := '0';
	signal ID_EX_MemRead  : std_logic := '0';
	signal ID_EX_ALUSrc   : std_logic := '0';
	signal ID_EX_Branch   : std_logic := '0';
	signal ID_EX_Jump     : std_logic := '0';
	signal ID_EX_JumpReg  : std_logic := '0';
	signal ID_EX_LinkReg  : std_logic := '0';
	signal ID_EX_aluOp    : std_logic_vector(5 downto 0) := (others => '0');
	signal ID_EX_opcode   : std_logic_vector(5 downto 0) := (others => '0');
	signal ID_EX_shamt    : std_logic_vector(4 downto 0) := (others => '0');
	signal ID_EX_BranchType : std_logic_vector(2 downto 0) := (others => '0');
	signal ID_EX_storeType : std_logic_vector(1 downto 0) := (others => '0');
	signal IF_ID_pred_taken : std_logic := '0';
	signal ID_EX_pred_taken : std_logic := '0';
	
	-- EX/MEM Pipeline Register
	signal EX_MEM_result  : std_logic_vector(31 downto 0) := (others => '0');
	signal EX_MEM_val2    : std_logic_vector(31 downto 0) := (others => '0');
	signal EX_MEM_regD    : std_logic_vector(4 downto 0) := (others => '0');
	signal EX_MEM_PC      : unsigned(31 downto 0) := (others => '0');
	-- Control signals
	signal EX_MEM_RegWrite : std_logic := '0';
	signal EX_MEM_MemWrite : std_logic := '0';
	signal EX_MEM_MemRead  : std_logic := '0';
	signal EX_MEM_Branch   : std_logic := '0';
	signal EX_MEM_Jump     : std_logic := '0';
	signal EX_MEM_JumpReg  : std_logic := '0';
	signal EX_MEM_LinkReg  : std_logic := '0';
	signal EX_MEM_opcode   : std_logic_vector(5 downto 0) := (others => '0');
	signal EX_MEM_branch_target : unsigned(31 downto 0) := (others => '0');
	signal EX_MEM_jump_target   : unsigned(31 downto 0) := (others => '0');
	signal EX_MEM_branch_taken  : std_logic := '0';
	signal EX_MEM_pred_taken    : std_logic := '0';

	-- MEM/WB Pipeline Register
	signal MEM_WB_result    : std_logic_vector(31 downto 0) := (others => '0');
	signal MEM_WB_memData   : std_logic_vector(31 downto 0) := (others => '0');
	signal MEM_WB_regD      : std_logic_vector(4 downto 0) := (others => '0');
	-- Control signals
	signal MEM_WB_RegWrite  : std_logic := '0';
	signal MEM_WB_MemRead   : std_logic := '0';
	signal MEM_WB_opcode    : std_logic_vector(5 downto 0) := (others => '0');
	signal MEM_WB_memAddr   : std_logic_vector(31 downto 0) := (others => '0');
	-- In EX/MEM Pipeline Register section
   signal EX_MEM_storeType : std_logic_vector(1 downto 0) := (others => '0');

   -- =========================
   -- Forwarding signals
   -- =========================
   -- forward codes: "00" = use ID/EX value, "10" = forward from EX/MEM, "01" = forward from MEM/WB
   signal forwardA : std_logic_vector(1 downto 0) := "00";
   signal forwardB : std_logic_vector(1 downto 0) := "00";

   signal MEM_WB_forward_value : std_logic_vector(31 downto 0) := (others => '0');
   signal forwardedA : std_logic_vector(31 downto 0) := (others => '0');
   signal forwardedB : std_logic_vector(31 downto 0) := (others => '0');
	signal stall : std_logic := '0';
	signal PC_plus1_slv : std_logic_vector(31 downto 0);
	signal bp_pred_taken   : std_logic := '0';
   signal bp_pred_pc      : std_logic_vector(31 downto 0) := (others => '0');

    -- ID-stage presentation to predictor (combinational)
   signal bp_id_is_branch : std_logic := '0';
   signal bp_id_pc        : std_logic_vector(31 downto 0) := (others => '0');
   signal bp_id_target    : std_logic_vector(31 downto 0) := (others => '0');

    -- Resolve presentation (EX-stage decision presented combinationally so predictor samples on clk)
   signal bp_resolve_valid : std_logic := '0';
   signal bp_resolve_pc    : std_logic_vector(31 downto 0) := (others => '0');
   signal bp_resolve_taken : std_logic := '0';
	signal bp_reset_n 		: std_logic := '0';
	
	-- Paste in the architecture declarative region (near your other signals)
-- hex helpers (put near the other signal declarations)
-- Portable hex helpers (put in architecture declarative region)
-- Simple, portable hex helpers for 32-bit vectors
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



begin

	-- Present ID-stage branch info (fill BTB) based on IF/ID (combinational, so it is stable at clock edge)
BP_ID_DRIVE: process(IF_ID_instr, IF_ID_PC)
    variable opc : std_logic_vector(5 downto 0);
    variable offset_i : integer;
    variable target_u  : unsigned(31 downto 0);
begin
    opc := IF_ID_instr(31 downto 26);
    -- default: not a branch
    bp_id_is_branch <= '0';
    bp_id_pc <= std_logic_vector(IF_ID_PC);
    bp_id_target <= (others => '0');

    -- Recognize branch opcodes (same encodings you use in decoder)
    if opc = "000100" or opc = "000101" or opc = "000110" or opc = "000111" then
        -- Compute branch target as (IF_ID_PC + 1) + sign_extend(offset)
        offset_i := to_integer(resize(signed(IF_ID_instr(15 downto 0)), 32));
        target_u := IF_ID_PC + to_unsigned(offset_i + 1, 32);
        bp_id_is_branch <= '1';
        bp_id_pc <= std_logic_vector(IF_ID_PC);
        bp_id_target <= std_logic_vector(target_u);
    end if;
end process;

-- Present resolve signals (branch decision made in EX stage)
-- Use ID_EX_* and branch_taken from the combinational EX logic: predictor sees these signals at rising edge.
-- Present resolve signals (branch decision made in EX stage)
-- Combinational so predictor sees resolve on same cycle
BP_RESOLVE_DRIVE: process(EX_MEM_Branch, EX_MEM_PC, EX_MEM_branch_taken)
begin
    if EX_MEM_Branch = '1' then
        bp_resolve_valid <= '1';
        bp_resolve_pc    <= std_logic_vector(EX_MEM_PC);
        bp_resolve_taken <= EX_MEM_branch_taken;
    else
        bp_resolve_valid <= '0';
        bp_resolve_pc    <= (others => '0');
        bp_resolve_taken <= '0';
    end if;
end process;
	
	PC_plus1_slv <= std_logic_vector(ID_EX_PC + 1);
-- ==========================================
-- Branch/Jump Logic (uses ID/EX pipeline registers)
-- Calculates in EX stage, decides in EX stage
-- ==========================================
-- Replace your existing Branch/Jump process with this block
process(ID_EX_Branch, ID_EX_Jump, ID_EX_JumpReg, ID_EX_BranchType, 
        ID_EX_instr, ID_EX_PC, forwardedA, forwardedB, ID_EX_regI1, ID_EX_regI2)
    variable offset    : signed(15 downto 0);
    variable offset32  : signed(31 downto 0);
    variable jump_addr : std_logic_vector(25 downto 0);
    variable jump_addr32 : unsigned(31 downto 0);
    variable PC_plus_1_local : unsigned(31 downto 0);
    variable branch_res_int : integer;
begin
    -- PC + 1 relative to the instruction in EX stage
    PC_plus_1_local := ID_EX_PC + 1;
    
    branch_taken <= '0';
    
    -- Branch Target Calculation
    if ID_EX_Branch = '1' then
        offset := signed(ID_EX_instr(15 downto 0));
        offset32 := resize(offset, 32);
        branch_res_int := to_integer(PC_plus_1_local) + to_integer(offset32);
        branch_target <= to_unsigned(branch_res_int, 32);
        
        -- Compare using forwarded operands so branches see recent ALU results
        case ID_EX_BranchType is
            when "000" =>  -- BEQ: rs == rt
                if forwardedA = forwardedB then
                    branch_taken <= '1';
                end if;
            
            when "001" =>  -- BNE: rs != rt
                if forwardedA /= forwardedB then
                    branch_taken <= '1';
                end if;
            
            when "010" =>  -- BLEZ: rs <= 0
                if signed(forwardedA) <= 0 then
                    branch_taken <= '1';
                end if;
            
            when "011" =>  -- BGTZ: rs > 0
                if signed(forwardedA) > 0 then
                    branch_taken <= '1';
                end if;
            
            when others =>
                branch_taken <= '0';
        end case;
    else
        branch_target <= (others => '0');
    end if;
    
    -- Jump Target Calculation (unchanged)
    if ID_EX_Jump = '1' then
        jump_addr := ID_EX_instr(25 downto 0);
        jump_addr32 := (others => '0');
        jump_addr32(25 downto 0) := unsigned(jump_addr);
        jump_target <= jump_addr32;
        
    elsif ID_EX_JumpReg = '1' then
        jump_target <= unsigned(forwardedA);  -- jump reg uses ID_EX_val1 (value forwarded into ALU if needed)
    else
        jump_target <= (others => '0');
    end if;
end process;

	 process(instr_from_ram)
    variable imm16 : std_logic_vector(15 downto 0);
begin
    imm16 := instr_from_ram(15 downto 0);

    case instr_from_ram(31 downto 26) is
        -- -------------------------
        -- Logical immediate: ANDI, ORI, XORI
        -- -------------------------
        when "001100" | "001101" | "001110" =>  
            imm32 <= (others => '0');  -- zero-extend
            imm32(15 downto 0) <= imm16;

        -- -------------------------
        -- LUI
        -- -------------------------
        when "001111" =>  
            imm32 <= imm16 & x"0000";  -- shift left 16 bits

        -- -------------------------
        -- Signed immediate arithmetic: ADDI, SLTI
        -- -------------------------
        when "001000" | "001010" =>  
            imm32 <= std_logic_vector(resize(signed(imm16), 32));

        -- -------------------------
        -- Unsigned immediate arithmetic: ADDIU, SLTIU
        -- -------------------------
        when "001001" | "001011" =>  
            imm32 <= std_logic_vector(resize(unsigned(imm16), 32));

        -- -------------------------
        -- Load/Store offsets (LW, LB, LBU, LH, LHU, SW, SH, SB)
        -- -------------------------
        when "100011" | "100000" | "100001" | "100100" | "100101" |
             "101011" | "101001" | "101000" =>  
            imm32 <= std_logic_vector(resize(signed(imm16), 32));

        -- -------------------------
        -- Default/fallback
        -- -------------------------
        when others =>
            imm32 <= (others => '0');
    end case;
end process;




    -- =========================
    -- ALU Input Selection (uses ID/EX pipeline registers) with forwarding
    -- The ALU operates on values selected here after forwarding decisions
    -- ==========================
process(ID_EX_val1, ID_EX_val2, ID_EX_ALUSrc, ID_EX_imm32,
        EX_MEM_RegWrite, EX_MEM_regD, EX_MEM_result, EX_MEM_MemRead,
        MEM_WB_RegWrite, MEM_WB_regD, MEM_WB_result, MEM_WB_memData,
        ID_EX_regI1, ID_EX_regI2)
begin
    -- Prepare MEM/WB forward value (load vs ALU result)
    if MEM_WB_MemRead = '1' then
        MEM_WB_forward_value <= MEM_WB_memData;
    else
        MEM_WB_forward_value <= MEM_WB_result;
    end if;

    -- Default forwarding (no forward)
    forwardA <= "00";
    forwardB <= "00";

    -- Forward for source A (ID/EX.rs)
    if EX_MEM_RegWrite = '1' and EX_MEM_regD /= "00000" and EX_MEM_regD = ID_EX_regI1 and EX_MEM_MemRead = '0' then
        forwardA <= "10";  -- from EX/MEM
    elsif MEM_WB_RegWrite = '1' and MEM_WB_regD /= "00000" and MEM_WB_regD = ID_EX_regI1 then
        forwardA <= "01";  -- from MEM/WB
    else
        forwardA <= "00";  -- use ID/EX value
    end if;

    -- Forward for source B (ID/EX.rt)
    if EX_MEM_RegWrite = '1' and EX_MEM_regD /= "00000" and EX_MEM_regD = ID_EX_regI2 and EX_MEM_MemRead = '0' then
        forwardB <= "10";
    elsif MEM_WB_RegWrite = '1' and MEM_WB_regD /= "00000" and MEM_WB_regD = ID_EX_regI2 then
        forwardB <= "01";
    else
        forwardB <= "00";
    end if;

    -- Select forwarded values for ALU inputs
    -- forwardedA
    case forwardA is
        when "10" => forwardedA <= EX_MEM_result;
        when "01" => forwardedA <= MEM_WB_forward_value;
        when others => forwardedA <= ID_EX_val1;
    end case;

    -- forwardedB
    case forwardB is
        when "10" => forwardedB <= EX_MEM_result;
        when "01" => forwardedB <= MEM_WB_forward_value;
        when others => forwardedB <= ID_EX_val2;
    end case;

    -- Assign final ALU inputs
    val1 <= forwardedA;

    if ID_EX_ALUSrc = '0' then
        val2 <= forwardedB;      -- use forwarded register value
    else
        val2 <= ID_EX_imm32;     -- immediate (no forwarding)
    end if;
end process;

    -- =========================
    
    -- =========================
    -- ALU
    -- =========================
    My_ALU: entity work.ALU32
        port map(
            val1_alu   => val1,
            val2_alu   => val2,
            I_aluop    => ID_EX_aluOp, -- function code
				I_aluopcode => ID_EX_opcode, -- opcode
				I_shamt 	  => ID_EX_shamt,
            result_alu => result,
				I_PC_plus_1 => PC_plus1_slv,
				I_LinkReg => ID_EX_LinkReg
        );
		  
	 
	  bp_inst: entity work.bp2
        generic map ( INDEX_BITS => 6 )  -- 64-entry predictor; tune if desired
        port map (
            clk => step_clk,
            reset_n => bp_reset_n,                 -- tie high (or wire your reset if available)
            if_pc => std_logic_vector(PC),
            pred_taken => bp_pred_taken,
            pred_pc => bp_pred_pc,
            id_is_branch => bp_id_is_branch,
            id_pc => bp_id_pc,
            id_target => bp_id_target,
            resolve_valid => bp_resolve_valid,
            resolve_pc => bp_resolve_pc,
            resolve_taken => bp_resolve_taken
        );
		  
	 -- instantiate data memory
	 U_DataRAM: entity work.DataMemory
			port map(
				clk => step_clk,
				addr => EX_MEM_result(7 downto 0),
				writeEn => EX_MEM_MemWrite,
				storeType => EX_MEM_storeType,
				writeData => EX_MEM_val2, -- from forwarded value now
				readData => memReadData
			);

    -- =========================
    -- Write back ALU result to register file
    -- =========================
-- Paste in place of your current writeback process
process(step_clk)
    variable byte_offset  : integer;
    variable byte_val     : std_logic_vector(7 downto 0);
    variable halfword_val : std_logic_vector(15 downto 0);
    variable wb_value     : std_logic_vector(31 downto 0);
begin
    if rising_edge(step_clk) then
        if MEM_WB_RegWrite = '1' and MEM_WB_regD /= "00000" then
            -- default wb_value
            wb_value := MEM_WB_result;
            -- compute byte offset within 32-bit word
            byte_offset := to_integer(unsigned(MEM_WB_memAddr(1 downto 0)));
            if MEM_WB_MemRead = '1' then
                -- Load cases: build wb_value from MEM_WB_memData
                case MEM_WB_opcode is
                    when "100011" =>  -- LW
                        wb_value := MEM_WB_memData;
                    when "100000" =>  -- LB (signed)
                        case byte_offset is
                            when 0 => byte_val := MEM_WB_memData(31 downto 24);
                            when 1 => byte_val := MEM_WB_memData(23 downto 16);
                            when 2 => byte_val := MEM_WB_memData(15 downto 8);
                            when 3 => byte_val := MEM_WB_memData(7 downto 0);
                            when others => byte_val := (others => '0');
                        end case;
                        wb_value := (others => byte_val(7));
                        wb_value(7 downto 0) := byte_val;
                    when "100100" =>  -- LBU
                        case byte_offset is
                            when 0 => byte_val := MEM_WB_memData(31 downto 24);
                            when 1 => byte_val := MEM_WB_memData(23 downto 16);
                            when 2 => byte_val := MEM_WB_memData(15 downto 8);
                            when 3 => byte_val := MEM_WB_memData(7 downto 0);
                            when others => byte_val := (others => '0');
                        end case;
                        wb_value := (others => '0');
                        wb_value(7 downto 0) := byte_val;
                    when "100001" =>  -- LH (signed)
                        if byte_offset = 0 or byte_offset = 1 then
                            halfword_val := MEM_WB_memData(31 downto 16);
                        else
                            halfword_val := MEM_WB_memData(15 downto 0);
                        end if;
                        wb_value := (others => halfword_val(15));
                        wb_value(15 downto 0) := halfword_val;
                    when "100101" =>  -- LHU
                        if byte_offset = 0 or byte_offset = 1 then
                            halfword_val := MEM_WB_memData(31 downto 16);
                        else
                            halfword_val := MEM_WB_memData(15 downto 0);
                        end if;
                        wb_value := (others => '0');
                        wb_value(15 downto 0) := halfword_val;
                    when others =>
                        wb_value := MEM_WB_memData;
                end case;
            end if;

            -- Perform the write
            Registers(to_integer(unsigned(MEM_WB_regD))) <= wb_value;

            -- WB trace (hex)
            report "WB: writing $" & integer'image(to_integer(unsigned(MEM_WB_regD))) & " <= 0x" & to_hex_string32(wb_value) & " (opcode=" & integer'image(to_integer(unsigned(MEM_WB_opcode))) & ")" severity note;
        end if;
    end if;
end process;



	 displayReg <= Registers(to_integer(unsigned(SW(2 downto 0)))) when (SW(9) = '1') else result;

    -- =========================
    -- Instruction RAM
    -- =========================
    U_RAM: entity work.InstructionMemory
        port map(
            I_clk  => step_clk,
            I_addr => std_logic_vector(PC(15 downto 0)),
            O_data => instr_from_ram
        );

    -- =========================
    -- Decoder
    -- =========================
    uut_decoder: entity work.decode
        port map(
            I_clk      => step_clk,
            I_en       => '1',
            I_dataInst => instr_from_ram,
            O_selA     => regI1,
            O_selB     => regI2,
            O_selD     => regD,
				O_shamt	  => shamt,
				O_aluop    => opcode,
            O_functCode    => aluOp,
				RegWrite => RegWriteEn,
				MemWrite => memWrite,
				MemRead => MemRead,
				ALUSrc  => ALUSrc,
				Branch      => Branch,
			   Jump        => Jump,
			   JumpReg     => JumpReg,
			   BranchType  => BranchType,
			   LinkReg     => LinkReg
        );

    -- =========================
    -- Slow clock generation
    -- =========================
    process(CLOCK_50)
    begin
        if rising_edge(CLOCK_50) then
            if clk_counter = DIVISOR then
                clk_counter <= (others => '0');
                slow_clk <= not slow_clk;
            else
                clk_counter <= clk_counter + 1;
            end if;
        end if;
    end process;

    -- Select step clock: KEY(0) for manual stepping OR slow_clk
    step_clk <= not KEY(0) or slow_clk;
	 
-- ==========================================
-- Main Pipeline Process
-- Updates ALL stages every clock cycle
-- ==========================================
-- Replace your main pipeline process with this version that adds load-use stall and flush on control transfers
process(step_clk)
    variable opcode_v : std_logic_vector(5 downto 0);
    variable funct_v  : std_logic_vector(5 downto 0);
    variable will_stall : boolean;
begin
    if rising_edge(step_clk) then
	 
		  bp_reset_n <= '1';
        -- ------------------------
        -- Hazard detection (load->use)
        -- If ID/EX is a load and its destination matches IF_ID.rs or IF_ID.rt, insert a stall
        -- NOTE: uses the current IF_ID_instr and ID_EX_regD from previous cycle
        if ID_EX_MemRead = '1' and ID_EX_regD /= "00000" and (ID_EX_regD = IF_ID_instr(25 downto 21) or ID_EX_regD = IF_ID_instr(20 downto 16)) then
            stall <= '1';
            will_stall := true;
        else
            stall <= '0';
            will_stall := false;
        end if;

        -- ==================
        -- IF Stage: Fetch next instruction (only if not stalled)
        -- ==================
        if stall = '0' then
            IF_ID_instr <= instr_from_ram;
            IF_ID_PC <= PC;
				IF_ID_pred_taken <= bp_pred_taken;
        else
            -- hold IF/ID (do nothing, keep previous IF_ID values)
            IF_ID_instr <= IF_ID_instr;
            IF_ID_PC <= IF_ID_PC;
				IF_ID_pred_taken <= IF_ID_pred_taken;
        end if;

        -- ==================
        -- ID Stage: Decode and read registers
        -- If we are stalling, inject a bubble (NOP) into ID/EX; otherwise do normal decode
        -- ==================
        if stall = '1' then
            -- Insert a bubble into ID/EX (NOP): clear instruction and control signals so EX sees a NOP
            ID_EX_instr <= (others => '0');
            ID_EX_PC    <= (others => '0');
            ID_EX_val1  <= (others => '0');
            ID_EX_val2  <= (others => '0');
            ID_EX_regI1 <= (others => '0');
            ID_EX_regI2 <= (others => '0');
            ID_EX_aluOp <= (others => '0');
            ID_EX_opcode <= (others => '0');
            ID_EX_shamt <= (others => '0');
            ID_EX_imm32 <= (others => '0');
            ID_EX_regD <= (others => '0');
            ID_EX_storeType <= (others => '0');
				ID_EX_pred_taken <= '0';

            -- control signals -> all zero (NOP)
            ID_EX_RegWrite <= '0';
            ID_EX_MemWrite <= '0';
            ID_EX_MemRead  <= '0';
            ID_EX_ALUSrc   <= '0';
            ID_EX_Branch   <= '0';
            ID_EX_Jump     <= '0';
            ID_EX_JumpReg  <= '0';
            ID_EX_LinkReg  <= '0';
            ID_EX_BranchType <= (others => '0');

        else
            -- Normal decode: update ID/EX pipeline registers from IF/ID and register file
            ID_EX_instr <= IF_ID_instr;
            ID_EX_PC <= IF_ID_PC;
				ID_EX_pred_taken <= IF_ID_pred_taken;
            ID_EX_val1 <= Registers(to_integer(unsigned(IF_ID_instr(25 downto 21))));  -- rs
            ID_EX_val2 <= Registers(to_integer(unsigned(IF_ID_instr(20 downto 16))));  -- rt

            opcode_v := IF_ID_instr(31 downto 26);
            funct_v := IF_ID_instr(5 downto 0);

            ID_EX_aluOp <= IF_ID_instr(5 downto 0);
            ID_EX_opcode <= IF_ID_instr(31 downto 26);
            ID_EX_shamt <= IF_ID_instr(10 downto 6);
            ID_EX_imm32 <= std_logic_vector(resize(signed(IF_ID_instr(15 downto 0)), 32));
            ID_EX_regI1 <= IF_ID_instr(25 downto 21);
            ID_EX_regI2 <= IF_ID_instr(20 downto 16);
            ID_EX_storeType <= "00";

            -- Default control values
            ID_EX_RegWrite <= '0';
            ID_EX_MemWrite <= '0';
            ID_EX_MemRead <= '0';
            ID_EX_ALUSrc <= '0';
            ID_EX_Branch <= '0';
            ID_EX_Jump <= '0';
            ID_EX_JumpReg <= '0';
            ID_EX_LinkReg <= '0';
            ID_EX_BranchType <= "000";
            ID_EX_regD <= IF_ID_instr(20 downto 16);

            -- Decoder control generation (same logic you already had)
            case opcode_v is
                when "000000" =>
                    if funct_v = "001000" then  -- JR
                        ID_EX_JumpReg <= '1';
                    elsif funct_v = "001001" then  -- JALR
                        ID_EX_JumpReg <= '1';
                        ID_EX_LinkReg <= '1';
                        ID_EX_RegWrite <= '1';
                        ID_EX_regD <= IF_ID_instr(15 downto 11);
                    else
                        ID_EX_RegWrite <= '1';
                        ID_EX_regD <= IF_ID_instr(15 downto 11);
                    end if;

                when "000010" => ID_EX_Jump <= '1';
                when "000011" =>
                    ID_EX_Jump <= '1';
                    ID_EX_LinkReg <= '1';
                    ID_EX_RegWrite <= '1';
                    ID_EX_regD <= "00111";  -- $7
                when "000100" =>
                    ID_EX_Branch <= '1'; ID_EX_BranchType <= "000";
                when "000101" =>
                    ID_EX_Branch <= '1'; ID_EX_BranchType <= "001";
                when "000110" =>
                    ID_EX_Branch <= '1'; ID_EX_BranchType <= "010";
                when "000111" =>
                    ID_EX_Branch <= '1'; ID_EX_BranchType <= "011";
                when "001000" => ID_EX_RegWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "001001" => ID_EX_RegWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "001010" => ID_EX_RegWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "001011" => ID_EX_RegWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "001100" => ID_EX_RegWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "001101" => ID_EX_RegWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "001110" => ID_EX_RegWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "001111" => ID_EX_RegWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "100000" => ID_EX_RegWrite <= '1'; ID_EX_MemRead <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "100001" => ID_EX_RegWrite <= '1'; ID_EX_MemRead <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "100011" => ID_EX_RegWrite <= '1'; ID_EX_MemRead <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "100100" => ID_EX_RegWrite <= '1'; ID_EX_MemRead <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "100101" => ID_EX_RegWrite <= '1'; ID_EX_MemRead <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_regD <= IF_ID_instr(20 downto 16);
                when "101000" => ID_EX_MemWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_storeType <= "00";
                when "101001" => ID_EX_MemWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_storeType <= "01";
                when "101011" => ID_EX_MemWrite <= '1'; ID_EX_ALUSrc <= '1'; ID_EX_storeType <= "10";
                when others => null;
            end case;
        end if;  -- stall or normal ID

        -- ==================
        -- EX Stage: Execute ALU operation (unchanged)
        -- ==================
        EX_MEM_result <= result;
        EX_MEM_val2 <= forwardedB;  -- forward store data if necessary
        EX_MEM_regD <= ID_EX_regD;
        EX_MEM_PC <= ID_EX_PC;
        EX_MEM_RegWrite <= ID_EX_RegWrite;
        EX_MEM_MemWrite <= ID_EX_MemWrite;
        EX_MEM_MemRead <= ID_EX_MemRead;
        EX_MEM_Branch <= ID_EX_Branch;
        EX_MEM_Jump <= ID_EX_Jump;
        EX_MEM_JumpReg <= ID_EX_JumpReg;
        EX_MEM_LinkReg <= ID_EX_LinkReg;
        EX_MEM_opcode <= ID_EX_opcode;
        EX_MEM_storeType <= ID_EX_storeType;
        EX_MEM_branch_target <= branch_target;
        EX_MEM_jump_target <= jump_target;
        EX_MEM_branch_taken <= branch_taken;
		  EX_MEM_pred_taken   <= ID_EX_pred_taken;

        -- ==================
        -- MEM Stage: Memory access (unchanged)
        -- ==================
        MEM_WB_result <= EX_MEM_result;
        MEM_WB_memData <= memReadData;
        MEM_WB_regD <= EX_MEM_regD;
        MEM_WB_memAddr <= EX_MEM_result;
        MEM_WB_RegWrite <= EX_MEM_RegWrite;
        MEM_WB_MemRead <= EX_MEM_MemRead;
        MEM_WB_opcode <= EX_MEM_opcode;

        -- ==================
        -- PC Update and flush handling
        -- ==================
                -- ==================
        -- PC Update and flush handling (with branch prediction)
        -- Priority: jumps/branch-corrected targets (control transfer) -> otherwise predictor / sequential
        -- ==================
        if EX_MEM_Jump = '1' or EX_MEM_JumpReg = '1' then
            -- control transfer (jump): use target from EX/MEM
            PC <= EX_MEM_jump_target;
            -- flush pipeline behind the transfer
            IF_ID_instr <= (others => '0');
            IF_ID_PC    <= (others => '0');
				IF_ID_pred_taken <= '0';
            ID_EX_instr <= (others => '0');
            ID_EX_RegWrite <= '0';
            ID_EX_MemWrite <= '0';
            ID_EX_MemRead  <= '0';
            ID_EX_ALUSrc   <= '0';
            ID_EX_Branch   <= '0';
            ID_EX_Jump     <= '0';
            ID_EX_JumpReg  <= '0';
            ID_EX_LinkReg  <= '0';
				ID_EX_pred_taken <= '0';
        elsif EX_MEM_Branch = '1' then
				if EX_MEM_branch_taken /= EX_MEM_pred_taken then
                -- Misprediction!  Correct the PC
                if EX_MEM_branch_taken = '1' then
                    -- Should have taken branch
                    PC <= EX_MEM_branch_target;
                else
                    -- Should NOT have taken branch (continue sequential)
                    PC <= EX_MEM_PC + 1;
                end if;
            -- Flush wrong-path instructions
                IF_ID_instr <= (others => '0');
                IF_ID_PC    <= (others => '0');
                IF_ID_pred_taken <= '0';
                ID_EX_instr <= (others => '0');
                ID_EX_RegWrite <= '0';
                ID_EX_MemWrite <= '0';
                ID_EX_MemRead  <= '0';
                ID_EX_ALUSrc   <= '0';
                ID_EX_Branch   <= '0';
                ID_EX_Jump     <= '0';
                ID_EX_JumpReg  <= '0';
                ID_EX_LinkReg  <= '0';
                ID_EX_pred_taken <= '0';
					 
					 report "MISPREDICT: PC=" & integer'image(to_integer(EX_MEM_PC)) &
                       " predicted=" & std_logic'image(EX_MEM_pred_taken) &
                       " actual=" & std_logic'image(EX_MEM_branch_taken)
                       severity note;
			  else
					-- No control-transfer correction required: use predictor when not stalling
					if stall = '0' then
						 if bp_pred_taken = '1' then
							  PC <= unsigned(bp_pred_pc);
						 else
							  PC <= PC + 1;
						 end if;
					else
						 PC <= PC; -- hold while stalling
					end if;
				end if;
				
		  -- Priority 3: Normal operation (no branch/jump)
        else
            if stall = '0' then
                if bp_pred_taken = '1' then
                    PC <= unsigned(bp_pred_pc);
                else
                    PC <= PC + 1;
                end if;
            else
                PC <= PC;  -- Hold during stall
            end if;
        end if;
		  
		  -- Paste this at the end of your main pipeline process, inside rising_edge(step_clk),
-- right after the PC update / flush code (before the end if).
report "CYCLE=" & integer'image(to_integer(PC)) &
       " IF=0x" & to_hex_string32(IF_ID_instr) &
       " ID_EX=0x" & to_hex_string32(ID_EX_instr) &
       " EX_RES=0x" & to_hex_string32(EX_MEM_result) &
       " EX_regD=" & integer'image(to_integer(unsigned(EX_MEM_regD))) &
       " MEM_WB_res=0x" & to_hex_string32(MEM_WB_result) &
       " MEM_WB_regD=" & integer'image(to_integer(unsigned(MEM_WB_regD))) &
       " stall=" & std_logic'image(stall)
       severity note;

report "DST: ID_EX_regD=" & integer'image(to_integer(unsigned(ID_EX_regD))) &
       " EX_MEM_regD=" & integer'image(to_integer(unsigned(EX_MEM_regD))) &
       " MEM_WB_regD=" & integer'image(to_integer(unsigned(MEM_WB_regD))) &
       " ID_EX_RegW=" & std_logic'image(ID_EX_RegWrite) &
       " EX_RegW=" & std_logic'image(EX_MEM_RegWrite) &
       " MEM_RegW=" & std_logic'image(MEM_WB_RegWrite)
       severity note;
    end if; -- rising_edge
end process;


-- One-shot simulation dump: print final registers when TB asserts SIM_DUMP
SIM_DUMP_PROC: process(SIM_DUMP)
begin
    if SIM_DUMP'event and SIM_DUMP = '1' then
        -- Optionally include a tiny delay if your simulator supports it:
        -- wait for 1 ns;
        report "FINAL REGS: " &
               "$0=0x" & to_hex_string32(Registers(0)) & " $1=0x" & to_hex_string32(Registers(1)) &
               " $2=0x" & to_hex_string32(Registers(2)) & " $3=0x" & to_hex_string32(Registers(3)) &
               " $4=0x" & to_hex_string32(Registers(4)) & " $5=0x" & to_hex_string32(Registers(5)) &
               " $6=0x" & to_hex_string32(Registers(6)) & " $7=0x" & to_hex_string32(Registers(7))
               severity note;
    end if;
end process;

    -- =========================
    -- HEX display wiring
    -- =========================
    h0: entity work.hex port map(value => displayReg(3 downto 0),  o => HEX0);
    h1: entity work.hex port map(value => displayReg(7 downto 4),  o => HEX1);
    h2: entity work.hex port map(value => displayReg(11 downto 8), o => HEX2);
    h3: entity work.hex port map(value => displayReg(15 downto 12), o => HEX3);
    h4: entity work.hex port map(value => displayReg(19 downto 16), o => HEX4);
    h5: entity work.hex port map(value => displayReg(23 downto 20), o => HEX5);

end architecture rtl;