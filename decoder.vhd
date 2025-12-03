library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
library work;

entity decode is
    Port ( 
        I_clk       : in  STD_LOGIC;
        I_dataInst  : in  STD_LOGIC_VECTOR (31 downto 0);
        I_en        : in  STD_LOGIC;
        O_selA      : out STD_LOGIC_VECTOR (4 downto 0);
        O_selB      : out STD_LOGIC_VECTOR (4 downto 0);
        O_selD      : out STD_LOGIC_VECTOR (4 downto 0);
        O_shamt     : out STD_LOGIC_VECTOR (4 downto 0);
        O_functCode : out STD_LOGIC_VECTOR (5 downto 0);
        O_aluop     : out STD_LOGIC_VECTOR (5 downto 0);
        RegWrite    : out std_logic;
        MemWrite    : out std_logic;
        MemRead     : out std_logic;
        ALUSrc      : out std_logic;
        storeType   : out std_logic_vector(1 downto 0);
        Branch      : out std_logic;
        Jump        : out std_logic;
        JumpReg     : out std_logic;
        BranchType  : out std_logic_vector(2 downto 0);
        LinkReg     : out std_logic
    );
end decode;

architecture Behavioral of decode is
begin
    -- ==========================================
    -- COMBINATIONAL DECODER
    -- Decodes instruction and generates control signals
    -- ==========================================
    process (I_dataInst, I_en)
        variable opcode : std_logic_vector(5 downto 0);
        variable funct  : std_logic_vector(5 downto 0);
    begin
        -- Extract opcode and function code
        opcode := I_dataInst(31 downto 26);
        funct  := I_dataInst(5 downto 0);
        
        if I_en = '1' then
            -- ==========================================
            -- Default Values (will be overridden as needed)
            -- ==========================================
            O_selA    <= I_dataInst(25 downto 21);  -- rs
            O_selB    <= I_dataInst(20 downto 16);  -- rt
            O_selD    <= I_dataInst(20 downto 16);  -- rt (default for I-type)
            O_shamt   <= (others => '0');
            O_functCode <= (others => '0');
            O_aluop   <= opcode;
            
            -- Default control signals (safe defaults)
            RegWrite  <= '0';
            MemWrite  <= '0';
            MemRead   <= '0';
            ALUSrc    <= '0';
            storeType <= "10";
            Branch    <= '0';
            Jump      <= '0';
            JumpReg   <= '0';
            LinkReg   <= '0';
            BranchType <= "000";
            
            -- ==========================================
            -- NOP Detection (0x00000000)
            -- ==========================================
            if I_dataInst = x"00000000" then
                -- NOP: Do nothing, all defaults are safe
                RegWrite <= '0';
                
            -- ==========================================
            -- R-Type Instructions (opcode = 000000)
            -- ==========================================
            elsif opcode = "000000" then
                O_selD      <= I_dataInst(15 downto 11);  -- rd
                O_shamt     <= I_dataInst(10 downto 6);   -- shift amount
                O_functCode <= funct;
                
                if funct = "001000" then
                    -- JR: Jump Register
                    JumpReg  <= '1';
                    RegWrite <= '0';
                    
                elsif funct = "001001" then
                    -- JALR: Jump and Link Register
                    JumpReg  <= '1';
                    RegWrite <= '1';
                    LinkReg  <= '1';
                    
                else
                    -- Normal R-type ALU operations
                    -- (ADD, SUB, AND, OR, XOR, NOR, SLT, SLTU, SLL, SRL, SRA, etc.)
                    RegWrite <= '1';
                    ALUSrc   <= '0';
                end if;
                
            -- ==========================================
            -- Jump Instructions
            -- ==========================================
            elsif opcode = "000010" then
                -- J: Jump
                Jump   <= '1';
                O_selA <= (others => '0');
                O_selB <= (others => '0');
                O_selD <= (others => '0');
                
            elsif opcode = "000011" then
                -- JAL: Jump and Link
                Jump     <= '1';
                RegWrite <= '1';
                LinkReg  <= '1';
                O_selD   <= "00111";  -- $31 (return address register)
                O_selA   <= (others => '0');
                O_selB   <= (others => '0');
                
            -- ==========================================
            -- Branch Instructions
            -- ==========================================
            elsif opcode = "000100" then
                -- BEQ: Branch on Equal
                Branch     <= '1';
                BranchType <= "000";
                
            elsif opcode = "000101" then
                -- BNE: Branch on Not Equal
                Branch     <= '1';
                BranchType <= "001";
                
            elsif opcode = "000110" then
                -- BLEZ: Branch on Less Than or Equal to Zero
                Branch     <= '1';
                BranchType <= "010";
                
            elsif opcode = "000111" then
                -- BGTZ: Branch on Greater Than Zero
                Branch     <= '1';
                BranchType <= "011";
                
            -- ==========================================
            -- I-Type Arithmetic: ADDI, ADDIU, SLTI, SLTIU
            -- ==========================================
            elsif opcode = "001000" then
                -- ADDI: Add Immediate
                RegWrite <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "001001" then
                -- ADDIU: Add Immediate Unsigned
                RegWrite <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "001010" then
                -- SLTI: Set Less Than Immediate
                RegWrite <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "001011" then
                -- SLTIU: Set Less Than Immediate Unsigned
                RegWrite <= '1';
                ALUSrc   <= '1';
                
            -- ==========================================
            -- I-Type Logical: ANDI, ORI, XORI
            -- ==========================================
            elsif opcode = "001100" then
                -- ANDI: AND Immediate
                RegWrite <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "001101" then
                -- ORI: OR Immediate
                RegWrite <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "001110" then
                -- XORI: XOR Immediate
                RegWrite <= '1';
                ALUSrc   <= '1';
                
            -- ==========================================
            -- LUI: Load Upper Immediate
            -- ==========================================
            elsif opcode = "001111" then
                -- LUI
                RegWrite <= '1';
                ALUSrc   <= '1';
                
            -- ==========================================
            -- Load Instructions
            -- ==========================================
            elsif opcode = "100011" then
                -- LW: Load Word
                RegWrite <= '1';
                MemRead  <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "100000" then
                -- LB: Load Byte (signed)
                RegWrite <= '1';
                MemRead  <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "100001" then
                -- LH: Load Halfword (signed)
                RegWrite <= '1';
                MemRead  <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "100100" then
                -- LBU: Load Byte Unsigned
                RegWrite <= '1';
                MemRead  <= '1';
                ALUSrc   <= '1';
                
            elsif opcode = "100101" then
                -- LHU: Load Halfword Unsigned
                RegWrite <= '1';
                MemRead  <= '1';
                ALUSrc   <= '1';
                
            -- ==========================================
            -- Store Instructions
            -- ==========================================
            elsif opcode = "101011" then
                -- SW: Store Word
                MemWrite  <= '1';
                ALUSrc    <= '1';
                storeType <= "10";
                
            elsif opcode = "101001" then
                -- SH: Store Halfword
                MemWrite  <= '1';
                ALUSrc    <= '1';
                storeType <= "01";
                
            elsif opcode = "101000" then
                -- SB: Store Byte
                MemWrite  <= '1';
                ALUSrc    <= '1';
                storeType <= "00";
                
            end if;
            
        else
            -- ==========================================
            -- Decoder Disabled (I_en = '0')
            -- ==========================================
            O_selA      <= (others => '0');
            O_selB      <= (others => '0');
            O_selD      <= (others => '0');
            O_shamt     <= (others => '0');
            O_functCode <= (others => '0');
            O_aluop     <= (others => '0');
            
            RegWrite    <= '0';
            MemWrite    <= '0';
            MemRead     <= '0';
            ALUSrc      <= '0';
            storeType   <= "10";
            Branch      <= '0';
            Jump        <= '0';
            JumpReg     <= '0';
            LinkReg     <= '0';
            BranchType  <= "000";
        end if;
        
    end process;

end Behavioral;