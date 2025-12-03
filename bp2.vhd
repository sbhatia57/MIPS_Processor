library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bp2 is
    generic (
        INDEX_BITS : integer := 6
    );
    port (
        clk         : in  std_logic;
        reset_n     : in  std_logic;
        if_pc       : in  std_logic_vector(31 downto 0);
        pred_taken  : out std_logic;
        pred_pc     : out std_logic_vector(31 downto 0);
        id_is_branch : in std_logic;
        id_pc        : in std_logic_vector(31 downto 0);
        id_target    : in std_logic_vector(31 downto 0);
        resolve_valid : in std_logic;
        resolve_pc    : in std_logic_vector(31 downto 0);
        resolve_taken : in std_logic
    );
end entity bp2;

architecture rtl of bp2 is
    constant DEPTH : integer := 2 ** INDEX_BITS;
    subtype cnt_t is std_logic_vector(1 downto 0);

    type bht_t is array(0 to DEPTH-1) of cnt_t;
    signal bht : bht_t; -- no declarative initializer

    constant TAG_WIDTH : integer := 32 - (INDEX_BITS + 2);
    subtype tag_t is std_logic_vector(TAG_WIDTH-1 downto 0);
    type tag_arr_t is array(0 to DEPTH-1) of tag_t;
    type targ_arr_t is array(0 to DEPTH-1) of std_logic_vector(31 downto 0);
    type valid_arr_t is array(0 to DEPTH-1) of std_logic;

    signal btb_tag   : tag_arr_t;    -- no declarative initializer
    signal btb_target: targ_arr_t;
    signal btb_valid : valid_arr_t;

    -- helper functions unchanged...
    function idx_of(pc: std_logic_vector(31 downto 0)) return integer is
        variable slice : std_logic_vector(INDEX_BITS+1 downto 2);
    begin
        slice := pc(INDEX_BITS+1 downto 2);
        return to_integer(unsigned(slice));
    end function;

function tag_of(pc: std_logic_vector(31 downto 0)) return tag_t is
begin
    return pc(31 downto 31-TAG_WIDTH+1);
end function;

begin
    -- runtime sanity check (optional)
    assert TAG_WIDTH > 0
        report "bp2: INDEX_BITS too large; TAG_WIDTH must be > 0"
        severity failure;

    -- combinational pred_read unchanged...
    pred_read: process(if_pc, bht, btb_valid, btb_tag, btb_target)
        variable index : integer;
        variable ttag  : tag_t;
    begin
        index := idx_of(if_pc);
        ttag := tag_of(if_pc);

        if bht(index)(1) = '1' then
            pred_taken <= '1';
        else
            pred_taken <= '0';
        end if;

        if btb_valid(index) = '1' and btb_tag(index) = ttag then
            pred_pc <= btb_target(index);
        else
            pred_pc <= std_logic_vector(unsigned(if_pc) + 4);
        end if;
    end process;

    -- BTB fill: initialize BTB arrays on reset, then update on id_is_branch
    btb_fill: process(clk)
        variable index_id : integer;
        variable i : integer;
    begin
        if rising_edge(clk) then
            if reset_n = '0' then
                -- initialize BHT/BTB on reset (runtime init)
                for i in 0 to DEPTH-1 loop
                    btb_valid(i) <= '0';
                    btb_target(i) <= (others => '0');
                    btb_tag(i) <= (others => '0');
                end loop;
            else
                if id_is_branch = '1' then
                    index_id := idx_of(id_pc);
                    btb_target(index_id) <= id_target;
                    btb_tag(index_id) <= tag_of(id_pc);
                    btb_valid(index_id) <= '1';
                end if;
            end if;
        end if;
    end process;

    -- BHT update: initialize bht on reset (runtime init)
    bht_update: process(clk)
        variable index_r : integer;
        variable c       : unsigned(1 downto 0);
        variable i : integer;
    begin
        if rising_edge(clk) then
            if reset_n = '0' then
                for i in 0 to DEPTH-1 loop
                    bht(i) <= "10"; -- weak not-taken
                end loop;
            else
                if resolve_valid = '1' then
                    index_r := idx_of(resolve_pc);
                    c := unsigned(bht(index_r));
						  
						  report "BHT_UPDATE: PC=" & integer'image(to_integer(unsigned(resolve_pc))) &
                       " index=" & integer'image(index_r) &
                       " old_cnt=" & integer'image(to_integer(c)) &
                       " taken=" & std_logic'image(resolve_taken)
                       severity note;
						  
                    if resolve_taken = '1' then
                        if c < 3 then c := c + 1; end if;
                    else
                        if c > 0 then c := c - 1; end if;
                    end if;
                    bht(index_r) <= std_logic_vector(c);
                end if;
            end if;
        end if;
    end process;

end architecture rtl;