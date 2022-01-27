library ieee;
use ieee.numeric_bit.all;
use std.textio.all;

entity reg is
generic (wordSize: natural := 4);
port(
	clock: in bit; 
	reset: in bit;
	load: in bit;
	d: in bit_vector(wordSize-1 downto 0); 
	q: out bit_vector(wordSize-1 downto 0)
);
end reg ;

architecture reg_arch of reg is
begin 
	process(clock, reset, load)
	begin 
		if (reset = '1') then 
			q <= (others => '0');
		elsif (clock = '1' and clock'event) then 
			if (load = '1') then 
				q <= d;
			end if;
		end if;
	end process;
end reg_arch;



library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;
use std.textio.all;

entity regfile is
	generic(
		regn: natural := 32;
		wordSize: natural := 64
	);
	port(
		clock: in bit;
		reset: in bit;
		regWrite: in bit;
		rr1 , rr2 , wr : in bit_vector(natural(ceil(log2(real(regn))))-1 downto 0);
		d: in bit_vector(wordSize-1 downto 0);
		q1 , q2 : out bit_vector(wordSize-1 downto 0)
	);
end regfile;

architecture regfile_arch of regfile is 

	type reg_type is array (0 to regn-1)of bit_vector(wordSize-1 downto 0);
	signal reg: reg_type;
	begin
	
	process (clock, reset, regWrite) is
	begin
		reg(regn-1) <= (others => '0'); 
		if (reset = '1') then
			for i in 0 to regn-1 loop
				reg(i) <= (others => '0');
			end loop;
		elsif(clock = '1' and clock'event) then 
			if (regWrite = '1' and (to_integer(unsigned(wr)) /= (regn-1))) then
				reg(to_integer(unsigned(wr))) <= d;
			end if;
		end if;
	end process;
		q1 <= reg(to_integer(unsigned(rr1)));
		q2 <= reg(to_integer(unsigned(rr2)));
	
end regfile_arch;



library ieee;
use ieee.numeric_bit.all;

entity signExtend is
	port(
	i: in bit_vector(31 downto 0);
	o: out bit_vector(63 downto 0)
	);
end signExtend;

architecture signExtend_arch of signExtend is
	signal zeros,ones,se_out : bit_vector(63 downto 0);
	begin
		zeros <= (63 downto 0 => '0');
		ones <= (63 downto 0 => '1');
		se_out <= zeros(63 downto 19) & i(23 downto 5) when (i(31 downto 24) = "10110100" and i(23) = '0') else     
                  ones(63 downto 19) & i(23 downto 5) when (i(31 downto 24) = "10110100" and i(23) = '1') else
				  zeros(63 downto 26) & i(25 downto 0) when (i(31 downto 26) = "000101" and i(25) = '0') else       
                  ones(63 downto 26) & i(25 downto 0) when (i(31 downto 26) = "000101" and i(25) = '1')else
				  zeros(63 downto 9) & i(20 downto 12) when (i(31 downto 21) = "11111000010" and i(20) = '0') else   
				  ones(63 downto 9) & i(20 downto 12) when (i(31 downto 21) = "11111000010" and i(20) = '1') else
				  zeros(63 downto 9) & i(20 downto 12) when (i(31 downto 21)="11111000000" and i(20) = '0') else   
				  ones(63 downto 9) & i(20 downto 12) when (i(31 downto 21)="11111000000" and i(20) = '1');
        o <= se_out;	
end architecture;



library ieee;
use ieee.numeric_bit.all;

entity alu1bit is
	port(
		a , b , less , cin : in bit;
		result , cout , set , overflow : out bit ;
		ainvert , binvert : in bit ;
		operation : in bit_vector(1 downto 0)
	);
end entity;


architecture alu1bit_arch of alu1bit is 

signal newa, newb, sum, co: bit; 

begin
	with ainvert select 
	newa <= a when '0',
			(not a) when '1';
	with binvert select 
	newb <= b when '0',
			(not b) when '1';
			
	sum <= newa xor newb xor cin;
	co <= (newa and newb) or (cin and newa) or (cin and newb);
	
	set <= sum;
	cout <= co;			
	overflow <= cin xor co;
	with operation select 
	result <= newa and newb when "00",
		      newa or newb when "01",
		      sum when "10",
		      b when "11";

	
end architecture ;

library ieee;
use ieee.numeric_bit.all;

entity alu is
generic (
	size : natural := 10 						
);
port(
	A, B : in bit_vector (size-1 downto 0 ); 	
	F : out bit_vector (size-1 downto 0 ); 		
	S : in bit_vector (3 downto 0 ) ; 			
	Z : out bit; 								
	Ov : out bit;								
	Co : out bit 								
) ;
end entity alu ;



architecture alu_arch of alu is 

component alu1bit is 

port(
		a , b , less , cin : in bit;
		result , cout , set , overflow : out bit ;
		ainvert , binvert : in bit ;
		operation : in bit_vector(1 downto 0)
	);
end component;

signal set_less, sub_a, sub_b, cin_first: bit;
signal res_v : bit_vector (size-1 downto 0);
signal b_v : bit_vector (size-1 downto 0);
signal cout_v : bit_vector(size-1 downto 0);   

begin 
	b_v <= B;
	cin_first <= '1' when S(3 downto 2) = "01" else 
				 '1' when S(3 downto 2) = "10" else
				 '1' when S(3 downto 2) = "11" else
				 '0';
			   
	first_bit : alu1bit port map(A(0),B(0),b_v(0),cin_first,
								 res_v(0),cout_v(0),open,open,
								 S(3),S(2),
								 S(1 downto 0)
								);
								
	gen : for i in size-2 downto 1 generate
		intern_bit : alu1bit port map(A(i),B(i),b_v(i), cout_v(i-1),
								      res_v(i),cout_v(i),open,open,
								      S(3),S(2),
								      S(1 downto 0)
									 ); 
	end generate;
	last_bit : alu1bit port map (A(size-1),B(size-1),b_v(size-1), cout_v(size-2),
								 res_v(size-1),Co,open,Ov,
								 S(3), S(2),
								 S(1 downto 0)
								);
								
	Z <= '1' when unsigned(res_v) = 0  else
         '0';
		 
	F <= res_v;
	
end architecture;



library ieee;
use ieee.numeric_bit.all;

entity alucontrol is
    port (
        aluop: in bit_vector (1 downto 0);
        opcode: in bit_vector(10 downto 0);
        aluCtrl: out bit_vector (3 downto 0)
    );
end entity;

architecture alucontrol_arch of alucontrol is
	begin
		aluCtrl <= "0010" when aluop = "00" else 
		           "0111" when aluop = "01" else
				   "0010" when aluop = "10" and opcode = "10001011000" else
                   "0110" when aluop = "10" and opcode = "11001011000" else 
                   "0000" when aluop = "10" and opcode = "10001010000" else 
                   "0001" when aluop = "10" and opcode = "10101010000"; 
end architecture; 



library ieee;
use ieee.numeric_bit.all;

entity controlunit is
	port (
    reg2loc : out bit;
    uncondBranch : out bit;
    branch : out bit;
    memRead : out bit;
    memToReg : out bit;
    aluOp : out bit_vector(1 downto 0);
    memWrite : out bit;
    aluSrc : out bit;
    regWrite : out bit;
    opcode : in bit_vector(10 downto 0)
    );
end entity;



architecture controlunit_arch of controlunit is
	begin
		reg2loc <= '1' when (opcode = "11111000000" or opcode(10 downto 3) = "10110100") else '0';
		uncondBranch <= '1' when opcode(10 downto 5) = "000101" else '0';
		branch <= '1' when opcode(10 downto 3) = "10110100" else '0';
		memRead <= '1' when opcode = "11111000010" else '0';
		memToReg <= '1' when opcode = "11111000010" else '0';
		aluOp <= "00" when opcode = "11111000010" or opcode = "11111000000" else
				 "01" when opcode(10 downto 3) = "10110100" or opcode(10 downto 5) = "000101" else
				 "10" when opcode = "10001011000" or opcode = "11001011000" or opcode = "10001010000" or opcode = "10101010000";
		memWrite <= '1' when opcode = "11111000000" else '0';
		aluSrc <= '1' when opcode = "11111000010" or opcode = "11111000000" else '0';
		regWrite <= '1' when opcode = "11111000010" or opcode = "10001011000" or opcode = "11001011000" or opcode = "10001010000" or opcode = "10101010000" else'0';
  end architecture;



library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;
use std.textio.all;

entity datapath is
port (
clock : in bit;
reset : in bit;
reg2loc : in bit;
pcsrc : in bit ;
memToReg : in bit ;
aluCtrl : in bit_vector (3 downto 0 );
aluSrc : in bit;
regWrite : in bit ;
opcode : out bit_vector (10 downto 0);
zero : out bit ;
imAddr : out bit_vector (63 downto 0);
imOut : in bit_vector (31 downto 0);
dmAddr : out bit_vector (63 downto 0);
dmIn : out bit_vector (63 downto 0);
dmOut : in bit_vector (63 downto 0)
);
end entity datapath;

architecture datapath_arch of datapath is 

	component reg is 
		generic (wordSize: natural := 64);
		port(
		clock: in bit; 
		reset: in bit;
		load: in bit;
		d: in bit_vector(wordSize-1 downto 0); 
		q: out bit_vector(wordSize-1 downto 0)
		);
	end component;
	
	component regfile is 
		generic(
			regn: natural := 32;
			wordSize: natural := 64
		);
		port(
			clock: in bit;
			reset: in bit;
			regWrite: in bit;
			rr1 , rr2 , wr : in bit_vector(natural(ceil(log2(real(regn))))-1 downto 0);
			d: in bit_vector(wordSize-1 downto 0);
			q1 , q2 : out bit_vector(wordSize-1 downto 0)
		);
	end component;

	component signExtend is 
		port(
		i: in bit_vector(31 downto 0);
		o: out bit_vector(63 downto 0)
		);
	end component;
	
	component alu is 
		generic (
		size : natural := 64 						
		);
		port(
		A, B : in bit_vector (size-1 downto 0 ); 	
		F : out bit_vector (size-1 downto 0 ); 		
		S : in bit_vector (3 downto 0 ) ; 			
		Z : out bit; 								
		Ov : out bit;								
		Co : out bit 								
		);
	end component;
	
	constant intquatro : bit_vector(63 downto 0) := (2 => '1', others=> '0');
	signal shiftLeft2, nextPC_shift2, nextPC_soma4, muxPC, signExtendOut, FOut, muxWriteData, q1_signal, q2_signal, muxALU : bit_vector(63 downto 0);
	signal muxReadRegister: bit_vector(4 downto 0);
	signal PC : bit_vector(63 downto 0) := (others => '0');
	
	begin 
	PCREGX : reg port map (clock, reset, '1', muxPC, PC);
	REGFILEX : regfile port map (clock, reset, regWrite, imOut(9 downto 5), muxReadRegister, imOut(4 downto 0),muxWriteData,q1_signal,q2_signal);
	SIGNEXTENDX : signExtend port map (imOut, signExtendOut);
	ALUX : alu port map (q1_signal, muxAlu, FOut, aluCtrl, zero, open, open);
    PCALUX_SHIFT2 : alu port map (PC, shiftLeft2, nextPC_shift2, "0010", open, open, open);
	PCALUX_SOMA4 : alu port map (PC, intquatro,  nextPC_soma4, "0010", open, open, open);
	
	shiftLeft2 <= signExtendOut (61 downto 0) & "00";
	opcode <= imOut(31 downto 21);
    muxPC <= nextPC_soma4 when pcsrc = '0' else
             nextPC_shift2;
	muxWriteData<= FOut when memToReg = '0' else
                    dmOut ;
    muxReadRegister <= imOut(20 downto 16) when reg2loc = '0' else
                  imOut(4  downto  0);
	muxAlu   <= q2_signal when aluSrc = '0' else
                signExtendOut ;
	imAddr <= PC;
	dmIn   <= q2_signal;
    dmAddr <= FOut;
	
end datapath_arch;



library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;
use std.textio.all;

entity polilegsc is
port (
clock , reset : in bit ;
dmem_addr : out bit_vector (63 downto 0 );
dmem_dati : out bit_vector (63 downto 0 );
dmem_dato : in bit_vector (63 downto 0);
dmem_we: out bit ;
imem_addr : out bit_vector (63 downto 0 ) ;
imem_data : in bit_vector (31 downto 0)
);
end entity;

architecture polilegsc_arch of polilegsc is

    component alucontrol is
        port(
        aluop : in bit_vector(1 downto 0);
        opcode : in bit_vector(10 downto 0);
        aluCtrl : out bit_vector(3 downto 0)
        ) ;
    end component;

	component controlunit is 
		port (
		reg2loc : out bit;
		uncondBranch : out bit;
		branch : out bit;
		memRead : out bit;
		memToReg : out bit;
		aluOp : out bit_vector(1 downto 0);
		memWrite : out bit;
		aluSrc : out bit;
		regWrite : out bit;
		opcode : in bit_vector(10 downto 0)
    ); 
	end component;

	component datapath is 
		port (
		clock : in bit;
		reset : in bit;
		reg2loc : in bit;	
		pcsrc : in bit ;
		memToReg : in bit ;
		aluCtrl : in bit_vector (3 downto 0 );
		aluSrc : in bit;
		regWrite : in bit ;
		opcode : out bit_vector (10 downto 0);
		zero : out bit ;
		imAddr : out bit_vector (63 downto 0);
		imOut : in bit_vector (31 downto 0);
		dmAddr : out bit_vector (63 downto 0);
		dmIn : out bit_vector (63 downto 0);
		dmOut : in bit_vector (63 downto 0)
	);
	end component;
	
	signal Reg2Loc_signal, MemtoReg_signal, ALUSrc_signal, RegWrite_signal,  Zero_signal, muxComb,Uncondbranch_signal, Branch_signal, MemRead_signal: bit;
	signal ALUOp_signal :  bit_vector(1 downto 0);
	signal OPcode_signal:  bit_vector (10 downto 0);
    signal ALUControl_signal: bit_vector (3 downto 0);
  
    begin  
	ALUCONTROLX : alucontrol  port map (ALUOp_signal,OPcode_signal,ALUControl_signal);
	CONTROLUNITX : controlunit port map (Reg2Loc_signal,Uncondbranch_signal,Branch_signal,MemRead_signal,MemtoReg_signal,ALUOp_signal,dmem_we,ALUSrc_signal,RegWrite_signal,OPcode_signal);
    DATAPATHX : datapath port map (clock,reset,Reg2Loc_signal,muxComb,MemtoReg_signal,ALUControl_signal,ALUSrc_signal,RegWrite_signal,OPcode_signal,Zero_signal,imem_addr,imem_Data,dmem_addr,dmem_dati,dmem_dato);
  	
    muxComb <= (Uncondbranch_signal or (Branch_signal and Zero_signal));
	
    end  polilegsc_arch; 
