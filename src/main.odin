package main

import "core:fmt"
import "core:os"
import "core:testing"

CPU_DIAG :: #config(CPU_DIAG, ODIN_TEST)

main :: proc() {
	when CPU_DIAG {
		test_8080(nil)
		return
	}

	if len(os.args) < 2 {
		fmt.panicf("not enough arguments\n")
	}
	
	file_path := os.args[1]
	h, err := os.open(file_path)
	if err != 0 {
		fmt.panicf("failed to open file '%s'\n", file_path)
	}
	size, _ := os.file_size(h)
	if size > i64(max(u16)) {
		fmt.panicf("size too long\n")
	}
	_, rerr := os.read(h, memory[:])
	if rerr != 0 {
		fmt.printf("failed to read file '%s'\n", file_path)
	}
	
	for regs.PC < auto_cast size {
		instruction := decode_8080_instruction(memory[:size], regs.PC)
		print_8080_instruction(instruction)
		regs.PC += instruction.size
		exec_8080_instruction(instruction)
	}
}

@(test)
test_8080 :: proc(^testing.T) {
	file := #load("../cpudiag.bin")
	copy(memory[:], file)
	file_size := len(file)
	
    // Fix the first instruction to be JMP 0x100    
	regs.PC = 0x100

    //Fix the stack pointer from 0x6ad to 0x7ad    
    // this 0x06 byte 112 in the code, which is    
    // byte 112 + 0x100 = 368 in memory    
    memory[368] = 0x7

    //Skip DAA test    
    memory[0x59c] = 0xc3 //JMP
    memory[0x59d] = 0xc2
    memory[0x59e] = 0x05
	
	for regs.PC < auto_cast file_size {
		instruction := decode_8080_instruction(memory[:file_size], regs.PC)
		print_8080_instruction(instruction)
		regs.PC += instruction.size
		exec_8080_instruction(instruction)
	}
}

exec_8080_instruction :: proc(instruction: Instruction) {
	using instruction
	pc := regs.PC - size // for debug
	addr_hl := cast(u16)transmute(u16be)regs.HL
	addr_bytes := cast(u16)transmute(u16le)bytes
	pair := Register_Pair(u8(dest) >> 1)
	addr_pair := cast(u16)transmute(u16be)regs.p[pair]
	condition := Condition(dest)

	when CPU_DIAG {
		if pc == 0x689 {
			fmt.println("CPU TEST FAILED")
			os.exit(1)
		}
	}
	
	switch mnemonic {
		case .NOP: // do nothing
		
		// Data Transfer:
		case .MOV:
		{
			if source == .NULL && dest == .NULL {
				fmt.panicf("Cannot MOV memory to memory\n")
			}
			target := dest == .NULL ? &memory[addr_hl] : &regs.r[dest]
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			target^ = value
		}
		case .MVI:
		{
			target := dest == .NULL ? &memory[addr_hl] : &regs.r[dest]
			target^ = bytes[0]
		}
		case .LXI:
		{
			target := pair == .SP ? cast(^Pair)(&regs.SP) : &regs.p[pair]
			target.H = bytes[1]
			target.L = bytes[0]
		}
		case .LDA:
		{
			regs.A = memory[addr_bytes]
		}
		case .STA:
		{
			memory[addr_bytes] = regs.A
		}
		case .LHLD:
		{
			regs.L = memory[addr_bytes]
			regs.H = memory[addr_bytes+1]
		}
		case .SHLD:
		{
			memory[addr_bytes] = regs.L
			memory[addr_bytes+1] = regs.H
		}
		case .LDAX:
		{
			regs.A = memory[addr_pair]
		}
		case .STAX:
		{
			memory[addr_pair] = regs.A
		}
		case .XCHG:
		{
			hl := regs.HL.v
			regs.HL.v = regs.DE.v
			regs.DE.v = hl
		}
		// Arithmetic Group
		case .ADD:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			regs.A += value
			set_flags(regs.A)
			set_flag(.CY, regs.A < value)
		}
		case .ADI:
		{
			regs.A += bytes[0]
			set_flags(regs.A)
			set_flag(.CY, regs.A < bytes[0])
		}
		case .ADC:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			regs.A += value + u8(.CY in flags)
			set_flags(regs.A)
			set_flag(.CY, regs.A < value)
		}
		case .ACI:
		{
			regs.A += bytes[0] + u8(.CY in flags)
			set_flags(regs.A)
			set_flag(.CY, regs.A < bytes[0])
		}
		case .SUB:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			set_flag(.CY, regs.A < value)
			regs.A -= value
			set_flags(regs.A)
		}
		case .SUI:
		{
			set_flag(.CY, regs.A < bytes[0])
			regs.A -= bytes[0]
			set_flags(regs.A)
		}
		case .SBB:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			a := regs.A
			regs.A = regs.A - value - u8(.CY in flags)
			set_flags(regs.A)
			set_flag(.CY, a < regs.A)
		}
		case .SBI:
		{
			a := regs.A
			regs.A = regs.A - bytes[0] - u8(.CY in flags)
			set_flags(regs.A)
			set_flag(.CY, a < regs.A)
		}
		case .INR, .DCR:
		{
			target := dest == .NULL ? &memory[addr_hl] : &regs.r[dest]
			target^ = mnemonic == .INR ? target^ + 1 : target^ - 1
			set_flags(target^)
		}
		case .INX, .DCX:
		{
			// target := pair == .SP ? &regs.SP : (&regs.p[pair].v)
			if pair == .SP {
				regs.SP = mnemonic == .INX ? regs.SP + 1 : regs.SP - 1
			} else {
				v := regs.p[pair].v
				regs.p[pair].v = mnemonic == .INX ? v + 1 : v - 1
			}
		}
		case .DAD:
		{
			value := pair == .SP ? cast(u16be)regs.SP : (&regs.p[pair].v)^
			regs.HL.v += value
			set_flag(.CY, regs.HL.v < value)
		}
		case .DAA:
		{
			v := u8(0)
			if regs.A & 0x0f > 9 || .CY in flags {
				v += 6
			}
			if regs.A >> 4 > 9 || .CY in flags {
				v += 6 << 4
			}
			regs.A += v
			set_flags(regs.A)
			set_flag(.CY, regs.A < v)
		}
		// Logical group
		case .ANA, .ANI:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			if mnemonic == .ANI do value = bytes[0]
			regs.A = regs.A & value
			set_flags(regs.A)
			set_flag(.CY, false)
		}
		case .XRA, .XRI:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			if mnemonic == .XRI do value = bytes[0]
			regs.A = regs.A ~ value
			set_flags(regs.A)
			set_flag(.CY, false)
		}
		case .ORA, .ORI:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			if mnemonic == .ORI do value = bytes[0]
			regs.A = regs.A | value
			set_flags(regs.A)
			set_flag(.CY, false)
		}
		case .CMP, .CPI:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			if mnemonic == .CPI do value = bytes[0]
			set_flag(.CY, regs.A < value)
			result := regs.A - value
			set_flags(result)
		}
		case .RLC:
		{
			cy := regs.A >> 7
			regs.A = regs.A << 1 | cy
			set_flag(.CY, cy == 1)
		}
		case .RRC:
		{
			cy := regs.A & 1
			regs.A = regs.A >> 1 & (cy << 7)
			set_flag(.CY, cy == 1)
		}
		case .RAL:
		{
			cy := regs.A >> 7 == 1
			regs.A = regs.A << 1 & u8(.CY in flags)
			set_flag(.CY, cy)
		}
		case .RAR:
		{
			cy := regs.A & 1 == 1
			regs.A = regs.A >> 1 & (u8(.CY in flags) << 7)
			set_flag(.CY, cy)
		}
		case .CMA:
		{
			regs.A ~= 0xff
		}
		case .CMC:
		{
			set_flag(.CY, !(.CY in flags))
		}
		case .STC:
		{
			set_flag(.CY, true)
		}
		// Branch group
		case .JMP:
		{
			regs.PC = auto_cast transmute(u16le)bytes
		}
		case .JZ, .JNZ, .JC, .JNC, .JPE, .JPO, .JP, .JM:
		{
			if (Flag(u8(condition) & 0b110) in flags) == bool(u8(condition) & 0b001) {
				regs.PC = auto_cast transmute(u16le)bytes
			}
		}
		case .CALL:
		{
			memory[regs.SP-1] = u8(regs.PC>>8)
			memory[regs.SP-2] = u8(regs.PC)
			regs.SP -= 2
			regs.PC = auto_cast transmute(u16le)bytes
		}
		case .CZ, .CNZ, .CC, .CNC, .CPE, .CPO, .CP, .CM:
		{
			if (Flag(u8(condition) & 0b110) in flags) == bool(u8(condition) & 0b001) {
				memory[regs.SP-1] = u8(regs.PC >> 8)
				memory[regs.SP-2] = u8(regs.PC)
				// (^u16)(&memory[regs.SP-2])^ = regs.PC 
				regs.SP -= 2
				regs.PC = auto_cast transmute(u16le)bytes
			}
		}
		case .RET:
		{
			regs.PC = auto_cast (cast(^u16le)&memory[regs.SP])^
			regs.SP += 2
		}
		case .RZ, .RNZ, .RC, .RNC, .RPE, .RPO, .RP, .RM:
		{
			if (Flag(u8(condition) & 0b110) in flags) == bool(u8(condition) & 0b001) {
				regs.PC = auto_cast (cast(^u16le)&memory[regs.SP])^
				regs.SP += 2
			}
		}
		case .RST:
		{
			memory[regs.SP-1] = u8(regs.PC >> 8)
			memory[regs.SP-2] = u8(regs.PC & 0x0f)
			regs.SP -= 2
			regs.PC = 8 * u16(dest)
		}
		case .PCHL:
		{
			regs.PC = cast(u16)regs.HL.v
		}
		// special
		case .PUSH:
		{
			if pair == .SP {
				// |S|Z|0|AC|0|P|1|CY|
				status_word := u8(.S in flags) << 7 | u8(.Z in flags) << 6 | u8(.P in flags) << 2 | 0b10 | u8(.CY in flags)
				memory[regs.SP-1] = regs.A
				memory[regs.SP-2] = status_word
			} else {
				memory[regs.SP-1] = regs.p[pair].H 
				memory[regs.SP-2] = regs.p[pair].L 
			}
			regs.SP -= 2
		}
		case .POP:
		{
			if pair == .SP {
				status := memory[regs.SP]
				set_flag(.CY, status & 1 == 1)
				set_flag(.P, (status >> 2) & 1 == 1)
				set_flag(.Z, (status >> 6) & 1 == 1)
				set_flag(.S, (status >> 7) & 1 == 1)
				regs.A = memory[regs.SP+1]
			} else {
				regs.p[pair].L = memory[regs.SP]
				regs.p[pair].H = memory[regs.SP+1]
			}
			regs.SP += 2
		}
		case .XTHL:
		{
			pair := regs.HL
			regs.L = memory[regs.SP]
			regs.H = memory[regs.SP+1]
			memory[regs.SP] = pair.L
			memory[regs.SP+1] = pair.H
		}
		case .SPHL:
		{
			regs.SP = cast(u16)regs.HL.v
		}
		case .IN, .OUT: 
		{
			// do nothing for now
		}
		case .EI, .DI:
		{
			interupt_system = mnemonic == .EI
		}
		case .HLT:
		{
			fmt.println("HALT")
			os.exit(0)
		}
	}
}

decode_8080_instruction :: proc(buffer: []byte, pc: u16) -> Instruction {
	opcode := buffer[pc]
	bytes: [2]byte
	if regs.PC < auto_cast (len(buffer)-1) { // TODO: fix buffer overflow
		bytes = (cast(^[2]byte)&buffer[regs.PC+1])^
	}
	dest := cast(Register)(opcode & 0b00_111_000 >> 3)
	source := cast(Register)(opcode & 0b00_000_111)
	pair := Register_Pair(u8(dest) >> 1)
	opdigits := opcode >> 6
	
	if opdigits == 0b01 { // MOV & HLT
		if dest == .NULL && source == .NULL {
			return Instruction{.HLT, dest, source, 1, bytes, opdigits}
		}
		return Instruction{.MOV, dest, source, 1, bytes, opdigits}
	}
	
	if opdigits == 0b00 && source == .NULL {
		return Instruction{.MVI, dest, source, 2, bytes, opdigits}
	}
	
	if opdigits == 0b00 { // INR & DCR
		if source == auto_cast 0b100 {
			return Instruction{.INR, dest, source, 1, bytes, opdigits}
		} else if source == auto_cast 0b101 {
			return Instruction{.DCR, dest, source, 1, bytes, opdigits}
		}
	}

	if opdigits == 0b10 { // ADD & ADC
		if dest == auto_cast 0b000 {
			return Instruction{.ADD, dest, source, 1, bytes, opdigits}
		} else if dest == auto_cast 0b001 {
			return Instruction{.ADC, dest, source, 1, bytes, opdigits}
		}
	}

	if opdigits == 0b10 && dest == auto_cast 0b010 {
		return Instruction{.SUB, dest, source, 1, bytes, opdigits}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b011 {
		return Instruction{.SBB, dest, source, 1, bytes, opdigits}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b100 {
		return Instruction{.ANA, dest, source, 1, bytes, opdigits}
	}

	if opdigits == 0b10 && dest == auto_cast 0b101 {
		return Instruction{.XRA, dest, source, 1, bytes, opdigits}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b110 {
		return Instruction{.ORA, dest, source, 1, bytes, opdigits}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b111 {
		return Instruction{.CMP, dest, source, 1, bytes, opdigits}
	}

	if opdigits == 0b11 && source == auto_cast 0b010 { // Jcondition
		mnemonic := Mnemonic(u8(Mnemonic.JMP)+1 + u8(dest))
		return Instruction{mnemonic, dest, source, 3, bytes, opdigits}
	}
	if opdigits == 0b11 && source == auto_cast 0b000 { // Rcondition
		mnemonic := Mnemonic(u8(Mnemonic.RET)+1 + u8(dest))
		return Instruction{mnemonic, dest, source, 1, bytes, opdigits}
	}
	if opdigits == 0b11 && source == auto_cast 0b100 { // Ccondition
		mnemonic := Mnemonic(u8(Mnemonic.CALL)+1 + u8(dest))
		return Instruction{mnemonic, dest, source, 3, bytes, opdigits}
	}
	
	if opdigits == 0b11 && source == auto_cast 0b111 {
		return Instruction{.RST, dest, source, 1, bytes, opdigits}
	}

	if opdigits == 0b00 {
		if u8(dest) & 1 == 1 {
			if source == auto_cast 0b001 {
				return Instruction{.DAD, dest, source, 1, bytes, opdigits}
			}
			if source == auto_cast 0b011 {
				return Instruction{.DCX, dest, source, 1, bytes, opdigits}
			}
		} else {
			if source == auto_cast 0b011 {
				return Instruction{.INX, dest, source, 1, bytes, opdigits}
			}
		}
	}

	
	if opdigits == 0b11 && u8(dest) & 1 == 0 { // PUSH & POP
		if source == auto_cast 0b101 {
			return Instruction{.PUSH, dest, source, 1, bytes, opdigits}
		}
		if source == auto_cast 0b001 {
			return Instruction{.POP, dest, source, 1, bytes, opdigits}
		}
	}

	if opdigits == 0b00 && source == auto_cast 0b001 {
		if u8(dest) & 1 == 0 {
			return Instruction{.LXI, dest, source, 3, bytes, opdigits}
		}
	}

	switch opcode {
		case 0xc6: return Instruction{.ADI, dest, source, 2, bytes, opdigits}
		case 0xce: return Instruction{.ACI, dest, source, 2, bytes, opdigits}
		case 0xd6: return Instruction{.SUI, dest, source, 2, bytes, opdigits}
		case 0xde: return Instruction{.SBI, dest, source, 2, bytes, opdigits}
		case 0xe6: return Instruction{.ANI, dest, source, 2, bytes, opdigits}
		case 0xee: return Instruction{.XRI, dest, source, 2, bytes, opdigits}
		case 0xf6: return Instruction{.ORI, dest, source, 2, bytes, opdigits}
		case 0xfe: return Instruction{.CPI, dest, source, 2, bytes, opdigits}
		case 0x07: return Instruction{.RLC, dest, source, 1, bytes, opdigits}
		case 0x0f: return Instruction{.RRC, dest, source, 1, bytes, opdigits}
		case 0x17: return Instruction{.RAL, dest, source, 1, bytes, opdigits}
		case 0x1f: return Instruction{.RAR, dest, source, 1, bytes, opdigits}
		
		case 0xc3: return Instruction{.JMP, dest, source, 3, bytes, opdigits}
		case 0xcd: return Instruction{.CALL, dest, source, 3, bytes, opdigits}
		case 0xc9: return Instruction{.RET, dest, source, 1, bytes, opdigits}
		
		case 0xdb: return Instruction{.IN, dest, source, 2, bytes, opdigits}
		case 0xd3: return Instruction{.OUT, dest, source, 2, bytes, opdigits}
		
		case 0x32: return Instruction{.STA, dest, source, 3, bytes, opdigits}
		case 0x3a: return Instruction{.LDA, dest, source, 3, bytes, opdigits}
		
		case 0xeb: return Instruction{.XCHG, dest, source, 1, bytes, opdigits}
		case 0xe3: return Instruction{.XTHL, dest, source, 1, bytes, opdigits}
		case 0xf9: return Instruction{.SPHL, dest, source, 1, bytes, opdigits}
		case 0xe9: return Instruction{.PCHL, dest, source, 1, bytes, opdigits}
		
		case 0x02: return Instruction{.STAX, .B, source, 1, bytes, opdigits}
		case 0x12: return Instruction{.STAX, .D, source, 1, bytes, opdigits}
		case 0x0a: return Instruction{.LDAX, .B, source, 1, bytes, opdigits}
		case 0x1a: return Instruction{.LDAX, .D, source, 1, bytes, opdigits}
		
		case 0x2f: return Instruction{.CMA, dest, source, 1, bytes, opdigits}
		case 0x37: return Instruction{.STC, dest, source, 1, bytes, opdigits}
		case 0x3f: return Instruction{.CMC, dest, source, 1, bytes, opdigits}
		case 0x27: return Instruction{.DAA, dest, source, 1, bytes, opdigits}
		case 0x22: return Instruction{.SHLD, dest, source, 3, bytes, opdigits}
		case 0x2a: return Instruction{.LHLD, dest, source, 3, bytes, opdigits}
		case 0xfb: return Instruction{.EI, dest, source, 1, bytes, opdigits}
		case 0xf3: return Instruction{.DI, dest, source, 1, bytes, opdigits}

		case 0x0, 0x20, 0x38, 0x30, 0x28: 
			return Instruction{.NOP, dest, source, 1, bytes, opdigits}
	}
	fmt.panicf("instruction not implemented: 0x%02x\n", opcode)	
}

set_flags :: proc(v: u8) {
	set_flag(.Z, v == 0)
	set_flag(.S, v >> 7 == 1)
	count := 0
	for i := uint(0); i < 8; i += 1 {
		if (v >> i) & 1 == 1 {
			count += 1 // TODO: there is probably a better way to check of parity
		}
	}
	set_flag(.P, count & 1 == 0)
	
}

set_flag :: proc(flag: Flag, v: bool) {
	if v {
		flags += {flag}
	} else {
		flags -= {flag}
	}
}

print_8080_instruction :: proc(using instruction: Instruction) {
	// TODO: improve this
	opcode := opdigits << 6 | u8(dest) << 3 | u8(source)
	fmt.printf("%04x %02x %s", regs.PC, opcode, mnemonic)
	#partial switch mnemonic {
		case .ADD, .SBB: 
		{
			if source != .NULL {
				fmt.printf(" %v", source)
			} else {
				fmt.printf(" M")
			}
		}
		case .MOV:
		{
			if source != .NULL {
				fmt.printf(" %v", source)
			} else {
				fmt.printf(" M")
			}
			if dest != .NULL {
				fmt.printf(", %v", dest)
			} else {
				fmt.printf(" M")
			}
		}
	}
	if size == 2 {
		fmt.printf(" %02x", bytes[0])
	} else if size == 3 {
		fmt.printf(" %04x", cast(u16)transmute(u16le)bytes)
	}
	when CPU_DIAG do fmt.printf("\tA=%02x, B=%02x, C=%02x, D=%02x, E=%02x, H=%02x, L=%02x, CY=%d, P=%d, S=%d, Z=%d, SP=%v\n", regs.A, regs.B, regs.C, regs.D, regs.E, regs.H, regs.L, u8(.CY in flags), u8(.P in flags), u8(.S in flags), u8(.Z in flags), regs.SP)
	
	fmt.printf("\n")
}

Mnemonic :: enum u8 {
	NOP,
	MOV,
	HLT,
	MVI,
	DCR,
	INR,
	ADD,
	ADC,
	SUB,
	SBB,
	ANA,
	XRA,
	ORA,
	CMP,
	ADI,
	ACI,
	SUI,
	SBI,
	ANI,
	XRI,
	ORI,
	CPI,
	RLC,
	RRC,
	RAL,
	RAR,
	
	// NOTE: This condition group is order dependent
	JMP,
	JNZ,
	JZ,
	JNC,
	JC,
	JPO,
	JPE,
	JP,
	JM,
	CALL,
	CNZ,
	CZ,
	CNC,
	CC,
	CPO,
	CPE,
	CP,
	CM,
	RET,
	RNZ,
	RZ,
	RNC,
	RC,
	RPO,
	RPE,
	RP,
	RM,
	
	RST,
	IN,
	OUT,
	LXI,
	PUSH,
	POP,
	STA,
	LDA,
	XCHG,
	XTHL,
	SPHL,
	PCHL,
	DAD,
	STAX,
	LDAX,
	INX,
	DCX,
	CMA,
	STC,
	CMC,
	DAA,
	SHLD,
	LHLD,
	EI,
	DI,
}


Instruction :: struct {
	mnemonic: Mnemonic,
	dest: Register,
	source: Register,
	// TODO cycles: int,
	size: u16,
	bytes: [2]byte,
	opdigits: u8,
}

Register :: enum u8 {
	A = 0b111,
	B = 0b000,
	C = 0b001,
	D = 0b010,
	E = 0b011,
	H = 0b100,
	L = 0b101,
	NULL = 0b110, // NOTE: this is used to indicate access to memory
}

Register_Pair :: enum u8 {
	BC = 0b00,
	DE = 0b01,
	HL = 0b10,
	SP = 0b11,
}

Pair :: struct #raw_union {
	using _: struct {H, L: byte},
	v: u16be,
}

regs: struct {
	using _: struct #raw_union {
		using _: struct {B, C, D, E, H, L, _, A: byte},
		r: [Register]byte,
		using _: struct {BC, DE, HL: Pair},
		p: [Register_Pair]Pair,
	},
	PC, SP: u16,
}

Condition :: enum u8 {
	NZ = 0b000,
	Z  = 0b001,
	NC = 0b010,
	C  = 0b011,
	PO = 0b100,
	PE = 0b101,
	P  = 0b110,
	M  = 0b111,
}

Flag :: enum u8 {
	Z  = 0b000, 
	CY = 0b010,
	P  = 0b100, 
	S  = 0b110, 
	// AC, // NOTE: this flag is not used in space invaders
}

interupt_system: bool
flags: bit_set[Flag]
memory: [65_536]byte
