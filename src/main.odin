package main

import "core:fmt"
import "core:os"


main :: proc() {
	if len(os.args) < 2 {
		fmt.panicf("not enough arguments\n")
	}

	data, ok := os.read_entire_file_from_filename(os.args[1])
	if !ok {
		fmt.panicf("failed to load file '%s'\n", os.args[1])
	}
	
	for regs.PC < auto_cast len(data) {
		instruction := decode_8080_instruction(data)
		print_8080_instruction(instruction)
		exec_8080_instruction(instruction)
		regs.PC += instruction.size
	}
}

exec_8080_instruction :: proc(using instruction: Instruction) {
	addr := transmute(u16)[2]byte{regs.r[.H], regs.r[.L]}
	switch mnemonic {
		case .NOP: // do nothing
		
		// Data Transfer:
		case .MOV:
		{
			if source == .NULL && dest == .NULL {
				fmt.panicf("Cannot MOV memory to memory\n")
			}
			target := dest == .NULL ? &memory[addr] : &regs.r[dest]
			value := source == .NULL ? memory[addr] : regs.r[source]
			target^ = value
		}
		case .MVI:
		{
			target := dest == .NULL ? &memory[addr] : &regs.r[dest]
			target^ = bytes[0]
		}
		case .LXI:
		{
			target := pair == .SP ? cast(^[2]byte)(&regs.SP) : cast(^[2]byte)&regs.r[dest]
			target[0] = bytes[1]
			target[1] = bytes[0]
		}
		case .LDA:
		{
			addr := transmute(u16)([2]byte{bytes[1], bytes[0]})
			regs.A = memory[addr]
		}
		case .STA:
		{
			addr := transmute(u16)([2]byte{bytes[1], bytes[0]})
			memory[addr] = regs.A
		}
		case .LHLD:
		{
			addr := transmute(u16)([2]byte{bytes[1], bytes[0]})
			regs.L = memory[addr]
			regs.H = memory[addr+1]
		}
		case .SHLD:
		{
			addr := transmute(u16)([2]byte{bytes[1], bytes[0]})
			memory[addr] = regs.L
			memory[addr+1] = regs.H
		}
		case .LDAX:
		{
			reg := Register(u8(pair) << 1)
			addr := transmute(u16)([2]byte{regs.r[reg], regs.r[Register(u8(reg)+1)]})
			regs.A = memory[addr]
		}
		case .STAX:
		{
			addr := transmute(u16)([2]byte{regs.r[dest], regs.r[Register(u8(dest)+1)]})
			memory[addr] = regs.A
		}
		case .XCHG:
		{
			hl := (^[2]byte)(&regs.H)
			de := (^[2]byte)(&regs.D)
			p := hl^
			hl^ = de^
			de^ = p
		}
		// Arithmetic Group
		case .ADD:
		{
			value := source == .NULL ? memory[addr] : regs.r[source]
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
			value := source == .NULL ? memory[addr] : regs.r[source]
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
			value := source == .NULL ? memory[addr] : regs.r[source]
			regs.A -= value
			set_flags(regs.A)
			set_flag(.CY, regs.A > value)
		}
		case .SUI:
		{
			regs.A -= bytes[0]
			set_flags(regs.A)
			set_flag(.CY, regs.A > bytes[0])
		}
		case .SBB:
		{
			value := source == .NULL ? memory[addr] : regs.r[source]
			regs.A -= value - u8(.CY in flags)
			set_flags(regs.A)
			set_flag(.CY, regs.A > value)
		}
		case .SBI:
		{
			regs.A -= bytes[0] - u8(.CY in flags)
			set_flags(regs.A)
			set_flag(.CY, regs.A > bytes[0])
		}
		case .INR, .DCR:
		{
			target := dest == .NULL ? &memory[addr] : &regs.r[dest]
			target^ = mnemonic == .INR ? target^ + 1 : target^ - 1
			set_flags(target^)
		}
		case .INX, .DCX:
		{
			reg := Register(u8(pair) << 1)
			target := pair == .SP ? &regs.PC : (^u16)(&regs.r[reg])
			target^ = mnemonic == .INX ? target^ + 1 : target^ - 1
		}
		case .DAD:
		{
			reg := Register(u8(pair) << 1)
			value := pair == .SP ? regs.PC : (^u16)(&regs.r[reg])^
			hl := (^u16)(&regs.H) 
			hl^ += value
			set_flag(.CY, hl^ < value)
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
			value := source == .NULL ? memory[addr] : regs.r[source]
			if mnemonic == .ANI do value = bytes[0]
			regs.A = regs.A & value
			set_flags(regs.A)
			set_flag(.CY, false)
		}
		case .XRA, .XRI:
		{
			value := source == .NULL ? memory[addr] : regs.r[source]
			if mnemonic == .XRI do value = bytes[0]
			regs.A = regs.A ~ value
			set_flags(regs.A)
			set_flag(.CY, false)
		}
		case .ORA, .ORI:
		{
			value := source == .NULL ? memory[addr] : regs.r[source]
			if mnemonic == .ORI do value = bytes[0]
			regs.A = regs.A | value
			set_flags(regs.A)
			set_flag(.CY, false)
		}
		case .CMP, .CPI:
		{
			value := source == .NULL ? memory[addr] : regs.r[source]
			if mnemonic == .CPI do value = bytes[0]
			result := regs.A - value
			set_flags(result)
			set_flag(.CY, regs.A < value)
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
			regs.PC = transmute(u16)([2]byte{bytes[1], bytes[0]})
		}
		case .JZ, .JNZ, .JC, .JNC, .JPE, .JPO, .JP, .JM:
		{
			if (Flag(u8(condition) & 0b110) in flags) == bool(u8(condition) & 0b001) {
				regs.PC = transmute(u16)([2]byte{bytes[1], bytes[0]})
			}
		}
		case .CALL:
		{
			// memory[regs.SP-1] = u8(regs.PC>>8)
			// memory[regs.SP-2] = u8(regs.PC)
			(^u16)(&memory[regs.SP-2])^ = regs.PC 
			regs.SP -= 2
			regs.PC = transmute(u16)([2]byte{bytes[1], bytes[0]})
		}
		case .CZ, .CNZ, .CC, .CNC, .CPE, .CPO, .CP, .CM:
		{
			if (Flag(u8(condition) & 0b110) in flags) == bool(u8(condition) & 0b001) {
				(^u16)(&memory[regs.SP-2])^ = regs.PC 
				regs.SP -= 2
				regs.PC = transmute(u16)([2]byte{bytes[1], bytes[0]})
			}
		}
		case .RET:
		{
			regs.PC = transmute(u16)([2]byte{memory[regs.SP+1], memory[regs.SP]})
		}
		case .RZ, .RNZ, .RC, .RNC, .RPE, .RPO, .RP, .RM:
		{
			if (Flag(u8(condition) & 0b110) in flags) == bool(u8(condition) & 0b001) {
				regs.PC = transmute(u16)([2]byte{memory[regs.SP+1], memory[regs.SP]})
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
			regs.PC = (cast(^u16)&regs.H)^
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
				rh := Register(u8(pair) << 1)
				rl := Register(u8(rh) + 1)
				memory[regs.SP-1] = regs.r[rh] 
				memory[regs.SP-2] = regs.r[rl] 
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
				rh := Register(u8(pair) << 1)
				rl := Register(u8(rh) + 1)
				regs.r[rl] = memory[regs.SP]
				regs.r[rh] = memory[regs.SP+1]
			}
			regs.SP += 2
		}
		case .XTHL:
		{
			pair := [2]byte{regs.L, regs.H}
			regs.L = memory[regs.SP]
			regs.H = memory[regs.SP+1]
			memory[regs.SP] = pair[0]
			memory[regs.SP+1] = pair[1]
		}
		case .SPHL:
		{
			regs.SP = (^u16)(&regs.H)^
		}
		case .IN, .OUT, .EI, .DI:
		{
			// TODO
			fmt.eprintln("instruction not implemented:")
			print_8080_instruction(instruction)
			os.exit(1)
		}
		case .HLT:
		{
			fmt.println("HALT")
			os.exit(0)
		}
	}
}

decode_8080_instruction :: proc(buffer: []byte) -> Instruction {
	opcode := buffer[regs.PC]

	bytes: [2]byte
	if regs.PC < auto_cast (len(buffer)-1) { // TODO: fix buffer overflow
		bytes = (cast(^[2]byte)&buffer[regs.PC+1])^
	}
	dest := cast(Register)(opcode & 0b00_111_000 >> 3)
	source := cast(Register)(opcode & 0b00_000_111)
	pair := Register_Pair(u8(dest) >> 1)
	opdigits := opcode >> 6
	
	if opcode == 0x0 {
		return Instruction{.NOP, {dest=dest}, source, 1, bytes}
	}

	if opdigits == 0b01 { // MOV & HLT
		if dest == .NULL && source == .NULL {
			return Instruction{.HLT, {dest=dest}, source, 1, bytes}
		}
		return Instruction{.MOV, {dest=dest}, source, 1, bytes}
	}
	
	if opdigits == 0b00 && source == .NULL {
		return Instruction{.MVI, {dest=dest}, source, 2, bytes}
	}
	
	if opdigits == 0b00 { // INR & DCR
		if source == auto_cast 0b100 {
			return Instruction{.INR, {dest=dest}, source, 1, bytes}
		} else if source == auto_cast 0b101 {
			return Instruction{.DCR, {dest=dest}, source, 1, bytes}
		}
	}

	if opdigits == 0b10 { // ADD & ADC
		if dest == auto_cast 0b000 {
			return Instruction{.ADD, {dest=dest}, source, 1, bytes}
		} else if dest == auto_cast 0b001 {
			return Instruction{.ADC, {dest=dest}, source, 1, bytes}
		}
	}

	if opdigits == 0b10 && dest == auto_cast 0b010 {
		return Instruction{.SUB, {dest=dest}, source, 1, bytes}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b011 {
		return Instruction{.SBB, {dest=dest}, source, 1, bytes}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b100 {
		return Instruction{.ANA, {dest=dest}, source, 1, bytes}
	}

	if opdigits == 0b10 && dest == auto_cast 0b101 {
		return Instruction{.XRA, {dest=dest}, source, 1, bytes}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b110 {
		return Instruction{.ORA, {dest=dest}, source, 1, bytes}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b111 {
		return Instruction{.CMP, {dest=dest}, source, 1, bytes}
	}

	if opdigits == 0b11 && source == auto_cast 0b010 { // Jcondition
		mnemonic := Mnemonic(u8(Mnemonic.JMP)+1 + u8(dest))
		return Instruction{mnemonic, {condition=Condition(dest)}, source, 3, bytes}
	}
	if opdigits == 0b11 && source == auto_cast 0b000 { // Rcondition
		mnemonic := Mnemonic(u8(Mnemonic.RET)+1 + u8(dest))
		return Instruction{mnemonic, {condition=Condition(dest)}, source, 3, bytes}
	}
	if opdigits == 0b11 && source == auto_cast 0b100 { // Ccondition
		mnemonic := Mnemonic(u8(Mnemonic.CALL)+1 + u8(dest))
		return Instruction{mnemonic, {condition=Condition(dest)}, source, 3, bytes}
	}
	
	if opdigits == 0b11 && source == auto_cast 0b111 {
		return Instruction{.RST, {dest=dest}, source, 1, bytes}
	}

	if opdigits == 0b00 {
		if u8(dest) & 1 == 1 {
			if source == auto_cast 0b001 {
				return Instruction{.DAD, {pair=pair}, source, 1, bytes}
			}
			if source == auto_cast 0b011 {
				return Instruction{.DCX, {pair=pair}, source, 1, bytes}
			}
		} else {
			if source == auto_cast 0b011 {
				return Instruction{.INX, {pair=pair}, source, 1, bytes}
			}
		}
	}

	
	if opdigits == 0b11 && u8(dest) & 1 == 0 { // PUSH & POP
		if source == auto_cast 0b101 {
			return Instruction{.PUSH, {pair=pair}, source, 1, bytes}
		}
		if source == auto_cast 0b001 {
			return Instruction{.POP, {pair=pair}, source, 1, bytes}
		}
	}

	if opdigits == 0b00 && source == auto_cast 0b001 {
		if u8(dest) & 1 == 0 {
			return Instruction{.LXI, {pair=pair}, source, 3, bytes}
		}
	}

	switch opcode {
		case 0xc6: return Instruction{.ADI, {dest=dest}, source, 2, bytes}
		case 0xce: return Instruction{.ACI, {dest=dest}, source, 2, bytes}
		case 0xd6: return Instruction{.SUI, {dest=dest}, source, 2, bytes}
		case 0xde: return Instruction{.SBI, {dest=dest}, source, 2, bytes}
		case 0xe6: return Instruction{.ANI, {dest=dest}, source, 2, bytes}
		case 0xee: return Instruction{.XRI, {dest=dest}, source, 2, bytes}
		case 0xf6: return Instruction{.ORI, {dest=dest}, source, 2, bytes}
		case 0xfe: return Instruction{.CPI, {dest=dest}, source, 2, bytes}
		case 0x07: return Instruction{.RLC, {dest=dest}, source, 1, bytes}
		case 0x0f: return Instruction{.RRC, {dest=dest}, source, 1, bytes}
		case 0x17: return Instruction{.RAL, {dest=dest}, source, 1, bytes}
		case 0x1f: return Instruction{.RAR, {dest=dest}, source, 1, bytes}
		
		case 0xc3: return Instruction{.JMP, {dest=dest}, source, 3, bytes}
		case 0xcd: return Instruction{.CALL, {dest=dest}, source, 3, bytes}
		case 0xc9: return Instruction{.RET, {dest=dest}, source, 1, bytes}
		
		case 0xdb: return Instruction{.IN, {dest=dest}, source, 2, bytes}
		case 0xd3: return Instruction{.OUT, {dest=dest}, source, 2, bytes}
		
		case 0x32: return Instruction{.STA, {dest=dest}, source, 3, bytes}
		case 0x3a: return Instruction{.LDA, {dest=dest}, source, 3, bytes}
		
		case 0xeb: return Instruction{.XCHG, {dest=dest}, source, 1, bytes}
		case 0xe3: return Instruction{.XTHL, {dest=dest}, source, 1, bytes}
		case 0xf9: return Instruction{.SPHL, {dest=dest}, source, 1, bytes}
		case 0xe9: return Instruction{.PCHL, {dest=dest}, source, 1, bytes}
		
		case 0x02: return Instruction{.STAX, {dest=.B}, source, 1, bytes}
		case 0x12: return Instruction{.STAX, {dest=.D}, source, 1, bytes}
		case 0x0a: return Instruction{.LDAX, {dest=.B}, source, 1, bytes}
		case 0x1a: return Instruction{.LDAX, {dest=.D}, source, 1, bytes}
		
		case 0x2f: return Instruction{.CMA, {dest=dest}, source, 1, bytes}
		case 0x37: return Instruction{.STC, {dest=dest}, source, 1, bytes}
		case 0x3f: return Instruction{.CMC, {dest=dest}, source, 1, bytes}
		case 0x27: return Instruction{.DAA, {dest=dest}, source, 1, bytes}
		case 0x22: return Instruction{.SHLD, {dest=dest}, source, 3, bytes}
		case 0x2a: return Instruction{.LHLD, {dest=dest}, source, 3, bytes}
		case 0xfb: return Instruction{.EI, {dest=dest}, source, 1, bytes}
		case 0xf3: return Instruction{.DI, {dest=dest}, source, 1, bytes}
	}
	fmt.panicf("instruction not implemented: 0x%02x\n", opcode)	
}

set_flags :: proc(v: u8) {
	set_flag(.Z, v == 0)
	set_flag(.S, v >> 7 == 1)
	set_flag(.P, v & 1 == 0)
	// set_flag(.CY, cy)
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
	fmt.printf("%04x %s\n", regs.PC, mnemonic)
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

Register_Pair :: enum u8 {
	BC = 0b00,
	DE = 0b01,
	HL = 0b10,
	SP = 0b11,
}

Dest :: struct #raw_union {
	dest: Register,
	condition: Condition,
	pair: Register_Pair,
}

Instruction :: struct {
	mnemonic: Mnemonic,
	using _: Dest,
	source: Register,
	// TODO cycles: int,
	size: u16,
	bytes: [2]byte,
}

Register :: enum u8 {
	A = 0b111,
	B = 0b000,
	C = 0b001,
	D = 0b010,
	E = 0b011,
	H = 0b100,
	L = 0b101,
	NULL = 0b110, // NOTE: this is not a register
}

regs: struct {
	using _: struct #raw_union {
		using _: struct {A, B, C, D, E, H, L, NULL: byte},
		r: [Register]byte,
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

flags: bit_set[Flag]

memory: [65_536]byte
