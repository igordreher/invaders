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
		regs.PC += instruction.size
	}
}

decode_8080_instruction :: proc(buffer: []byte) -> Instruction {
	opcode := buffer[regs.PC]

	bytes: [2]byte
	if regs.PC < auto_cast (len(buffer)-1) {
		bytes = (cast(^[2]byte)&buffer[regs.PC+1])^
	}
	dest := cast(Register)(opcode & 0b00_111_000 >> 3)
	source := cast(Register)(opcode & 0b00_000_111)
	opdigits := opcode >> 6
	bdhsp := bit_set[Register]{.B, .D, .H, .NULL}
	
	if opcode == 0x0 {
		return Instruction{.NOP, dest, source, 1, bytes}
	}

	if opdigits == 0b01 { // MOV & HLT
		if dest == .NULL && source == .NULL {
			return Instruction{.HLT, dest, source, 1, bytes}
		}
		return Instruction{.MOV, dest, source, 1, bytes}
	}
	
	if opdigits == 0b00 && source == .NULL {
		return Instruction{.MVI, dest, source, 2, bytes}
	}
	
	if opdigits == 0b00 { // INR & DCR
		if source == auto_cast 0b100 {
			return Instruction{.INR, dest, source, 1, bytes}
		} else if source == auto_cast 0b101 {
			return Instruction{.DCR, dest, source, 1, bytes}
		}
	}

	if opdigits == 0b10 { // ADD & ADC
		if dest == auto_cast 0b000 {
			return Instruction{.ADD, dest, source, 1, bytes}
		} else if dest == auto_cast 0b001 {
			return Instruction{.ADC, dest, source, 1, bytes}
		}
	}

	if opdigits == 0b10 && dest == auto_cast 0b010 {
		return Instruction{.SUB, dest, source, 1, bytes}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b011 {
		return Instruction{.SBB, dest, source, 1, bytes}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b100 {
		return Instruction{.ANA, dest, source, 1, bytes}
	}

	if opdigits == 0b10 && dest == auto_cast 0b101 {
		return Instruction{.XRA, dest, source, 1, bytes}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b110 {
		return Instruction{.ORA, dest, source, 1, bytes}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b111 {
		return Instruction{.CMP, dest, source, 1, bytes}
	}

	switch opcode {
		case 0xc6: return Instruction{.ADI, dest, source, 2, bytes}
		case 0xce: return Instruction{.ACI, dest, source, 2, bytes}
		case 0xd6: return Instruction{.SUI, dest, source, 2, bytes}
		case 0xde: return Instruction{.SBI, dest, source, 2, bytes}
		case 0xe6: return Instruction{.ANI, dest, source, 2, bytes}
		case 0xee: return Instruction{.XRI, dest, source, 2, bytes}
		case 0xf6: return Instruction{.ORI, dest, source, 2, bytes}
		case 0xfe: return Instruction{.CPI, dest, source, 2, bytes}
		case 0x07: return Instruction{.RLC, dest, source, 1, bytes}
		case 0x0f: return Instruction{.RRC, dest, source, 1, bytes}
		case 0x17: return Instruction{.RAL, dest, source, 1, bytes}
		case 0x1f: return Instruction{.RAR, dest, source, 1, bytes}
		
		case 0xc3: return Instruction{.JMP, dest, source, 3, bytes}
		case 0xda: return Instruction{.JC, dest, source, 3, bytes}
		case 0xd2: return Instruction{.JNC, dest, source, 3, bytes}
		case 0xca: return Instruction{.JZ, dest, source, 3, bytes}
		case 0xc2: return Instruction{.JNZ, dest, source, 3, bytes}
		case 0xfa: return Instruction{.JM, dest, source, 3, bytes}
		case 0xf2: return Instruction{.JP, dest, source, 3, bytes}
		case 0xea: return Instruction{.JPE, dest, source, 3, bytes}
		case 0xe2: return Instruction{.JPO, dest, source, 3, bytes}
		
		case 0xcd: return Instruction{.CALL, dest, source, 3, bytes}
		case 0xdc: return Instruction{.CC, dest, source, 3, bytes}
		case 0xd4: return Instruction{.CNC, dest, source, 3, bytes}
		case 0xcc: return Instruction{.CZ, dest, source, 3, bytes}
		case 0xc4: return Instruction{.CNZ, dest, source, 3, bytes}
		case 0xfc: return Instruction{.CM, dest, source, 3, bytes}
		case 0xf4: return Instruction{.CP, dest, source, 3, bytes}
		case 0xec: return Instruction{.CPE, dest, source, 3, bytes}
		case 0xe4: return Instruction{.CPO, dest, source, 3, bytes}

		case 0xc9: return Instruction{.RET, dest, source, 1, bytes}
		case 0xd8: return Instruction{.RC, dest, source, 1, bytes}
		case 0xd0: return Instruction{.RNC, dest, source, 1, bytes}
		case 0xc8: return Instruction{.RZ, dest, source, 1, bytes}
		case 0xc0: return Instruction{.RNZ, dest, source, 1, bytes}
		case 0xf8: return Instruction{.RM, dest, source, 1, bytes}
		case 0xf0: return Instruction{.RP, dest, source, 1, bytes}
		case 0xe8: return Instruction{.RPE, dest, source, 1, bytes}
		case 0xe0: return Instruction{.RPO, dest, source, 1, bytes}
		
		case 0xdb: return Instruction{.IN, dest, source, 2, bytes}
		case 0xd3: return Instruction{.OUT, dest, source, 2, bytes}
		
		case 0x32: return Instruction{.STA, dest, source, 3, bytes}
		case 0x3a: return Instruction{.LDA, dest, source, 3, bytes}
		
		case 0xeb: return Instruction{.XCHG, dest, source, 1, bytes}
		case 0xe3: return Instruction{.XTHL, dest, source, 1, bytes}
		case 0xf9: return Instruction{.SPHL, dest, source, 1, bytes}
		case 0xe9: return Instruction{.PCHL, dest, source, 1, bytes}
		
		case 0x02: return Instruction{.STAX, .B, source, 1, bytes}
		case 0x12: return Instruction{.STAX, .D, source, 1, bytes}
		case 0x0a: return Instruction{.LDAX, .B, source, 1, bytes}
		case 0x1a: return Instruction{.LDAX, .D, source, 1, bytes}
		
		case 0x2f: return Instruction{.CMA, dest, source, 1, bytes}
		case 0x37: return Instruction{.STC, dest, source, 1, bytes}
		case 0x3f: return Instruction{.CMC, dest, source, 1, bytes}
		case 0x27: return Instruction{.DAA, dest, source, 1, bytes}
		case 0x22: return Instruction{.SHLD, dest, source, 3, bytes}
		case 0x2a: return Instruction{.LHLD, dest, source, 3, bytes}
		case 0xfb: return Instruction{.EI, dest, source, 1, bytes}
		case 0xf3: return Instruction{.DI, dest, source, 1, bytes}
	}

	if opdigits == 0b11 && source == auto_cast 0b111 {
		return Instruction{.RST, dest, source, 1, bytes}
	}

	if opdigits == 0b00 {
		if Register(int(dest)-1) in bdhsp {
			if source == auto_cast 0b001 {
				return Instruction{.DAD, Register(int(dest)-1), source, 1, bytes}
			}
			if source == auto_cast 0b011 {
				return Instruction{.DCX, Register(int(dest)-1), source, 1, bytes}
			}
		}
		if dest in bdhsp {
			if source == auto_cast 0b011 {
				return Instruction{.INX, dest, source, 1, bytes}
			}
		}
	}

	
	if opdigits == 0b11 { // PUSH & POP
		if dest in bdhsp {
			if source == auto_cast 0b101 {
				return Instruction{.PUSH, dest, source, 1, bytes}
			}
			if source == auto_cast 0b001 {
				return Instruction{.POP, dest, source, 1, bytes}
			}
		}
	}

	if opdigits == 0b00 && source == auto_cast 0b001 {
		if dest in bdhsp {
			return Instruction{.LXI, dest, source, 3, bytes}
		}
	}
	
	fmt.panicf("instruction not implemented: 0x%02x\n", opcode)	
}

print_8080_instruction :: proc(using instruction: Instruction) {
	// TODO: improve this
	fmt.printf("%04x %s\n", regs.PC, mnemonic)
}

Instruction_Mnemonic :: enum {
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
	JMP,
	JC,
	JNC,
	JZ,
	JNZ,
	JP,
	JM,
	JPE,
	JPO,
	CALL,
	CC,
	CNC,
	CZ,
	CNZ,
	CP,
	CM,
	CPE,
	CPO,
	RET,
	RC,
	RNC,
	RZ,
	RNZ,
	RP,
	RM,
	RPE,
	RPO,
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
	mnemonic: Instruction_Mnemonic,
	dest: Register,
	source: Register,
	// TODO cycles: int,
	size: u16,
	bytes: [2]byte,
}

Register :: enum {
	A = 0b111,
	B = 0b000,
	C = 0b001,
	D = 0b010,
	E = 0b011,
	H = 0b100,
	L = 0b101,
	NULL = 0b110, // NOTE: this is not a register
}

regs : struct {
	A: [2]byte,
	B, C, D, E, H, L: byte,
	SP: u16,
	PC: u16,
}

Flag :: enum {
	Z, 
	S, 
	P, 
	CY, 
	AC,
}

flags: bit_set[Flag]

memory: [65_536]byte
