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
	
	for {
		instruction := decode_8080_instruction(data)
		fmt.printf("%04d %s\n", regs.PC, instruction.mnemonic)
		regs.PC += instruction.size
	}
}

decode_8080_instruction :: proc(buffer: []byte) -> Instruction {
	opcode := buffer[regs.PC]
	
	if opcode == 0x0 {
		return Instruction{.NOP, nil, nil, 1, {}}
	}

	if opcode & 0b11000000 == 0b01_000000 { // MOV & HLT
		dest := cast(Register)(opcode & 0b00_111_000 >> 3)
		source := cast(Register)(opcode & 0b00_000_111)
		if dest == .NULL && source == .NULL {
			return Instruction{.HLT, dest, source, 1, {}}
		}
		return Instruction{.MOV, dest, source, 1, {}}
	}

	
	fmt.panicf("instruction not implemented: 0x%02x\n", opcode)	
}

Instruction_Mnemonic :: enum {
	NOP,
	MOV,
	HLT,
	MVI,
	// ...etc.
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
