package main

import "core:testing"
import "core:fmt"
import "core:os"

@(test)
test_8080 :: proc(^testing.T) {
	file := #load("../cpudiag.bin")
	cpu := i8080_init({})
	using cpu
	copy(memory[0x100:], file)
	file_size := len(file) + 0x100
	
    // Fix the first instruction to be JMP 0x100    
	regs.PC = 0x100

	for regs.PC < auto_cast file_size {
		instruction := decode_8080_instruction(cpu, regs.PC)
		c := print_8080_instruction(cpu, instruction, regs.PC)
		exec_8080_instruction(&cpu, &instruction)
		print_registers(cpu, c)
	}
}

i8080_init_from_filename :: proc(filename: string, port_in := def_port_in, port_out := def_port_out) -> (state: i8080_State, size: u16) {
	// TODO: return errors instead of panic
	h, err := os.open(filename)
	if err != 0 {
		fmt.panicf("failed to open file '%s'\n", filename)
	}
	file_size, _ := os.file_size(h)
	size = u16(file_size)
	if size > max(u16) {
		fmt.panicf("size too long\n")
	}
	_, rerr := os.read(h, state.memory[:])
	if rerr != 0 {
		fmt.printf("failed to read file '%s'\n", filename)
	}
	state.interrupt_delay = -1
	state.port_in = port_in
	state.port_out = port_out
	return state, size
}

i8080_init :: proc(buffer: []byte, port_in := def_port_in, port_out := def_port_out) -> i8080_State {
	// TODO: return errors instead of panic
	state: i8080_State
	assert(len(buffer) <= len(state.memory))
	copy(state.memory[:], buffer)
	state.interrupt_delay = -1
	state.port_in = port_in
	state.port_out = port_out
	return state
}

i8080_next_instruction :: proc(state: ^i8080_State) {
	profile_scope(#procedure)
	instruction := decode_8080_instruction(state^, state.regs.PC)
	// print_8080_instruction(cpu, instruction); fmt.printf("\n")
	exec_8080_instruction(state, &instruction)
}

exec_8080_instruction :: proc(using state: ^i8080_State, using instruction: ^Instruction) {
	profile_scope(#procedure)
	defer cycle_count += cycles
	if interrupt_delay == 0 {
		interrupt_delay -= 1
		interrupt_enabled = true
	}
	if interrupt_delay > 0 do interrupt_delay -= 1
	pc := regs.PC
	regs.PC += size
	opcode := memory[pc]
	dest := cast(Register)(opcode & 0b00_111_000 >> 3)
	source := cast(Register)(opcode & 0b00_000_111)
	pair := Register_Pair(u8(dest) >> 1)
	addr_hl := cast(u16)regs.HL.v
	bytes: [2]byte
	for i in 0..<size-1 {
		bytes[i] = memory[pc+1+i]
	}
	word :=	cast(u16) transmute(u16le)bytes
	addr_pair := cast(u16)regs.p[pair].v
	condition := Condition(dest)

	when CPU_DIAG {
		if pc == 0x689 {
			fmt.println("\nCPU TEST FAILED")
			os.exit(1)
		} else if pc == 0x069B {
			fmt.println("\nCPU OK")
			os.exit(0)
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
			if pair == .SP {
				regs.SP = word
			} else {
				regs.p[pair].H = bytes[1]
				regs.p[pair].L = bytes[0]
			}
		}
		case .LDA:
		{
			regs.A = memory[word]
		}
		case .STA:
		{
			write(state, word, regs.A)
		}
		case .LHLD:
		{
			regs.L = memory[word]
			regs.H = memory[word+1]
		}
		case .SHLD:
		{
			write(state, word, regs.L)
			write(state, word+1, regs.H)
		}
		case .LDAX:
		{
			regs.A = memory[addr_pair]
		}
		case .STAX:
		{
			write(state, addr_pair, regs.A)
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
			set_flags(state, regs.A)
			set_flag(state, .AC, regs.A & 0xf < value & 0xf)
			set_flag(state, .CY, regs.A < value)
		}
		case .ADI:
		{
			regs.A += bytes[0]
			set_flags(state, regs.A)
			set_flag(state, .AC, regs.A & 0xf < bytes[0] & 0xf)
			set_flag(state, .CY, regs.A < bytes[0])
		}
		case .ADC:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			regs.A += value + u8(.CY in flags)
			set_flags(state, regs.A)
			set_flag(state, .AC, regs.A & 0xf < value & 0xf)
			set_flag(state, .CY, regs.A < value)
		}
		case .ACI:
		{
			regs.A += bytes[0] + u8(.CY in flags)
			set_flags(state, regs.A)
			set_flag(state, .AC, regs.A & 0xf < bytes[0] & 0xf)
			set_flag(state, .CY, regs.A < bytes[0])
		}
		case .SUB:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			set_flag(state, .CY, regs.A < value)
			set_flag(state, .AC, regs.A & 0xf < value & 0xf)
			regs.A -= value
			set_flags(state, regs.A)
		}
		case .SUI:
		{
			set_flag(state, .CY, regs.A < bytes[0])
			set_flag(state, .AC, regs.A & 0xf < bytes[0] & 0xf)
			regs.A -= bytes[0]
			set_flags(state, regs.A)
		}
		case .SBB:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			a := regs.A
			regs.A = regs.A - value - u8(.CY in flags)
			set_flags(state, regs.A)
			set_flag(state, .CY, a < regs.A)
			set_flag(state, .AC, a & 0xf < regs.A & 0xf)
		}
		case .SBI:
		{
			a := regs.A
			regs.A = regs.A - bytes[0] - u8(.CY in flags)
			set_flags(state, regs.A)
			set_flag(state, .CY, a < regs.A)
			set_flag(state, .AC, a & 0xf < regs.A & 0xf)
		}
		case .INR, .DCR:
		{
			target := dest == .NULL ? &memory[addr_hl] : &regs.r[dest]
			target^ = mnemonic == .INR ? target^ + 1 : target^ - 1
			set_flag(state, .AC, target^ & 0xf < (target^+1) & 0xf)
			set_flags(state, target^)
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
			value := pair == .SP ? cast(u16be)regs.SP : regs.p[pair].v
			regs.HL.v += value
			set_flag(state, .CY, regs.HL.v < value)
		}
		case .DAA:
		{
			a := regs.A
			if .AC in flags || regs.A & 0xf > 9 {
				regs.A += 6
			}
			if .CY in flags || regs.A & 0xf0 > 0x90 {
				regs.A += 0x60
			}
			set_flags(state, regs.A)
			set_flag(state, .AC, regs.A & 0xf < a & 0xf)
			set_flag(state, .CY, regs.A < a)
			// if regs.A & 0xf > 9 {
			// 	regs.A += 6	
			// 	set_flag(state, .CY, regs.A < 6)
			// } 
			// if regs.A & 0xf0 > 0x90 || .CY in flags {
			// 	regs.A = regs.A + 0x60
			// 	set_flag(state, .CY, regs.A < 0x60)
			// }
			// set_flags(state, regs.A)
		}
		// Logical group
		case .ANA, .ANI:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			if mnemonic == .ANI do value = bytes[0]
			regs.A = regs.A & value
			set_flags(state, regs.A)
			set_flag(state, .AC, regs.A & 0xf < value & 0xf)
			set_flag(state, .CY, false)
		}
		case .XRA, .XRI:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			if mnemonic == .XRI do value = bytes[0]
			regs.A = regs.A ~ value
			set_flags(state, regs.A)
			set_flag(state, .AC, false)
			set_flag(state, .CY, false)
		}
		case .ORA, .ORI:
		{
			value := source == .NULL ? memory[addr_hl] : regs.r[source]
			if mnemonic == .ORI do value = bytes[0]
			regs.A = regs.A | value
			set_flags(state, regs.A)
			set_flag(state, .AC, false)
			set_flag(state, .CY, false)
		}
		case .CMP, .CPI:
		{
			value: u8
			if mnemonic == .CPI do value = bytes[0]
			else if source == .NULL do value = memory[addr_hl]
			else do value = regs.r[source]
			set_flag(state, .CY, regs.A < value)
			set_flag(state, .AC, regs.A & 0xf < value & 0xf)
			result := regs.A - value
			set_flags(state, result)
		}
		case .RLC:
		{
			cy := regs.A >> 7
			regs.A = regs.A << 1 | cy
			set_flag(state, .CY, cy == 1)
		}
		case .RRC:
		{
			cy := regs.A & 1
			regs.A = regs.A >> 1 | (cy << 7)
			set_flag(state, .CY, cy == 1)
		}
		case .RAL:
		{
			cy := regs.A >> 7 == 1
			regs.A = regs.A << 1 | u8(.CY in flags)
			set_flag(state, .CY, cy)
		}
		case .RAR:
		{
			cy := regs.A & 1 == 1
			regs.A = regs.A >> 1 | (u8(.CY in flags) << 7)
			set_flag(state, .CY, cy)
		}
		case .CMA:
		{
			regs.A ~= 0xff
		}
		case .CMC:
		{
			set_flag(state, .CY, !(.CY in flags))
		}
		case .STC:
		{
			set_flag(state, .CY, true)
		}
		// Branch group
		case .JMP:
		{
			regs.PC = word
		}
		case .JZ, .JNZ, .JC, .JNC, .JPE, .JPO, .JP, .JM:
		{
			if (Flag(u8(condition) & 0b110) in flags) == bool(u8(condition) & 0b001) {
				regs.PC = word
			}
		}
		case .CALL:
		{
			push(state, regs.PC)
			regs.PC = word
		}
		case .CZ, .CNZ, .CC, .CNC, .CPE, .CPO, .CP, .CM:
		{
			if (Flag(u8(condition) & 0b110) in flags) == bool(u8(condition) & 0b001) {
				cycles = 5
				push(state, regs.PC)
				regs.PC = word
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
				cycles = 3
				regs.PC = auto_cast (cast(^u16le)&memory[regs.SP])^
				regs.SP += 2
			}
		}
		case .RST:
		{
			push(state, regs.PC)
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
				push(state, regs.A, status_word)
			} else {
				push(state, regs.p[pair])
			}
		}
		case .POP:
		{
			if pair == .SP {
				status := memory[regs.SP]
				set_flag(state, .CY, status & 1 == 1)
				set_flag(state, .P, (status >> 2) & 1 == 1)
				set_flag(state, .Z, (status >> 6) & 1 == 1)
				set_flag(state, .S, (status >> 7) & 1 == 1)
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
			write(state, regs.SP, pair.L)
			write(state, regs.SP+1, pair.H)
		}
		case .SPHL:
		{
			regs.SP = cast(u16)regs.HL.v
		}
		case .IN:
		{
			regs.A = port_in(state, bytes[0])
		}
		case .OUT: 
		{
			port_out(state, bytes[0], regs.A)
		}
		case .DI:
		{
			interrupt_enabled = false
		}
		case .EI:
		{
			interrupt_delay = 1
		}
		case .HLT:
		{
			fmt.println("HALT")
			os.exit(0)
		}
	}
}

generate_interrupt :: proc(cpu: ^i8080_State, num: u16) {
	push(cpu, cpu.regs.PC)
	cpu.regs.PC = 8 * num
	cpu.interrupt_enabled = false
}

push :: proc{push_hl, push_u16, push_pair}
push_pair :: proc(using state: ^i8080_State, pair: Pair, location := #caller_location) {
	push_hl(state, pair.H, pair.L, location)
}
push_u16 :: proc(using state: ^i8080_State, v: u16, location := #caller_location) {
	v := cast(u16le)v
	high := u8((v & 0xff00)>>8)
	low := u8(v & 0xff)
	push_hl(state, high, low, location)
}
push_hl :: proc(using state: ^i8080_State, high, low: u8, location := #caller_location) {
	write(state, regs.SP-1, high, location)
	write(state, regs.SP-2, low, location)
	regs.SP -= 2
}

write :: proc(using state: ^i8080_State, addr: u16, value: u8, location := #caller_location) {
	if addr < 0 || addr >= len(memory) {
		fmt.printf("Out of bounds write to memory %04x\n", addr)
		os.exit(1)
	}
		
	if addr < 0x2000 && !CPU_DIAG {
		fmt.printf("\n[%s:%s][%d:%d] Writing to ROM not allowed %04x\n", location.file_path, location.procedure, location.line, location.column, addr)
		fmt.printf("%04x\n", regs.PC)
		return
	} else if addr >= 0x4000 && !CPU_DIAG {
		fmt.printf("\n[%s:%s][%d:%d] Writing out of Space Invaders RAM not allowed %x\n", location.file_path, location.procedure, location.line, location.column, addr)
		fmt.printf("%04x\n", regs.PC)
		return
    }
	
	memory[addr] = value
}

decode_8080_instruction :: proc(using state: i8080_State, pc: u16) -> Instruction {
	profile_scope(#procedure)
	opcode := memory[pc]
	dest := cast(Register)(opcode & 0b00_111_000 >> 3)
	source := cast(Register)(opcode & 0b00_000_111)
	pair := Register_Pair(u8(dest) >> 1)
	opdigits := opcode >> 6
	
	if opdigits == 0b01 { // MOV & HLT
		if dest == .NULL && source == .NULL {
			return Instruction{.HLT, 1, 1}
		}
		if source == .NULL || dest == .NULL {
			return Instruction{.MOV, 1, 2}
		}
		return Instruction{.MOV, 1, 1}
	}
	
	if opdigits == 0b00 && source == .NULL {
		cycles := dest == .NULL ? 3 : 2
		return Instruction{.MVI, 2, cycles}
	}
	
	if opdigits == 0b00 { // INR & DCR
		cycles := dest == .NULL ? 3 : 1
		if source == auto_cast 0b100 {
			return Instruction{.INR, 1, cycles}
		} else if source == auto_cast 0b101 {
			return Instruction{.DCR, 1, cycles}
		}
	}

	if opdigits == 0b10 { // ADD & ADC
		cycles := source == .NULL ? 2 : 1
		if dest == auto_cast 0b000 {
			return Instruction{.ADD, 1, cycles}
		} else if dest == auto_cast 0b001 {
			return Instruction{.ADC, 1, cycles}
		}
	}

	if opdigits == 0b10 && dest == auto_cast 0b010 {
		cycles := source == .NULL ? 2 : 1
		return Instruction{.SUB, 1, cycles}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b011 {
		cycles := source == .NULL ? 2 : 1
		return Instruction{.SBB, 1, cycles}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b100 {
		cycles := source == .NULL ? 2 : 1
		return Instruction{.ANA, 1, cycles}
	}

	if opdigits == 0b10 && dest == auto_cast 0b101 {
		cycles := source == .NULL ? 2 : 1
		return Instruction{.XRA, 1, cycles}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b110 {
		cycles := source == .NULL ? 2 : 1
		return Instruction{.ORA, 1, cycles}
	}
	
	if opdigits == 0b10 && dest == auto_cast 0b111 {
		cycles := source == .NULL ? 2 : 1
		return Instruction{.CMP, 1, cycles}
	}

	if opdigits == 0b11 && source == auto_cast 0b010 { // Jcondition
		mnemonic := Mnemonic(u8(Mnemonic.JMP)+1 + u8(dest))
		return Instruction{mnemonic, 3, 3}
	}
	if opdigits == 0b11 && source == auto_cast 0b000 { // Rcondition
		mnemonic := Mnemonic(u8(Mnemonic.RET)+1 + u8(dest))
		return Instruction{mnemonic, 1, 1} // cycles = 1/3
	}
	if opdigits == 0b11 && source == auto_cast 0b100 { // Ccondition
		mnemonic := Mnemonic(u8(Mnemonic.CALL)+1 + u8(dest))
		return Instruction{mnemonic, 3, 3} // cycles = 3/5
	}
	
	if opdigits == 0b11 && source == auto_cast 0b111 {
		return Instruction{.RST, 1, 3}
	}

	if opdigits == 0b00 {
		if u8(dest) & 1 == 1 {
			if source == auto_cast 0b001 {
				return Instruction{.DAD, 1, 3}
			}
			if source == auto_cast 0b011 {
				return Instruction{.DCX, 1, 1}
			}
		} else {
			if source == auto_cast 0b011 {
				return Instruction{.INX, 1, 1}
			}
		}
	}

	
	if opdigits == 0b11 && u8(dest) & 1 == 0 { // PUSH & POP
		if source == auto_cast 0b101 {
			return Instruction{.PUSH, 1, 3}
		}
		if source == auto_cast 0b001 {
			return Instruction{.POP, 1, 3}
		}
	}

	if opdigits == 0b00 && source == auto_cast 0b001 {
		if u8(dest) & 1 == 0 {
			return Instruction{.LXI, 3, 3}
		}
	}

	switch opcode {
		case 0xc6: return Instruction{.ADI, 2, 2}
		case 0xce: return Instruction{.ACI, 2, 2}
		case 0xd6: return Instruction{.SUI, 2, 2}
		case 0xde: return Instruction{.SBI, 2, 2}
		case 0xe6: return Instruction{.ANI, 2, 2}
		case 0xee: return Instruction{.XRI, 2, 2}
		case 0xf6: return Instruction{.ORI, 2, 2}
		case 0xfe: return Instruction{.CPI, 2, 2}
		case 0x07: return Instruction{.RLC, 1, 1}
		case 0x0f: return Instruction{.RRC, 1, 1}
		case 0x17: return Instruction{.RAL, 1, 1}
		case 0x1f: return Instruction{.RAR, 1, 1}
		
		case 0xc3: return Instruction{.JMP, 3, 3}
		case 0xcd: return Instruction{.CALL, 3, 5}
		case 0xc9: return Instruction{.RET, 1, 3}
		
		case 0xdb: return Instruction{.IN, 2, 3}
		case 0xd3: return Instruction{.OUT, 2, 3}
		
		case 0x32: return Instruction{.STA, 3, 4}
		case 0x3a: return Instruction{.LDA, 3, 4}
		
		case 0xeb: return Instruction{.XCHG, 1, 1}
		case 0xe3: return Instruction{.XTHL, 1, 5}
		case 0xf9: return Instruction{.SPHL, 1, 1}
		case 0xe9: return Instruction{.PCHL, 1, 1}
		
		case 0x02, 0x12: return Instruction{.STAX, 1, 2}
		case 0x0a, 0x1a: return Instruction{.LDAX, 1, 2}
		
		case 0x2f: return Instruction{.CMA, 1, 1}
		case 0x37: return Instruction{.STC, 1, 1}
		case 0x3f: return Instruction{.CMC, 1, 1}
		case 0x27: return Instruction{.DAA, 1, 1}
		case 0x22: return Instruction{.SHLD, 3, 5}
		case 0x2a: return Instruction{.LHLD, 3, 5}
		case 0xfb: return Instruction{.EI, 1, 1}
		case 0xf3: return Instruction{.DI, 1, 1}

		case: 
			return Instruction{.NOP, 1, 1}
	}
}

set_flags :: proc(using state: ^i8080_State, v: u8) {
	set_flag(state, .Z, v == 0)
	set_flag(state, .S, v >> 7 == 1)
	count := 0
	for i := uint(0); i < 8; i += 1 {
		if (v >> i) & 1 == 1 {
			count += 1 // TODO: there is probably a better way to check of parity
		}
	}
	set_flag(state, .P, count & 1 == 0)
}

set_flag :: proc(using state: ^i8080_State, flag: Flag, v: bool) {
	if v {
		flags += {flag}
	} else {
		flags -= {flag}
	}
}

print_8080 :: proc(state: i8080_State, addr: u16) -> int {
	instruction := decode_8080_instruction(state, addr)
	return print_8080_instruction(state, instruction, addr)
}
	
print_8080_instruction :: proc(using state: i8080_State, using instruction: Instruction, pc: u16) -> int {
	opcode := memory[pc]
	dest := cast(Register)(opcode & 0b00_111_000 >> 3)
	source := cast(Register)(opcode & 0b00_000_111)
	pair := Register_Pair(u8(dest) >> 1)
	c := fmt.printf("%04x %02x %s", pc, opcode, mnemonic)
	#partial switch mnemonic {
		case .ADD, .SBB, .ADC, .SUB, .CMP, .ORA, .XRA, .ANA: 
		{
			c += print_reg(source)
		}
		case .INR, .DCR:
		{
			c += print_reg(dest)
		}
		case .LDAX, .LXI, .STAX, .INX, .DCX, .DAD, .PUSH, .POP:
		{
			c += print_pair(auto_cast (u8(dest) >> 1))
		}
		case .MOV:
		{
			c += print_reg(dest)
			c += fmt.printf(",")
			c += print_reg(source)
		}
		case .MVI: 
		{
			c += print_reg(dest)
			c += fmt.printf(",")
		}
	}
	print_reg :: proc(reg: Register) -> int {
		if reg != .NULL {
			return fmt.printf(" %v", reg)
		} else {
			return fmt.printf(" M")
		}
	}
	print_pair :: proc(pair: Register_Pair) -> int {
		return fmt.printf(" %v", pair)
	}
	if size == 2 {
		c += fmt.printf(" %02x", memory[pc+1])
	} else if size == 3 {
		c += fmt.printf(" %04x", transmute(u16le)[2]byte{memory[pc+1], memory[pc+2]})
	}
	return c
}

print_registers :: proc(using state: i8080_State, prev_length: int) {
	for _ in 0..<50-prev_length {
		fmt.printf(" ")
	}
	fmt.printf("A=%02x, B=%02x, C=%02x, D=%02x, E=%02x, H=%02x, L=%02x, CY=%d, P=%d, S=%d, Z=%d, SP=%04x", regs.A, regs.B, regs.C, regs.D, regs.E, regs.H, regs.L, u8(.CY in flags), u8(.P in flags), u8(.S in flags), u8(.Z in flags), regs.SP)
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
	size: u16,
	cycles: int,
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
	AC, // NOTE: this flag is not used in space invaders
}

Registers :: struct {
	using _: struct #raw_union {
		using _: struct {B, C, D, E, H, L, _, A: byte},
		r: [Register]byte,
		using _: struct {BC, DE, HL: Pair},
		p: [Register_Pair]Pair,
	},
	PC, SP: u16,
}

i8080_State :: struct {
	regs: Registers,
	interrupt_enabled: bool,
	interrupt_delay: i8,
	flags: bit_set[Flag],
	memory: [max(u16)]byte,
	cycle_count: int,
	ports: [16]u8,
	port_in: proc(state: ^i8080_State, port: u8) -> u8,
	port_out: proc(state: ^i8080_State, port: u8, value: u8),
}

def_port_in :: proc(state: ^i8080_State, port: u8) -> u8 {
	return state.ports[port]
}
def_port_out :: proc(state: ^i8080_State, port, value: u8) {
	state.ports[port] = value
}

