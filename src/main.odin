package main

import "core:fmt"
import "core:os"
import "core:testing"
import "core:mem"
import "core:time"
import sdl "vendor:sdl2"
import "core:prof/spall"

CPU_DIAG :: #config(CPU_DIAG, ODIN_TEST)

buf: spall.Buffer
ctx: spall.Context

main :: proc() {
	ctx, _ = spall.context_create("prof.spall")
	defer spall.context_destroy(&ctx)
	buf_backing := make([]byte, spall.BUFFER_DEFAULT_SIZE)
	buf, _ = spall.buffer_create(buf_backing)
	defer spall.buffer_destroy(&ctx, &buf)
	
	when CPU_DIAG {
		test_8080(nil)
		return
	}

	sdl.Init({.VIDEO})
	window := sdl.CreateWindow("Space Invaders", sdl.WINDOWPOS_CENTERED, sdl.WINDOWPOS_CENTERED, WIDTH, HEIGHT, {})
	if window == nil {
		fmt.eprintln("failed to create SDL window")
		os.exit(1)
	}
	renderer := sdl.CreateRenderer(window, -1, {.ACCELERATED})
	if renderer == nil {
		fmt.eprintln("failed to create SDL renderer")
		os.exit(1)
	}

	if len(os.args) < 2 {
		fmt.panicf("not enough arguments\n")
	}
	
	cpu, size := i8080_init_from_filename(os.args[1])
	vram := cpu.memory[0x2400:0x4000]
	
	texture := sdl.CreateTexture(renderer, auto_cast sdl.PixelFormatEnum.RGB888, sdl.TextureAccess.STREAMING, WIDTH, HEIGHT)
	sdl_assert_err()
		
	cpu_speed := 2e+6
	refresh_rate := 60
	cycles_per_interrupt := cpu_speed / refresh_rate / 2
	next_interrupt := cycles_per_interrupt
	game_loop: for cpu.regs.PC < size {
		spall.SCOPED_EVENT(&ctx, &buf, "main_loop")
		event: sdl.Event
		spall._buffer_begin(&ctx, &buf, "poll_events")
		for sdl.PollEvent(&event) {
			#partial switch event.type {
				case .QUIT: break game_loop
				case .KEYDOWN:
					#partial switch event.key.keysym.sym {
						case .LEFT: cpu.ports[1] |= 0x20 // 0b0010_0000
						case .RIGHT: cpu.ports[1] |= 0x40
						case .RETURN: cpu.ports[1] |= 0x04
					}
				case .KEYUP:
					#partial switch event.key.keysym.sym {
						case .LEFT: cpu.ports[1] &= 0xDF
						case .RIGHT: cpu.ports[1] &= 0xBF
						case .RETURN: cpu.ports[1] &= 0b1111_1011
					}
			}
		}
		spall._buffer_end(&ctx, &buf)
		
		// if cpu.interrupt_enabled && time.now()._nsec > next_interrupt {
		if cpu.interrupt_enabled && cpu.cycle_count > next_interrupt {
			spall.SCOPED_EVENT(&ctx, &buf, "generate_interrupt")
			// fmt.printf("%v %v\n", time.now()._nsec, next_interrupt)
				
			render_vram(vram, renderer, texture)
			generate_interrupt(&cpu, which_interrupt)
			which_interrupt = ((which_interrupt + 2) % 2) + 1
			// next_interrupt = time.now()._nsec + 8e+6
			next_interrupt += cycles_per_interrupt
		}
			
		i8080_next_instruction(&cpu)
		if cpu.regs.SP != 0 && cpu.regs.SP <= 0x2300 {
			fmt.printf("stack pointer is getting low %04x", cpu.regs.SP)
		}
	}
}

which_interrupt: u16 = 1

render_vram :: proc(vram: []byte, renderer: ^sdl.Renderer, texture: ^sdl.Texture) {
	spall.SCOPED_EVENT(&ctx, &buf, #procedure)
	sdl.SetRenderDrawColor(renderer, 255, 255, 0, 255)
	sdl.RenderClear(renderer)

	for i in 0..<WIDTH {
		for j := 0; j < HEIGHT; j+= 8 {
			offset := (HEIGHT-1-j)*(WIDTH*BPP) + (i*BPP)
			pix := vram[(i*(HEIGHT/8)) + j/8]
			p1 := (^u32)(&pixels[offset])
			for p := 0; p < 8; p+=1 {
				if 0 != (pix & (1<<u8(p))) do p1^ = 0xffffffff
				else do p1^ = 0
				p1 = mem.ptr_offset(p1, -WIDTH)
			}
		}
	}
	sdl.UpdateTexture(texture, nil, &pixels[0], auto_cast WIDTH*BPP)
	sdl_assert_err()
	sdl.RenderCopy(renderer, texture, nil, nil)
	sdl.RenderPresent(renderer)
}
	
sdl_assert_err :: proc(location := #caller_location) {
	err := sdl.GetErrorString()
	if err != "" {
		fmt.eprintf("%d: %s", location.line, err)
		os.exit(1)
	}
}

// pixels := [WIDTH*HEIGHT][BPP]byte{}
pixels := [WIDTH*HEIGHT*BPP]byte{}
BPP :: 4
WIDTH :: 224
HEIGHT :: 256
