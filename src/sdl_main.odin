//+build linux
package main

import "core:fmt"
import "core:os"
import "core:mem"
import sdl "vendor:sdl2"

pixels := [WIDTH*HEIGHT*BPP]byte{}
BPP :: 4
WIDTH :: 224
HEIGHT :: 256
SCALE :: 2

main :: proc() {
	when PROFILE do profile_init()
	defer when PROFILE do profile_end()

	rom := #load("../invaders_rom")
	cpu := i8080_init(rom, port_in, port_out)
	vram := cpu.memory[0x2400:0x4000]

	sdl.Init({.VIDEO})
	window := sdl.CreateWindow("Space Invaders", sdl.WINDOWPOS_CENTERED, sdl.WINDOWPOS_CENTERED, WIDTH * SCALE, HEIGHT * SCALE, {})
	if window == nil {
		fmt.eprintln("failed to create SDL window")
		os.exit(1)
	}
	renderer := sdl.CreateRenderer(window, -1, {.ACCELERATED, .PRESENTVSYNC})
	if renderer == nil {
		fmt.eprintln("failed to create SDL renderer")
		os.exit(1)
	}

	texture := sdl.CreateTexture(renderer, auto_cast sdl.PixelFormatEnum.RGB888, sdl.TextureAccess.STREAMING, WIDTH, HEIGHT)
	sdl_assert_err()

	sound_engine: Sound_Engine
	init_sounds(&sound_engine)

	which_interrupt: u16 = 1
	cpu_speed := 2e+6
	refresh_rate :: 60
	cycles_per_interrupt := cpu_speed / refresh_rate / 2
	next_interrupt := cycles_per_interrupt
	game_loop: for cpu.regs.PC < auto_cast len(rom) {
		profile_scope("main_loop")
		event: sdl.Event
		for sdl.PollEvent(&event) {
			#partial switch event.type {
				case .QUIT: break game_loop
				case .KEYDOWN, .KEYUP:
				{
					state := event.key.state == 1
					#partial switch event.key.keysym.sym {
						case .RETURN: toggle_bit(&cpu.ports[1], 0, state)
						case .LEFT:   toggle_bit(&cpu.ports[1], 5, state)
						case .RIGHT:  toggle_bit(&cpu.ports[1], 6, state)
						case .SPACE:  toggle_bit(&cpu.ports[1], 4, state)
						case .NUM1:   toggle_bit(&cpu.ports[1], 2, state)
					}
				}
			}
		}

		for {
			i8080_next_instruction(&cpu)
			if cpu.interrupt_enabled && cpu.cycle_count > next_interrupt {
				generate_interrupt(&cpu, which_interrupt)
				next_interrupt += cycles_per_interrupt
				which_interrupt = ((which_interrupt + 2) % 2) + 1
				if which_interrupt == 1 do break
			}
		}
		render_vram(vram, renderer, texture)
	}
}


render_vram :: proc(vram: []byte, renderer: ^sdl.Renderer, texture: ^sdl.Texture) {
	profile_scope(#procedure)
	sdl.SetRenderDrawColor(renderer, 255, 255, 0, 255)
	sdl.RenderClear(renderer)

    {
    profile_scope("copy loop")
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
	}
	sdl.UpdateTexture(texture, nil, &pixels[0], auto_cast WIDTH*BPP)
	sdl_assert_err()
	sdl.RenderCopy(renderer, texture, nil, nil)
	sdl.RenderPresent(renderer)
}

sdl_assert_err :: proc(location := #caller_location) {
	when !ODIN_DISABLE_ASSERT {
		err := sdl.GetErrorString()
		if err != "" {
			fmt.eprintf("%d: %s", location.line, err)
			os.exit(1)
		}
	}
}
