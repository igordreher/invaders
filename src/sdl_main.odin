//+ignore
package main

import "core:fmt"
import "core:os"
import "core:testing"
import "core:mem"
import "core:time"
import sdl "vendor:sdl2"
import "vendor:sdl2/mixer"


main :: proc() {
	when PROFILE
	{
		ctx, _ = spall.context_create("prof.spall")
		buf_backing := make([]byte, spall.BUFFER_DEFAULT_SIZE)
		buf, _ = spall.buffer_create(buf_backing)
	}
	defer when PROFILE {
		spall.buffer_destroy(&ctx, &buf)
		spall.context_destroy(&ctx)
	}

	rom := #load("../invaders_rom")
	cpu := i8080_init(rom, port_in, port_out)
	vram := cpu.memory[0x2400:0x4000]

	sdl.Init({.VIDEO, .AUDIO})
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

	// mixer.OpenAudio(44100, sdl.AUDIO_S16LSB, 1, 4096)
	mixer.OpenAudio(11025, sdl.AUDIO_U8, 1, 4096/4)
	sdl_assert_err()

	mixer.Volume(-1, 20)
	load_chunk :: proc($file: string) -> ^mixer.Chunk {
		mem := #load(file)
		return mixer.QuickLoad_WAV(&mem[0])
	}
	audio_chunks[0] = load_chunk("../sound/0.wav")
	audio_chunks[1] = load_chunk("../sound/1.wav")
	audio_chunks[2] = load_chunk("../sound/2.wav")
	audio_chunks[3] = load_chunk("../sound/3.wav")
	audio_chunks[4] = load_chunk("../sound/4.wav")
	audio_chunks[5] = load_chunk("../sound/5.wav")
	audio_chunks[6] = load_chunk("../sound/6.wav")
	audio_chunks[7] = load_chunk("../sound/7.wav")
	audio_chunks[8] = load_chunk("../sound/8.wav")

	texture := sdl.CreateTexture(renderer, auto_cast sdl.PixelFormatEnum.RGB888, sdl.TextureAccess.STREAMING, WIDTH, HEIGHT)
	sdl_assert_err()

	cpu_speed := 2e+6
	refresh_rate :: 60
	cycles_per_interrupt := cpu_speed / refresh_rate / 2
	next_interrupt := cycles_per_interrupt
	sec_per_frame := time.Duration(1)*time.Second/refresh_rate
	last_refresh := time.now()
	game_loop: for cpu.regs.PC < auto_cast len(rom) {
		profile_scope("main_loop")
		event: sdl.Event
		for sdl.PollEvent(&event) {
			#partial switch event.type {
				case .QUIT: break game_loop
				case .KEYDOWN, .KEYUP:
				{
					#partial switch event.key.keysym.sym {
						case .RETURN: toggle_bit(&cpu.ports[1], 0, event.key.state == 1)
						case .LEFT: toggle_bit(&cpu.ports[1], 5, event.key.state == 1)
						case .RIGHT: toggle_bit(&cpu.ports[1], 6, event.key.state == 1)
						case .SPACE: toggle_bit(&cpu.ports[1], 4, event.key.state == 1)
						case .NUM1: toggle_bit(&cpu.ports[1], 2, event.key.state == 1)
					}
				}
			}
		}

		if cpu.interrupt_enabled && cpu.cycle_count > next_interrupt {
            profile_scope("interrupt")
            if which_interrupt == 2 {
                profile_scope("refresh")
                 render_vram(vram, renderer, texture)
			     d := time.since(last_refresh)
			     sleep_time := sec_per_frame - d
			     if sleep_time > 0 {
                     time.sleep(sleep_time)
			     }
			     last_refresh = time.now()
			}
			generate_interrupt(&cpu, which_interrupt)
			which_interrupt = ((which_interrupt + 2) % 2) + 1
			next_interrupt += cycles_per_interrupt
		}

		i8080_next_instruction(&cpu)
	}
}

toggle_bit :: proc(v: ^u8, bit: u8, state: bool) {
	if state {
		v^ |= 1 << bit
	} else {
		v^ &= 0xff ~ (1 << bit)
	}
}
read_bit :: proc(v: u8, bit: u8) -> bool {
	return (v & (1 << bit)) >> bit == 1
}

which_interrupt: u16 = 1

shift0, shift1, shift_offset: u8
port_in :: proc(state: ^i8080_State, port: u8) -> u8 {
    switch(port)
    {
        case 3:
        {
            v := u16(shift1)<<8 | u16(shift0)
            return u8((v >> (8-shift_offset)) & 0xff)
        }
		case: return state.ports[port]
    }
}
port_out :: proc(state: ^i8080_State, port: u8, value: u8) {
    switch(port)
    {
        case 2: shift_offset = value & 0x7
        case 4:
		{
            shift0 = shift1
            shift1 = value
		}
		case 3:
		{
			if read_bit(value, 0) && !read_bit(state.ports[port], 0) {
				ufo_repeat_channel = mixer.PlayChannel(-1, audio_chunks[0], -1)
			} else if !read_bit(value, 0) && read_bit(state.ports[port], 0) {
				mixer.Pause(ufo_repeat_channel)
			}
			for i in 1..<4 {
				if read_bit(value, auto_cast i) && !read_bit(state.ports[port], auto_cast i) {
					mixer.PlayChannel(-1, audio_chunks[i], 0)
					sdl_assert_err()
				}
			}
			state.ports[port] = value
		}
		case 5:
		{
			for i in 0..<5 {
				if read_bit(value, auto_cast i) && !read_bit(state.ports[port], auto_cast i) {
					mixer.PlayChannel(-1, audio_chunks[i+4], 0)
					sdl_assert_err()
				}
			}
			state.ports[port] = value
		}
		case: state.ports[port] = value
    }
}

ufo_repeat_channel: i32
audio_chunks: [9]^mixer.Chunk

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

// pixels := [WIDTH*HEIGHT][BPP]byte{}
pixels := [WIDTH*HEIGHT*BPP]byte{}
BPP :: 4
WIDTH :: 224
HEIGHT :: 256
