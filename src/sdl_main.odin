//+build linux
package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:reflect"
import sdl "vendor:sdl2"
import gl "vendor:OpenGL"

pixels := [WIDTH*HEIGHT*BPP]byte{}
BPP :: 4
WIDTH :: 224
HEIGHT :: 256
SCALE :: 2

main :: proc() {
	when PROFILE do profile_init()
	defer when PROFILE do profile_end()

	sound_engine: Sound_Engine
	init_sounds(&sound_engine)

	rom := #load("../invaders_rom")
	cpu := i8080_init(rom, port_in, port_out)
	vram := cpu.memory[0x2400:0x4000]

	sdl.Init({.VIDEO})
	window := sdl.CreateWindow("Space Invaders", sdl.WINDOWPOS_CENTERED, sdl.WINDOWPOS_CENTERED, WIDTH * SCALE, HEIGHT * SCALE, {.OPENGL, .RESIZABLE})
	if window == nil {
		fmt.eprintln("failed to create SDL window")
		os.exit(1)
	}

	sdl.GL_SetAttribute(.CONTEXT_MAJOR_VERSION, 3)
	sdl.GL_SetAttribute(.CONTEXT_MINOR_VERSION, 3)
	gl_context := sdl.GL_CreateContext(window)
	if sdl.GL_MakeCurrent(window, gl_context) != 0 {
		fmt.eprintf("%s\n", sdl.GetErrorString())
		os.exit(1)
	}
	gl.load_up_to(3, 3, sdl.gl_set_proc_address)
	sdl.GL_SetSwapInterval(1)

	shader, ok := load_shader_single_file("shader.glsl")
	assert(ok)

	vao: u32
	{
		vbo, ebo: u32
		gl.GenVertexArrays(1, &vao)
		gl.GenBuffers(1, &vbo)
		gl.GenBuffers(1, &ebo)

		Vertex :: struct { pos: [2]f32, uv: [2]f32 }
		vertices := [4]Vertex {
			{{+1, +1}, {0, 1}},
			{{+1, -1}, {0, 0}},
			{{-1, -1}, {1, 0}},
			{{-1, +1}, {1, 1}},
		}
		indices := [6]u8 { 0, 1, 2, 0, 2, 3 }

		gl.BindVertexArray(vao)
		gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
		gl.BufferData(gl.ARRAY_BUFFER, size_of(vertices), &vertices[0], gl.STATIC_DRAW)
		gl.VertexAttribPointer(0, 2, gl.FLOAT, false, size_of(Vertex), offset_of(Vertex, pos))
		gl.VertexAttribPointer(1, 2, gl.FLOAT, false, size_of(Vertex), offset_of(Vertex, uv))
		gl.EnableVertexAttribArray(0)
		gl.EnableVertexAttribArray(1)

		gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, ebo)
		gl.BufferData(gl.ELEMENT_ARRAY_BUFFER, size_of(indices), &indices[0], gl.STATIC_DRAW)
	}

	texture: u32
	{
		gl.GenTextures(1, &texture)
		gl.BindTexture(gl.TEXTURE_2D, texture)
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP)
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP)
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
		gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RED, HEIGHT/8, WIDTH, 0, gl.RED, gl.UNSIGNED_BYTE, &vram[0])
		gl.GenerateMipmap(gl.TEXTURE_2D)
	}

	screen_color_tex: u32
	{
		screen_colors := init_screen_colors()
		gl.GenTextures(1, &screen_color_tex)
		gl.BindTexture(gl.TEXTURE_1D, screen_color_tex)
		gl.TexParameteri(gl.TEXTURE_1D, gl.TEXTURE_WRAP_S, gl.CLAMP)
		gl.TexParameteri(gl.TEXTURE_1D, gl.TEXTURE_WRAP_T, gl.CLAMP)
		gl.TexParameteri(gl.TEXTURE_1D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
		gl.TexParameteri(gl.TEXTURE_1D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
		gl.TexImage1D(gl.TEXTURE_1D, 0, gl.RGBA, HEIGHT, 0, gl.RGBA, gl.UNSIGNED_BYTE, &screen_colors)
		gl.GenerateMipmap(gl.TEXTURE_1D)
	}

	{	// only need to set once
		gl.UseProgram(shader)
		gl.BindVertexArray(vao)
		gl.BindTexture(gl.TEXTURE_2D, texture)
		gl.BindTexture(gl.TEXTURE_1D, screen_color_tex)
	}

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
						case .RETURN, .NUM_3: toggle_bit(&cpu.ports[1], 0, state)
						case .LEFT, .A:   toggle_bit(&cpu.ports[1], 5, state); toggle_bit(&cpu.ports[2], 5, state)
						case .RIGHT, .D:  toggle_bit(&cpu.ports[1], 6, state); toggle_bit(&cpu.ports[2], 6, state)
						case .SPACE:  toggle_bit(&cpu.ports[1], 4, state); toggle_bit(&cpu.ports[2], 4, state)
						case .NUM1:   toggle_bit(&cpu.ports[1], 2, state)
						case .NUM3:   toggle_bit(&cpu.ports[1], 2, state)
					}
				}
			}
		}

		for {
			i8080_next_instruction(&cpu)
			if cpu.interrupt_enabled && cpu.cycle_count > next_interrupt {
				generate_interrupt(&cpu, which_interrupt)
				next_interrupt += cycles_per_interrupt
				which_interrupt = (which_interrupt % 2) + 1
				if which_interrupt == 1 do break
			}
		}

		gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RED, HEIGHT/8, WIDTH, 0, gl.RED, gl.UNSIGNED_BYTE, &vram[0])
		gl.DrawElements(gl.TRIANGLES, 6, gl.UNSIGNED_BYTE, nil)
		sdl.GL_SwapWindow(window)
	}
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


load_shader_single_file :: proc($PATH: string, location := #caller_location) -> (program: u32, ok: bool) {
	data := string(#load(PATH))

	Shader_Type :: enum {VERTEX, FRAGMENT}
	type: Shader_Type
	sources: [Shader_Type]strings.Builder

	for type in Shader_Type {
		strings.builder_init(&sources[type], context.temp_allocator)
	}
	defer {
		for type in Shader_Type {
			strings.builder_destroy(&sources[type])
		}
	}

	for line in strings.split_lines_after_iterator(&data) {
		if strings.has_prefix(line, "---") {
			for sh_type in Shader_Type {
				if strings.contains(line, reflect.enum_string(sh_type)) {
					type = sh_type
					break
				}
			}
			continue
		}
		strings.write_string(&sources[type], line)
	}

	program, ok = gl.load_shaders_source(strings.to_string(sources[.VERTEX]), strings.to_string(sources[.FRAGMENT]))
	if !ok {
		comp_message, comp_type, link_message, link_type := gl.get_last_error_messages()
		if comp_message != "" {
			fmt.eprintf("%v: failed to compile %v shader\n%s\n", location, comp_type, comp_message)
		}
		if link_message != "" {
			fmt.eprintf("%v: failed to link %v shader\n%s\n", location, link_type, link_message)
		}
	}
	return program, ok
}
