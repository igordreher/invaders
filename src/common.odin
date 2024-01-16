package main

import ma "vendor:miniaudio"
import "core:mem"
import "core:fmt"

Sound_Engine :: ma.engine

init_sounds :: proc(engine: ^Sound_Engine) {
	wavs := map[cstring][]byte {
		"0" = #load("../sound/0.wav"),
		"1" = #load("../sound/1.wav"),
		"2" = #load("../sound/2.wav"),
		"3" = #load("../sound/3.wav"),
		"4" = #load("../sound/4.wav"),
		"5" = #load("../sound/5.wav"),
		"6" = #load("../sound/6.wav"),
		"7" = #load("../sound/7.wav"),
		"8" = #load("../sound/8.wav"),
	}
	defer delete(wavs)

	VFS :: struct {
		using cb: ma.vfs_callbacks,
		allocationCallbacks: ma.allocation_callbacks, // unused
		wavs: map[cstring][]byte,
	}
	vfs: VFS
	vfs.wavs = wavs
	vfs.onOpen = proc "c" (pVFS: ^ma.vfs, pFilePath: cstring, openMode: u32, pFile: ^ma.vfs_file) -> ma.result
	{
		pFile^ = auto_cast &((^VFS)(pVFS).wavs[pFilePath])
		return .SUCCESS
	}
	vfs.onClose = proc "c" (pVFS: ^ma.vfs, file: ma.vfs_file) -> ma.result
	{
		return .SUCCESS
	}
	vfs.onRead =  proc "c" (pVFS: ^ma.vfs, file: ma.vfs_file, pDst: rawptr, sizeInBytes: uint, pBytesRead: ^uint) -> ma.result
	{
		data := (^[]byte)(file)^
		bytes_read := min(sizeInBytes, cast(uint)len(data))
		pBytesRead^ = bytes_read
		mem.copy(pDst, &data[0], cast(int)bytes_read)
		return .SUCCESS
	}
	vfs.onInfo =  proc "c" (pVFS: ^ma.vfs, file: ma.vfs_file, pInfo: ^ma.file_info) -> ma.result
	{
		data := (^[]byte)(file)^
		pInfo.sizeInBytes = auto_cast len(data)
		return .SUCCESS
	}

	engine_config := ma.engine_config_init()
	engine_config.pResourceManagerVFS = auto_cast &vfs
	result := ma.engine_init(&engine_config, engine)
	assert(result == .SUCCESS) // TODO
	ma.engine_set_volume(engine, 0.1)

	for _, i in sounds {
		name := fmt.ctprintf("%d", i)
		result = ma.sound_init_from_file(engine, name, 0, nil, nil, &sounds[i])
		assert(result == .SUCCESS, fmt.tprintf("%s", result))
		delete(name, context.temp_allocator)
	}
	ma.sound_set_looping(&sounds[0], true) // ufo sound
}

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

sounds: [9]ma.sound
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
			if !read_bit(value, 0) && read_bit(state.ports[port], 0) {
				ma.sound_stop(&sounds[0])
			}
			for i in 0..<4 {
				if read_bit(value, i) && !read_bit(state.ports[port], i) {
					ma.sound_start(&sounds[i])
				}
			}
			state.ports[port] = value
		}
		case 5:
		{
			for i in 0..<5 {
				if read_bit(value, i) && !read_bit(state.ports[port], i) {
					ma.sound_start(&sounds[i+4])
				}
			}
			state.ports[port] = value
		}
		case: state.ports[port] = value
    }
}

toggle_bit :: proc "stdcall" (v: ^u8, bit: u8, state: bool) {
	if state {
		v^ |= 1 << bit
	} else {
		v^ &= 0xff ~ (1 << bit)
	}
}

read_bit :: proc(v: u8, #any_int bit: u8) -> bool {
	return (v & (1 << bit)) >> bit == 1
}

init_screen_colors :: proc() -> [HEIGHT][4]u8 {
	screen_color := [HEIGHT][4]u8{}
	yellow := [4]u8{255, 255, 0, 255}
	green := [4]u8{0, 255, 0, 255}
	white := [4]u8{255, 255, 255, 255}
	for _, i in screen_color {
		if i > 207 && i < 236 {
			screen_color[i] = yellow
		} else if i > 27 && i < 72 {
			screen_color[i] = green
		} else {
			screen_color[i] = white
		}
	}
	return screen_color
}