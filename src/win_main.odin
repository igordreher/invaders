//+build windows
package main

import "core:fmt"
import "core:sys/windows"
import "core:mem"
import "core:runtime"
import "vendor:directx/dxgi"
import "vendor:directx/d3d11"
import "vendor:directx/d3d_compiler"


WIDTH :: 224
HEIGHT :: 256
SCALE :: 2

should_close: bool

main :: proc() {
	when PROFILE do profile_init()
	defer when PROFILE do profile_end()

	h_instance := windows.HINSTANCE(windows.GetModuleHandleW(nil))
	window_name := windows.utf8_to_wstring("Space Invaders")
	wc: windows.WNDCLASSW
	wc.lpfnWndProc = window_proc
	wc.hInstance = h_instance
	wc.lpszClassName = window_name
	windows.RegisterClassW(&wc)

	hwnd := windows.CreateWindowExW(0, window_name, window_name,
		// windows.WS_OVERLAPPEDWINDOW &~ windows.WS_MAXIMIZEBOX &~ windows.WS_THICKFRAME,
		windows.WS_OVERLAPPEDWINDOW,
		windows.CW_USEDEFAULT,
		windows.CW_USEDEFAULT,
		WIDTH,
		HEIGHT,
		nil,
		nil,
		h_instance,
		nil,
	)
	if hwnd == nil {
		// TODO
	}

	{	// set client size to WIDTH, HEIGHT
		rect: windows.RECT
		windows.GetClientRect(hwnd, &rect)
		client := [2]i32{rect.right - rect.left, rect.bottom - rect.top}
		border := [2]i32{WIDTH, HEIGHT} - client
		new_size := [2]i32{WIDTH, HEIGHT} * SCALE + border
		// center window
		screen: [2]i32
		screen.x = windows.GetSystemMetrics(windows.SM_CXSCREEN)
		screen.y = windows.GetSystemMetrics(windows.SM_CYSCREEN)
		pos := screen/2 - new_size/2
		//
		windows.SetWindowPos(hwnd, nil, pos.x, pos.y, new_size.x, new_size.y, 0)
		windows.ShowWindow(hwnd, windows.SW_NORMAL)
	}


	device: ^d3d11.IDevice
	swap_chain: ^dxgi.ISwapChain
	device_context: ^d3d11.IDeviceContext
	{
		swap_chain_desc := dxgi.SWAP_CHAIN_DESC{}
		swap_chain_desc.BufferDesc.RefreshRate.Numerator   = 0
		swap_chain_desc.BufferDesc.RefreshRate.Denominator = 1
		swap_chain_desc.BufferDesc.Format = .B8G8R8A8_UNORM
		swap_chain_desc.BufferDesc.Width  = WIDTH
		swap_chain_desc.BufferDesc.Height = HEIGHT
		swap_chain_desc.SampleDesc.Count = 1
		swap_chain_desc.BufferUsage = {.RENDER_TARGET_OUTPUT}
		swap_chain_desc.BufferCount = 1
		swap_chain_desc.OutputWindow = hwnd
		swap_chain_desc.Windowed = true
		swap_chain_desc.Flags = dxgi.SWAP_CHAIN{.ALLOW_MODE_SWITCH}

		flags := d3d11.CREATE_DEVICE_FLAGS{.SINGLETHREADED}
		when ODIN_DEBUG do flags += {.DEBUG}
		feature_level: d3d11.FEATURE_LEVEL

		hr := d3d11.CreateDeviceAndSwapChain(
			nil,
			.HARDWARE,
			nil,
			flags,
			nil,
			0,
			d3d11.SDK_VERSION,
			&swap_chain_desc,
			&swap_chain,
			&device,
			&feature_level,
		    &device_context,
		)
		win_assert(hr)
		viewport := d3d11.VIEWPORT {0, 0, WIDTH, HEIGHT, 0, 1}
		device_context->RSSetViewports(1, &viewport)
	}

	render_target_view: ^d3d11.IRenderTargetView
	{
		frame_buffer: ^d3d11.ITexture2D
		hr := swap_chain->GetBuffer(0, d3d11.ITexture2D_UUID, cast(^rawptr)&frame_buffer)
		win_assert(hr) // TODO
		hr = device->CreateRenderTargetView(frame_buffer, nil, &render_target_view)
		win_assert(hr)
		frame_buffer->Release()
	}

	vertex_shader: ^d3d11.IVertexShader
	pixel_shader: ^d3d11.IPixelShader
	vertex_buffer: ^d3d11.IBuffer
	index_buffer: ^d3d11.IBuffer
	input_layout: ^d3d11.IInputLayout
	{
		flags := d3d_compiler.D3DCOMPILE{.ENABLE_STRICTNESS}
		when ODIN_DEBUG do flags += {.DEBUG}
		vs_blob, ps_blob, err_blob: ^d3d11.IBlob
		raw := #load("shader.hlsl")

		hr := d3d_compiler.Compile(
			&raw[0],
			len(raw),
			nil,
			nil,
			d3d_compiler.D3DCOMPILE_STANDARD_FILE_INCLUDE,
			"vs_main",
			"vs_5_0",
			transmute(u32)flags,
			0,
			&vs_blob,
			&err_blob,
		)
		if hr != 0 {
			if err_blob != nil {
				fmt.eprintln(cast(cstring)err_blob->GetBufferPointer())
			}
			if vs_blob != nil do vs_blob->Release()
			panic("failed to compile ps shader")
		}
		hr = d3d_compiler.Compile(
			&raw[0],
			len(raw),
			nil,
			nil,
			d3d_compiler.D3DCOMPILE_STANDARD_FILE_INCLUDE,
			"ps_main",
			"ps_5_0",
			transmute(u32)flags,
			0,
			&ps_blob,
			&err_blob,
		)
		if hr != 0 {
			if err_blob != nil {
				fmt.eprintln(cast(cstring)err_blob->GetBufferPointer())
			}
			if ps_blob != nil do ps_blob->Release()
			panic("failed to compile ps shader")
		}

	    hr = device->CreateVertexShader(
			vs_blob->GetBufferPointer(),
			vs_blob->GetBufferSize(),
			nil,
			&vertex_shader,
	    )
		win_assert(hr) // TODO

	    hr = device->CreatePixelShader(
			ps_blob->GetBufferPointer(),
			ps_blob->GetBufferSize(),
			nil,
			&pixel_shader,
	    )
		win_assert(hr) // TODO

	    input_elements := [?]d3d11.INPUT_ELEMENT_DESC {
			{ "POS", 0, .R32G32_FLOAT, 0, 0, .VERTEX_DATA, 0 },
			{ "TEX", 0, .R32G32_FLOAT, 0, d3d11.APPEND_ALIGNED_ELEMENT, .VERTEX_DATA, 0 },
	    }
	    hr = device->CreateInputLayout(
			&input_elements[0],
			len(input_elements),
			vs_blob->GetBufferPointer(),
			vs_blob->GetBufferSize(),
			&input_layout,
	    )
		win_assert(hr) // TODO

		Vertex :: struct {pos, tex: [2]f32}
		vertices := [4]Vertex {
			{{+1, +1}, {1, 0}},
			{{+1, -1}, {1, 1}},
			{{-1, -1}, {0, 1}},
			{{-1, +1}, {0, 0}},
		}
		vertex_buf_desc: d3d11.BUFFER_DESC
		vertex_buf_desc.ByteWidth = size_of(vertices)
		vertex_buf_desc.Usage = .DEFAULT
		vertex_buf_desc.BindFlags = {.VERTEX_BUFFER}
		v_init_data := d3d11.SUBRESOURCE_DATA{pSysMem=&vertices[0]}
		hr = device->CreateBuffer(
			&vertex_buf_desc,
			&v_init_data,
			&vertex_buffer,
		)
		win_assert(hr)// TODO

		indices := [6]u32 { 0, 1, 2, 0, 2, 3, }
		index_buf_desc: d3d11.BUFFER_DESC
		index_buf_desc.ByteWidth = size_of(indices)
		index_buf_desc.Usage = .DEFAULT
		index_buf_desc.BindFlags = {.INDEX_BUFFER}
		i_init_data := d3d11.SUBRESOURCE_DATA{pSysMem=&indices[0]}
		hr = device->CreateBuffer(
			&index_buf_desc,
			&i_init_data,
			&index_buffer,
		)
		win_assert(hr) // TODO
	}

	texture_res: ^d3d11.IShaderResourceView
	texture: ^d3d11.ITexture2D
	{
		tex_desc := d3d11.TEXTURE2D_DESC {
			Width = HEIGHT/8, Height = WIDTH,
			MipLevels = 1, ArraySize = 1,
			Format = .R8_UNORM,
			SampleDesc = {Count=1},
			Usage = .DYNAMIC,
			BindFlags = {.SHADER_RESOURCE},
			CPUAccessFlags = {.WRITE},
		}
		hr := device->CreateTexture2D(&tex_desc, nil, &texture)
		win_assert(hr)

		res_desc := d3d11.SHADER_RESOURCE_VIEW_DESC {
			Format = .R8_UNORM,
			ViewDimension = .TEXTURE2D,
			Texture2D = {MipLevels=1},
		}
		hr = device->CreateShaderResourceView(&texture.id3d11resource, &res_desc, &texture_res)
		win_assert(hr)
	}

	sampler: ^d3d11.ISamplerState
	{
		sampler_desc := d3d11.SAMPLER_DESC {
			Filter = .MIN_MAG_MIP_POINT,
			AddressU = .CLAMP,
			AddressV = .CLAMP,
			AddressW = .CLAMP,
		}
		hr := device->CreateSamplerState(&sampler_desc, &sampler)
		win_assert(hr)
	}

	{	// only need to set theese once
		background_color := [4]f32 {1, 0, 1, 1}
		device_context->OMSetRenderTargets(1, &render_target_view, nil)
		device_context->ClearRenderTargetView(render_target_view, &background_color)
		device_context->IASetPrimitiveTopology(.TRIANGLELIST)
		device_context->IASetInputLayout(input_layout)
		vertex_offset := u32(0)
		vertex_stride := u32(size_of([4]f32))
		device_context->IASetVertexBuffers(0, 1, &vertex_buffer, &vertex_stride, &vertex_offset)
		device_context->IASetIndexBuffer(index_buffer, .R32_UINT, 0)

		device_context->VSSetShader(vertex_shader, nil, 0)
		device_context->PSSetShader(pixel_shader, nil, 0)
		device_context->PSSetSamplers(0, 1, &sampler)
		device_context->PSSetShaderResources(0, 1, &texture_res)
	}

	rom := #load("../invaders_rom")
	cpu := i8080_init(rom, port_in, port_out)
	vram := cpu.memory[0x2400:0x4000]
	ports = cpu.ports[:]

	cpu_speed := 2e+6
	refresh_rate :: 60
	cycles_per_interrupt := cpu_speed / refresh_rate / 2
	next_interrupt := cycles_per_interrupt
	which_interrupt: u16

	for !should_close && cpu.regs.PC < auto_cast len(rom)
	{
		msg: windows.MSG
		if windows.PeekMessageW(&msg, nil, 0, 0, windows.PM_REMOVE) {
			windows.TranslateMessage(&msg)
			windows.DispatchMessageW(&msg)
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

		{
			profile_scope("render")
			update_vram(vram, device_context, &texture.id3d11resource)
			hr := swap_chain->Present(1, {})
			win_assert(hr)
		}
	}
}


update_vram :: proc(vram: []byte, device_context: ^d3d11.IDeviceContext, texture: ^d3d11.IResource) {
	profile_scope(#procedure)
	mapped: d3d11.MAPPED_SUBRESOURCE
	hr := device_context->Map(texture, 0, .WRITE_DISCARD, {}, &mapped)
	if win_assert(hr) {
		texture_height :: WIDTH
		texture_pitch :: HEIGHT/8
		for i in 0..<texture_height {
			ptr := mem.ptr_offset(([^]byte)(mapped.pData), u32(i)*mapped.RowPitch)
			offset := texture_pitch*i
			mem.copy(ptr, &vram[offset], texture_pitch)
		}
		device_context->Unmap(texture, 0)
	}
	device_context->DrawIndexed(6, 0, 0)
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
			state.ports[port] = value
			// TODO: play audio
		}
		case 5:
		{
			state.ports[port] = value
			// TODO: play audio
		}
		case: state.ports[port] = value
    }
}

ports: []byte
window_proc :: proc "stdcall" (hwnd: windows.HWND, msg: windows.UINT, w_param: windows.WPARAM, l_param: windows.LPARAM) -> windows.LRESULT
{
	// context = runtime.default_context()
	switch msg {
		case windows.WM_CLOSE, windows.WM_QUIT, windows.WM_DESTROY:
		{
			should_close = true
			return 0
		}
		case windows.WM_KEYDOWN, windows.WM_KEYUP:
		{
			state := msg == windows.WM_KEYDOWN
			switch w_param
			{
				case windows.VK_RETURN: toggle_bit(&ports[1], 0, state)
				case windows.VK_LEFT, windows.VK_A: toggle_bit(&ports[1], 5, state)
				case windows.VK_RIGHT, windows.VK_D: toggle_bit(&ports[1], 6, state)
				case windows.VK_SPACE: toggle_bit(&ports[1], 4, state)
				case windows.VK_1: toggle_bit(&ports[1], 2, state)
			}
		}
	}
	return windows.DefWindowProcW(hwnd, msg, w_param, l_param)
}

toggle_bit :: proc "stdcall" (v: ^u8, bit: u8, state: bool) {
	if state {
		v^ |= 1 << bit
	} else {
		v^ &= 0xff ~ (1 << bit)
	}
}

win_assert :: proc(#any_int hresult: int, location := #caller_location) -> bool {
	if windows.SUCCEEDED(hresult) do return true
	err := windows.System_Error(windows.GetLastError())
	if err == .SUCCESS do err = windows.System_Error(hresult & 0xffff)
	message := fmt.tprintf("WIN32 ERROR: %s", err)
	windows.MessageBoxW(nil,
		windows.utf8_to_wstring(message),
		windows.utf8_to_wstring("ERROR"),
		windows.MB_ICONERROR | windows.MB_OK,
	)
	return false
}