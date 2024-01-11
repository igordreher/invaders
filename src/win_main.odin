//+build windows
package main

import "core:fmt"
import "core:sys/windows"
import "vendor:directx/dxgi"
import "vendor:directx/d3d11"

WIDTH :: 224
HEIGHT :: 256

should_close: bool

main :: proc() {
	h_instance := windows.HINSTANCE(windows.GetModuleHandleW(nil))
	window_name := windows.utf8_to_wstring("Space Invaders")
	wc: windows.WNDCLASSW
	wc.lpfnWndProc = window_proc
	wc.hInstance = h_instance
	wc.lpszClassName = window_name
	windows.RegisterClassW(&wc)

	hwnd := windows.CreateWindowExW(0, window_name, window_name,
		windows.WS_OVERLAPPEDWINDOW &~ windows.WS_MAXIMIZEBOX &~ windows.WS_THICKFRAME,
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

	windows.ShowWindow(hwnd, windows.SW_NORMAL)


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
		// TODO: handle hr error
	}

	render_target_view: ^d3d11.IRenderTargetView
	{
		frame_buffer: ^d3d11.ITexture2D
		hr := swap_chain->GetBuffer(0, d3d11.ITexture2D_UUID, cast(^rawptr)&frame_buffer)
		// win_assert(hr) // TODO
		hr = device->CreateRenderTargetView(frame_buffer, nil, &render_target_view)
		// win_assert(hr)
		frame_buffer->Release()
	}

	for !should_close
	{
		msg: windows.MSG
		if windows.PeekMessageW(&msg, nil, 0, 0, windows.PM_REMOVE) {
			windows.TranslateMessage(&msg)
			windows.DispatchMessageW(&msg)
		}

		// render
		background_color := [4]f32 {1, 0, 1, 1}
		device_context->OMSetRenderTargets(1, &render_target_view, nil)
		device_context->ClearRenderTargetView(render_target_view, &background_color)


		swap_chain->Present(1, {})
	}
}

window_proc :: proc "stdcall" (hwnd: windows.HWND, msg: windows.UINT, w_param: windows.WPARAM, l_param: windows.LPARAM) -> windows.LRESULT
{
	switch msg {
		case windows.WM_CLOSE, windows.WM_QUIT, windows.WM_DESTROY:
		{
			should_close = true
			return 0
		}
	}
	return windows.DefWindowProcW(hwnd, msg, w_param, l_param)
}