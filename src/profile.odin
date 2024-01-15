package main

import "core:prof/spall"

PROFILE :: #config(PROFILE, ODIN_DEBUG)

when PROFILE
{
	buf: spall.Buffer
	ctx: spall.Context
	@(private)
	_buf_backing: []byte
}

profile_init :: proc() {
when PROFILE
{
	ctx, _ = spall.context_create("prof.spall")
	_buf_backing = make([]byte, spall.BUFFER_DEFAULT_SIZE)
	buf, _ = spall.buffer_create(_buf_backing)
}
}
profile_end :: proc() {
when PROFILE
{
	spall.buffer_destroy(&ctx, &buf)
	spall.context_destroy(&ctx)
	delete(_buf_backing)
}
}

@(deferred_none=_profile_scope_end)
profile_scope :: proc(name: string, location := #caller_location) {
	when PROFILE do spall._buffer_begin(&ctx, &buf, name, "", location)
}
_profile_scope_end :: proc() {
	when PROFILE do spall._buffer_end(&ctx, &buf)
}