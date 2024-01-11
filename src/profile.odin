package main

import "core:prof/spall"

PROFILE :: #config(PROFILE, ODIN_DEBUG)

when PROFILE
{
	buf: spall.Buffer
	ctx: spall.Context
}
@(deferred_none=_profile_scope_end)
profile_scope :: proc(name: string, location := #caller_location) {
	when PROFILE do spall._buffer_begin(&ctx, &buf, name, "", location)
}
_profile_scope_end :: proc() {
	when PROFILE do spall._buffer_end(&ctx, &buf)
}