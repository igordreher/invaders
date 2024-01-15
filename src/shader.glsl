--- VERTEX
#version 330 core

layout (location = 0) in vec2 pos;
layout (location = 1) in vec2 in_uv;

out vec2 uv;

void main() {
	mat4 rotation = mat4(
		0, -1, 0, 0,
		1,  0, 0, 0,
		0,  0, 1, 0,
		0,	0, 0, 1
	);
	gl_Position = rotation * vec4(pos, 1, 1);
	uv = in_uv;
}

--- FRAGMENT
#version 330 core

in vec2 uv;
out vec4 color;

uniform sampler2D image;

void main() {
	float byteF = texture(image, uv).r * 255;
	int byte = int(byteF);
	// int pixel_index = int(mod(uv.x * 256, 8));
	int pixel_index = int(uv.x * 256) % 8;
	int pixel = (byte & (1 << pixel_index)) >> pixel_index;
	color = vec4(pixel, pixel, pixel, pixel);
}
