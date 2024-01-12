struct vs_in {
	float2 pos : POS;
	float2 uv : TEX;
};

struct vs_out {
	float4 pos_clip : SV_POSITION;
	float2 uv : TEX;
};

vs_out vs_main(vs_in input) {
	float w = 224;
	float h = 256;
	matrix rotation = matrix(
		0, -1, 0, 0,
		1,  0, 0, 0,
		0,  0, 1, 0,
		0,	0, 0, 1
	);

	vs_out output = (vs_out)0; // zero the memory first
	output.pos_clip = float4(input.pos, 0, 1);
	output.pos_clip = mul(rotation, output.pos_clip);

	output.uv = input.uv;
	return output;
}

sampler Sampler : register(s0);
Texture2D Texture : register(t0);

float4 ps_main(vs_out p) : SV_TARGET {
	uint byte = Texture.Sample(Sampler, p.uv).r * 255;
	uint pixel_index = (p.uv.x * 256) % 8;
	uint pixel = (byte & (1 << pixel_index)) >> pixel_index;
	return float4(pixel, pixel, pixel, pixel);
}