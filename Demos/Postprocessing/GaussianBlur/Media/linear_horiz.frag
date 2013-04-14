uniform sampler2D image;
uniform vec4 TexSize;

const float offset[3] = { 0.0, 1.3846153846, 3.2307692308 };
const float weight[3] = { 0.2270270270, 0.3162162162, 0.0702702703 };


void main(void)
{
	gl_FragColor = texture2D( image, vec2(gl_FragCoord)/TexSize.xy ) * weight[0];
	for (int i=1; i<3; i++) {
		gl_FragColor += texture2D( image, ( vec2(gl_FragCoord)+vec2(offset[i], 0.0) )/TexSize.xy ) * weight[i];
		gl_FragColor += texture2D( image, ( vec2(gl_FragCoord)-vec2(offset[i], 0.0) )/TexSize.xy ) * weight[i];
	}
	gl_FragColor *=vec4(1.0, 0.1, 0.1, 1.0);
}
