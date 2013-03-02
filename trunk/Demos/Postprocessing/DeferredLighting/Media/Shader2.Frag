uniform sampler2D NormZTex;
uniform sampler2D ColorTex;
uniform sampler2D PosTex;
uniform vec4 LightPos;
uniform vec4 LightColor;

varying vec2 TexCoord;

void main ()
{
    vec4 NormZ = texture2D(NormZTex, TexCoord.xy);
    vec4 pp = texture2D(PosTex, TexCoord.xy);
    vec3 n = NormZ.xyz;
    vec3 l = normalize ( LightPos.xyz - pp.xyz );
    float lambertTerm = dot(n,l);
    vec4 TexColor = texture2D(ColorTex,TexCoord.xy);
    if (pp.z>0.0) TexColor.a = 0.0;
    vec3 color = clamp(lambertTerm * TexColor.rgb * LightColor.rgb, 0.0,1.0);
    gl_FragColor = vec4(color.rgb*TexColor.a, 1.0);
}

