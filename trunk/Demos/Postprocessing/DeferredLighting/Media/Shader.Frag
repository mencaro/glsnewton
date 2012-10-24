uniform sampler2D DiffuseMap;
uniform sampler2D NormalMap;

varying vec2 TexCoord;
varying vec3 T,B,N;
varying vec4 pos;
void main(void)
{
  vec3 nm = texture2D(NormalMap, TexCoord).rgb * 2.0 - 1.0;
  vec3 Normal = normalize(nm.x*T + nm.y*B + nm.z*N);

  gl_FragData[0].xyz = Normal;
  gl_FragData[1].rgba = texture2D(DiffuseMap,TexCoord).rgba;
  gl_FragData[2].rgba = pos;
}
