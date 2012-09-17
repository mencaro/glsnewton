uniform sampler2D image;

float LinearizeDepth(vec2 uv)
{
  float n = 1.0; // camera z near
  float f = 100.0; // camera z far
  float z = texture2D(image, uv).x;
  return (2.0 * n) / (f + n - z * (f - n));	
}

void main() 
{ 
  vec2 uv =  gl_TexCoord[0].xy;
  float d = LinearizeDepth(uv);
  gl_FragColor.rgb = vec4(d, d, d, 1.0);
}