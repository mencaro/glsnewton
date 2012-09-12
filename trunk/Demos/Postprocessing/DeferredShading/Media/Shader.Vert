varying vec2 TexCoord;
varying vec3 T,B,N;
varying vec4 pos;

attribute vec3 Tangent;
attribute vec3 Binormal;
void main ()
{
  gl_Position = ftransform();
  TexCoord = gl_MultiTexCoord0.xy;
  T = gl_NormalMatrix*Tangent;
  B = gl_NormalMatrix*Binormal;
  N = gl_NormalMatrix*gl_Normal;
  pos = (gl_ModelViewMatrix*gl_Vertex);
}
