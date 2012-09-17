varying vec4 vPos;
void main(void) 
{
  gl_Position = ftransform();
  gl_TexCoord[0] = gl_MultiTexCoord0;
  vPos = gl_Vertex;
}
