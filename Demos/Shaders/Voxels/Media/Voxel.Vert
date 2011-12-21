varying float gs_Connections;

void main ()
{
  gl_Position = gl_Vertex;
  
  gs_Connections = floor(gl_Normal.x*255.0);
}