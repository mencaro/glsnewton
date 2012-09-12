varying vec2 TexCoord;

void main ()
{
  gl_Position = vec4( gl_Vertex.xy, 0.0, 1.0 );
  TexCoord = 0.5*(gl_Position.xy+1.0);
}

