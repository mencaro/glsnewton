#version 150
#extension GL_EXT_gpu_shader4: enable
layout(points) in;
layout(triangle_strip, max_vertices = 24) out;

precision highp float;

uniform mat4 ModelViewProjection;
uniform mat4 ModelMatrix;
uniform mat4 ModelViewMatrix;
uniform mat3 NormalMatrix;
uniform vec3 cubeSize;
uniform vec4 LightPos;

in float gs_Connections[];
//out vec4 gl_TexCoord[];
vec4 Corners[8];
vec3 Normals[6];


void Lighting(in vec4 vert, in vec3 norm)
{

/* vec3 ecPosition = (ModelViewMatrix * vert).xyz;
 vec3 p = LightPos.xyz - ecPosition.xyz;
 gl_FrontColor.rgb = ((0.8 * max(dot(norm, p), 0.0)) + 0.2) * vec3(1.0);
 gl_FrontColor.a = 1.0;
*/
gl_FrontColor = vec4(1.0);
}

void main()
{

 vec3 cPos = -(ModelViewMatrix*gl_in[0].gl_Position).xyz;

 int bits = int(gs_Connections[0]);
 if (bits > 192) 
 {
  vec3 up = ModelMatrix[1].xyz;  
  vec3 left = ModelMatrix[0].xyz;
  vec3 dir = ModelMatrix[2].xyz;
  vec3 csd2 = cubeSize / 2.0;

  Corners[0] = ModelViewProjection * vec4((up+left-dir)*csd2+gl_in[0].gl_Position.xyz,1.0);
  Corners[1] = ModelViewProjection * vec4((-up+left-dir)*csd2+gl_in[0].gl_Position.xyz,1.0);
  Corners[2] = ModelViewProjection * vec4((-up-left-dir)*csd2+gl_in[0].gl_Position.xyz,1.0);
  Corners[3] = ModelViewProjection * vec4((up-left-dir)*csd2+gl_in[0].gl_Position.xyz,1.0);

  Corners[4] = ModelViewProjection * vec4((up+left+dir)*csd2+gl_in[0].gl_Position.xyz,1.0);
  Corners[5] = ModelViewProjection * vec4((-up+left+dir)*csd2+gl_in[0].gl_Position.xyz,1.0);
  Corners[6] = ModelViewProjection * vec4((-up-left+dir)*csd2+gl_in[0].gl_Position.xyz,1.0);
  Corners[7] = ModelViewProjection * vec4((up-left+dir)*csd2+gl_in[0].gl_Position.xyz,1.0);

  Normals[0] = NormalMatrix*vec3(0, 0,-1);
  Normals[1] = NormalMatrix*vec3(0, 0, 1);
  Normals[2] = NormalMatrix*vec3(0, 1, 0);
  Normals[3] = NormalMatrix*vec3(0,-1, 0);
  Normals[4] = NormalMatrix*vec3( 1,0, 0);
  Normals[5] = NormalMatrix*vec3(-1,0, 0);


  if (((bits & 1) == 1) && (dot(Normals[0],cPos)>0.0)) 
  {
    //Front side
    gl_Position = Corners[0]; Lighting (gl_Position,Normals[0]);
    gl_FrontColor *= vec4(1.0,0.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[1]; Lighting (gl_Position,Normals[0]);
    gl_FrontColor *= vec4(1.0,0.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[3]; Lighting (gl_Position,Normals[0]);
    gl_FrontColor *= vec4(1.0,0.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[2]; Lighting (gl_Position,Normals[0]);
    gl_FrontColor *= vec4(1.0,0.0,0.0,1.0); EmitVertex();  
    EndPrimitive();
  }

  if (((bits & 2) == 2) && (dot(Normals[1],cPos)>0.0)) 
  {
    //Back side
    gl_Position = Corners[7]; Lighting (gl_Position,Normals[1]);
    gl_FrontColor *= vec4(0.0,1.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[6]; Lighting (gl_Position,Normals[1]);
    gl_FrontColor *= vec4(0.0,1.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[4]; Lighting (gl_Position,Normals[1]);
    gl_FrontColor *= vec4(0.0,1.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[5]; Lighting (gl_Position,Normals[1]);
    gl_FrontColor *= vec4(0.0,1.0,0.0,1.0); EmitVertex();  
    EndPrimitive();
  }

  if (((bits & 4) == 4) && (dot(Normals[2],cPos)>0.0))
  {
    //Top side
    gl_Position = Corners[4]; Lighting (gl_Position,Normals[2]); 
    gl_FrontColor *= vec4(0.0,0.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[0]; Lighting (gl_Position,Normals[2]);
    gl_FrontColor *= vec4(0.0,0.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[7]; Lighting (gl_Position,Normals[2]);
    gl_FrontColor *= vec4(0.0,0.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[3]; Lighting (gl_Position,Normals[2]);
    gl_FrontColor *= vec4(0.0,0.0,1.0,1.0); EmitVertex();  
    EndPrimitive();
  }

  if (((bits & 8) == 8) && (dot(Normals[3],cPos)>0.0)) 
  {
    //Bottom side
    gl_Position = Corners[1]; Lighting (gl_Position,Normals[3]);
    gl_FrontColor *= vec4(1.0,0.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[5]; Lighting (gl_Position,Normals[3]);
    gl_FrontColor *= vec4(1.0,0.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[2]; Lighting (gl_Position,Normals[3]);
    gl_FrontColor *= vec4(1.0,0.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[6]; Lighting (gl_Position,Normals[3]);
    gl_FrontColor *= vec4(1.0,0.0,1.0,1.0); EmitVertex();  
    EndPrimitive();
  }

  if (((bits & 16) == 16) && (dot(Normals[4],cPos)>0.0))
  {
    //Left side
    gl_Position = Corners[4]; Lighting (gl_Position,Normals[4]);
    gl_FrontColor *= vec4(1.0,1.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[5]; Lighting (gl_Position,Normals[4]);
    gl_FrontColor *= vec4(1.0,1.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[0]; Lighting (gl_Position,Normals[4]);
    gl_FrontColor *= vec4(1.0,1.0,0.0,1.0); EmitVertex();
    gl_Position = Corners[1]; Lighting (gl_Position,Normals[4]);
    gl_FrontColor *= vec4(1.0,1.0,0.0,1.0); EmitVertex();  
    EndPrimitive();
  }

  if (((bits & 32) == 32) && (dot(Normals[5],cPos)>0.0))
  {
    //Right side
    gl_Position = Corners[3]; Lighting (gl_Position,Normals[5]);
    gl_FrontColor *= vec4(0.0,1.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[2]; Lighting (gl_Position,Normals[5]);
    gl_FrontColor *= vec4(0.0,1.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[7]; Lighting (gl_Position,Normals[5]);
    gl_FrontColor *= vec4(0.0,1.0,1.0,1.0); EmitVertex();
    gl_Position = Corners[6]; Lighting (gl_Position,Normals[5]);
    gl_FrontColor *= vec4(0.0,1.0,1.0,1.0); EmitVertex();  
    EndPrimitive();
  }
 }
}