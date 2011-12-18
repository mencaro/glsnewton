uniform sampler2D NormalMap;
uniform float scalefactor = 0.0;
uniform float invtexsize;
varying vec2 TexCoord;

void main(void)
{
  vec4 n = texture2D(NormalMap, TexCoord);
  vec3 nn = normalize(n.xyz); 
  vec4 n2,n3;
       n2 = texture2D(NormalMap, vec2(TexCoord.s,TexCoord.t+invtexsize)); 
       n3 = texture2D(NormalMap, TexCoord+vec2(invtexsize,invtexsize)); 
       n.xy=vec2(0.0,0.0);
       n2.xy=vec2(0.0,1.0);
       n3.xy=vec2(1.0,1.0);
  vec4 d1, d2;
       d1=n2-n;
       d2=n3-n;
  vec3 nx = cross(d1.xyz,d2.xyz);         
  vec3 pos;
       pos.xy = (TexCoord.st - vec2(0.5,0.5))*2.0;//-------------------------
       pos.z = n.z*scalefactor;
//       pos.xyz+=nn.xyz*scalefactor;   
//nn.y*scalefactor;
       //norm.y*2.0;//sin(TexCoord.s*TexCoord.t); 
       //pos.z+=n.z*scalefactor;   
//  gl_FragData[0] = vec4(pos,1.0);
  gl_FragData[0] = vec4(pos,1.0);
  gl_FragData[1] = vec4(-nx,1.0);
  gl_FragData[2] = vec4(TexCoord,0.0, 0.0);
}
