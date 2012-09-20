uniform sampler2D ShadowTex;
uniform sampler2D MainTex;
uniform sampler2D DepthTex;

uniform mat4 imvp;
uniform mat4 shadowMatrix;

varying vec4 vPos;

void main() 
{ 

  vec4 Color = texture2D(MainTex, gl_TexCoord[0].st);

  float fDepth = texture2D(DepthTex, gl_TexCoord[0].st).r;  
  if (fDepth == 1.0) discard;

  vec4 wPos =  imvp*vec4(vPos.xy, fDepth * 2.0 - 1.0, 1.0); wPos /= wPos.w;  

  vec4 shadow_proj =  shadowMatrix * wPos; shadow_proj.xyz /= shadow_proj.w; 
       shadow_proj.z -= 0.0001;

  float sDepth = texture2D(ShadowTex,shadow_proj.xy).z;

  float shadow = 1.0;
  if (shadow_proj.w > 0.0 && sDepth < 1.0)
    shadow = sDepth < shadow_proj.z ? 0.5 : 1.0 ;

  gl_FragColor = vec4(Color.rgb*shadow,1.0);
}