//
// Lambert fragment shader
//

varying vec3 normal, lightDir, eyeVec;

void main (void)
{
	vec4 final_color = 
	(gl_FrontLightModelProduct.sceneColor * gl_FrontMaterial.ambient) + 
	(gl_LightSource[0].ambient * gl_FrontMaterial.ambient);
							
	vec3 N = normalize(normal);
	vec3 L = normalize(lightDir);
	
	float lambertTerm = max (dot(N,L), 0.0);
        final_color += gl_LightSource[0].diffuse * gl_FrontMaterial.diffuse * lambertTerm;	
	
	gl_FragColor = final_color;			
}
