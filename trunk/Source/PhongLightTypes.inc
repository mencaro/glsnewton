    if att then begin
      s:=s+'float calculateAttenuation(in int i, in float dist)'+br;
      s:=s+'{'+br;
      s:=s+'    return(1.0 / (lConstAtt[i] + (lLinAtt[i] + lQuadAtt[i] * dist) * dist);'+br;
      s:=s+'}'+br;
    end;
    if lsOmni in ls then begin
      //Lights
      s:=s+'void pointLight(in int i, in vec3 N, in vec3 V, in float shininess,'+br;
      s:=s+'                inout vec4 ambient, inout vec4 diffuse, inout vec4 specular)'+br;
      s:=s+'{'+br;
      s:=s+'    vec3 D = lPosition[i].xyz - V;'+br;
      s:=s+'    vec3 L = normalize(D);'+br;
      s:=s+'    float dist = length(D);'+br;
      if att then
        s:=s+'    float attenuation = calculateAttenuation(i, dist);'+br
      else s:=s+'    float attenuation = 1.0;'+br;
      s:=s+'    float nDotL = dot(N,L);'+br;
      s:=s+'    if (nDotL > 0.0)'+br;
      s:=s+'    {'+br;
      s:=s+'        diffuse  += lDiffuseColor[i] * attenuation * nDotL;'+br;
      if not FNoSpecular then begin
        s:=s+'        vec3 E = normalize(-V);'+br;
        s:=s+'        vec3 R = reflect(-L, N);'+br;
        s:=s+'        float pf = pow(max(dot(R,E), 0.0), shininess);'+br;
        s:=s+'        specular += lSpecularColor[i] * attenuation * pf;'+br;
      end;
      s:=s+'    }'+br;
      s:=s+'    ambient  += lAmbientColor[i] * attenuation;'+br;
      s:=s+'}'+br;
    end;
    if lsDirectional in ls then begin
      s:=s+'void directionalLight(in int i, in vec3 N, in float shininess,'+br;
      s:=s+'                      inout vec4 ambient, inout vec4 diffuse, inout vec4 specular)'+br;
      s:=s+'{'+br;
      s:=s+'    vec3 L = normalize(lPosition[i].xyz);'+br;
      s:=s+'    float nDotL = dot(N, L);'+br;
      s:=s+'    if (nDotL > 0.0)'+br;
      s:=s+'    {'+br;
      s:=s+'        diffuse  += lDiffuseColor[i]  * nDotL;'+br;
      if not FNoSpecular then begin
        s:=s+'        vec3 H = normalize(L + vec3(0.0, 0.0, 1.0));'+br;
        s:=s+'        float pf = pow(max(dot(N,H), 0.0), shininess);'+br;
        s:=s+'        specular += lSpecularColor[i] * pf;'+br;
      end;
      s:=s+'    }'+br;
      s:=s+'    ambient  += lAmbientColor[i];'+br;
      s:=s+'}'+br;
    end;
    if lsSpot in ls then begin
      s:=s+'void spotLight(in int i, in vec3 N, in vec3 V, in float shininess,'+br;
      s:=s+'               inout vec4 ambient, inout vec4 diffuse, inout vec4 specular)'+br;
      s:=s+'{'+br;
      s:=s+'    vec3 D = lPosition[i].xyz - V;'+br;
      s:=s+'    vec3 L = normalize(D);'+br;
      s:=s+'    float dist = length(D);'+br;
      if att then
        s:=s+'    float attenuation = calculateAttenuation(i, dist);'+br
      else s:=s+'    float attenuation = 1.0;'+br;
      s:=s+'    float nDotL = dot(N,L);'+br;
      s:=s+'    if (nDotL > 0.0)'+br;
      s:=s+'    {'+br;
      s:=s+'        float spotEffect = dot(normalize(lDirection[i]), -L);'+br;
      s:=s+'        if (spotEffect > lSpotCosCutoff[i])'+br;
      s:=s+'        {'+br;
      s:=s+'            attenuation *=  pow(spotEffect, lSpotExp[i]);'+br;
      s:=s+'            diffuse  += lDiffuseColor[i]  * attenuation * nDotL;'+br;
      if not FNoSpecular then begin
        s:=s+'            vec3 E = normalize(-V);'+br;
        s:=s+'            vec3 R = reflect(-L, N);'+br;
        s:=s+'            float pf = pow(max(dot(R,E), 0.0), shininess);'+br;
        s:=s+'            specular +=lSpecularColor[i] * attenuation * pf;'+br;
      end;
      s:=s+'        }'+br;
      s:=s+'    }'+br;
      s:=s+'    ambient  += lAmbientColor[i] * attenuation;'+br;
      s:=s+'}'+br;
    end;
    if lsParallel in ls then begin
      s:=s+'void parallelLight(in int i, in vec3 N, in vec3 V, in float shininess,' +br;
      s:=s+'    inout vec4 ambient, inout vec4 diffuse, inout vec4 specular);'+br;
      s:=s+'{'+br;
      s:=s+'   float nDotVP;         // normal . light direction'+br;
      s:=s+'   float nDotHV;         // normal . light half vector'+br;
      s:=s+'   float pf=0.0;         // power factor'+br;
      s:=s+'   float spotAttenuation;'+br;
      s:=s+'   vec3  Ppli;'+br;
      s:=s+'   vec3  Sdli;'+br;
      s:=s+'   vec3 D = lPosition[i].xyz - V;'+br;
      s:=s+'   vec3 L = normalize(D);'+br;
      s:=s+'   vec3 H = normalize(L + vec3(0.0, 0.0, 1.0));'+br;
      s:=s+'   nDotVP = max(0.0, dot(N, D));'+br;
      s:=s+'   Ppli = -L;'+br;
      s:=s+'   Sdli = normalize(lDirection[i]);'+br;
      s:=s+'   spotAttenuation = pow(dot(Ppli, Sdli), lSpotExp[i]);'+br;
      s:=s+'   ambient += lAmbientColor[i] * spotAttenuation;'+br;
      s:=s+'   diffuse += lDiffuseColor[i] * nDotVP * spotAttenuation;'+br;
      if not FNoSpecular then begin
        s:=s+'   if (nDotVP != 0.0)'+br;
        s:=s+'   {'+br;
        s:=s+'       nDotHV = max(0.0, dot(N, H));'+br;
        s:=s+'       pf = pow(nDotHV, shininess);'+br;
        s:=s+'       specular += lSpecularColor[i] * pf * spotAttenuation;'+br;
        s:=s+'   }'+br;
      end;
      s:=s+'}'+br;
    end;

    s:=s+'vec4 flight(in vec3 normal, in vec4 ecPosition)'+br;
    s:=s+'{'+br;
    s:=s+'    vec4 color;'+br;
    s:=s+'    vec3 ecPosition3 = ecPosition.xyz / ecPosition.w;'+br;
    s:=s+'    Ambient  = vec4 (0.0);'+br;
    s:=s+'    Diffuse  = vec4 (0.0);'+br;
    s:=s+'    Specular = vec4 (0.0);'+br;
    n:=0;
    for i:=0 to GLStateCache.LightingCache.MaxLights-1 do begin
      if GLStateCache.LightingCache.Lights[i].Enabled then begin
        lt:=LightType(i);
        case lt of
          lsOmni: begin
            s:=s+'    pointLight('+inttostr(n)+', normal, ecPosition3, mShininess,'+br;
            s:=s+'      Ambient, Diffuse, Specular);'+br;
          end;
          lsDirectional: begin
            s:=s+'    directionalLight('+inttostr(n)+', normal, ecPosition3, mShininess,'+br;
            s:=s+'      Ambient, Diffuse, Specular);'+br;
          end;
          lsSpot: begin
            s:=s+'    spotLight('+inttostr(n)+', normal, ecPosition3, mShininess,'+br;
            s:=s+'      Ambient, Diffuse, Specular);'+br;
          end;
          lsParallel: begin
            s:=s+'    parallelLight('+inttostr(n)+', normal, ecPosition3, mShininess,'+br;
            s:=s+'      Ambient, Diffuse, Specular);'+br;
          end;
        end;
        inc(n);
      end;
    end;
    s:=s+'    color = mEmissionColor + '+br;
    s:=s+'      Ambient  * mAmbientColor +'+br;
    s:=s+'      Diffuse  * mDiffuseColor;'+br;
    if not FNoSpecular then
      s:=s+'    color += Specular * mSpecularColor;'+br;
    //s:=s+'    color = clamp( color, 0.0, 1.0 );'+br;
    s:=s+'    return (color);'+br;
    s:=s+'}'+br;

