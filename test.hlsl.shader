
Shader "Converted/Template"
{
    Properties
    {
        _MainTex ("iChannel0", 2D) = "white" {}
        _SecondTex ("iChannel1", 2D) = "white" {}
        _ThirdTex ("iChannel2", 2D) = "white" {}
        _FourthTex ("iChannel3", 2D) = "white" {}
        _Mouse ("Mouse", Vector) = (0.5, 0.5, 0.5, 0.5)
    }
    SubShader
    {
        Pass
        {
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            #include "UnityCG.cginc"

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float2 uv : TEXCOORD0;
                float4 vertex : SV_POSITION;
            };

            sampler2D _MainTex;   float4 _MainTex_TexelSize;
            sampler2D _SecondTex; float4 _SecondTex_TexelSize;
            sampler2D _ThirdTex;  float4 _ThirdTex_TexelSize;
            sampler2D _FourthTex; float4 _FourthTex_TexelSize;
            float4 _Mouse;

            // GLSL Compatability macros
            #define iFrame (floor(_Time.y / 60))
            #define glsl_mod(x,y) (((x)-(y)*floor((x)/(y))))
            #define texelFetch(ch, uv, lod) tex2Dlod(ch, float4((uv).xy * ch##_TexelSize.xy + ch##_TexelSize.xy * 0.5, 0, lod))
            #define textureLod(ch, uv, lod) tex2Dlod(ch, float4(uv, 0, lod))

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv =  v.uv;
                return o;
            }

            float hash(float n)
            {
                return frac(sin(n)*158.54532);
            }

            float noise(in float x)
            {
                float p = floor(x);
                float f = frac(x);
                f = f*f*(3.-2.*f);
                return lerp(hash(p+0.), hash(p+1.), f);
            }

            float noise(in float2 x)
            {
                float2 p = floor(x);
                float2 f = frac(x);
                f = f*f*(3.-2.*f);
                float n = p.x+p.y*157.;
                return lerp(lerp(hash(n+0.), hash(n+1.), f.x), lerp(hash(n+157.), hash(n+158.), f.x), f.y);
            }

            static const float2x2 m2 = float2x2(0.8, -0.6, 0.6, 0.8);
            float fbm(float2 p)
            {
                float f = 0.;
                f += 0.5*noise(p);
                p = mul(m2,p)*2.02;
                f += 0.25*noise(p);
                p = mul(m2,p)*2.03;
                f += 0.125*noise(p);
                p = mul(m2,p)*2.01;
                f += 0.0625*noise(p);
                return f/0.9375;
            }

            float3 texturize(sampler2D sa, float3 p, float3 n)
            {
                float3 x = tex2D(sa, p.yz).xyz;
                float3 y = tex2D(sa, p.zx).xyz;
                float3 z = tex2D(sa, p.xy).xyz;
                return x*abs(n.x)+y*abs(n.y)+z*abs(n.z);
            }

            float2 sdSegment(in float3 p, in float3 a, in float3 b)
            {
                float3 pa = p-a, ba = b-a;
                float h = clamp(dot(pa, ba)/dot(ba, ba), 0., 1.);
                return float2(length(pa-ba*h), h);
            }

            float sdEllipsoid(in float3 p, in float3 r)
            {
                return (length(p/r)-1.)*min(min(r.x, r.y), r.z);
            }

            float smin(float a, float b, float k)
            {
                float h = max(k-abs(a-b), 0.)/k;
                return min(a, b)-h*h*h*k*(1./6.);
            }

            float opS(float d1, float d2)
            {
                return max(-d1, d2);
            }

#define ZERO min(iFrame, 0)
            float2 map(float3 p)
            {
                p.y -= 1.8;
                p.x = abs(p.x);
                float3 q = p;
                q.y -= 0.3*pow(1.-length(p.xz), 1.)*smoothstep(0., 0.2, p.y);
                q.y *= 1.05;
                q.z *= 1.+0.1*smoothstep(0., 0.5, q.z)*smoothstep(-0.5, 0.5, p.y);
                float dd = length((p-float3(0., 0.65, 0.8))*float3(1., 0.75, 1.));
                float am = clamp(4.*abs(p.y-0.45), 0., 1.);
                float fo = -0.03*(1.-smoothstep(0., 0.04*am, abs(dd-0.42)))*am;
                float dd2 = length((p-float3(0., 0.65, 0.8))*float3(1., 0.25, 1.));
                float am2 = clamp(1.5*(p.y-0.45), 0., 1.);
                float fo2 = -0.085*(1.-smoothstep(0., 0.08*am2, abs(dd2-0.42)))*am2;
                q.y += -0.05+0.05*length(q.x);
                float d1 = length(q)-0.9+fo+fo2;
                float2 res = float2(d1, 1.);
                float2 h = sdSegment(p, float3(0.83, 0.15, 0.), float3(1.02, -0.6, -0.1));
                float d2 = h.x-0.07;
                res.x = smin(res.x, d2, 0.03);
                h = sdSegment(p, float3(1.02, -0.6, -0.1), float3(0.95, -1.2, 0.1));
                d2 = h.x-0.07+h.y*0.02;
                res.x = smin(res.x, d2, 0.02);
                if (p.y<-1.)
                {
                    float fa = sin(3.*_Time.y);
                    h = sdSegment(p, float3(0.95, -1.2, 0.1), float3(0.97, -1.5, 0.));
                    d2 = h.x-0.03;
                    res.x = smin(res.x, d2, 0.01);
                    h = sdSegment(p, float3(0.97, -1.5, 0.), float3(0.95, -1.7, 0.)-0.01*fa);
                    d2 = h.x-0.03+0.01*h.y;
                    res.x = smin(res.x, d2, 0.02);
                    h = sdSegment(p, float3(0.95, -1.2, 0.1), float3(1.05, -1.5, 0.1));
                    d2 = h.x-0.03;
                    res.x = smin(res.x, d2, 0.02);
                    h = sdSegment(p, float3(1.05, -1.5, 0.1), float3(1., -1.75, 0.1)-0.01*fa);
                    d2 = h.x-0.03+0.01*h.y;
                    res.x = smin(res.x, d2, 0.02);
                    h = sdSegment(p, float3(0.95, -1.2, 0.1), float3(0.98, -1.5, 0.2));
                    d2 = h.x-0.03;
                    res.x = smin(res.x, d2, 0.03);
                    h = sdSegment(p, float3(0.98, -1.5, 0.2), float3(0.95, -1.7, 0.15)-0.01*fa);
                    d2 = h.x-0.03+0.01*h.y;
                    res.x = smin(res.x, d2, 0.03);
                    h = sdSegment(p, float3(0.95, -1.2, 0.1), float3(0.85, -1.4, 0.2));
                    d2 = h.x-0.04+0.01*h.y;
                    res.x = smin(res.x, d2, 0.05);
                    h = sdSegment(p, float3(0.85, -1.4, 0.2), float3(0.85, -1.63, 0.15)+0.01*fa);
                    d2 = h.x-0.03+0.01*h.y;
                    res.x = smin(res.x, d2, 0.03);
                }
                
                if (p.y<0.)
                {
                    h = sdSegment(p, float3(0.5, -0.5, 0.), float3(0.6, -1.2, 0.1));
                    d2 = h.x-0.14+h.y*0.08;
                    res.x = smin(res.x, d2, 0.06);
                    h = sdSegment(p, float3(0.6, -1.2, 0.1), float3(0.5, -1.8, 0.));
                    d2 = h.x-0.06;
                    res.x = smin(res.x, d2, 0.06);
                }
                
                if (p.y<-0.5)
                {
                    h = sdSegment(p, float3(0.5, -1.8, 0.), float3(0.6, -1.8, 0.4));
                    d2 = h.x-0.09+0.02*h.y;
                    res.x = smin(res.x, d2, 0.06);
                    h = sdSegment(p, float3(0.5, -1.8, 0.), float3(0.77, -1.8, 0.35));
                    d2 = h.x-0.08+0.02*h.y;
                    res.x = smin(res.x, d2, 0.06);
                    h = sdSegment(p, float3(0.5, -1.8, 0.), float3(0.9, -1.8, 0.2));
                    d2 = h.x-0.07+0.02*h.y;
                    res.x = smin(res.x, d2, 0.06);
                }
                
                float3 hp = p-float3(0.25, 0.7, 0.);
                hp.xy = mul(float2x2(0.6, 0.8, -0.8, 0.6),hp.xy);
                hp.x += 0.8*hp.y*hp.y;
                float d4 = sdEllipsoid(hp, float3(0.13, 0.5, 0.16));
                if (d4<res.x)
                    res = float2(d4, 3.);
                    
                float d3 = length((p-float3(0., 0.25, 0.35))*float3(1., 0.8, 1.))-0.5;
                if (d3<res.x)
                    res = float2(d3, 2.);
                    
                float mo = length((q-float3(0., -0.35, 1.))*float3(1., 1.2, 0.25)/1.2)-0.3/1.2;
                float of = 0.1*pow(smoothstep(0., 0.2, abs(p.x-0.3)), 0.5);
                mo = max(mo, -q.y-0.35-of);
                float li = smoothstep(0., 0.05, mo+0.02)-smoothstep(0.05, 0.1, mo+0.02);
                res.x -= 0.03*li*clamp((-q.y-0.4)*10., 0., 1.);
                if (-mo>res.x)
                    res = float2(-mo, 4.);
                    
                res.x += 0.01*(smoothstep(0., 0.05, mo+0.062)-smoothstep(0.05, 0.1, mo+0.062));
                if (p.x<0.3)
                {
                    p.x = glsl_mod(p.x, 0.16)-0.08;
                    float d5 = length((p-float3(0., -0.37, 0.65))*float3(1., 2., 1.))-0.08;
                    if (d5<res.x)
                        res = float2(d5, 2.);
                        
                }
                
                return float2(res.x*0.8, res.y);
            }

            float3 calcNormal(in float3 pos)
            {
#if 0    
                float3 eps = float3(0.002, 0., 0.);
                return normalize(float3(map(pos+eps.xyy).x-map(pos-eps.xyy).x, map(pos+eps.yxy).x-map(pos-eps.yxy).x, map(pos+eps.yyx).x-map(pos-eps.yyx).x));
#else
                float3 n = float3(0., 0., 0.);
                for (int i = ZERO;i<4; i++)
                {
                    float3 e = 0.5773*(2.*float3(i+3>>1&1, i>>1&1, i&1)-1.);
                    n += e*map(pos+0.002*e).x;
                }
                return normalize(n);
#endif
            }

            float3 intersect(in float3 ro, in float3 rd)
            {
                float m = -1.;
                float mint = 10.;
                float tf = (0.-ro.y)/rd.y;
                if (tf>0.)
                {
                    mint = tf;
                    m = 0.;
                }
                
                float maxd = min(5., mint);
                float precis = 0.001;
                float t = 0.;
                float d = 0.;
                for (int i = ZERO;i<128; i++)
                {
                    float2 res = map(ro+rd*t);
                    float h = res.x;
                    d = res.y;
                    if (h<precis||t>maxd)
                        break;
                        
                    t += h;
                }
                if (t<maxd&&t<mint)
                {
                    mint = t;
                    m = d;
                }
                
                return float3(mint, m, m);
            }

            float softshadow(in float3 ro, in float3 rd, float mint, float k)
            {
                float res = 1.;
                float t = mint;
                for (int i = ZERO;i<50; i++)
                {
                    float h = map(ro+rd*t).x;
                    res = min(res, smoothstep(0., 1., k*h/t));
                    t += clamp(h, 0.01, 0.25);
                    if (res<0.005||t>10.)
                        break;
                        
                }
                return clamp(res, 0., 1.);
            }

            static const float3 lig = normalize(float3(1., 0.7, 0.9));
            float4 frag (v2f i) : SV_Target
            {
                float4 fragColor = 0;
                float2 fragCoord = i.uv;
                float3 iResolution = 1;
                float2 p = (2.*fragCoord-iResolution.xy)/iResolution.y;
#ifdef STEREO
                float eyeID = glsl_mod(fragCoord.x+glsl_mod(fragCoord.y, 2.), 2.);
#endif
                float an = sin(-0.25+0.31416*_Time.y);
                float3 ro = float3(3.5*sin(an), 1.8, 3.5*cos(an));
                float3 ta = float3(0., 1.5, 0.);
                float3 ww = normalize(ta-ro);
                float3 uu = normalize(cross(ww, float3(0., 1., 0.)));
                float3 vv = normalize(cross(uu, ww));
                float3 rd = normalize(p.x*uu+p.y*vv+2.*ww);
#ifdef STEREO
                float3 fo = ro+rd*7.;
                ro -= 0.1*uu*eyeID;
                rd = normalize(fo-ro);
#endif
                float3 col = float3(1., 1., 1.);
                float3 tmat = intersect(ro, rd);
                if (tmat.z>-0.5)
                {
                    float3 pos = ro+tmat.x*rd;
                    float3 nor = calcNormal(pos);
                    float3 ref = reflect(rd, nor);
                    float4 mate = float4(0., 0., 0., 0.);
                    float2 mate2 = float2(1., 1.);
                    if (tmat.z<0.5)
                    {
                        nor = float3(0., 1., 0.);
                        ref = reflect(rd, nor);
                        mate.xyz = float3(1., 1., 1.);
                        mate2.y = 1.-0.9*(2./(2.+dot(pos.xz, pos.xz)));
                    }
                    else if (tmat.z<1.5)
                    {
                        mate2.x = 4.;
                        mate = float4(0.16, 0.32, 0., 0.8);
                        float f = texturize(_MainTex, 0.15*pos, nor).x*texturize(_MainTex, 0.2*0.25*pos, nor).x;
                        mate.xyz = lerp(0.56*mate.xyz, float3(0.21, 0.28, 0.), f);
                        f = texturize(_MainTex, pos, nor).x*texturize(_MainTex, 0.25*pos, nor).x;
                        f = f*f;
                        mate.xyz = lerp(mate.xyz, float3(0.21, 0.28, 0.), 0.6*f);
                        float3 bnor = -1.+2.*texturize(_MainTex, 4.*pos, nor);
                        nor = normalize(nor+0.15*bnor);
                    }
                    else if (tmat.z<2.5)
                    {
                        float3 q = pos-float3(0., 1.8, 0.)-float3(0., 0.44, 0.35);
                        float an2 = an+0.015*(-1.+2.*noise(6.*_Time.y));
                        float3 oq = q;
                        q.x += -2.*0.15*clamp(sin(an2), -0.42, 0.42);
                        float f = length(q.xy);
                        mate2.y = 1.-smoothstep(0.24, 0.35, length(oq.xy-float2(0., 0.035)));
                        mate = float4(0.5, 0.5, 0.5, 1.);
                        mate.xyz = lerp(mate.xyz, float3(0.6, 0.4, 0.3), 0.5*smoothstep(0.1, 0.4, f));
                        float3 c1 = float3(0., 0.15, 0.05)*(1.-0.85*f/0.19);
                        float a = atan2(q.y, -q.x);
                        float te = fbm(20.*float2(0.3*a, 1.*f));
                        c1 *= 0.3+5.*te;
                        c1 += float3(0.5, 1., 0.1)*0.35*(0.5+te)*(1.-smoothstep(0.3, 1.2, abs(a+0.5)))*(1.-smoothstep(0., 0.06, abs(f-0.125)));
                        mate.xyz = lerp(mate.xyz, c1, 1.-smoothstep(0.18, 0.19, f));
                        oq.x += -2.*0.19*clamp(sin(an2), -0.42, 0.42);
                        f = length(oq.xy);
                        mate.xyz *= smoothstep(0.07, 0.1, f);
                        mate.xyz *= 1.-0.2*float3(0.5, 0.9, 1.)*smoothstep(0.2, 0.4, f);
                        mate.w = 2.;
                        mate2.x = 64.;
                    }
                    else if (tmat.z<3.5)
                    {
                        mate = 0.8*float4(0.85, 0.7, 0.6, 0.);
                        mate2.x = 0.;
                        float f = smoothstep(0., 0.1, 0.5*abs(pos.x)+pos.y-3.02);
                        mate.xyz *= 1.-0.8*float3(f, f, f);
                        mate.xyz *= 0.2+0.8*smoothstep(0., 1., texturize(_MainTex, 0.1*2.1*pos*float3(4., 0.1, 4.), nor).x);
                    }
                    else if (tmat.z<4.5)
                    {
                        float z = smoothstep(0., 2., pos.z+0.5);
                        mate = 0.5*float4(0.5, 0.25, 0.1, 0.);
                        mate.x += 0.1*(1.-z);
                        mate2.y = z;
                    }
                    
                    float occ = (0.5+0.5*nor.y)*mate2.y;
                    float amb = 0.5;
                    float bou = clamp(-nor.y, 0., 1.);
                    float dif = max(dot(nor, lig), 0.);
                    float bac = max(0.3+0.7*dot(nor, -lig), 0.);
                    float sha = 0.;
                    if (dif>0.01)
                        sha = softshadow(pos+0.01*nor, lig, 0.001, 32.);
                        
                    float fre = pow(clamp(1.+dot(nor, rd), 0., 1.), 2.);
                    float3 hal = normalize(lig-rd);
                    float spe = max(pow(clamp(dot(nor, hal), 0., 1.), mate2.x*4.), 0.);
                    float3 lin = float3(0., 0., 0.);
                    lin += 2.*dif*float3(1., 1., 1.)*pow(float3(sha, sha, sha), float3(1., 1.2, 1.5));
                    lin += 1.*amb*float3(0.3, 0.3, 0.3)*occ;
                    lin += 2.*bou*float3(0.4, 0.4, 0.4)*mate2.y;
                    lin += 4.*bac*float3(0.4, 0.3, 0.25)*occ;
                    lin += 1.*fre*float3(1., 1., 1.)*2.*mate.w*(0.5+0.5*dif*sha)*occ;
                    col = mate.xyz*lin;
                    col += 4.*spe*float3(2., 2., 2.)*mate.w*dif*sha*(0.04+0.96*pow(clamp(1.+dot(hal, rd), 0., 1.), 5.));
                }
                
                col = pow(clamp(col, 0., 1.), float3(0.45, 0.45, 0.45));
                float2 q = fragCoord/iResolution.xy;
                col *= 0.5+0.5*pow(16.*q.x*q.y*(1.-q.x)*(1.-q.y), 0.25);
#ifdef STEREO
                col *= float3(eyeID, 1.-eyeID, 1.-eyeID);
#endif
                fragColor = float4(col, 1.);
                return fragColor;
            }

        ENDCG
        }
    }
}
