Shader "Converted/Template"
{
    Properties
    {
        [Header(General)]
        _MainTex ("iChannel0", 2D) = "white" {}
        _SecondTex ("iChannel1", 2D) = "white" {}
        _ThirdTex ("iChannel2", 2D) = "white" {}
        _FourthTex ("iChannel3", 2D) = "white" {}
        _Mouse ("Mouse", Vector) = (0.5, 0.5, 0.5, 0.5)
        [ToggleUI] _GammaCorrect ("Gamma Correction", Float) = 1

        [Header(Raymarching)]
        [ToggleUI] _WorldSpace ("World Space Marching", Float) = 0
        _Offset ("Offset (W=Scale)", Vector) = (0, 0, 0, 1)

        [Header(Extracted)]
        RAY_STEPS ("RAY_STEPS", Float) = 150
        BRIGHTNESS ("BRIGHTNESS", Float) = 1.2
        GAMMA ("GAMMA", Float) = 1.4
        SATURATION ("SATURATION", Float) = 0.65
        detail ("detail", Float) = 0.001
        origin ("origin", Vector) = (-1,0.7,0)

    }
    SubShader
    {
        Pass
        {
            Cull Off

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
                float3 ro_w : TEXCOORD1;
                float3 hitPos_w : TEXCOORD2;
            };

            sampler2D _MainTex;   float4 _MainTex_TexelSize;
            sampler2D _SecondTex; float4 _SecondTex_TexelSize;
            sampler2D _ThirdTex;  float4 _ThirdTex_TexelSize;
            sampler2D _FourthTex; float4 _FourthTex_TexelSize;
            float4 _Mouse;
            float _GammaCorrect;
            float _WorldSpace;
            float4 _Offset;

            // GLSL Compatability macros
            #define iFrame (floor(_Time.y / 60))
            #define iResolution float3(1, 1, 1)
            #define glsl_mod(x,y) (((x)-(y)*floor((x)/(y))))
            #define texelFetch(ch, uv, lod) tex2Dlod(ch, float4((uv).xy * ch##_TexelSize.xy + ch##_TexelSize.xy * 0.5, 0, lod))
            #define textureLod(ch, uv, lod) tex2Dlod(ch, float4(uv, 0, lod))

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv =  v.uv;

                if (_WorldSpace)
                {
                    o.ro_w = _WorldSpaceCameraPos;
                    o.hitPos_w = mul(unity_ObjectToWorld, v.vertex);
                }
                else
                {
                    o.ro_w = mul(unity_WorldToObject, float4(_WorldSpaceCameraPos, 1));
                    o.hitPos_w = v.vertex;
                }

                return o;
            }

#define NYAN 
#define WAVES 
#define BORDER 
float RAY_STEPS;
float BRIGHTNESS;
float GAMMA;
float SATURATION;
float detail;
#define t _Time.y*0.5
            const float3 origin;
            static float det = 0.;
            float2x2 rot(float a)
            {
                return float2x2(cos(a), sin(a), -sin(a), cos(a));
            }

            float4 formula(float4 p)
            {
                p.xz = abs(p.xz+1.)-abs(p.xz-1.)-p.xz;
                p.y -= 0.25;
                p.xy = mul(rot(radians(35.)),p.xy);
                p = p*2./clamp(dot(p.xyz, p.xyz), 0.2, 1.);
                return p;
            }

            float de(float3 pos)
            {
#ifdef WAVES
                pos.y += sin(pos.z-t*6.)*0.15;
#endif
                float hid = 0.;
                float3 tpos = pos;
                tpos.z = abs(3.-glsl_mod(tpos.z, 6.));
                float4 p = float4(tpos, 1.);
                for (int i = 0;i<4; i++)
                {
                    p = formula(p);
                }
                float fr = (length(max(((float2)0.), p.yz-1.5))-1.)/p.w;
                float ro = max(abs(pos.x+1.)-0.3, pos.y-0.35);
                ro = max(ro, -max(abs(pos.x+1.)-0.1, pos.y-0.5));
                pos.z = abs(0.25-glsl_mod(pos.z, 0.5));
                ro = max(ro, -max(abs(pos.z)-0.2, pos.y-0.3));
                ro = max(ro, -max(abs(pos.z)-0.01, -pos.y+0.32));
                float d = min(fr, ro);
                return d;
            }

            float3 path(float ti)
            {
                ti *= 1.5;
                float3 p = float3(sin(ti), (1.-sin(ti*2.))*0.5, -ti*5.)*0.5;
                return p;
            }

            static float edge = 0.;
            float3 normal(float3 p)
            {
                float3 e = float3(0., det*5., 0.);
                float d1 = de(p-e.yxx), d2 = de(p+e.yxx);
                float d3 = de(p-e.xyx), d4 = de(p+e.xyx);
                float d5 = de(p-e.xxy), d6 = de(p+e.xxy);
                float d = de(p);
                edge = abs(d-0.5*(d2+d1))+abs(d-0.5*(d4+d3))+abs(d-0.5*(d6+d5));
                edge = min(1., pow(edge, 0.55)*15.);
                return normalize(float3(d1-d2, d3-d4, d5-d6));
            }

            float4 rainbow(float2 p)
            {
                float q = max(p.x, -0.1);
                float s = sin(p.x*7.+t*70.)*0.08;
                p.y += s;
                p.y *= 1.1;
                float4 c;
                if (p.x>0.)
                c = float4(0, 0, 0, 0);
                else if (0./6.<p.y&&p.y<1./6.)
                c = float4(255, 43, 14, 255)/255.;
                else if (1./6.<p.y&&p.y<2./6.)
                c = float4(255, 168, 6, 255)/255.;
                else if (2./6.<p.y&&p.y<3./6.)
                c = float4(255, 244, 0, 255)/255.;
                else if (3./6.<p.y&&p.y<4./6.)
                c = float4(51, 234, 5, 255)/255.;
                else if (4./6.<p.y&&p.y<5./6.)
                c = float4(8, 163, 255, 255)/255.;
                else if (5./6.<p.y&&p.y<6./6.)
                c = float4(122, 85, 255, 255)/255.;
                else if (abs(p.y)-0.05<0.0001)
                c = float4(0., 0., 0., 1.);
                else if (abs(p.y-1.)-0.05<0.0001)
                c = float4(0., 0., 0., 1.);
                else c = float4(0, 0, 0, 0);
                c.a *= 0.8-min(0.8, abs(p.x*0.08));
                c.xyz = lerp(c.xyz, ((float3)length(c.xyz)), 0.15);
                return c;
            }

            float4 nyan(float2 p)
            {
                float2 uv = p*float2(0.4, 1.);
                float ns = 3.;
                float nt = _Time.y*ns;
                nt -= glsl_mod(nt, 240./256./6.);
                nt = glsl_mod(nt, 240./256.);
                float ny = glsl_mod(_Time.y*ns, 1.);
                ny -= glsl_mod(ny, 0.75);
                ny *= -0.05;
                float4 color = tex2D(_SecondTex, float2(uv.x/3.+210./256.-nt+0.05, 0.5-uv.y-ny));
                if (uv.x<-0.3)
                    color.a = 0.;
                    
                if (uv.x>0.2)
                    color.a = 0.;
                    
                return color;
            }

            float3 raymarch(in float3 from, in float3 dir)
            {
                edge = 0.;
                float3 p, norm;
                float d = 100.;
                float totdist = 0.;
                for (int i = 0;i<RAY_STEPS; i++)
                {
                    if (d>det&&totdist<25.)
                    {
                        p = from+totdist*dir;
                        d = de(p);
                        det = detail*exp(0.13*totdist);
                        totdist += d;
                    }
                    
                }
                float3 col = ((float3)0.);
                p -= (det-d)*dir;
                norm = normal(p);
#ifdef SHOWONLYEDGES
                col = 1.-((float3)edge);
#else
                col = (1.-abs(norm))*max(0., 1.-edge*0.8);
#endif
                totdist = clamp(totdist, 0., 26.);
                dir.y -= 0.02;
                float sunsize = 7.-max(0., tex2D(_MainTex, float2(0.6, 0.2)).x)*5.;
                float an = atan2(dir.x, dir.y)+_Time.y*1.5;
                float s = pow(clamp(1.-length(dir.xy)*sunsize-abs(0.2-glsl_mod(an, 0.4)), 0., 1.), 0.1);
                float sb = pow(clamp(1.-length(dir.xy)*(sunsize-0.2)-abs(0.2-glsl_mod(an, 0.4)), 0., 1.), 0.1);
                float sg = pow(clamp(1.-length(dir.xy)*(sunsize-4.5)-0.5*abs(0.2-glsl_mod(an, 0.4)), 0., 1.), 3.);
                float y = lerp(0.45, 1.2, pow(smoothstep(0., 1., 0.75-dir.y), 2.))*(1.-sb*0.5);
                float3 backg = float3(0.5, 0., 1.)*((1.-s)*(1.-sg)*y+(1.-sb)*sg*float3(1., 0.8, 0.15)*3.);
                backg += float3(1., 0.9, 0.1)*s;
                backg = max(backg, sg*float3(1., 0.9, 0.5));
                col = lerp(float3(1., 0.9, 0.3), col, exp(-0.004*totdist*totdist));
                if (totdist>25.)
                    col = backg;
                    
                col = pow(col, ((float3)GAMMA))*BRIGHTNESS;
                col = lerp(((float3)length(col)), col, SATURATION);
#ifdef SHOWONLYEDGES
                col = 1.-((float3)length(col));
#else
                col *= float3(1., 0.9, 0.85);
#ifdef NYAN
                dir.yx = mul(rot(dir.x),dir.yx);
                float2 ncatpos = dir.xy+float2(-3.+glsl_mod(-t, 6.), -0.27);
                float4 ncat = nyan(ncatpos*5.);
                float4 rain = rainbow(ncatpos*10.+float2(0.8, 0.5));
                if (totdist>8.)
                    col = lerp(col, max(((float3)0.2), rain.xyz), rain.a*0.9);
                    
                if (totdist>8.)
                    col = lerp(col, max(((float3)0.2), ncat.xyz), ncat.a*0.9);
                    
#endif
#endif
                return col;
            }

            float3 move(inout float3 dir)
            {
                float3 go = path(t);
                float3 adv = path(t+0.7);
                float hd = de(adv);
                float3 advec = normalize(adv-go);
                float an = adv.x-go.x;
                an *= min(1., abs(adv.z-go.z))*sign(adv.z-go.z)*0.7;
                dir.xy = mul(float2x2(cos(an), sin(an), -sin(an), cos(an)),dir.xy);
                an = advec.y*1.7;
                dir.yz = mul(float2x2(cos(an), sin(an), -sin(an), cos(an)),dir.yz);
                an = atan2(advec.x, advec.z);
                dir.xz = mul(float2x2(cos(an), sin(an), -sin(an), cos(an)),dir.xz);
                return go;
            }

            float4 frag (v2f vertex_output, float facing : VFACE) : SV_Target
            {
                float4 fragColor = 0;
                float2 fragCoord = vertex_output.uv;
                float2 uv = fragCoord.xy/iResolution.xy*2.-1.;
                float2 oriuv = uv;
                uv.y *= iResolution.y/iResolution.x;
                float2 mouse = (_Mouse.xy/iResolution.xy-0.5)*3.;
                if (_Mouse.z<1.)
                    mouse = float2(0., -0.05);
                    
                float fov = 0.9-max(0., 0.7-_Time.y*0.3);
                float3 dir = normalize(vertex_output.hitPos_w-vertex_output.ro_w);
                dir.yz = mul(rot(mouse.y),dir.yz);
                dir.xz = mul(rot(mouse.x),dir.xz);
                float3 from = ((facing>0 ? vertex_output.hitPos_w : vertex_output.ro_w)+_Offset)*_Offset.w;
                float3 color = raymarch(from, dir);
#ifdef BORDER
                color = lerp(((float3)0.), color, pow(max(0., 0.95-length(oriuv*oriuv*oriuv*float2(1.05, 1.1))), 0.3));
#endif
                fragColor = float4(color, 1.);
                if (_GammaCorrect) fragColor.rgb = pow(fragColor.rgb, 2.2);
                return fragColor;
            }
            ENDCG
        }
    }
}
