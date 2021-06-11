
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

            float2x2 rot(in float a)
            {
                float c = cos(a), s = sin(a);
                return float2x2(c, s, -s, c);
            }

            static const float3x3 m3 = float3x3(0.33338, 0.56034, -0.71817, -0.87887, 0.32651, -0.15323, 0.15162, 0.69596, 0.61339)*1.93;
            float mag2(float2 p)
            {
                return dot(p, p);
            }

            float linstep(in float mn, in float mx, in float x)
            {
                return clamp((x-mn)/(mx-mn), 0., 1.);
            }

            static float prm1 = 0.;
            static float2 bsMo = float2(0, 0);
            float2 disp(float t)
            {
                return float2(sin(t*0.22)*1., cos(t*0.175)*1.)*2.;
            }

            float2 map(float3 p)
            {
                float3 p2 = p;
                p2.xy -= disp(p.z).xy;
                p.xy = mul(rot(sin(p.z+_Time.y)*(0.1+prm1*0.05)+_Time.y*0.09),p.xy);
                float cl = mag2(p2.xy);
                float d = 0.;
                p *= 0.61;
                float z = 1.;
                float trk = 1.;
                float dspAmp = 0.1+prm1*0.2;
                for (int i = 0;i<5; i++)
                {
                    p += sin(p.zxy*0.75*trk+_Time.y*trk*0.8)*dspAmp;
                    d -= abs(dot(cos(p), sin(p.yzx))*z);
                    z *= 0.57;
                    trk *= 1.4;
                    p = mul(p,m3);
                }
                d = abs(d+prm1*3.)+prm1*0.3-2.5+bsMo.y;
                return float2(d+cl*0.2+0.25, cl);
            }

            float4 render(in float3 ro, in float3 rd, float time)
            {
                float4 rez = float4(0, 0, 0, 0);
                const float ldst = 8.;
                float3 lpos = float3(disp(time+ldst)*0.5, time+ldst);
                float t = 1.5;
                float fogT = 0.;
                for (int i = 0;i<130; i++)
                {
                    if (rez.a>0.99)
                        break;
                        
                    float3 pos = ro+t*rd;
                    float2 mpv = map(pos);
                    float den = clamp(mpv.x-0.3, 0., 1.)*1.12;
                    float dn = clamp(mpv.x+2., 0., 3.);
                    float4 col = float4(0, 0, 0, 0);
                    if (mpv.x>0.6)
                    {
                        col = float4(sin(float3(5., 0.4, 0.2)+mpv.y*0.1+sin(pos.z*0.4)*0.5+1.8)*0.5+0.5, 0.08);
                        col *= den*den*den;
                        col.rgb *= linstep(4., -2.5, mpv.x)*2.3;
                        float dif = clamp((den-map(pos+0.8).x)/9., 0.001, 1.);
                        dif += clamp((den-map(pos+0.35).x)/2.5, 0.001, 1.);
                        col.xyz *= den*(float3(0.005, 0.045, 0.075)+1.5*float3(0.033, 0.07, 0.03)*dif);
                    }
                    
                    float fogC = exp(t*0.2-2.2);
                    col.rgba += float4(0.06, 0.11, 0.11, 0.1)*clamp(fogC-fogT, 0., 1.);
                    fogT = fogC;
                    rez = rez+col*(1.-rez.a);
                    t += clamp(0.5-dn*dn*0.05, 0.09, 0.3);
                }
                return clamp(rez, 0., 1.);
            }

            float getsat(float3 c)
            {
                float mi = min(min(c.x, c.y), c.z);
                float ma = max(max(c.x, c.y), c.z);
                return (ma-mi)/(ma+0.0000001);
            }

            float3 iLerp(in float3 a, in float3 b, in float x)
            {
                float3 ic = lerp(a, b, x)+float3(0.000001, 0., 0.);
                float sd = abs(getsat(ic)-lerp(getsat(a), getsat(b), x));
                float3 dir = normalize(float3(2.*ic.x-ic.y-ic.z, 2.*ic.y-ic.x-ic.z, 2.*ic.z-ic.y-ic.x));
                float lgt = dot(float3(1., 1., 1.), ic);
                float ff = dot(dir, normalize(ic));
                ic += 1.5*dir*sd*ff*lgt;
                return clamp(ic, 0., 1.);
            }

            float4 frag (v2f i) : SV_Target
            {
                float4 fragColor = 0;
                float2 fragCoord = i.uv;
                float3 iResolution = 1;
                float2 q = fragCoord.xy/iResolution.xy;
                float2 p = (i.uv.xy-0.5*iResolution.xy)/iResolution.y;
                bsMo = (_Mouse.xy-0.5*iResolution.xy)/iResolution.y;
                float time = _Time.y*3.;
                float3 ro = float3(0, 0, time);
                ro += float3(sin(_Time.y)*0.5, sin(_Time.y*1.)*0., 0);
                float dspAmp = 0.85;
                ro.xy += disp(ro.z)*dspAmp;
                float tgtDst = 3.5;
                float3 target = normalize(ro-float3(disp(time+tgtDst)*dspAmp, time+tgtDst));
                ro.x -= bsMo.x*2.;
                float3 rightdir = normalize(cross(target, float3(0, 1, 0)));
                float3 updir = normalize(cross(rightdir, target));
                rightdir = normalize(cross(updir, target));
                float3 rd = normalize((p.x*rightdir+p.y*updir)*1.-target);
                rd.xy = mul(rot(-disp(time+3.5).x*0.2+bsMo.x),rd.xy);
                prm1 = smoothstep(-0.4, 0.4, sin(_Time.y*0.3));
                float4 scn = render(ro, rd, time);
                float3 col = scn.rgb;
                col = iLerp(col.bgr, col.rgb, clamp(1.-prm1, 0.05, 1.));
                col = pow(col, float3(0.55, 0.65, 0.6))*float3(1., 0.97, 0.9);
                col *= pow(16.*q.x*q.y*(1.-q.x)*(1.-q.y), 0.12)*0.7+0.3;
                fragColor = float4(col, 1.);
                return fragColor;
            }

        ENDCG
        }
    }
}
