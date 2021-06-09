
Shader "Converted/Template"
{
    Properties
    {
        _MainTex ("iChannel0", 2D) = "white" {}
        _SecondTex ("iChannel1", 2D) = "white" {}
        _ThirdTex ("iChannel2", 2D) = "white" {}
        _FourthTex ("iChannel3", 2D) = "white" {}
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

            sampler2D _MainTex;
            sampler2D _SecondTex;
            sampler2D _ThirdTex;
            sampler2D _FourthTex;

            #define glsl_mod(x,y) (((x)-(y)*floor((x)/(y)))) 

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv =  v.uv;
                return o;
            }

            #define shape(p) length(p)-2.8
            static const float2 RMPrec = float2(0.2, 0.001);
            static const float3 DPrec = float3(0.005, 12., 0.000001);
            float Voronesque(in float3 p)
            {
                float3 i = floor(p+dot(p, 0.333333));
                p -= i-dot(i, 0.166666);
                float3 i1 = step(0., p-p.yzx), i2 = max(i1, 1.-i1.zxy);
                i1 = min(i1, 1.-i1.zxy);
                float3 p1 = p-i1+0.166666, p2 = p-i2+0.333333, p3 = p-0.5;
                float3 rnd = float3(7, 157, 113);
                float4 v = max(0.5-float4(dot(p, p), dot(p1, p1), dot(p2, p2), dot(p3, p3)), 0.);
                float4 d = float4(dot(i, rnd), dot(i+i1, rnd), dot(i+i2, rnd), dot(i+1., rnd));
                d = frac(sin(d)*262144.)*v*2.;
                v.x = max(d.x, d.y), (v.y = max(d.z, d.w), (v.z = max(min(d.x, d.y), min(d.z, d.w)), v.w = min(v.x, v.y)));
                return max(v.x, v.y)-max(v.z, v.w);
            }

            float2 map(float3 p)
            {
                float2 res = 0.;
                float voro = Voronesque(p);
                float3 col = voro*0.5;
                float sphere = shape(p);
                float sphereOut = sphere-voro;
                float sphereIn = sphere+voro*0.5;
                float dist = max(-sphereIn, sphereOut+0.29);
                res = float2(dist, 1.);
                float kernel = sphere+2.2;
                if (kernel<res.x)
                    res = float2(kernel, 2.);
                    
                return res;
            }

            float3 nor(float3 pos, float prec)
            {
                float2 e = float2(prec, 0.);
                float3 n = float3(map(pos+e.xyy).x-map(pos-e.xyy).x, map(pos+e.yxy).x-map(pos-e.yxy).x, map(pos+e.yyx).x-map(pos-e.yyx).x);
                return normalize(n);
            }

            float3 cam(float2 uv, float3 ro, float3 cu, float3 cv)
            {
                float3 rov = normalize(cv-ro);
                float3 u = normalize(cross(cu, rov));
                float3 v = normalize(cross(rov, u));
                float3 rd = normalize(rov+u*uv.x+v*uv.y);
                return rd;
            }

            float3 blackbody(float Temp)
            {
                float3 col = 255.;
                col.x = 56100000.*pow(Temp, -3./2.)+148.;
                col.y = 100.04*log(Temp)-623.6;
                if (Temp>6500.)
                    col.y = 35200000.*pow(Temp, -3./2.)+184.;
                    
                col.z = 194.18*log(Temp)-1448.6;
                col = clamp(col, 0., 255.)/255.;
                if (Temp<1000.)
                    col *= Temp/1000.;
                    
                return col;
            }

            static const float3 LPos = float3(-0.6, 0.7, -0.5);
            static const float3 LAmb = 0.;
            static const float3 LDif = float3(1., 0.5, 0.);
            static const float3 LSpe = 0.8;
            static const float3 MAmb = 0.;
            static const float3 MDif = float3(1., 0.5, 0.);
            static const float3 MSpe = float3(0.6, 0.6, 0.6);
            static const float MShi = 30.;
            float3 ads(float3 p, float3 n)
            {
                float3 ldif = normalize(LPos-p);
                float3 vv = normalize(0.-p);
                float3 refl = reflect(0.-ldif, n);
                float3 amb = MAmb*LAmb+blackbody(2000.);
                float3 dif = max(0., dot(ldif, n.xyz))*MDif*LDif;
                float3 spe = 0.;
                if (dot(ldif, vv)>0.)
                    spe = pow(max(0., dot(vv, refl)), MShi)*MSpe*LSpe;
                    
                return amb*1.2+dif*1.5+spe*0.8;
            }

            float3 nrand3(float2 co)
            {
                float3 a = frac(cos(co.x*0.0083+co.y)*float3(130000., 470000., 290000.));
                float3 b = frac(sin(co.x*0.0003+co.y)*float3(810000., 100000., 10000.));
                float3 c = lerp(a, b, 0.5);
                return c;
            }

            float4 frag (v2f i) : SV_Target
            {
                float4 f = 0;
                float2 g = i.uv;
                float3 iResolution = 1;
                float2 si = iResolution.xy;
                float t = _Time.y;
                float ca = t*0.14;
                float ce = 1.;
                float cd = 1.;
                float3 cu = float3(0, 1, 0);
                float3 cv = float3(0, 0, 0);
                float2 uv = (g+g-si)/min(si.x, si.y);
                float3 ro = float3(sin(ca)*cd, ce+1., cos(ca)*cd);
                float3 rd = cam(uv, ro, cu, cv);
                float3 d = 0.;
                float3 p = ro+rd*d.x;
                float2 s = DPrec.y;
                for (int i = 0;i<200; i++)
                {
                    if (s.x<DPrec.x||s.x>DPrec.y)
                        break;
                        
                    s = map(p);
                    s.x *= s.x>DPrec.x ? RMPrec.x : RMPrec.y;
                    d.x += s.x;
                    p = ro+rd*d.x;
                }
                if (d.x<DPrec.y)
                {
                    float nPrec = 0.1;
                    float3 n = nor(p, nPrec);
                    if (s.y<1.5)
                    {
                        float3 SSS = ads(n, n)-ads(p, rd);
                        SSS += blackbody(1500.*(d.x-3.));
                        f.rgb = SSS;
                    }
                    else if (s.y<2.5)
                    {
                        float b = dot(n, normalize(ro-p))*0.9;
                        f = (b*float4(blackbody(2000.), 0.8)+pow(b, 0.2))*(1.-d.x*0.01);
                    }
                    
                }
                else 
                {
                    float3 rnd = nrand3(floor(uv*2.*iResolution.x));
                    f = pow(rnd.y, 10.);
                }
                return f;
            }

        ENDCG
        }
    }
}