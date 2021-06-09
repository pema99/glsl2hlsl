
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

            #define iMouse float2(_Time.y, _Time.y)
            #define glsl_mod(x,y) (((x)-(y)*floor((x)/(y)))) 

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv =  v.uv;
                return o;
            }

            #define I_MAX 100
            #define E 0.001
            #define LPOS vec3(0., 0., 0.)
            #define L2 vec3(+cos(t*1.)*3.,2.0,-0.+sin(t*1.)*3.)
            void rotate(inout float2 v, float angle);
            float sdTorus(float3 p, float2 t);
            float2 march(float3 pos, float3 dir);
            float3 camera(float2 uv);
            float3 blackbody(float Temp);
            float scene(float3 p);
            static float t;
            static float3 h;
            static const float3 lightCol = float3(1., 0.7, 0.51);
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

            float sdCappedCylinder(float3 p, float2 h)
            {
                float2 d = abs(float2(length(p.xy), p.z))-h;
                return min(max(d.x, d.y), 0.)+length(max(d, 0.));
            }

            float3 evaluateLight(in float3 pos)
            {
                float distanceToL = length(LPOS-pos);
                float distanceToL2 = length(L2-pos);
                return +lightCol*1./(distanceToL2*distanceToL2);
            }

            float spiral(float2 m)
            {
                float r = length(m);
                float a = atan2(m.y, m.x);
                float v = sin(100.*(sqrt(r)-0.02*a-0.3*t*1.));
                return clamp(v, 0., 1.);
            }

            float de_0(float3 p)
            {
                float ming = 100000.;
                float a = (t+p.z*0.515)*1.;
                float3 pr = p;
                rotate(pr.yz, t);
                ming = length(pr.zy)-1.5+0.051*spiral(0.05*pr.xy)-(0.5*length(pr)-2.1);
                return ming;
            }

            float scene(float3 p)
            {
                float mind = length(p-L2)-0.1;
                p.y += 2.;
                mind = min(mind, de_0(float3(p.x, p.z, p.y)));
                return mind;
            }

            float2 march(float3 pos, float3 dir)
            {
                float2 dist = 0.;
                float3 p = 0.;
                float2 s = 0.;
                float3 dirr;
                for (int i = 0;i<I_MAX; ++i)
                {
                    dirr = dir;
                    rotate(dirr.zx, 0.05*dist.y);
                    p = pos+dirr*dist.y;
                    dist.x = scene(p);
                    dist.y += dist.x;
                    h += evaluateLight(p);
                    if (dist.x<E)
                    {
                        break;
                    }
                    
                    (s.x)++;
                }
                s.y = dist.y;
                return s;
            }

            float4 frag (v2f i) : SV_Target
            {
                float4 o = 0;
                float2 f = i.uv;
                float3 iResolution = 1;
                o -= o;
                h = 0.;
                t = _Time.y*0.5;
                float2 R = iResolution.xy, uv = f-R/2./R.y;
                float3 dir = camera(uv);
                float3 pos = float3(0., 0., 7.);
                float2 inter = march(pos, dir);
                o.xyz += h*1.;
                o.xyz += 0.25*blackbody(inter.y*150.);
                return o;
            }
            float sdTorus(float3 p, float2 t)
            {
                float2 q = float2(length(p.zy)-t.x, p.x);
                return length(q)-t.y;
            }

            float3 camera(float2 uv)
            {
                float fov = 1.;
                float3 forw = float3(0., 0., -1.);
                float3 right = float3(1., 0., 0.);
                float3 up = float3(0., 1., 0.);
                return normalize(uv.x*right+uv.y*up+fov*forw);
            }

            void rotate(inout float2 v, float angle)
            {
                v = float2(cos(angle)*v.x+sin(angle)*v.y, -sin(angle)*v.x+cos(angle)*v.y);
            }


        ENDCG
        }
    }
}