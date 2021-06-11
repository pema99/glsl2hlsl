
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

            float bayer(int2 uv)
            {
                return texelFetch(_MainTex, uv%8, 0).x;
            }

#define FK(k) floatBitsToInt(cos(k))^floatBitsToInt(k)
            float hash(float2 k)
            {
                int x = FK(k.x);
                int y = FK(k.y);
                return float((x*x-y)*(y*y+x)-x)/2140000000.;
            }

            float hash3(float3 k)
            {
                float h1 = hash(k.xy);
                return hash(float2(h1, k.z));
            }

            float3 hash33(float3 k)
            {
                float h1 = hash3(k);
                float h2 = hash3(k*h1);
                float h3 = hash3(k*h2);
                return float3(h1, h2, h3);
            }

            float smin(float a, float b, float k)
            {
                float h = max(k-abs(a-b), 0.)/k;
                return min(a, b)-h*h*h*k*(1./6.);
            }

            float3 sphercoord(float2 p)
            {
                float l1 = acos(p.x);
                float l2 = acos(-1.)*p.y;
                return float3(cos(l1), sin(l1)*sin(l2), sin(l1)*cos(l2));
            }

            float3 erot(float3 p, float3 ax, float ro)
            {
                return lerp(dot(p, ax)*ax, p, cos(ro))+sin(ro)*cross(p, ax);
            }

            float comp(float3 p, float3 ro, float t)
            {
                float3 ax = sphercoord(ro.xy);
                p.z -= t;
                p = erot(p, ax, ro.z*acos(-1.));
                float scale = 4.+hash(ro.xz)*0.5+0.5;
                p = (frac(p/scale)-0.5)*scale;
                return length(p)-0.8;
            }

            float scene(float3 p)
            {
                float rad = 3.+p.z+sin(p.y/2.+_Time.y)+cos(p.x/3.+_Time.y*0.9);
                float dist = 10000.;
                for (int i = 0;i<4; i++)
                {
                    float3 rot = hash33(float3(float(i+1), cos(float(i)), sin(float(i))));
                    float d = comp(p, rot, _Time.y/2.*float(i+1));
                    dist = smin(dist, d, 1.);
                }
                return lerp(dist, rad, lerp(0.3, 0.8+sin(_Time.y)*0.2, 0.1));
            }

            float3 norm(float3 p)
            {
                float3x3 k = float3x3(p, p, p)-float3x3(0.1);
                return normalize(scene(p)-float3(scene(k[0]), scene(k[1]), scene(k[2])));
            }

            float march(float3 p, float3 bias, float seed)
            {
                for (int i = 0;i<10; i++)
                {
                    p += normalize(bias+tan(hash33(float3(float(i), seed, 2.))))*scene(p);
                }
                return sqrt(smoothstep(0., 2., scene(p)));
            }

            float4 frag (v2f i) : SV_Target
            {
                float4 fragColor = 0;
                float2 fragCoord = i.uv;
                float3 iResolution = 1;
                float2 uv = float2(fragCoord.x/iResolution.x, fragCoord.y/iResolution.y);
                uv -= 0.5;
                uv /= float2(iResolution.y/iResolution.x, 1);
                float3 cam = normalize(float3(4., uv));
                float3 init = float3(-50, 0, sin(_Time.y*0.37)*1.4);
                cam = erot(cam, float3(0, 1, 0), -0.5);
                init = erot(init, float3(0, 1, 0), -0.5);
                float3 p = init;
                bool hit = false;
                bool trig = false;
                bool outline = false;
                for (int i = 0;i<500&&!hit; i++)
                {
                    float dist = scene(p);
                    if (dist<0.08)
                        trig = true;
                        
                    if (trig)
                    {
                        float odist = 0.09-dist;
                        outline = odist<dist;
                        dist = min(dist, odist);
                    }
                    
                    hit = dist*dist<0.000001;
                    p += dist*cam;
                }
                float3 n = norm(p);
                float3 r = reflect(cam, n);
                float2 ao = (0);
                for (int i = 0;i<8; i++)
                {
                    int2 id = (fragCoord/16.+float2(_Time.y*10., _Time.y*20.))%2*2-1;
                    ao += float2(march(p+n*0.1, r, bayer((fragCoord)+i+int2(i/4, 0))), 1.);
                }
                ao.x /= ao.y;
                fragColor.xyz = hit&&!outline ? (ao.x) : (0.);
                fragColor.xyz = pow(smoothstep((0), (1), sqrt(fragColor.xyz)), float3(1.7, 1.6, 1.5));
                return fragColor;
            }

        ENDCG
        }
    }
}
