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
        _Resolution ("Resolution (Change if AA is bad)", Range(1, 1024)) = 1

        [Header(Raymarching)]
        [ToggleUI] _WorldSpace ("World Space Marching", Float) = 0
        _Offset ("Offset (W=Scale)", Vector) = (0, 0, 0, 1)
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

            // Built-in properties
            sampler2D _MainTex;   float4 _MainTex_TexelSize;
            sampler2D _SecondTex; float4 _SecondTex_TexelSize;
            sampler2D _ThirdTex;  float4 _ThirdTex_TexelSize;
            sampler2D _FourthTex; float4 _FourthTex_TexelSize;
            float4 _Mouse;
            float _GammaCorrect;
            float _Resolution;
            float _WorldSpace;
            float4 _Offset;

            // GLSL Compatability macros
            #define glsl_mod(x,y) (((x)-(y)*floor((x)/(y))))
            #define texelFetch(ch, uv, lod) tex2Dlod(ch, float4((uv).xy * ch##_TexelSize.xy + ch##_TexelSize.xy * 0.5, 0, lod))
            #define textureLod(ch, uv, lod) tex2Dlod(ch, float4(uv, 0, lod))
            #define iResolution float3(_Resolution, _Resolution, _Resolution)
            #define iFrame (floor(_Time.y / 60))
            #define iChannelTime float4(_Time.y, _Time.y, _Time.y, Time.y)
            #define iDate float4(2020, 6, 18, 30)
            #define iSampleRate (44100)
            #define iChannelResolution float4x4(                      \
                _MainTex_TexelSize.z,   _MainTex_TexelSize.w,   0, 0, \
                _SecondTex_TexelSize.z, _SecondTex_TexelSize.w, 0, 0, \
                _ThirdTex_TexelSize.z,  _ThirdTex_TexelSize.w,  0, 0, \
                _FourthTex_TexelSize.z, _FourthTex_TexelSize.w, 0, 0)

            // Global access to uv data
            static v2f vertex_output;

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

            float3 palette(float d)
            {
                return lerp(float3(0.2, 0.7, 0.9), float3(1., 0., 1.), d);
            }

            float2 rotate(float2 p, float a)
            {
                float c = cos(a);
                float s = sin(a);
                return mul(p,float2x2(c, s, -s, c));
            }

            float map(float3 p)
            {
                for (int i = 0;i<8; ++i)
                {
                    float t = _Time.y*0.2;
                    p.xz = rotate(p.xz, t);
                    p.xy = rotate(p.xy, t*1.89);
                    p.xz = abs(p.xz);
                    p.xz -= 0.5;
                }
                return dot(sign(p), p)/5.;
            }

            float4 rm(float3 ro, float3 rd)
            {
                float t = 0.;
                float3 col = ((float3)0.);
                float d;
                for (float i = 0.;i<64.; i++)
                {
                    float3 p = ro+rd*t;
                    d = map(p)*0.5;
                    if (d<0.02)
                    {
                        break;
                    }
                    
                    if (d>100.)
                    {
                        break;
                    }
                    
                    col += palette(length(p)*0.1)/(400.*d);
                    t += d;
                }
                return float4(col, 1./(d*100.));
            }

            float4 frag (v2f __vertex_output, float facing : VFACE) : SV_Target
            {
                vertex_output = __vertex_output;
                float4 fragColor = 0;
                float2 fragCoord = vertex_output.uv * _Resolution;
                float2 uv = (fragCoord-iResolution.xy/2.)/iResolution.x;
                float3 ro = ((facing>0 ? vertex_output.hitPos_w : vertex_output.ro_w)+_Offset)*_Offset.w;
                ro.xz = rotate(ro.xz, _Time.y);
                float3 cf = normalize(-ro);
                float3 cs = normalize(cross(cf, float3(0., 1., 0.)));
                float3 cu = normalize(cross(cf, cs));
                float3 uuv = ro+cf*3.+uv.x*cs+uv.y*cu;
                float3 rd = normalize(vertex_output.hitPos_w-vertex_output.ro_w);
                float4 col = rm(ro, rd);
                fragColor = col;
                if (_GammaCorrect) fragColor.rgb = pow(fragColor.rgb, 2.2);
                return fragColor;
            }
            ENDCG
        }
    }
}
