
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
            #define iResolution float3(1, 1, 1)
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

#define t _Time.y
#define r iResolution.xy
            float4 frag (v2f i) : SV_Target
            {
                float4 fragColor = 0;
                float2 fragCoord = i.uv;
                float3 c;
                float l, z = t;
                for (int i = 0;i<3; i++)
                {
                    float2 uv, p = fragCoord.xy/r;
                    uv = p;
                    p -= 0.5;
                    p.x *= r.x/r.y;
                    z += 0.07;
                    l = length(p);
                    uv += p/l*(sin(z)+1.)*abs(sin(l*9.-z*2.));
                    c[i] = 0.01/length(abs(glsl_mod(uv, 1.)-0.5));
                }
                fragColor = float4(c/l, t);
                return fragColor;
            }

        ENDCG
        }
    }
}
