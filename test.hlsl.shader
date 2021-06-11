
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

#if 0
            void lol();
#endif
#define a (0.)
            float4 frag (v2f i) : SV_Target
            {
                float4 fragColor = 0;
                float2 fragCoord = i.uv;
                float3 iResolution = 1;
#define Hello mul(float2x2(3, 2, 1, 3),(3))
#if 0
                fragColor = (1);
#endif
                fragColor = (3);
                return fragColor;
            }

        ENDCG
        }
    }
}
