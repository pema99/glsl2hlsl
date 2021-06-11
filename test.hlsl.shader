
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

#if HW_PERFORMANCE==0
#define AA 1
#else
#define AA 2
#endif
            float smin(float a, float b, float k)
            {
                float h = max(k-abs(a-b), 0.);
                return min(a, b)-h*h*0.25/k;
            }

            float2 smin(float2 a, float2 b, float k)
            {
                float h = clamp(0.5+0.5*(b.x-a.x)/k, 0., 1.);
                return lerp(b, a, h)-k*h*(1.-h);
            }

            float smax(float a, float b, float k)
            {
                float h = max(k-abs(a-b), 0.);
                return max(a, b)+h*h*0.25/k;
            }

            float sdSphere(float3 p, float s)
            {
                return length(p)-s;
            }

            float sdEllipsoid(in float3 p, in float3 r)
            {
                float k0 = length(p/r);
                float k1 = length(p/(r*r));
                return k0*(k0-1.)/k1;
            }

            float2 sdStick(float3 p, float3 a, float3 b, float r1, float r2)
            {
                float3 pa = p-a, ba = b-a;
                float h = clamp(dot(pa, ba)/dot(ba, ba), 0., 1.);
                return float2(length(pa-ba*h)-lerp(r1, r2, h*h*(3.-2.*h)), h);
            }

            float4 opU(float4 d1, float4 d2)
            {
                return d1.x<d2.x ? d1 : d2;
            }

#define ZERO min(iFrame, 0)
            static float href;
            static float hsha;
            float4 map(in float3 pos, float atime)
            {
                hsha = 1.;
                float t1 = frac(atime);
                float t4 = abs(frac(atime*0.5)-0.5)/0.5;
                float p = 4.*t1*(1.-t1);
                float pp = 4.*(1.-2.*t1);
                float3 cen = float3(0.5*(-1.+2.*t4), pow(p, 2.-p)+0.1, floor(atime)+pow(t1, 0.7)-1.);
                float2 uu = normalize(float2(1., -pp));
                float2 vv = float2(-uu.y, uu.x);
                float sy = 0.5+0.5*p;
                float compress = 1.-smoothstep(0., 0.4, p);
                sy = sy*(1.-compress)+compress;
                float sz = 1./sy;
                float3 q = pos-cen;
                float rot = -0.25*(-1.+2.*t4);
                float rc = cos(rot);
                float rs = sin(rot);
                q.xy = mul(float2x2(rc, rs, -rs, rc),q.xy);
                float3 r = q;
                href = q.y;
                q.yz = float2(dot(uu, q.yz), dot(vv, q.yz));
                float deli = sdEllipsoid(q, float3(0.25, 0.25*sy, 0.25*sz));
                float4 res = float4(deli, 2., 0., 1.);
                float fh = -0.1-0.05*(sin(pos.x*2.)+sin(pos.z*2.));
                float t5f = frac(atime+0.05);
                float t5i = floor(atime+0.05);
                float bt4 = abs(frac(t5i*0.5)-0.5)/0.5;
                float2 bcen = float2(0.5*(-1.+2.*bt4), t5i+pow(t5f, 0.7)-1.);
                float k = length(pos.xz-bcen);
                float tt = t5f*15.-6.2831-k*3.;
                fh -= 0.1*exp(-k*k)*sin(tt)*exp(-max(tt, 0.)/2.)*smoothstep(0., 0.01, t5f);
                float d = pos.y-fh;
                {
                    float3 vp = float3(glsl_mod(abs(pos.x), 3.)-1.5, pos.y, glsl_mod(pos.z+1.5, 3.)-1.5);
                    float2 id = float2(floor(pos.x/3.), floor((pos.z+1.5)/3.));
                    float fid = id.x*11.1+id.y*31.7;
                    float fy = frac(fid*1.312+atime*0.1);
                    float y = -1.+4.*fy;
                    float3 rad = float3(0.7, 1.+0.5*sin(fid), 0.7);
                    rad -= 0.1*(sin(pos.x*3.)+sin(pos.y*4.)+sin(pos.z*5.));
                    float siz = 4.*fy*(1.-fy);
                    float d2 = sdEllipsoid(vp-float3(0.5, y, 0.), siz*rad);
                    d2 -= 0.03*smoothstep(-1., 1., sin(18.*pos.x)+sin(18.*pos.y)+sin(18.*pos.z));
                    d2 *= 0.6;
                    d2 = min(d2, 2.);
                    d = smin(d, d2, 0.32);
                    if (d<res.x)
                    {
                        res = float4(d, 1., 0., 1.);
                        hsha = sqrt(siz);
                    }
                    
                }
                if (deli-1.<res.x)
                {
                    float t2 = frac(atime+0.8);
                    float p2 = 0.5-0.5*cos(6.2831*t2);
                    r.z += 0.05-0.2*p2;
                    r.y += 0.2*sy-0.2;
                    float3 sq = float3(abs(r.x), r.yz);
                    float3 h = r;
                    float hr = sin(0.791*atime);
                    hr = 0.7*sign(hr)*smoothstep(0.5, 0.7, abs(hr));
                    h.xz = mul(float2x2(cos(hr), sin(hr), -sin(hr), cos(hr)),h.xz);
                    float3 hq = float3(abs(h.x), h.yz);
                    float d = sdEllipsoid(h-float3(0., 0.2, 0.02), float3(0.08, 0.2, 0.15));
                    float d2 = sdEllipsoid(h-float3(0., 0.21, -0.1), float3(0.2, 0.2, 0.2));
                    d = smin(d, d2, 0.1);
                    res.x = smin(res.x, d, 0.1);
                    {
                        float yy = r.y-0.02-2.5*r.x*r.x;
                        res.x += 0.001*sin(yy*120.)*(1.-smoothstep(0., 0.1, abs(yy)));
                    }
                    {
                        float2 arms = sdStick(sq, float3(0.18-0.06*hr*sign(r.x), 0.2, -0.05), float3(0.3+0.1*p2, -0.2+0.3*p2, -0.15), 0.03, 0.06);
                        res.xz = smin(res.xz, arms, 0.01+0.04*(1.-arms.y)*(1.-arms.y)*(1.-arms.y));
                    }
                    {
                        float t3 = frac(atime+0.9);
                        float p3 = 4.*t3*(1.-t3);
                        float2 ear = sdStick(hq, float3(0.15, 0.32, -0.05), float3(0.2+0.05*p3, 0.2+0.2*p3, -0.07), 0.01, 0.04);
                        res.xz = smin(res.xz, ear, 0.01);
                    }
                    {
                        d = sdEllipsoid(h-float3(0., 0.15+4.*hq.x*hq.x, 0.15), float3(0.1, 0.04, 0.2));
                        res.w = 0.3+0.7*clamp(d*150., 0., 1.);
                        res.x = smax(res.x, -d, 0.03);
                    }
                    {
                        float t6 = cos(6.2831*(atime*0.5+0.25));
                        float ccc = cos(1.57*t6*sign(r.x));
                        float sss = sin(1.57*t6*sign(r.x));
                        float3 base = float3(0.12, -0.07, -0.1);
                        base.y -= 0.1/sy;
                        float2 legs = sdStick(sq, base, base+float3(0.2, -ccc, sss)*0.2, 0.04, 0.07);
                        res.xz = smin(res.xz, legs, 0.07);
                    }
                    {
                        float blink = pow(0.5+0.5*sin(2.1*_Time.y), 20.);
                        float eyeball = sdSphere(hq-float3(0.08, 0.27, 0.06), 0.065+0.02*blink);
                        res.x = smin(res.x, eyeball, 0.03);
                        float3 cq = hq-float3(0.1, 0.34, 0.08);
                        cq.xy = mul(float2x2(0.8, 0.6, -0.6, 0.8),cq.xy);
                        d = sdEllipsoid(cq, float3(0.06, 0.03, 0.03));
                        res.x = smin(res.x, d, 0.03);
                        float eo = 1.-0.5*smoothstep(0.01, 0.04, length((hq.xy-float2(0.095, 0.285))*float2(1., 1.1)));
                        res = opU(res, float4(sdSphere(hq-float3(0.08, 0.28, 0.08), 0.06), 3., 0., eo));
                        res = opU(res, float4(sdSphere(hq-float3(0.075, 0.28, 0.102), 0.0395), 4., 0., 1.));
                    }
                }
                
                if (pos.y-1.<res.x)
                {
                    float fs = 5.;
                    float3 qos = fs*float3(pos.x, pos.y-fh, pos.z);
                    float2 id = float2(floor(qos.x+0.5), floor(qos.z+0.5));
                    float3 vp = float3(frac(qos.x+0.5)-0.5, qos.y, frac(qos.z+0.5)-0.5);
                    vp.xz += 0.1*cos(id.x*130.143+id.y*120.372+float2(0., 2.));
                    float den = sin(id.x*0.1+sin(id.y*0.091))+sin(id.y*0.1);
                    float fid = id.x*0.143+id.y*0.372;
                    float ra = smoothstep(0., 0.1, den*0.1+frac(fid)-0.95);
                    d = sdSphere(vp, 0.35*ra)/fs;
                    if (d<res.x)
                        res = float4(d, 5., qos.y, 1.);
                        
                }
                
                return res;
            }

            float4 raycast(in float3 ro, in float3 rd, float time)
            {
                float4 res = float4(-1., -1., 0., 1.);
                float tmin = 0.5;
                float tmax = 20.;
#if 1
                float tp = (3.4-ro.y)/rd.y;
                if (tp>0.)
                    tmax = min(tmax, tp);
                    
#endif
                float t = tmin;
                for (int i = 0;i<256&&t<tmax; i++)
                {
                    float4 h = map(ro+rd*t, time);
                    if (abs(h.x)<0.0005*t)
                    {
                        res = float4(t, h.yzw);
                        break;
                    }
                    
                    t += h.x;
                }
                return res;
            }

            float calcSoftshadow(in float3 ro, in float3 rd, float time)
            {
                float res = 1.;
                float tmax = 12.;
#if 1
                float tp = (3.4-ro.y)/rd.y;
                if (tp>0.)
                    tmax = min(tmax, tp);
                    
#endif
                float t = 0.02;
                for (int i = 0;i<50; i++)
                {
                    float h = map(ro+rd*t, time).x;
                    res = min(res, lerp(1., 16.*h/t, hsha));
                    t += clamp(h, 0.05, 0.4);
                    if (res<0.005||t>tmax)
                        break;
                        
                }
                return clamp(res, 0., 1.);
            }

            float3 calcNormal(in float3 pos, float time)
            {
#if 0
                float2 e = float2(1., -1.)*0.5773*0.001;
                return normalize(e.xyy*map(pos+e.xyy, time).x+e.yyx*map(pos+e.yyx, time).x+e.yxy*map(pos+e.yxy, time).x+e.xxx*map(pos+e.xxx, time).x);
#else
                float3 n = float3(0., 0., 0.);
                for (int i = ZERO;i<4; i++)
                {
                    float3 e = 0.5773*(2.*float3(i+3>>1&1, i>>1&1, i&1)-1.);
                    n += e*map(pos+0.001*e, time).x;
                }
                return normalize(n);
#endif
            }

            float calcOcclusion(in float3 pos, in float3 nor, float time)
            {
                float occ = 0.;
                float sca = 1.;
                for (int i = ZERO;i<5; i++)
                {
                    float h = 0.01+0.11*float(i)/4.;
                    float3 opos = pos+h*nor;
                    float d = map(opos, time).x;
                    occ += (h-d)*sca;
                    sca *= 0.95;
                }
                return clamp(1.-2.*occ, 0., 1.);
            }

            float3 render(in float3 ro, in float3 rd, float time)
            {
                float3 col = float3(0.5, 0.8, 0.9)-max(rd.y, 0.)*0.5;
                float2 uv = 1.5*rd.xz/rd.y;
                float cl = 1.*(sin(uv.x)+sin(uv.y));
                uv = mul(float2x2(0.8, 0.6, -0.6, 0.8)*2.1,uv);
                cl += 0.5*(sin(uv.x)+sin(uv.y));
                col += 0.1*(-1.+2.*smoothstep(-0.1, 0.1, cl-0.4));
                col = lerp(col, float3(0.5, 0.7, 0.9), exp(-10.*max(rd.y, 0.)));
                float4 res = raycast(ro, rd, time);
                if (res.y>-0.5)
                {
                    float t = res.x;
                    float3 pos = ro+t*rd;
                    float3 nor = calcNormal(pos, time);
                    float3 ref = reflect(rd, nor);
                    float focc = res.w;
                    col = float3(0.2, 0.2, 0.2);
                    float ks = 1.;
                    if (res.y>4.5)
                    {
                        col = float3(0.14, 0.048, 0.);
                        float2 id = floor(5.*pos.xz+0.5);
                        col += 0.036*cos(id.x*11.1+id.y*37.341+float3(0., 1., 2.));
                        col = max(col, 0.);
                        focc = clamp(4.*res.z, 0., 1.);
                    }
                    else if (res.y>3.5)
                    {
                        col = float3(0., 0., 0.);
                    }
                    else if (res.y>2.5)
                    {
                        col = float3(0.4, 0.4, 0.4);
                    }
                    else if (res.y>1.5)
                    {
                        col = lerp(float3(0.144, 0.09, 0.0036), float3(0.36, 0.1, 0.04), res.z*res.z);
                        col = lerp(col, float3(0.14, 0.09, 0.06)*2., (1.-res.z)*smoothstep(-0.15, 0.15, -href));
                    }
                    else 
                    {
                        col = float3(0.05, 0.09, 0.02);
                        float f = 0.2*(-1.+2.*smoothstep(-0.2, 0.2, sin(18.*pos.x)+sin(18.*pos.y)+sin(18.*pos.z)));
                        col += f*float3(0.06, 0.06, 0.02);
                        ks = 0.5+pos.y*0.15;
                        float2 mp = float2(pos.x-0.5*(glsl_mod(floor(pos.z+0.5), 2.)*2.-1.), frac(pos.z+0.5)-0.5);
                        float mark = 1.-smoothstep(0.1, 0.5, length(mp));
                        mark *= smoothstep(0., 0.1, floor(time)-floor(pos.z+0.5));
                        col *= lerp(float3(1., 1., 1.), float3(0.5, 0.5, 0.4), mark);
                        ks *= 1.-0.5*mark;
                    }
                    float occ = calcOcclusion(pos, nor, time)*focc;
                    float fre = clamp(1.+dot(nor, rd), 0., 1.);
                    float3 sun_lig = normalize(float3(0.6, 0.35, 0.5));
                    float sun_dif = clamp(dot(nor, sun_lig), 0., 1.);
                    float3 sun_hal = normalize(sun_lig-rd);
                    float sun_sha = calcSoftshadow(pos, sun_lig, time);
                    float sun_spe = ks*pow(clamp(dot(nor, sun_hal), 0., 1.), 8.)*sun_dif*(0.04+0.96*pow(clamp(1.+dot(sun_hal, rd), 0., 1.), 5.));
                    float sky_dif = sqrt(clamp(0.5+0.5*nor.y, 0., 1.));
                    float sky_spe = ks*smoothstep(0., 0.5, ref.y)*(0.04+0.96*pow(fre, 4.));
                    float bou_dif = sqrt(clamp(0.1-0.9*nor.y, 0., 1.))*clamp(1.-0.1*pos.y, 0., 1.);
                    float bac_dif = clamp(0.1+0.9*dot(nor, normalize(float3(-sun_lig.x, 0., -sun_lig.z))), 0., 1.);
                    float sss_dif = fre*sky_dif*(0.25+0.75*sun_dif*sun_sha);
                    float3 lin = float3(0., 0., 0.);
                    lin += sun_dif*float3(8.1, 6., 4.2)*float3(sun_sha, sun_sha*sun_sha*0.5+0.5*sun_sha, sun_sha*sun_sha);
                    lin += sky_dif*float3(0.5, 0.7, 1.)*occ;
                    lin += bou_dif*float3(0.2, 0.7, 0.1)*occ;
                    lin += bac_dif*float3(0.45, 0.35, 0.25)*occ;
                    lin += sss_dif*float3(3.25, 2.75, 2.5)*occ;
                    col = col*lin;
                    col += sun_spe*float3(9.9, 8.1, 6.3)*sun_sha;
                    col += sky_spe*float3(0.2, 0.3, 0.65)*occ*occ;
                    col = pow(col, float3(0.8, 0.9, 1.));
                    col = lerp(col, float3(0.5, 0.7, 0.9), 1.-exp(-0.0001*t*t*t));
                }
                
                return col;
            }

            float3x3 setCamera(in float3 ro, in float3 ta, float cr)
            {
                float3 cw = normalize(ta-ro);
                float3 cp = float3(sin(cr), cos(cr), 0.);
                float3 cu = normalize(cross(cw, cp));
                float3 cv = cross(cu, cw);
                return float3x3(cu, cv, cw);
            }

            float4 frag (v2f i) : SV_Target
            {
                float4 fragColor = 0;
                float2 fragCoord = i.uv;
                float3 iResolution = 1;
                float3 tot = float3(0., 0., 0.);
#if AA>1
                for (int m = ZERO;m<AA; m++)
                for (int n = ZERO;n<AA; n++)
                {
                    float2 o = float2(float(m), float(n))/float(AA)-0.5;
                    float2 p = (-iResolution.xy+2.*(fragCoord+o))/iResolution.y;
                    float d = 0.5+0.5*sin(fragCoord.x*147.)*sin(fragCoord.y*131.);
                    float time = _Time.y-0.5*(1./24.)*(float(m*AA+n)+d)/float(AA*AA);
#else
                    float2 p = (-iResolution.xy+2.*fragCoord)/iResolution.y;
                    float time = _Time.y;
#endif
                    time += -2.6;
                    time *= 0.9;
                    float cl = sin(0.5*time);
                    float an = 1.57+0.7*sin(0.15*time);
                    float3 ta = float3(0., 0.65, -0.6+time*1.-0.4*cl);
                    float3 ro = ta+float3(1.3*cos(an), -0.25, 1.3*sin(an));
                    float ti = frac(time-0.15);
                    ti = 4.*ti*(1.-ti);
                    ta.y += 0.15*ti*ti*(3.-2.*ti)*smoothstep(0.4, 0.9, cl);
                    float t4 = abs(frac(time*0.5)-0.5)/0.5;
                    float bou = -1.+2.*t4;
                    ro += 0.06*sin(time*12.+float3(0., 2., 4.))*smoothstep(0.85, 1., abs(bou));
                    float3x3 ca = setCamera(ro, ta, 0.);
                    float3 rd = mul(ca,normalize(float3(p, 1.8)));
                    float3 col = render(ro, rd, time);
                    col = col*float3(1.11, 0.89, 0.79);
                    col = 1.35*col/(1.+col);
                    col = pow(col, float3(0.4545, 0.4545, 0.4545));
                    tot += col;
#if AA>1
                }
                tot /= float(AA*AA);
#endif
                tot = clamp(tot, 0., 1.);
                tot = tot*tot*(3.-2.*tot);
                float2 q = fragCoord/iResolution.xy;
                tot *= 0.5+0.5*pow(16.*q.x*q.y*(1.-q.x)*(1.-q.y), 0.25);
                fragColor = float4(tot, 1.);
                return fragColor;
            }

        ENDCG
        }
    }
}
