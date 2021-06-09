// Created by Stephane Cuillerdier - Aiekick/2015
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

#define shape(p) length(p)-2.8
    
const vec2 RMPrec = vec2(.2, 0.001); 
const vec3 DPrec = vec3(0.005, 12., 1e-6); 
    
// by shane
float Voronesque( in vec3 p )
{
    vec3 i  = floor(p + dot(p, vec3(0.333333)) );  p -= i - dot(i, vec3(0.166666)) ;
    vec3 i1 = step(0., p-p.yzx), i2 = max(i1, 1.0-i1.zxy); i1 = min(i1, 1.0-i1.zxy);    
    vec3 p1 = p - i1 + 0.166666, p2 = p - i2 + 0.333333, p3 = p - 0.5;
    vec3 rnd = vec3(7, 157, 113); // I use this combination to pay homage to Shadertoy.com. :)
    vec4 v = max(0.5 - vec4(dot(p, p), dot(p1, p1), dot(p2, p2), dot(p3, p3)), 0.);
    vec4 d = vec4( dot(i, rnd), dot(i + i1, rnd), dot(i + i2, rnd), dot(i + 1., rnd) ); 
    d = fract(sin(d)*262144.)*v*2.; 
    v.x = max(d.x, d.y), v.y = max(d.z, d.w), v.z = max(min(d.x, d.y), min(d.z, d.w)), v.w = min(v.x, v.y); 
    return  max(v.x, v.y) - max(v.z, v.w); // Maximum minus second order, for that beveled Voronoi look. Range [0, 1].
}

vec2 map(vec3 p)
{
	vec2 res = vec2(0.);

	float voro = Voronesque(p);
	
   	vec3 col = vec3(voro)*.5;
	
	float sphere = shape(p);
    float sphereOut = sphere - voro;
    float sphereIn = sphere + voro * .5;
    
   	float dist = max(-sphereIn, sphereOut + .29);
               
	res = vec2(dist, 1.);
	
	float kernel = sphere + 2.2;
    
	if (kernel < res.x ) 
		res = vec2(kernel, 2.);
	
	return res;
}

vec3 nor( vec3 pos, float prec )
{
    vec2 e = vec2( prec, 0. );
    vec3 n = vec3(
        map(pos+e.xyy).x - map(pos-e.xyy).x,
        map(pos+e.yxy).x - map(pos-e.yxy).x,
        map(pos+e.yyx).x - map(pos-e.yyx).x );
    return normalize(n);
}

vec3 cam(vec2 uv, vec3 ro, vec3 cu, vec3 cv)
{
	vec3 rov = normalize(cv-ro);
    vec3 u =  normalize(cross(cu, rov));
    vec3 v =  normalize(cross(rov, u));
    vec3 rd = normalize(rov + u*uv.x + v*uv.y);
    return rd;
}

vec3 blackbody(float Temp)
{
	vec3 col = vec3(255.);
    col.x = 56100000. * pow(Temp,(-3. / 2.)) + 148.;
   	col.y = 100.04 * log(Temp) - 623.6;
   	if (Temp > 6500.) col.y = 35200000. * pow(Temp,(-3. / 2.)) + 184.;
   	col.z = 194.18 * log(Temp) - 1448.6;
   	col = clamp(col, 0., 255.)/255.;
    if (Temp < 1000.) col *= Temp/1000.;
   	return col;
}

// light
const vec3 LPos = vec3(-0.6, 0.7, -0.5);
const vec3 LAmb = vec3( 0. );
const vec3 LDif = vec3( 1. , 0.5, 0. );
const vec3 LSpe = vec3( 0.8 );

// material
const vec3 MAmb = vec3( 0. );
const vec3 MDif = vec3( 1. , 0.5, 0. );
const vec3 MSpe = vec3( 0.6, 0.6, 0.6 );
const float MShi =30.;

vec3 ads( vec3 p, vec3 n )
{
    vec3 ldif = normalize( LPos - p);
    vec3 vv = normalize( vec3(0.) - p );
    vec3 refl = reflect( vec3(0.) - ldif, n );
    
    vec3 amb = MAmb*LAmb+ blackbody(2000.);
    vec3 dif = max(0., dot(ldif, n.xyz)) * MDif * LDif;
    vec3 spe = vec3( 0. );
    if( dot(ldif, vv) > 0.)
        spe = pow(max(0., dot(vv,refl)),MShi)*MSpe*LSpe;
    
    return amb*1.2 + dif*1.5 + spe*0.8;
}

vec3 nrand3( vec2 co )
{
	vec3 a = fract( cos( co.x*8.3e-3 + co.y )*vec3(1.3e5, 4.7e5, 2.9e5) );
	vec3 b = fract( sin( co.x*0.3e-3 + co.y )*vec3(8.1e5, 1.0e5, 0.1e5) );
	vec3 c = mix(a, b, 0.5);
	return c;
}

void mainImage( out vec4 f, in vec2 g )
{
    vec2 si = iResolution.xy;
	float t = iTime;
    
    float ca = t*.14; // angle z
    float ce = 1.; // elevation
    float cd = 1.; // distance to origin axis
   	
    vec3 cu=vec3(0,1,0);//Change camere up vector here
    vec3 cv=vec3(0,0,0); //Change camere view here
    vec2 uv = (g+g-si)/min(si.x, si.y);
    vec3 ro = vec3(sin(ca)*cd, ce+1., cos(ca)*cd); //
    vec3 rd = cam(uv, ro, cu, cv);
    
    vec3 d = vec3(0.);
    vec3 p = ro+rd*d.x;
    vec2 s = vec2(DPrec.y);
    
    for(int i=0;i<200;i++)
    {      
		if(s.x<DPrec.x||s.x>DPrec.y) break;
        s = map(p);
		s.x *= (s.x>DPrec.x?RMPrec.x:RMPrec.y);
        d.x += s.x;
        p = ro+rd*d.x;
   	}
	
	if (d.x<DPrec.y)
    {
		float nPrec = 0.1;
		vec3 n = nor(p, nPrec);
		
    	if ( s.y < 1.5) // rock
        {
			vec3 SSS = ads(n,n) - ads(p, rd);
			SSS += blackbody(1500. * (d.x - 3.));
			f.rgb = SSS;
        }
		else if( s.y < 2.5) // kernel
		{
			float b = dot(n,normalize(ro-p))*0.9;
            f = (b*vec4(blackbody(2000.),0.8)+pow(b,0.2))*(1.0-d.x*.01);
		}	
   	}
    else
    {
		vec3 rnd = nrand3( floor(uv * 2.0 * iResolution.x) );
		f = vec4(pow(rnd.y,10.0));
	}
}

