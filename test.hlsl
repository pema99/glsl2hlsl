//CC0 1.0 Universal https://creativecommons.org/publicdomain/zero/1.0/
//To the extent possible under law, Blackle Mori has waived all copyright and related or neighboring rights to this work.

float bayer(ivec2 uv) {
    return texelFetch(iChannel0,uv % 8,0).x;
}

#define FK(k) floatBitsToInt(cos(k))^floatBitsToInt(k)
float hash(vec2 k) {
  int x = FK(k.x);int y = FK(k.y);
  return float((x*x-y)*(y*y+x)-x)/2.14e9;
}

float hash3(vec3 k) {
  float h1 = hash(k.xy);
  return hash(vec2(h1, k.z));
}

vec3 hash33(vec3 k) {
  float h1 = hash3(k);
  float h2 = hash3(k*h1);
  float h3 = hash3(k*h2);
  return vec3(h1, h2, h3);
}

float smin( float a, float b, float k ) {
    float h = max( k-abs(a-b), 0.0 )/k;
    return min( a, b ) - h*h*h*k*(1.0/6.0);
}

vec3 sphercoord(vec2 p) {
  float l1 = acos(p.x);
  float l2 = acos(-1.)*p.y;
  return vec3(cos(l1), sin(l1)*sin(l2), sin(l1)*cos(l2));
}

vec3 erot(vec3 p, vec3 ax, float ro) {
  return mix(dot(p,ax)*ax, p, cos(ro)) + sin(ro)*cross(p,ax);
}

float comp(vec3 p, vec3 ro, float t) {
  vec3 ax = sphercoord(ro.xy);
  p.z -= t;
  p = erot(p, ax, ro.z*acos(-1.));
  float scale = 4. + hash(ro.xz)*0.5+0.5;
  p = (fract(p/scale)-0.5)*scale;
  return length(p) - 0.8;
}

float scene(vec3 p) {
  float rad = 3.+p.z+sin(p.y/2.+iTime)+cos(p.x/3.+iTime*0.9);
  float dist = 10000.;
  for (int i = 0; i < 4; i++) {
    vec3 rot = hash33(vec3(float(i+1), cos(float(i)), sin(float(i))));
    float d = comp(p, rot, iTime/2.*(float(i+1)));
    dist = smin(dist, d, 1.);
  }
  return mix(dist, rad, mix(0.3,0.8+sin(iTime)*0.2, 0.1));
}

vec3 norm(vec3 p) {
  mat3 k = mat3(p,p,p)-mat3(0.1);
  return normalize(scene(p) - vec3(scene(k[0]),scene(k[1]),scene(k[2])));
}

float march(vec3 p, vec3 bias, float seed) {
    for (int i = 0; i < 10; i++) {
        p += normalize(bias+tan(hash33(vec3(float(i),seed,2.))))*scene(p);
    }
    return sqrt(smoothstep(0.0,2.0,scene(p)));
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
  vec2 uv = vec2(fragCoord.x / iResolution.x, fragCoord.y / iResolution.y);
  uv -= 0.5;
  uv /= vec2(iResolution.y / iResolution.x, 1);

  
  vec3 cam = normalize(vec3(4.,uv));
  vec3 init = vec3(-50,0,sin(iTime*0.37)*1.4);
  cam = erot(cam, vec3(0,1,0), -0.5);
  init = erot(init, vec3(0,1,0), -0.5);

  vec3 p = init;
  bool hit = false;
  bool trig = false;
  bool outline = false;
  for (int i = 0; i < 500 && !hit; i++) {
    float dist = scene(p);
    if (dist<0.08) trig = true;
    if (trig) {
      float odist = 0.09-dist;
      outline = odist<dist;
      dist = min(dist,odist);
    }
    hit = dist*dist < 1e-6;
    p += dist*cam;
  }
  vec3 n = norm(p);
  vec3 r = reflect(cam,n);
  vec2 ao = vec2(0);
  for (int i = 0; i < 8; i++) {
    ivec2 id = (ivec2(fragCoord/16.+vec2(iTime*10.,iTime*20.)) % 2)*2-1;
    ao += vec2(march(p+n*0.1, r, bayer(ivec2(fragCoord)+i+ivec2(i/4,0))) , 1.);
  }
  ao.x/=ao.y;

  fragColor.xyz = hit&&!outline ? vec3(ao.x) : vec3(0.);
  fragColor.xyz = pow(smoothstep(vec3(0),vec3(1),sqrt(fragColor.xyz)),vec3(1.7,1.6,1.5));
}