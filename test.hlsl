#if 0
void lol();
#endif

#define a vec3(0.)

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    #define Hello mat2(3, 2, 1, 3) * vec2(3)
    #if 0
    fragColor = vec3(1);
    #endif
	fragColor = vec3(3);
}

