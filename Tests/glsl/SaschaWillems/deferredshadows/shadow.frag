#version 450
//TEST:COMPARE_GLSL:

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) out float fragmentdepth;

void main() 
{	
	fragmentdepth = gl_FragCoord.z;
}