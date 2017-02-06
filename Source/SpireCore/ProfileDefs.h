//

// Define all the various language "profiles" we want to support.

#ifndef LANGUAGE
#define LANGUAGE(TAG, NAME) /* emptry */
#endif

#ifndef LANGUAGE_VERSION
#define LANGUAGE_VERSION(TAG) /* empty */
#endif

#ifndef STAGE
#define STAGE(TAG, NAME) /* empty */
#endif

#ifndef STAGE_ALIAS
#define STAGE_ALIAS(TAG, NAME) /* empty */
#endif


#ifndef PROFILE
#define PROFILE(TAG, NAME, LANG, STAGE, LANG_VERSION)
#endif

#ifndef PROFILE_ALIAS
#define PROFILE_ALIAS(TAG, NAME) /* empty */
#endif

// Source and destination languages
LANGUAGE(HLSL, hlsl)
LANGUAGE(GLSL, glsl)

// Pipeline stages to target
STAGE(Vertex,	vertex)
STAGE(Hull,		hull)
STAGE(Domain,	domain)
STAGE(Geometry, geometry)
STAGE(Fragment, fragment)
STAGE(Compute,	compute)

// Allow "pixel" as an alias for "fragment"
STAGE_ALIAS(Fragment, pixel)

// Language versions


LANGUAGE_VERSION(DX_4_0)
LANGUAGE_VERSION(DX_4_0_Level_9_0)
LANGUAGE_VERSION(DX_4_0_Level_9_1)
LANGUAGE_VERSION(DX_4_0_Level_9_3)
LANGUAGE_VERSION(DX_4_1)
LANGUAGE_VERSION(DX_5_0)


#undef LANGUAGE
#undef STAGE

PROFILE(HLSL_Compute_4_0, hlsl_cs_4_0, HLSL, Compute, DX_4_0)
PROFILE(HLSL_Compute_4_1, hlsl_cs_4_1, HLSL, Compute, DX_4_1)
PROFILE(HLSL_Compute_5_0, hlsl_cs_5_0, HLSL, Compute, DX_5_0)

PROFILE(HLSL_Domain_5_0, hlsl_ds_5_0, HLSL, Domain, DX_5_0)

PROFILE(HLSL_Geometry_4_0, hlsl_gs_4_0, HLSL, Geometry, DX_4_0)
PROFILE(HLSL_Geometry_4_1, hlsl_gs_4_1, HLSL, Geometry, DX_4_1)
PROFILE(HLSL_Geometry_5_0, hlsl_gs_5_0, HLSL, Geometry, DX_5_0)

PROFILE(HLSL_Hull_5_0, hlsl_hs_5_0, HLSL, HULL, DX_5_0)

PROFILE(HLSL_Fragment_4_0,				hlsl_ps_4_0,			HLSL, Fragment, DX_4_0)
PROFILE(HLSL_Fragment_4_0_Level_9_0,	hlsl_ps_4_0_level_9_0,	HLSL, Fragment, DX_4_0_Level_9_0)
PROFILE(HLSL_Fragment_4_0_Level_9_1,	hlsl_ps_4_0_level_9_1,	HLSL, Fragment, DX_4_0_Level_9_1)
PROFILE(HLSL_Fragment_4_0_Level_9_3,	hlsl_ps_4_0_level_9_3,	HLSL, Fragment, DX_4_0_Level_9_3)
PROFILE(HLSL_Fragment_4_1,				hlsl_ps_4_1,			HLSL, Fragment, DX_4_1)
PROFILE(HLSL_Fragment_5_0,				hlsl_ps_5_0,			HLSL, Fragment, DX_5_0)

PROFILE(HLSL_Vertex_4_0,			hlsl_vs_4_0,			HLSL, Vertex, DX_4_0)
PROFILE(HLSL_Vertex_4_0_Level_9_0,	hlsl_vs_4_0_level_9_0,	HLSL, Vertex, DX_4_0_Level_9_0)
PROFILE(HLSL_Vertex_4_0_Level_9_1,	hlsl_vs_4_0_level_9_1,	HLSL, Vertex, DX_4_0_Level_9_1)
PROFILE(HLSL_Vertex_4_0_Level_9_3,	hlsl_vs_4_0_level_9_3,	HLSL, Vertex, DX_4_0_Level_9_3)
PROFILE(HLSL_Vertex_4_1,			hlsl_vs_4_1,			HLSL, Vertex, DX_4_1)
PROFILE(HLSL_Vertex_5_0,			hlsl_vs_5_0,			HLSL, Vertex, DX_5_0)
