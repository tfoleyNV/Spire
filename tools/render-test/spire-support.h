// spire-support.h
#pragma once

#include "render.h"

#include <Spire.h>

namespace renderer_test {

ShaderCompiler* createSpireShaderCompiler(ShaderCompiler* innerCompiler, SpireCompileTarget target);

} // renderer_test
