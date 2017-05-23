// render-gl.cpp
#include "render-gl.h"

#include "options.h"
#include "render.h"

// TODO(tfoley): eventually we should be able to run these
// tests on non-Windows targets to confirm that cross-compilation
// at least *works* on those platforms...
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#undef WIN32_LEAN_AND_MEAN
#undef NOMINMAX

#ifdef _MSC_VER
#include <stddef.h>
#if (_MSC_VER < 1900)
#define snprintf sprintf_s
#endif
#endif

#pragma comment(lib, "opengl32")

#include <GL/GL.h>

namespace renderer_test {

class GLRenderer : public Renderer, public ShaderCompiler
{
public:
    HDC     deviceContext;
    HGLRC   glContext;

    // Renderer interface

    virtual void initialize(void* inWindowHandle) override
    {
        auto windowHandle = (HWND) inWindowHandle;

        deviceContext = GetDC(windowHandle);

        PIXELFORMATDESCRIPTOR pixelFormatDesc = { sizeof(PIXELFORMATDESCRIPTOR) };
        pixelFormatDesc.nVersion = 1;
        pixelFormatDesc.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
        pixelFormatDesc.iPixelType = PFD_TYPE_RGBA;
        pixelFormatDesc.cColorBits = 32;
        pixelFormatDesc.cDepthBits = 24;
        pixelFormatDesc.cStencilBits = 8;
        pixelFormatDesc.iLayerType = PFD_MAIN_PLANE;

        int pixelFormatIndex = ChoosePixelFormat(deviceContext, &pixelFormatDesc);
        SetPixelFormat(deviceContext, pixelFormatIndex, &pixelFormatDesc);

        glContext = wglCreateContext(deviceContext);
        wglMakeCurrent(deviceContext, glContext);
    }

    virtual void clearFrame() override
    {

    }

    virtual void presentFrame() override
    {
        glFlush();
        SwapBuffers(deviceContext);
    }

    virtual void captureScreenShot(char const* outputPath) override
    {

    }

    virtual ShaderCompiler* getShaderCompiler() override
    {
        return this;
    }

    virtual Buffer* createBuffer(BufferDesc const& desc) override
    {
        return nullptr;
    }

    virtual InputLayout* createInputLayout(InputElementDesc const* inputElements, UInt inputElementCount) override
    {
        return nullptr;
    }

    virtual void* map(Buffer* buffer, MapFlavor flavor) override
    {
        return nullptr;
    }

    virtual void unmap(Buffer* buffer) override
    {
    }

    virtual void setInputLayout(InputLayout* inputLayout) override
    {
    }

    virtual void setPrimitiveTopology(PrimitiveTopology topology) override
    {
    }

    virtual void setVertexBuffers(UInt startSlot, UInt slotCount, Buffer* const* buffers, UInt const* strides, UInt const* offsets) override
    {
    }

    virtual void setShaderProgram(ShaderProgram* program) override
    {
    }

    virtual void setConstantBuffers(UInt startSlot, UInt slotCount, Buffer* const* buffers, UInt const* offsets) override
    {
    }


    virtual void draw(UInt vertexCount, UInt startVertex = 0) override
    {
    }

    // ShaderCompiler interface

    virtual ShaderProgram* compileProgram(ShaderCompileRequest const& request) override
    {
        return nullptr;
    }
};



Renderer* createGLRenderer()
{
    return new GLRenderer();
}

} // renderer_test
