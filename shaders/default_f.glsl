#version 330 core

const int playerColorIndex = 5;
uniform usampler2D bitmap;
uniform sampler1D palette;
uniform sampler2D bitmapRGB;

uniform int useTexture = 0;

uniform int useFlag = 0;
uniform vec4 flagColor;
uniform vec4 fragmentColor;
in vec2 UV;
out vec4 outColor;

vec4 applyFlag(int inColor)
{
   if(useFlag == 1)
   {
      if(inColor == playerColorIndex)
        return flagColor;
   }
   return texelFetch(palette, inColor, 0);
}

vec4 applyTextureWithPalette()
{
    return applyFlag(int(texture(bitmap,UV).r));
}

vec4 applyTexture()
{
    return texture(bitmapRGB,UV);
}

void main()
{
  if(useTexture == 2)
  {
    outColor = applyTexture();
  }
  else if(useTexture == 1)
  {
      outColor = applyTextureWithPalette();
  }
  else
  {
     outColor = fragmentColor;
  }
}
