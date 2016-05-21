#version 130

const vec4 eps = vec4(0.009, 0.009, 0.009, 0.009);
const vec4 maskColor = vec4(1.0, 1.0, 0.0, 0.0);
uniform usampler2D bitmap;
uniform sampler1D palette;
uniform int useTexture = 0;
uniform int usePalette = 0;
uniform int useFlag = 0;
uniform vec4 flagColor;
uniform vec4 fragmentColor;
in vec2 UV;
out vec4 outColor;

vec4 applyTexture(vec4 inColor)
{
    if(useTexture == 1)
    {
       if(usePalette == 1)
        {
            return texelFetch(palette, int(texture(bitmap,UV).r), 0);
        }
        else
        {
           // 'return texture(bitmap,UV); //???
        }
    }
    return inColor;
}

vec4 applyFlag(vec4 inColor)
{
   if(useFlag == 1)
   {
      if(all(greaterThanEqual(inColor,maskColor-eps)) && all(lessThanEqual(inColor,maskColor+eps)))
        return flagColor;
   }
   return inColor;
}

void main(){
  outColor = applyTexture(fragmentColor);
  outColor = applyFlag(outColor);
}
