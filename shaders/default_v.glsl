#version 130

uniform mat4 projMatrix;
uniform mat4 translateMatrix;

in vec2 coords;
in vec2 uv;
out vec2 UV;

void main()
{
  UV = uv;
  gl_Position = projMatrix * translateMatrix * vec4(coords,0.0,1.0);
}
