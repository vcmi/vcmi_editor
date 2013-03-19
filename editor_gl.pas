{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge,net

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit editor_gl;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, math, GL, GLext, LazLoggerBase;

const

  FRAGMENT_PALETTE_SHADER =
  '#version 150'#13#10 +
  'uniform usampler2DRect bitmap;'+
  'uniform sampler1D palette;'+
  'uniform ivec2 coordOffs;'+
  'layout (origin_upper_left) in vec4 gl_FragCoord;'+

  'void main(){'+
    'gl_FragColor = texelFetch(palette, int(texelFetch(bitmap, ivec2(gl_FragCoord.xy) - coordOffs).r), 0);'+
  '}';

type

  TGLSprite = record
    TextureID: GLuint;
    Width: Int32;
    Height: Int32;

    X: Int32;
    Y: Int32;
  end;

  { TShaderContext }

  TShaderContext = class
  public
    PaletteProgram: GLuint;
    PaletteUniform: GLuint;
    BitmapUniform:GLUint;
    CoordUniform: GLUint;
  public
    destructor Destroy; override;
    procedure Init;

    procedure UseNoShader();
    procedure UseShader(offs_x,offs_y: int32);
  end;

procedure BindPalette(ATextureId: GLuint; ARawImage: Pointer);

procedure BindUncompressedPaletted(ATextureId: GLuint; w,h: Int32; ARawImage: Pointer);

procedure BindUncompressedRGBA(ATextureId: GLuint; w,h: Int32; var ARawImage);
procedure BindCompressedRGBA(ATextureId: GLuint; w,h: Int32; var ARawImage);
procedure Unbind(var ATextureId: GLuint); inline;

procedure RenderSprite(const ASprite: TGLSprite; dim: integer = -1; mir: UInt8 = 0);
procedure RenderRect(x,y: Integer; dimx,dimy:integer);

procedure CheckGLErrors(Stage: string);

function MakeShaderProgram(const AShaderSource: AnsiString):GLuint;

implementation


procedure BindRGBA(ATextureId: GLuint; w, h: Int32; ARawImage: Pointer; AInternalFormat: GLEnum); inline;
begin
  glEnable(GL_TEXTURE_RECTANGLE);
  glBindTexture(GL_TEXTURE_RECTANGLE, ATextureId);

  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexImage2D(GL_TEXTURE_RECTANGLE, 0,AInternalFormat,w,h,0,GL_RGBA, GL_UNSIGNED_BYTE, ARawImage);
  glDisable(GL_TEXTURE_RECTANGLE);
end;

procedure BindPalette(ATextureId: GLuint; ARawImage: Pointer);
begin
  glEnable(GL_TEXTURE_1D);
  glBindTexture(GL_TEXTURE_1D, ATextureId);

  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  glTexImage1D(GL_TEXTURE_1D, 0,GL_RGBA,256,0,GL_RGBA, GL_UNSIGNED_BYTE, ARawImage);
  glDisable(GL_TEXTURE_1D);
end;


procedure BindUncompressedPaletted(ATextureId: GLuint; w, h: Int32;
  ARawImage: Pointer);
begin
  glEnable(GL_TEXTURE_RECTANGLE);
  glBindTexture(GL_TEXTURE_RECTANGLE, ATextureId);

  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexImage2D(GL_TEXTURE_RECTANGLE, 0,GL_LUMINANCE8,w,h,0,GL_LUMINANCE8, GL_UNSIGNED_BYTE, ARawImage);
  glDisable(GL_TEXTURE_RECTANGLE);
end;

procedure BindUncompressedRGBA(ATextureId: GLuint; w, h: Int32; var ARawImage);
begin
  BindRGBA(ATextureId,w, h,@ARawImage,GL_RGBA);
end;

procedure BindCompressedRGBA(ATextureId: GLuint; w, h: Int32; var ARawImage);
begin
  BindRGBA(ATextureId,w, h,@ARawImage,GL_COMPRESSED_RGBA);
end;

procedure Unbind(var ATextureId: GLuint);
begin
  glDeleteTextures(1,@ATextureId);
  ATextureId := 0;
end;

procedure RenderSprite(const ASprite: TGLSprite; dim: integer; mir: UInt8);
var
  factor: Double;
  cur_dim: integer;
  H: Int32;
  W: Int32;
begin

  if dim <=0 then //render real size w|o scale
  begin
    H := ASprite.Height;
    W := ASprite.Width;
  end
  else
  begin
    cur_dim := Max(ASprite.Width,ASprite.Height);
    factor := Min(dim / cur_dim, 1); //no zoom

    h := round(Double(ASprite.Height) * factor);
    w := round(Double(ASprite.Width) * factor);
  end;

  glEnable(GL_TEXTURE_RECTANGLE);
    glBindTexture(GL_TEXTURE_RECTANGLE,ASprite.TextureID);
    glBegin(GL_POLYGON);

      case mir of
        0:begin
          glTexCoord2i(0,0);
          glVertex2i(ASprite.X,  ASprite.Y);

          glTexCoord2i(ASprite.Width, 0);
          glVertex2i(ASprite.X+W,ASprite.Y);

          glTexCoord2i(ASprite.Width, ASprite.Height);
          glVertex2i(ASprite.X+W,ASprite.Y+H);

          glTexCoord2i(0,ASprite.Height);
          glVertex2i(ASprite.X,  ASprite.Y+H);
        end;
        1: begin
          glTexCoord2i(ASprite.Width,0);
          glVertex2i(ASprite.X,  ASprite.Y);

          glTexCoord2i(0, 0);
          glVertex2i(ASprite.X+W,ASprite.Y);

          glTexCoord2i(0, ASprite.Height);
          glVertex2i(ASprite.X+W,ASprite.Y+H);

          glTexCoord2i(ASprite.Width,ASprite.Height);
          glVertex2i(ASprite.X,  ASprite.Y+H);
          end;
        2: begin
          glTexCoord2i(0,ASprite.Height);
          glVertex2i(ASprite.X,  ASprite.Y);

          glTexCoord2i(ASprite.Width, ASprite.Height);
          glVertex2i(ASprite.X+W,ASprite.Y);

          glTexCoord2i(ASprite.Width, 0);
          glVertex2i(ASprite.X+W,ASprite.Y+H);

          glTexCoord2i(0,0);
          glVertex2i(ASprite.X,  ASprite.Y+H);
          end;
        3:begin
          glTexCoord2i(ASprite.Width,ASprite.Height);
          glVertex2i(ASprite.X,  ASprite.Y);

          glTexCoord2i(0, ASprite.Height);
          glVertex2i(ASprite.X+W,ASprite.Y);

          glTexCoord2i(0, 0);
          glVertex2i(ASprite.X+W,ASprite.Y+H);

          glTexCoord2i(ASprite.Width,0);
          glVertex2i(ASprite.X,  ASprite.Y+H);
          end;
      end;



    glEnd();

  glDisable(GL_TEXTURE_RECTANGLE);

end;

procedure RenderRect(x, y: Integer; dimx, dimy: integer);
begin
      glPushAttrib(GL_CURRENT_BIT);
    glBegin(GL_LINE_LOOP);

    glColor4ub(200, 200, 200, 255);
    glLineWidth(1);


    glVertex2i(x, y);
    glVertex2i(x + dimx, y);

    glVertex2i(x + dimx, y + dimy);
    glVertex2i(x, y + dimy);


    glEnd();
    glPopAttrib();
end;

procedure CheckGLErrors(Stage: string);
var
  err: GLenum;
begin

  repeat
    err := glGetError();

    if err<>GL_NO_ERROR then
    begin
      DebugLogger.DebugLn(Stage +': Gl error '+IntToStr(err));
    end;

  until err = GL_NO_ERROR ;


end;

function MakeShaderProgram(const AShaderSource: AnsiString): GLuint;
var
  shader_object, program_object: GLuint;
  status: GLint;
begin
  Result := 0;
  shader_object := glCreateShader(GL_FRAGMENT_SHADER);

  if shader_object = 0 then
  begin
    CheckGLErrors('MakeShaderProgram');
    Exit;
  end;

  glShaderSource(shader_object,1,@(AShaderSource),nil);
  glCompileShader(shader_object);
  glGetShaderiv(shader_object,GL_COMPILE_STATUS,@status);
  //todo: print log

  if status = GL_TRUE then
  begin
    program_object := glCreateProgram;
    glAttachShader(program_object, shader_object);

    glLinkProgram(program_object);
    status := GL_FALSE;
    glGetProgramiv(program_object, GL_LINK_STATUS, @status);

    //todo: print log

    if (status = GL_TRUE) then
      Result := program_object;
  end;

  glDeleteShader(shader_object); //always mark shader for deletion
end;

{ TShaderContext }

destructor TShaderContext.Destroy;
begin
  UseShader(false);
  glDeleteProgram(PaletteProgram);
  //TODO: delete shader?
  inherited Destroy;
end;

procedure TShaderContext.Init;
begin
  PaletteProgram := MakeShaderProgram(FRAGMENT_PALETTE_SHADER);
  if PaletteProgram = 0 then
    raise Exception.Create('Error compiling palette shader');

  BitmapUniform := glGetUniformLocation(PaletteProgram, PChar('bitmap'));
  CoordUniform := glGetUniformLocation(PaletteProgram, PChar('coordOffs'));
  PaletteUniform := glGetUniformLocation(PaletteProgram, PChar('palette'));
end;

procedure TShaderContext.UseNoShader;
begin
    glUseProgram(0);
end;

procedure TShaderContext.UseShader(offs_x, offs_y: int32);
begin
  glUseProgram(PaletteProgram);
  glUniform1i(BitmapUniform, 0); //texture unit0
  glUniform1i(PaletteUniform, 1);//texture unit1
  glUniform2i(CoordUniform, offs_x,offs_y);
end;


end.

