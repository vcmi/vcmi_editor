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
  Classes, SysUtils, math, matrix, GL, glext40, LazLoggerBase;

type
  TRBGAColor = packed record   //todo: optimise
    r,g,b,a : UInt8;
  end;

const
  VERTEX_DEFAULT_SHADER =
  '#version 330 core'#13#10+
  'uniform mat4 projMatrix;'#13#10+
  'in vec2 coords;'#13#10+
  'in vec2 uv;'#13#10+
  'out vec2 UV;'#13#10+
  'void main(){'+
    'UV = uv;'+
  	'gl_Position = projMatrix * vec4(coords,0.0,1.0);'#13#10+
  '}';

  FRAGMENT_DEFAULT_SHADER =
  '#version 330 core'#13#10+
  'const vec4 eps = vec4(0.009, 0.009, 0.009, 0.009);'+
  'const vec4 maskColor = vec4(1.0, 1.0, 0.0, 0.0);'+
  'uniform usampler2D bitmap;'+
  'uniform sampler1D palette;'+
  'uniform int useTexture = 0;'+
  'uniform int usePalette = 0;'+
  'uniform int useFlag = 0;'+
  'uniform vec4 flagColor;'+
  'uniform vec4 fragmentColor;'#13#10+
  'in vec2 UV;'#13#10+
  'out vec4 outColor;'#13#10+

  'vec4 applyTexture(vec4 inColor)'+
  '{'+
      'if(useTexture == 1)'+
      '{'+
         'if(usePalette == 1)'+
          '{'+
              'return texelFetch(palette, int(texture(bitmap,UV).r), 0);'+
          '}'+
          'else'+
          '{' +
             // 'return texture(bitmap,UV);'+ //???
          '}'+
      '}'+
       'return inColor;'+
  '}'+

  'vec4 applyFlag(vec4 inColor)' +
  '{'+
     'if(useFlag == 1)'+
     '{'+
        'if(all(greaterThanEqual(inColor,maskColor-eps)) && all(lessThanEqual(inColor,maskColor+eps)))'+
        '  return flagColor;'+
     '}'+
     'return inColor;'+
  '}'+

  'void main(){'+
      'outColor = applyTexture(fragmentColor);'+
      'outColor = applyFlag(outColor);'+
  '}';

type

  TGLSprite = record
    TextureID: GLuint;
    PaletteID: Gluint;
    Width: Int32;
    Height: Int32;

    TopMagin: int32;
    LeftMargin: int32;

    SpriteWidth: Int32;
    SpriteHeight: int32;

    X: Int32;
    Y: Int32;
  end;

  { TGlobalState }

  TGlobalState = class
  private
    const
       DEFAULT_BUFFER:packed array[1..12] of GLfloat = (0,0,0,0,0,0, 0,0,0,0,0,0);//deprecated
  private
    FCurrentProgram: GLuint;
    EmptyBufferData:array of GLfloat; //used to initialize VBO
  private

    DefaultProgram: GLuint;
    DefaultFragmentColorUniform: GLint;

    DefaultProjMatrixUniform: GLint;
    DefaultCoordsAttrib: GLint;
    UVAttrib: GLint;
    UseTextureUniform: GLint;
    UsePaletteUniform: GLint;
    UseFlagUniform: GLint;

    PaletteUniform: GLint;
    BitmapUniform: GLint;
    FlagColorUniform: GLint;

    CoordsBuffer: GLuint;

    MirroredUVBuffers: array[0..3] of GLuint;

    procedure SetupUVBuffer;

  public
    destructor Destroy; override;
    procedure Init;

    procedure UseNoTextures();
    procedure UsePalettedTextures();

    procedure SetFlagColor(FlagColor: TRBGAColor);
    procedure SetFragmentColor(AColor: TRBGAColor);
    procedure SetProjection(constref AMatrix: Tmatrix4_single);
    procedure SetOrtho(left, right, bottom, top: GLfloat);

    procedure SetUseFlag(const Value: Boolean);
    procedure SetUsePalette(const Value: Boolean);
    procedure SetUseTexture(const Value: Boolean);
  end;


  { TLocalState }

  TLocalState = class
  private
    SpriteVAO: array[0..3] of GLuint;
    RectVAO: GLuint;
    procedure SetupSpriteVAO;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;
  end;



procedure BindPalette(ATextureId: GLuint; ARawImage: Pointer);

procedure BindUncompressedPaletted(ATextureId: GLuint; w,h: Int32; ARawImage: Pointer);

procedure BindUncompressedRGBA(ATextureId: GLuint; w,h: Int32; var ARawImage);
procedure BindCompressedRGBA(ATextureId: GLuint; w,h: Int32; var ARawImage);
procedure Unbind(var ATextureId: GLuint); inline;

procedure RenderSprite(ASprite: TGLSprite; dim: integer = -1; mir: UInt8 = 0);
procedure RenderRect(x,y: Integer; dimx,dimy:integer);

procedure CheckGLErrors(Stage: string);

function MakeShaderProgram(const AVertexSource: AnsiString; const AFragmentSource: AnsiString):GLuint;

var
  GlobalContextState: TGlobalState;
  CurrentContextState:TLocalState = nil;

implementation


procedure BindRGBA(ATextureId: GLuint; w, h: Int32; ARawImage: Pointer; AInternalFormat: GLEnum); inline;
begin

  glBindTexture(GL_TEXTURE_2D, ATextureId);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexImage2D(GL_TEXTURE_2D, 0,AInternalFormat,w,h,0,GL_RGBA, GL_UNSIGNED_BYTE, ARawImage);

end;

procedure BindPalette(ATextureId: GLuint; ARawImage: Pointer);
begin
  glBindTexture(GL_TEXTURE_1D, ATextureId);

  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  glTexImage1D(GL_TEXTURE_1D, 0,GL_RGBA,256,0,GL_RGBA, GL_UNSIGNED_BYTE, ARawImage);


  CheckGLErrors('Bind palette');
end;


procedure BindUncompressedPaletted(ATextureId: GLuint; w, h: Int32;
  ARawImage: Pointer);
begin
  glBindTexture(GL_TEXTURE_2D, ATextureId);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  //glTexImage2D(GL_TEXTURE_2D, 0,GL_LUMINANCE,w,h,0,GL_RED, GL_UNSIGNED_BYTE, ARawImage);

  glTexImage2D(GL_TEXTURE_2D, 0,GL_R8UI,w,h,0,GL_RED_INTEGER, GL_UNSIGNED_BYTE, ARawImage);

   CheckGLErrors('Bind paletted');
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

procedure RenderSprite(ASprite: TGLSprite; dim: integer; mir: UInt8);
var
  factor: Double;
  cur_dim: integer;
  H,W,
  x,
  y: Int32;

  vertex_data: packed array[1..12] of GLfloat;
begin
  factor := 1;
  if dim <=0 then //render real size w|o scale
  begin
    H := ASprite.SpriteHeight;
    W := ASprite.Width;
  end
  else
  begin
    cur_dim := Max(ASprite.Width,ASprite.SpriteHeight);
    factor := Min(dim / cur_dim, 1); //no zoom

    h := round(Double(ASprite.SpriteHeight) * factor);
    w := round(Double(ASprite.Width) * factor);
  end;

   //todo: use indexes

  case mir of
    0:begin
      x := ASprite.x;//+round(factor * ASprite.LeftMargin);
      y := ASprite.y+round(factor * ASprite.TopMagin);
    end;
    1:begin
      x := ASprite.x;//+round(factor * (ASprite.Width - ASprite.SpriteWidth - ASprite.LeftMargin));
      y := ASprite.y+round(factor *  ASprite.TopMagin);

    end;
    2:begin
      x := ASprite.x;//+ round(factor * ASprite.LeftMargin);
      y := ASprite.y+round(factor * (ASprite.Height - ASprite.SpriteHeight - ASprite.TopMagin));

    end;
    3:begin
      x := ASprite.x;//+round(factor * (ASprite.Width - ASprite.SpriteWidth - ASprite.LeftMargin));
      y := ASprite.y+round(factor * (ASprite.Height - ASprite.SpriteHeight - ASprite.TopMagin));
     end;
  end;

  vertex_data[1] := x;   vertex_data[2] := y;
  vertex_data[3] := x+w; vertex_data[4] := y;
  vertex_data[5] := x+w; vertex_data[6] := y+h;

  vertex_data[7] := x+w; vertex_data[8] := y+h;
  vertex_data[9] := x;   vertex_data[10] := y+h;
  vertex_data[11] := x;  vertex_data[12] := y;

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D,ASprite.TextureID);

  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_1D,ASprite.PaletteID);

  glBindBuffer(GL_ARRAY_BUFFER,GlobalContextState.CoordsBuffer);
  glBufferSubData(GL_ARRAY_BUFFER,0,  sizeof(vertex_data),@vertex_data);
  glBindBuffer(GL_ARRAY_BUFFER,0);

  Assert(Assigned(CurrentContextState), 'no current context state');

  Assert(glIsVertexArray(CurrentContextState.SpriteVAO[mir]) = GL_TRUE, 'invalid SpriteVAO');

  glBindVertexArray(CurrentContextState.SpriteVAO[mir]);


  glEnableVertexAttribArray(GlobalContextState.DefaultCoordsAttrib);
  glEnableVertexAttribArray(GlobalContextState.UVAttrib);
  glDrawArrays(GL_TRIANGLES,0,6);
  glDisableVertexAttribArray(GlobalContextState.UVAttrib);
  glDisableVertexAttribArray(GlobalContextState.DefaultCoordsAttrib);

  glBindVertexArray(0);

  CheckGLErrors('render sprite mir='+IntToStr(mir)+ ' xy='+IntToStr(ASprite.X)+' '+ IntToStr(ASprite.Y));
end;

procedure RenderRect(x, y: Integer; dimx, dimy: integer);
const
  RECT_COLOR: TRBGAColor = (r:200; g:200; b:200; a:255);
var
  position_attrib_index: integer;

  vertex_data: packed array[1..8] of GLfloat;

  tmp_VAO: GLuint;
begin
  GlobalContextState.SetFragmentColor(RECT_COLOR);
//  glLineWidth(1);

  vertex_data[1] := x;
  vertex_data[2] := y;
  vertex_data[3] := x + dimx;
  vertex_data[4] := y;
  vertex_data[5] := x + dimx;
  vertex_data[6] := y + dimy;
  vertex_data[7] := x;
  vertex_data[8] := y + dimy;

  glGenVertexArrays(1, @tmp_VAO);
  glBindVertexArray(tmp_VAO);

  glBindBuffer(GL_ARRAY_BUFFER,GlobalContextState.CoordsBuffer);
  glBufferSubData(GL_ARRAY_BUFFER,0, sizeof(vertex_data),@vertex_data);

  glEnableVertexAttribArray(GlobalContextState.DefaultCoordsAttrib);

  glVertexAttribPointer(GlobalContextState.DefaultCoordsAttrib, 2, GL_FLOAT, GL_FALSE, 0,nil);

  glDrawArrays(GL_LINE_LOOP,0,4);

  glDisableVertexAttribArray(GlobalContextState.DefaultCoordsAttrib);

  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glDeleteVertexArrays(1, @tmp_VAO);
  CheckGLErrors('RenderRect');
end;

procedure CheckGLErrors(Stage: string);
var
  err: GLenum;
begin

  repeat
    err := glGetError();

    if err<>GL_NO_ERROR then
    begin
      DebugLogger.DebugLn(Stage +': Gl error '+IntToHex(err,8));
    end;

  until err = GL_NO_ERROR ;


end;

function MakeShader(const ShaderSource: AnsiString; ShaderType: GLenum): GLuint;
var
  shader_object: GLuint;
  status: Integer;
  info_log_len: GLint;

  info_log: string;
begin
  Result := 0;
  shader_object := glCreateShader(ShaderType);

  if shader_object = 0 then
  begin
    CheckGLErrors('MakeShader');
    Exit;
  end;

  glShaderSource(shader_object,1,@(ShaderSource),nil);
  glCompileShader(shader_object);
  status := GL_FALSE;
  glGetShaderiv(shader_object,GL_COMPILE_STATUS,@status);

  if status <> GL_TRUE then
  begin
    glGetShaderiv(shader_object,GL_INFO_LOG_LENGTH, @info_log_len);
    SetLength(info_log,info_log_len);
    glGetShaderInfoLog(shader_object,info_log_len,@info_log_len,@info_log[1]);

    DebugLn('Shader compile log:');
    DebugLn(info_log);
    exit;
  end;

  Result := shader_object;

end;

function MakeShaderProgram(const AVertexSource: AnsiString;
  const AFragmentSource: AnsiString): GLuint;
var
  vertex_shader, fragment_shader: GLuint;
  status: GLint;
  program_object: GLuint;
  doVertex: Boolean;
  doFragment: Boolean;
begin
  Result := 0;
  vertex_shader := 0;
  fragment_shader := 0;

  doVertex := AVertexSource <> '';
  doFragment := AFragmentSource <> '';

  program_object := glCreateProgram();

  if doVertex then
  begin
    vertex_shader := MakeShader(AVertexSource, GL_VERTEX_SHADER);
    if vertex_shader = 0 then Exit;
    glAttachShader(program_object, vertex_shader);
  end;

  if doFragment then
  begin
    fragment_shader := MakeShader(AFragmentSource, GL_FRAGMENT_SHADER);
    if fragment_shader = 0 then Exit;
    glAttachShader(program_object, fragment_shader);
  end;

  glLinkProgram(program_object);
  status := GL_FALSE;
  glGetProgramiv(program_object, GL_LINK_STATUS, @status);

  //todo: print log

  if (status = GL_TRUE) then
    Result := program_object;


  if doVertex then glDeleteShader(vertex_shader); //always mark shader for deletion
  if doFragment then glDeleteShader(fragment_shader);
end;

{ TGlobalState }

procedure TGlobalState.SetupUVBuffer;
var
  uv_data: packed array[1..12] of GLfloat;
  u: GLfloat;
  v: GLfloat;
  mir: Integer;
begin
  glGenBuffers(Length(MirroredUVBuffers), @MirroredUVBuffers);

  u:=1;
  v:=1;

  for mir := 0 to 3 do
  begin
    glBindBuffer(GL_ARRAY_BUFFER,MirroredUVBuffers[mir]);
    case mir of
      0:begin
          uv_data[1] := 0;   uv_data[2] := 0;
          uv_data[3] := u;   uv_data[4] := 0;
          uv_data[5] := u;   uv_data[6] := v;

          uv_data[7] := u;   uv_data[8] := v;
          uv_data[9] := 0;   uv_data[10] := v;
          uv_data[11] := 0;   uv_data[12] := 0;
      end;
      1: begin
            uv_data[1] := u;   uv_data[2] := 0;
            uv_data[3] := 0;   uv_data[4] := 0;
            uv_data[5] := 0;   uv_data[6] := v;

            uv_data[7] := 0;   uv_data[8] := v;
            uv_data[9] := u;   uv_data[10] := v;
            uv_data[11] := u;   uv_data[12] := 0;
        end;
      2: begin
            uv_data[1] := 0;   uv_data[2] := v;
            uv_data[3] := u;   uv_data[4] := v;
            uv_data[5] := u;   uv_data[6] := 0;

            uv_data[7] := u;   uv_data[8] := 0;
            uv_data[9] := 0;   uv_data[10] := 0;
            uv_data[11] := 0;   uv_data[12] := v;
        end;
      3:begin
          uv_data[1] := u;   uv_data[2] := v;
          uv_data[3] := 0;   uv_data[4] := v;
          uv_data[5] := 0;   uv_data[6] := 0;

          uv_data[7] := 0;   uv_data[8] := 0;
          uv_data[9] := u;   uv_data[10] := 0;
          uv_data[11] := u;   uv_data[12] := v;
        end;
     end;
     glBufferData(GL_ARRAY_BUFFER, sizeof(uv_data),@uv_data,GL_STATIC_DRAW);
  end;
end;

destructor TGlobalState.Destroy;
begin
  glDeleteProgram(DefaultProgram);

  inherited Destroy;
end;

procedure TGlobalState.Init;
begin
  DefaultProgram := MakeShaderProgram(VERTEX_DEFAULT_SHADER, FRAGMENT_DEFAULT_SHADER);
  if DefaultProgram = 0 then
    raise Exception.Create('Error compiling default shader');

  DefaultFragmentColorUniform:= glGetUniformLocation(DefaultProgram, PChar('fragmentColor'));


  DefaultProjMatrixUniform := glGetUniformLocation(DefaultProgram, PChar('projMatrix'));
  DefaultCoordsAttrib := glGetAttribLocation(DefaultProgram, PChar('coords'));
  UVAttrib:=glGetAttribLocation(DefaultProgram, PChar('uv'));

  UseFlagUniform:=glGetUniformLocation(DefaultProgram, PChar('useFlag'));
  UsePaletteUniform:=glGetUniformLocation(DefaultProgram, PChar('usePalette'));
  UseTextureUniform:=glGetUniformLocation(DefaultProgram, PChar('useTexture'));

  PaletteUniform:=glGetUniformLocation(DefaultProgram, PChar('palette'));
  FlagColorUniform:=glGetUniformLocation(DefaultProgram, PChar('flagColor'));
  BitmapUniform:=glGetUniformLocation(DefaultProgram, PChar('bitmap'));

  CheckGLErrors('default shader get uniforms');


  glGenBuffers(1,@CoordsBuffer);

  glBindBuffer(GL_ARRAY_BUFFER,GlobalContextState.CoordsBuffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(DEFAULT_BUFFER),@DEFAULT_BUFFER,GL_STREAM_DRAW);


  SetupUVBuffer;
  CheckGLErrors('VBO');




  //glUseProgram(DefaultProgram);
  //FCurrentProgram:=DefaultProgram;
end;

procedure TGlobalState.SetFlagColor(FlagColor: TRBGAColor);

begin
  if FCurrentProgram = DefaultProgram then
  begin
    glUniform4f(FlagColorUniform, FlagColor.r/255, FlagColor.g/255, FlagColor.b/255, FlagColor.a/255);
  end;

end;

procedure TGlobalState.SetFragmentColor(AColor: TRBGAColor);
begin
  if FCurrentProgram = DefaultProgram then
  begin
    glUniform4f(DefaultFragmentColorUniform, AColor.r/255, AColor.g/255, AColor.b/255, AColor.a/255 );
  end
  else
    Assert(false);
end;

procedure TGlobalState.SetProjection(constref AMatrix: Tmatrix4_single);
begin
  if FCurrentProgram = DefaultProgram then
  begin
    glUniformMatrix4fv(DefaultProjMatrixUniform,1,GL_TRUE,@AMatrix.data);
  end
  else
    Assert(false);
end;

procedure TGlobalState.SetOrtho(left, right, bottom, top: GLfloat);
var
  M:Tmatrix4_single;
begin
  m.init_identity;
  m.data[0,0] := 2 /(right - left);
  m.data[1,1] := 2 /(top - bottom);
  m.data[2,2] := -2;

  m.data[0,3] := - (right+left)/(right-left);
  m.data[1,3] := - (top + bottom)/(top - bottom);
  m.data[2,3] := - 1;

  SetProjection(m);
end;

procedure TGlobalState.SetUseFlag(const Value: Boolean);
begin
  glUniform1i(UseFlagUniform, ifthen(Value, 1, 0));
end;

procedure TGlobalState.SetUsePalette(const Value: Boolean);
begin
  glUniform1i(UsePaletteUniform, ifthen(Value, 1, 0));
end;

procedure TGlobalState.SetUseTexture(const Value: Boolean);
begin
  glUniform1i(UseTextureUniform, ifthen(Value, 1, 0));
end;

procedure TGlobalState.UseNoTextures;
begin
  FCurrentProgram := DefaultProgram;
  glUseProgram(DefaultProgram);
  SetUseFlag(false);
  SetUsePalette(false);
  SetUseTexture(False);
end;

procedure TGlobalState.UsePalettedTextures;
begin
  //FCurrentProgram := PaletteFlagProgram;
  //  glUseProgram(FCurrentProgram);
  FCurrentProgram := DefaultProgram;
  glUseProgram(DefaultProgram);

  SetUseFlag(true);
  SetUsePalette(true);
  SetUseTexture(true);

  glUniform1i(BitmapUniform, 0); //texture unit0
  glUniform1i(PaletteUniform, 1);//texture unit1

end;

{ TLocalState }

constructor TLocalState.Create;
begin

end;

destructor TLocalState.Destroy;
begin
  inherited Destroy;
end;

procedure TLocalState.Init;
begin
  SetupSpriteVAO;
  CheckGLErrors('VAO');
end;

procedure TLocalState.SetupSpriteVAO;
var
  i: Integer;
  BufferHandle: LongWord;
begin
  glGenVertexArrays(Length(SpriteVAO),@SpriteVAO[0]);

  for i := Low(SpriteVAO) to High(SpriteVAO) do
  begin
    glBindVertexArray(SpriteVAO[i]);

    glBindBuffer(GL_ARRAY_BUFFER,GlobalContextState.CoordsBuffer);
    glEnableVertexAttribArray(GlobalContextState.DefaultCoordsAttrib);
    glVertexAttribPointer(GlobalContextState.DefaultCoordsAttrib, 2, GL_FLOAT, GL_FALSE, 0,nil);

    BufferHandle:=GlobalContextState.MirroredUVBuffers[i];

    glBindBuffer(GL_ARRAY_BUFFER,BufferHandle);

    glEnableVertexAttribArray(GlobalContextState.UVAttrib);
    glVertexAttribPointer(GlobalContextState.UVAttrib, 2, GL_FLOAT, GL_FALSE, 0,nil);


    glDisableVertexAttribArray(GlobalContextState.UVAttrib);
    glDisableVertexAttribArray(GlobalContextState.DefaultCoordsAttrib);

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindVertexArray(0);
  end;

end;


end.

