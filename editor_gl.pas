{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit editor_gl;

{$I compilersetup.inc}

interface

uses
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  Classes, SysUtils, Controls, math, matrix, GL, vcmi.OpenGLContext, vcmi.glext, editor_types, editor_consts;

const
  DEFAULT_F_S_RES = 'DEFAULT_FRAGMENT_SHADER';
  DEFAULT_V_S_RES = 'DEFAULT_VERTEX_SHADER';

type

  { TGLSprite }

  PGLSprite = ^TGLSprite;

  TGLSprite = record
    TextureID: GLuint;
    PaletteID: Gluint;//not owned

    Width: Int32;
    Height: Int32;

    TopMargin: int32;
    LeftMargin: int32;

    SpriteWidth: Int32;
    SpriteHeight: int32;

    procedure Init; inline;
  end;

  { TShaderResourceStream }

  TShaderResourceStream = class(TResourceStream)
  private
    FShaderType: GLenum;
  public
    constructor Create(const ResName: string; AShaderType: GLenum);

    function Make: GLuint;
  end;

  { TShaderProgram }

  TShaderProgram = class
  strict private
    FHandle:GLuint;

    FProjMatrixUniform: GLint;
    FTranslateMatrixUniform: GLint;
  public
    constructor Create(const AVertexSource: AnsiString; const AFragmentSource: AnsiString);
    destructor Destroy; override;

    procedure SetProjection(constref AMatrix: Tmatrix4_single);
    procedure SetTranslation(constref AMatrix: Tmatrix4_single);

    property Handle: GLuint read FHandle;
  end;

  { TGlobalState }

  TGlobalState = class

  public
    constructor Create;
    destructor Destroy; override;

  end;


  { TLocalState }

  TLocalState = class
  strict private
  const
     VERTEX_BUFFER_SIZE = 4 * 2 * 3 * 2; //4 rotation * 2 triangles * 3 poitns * 2 coordinates
     UV_BUFFER_SIZE = VERTEX_BUFFER_SIZE; //also 2 coordinates
  var
    FInitialised: Boolean;
    FContext: TOpenGLControl;

    SpriteVAO: GLuint;
    RectVAO: GLuint;

    DefaultProgram: TShaderProgram;

    FCurrentProgram: TShaderProgram;

    FScale: GLfloat;

    FCoordAttribLocation: GLint;
    FUVAttribLocation: GLint;

    DefaultFragmentColorUniform: GLint;

    UseTextureUniform: GLint;
    UseFlagUniform: GLint;
    PaletteUniform: GLint;
    BitmapUniform: GLint;
    BitmapRGBUniform: GLint;

    FlagColorUniform: GLint;

    CoordsBuffer: GLuint;
    MirroredUVBuffer: GLuint;

    procedure Init;

    procedure SetupUVBuffer;
    procedure SetupCoordsBuffer;

    procedure SetupSpriteVAO;
    procedure SetupRectVAO;

    procedure DoRenderSprite(constref ASprite: TGLSprite; x, y, w, h: Int32; mir: UInt8);
 public
    constructor Create(AContext: TOpenGLControl);
    destructor Destroy; override;

    procedure SetFlagColor(FlagColor: TRBGAColor);
    procedure SetFragmentColor(AColor: TRBGAColor);
    procedure SetOrtho(left, right, bottom, top: GLfloat);

    procedure SetPlayerColor(APlayer: TPlayer);
    procedure SetTranslation(X,Y: Integer);

    procedure SetUseFlag(const Value: Boolean);

    procedure UseTextures(Use, withpalette: Boolean);
    procedure UsePalette(Use: Boolean);

    function StartFrame: Boolean;
    procedure FinishFrame;

    procedure StartDrawingRects;
    procedure RenderRect(x, y: Integer; dimx, dimy: integer);

    procedure RenderSolidRect(x, y: Integer; dimx, dimy: integer; color: TRBGAColor);

    procedure RenderGrid(AColor: TRBGAColor; ATileSize: Integer; TilesW,TilesH: integer);

    procedure StartDrawingSprites;
    procedure RenderSpriteMirrored(ASprite: PGLSprite; mir: UInt8);
    procedure RenderSpriteSimple(ASprite: PGLSprite);
    procedure RenderSpriteIcon(ASprite: PGLSprite; dim: Integer);
    procedure RenderSpriteOverlayIcon(ASprite: PGLSprite; dim: Integer; main_h: integer);

    procedure StopDrawing;

    procedure EnableScissor(); inline;
    procedure DisableScissor(); inline;

    procedure SetScissor();

    function UseDefaultProgram: TShaderProgram;

    property Scale: GLfloat read FScale write FScale;
  end;

procedure SetupGLControl(AControl: TOpenGLControl; ARoot: TOpenGLControl);

procedure BindPalette(ATextureId: GLuint; ARawImage: Pointer);
procedure BindUncompressedPaletted(ATextureId: GLuint; w,h: Int32; ARawImage: Pointer);
procedure BindUncompressedPalettedSub(ATextureId: GLuint; x, y, w, h: Int32; ARawImage: Pointer);
procedure BindUncompressedRGBA(ATextureId: GLuint; w,h: Int32; var ARawImage);

procedure Unbind(var ATextureId: GLuint); inline;

procedure CheckGLErrors(Stage: string);

function MakeShaderProgram(const AVertexSource: AnsiString; const AFragmentSource: AnsiString):GLuint;


implementation

uses LazLoggerBase;

procedure BindRGBA(ATextureId: GLuint; w, h: Int32; ARawImage: Pointer; AInternalFormat: GLEnum); inline;
begin
  glBindTexture(GL_TEXTURE_2D, ATextureId);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexImage2D(GL_TEXTURE_2D, 0, AInternalFormat, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, ARawImage);

  CheckGLErrors('Bind RGBA');
end;

procedure SetupGLControl(AControl: TOpenGLControl; ARoot: TOpenGLControl);
begin
  AControl.AlphaBits:=8;
  AControl.AutoResizeViewport := true;
  AControl.OpenGLMajorVersion:=3;
  AControl.OpenGLMinorVersion:=3;

  if Assigned(ARoot) and (AControl <> ARoot) then
  begin
    AControl.SharedControl := ARoot;
  end;
end;

procedure BindPalette(ATextureId: GLuint; ARawImage: Pointer);
begin
  glBindTexture(GL_TEXTURE_1D, ATextureId);

  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  glTexImage1D(GL_TEXTURE_1D, 0,GL_RGBA,256,0,GL_RGBA, GL_UNSIGNED_BYTE, ARawImage);


  CheckGLErrors('Bind palette');
end;

procedure BindUncompressedPaletted(ATextureId: GLuint; w, h: Int32; ARawImage: Pointer);
begin
  glBindTexture(GL_TEXTURE_2D, ATextureId);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8UI, w, h, 0, GL_RED_INTEGER, GL_UNSIGNED_BYTE, ARawImage);

  CheckGLErrors('Bind paletted');
end;

procedure BindUncompressedPalettedSub(ATextureId: GLuint; x, y, w, h: Int32; ARawImage: Pointer);
begin
  glBindTexture(GL_TEXTURE_2D, ATextureId);
  glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, w, h,GL_RED_INTEGER,GL_UNSIGNED_BYTE, ARawImage);
  CheckGLErrors('Bind paletted sub');
end;

procedure BindUncompressedRGBA(ATextureId: GLuint; w, h: Int32; var ARawImage);
begin
  BindRGBA(ATextureId,w, h,@ARawImage,GL_RGBA);
end;

procedure Unbind(var ATextureId: GLuint);
begin
  glDeleteTextures(1,@ATextureId);
  ATextureId := 0;
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

  until err = GL_NO_ERROR;
end;

function MakeShader(const ShaderSource: AnsiString; ShaderType: GLenum): GLuint;
var
  stm: TShaderResourceStream;
begin
  stm := TShaderResourceStream.Create(ShaderSource, ShaderType);
  try
    Result := stm.Make;
  finally
    stm.Free;
  end;
end;

function MakeShaderProgram(const AVertexSource: AnsiString; const AFragmentSource: AnsiString): GLuint;
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

  if (status = GL_TRUE) then
    Result := program_object;


  if doVertex then glDeleteShader(vertex_shader); //always mark shader for deletion
  if doFragment then glDeleteShader(fragment_shader);
end;

{ TShaderProgram }

constructor TShaderProgram.Create(const AVertexSource: AnsiString; const AFragmentSource: AnsiString);
begin
  FHandle := MakeShaderProgram(AVertexSource,AFragmentSource);

  if FHandle = 0 then
    raise Exception.Create('Error compiling shader');

  FProjMatrixUniform := glGetUniformLocation(Handle, PChar('projMatrix'));
  FTranslateMatrixUniform := glGetUniformLocation(Handle, PChar('translateMatrix'));
end;

destructor TShaderProgram.Destroy;
begin
  glDeleteProgram(FHandle);
  inherited Destroy;
end;

procedure TShaderProgram.SetProjection(constref AMatrix: Tmatrix4_single);
begin
  glUniformMatrix4fv(FProjMatrixUniform,1,GL_TRUE,@AMatrix.data);
end;

procedure TShaderProgram.SetTranslation(constref AMatrix: Tmatrix4_single);
begin
  glUniformMatrix4fv(FTranslateMatrixUniform,1,GL_TRUE,@AMatrix.data);
end;

{ TShaderResourceStream }

constructor TShaderResourceStream.Create(const ResName: string; AShaderType: GLenum);
begin
  inherited Create(HINSTANCE, ResName, RT_RCDATA);
  FShaderType := AShaderType;
end;

function TShaderResourceStream.Make: GLuint;
var
  status: Integer;
  info_log_len: GLint;

  info_log: string;

  p: Pointer;
  sizes: array[0..0] of GLint;
begin
  Result := glCreateShader(FShaderType);

  if Result = 0 then
  begin
    CheckGLErrors('MakeShader');
    Exit;
  end;

  p := Memory;

  sizes[0] := Size;

  glShaderSource(Result,1, @p, @sizes);
  glCompileShader(Result);
  status := GL_FALSE;
  glGetShaderiv(Result,GL_COMPILE_STATUS,@status);

  if status <> GL_TRUE then
  begin
    glGetShaderiv(Result,GL_INFO_LOG_LENGTH, @info_log_len);
    SetLength(info_log,info_log_len);
    glGetShaderInfoLog(Result,info_log_len,@info_log_len,@info_log[1]);

    DebugLn('Shader compile log:');
    DebugLn(info_log);
    exit(0);
  end;
end;

{ TGLSprite }

procedure TGLSprite.Init;
begin
  TextureID:=0;
  PaletteID:=0;
end;


{ TGlobalState }


constructor TGlobalState.Create;
begin

end;

destructor TGlobalState.Destroy;
begin

  inherited Destroy;
end;


{ TLocalState }

constructor TLocalState.Create(AContext: TOpenGLControl);
begin
  FInitialised := False;
  FContext := AContext;
  FScale:=1.0;
end;

destructor TLocalState.Destroy;
begin
  FreeAndNil(DefaultProgram);
  inherited Destroy;
end;

function TLocalState.StartFrame: Boolean;
begin
  if FContext.MakeCurrent() then
  begin
    Result := true;

    if not FInitialised then
    begin
      Init;
      FInitialised:=True;
    end;
  end
  else
    Result := false;
end;

procedure TLocalState.FinishFrame;
begin
  StopDrawing;
  FContext.SwapBuffers;
end;

procedure TLocalState.Init;
begin
  DefaultProgram := TShaderProgram.Create(DEFAULT_V_S_RES, DEFAULT_F_S_RES);

  CheckGLErrors('default shader create');

  DefaultFragmentColorUniform:= glGetUniformLocation(DefaultProgram.Handle, PChar('fragmentColor'));

  UseFlagUniform:=glGetUniformLocation(DefaultProgram.Handle, PChar('useFlag'));
  UseTextureUniform:=glGetUniformLocation(DefaultProgram.Handle, PChar('useTexture'));

  PaletteUniform:=glGetUniformLocation(DefaultProgram.Handle, PChar('palette'));
  FlagColorUniform:=glGetUniformLocation(DefaultProgram.Handle, PChar('flagColor'));
  BitmapUniform:=glGetUniformLocation(DefaultProgram.Handle, PChar('bitmap'));
  BitmapRGBUniform:=glGetUniformLocation(DefaultProgram.Handle, PChar('bitmapRGB'));

  FCoordAttribLocation:=glGetAttribLocation(DefaultProgram.Handle, PChar('coords'));
  FUVAttribLocation:=glGetAttribLocation(DefaultProgram.Handle, PChar('uv'));

  CheckGLErrors('TLocalState: shader uniforms setup');

  SetupCoordsBuffer;
  SetupUVBuffer;
  CheckGLErrors('TLocalState: VBO setup');

  SetupSpriteVAO;
  SetupRectVAO;
  CheckGLErrors('TLocalState: VAO setup');

  glDisable(GL_DEPTH_TEST);
  glDisable(GL_DITHER);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  CheckGLErrors('TLocalState: settings');
end;

procedure TLocalState.SetupUVBuffer;
var
  uv_data: packed array of GLfloat;
  u: GLfloat;
  v: GLfloat;

  ofc: integer;

  function next_ofc(): integer;
  begin
    Result := ofc;
    inc(ofc);
  end;
begin
  SetLength(uv_data, UV_BUFFER_SIZE);

  u:=1;
  v:=1;
  ofc := 0;


  //0
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := 0;
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := 0;
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := v;

  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := 0;
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := v;
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := v;

  //1
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := 0;
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := 0;
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := v;

  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := 0;
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := v;
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := v;

  //2
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := v;
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := v;
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := 0;

  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := v;
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := 0;
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := 0;

  //3
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := v;
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := v;
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := 0;

  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := v;
  uv_data[next_ofc()] := 0;   uv_data[next_ofc()] := 0;
  uv_data[next_ofc()] := u;   uv_data[next_ofc()] := 0;


  glGenBuffers(1, @MirroredUVBuffer);
  glBindBuffer(GL_ARRAY_BUFFER,MirroredUVBuffer);
  glBufferData(GL_ARRAY_BUFFER,UV_BUFFER_SIZE * SizeOf(GLfloat),@uv_data[0],GL_STATIC_DRAW);

end;

procedure TLocalState.SetupCoordsBuffer;
begin
  glGenBuffers(1,@CoordsBuffer);
  glBindBuffer(GL_ARRAY_BUFFER, CoordsBuffer);
  glBufferData(GL_ARRAY_BUFFER, VERTEX_BUFFER_SIZE * SizeOf(GLfloat), nil, GL_STREAM_DRAW);
end;

procedure TLocalState.RenderRect(x, y: Integer; dimx, dimy: integer);
var
  vertex_data: packed array[1..8] of GLfloat;
begin
  vertex_data[1] := X;         vertex_data[2] := Y;
  vertex_data[3] := X + dimx;  vertex_data[4] := Y;
  vertex_data[5] := X + dimx;  vertex_data[6] := Y + dimy;
  vertex_data[7] := X;         vertex_data[8] := Y + dimy;

  glBindBuffer(GL_ARRAY_BUFFER, CoordsBuffer);
  glBufferSubData(GL_ARRAY_BUFFER,0, sizeof(vertex_data),@vertex_data);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glDrawArrays(GL_LINE_LOOP,0,4);
end;

procedure TLocalState.RenderSolidRect(x, y: Integer; dimx, dimy: integer;
  color: TRBGAColor);
var
  vertex_data: packed array[1..12] of GLfloat;
begin
  SetFragmentColor(color);

  vertex_data[1] := x;   vertex_data[2] := y;
  vertex_data[3] := x+dimx; vertex_data[4] := y;
  vertex_data[5] := x+dimx; vertex_data[6] := y+dimy;

  vertex_data[7] := x+dimx; vertex_data[8] := y+dimy;
  vertex_data[9] := x;   vertex_data[10] := y+dimy;
  vertex_data[11] := x;  vertex_data[12] := y;

  glBindBuffer(GL_ARRAY_BUFFER, CoordsBuffer);
  glBufferSubData(GL_ARRAY_BUFFER, sizeof(vertex_data),  sizeof(vertex_data),@vertex_data);
  glBindBuffer(GL_ARRAY_BUFFER,0);

  glDrawArrays(GL_TRIANGLES,6,6);  //todo: use triangle strip
end;

procedure TLocalState.RenderGrid(AColor: TRBGAColor; ATileSize: Integer; TilesW, TilesH: integer);
begin
  //todo:
end;

procedure TLocalState.StopDrawing;
begin
  glBindVertexArray(0);
  glDisableVertexAttribArray(FCoordAttribLocation);
  glDisableVertexAttribArray(FUVAttribLocation);
  SetTranslation(0,0);
end;

procedure TLocalState.EnableScissor();
begin
  glEnable(GL_SCISSOR_TEST);
end;

procedure TLocalState.DisableScissor();
begin
  glDisable(GL_SCISSOR_TEST);
end;

procedure TLocalState.SetScissor();
begin
  glScissor(0, 0, FContext.Width, FContext.Height);
end;

function TLocalState.UseDefaultProgram: TShaderProgram;
begin
  Result := DefaultProgram;
  glUseProgram(Result.Handle);
end;

procedure TLocalState.StartDrawingRects;
begin
  SetTranslation(0,0);
  glBindVertexArray(RectVAO);
  glEnableVertexAttribArray(FCoordAttribLocation);
  glDisableVertexAttribArray(FUVAttribLocation);
end;

procedure TLocalState.StartDrawingSprites;
begin
  SetTranslation(0,0);
  glBindVertexArray(SpriteVAO);
  glEnableVertexAttribArray(FCoordAttribLocation);
  glEnableVertexAttribArray(FUVAttribLocation);
end;

procedure TLocalState.RenderSpriteMirrored(ASprite: PGLSprite; mir: UInt8);
var
  H,W,
  x,
  y,
  TopMargin: Int32;
begin
  H := ASprite^.SpriteHeight;
  W := ASprite^.Width;
  TopMargin := ASprite^.TopMargin;

  //todo: use indexes
  x := 0;
  y := 0;

  case mir of
    0:begin
      x := 0;//+round(factor * ASprite.LeftMargin);
      y := 0 +TopMargin;
    end;
    1:begin
      x := 0;//+round(factor * (ASprite.Width - ASprite.SpriteWidth - ASprite.LeftMargin));
      y := 0+TopMargin;
    end;
    2:begin
      x := 0;//+ round(factor * ASprite.LeftMargin);
      y := 0+(ASprite^.Height - H - TopMargin);
    end;
    3:begin
      x := 0;//+round(factor * (ASprite.Width - ASprite.SpriteWidth - ASprite.LeftMargin));
      y := 0+(ASprite^.Height - H - TopMargin);
    end;
  end;

  DoRenderSprite(ASprite^, x,y,w,h, mir);
end;

procedure TLocalState.RenderSpriteSimple(ASprite: PGLSprite);
var
  H,W,
  x,
  y,
  TopMargin: Int32;
begin
  H := ASprite^.SpriteHeight;
  W := ASprite^.Width;
  TopMargin := ASprite^.TopMargin;
  x := 0;//+round(factor * ASprite^.LeftMargin);
  y := TopMargin;
  DoRenderSprite(ASprite^, x,y,w,h, 0);
end;

procedure TLocalState.RenderSpriteIcon(ASprite: PGLSprite; dim: Integer);
var
  factor: Double;
  cur_dim: integer;
  H, W, x, y, TopMargin: Int32;
begin
  cur_dim := Max(ASprite^.Width,ASprite^.SpriteHeight);
  factor := Min((dim) / cur_dim, 1); //no zoom
  h := round(Double(ASprite^.SpriteHeight) * factor);
  w := round(Double(ASprite^.Width) * factor);
  TopMargin := dim - h;
  x := 0;//+round(factor * ASprite.LeftMargin);
  y := TopMargin;
  DoRenderSprite(ASprite^, x,y,w,h, 0);
end;

procedure TLocalState.RenderSpriteOverlayIcon(ASprite: PGLSprite; dim: Integer; main_h: integer);
var
  factor: Double;
  cur_dim: integer;
  H, W, x, y, TopMargin: Int32;
begin
  cur_dim := Max(ASprite^.Width,ASprite^.SpriteHeight);
  factor := Min((dim) / cur_dim, 1); //no zoom
  h := round(Double(ASprite^.SpriteHeight) * factor);
  w := round(Double(ASprite^.Width) * factor);
  TopMargin := dim - round(Double(main_h) * factor);
  x := 0;//+round(factor * ASprite.LeftMargin);
  y := TopMargin;
  DoRenderSprite(ASprite^, x,y,w,h, 0);
end;

procedure TLocalState.SetOrtho(left, right, bottom, top: GLfloat);
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

  FCurrentProgram.SetProjection(m);
end;

procedure TLocalState.SetPlayerColor(APlayer: TPlayer);
begin
  if APlayer = TPlayer.NONE then
  begin
    SetFlagColor(NEUTRAL_PLAYER_COLOR);
  end
  else
  begin
    SetFlagColor(PLAYER_FLAG_COLORS[APlayer]);
  end;
end;

procedure TLocalState.SetTranslation(X, Y: Integer);
var
  FTranslateMaxrix: Tmatrix4_single;
begin
  FTranslateMaxrix.init_identity;
  FTranslateMaxrix.data[0,0] := FScale;
  FTranslateMaxrix.data[1,1] := FScale;

  FTranslateMaxrix.data[0,3] := x * FScale;
  FTranslateMaxrix.data[1,3] := y * FScale;

  FCurrentProgram.SetTranslation(FTranslateMaxrix);
end;

procedure TLocalState.SetupSpriteVAO;
begin
  glGenVertexArrays(1,@SpriteVAO);
  glBindVertexArray(SpriteVAO);
  glBindBuffer(GL_ARRAY_BUFFER, CoordsBuffer);
  glVertexAttribPointer(FCoordAttribLocation, 2, GL_FLOAT, GL_FALSE, 0,nil);
  glBindBuffer(GL_ARRAY_BUFFER, MirroredUVBuffer);
  glVertexAttribPointer(FUVAttribLocation, 2, GL_FLOAT, GL_FALSE, 0,nil);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
end;

procedure TLocalState.SetupRectVAO;
begin
   glGenVertexArrays(1,@RectVAO);
   glBindVertexArray(RectVAO);
   glBindBuffer(GL_ARRAY_BUFFER,CoordsBuffer);
   glVertexAttribPointer(FCoordAttribLocation, 2, GL_FLOAT, GL_FALSE, 0,nil);
   glBindBuffer(GL_ARRAY_BUFFER, 0);
   glBindVertexArray(0);
end;

procedure TLocalState.SetFlagColor(FlagColor: TRBGAColor);
begin
  glUniform4f(FlagColorUniform, FlagColor.r/255, FlagColor.g/255, FlagColor.b/255, FlagColor.a/255);
end;

procedure TLocalState.SetFragmentColor(AColor: TRBGAColor);
begin
  glUniform4f(DefaultFragmentColorUniform, AColor.r/255, AColor.g/255, AColor.b/255, AColor.a/255 );
end;

procedure TLocalState.DoRenderSprite(constref ASprite: TGLSprite; x, y, w, h: Int32; mir: UInt8);
var
  vertex_data: packed array[1..12] of GLfloat;
begin
  if ASprite.TextureID = 0 then
  begin
    Exit;
  end;

  vertex_data[1] := x;   vertex_data[2] := y;
  vertex_data[3] := x+w; vertex_data[4] := y;
  vertex_data[5] := x+w; vertex_data[6] := y+h;

  vertex_data[7] := x;   vertex_data[8] := y;
  vertex_data[9] := x+w; vertex_data[10] := y+h;
  vertex_data[11] := x;  vertex_data[12] := y+h;

  if ASprite.PaletteID <> 0 then
  begin
    UsePalette(true);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D,ASprite.TextureID);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_1D,ASprite.PaletteID);
  end
  else
  begin
    UsePalette(false);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D,ASprite.TextureID);
  end;

  glBindBuffer(GL_ARRAY_BUFFER,CoordsBuffer);
  glBufferSubData(GL_ARRAY_BUFFER, mir*sizeof(vertex_data),  sizeof(vertex_data),@vertex_data);
  glBindBuffer(GL_ARRAY_BUFFER,0);

  glDrawArrays(GL_TRIANGLES,mir*6,6);  //todo: use triangle strip
end;

procedure TLocalState.SetUseFlag(const Value: Boolean);
begin
  glUniform1i(UseFlagUniform, ifthen(Value, 1, 0));
end;

procedure TLocalState.UseTextures(Use, withpalette: Boolean);
begin
  FCurrentProgram := UseDefaultProgram();

  if Use then
  begin
    glUniform1i(BitmapUniform, 0); //texture unit0
    glUniform1i(PaletteUniform, 1);//texture unit1
    glUniform1i(BitmapRGBUniform, 2);//texture unit2
    glUniform1i(UseTextureUniform, ifthen(withpalette, 1, 2));
  end
  else
  begin
    SetUseFlag(false);
    glUniform1i(UseTextureUniform, 0);
  end;
end;

procedure TLocalState.UsePalette(Use: Boolean);
begin
  if Use then
  begin
    glUniform1i(UseTextureUniform, 1);
  end
  else
  begin
    glUniform1i(UseTextureUniform, 2);
  end;
end;


end.

