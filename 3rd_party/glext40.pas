(* OpenGL extension loading library
   Generated from glext.h version 60 (OpenGL 4.0 initial release)
   Uses small parts of
   {
     Adaption of the delphi3d.net OpenGL units to FreePascal
     Sebastian Guenther (sg@freepascal.org) in 2002
   }
   These units are free to use
*)

{$MACRO ON}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall }
{$ELSE}
  {$DEFINE extdecl:=cdecl }
{$ENDIF}

unit glext40;
interface
uses
  SysUtils,
  {$IFDEF Windows}
  Windows,
  {$ELSE}
  dynlibs,
  {$ENDIF}
  GL;

{$IFDEF Windows}
{ Declared in Windows unit as well in FPC; but declared here as well, to be
  fully compatible to upstream version  - sg }
function wglGetProcAddress(proc: PChar): Pointer; extdecl; external 'OpenGL32.dll';
{$ELSE}
function wglGetProcAddress(proc: PChar): Pointer;
{$ENDIF}

// Test if the given extension name is present in the given extension string.
function glext_ExtensionSupported(const extension: String; const searchIn: String): Boolean;

// Load the extension with the given name.
function glext_LoadExtension(const ext: String): Boolean;

type
  GLcharARB = Char;
  TGLcharARB = GLcharARB;
  PGLcharARB = ^GLcharARB;
  PPGLcharARB = ^PGLcharARB;

  PPGLboolean = ^PGLboolean;

  GLhandleARB = Cardinal;
  TGLhandleARB = GLhandleARB;
  PGLhandleARB = ^GLhandleARB;

  GLintptr = PtrInt;
  TGLintptr = GLintptr;
  PGLintptr = ^GLintptr;

  GLsizeiptr = PtrInt;
  TGLsizeiptr = GLsizeiptr;
  PGLsizeiptr = ^GLsizeiptr;

  GLintptrARB = PtrInt;
  TGLintptrARB = GLintptrARB;
  PGLintptrARB = ^GLintptrARB;

  GLsizeiptrARB = PtrInt;
  TGLsizeiptrARB = GLsizeiptrARB;
  PGLsizeiptrARB = ^GLsizeiptrARB;

  GLchar = Char;
  TGLchar = GLchar;
  PGLchar = ^TGLchar;
  PPGLchar = ^PGLchar;

  GLint64 = Int64;
  TGLint64 = GLint64;
  PGLint64 = ^GLint64;

  GLuint64 = QWord;
  TGLuint64 = GLuint64;
  PGLuint64 = ^GLuint64;

  GLint64EXT = Int64;
  TGLint64EXT = GLint64EXT;
  PGLint64EXT = ^GLint64EXT;

  GLuint64EXT = QWord;
  TGLuint64EXT = GLuint64EXT;
  PGLuint64EXT = ^GLuint64EXT;

  GLhalfARB = shortint;
  TGLhalfARB = GLhalfARB;
  PGLhalfARB = ^GLhalfARB;

  GLhalfNV = shortint;
  TGLhalfNV = GLhalfNV;
  PGLhalfNV = ^GLhalfNV;

  GLsync = Pointer;
  TGLsync = GLSync;
  PGLsync = ^GLSync;


//**** GL_VERSION_1_2 *****//
const
  GL_UNSIGNED_BYTE_3_3_2 = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_INT_8_8_8_8 = $8035;
  GL_UNSIGNED_INT_10_10_10_2 = $8036;
  GL_TEXTURE_BINDING_3D = $806A;
  GL_PACK_SKIP_IMAGES = $806B;
  GL_PACK_IMAGE_HEIGHT = $806C;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_PROXY_TEXTURE_3D = $8070;
  GL_TEXTURE_DEPTH = $8071;
  GL_TEXTURE_WRAP_R = $8072;
  GL_MAX_3D_TEXTURE_SIZE = $8073;
  GL_UNSIGNED_BYTE_2_3_3_REV = $8362;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
  GL_BGR = $80E0;
  GL_BGRA = $80E1;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_CLAMP_TO_EDGE = $812F;
  GL_TEXTURE_MIN_LOD = $813A;
  GL_TEXTURE_MAX_LOD = $813B;
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;
  GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY = $0B23;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
var
  glBlendColor: procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); extdecl;
  glBlendEquation: procedure(mode: GLenum); extdecl;
  glDrawRangeElements: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid); extdecl;
  glTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glCopyTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); extdecl;

function Load_GL_VERSION_1_2(): Boolean;

//**** GL_ARB_imaging *****//
const
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_BLEND_COLOR = $8005;
  GL_FUNC_ADD = $8006;
  GL_MIN = $8007;
  GL_MAX = $8008;
  GL_BLEND_EQUATION = $8009;
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;

function Load_GL_ARB_imaging(): Boolean;


//**** GL_VERSION_1_3 *****//
const
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  GL_MULTISAMPLE = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_ALPHA_TO_ONE = $809F;
  GL_SAMPLE_COVERAGE = $80A0;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  GL_COMPRESSED_RGB = $84ED;
  GL_COMPRESSED_RGBA = $84EE;
  GL_TEXTURE_COMPRESSION_HINT = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
  GL_TEXTURE_COMPRESSED = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  GL_CLAMP_TO_BORDER = $812D;
var
  glActiveTexture: procedure(texture: GLenum); extdecl;
  glSampleCoverage: procedure(value: GLclampf; invert: GLboolean); extdecl;
  glCompressedTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexImage1D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexSubImage1D: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glGetCompressedTexImage: procedure(target: GLenum; level: GLint; img: PGLvoid); extdecl;

function Load_GL_VERSION_1_3(): Boolean;

//**** GL_VERSION_1_4 *****//
const
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_POINT_FADE_THRESHOLD_SIZE = $8128;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_DEPTH_COMPONENT32 = $81A7;
  GL_MIRRORED_REPEAT = $8370;
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
  GL_TEXTURE_LOD_BIAS = $8501;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;
  GL_TEXTURE_DEPTH_SIZE = $884A;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
var
  glBlendFuncSeparate: procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); extdecl;
  glMultiDrawArrays: procedure(mode: GLenum; first: PGLint; count: PGLsizei; primcount: GLsizei); extdecl;
  glMultiDrawElements: procedure(mode: GLenum; const count: PGLsizei; _type: GLenum; const indices: PPGLvoid; primcount: GLsizei); extdecl;
  glPointParameterf: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPointParameterfv: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glPointParameteri: procedure(pname: GLenum; param: GLint); extdecl;
  glPointParameteriv: procedure(pname: GLenum; const params: PGLint); extdecl;

function Load_GL_VERSION_1_4(): Boolean;

//**** GL_VERSION_1_5 *****//
const
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_QUERY_COUNTER_BITS = $8864;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY = $88B8;
  GL_WRITE_ONLY = $88B9;
  GL_READ_WRITE = $88BA;
  GL_BUFFER_ACCESS = $88BB;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_DRAW = $88E0;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_DRAW = $88E4;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_DRAW = $88E8;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;
  GL_SAMPLES_PASSED = $8914;
var
  glGenQueries: procedure(n: GLsizei; ids: PGLuint); extdecl;
  glDeleteQueries: procedure(n: GLsizei; const ids: PGLuint); extdecl;
  glIsQuery: function(id: GLuint): GLboolean; extdecl;
  glBeginQuery: procedure(target: GLenum; id: GLuint); extdecl;
  glEndQuery: procedure(target: GLenum); extdecl;
  glGetQueryiv: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetQueryObjectiv: procedure(id: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetQueryObjectuiv: procedure(id: GLuint; pname: GLenum; params: PGLuint); extdecl;
  glBindBuffer: procedure(target: GLenum; buffer: GLuint); extdecl;
  glDeleteBuffers: procedure(n: GLsizei; const buffers: PGLuint); extdecl;
  glGenBuffers: procedure(n: GLsizei; buffers: PGLuint); extdecl;
  glIsBuffer: function(buffer: GLuint): GLboolean; extdecl;
  glBufferData: procedure(target: GLenum; size: GLsizeiptr; const data: PGLvoid; usage: GLenum); extdecl;
  glBufferSubData: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; const data: PGLvoid); extdecl;
  glGetBufferSubData: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: PGLvoid); extdecl;
  glMapBuffer: function(target: GLenum; access: GLenum): PGLvoid; extdecl;
  glUnmapBuffer: function(target: GLenum): GLboolean; extdecl;
  glGetBufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetBufferPointerv: procedure(target: GLenum; pname: GLenum; params: PPGLvoid); extdecl;

function Load_GL_VERSION_1_5(): Boolean;

//**** GL_VERSION_2_0 *****//
const
  GL_BLEND_EQUATION_RGB = $8009;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  GL_VERTEX_PROGRAM_POINT_SIZE = $8642;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;
  GL_BLEND_EQUATION_ALPHA = $883D;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_MAX_VARYING_FLOATS = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_SHADER_TYPE = $8B4F;
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_1D = $8B5D;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_3D = $8B5F;
  GL_SAMPLER_CUBE = $8B60;
  GL_SAMPLER_1D_SHADOW = $8B61;
  GL_SAMPLER_2D_SHADOW = $8B62;
  GL_DELETE_STATUS = $8B80;
  GL_COMPILE_STATUS = $8B81;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  GL_CURRENT_PROGRAM = $8B8D;
  GL_POINT_SPRITE_COORD_ORIGIN = $8CA0;
  GL_LOWER_LEFT = $8CA1;
  GL_UPPER_LEFT = $8CA2;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;
// GL type for program/shader text 
var
  glBlendEquationSeparate: procedure(modeRGB: GLenum; modeAlpha: GLenum); extdecl;
  glDrawBuffers: procedure(n: GLsizei; const bufs: PGLenum); extdecl;
  glStencilOpSeparate: procedure(face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); extdecl;
  glStencilFuncSeparate: procedure(frontfunc: GLenum; backfunc: GLenum; ref: GLint; mask: GLuint); extdecl;
  glStencilMaskSeparate: procedure(face: GLenum; mask: GLuint); extdecl;
  glAttachShader: procedure(_program: GLuint; shader: GLuint); extdecl;
  glBindAttribLocation: procedure(_program: GLuint; index: GLuint; const name: PGLchar); extdecl;
  glCompileShader: procedure(shader: GLuint); extdecl;
  glCreateProgram: function(): GLuint; extdecl;
  glCreateShader: function(_type: GLenum): GLuint; extdecl;
  glDeleteProgram: procedure(_program: GLuint); extdecl;
  glDeleteShader: procedure(shader: GLuint); extdecl;
  glDetachShader: procedure(_program: GLuint; shader: GLuint); extdecl;
  glDisableVertexAttribArray: procedure(index: GLuint); extdecl;
  glEnableVertexAttribArray: procedure(index: GLuint); extdecl;
  glGetActiveAttrib: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLchar); extdecl;
  glGetActiveUniform: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLchar); extdecl;
  glGetAttachedShaders: procedure(_program: GLuint; maxCount: GLsizei; count: PGLsizei; obj: PGLuint); extdecl;
  glGetAttribLocation: function(_program: GLuint; const name: PGLchar): GLint; extdecl;
  glGetProgramiv: procedure(_program: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetProgramInfoLog: procedure(_program: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); extdecl;
  glGetShaderiv: procedure(shader: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetShaderInfoLog: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); extdecl;
  glGetShaderSource: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; source: PGLchar); extdecl;
  glGetUniformLocation: function(_program: GLuint; const name: PGLchar): GLint; extdecl;
  glGetUniformfv: procedure(_program: GLuint; location: GLint; params: PGLfloat); extdecl;
  glGetUniformiv: procedure(_program: GLuint; location: GLint; params: PGLint); extdecl;
  glGetVertexAttribdv: procedure(index: GLuint; pname: GLenum; params: PGLdouble); extdecl;
  glGetVertexAttribfv: procedure(index: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetVertexAttribiv: procedure(index: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVertexAttribPointerv: procedure(index: GLuint; pname: GLenum; _pointer: PPGLvoid); extdecl;
  glIsProgram: function(_program: GLuint): GLboolean; extdecl;
  glIsShader: function(shader: GLuint): GLboolean; extdecl;
  glLinkProgram: procedure(_program: GLuint); extdecl;
  glShaderSource: procedure(shader: GLuint; count: GLsizei; const _string: PPGLchar; const length: PGLint); extdecl;
  glUseProgram: procedure(_program: GLuint); extdecl;
  glUniform1f: procedure(location: GLint; v0: GLfloat); extdecl;
  glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); extdecl;
  glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); extdecl;
  glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); extdecl;
  glUniform1i: procedure(location: GLint; v0: GLint); extdecl;
  glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); extdecl;
  glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); extdecl;
  glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); extdecl;
  glUniform1fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glUniform2fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glUniform3fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glUniform4fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glUniform1iv: procedure(location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glUniform2iv: procedure(location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glUniform3iv: procedure(location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glUniform4iv: procedure(location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glValidateProgram: procedure(_program: GLuint); extdecl;
  glVertexAttrib1d: procedure(index: GLuint; x: GLdouble); extdecl;
  glVertexAttrib1dv: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib1f: procedure(index: GLuint; x: GLfloat); extdecl;
  glVertexAttrib1fv: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib1s: procedure(index: GLuint; x: GLshort); extdecl;
  glVertexAttrib1sv: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib2d: procedure(index: GLuint; x: GLdouble; y: GLdouble); extdecl;
  glVertexAttrib2dv: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib2f: procedure(index: GLuint; x: GLfloat; y: GLfloat); extdecl;
  glVertexAttrib2fv: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib2s: procedure(index: GLuint; x: GLshort; y: GLshort); extdecl;
  glVertexAttrib2sv: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib3d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glVertexAttrib3dv: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib3f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glVertexAttrib3fv: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib3s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); extdecl;
  glVertexAttrib3sv: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4Nbv: procedure(index: GLuint; const v: PGLbyte); extdecl;
  glVertexAttrib4Niv: procedure(index: GLuint; const v: PGLint); extdecl;
  glVertexAttrib4Nsv: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4Nub: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); extdecl;
  glVertexAttrib4Nubv: procedure(index: GLuint; const v: PGLubyte); extdecl;
  glVertexAttrib4Nuiv: procedure(index: GLuint; const v: PGLuint); extdecl;
  glVertexAttrib4Nusv: procedure(index: GLuint; const v: PGLushort); extdecl;
  glVertexAttrib4bv: procedure(index: GLuint; const v: PGLbyte); extdecl;
  glVertexAttrib4d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glVertexAttrib4dv: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib4f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glVertexAttrib4fv: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib4iv: procedure(index: GLuint; const v: PGLint); extdecl;
  glVertexAttrib4s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); extdecl;
  glVertexAttrib4sv: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4ubv: procedure(index: GLuint; const v: PGLubyte); extdecl;
  glVertexAttrib4uiv: procedure(index: GLuint; const v: PGLuint); extdecl;
  glVertexAttrib4usv: procedure(index: GLuint; const v: PGLushort); extdecl;
  glVertexAttribPointer: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const _pointer: PGLvoid); extdecl;

function Load_GL_VERSION_2_0(): Boolean;

//**** GL_VERSION_2_1 *****//
const
  GL_PIXEL_PACK_BUFFER = $88EB;
  GL_PIXEL_UNPACK_BUFFER = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;
  GL_FLOAT_MAT2x3 = $8B65;
  GL_FLOAT_MAT2x4 = $8B66;
  GL_FLOAT_MAT3x2 = $8B67;
  GL_FLOAT_MAT3x4 = $8B68;
  GL_FLOAT_MAT4x2 = $8B69;
  GL_FLOAT_MAT4x3 = $8B6A;
  GL_SRGB = $8C40;
  GL_SRGB8 = $8C41;
  GL_SRGB_ALPHA = $8C42;
  GL_SRGB8_ALPHA8 = $8C43;
  GL_COMPRESSED_SRGB = $8C48;
  GL_COMPRESSED_SRGB_ALPHA = $8C49;
var
  glUniformMatrix2x3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix3x2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix2x4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix4x2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix3x4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix4x3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;

function Load_GL_VERSION_2_1(): Boolean;

//**** GL_VERSION_3_0 *****//
const
  GL_COMPARE_REF_TO_TEXTURE = $884E;
  GL_CLIP_DISTANCE0 = $3000;
  GL_CLIP_DISTANCE1 = $3001;
  GL_CLIP_DISTANCE2 = $3002;
  GL_CLIP_DISTANCE3 = $3003;
  GL_CLIP_DISTANCE4 = $3004;
  GL_CLIP_DISTANCE5 = $3005;
  GL_CLIP_DISTANCE6 = $3006;
  GL_CLIP_DISTANCE7 = $3007;
  GL_MAX_CLIP_DISTANCES = $0D32;
  GL_MAJOR_VERSION = $821B;
  GL_MINOR_VERSION = $821C;
  GL_NUM_EXTENSIONS = $821D;
  GL_CONTEXT_FLAGS = $821E;
  GL_DEPTH_BUFFER = $8223;
  GL_STENCIL_BUFFER = $8224;
  GL_COMPRESSED_RED = $8225;
  GL_COMPRESSED_RG = $8226;
  GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT = $0001;
  GL_RGBA32F = $8814;
  GL_RGB32F = $8815;
  GL_RGBA16F = $881A;
  GL_RGB16F = $881B;
  GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
  GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
  GL_MIN_PROGRAM_TEXEL_OFFSET = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET = $8905;
  GL_CLAMP_READ_COLOR = $891C;
  GL_FIXED_ONLY = $891D;
  GL_MAX_VARYING_COMPONENTS = $8B4B;
  GL_TEXTURE_1D_ARRAY = $8C18;
  GL_PROXY_TEXTURE_1D_ARRAY = $8C19;
  GL_TEXTURE_2D_ARRAY = $8C1A;
  GL_PROXY_TEXTURE_2D_ARRAY = $8C1B;
  GL_TEXTURE_BINDING_1D_ARRAY = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
  GL_R11F_G11F_B10F = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
  GL_RGB9_E5 = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
  GL_TEXTURE_SHARED_SIZE = $8C3F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
  GL_PRIMITIVES_GENERATED = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
  GL_RASTERIZER_DISCARD = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
  GL_INTERLEAVED_ATTRIBS = $8C8C;
  GL_SEPARATE_ATTRIBS = $8C8D;
  GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
  GL_RGBA32UI = $8D70;
  GL_RGB32UI = $8D71;
  GL_RGBA16UI = $8D76;
  GL_RGB16UI = $8D77;
  GL_RGBA8UI = $8D7C;
  GL_RGB8UI = $8D7D;
  GL_RGBA32I = $8D82;
  GL_RGB32I = $8D83;
  GL_RGBA16I = $8D88;
  GL_RGB16I = $8D89;
  GL_RGBA8I = $8D8E;
  GL_RGB8I = $8D8F;
  GL_RED_INTEGER = $8D94;
  GL_GREEN_INTEGER = $8D95;
  GL_BLUE_INTEGER = $8D96;
  GL_RGB_INTEGER = $8D98;
  GL_RGBA_INTEGER = $8D99;
  GL_BGR_INTEGER = $8D9A;
  GL_BGRA_INTEGER = $8D9B;
  GL_SAMPLER_1D_ARRAY = $8DC0;
  GL_SAMPLER_2D_ARRAY = $8DC1;
  GL_SAMPLER_1D_ARRAY_SHADOW = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
  GL_SAMPLER_CUBE_SHADOW = $8DC5;
  GL_UNSIGNED_INT_VEC2 = $8DC6;
  GL_UNSIGNED_INT_VEC3 = $8DC7;
  GL_UNSIGNED_INT_VEC4 = $8DC8;
  GL_INT_SAMPLER_1D = $8DC9;
  GL_INT_SAMPLER_2D = $8DCA;
  GL_INT_SAMPLER_3D = $8DCB;
  GL_INT_SAMPLER_CUBE = $8DCC;
  GL_INT_SAMPLER_1D_ARRAY = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY = $8DCF;
  GL_UNSIGNED_INT_SAMPLER_1D = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
  GL_QUERY_WAIT = $8E13;
  GL_QUERY_NO_WAIT = $8E14;
  GL_QUERY_BY_REGION_WAIT = $8E15;
  GL_QUERY_BY_REGION_NO_WAIT = $8E16;
  GL_BUFFER_ACCESS_FLAGS = $911F;
  GL_BUFFER_MAP_LENGTH = $9120;
  GL_BUFFER_MAP_OFFSET = $9121;
// Reuse tokens from ARB_depth_buffer_float 
// reuse GL_DEPTH_COMPONENT32F 
// reuse GL_DEPTH32F_STENCIL8 
// reuse GL_FLOAT_32_UNSIGNED_INT_24_8_REV 
// Reuse tokens from ARB_framebuffer_object 
// reuse GL_INVALID_FRAMEBUFFER_OPERATION 
// reuse GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING 
// reuse GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE 
// reuse GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE 
// reuse GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE 
// reuse GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE 
// reuse GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE 
// reuse GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE 
// reuse GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE 
// reuse GL_FRAMEBUFFER_DEFAULT 
// reuse GL_FRAMEBUFFER_UNDEFINED 
// reuse GL_DEPTH_STENCIL_ATTACHMENT 
// reuse GL_INDEX 
// reuse GL_MAX_RENDERBUFFER_SIZE 
// reuse GL_DEPTH_STENCIL 
// reuse GL_UNSIGNED_INT_24_8 
// reuse GL_DEPTH24_STENCIL8 
// reuse GL_TEXTURE_STENCIL_SIZE 
// reuse GL_TEXTURE_RED_TYPE 
// reuse GL_TEXTURE_GREEN_TYPE 
// reuse GL_TEXTURE_BLUE_TYPE 
// reuse GL_TEXTURE_ALPHA_TYPE 
// reuse GL_TEXTURE_DEPTH_TYPE 
// reuse GL_UNSIGNED_NORMALIZED 
// reuse GL_FRAMEBUFFER_BINDING 
// reuse GL_DRAW_FRAMEBUFFER_BINDING 
// reuse GL_RENDERBUFFER_BINDING 
// reuse GL_READ_FRAMEBUFFER 
// reuse GL_DRAW_FRAMEBUFFER 
// reuse GL_READ_FRAMEBUFFER_BINDING 
// reuse GL_RENDERBUFFER_SAMPLES 
// reuse GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE 
// reuse GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME 
// reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL 
// reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE 
// reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER 
// reuse GL_FRAMEBUFFER_COMPLETE 
// reuse GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT 
// reuse GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT 
// reuse GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER 
// reuse GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER 
// reuse GL_FRAMEBUFFER_UNSUPPORTED 
// reuse GL_MAX_COLOR_ATTACHMENTS 
// reuse GL_COLOR_ATTACHMENT0 
// reuse GL_COLOR_ATTACHMENT1 
// reuse GL_COLOR_ATTACHMENT2 
// reuse GL_COLOR_ATTACHMENT3 
// reuse GL_COLOR_ATTACHMENT4 
// reuse GL_COLOR_ATTACHMENT5 
// reuse GL_COLOR_ATTACHMENT6 
// reuse GL_COLOR_ATTACHMENT7 
// reuse GL_COLOR_ATTACHMENT8 
// reuse GL_COLOR_ATTACHMENT9 
// reuse GL_COLOR_ATTACHMENT10 
// reuse GL_COLOR_ATTACHMENT11 
// reuse GL_COLOR_ATTACHMENT12 
// reuse GL_COLOR_ATTACHMENT13 
// reuse GL_COLOR_ATTACHMENT14 
// reuse GL_COLOR_ATTACHMENT15 
// reuse GL_DEPTH_ATTACHMENT 
// reuse GL_STENCIL_ATTACHMENT 
// reuse GL_FRAMEBUFFER 
// reuse GL_RENDERBUFFER 
// reuse GL_RENDERBUFFER_WIDTH 
// reuse GL_RENDERBUFFER_HEIGHT 
// reuse GL_RENDERBUFFER_INTERNAL_FORMAT 
// reuse GL_STENCIL_INDEX1 
// reuse GL_STENCIL_INDEX4 
// reuse GL_STENCIL_INDEX8 
// reuse GL_STENCIL_INDEX16 
// reuse GL_RENDERBUFFER_RED_SIZE 
// reuse GL_RENDERBUFFER_GREEN_SIZE 
// reuse GL_RENDERBUFFER_BLUE_SIZE 
// reuse GL_RENDERBUFFER_ALPHA_SIZE 
// reuse GL_RENDERBUFFER_DEPTH_SIZE 
// reuse GL_RENDERBUFFER_STENCIL_SIZE 
// reuse GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE 
// reuse GL_MAX_SAMPLES 
// Reuse tokens from ARB_framebuffer_sRGB 
// reuse GL_FRAMEBUFFER_SRGB 
// Reuse tokens from ARB_half_float_vertex 
// reuse GL_HALF_FLOAT 
// Reuse tokens from ARB_map_buffer_range 
// reuse GL_MAP_READ_BIT 
// reuse GL_MAP_WRITE_BIT 
// reuse GL_MAP_INVALIDATE_RANGE_BIT 
// reuse GL_MAP_INVALIDATE_BUFFER_BIT 
// reuse GL_MAP_FLUSH_EXPLICIT_BIT 
// reuse GL_MAP_UNSYNCHRONIZED_BIT 
// Reuse tokens from ARB_texture_compression_rgtc 
// reuse GL_COMPRESSED_RED_RGTC1 
// reuse GL_COMPRESSED_SIGNED_RED_RGTC1 
// reuse GL_COMPRESSED_RG_RGTC2 
// reuse GL_COMPRESSED_SIGNED_RG_RGTC2 
// Reuse tokens from ARB_texture_rg 
// reuse GL_RG 
// reuse GL_RG_INTEGER 
// reuse GL_R8 
// reuse GL_R16 
// reuse GL_RG8 
// reuse GL_RG16 
// reuse GL_R16F 
// reuse GL_R32F 
// reuse GL_RG16F 
// reuse GL_RG32F 
// reuse GL_R8I 
// reuse GL_R8UI 
// reuse GL_R16I 
// reuse GL_R16UI 
// reuse GL_R32I 
// reuse GL_R32UI 
// reuse GL_RG8I 
// reuse GL_RG8UI 
// reuse GL_RG16I 
// reuse GL_RG16UI 
// reuse GL_RG32I 
// reuse GL_RG32UI 
// Reuse tokens from ARB_vertex_array_object 
// reuse GL_VERTEX_ARRAY_BINDING 
// OpenGL 3.0 also reuses entry points from these extensions: 
// ARB_framebuffer_object 
// ARB_map_buffer_range 
// ARB_vertex_array_object 
var
  glColorMaski: procedure(index: GLuint; r: GLboolean; g: GLboolean; b: GLboolean; a: GLboolean); extdecl;
  glGetBooleani_v: procedure(target: GLenum; index: GLuint; data: PGLboolean); extdecl;
  glGetIntegeri_v: procedure(target: GLenum; index: GLuint; data: PGLint); extdecl; (* Also used in GL_ARB_uniform_buffer_object *)
  glEnablei: procedure(target: GLenum; index: GLuint); extdecl;
  glDisablei: procedure(target: GLenum; index: GLuint); extdecl;
  glIsEnabledi: function(target: GLenum; index: GLuint): GLboolean; extdecl;
  glBeginTransformFeedback: procedure(primitiveMode: GLenum); extdecl;
  glEndTransformFeedback: procedure(); extdecl;
  glBindBufferRange: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); extdecl; (* Also used in GL_ARB_uniform_buffer_object *)
  glBindBufferBase: procedure(target: GLenum; index: GLuint; buffer: GLuint); extdecl; (* Also used in GL_ARB_uniform_buffer_object *)
  glTransformFeedbackVaryings: procedure(_program: GLuint; count: GLsizei; const varyings: PPGLchar; bufferMode: GLenum); extdecl;
  glGetTransformFeedbackVarying: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLsizei; _type: PGLenum; name: PGLchar); extdecl;
  glClampColor: procedure(target: GLenum; clamp: GLenum); extdecl;
  glBeginConditionalRender: procedure(id: GLuint; mode: GLenum); extdecl;
  glEndConditionalRender: procedure(); extdecl;
  glVertexAttribIPointer: procedure(index: GLuint; size: GLint; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;
  glGetVertexAttribIiv: procedure(index: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVertexAttribIuiv: procedure(index: GLuint; pname: GLenum; params: PGLuint); extdecl;
  glVertexAttribI1i: procedure(index: GLuint; x: GLint); extdecl;
  glVertexAttribI2i: procedure(index: GLuint; x: GLint; y: GLint); extdecl;
  glVertexAttribI3i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint); extdecl;
  glVertexAttribI4i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); extdecl;
  glVertexAttribI1ui: procedure(index: GLuint; x: GLuint); extdecl;
  glVertexAttribI2ui: procedure(index: GLuint; x: GLuint; y: GLuint); extdecl;
  glVertexAttribI3ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint); extdecl;
  glVertexAttribI4ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); extdecl;
  glVertexAttribI1iv: procedure(index: GLuint; const v: PGLint); extdecl;
  glVertexAttribI2iv: procedure(index: GLuint; const v: PGLint); extdecl;
  glVertexAttribI3iv: procedure(index: GLuint; const v: PGLint); extdecl;
  glVertexAttribI4iv: procedure(index: GLuint; const v: PGLint); extdecl;
  glVertexAttribI1uiv: procedure(index: GLuint; const v: PGLuint); extdecl;
  glVertexAttribI2uiv: procedure(index: GLuint; const v: PGLuint); extdecl;
  glVertexAttribI3uiv: procedure(index: GLuint; const v: PGLuint); extdecl;
  glVertexAttribI4uiv: procedure(index: GLuint; const v: PGLuint); extdecl;
  glVertexAttribI4bv: procedure(index: GLuint; const v: PGLbyte); extdecl;
  glVertexAttribI4sv: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttribI4ubv: procedure(index: GLuint; const v: PGLubyte); extdecl;
  glVertexAttribI4usv: procedure(index: GLuint; const v: PGLushort); extdecl;
  glGetUniformuiv: procedure(_program: GLuint; location: GLint; params: PGLuint); extdecl;
  glBindFragDataLocation: procedure(_program: GLuint; color: GLuint; const name: PGLchar); extdecl;
  glGetFragDataLocation: function(_program: GLuint; const name: PGLchar): GLint; extdecl;
  glUniform1ui: procedure(location: GLint; v0: GLuint); extdecl;
  glUniform2ui: procedure(location: GLint; v0: GLuint; v1: GLuint); extdecl;
  glUniform3ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); extdecl;
  glUniform4ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); extdecl;
  glUniform1uiv: procedure(location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glUniform2uiv: procedure(location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glUniform3uiv: procedure(location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glUniform4uiv: procedure(location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glTexParameterIiv: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexParameterIuiv: procedure(target: GLenum; pname: GLenum; const params: PGLuint); extdecl;
  glGetTexParameterIiv: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetTexParameterIuiv: procedure(target: GLenum; pname: GLenum; params: PGLuint); extdecl;
  glClearBufferiv: procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLint); extdecl;
  glClearBufferuiv: procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLuint); extdecl;
  glClearBufferfv: procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLfloat); extdecl;
  glClearBufferfi: procedure(buffer: GLenum; drawbuffer: GLint; depth: GLfloat; stencil: GLint); extdecl;
  glGetStringi: function(name: GLenum; index: GLuint): PGLubyte; extdecl;

function Load_GL_VERSION_3_0(): Boolean;

//**** GL_VERSION_3_1 *****//
const
  GL_SAMPLER_2D_RECT = $8B63;
  GL_SAMPLER_2D_RECT_SHADOW = $8B64;
  GL_SAMPLER_BUFFER = $8DC2;
  GL_INT_SAMPLER_2D_RECT = $8DCD;
  GL_INT_SAMPLER_BUFFER = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_2D_RECT = $8DD5;
  GL_UNSIGNED_INT_SAMPLER_BUFFER = $8DD8;
  GL_TEXTURE_BUFFER = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE = $8C2B;
  GL_TEXTURE_BINDING_BUFFER = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT = $8C2E;
  GL_TEXTURE_RECTANGLE = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE = $84F8;
  GL_RED_SNORM = $8F90;
  GL_RG_SNORM = $8F91;
  GL_RGB_SNORM = $8F92;
  GL_RGBA_SNORM = $8F93;
  GL_R8_SNORM = $8F94;
  GL_RG8_SNORM = $8F95;
  GL_RGB8_SNORM = $8F96;
  GL_RGBA8_SNORM = $8F97;
  GL_R16_SNORM = $8F98;
  GL_RG16_SNORM = $8F99;
  GL_RGB16_SNORM = $8F9A;
  GL_RGBA16_SNORM = $8F9B;
  GL_SIGNED_NORMALIZED = $8F9C;
  GL_PRIMITIVE_RESTART = $8F9D;
  GL_PRIMITIVE_RESTART_INDEX = $8F9E;
// Reuse tokens from ARB_copy_buffer 
// reuse GL_COPY_READ_BUFFER 
// reuse GL_COPY_WRITE_BUFFER 
// Would reuse tokens from ARB_draw_instanced, but it has none 
// Reuse tokens from ARB_uniform_buffer_object 
// reuse GL_UNIFORM_BUFFER 
// reuse GL_UNIFORM_BUFFER_BINDING 
// reuse GL_UNIFORM_BUFFER_START 
// reuse GL_UNIFORM_BUFFER_SIZE 
// reuse GL_MAX_VERTEX_UNIFORM_BLOCKS 
// reuse GL_MAX_FRAGMENT_UNIFORM_BLOCKS 
// reuse GL_MAX_COMBINED_UNIFORM_BLOCKS 
// reuse GL_MAX_UNIFORM_BUFFER_BINDINGS 
// reuse GL_MAX_UNIFORM_BLOCK_SIZE 
// reuse GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS 
// reuse GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS 
// reuse GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT 
// reuse GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH 
// reuse GL_ACTIVE_UNIFORM_BLOCKS 
// reuse GL_UNIFORM_TYPE 
// reuse GL_UNIFORM_SIZE 
// reuse GL_UNIFORM_NAME_LENGTH 
// reuse GL_UNIFORM_BLOCK_INDEX 
// reuse GL_UNIFORM_OFFSET 
// reuse GL_UNIFORM_ARRAY_STRIDE 
// reuse GL_UNIFORM_MATRIX_STRIDE 
// reuse GL_UNIFORM_IS_ROW_MAJOR 
// reuse GL_UNIFORM_BLOCK_BINDING 
// reuse GL_UNIFORM_BLOCK_DATA_SIZE 
// reuse GL_UNIFORM_BLOCK_NAME_LENGTH 
// reuse GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS 
// reuse GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES 
// reuse GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER 
// reuse GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER 
// reuse GL_INVALID_INDEX 
// OpenGL 3.1 also reuses entry points from these extensions: 
// ARB_copy_buffer 
// ARB_uniform_buffer_object 
var
  glDrawArraysInstanced: procedure(mode: GLenum; first: GLint; count: GLsizei; primcount: GLsizei); extdecl;
  glDrawElementsInstanced: procedure(mode: GLenum; count: GLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei); extdecl;
  glTexBuffer: procedure(target: GLenum; internalformat: GLenum; buffer: GLuint); extdecl;
  glPrimitiveRestartIndex: procedure(index: GLuint); extdecl;

function Load_GL_VERSION_3_1(): Boolean;

//**** GL_VERSION_3_2 *****//
const
  GL_CONTEXT_CORE_PROFILE_BIT = $00000001;
  GL_CONTEXT_COMPATIBILITY_PROFILE_BIT = $00000002;
  GL_LINES_ADJACENCY = $000A;
  GL_LINE_STRIP_ADJACENCY = $000B;
  GL_TRIANGLES_ADJACENCY = $000C;
  GL_TRIANGLE_STRIP_ADJACENCY = $000D;
  GL_PROGRAM_POINT_SIZE = $8642;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS = $8C29;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED = $8DA7;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS = $8DA8;
  GL_GEOMETRY_SHADER = $8DD9;
  GL_GEOMETRY_VERTICES_OUT = $8916;
  GL_GEOMETRY_INPUT_TYPE = $8917;
  GL_GEOMETRY_OUTPUT_TYPE = $8918;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS = $8DE1;
  GL_MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
  GL_MAX_GEOMETRY_INPUT_COMPONENTS = $9123;
  GL_MAX_GEOMETRY_OUTPUT_COMPONENTS = $9124;
  GL_MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
  GL_CONTEXT_PROFILE_MASK = $9126;
// reuse GL_MAX_VARYING_COMPONENTS 
// reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER 
// Reuse tokens from ARB_depth_clamp 
// reuse GL_DEPTH_CLAMP 
// Would reuse tokens from ARB_draw_elements_base_vertex, but it has none 
// Would reuse tokens from ARB_fragment_coord_conventions, but it has none 
// Reuse tokens from ARB_provoking_vertex 
// reuse GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION 
// reuse GL_FIRST_VERTEX_CONVENTION 
// reuse GL_LAST_VERTEX_CONVENTION 
// reuse GL_PROVOKING_VERTEX 
// Reuse tokens from ARB_seamless_cube_map 
// reuse GL_TEXTURE_CUBE_MAP_SEAMLESS 
// Reuse tokens from ARB_sync 
// reuse GL_MAX_SERVER_WAIT_TIMEOUT 
// reuse GL_OBJECT_TYPE 
// reuse GL_SYNC_CONDITION 
// reuse GL_SYNC_STATUS 
// reuse GL_SYNC_FLAGS 
// reuse GL_SYNC_FENCE 
// reuse GL_SYNC_GPU_COMMANDS_COMPLETE 
// reuse GL_UNSIGNALED 
// reuse GL_SIGNALED 
// reuse GL_ALREADY_SIGNALED 
// reuse GL_TIMEOUT_EXPIRED 
// reuse GL_CONDITION_SATISFIED 
// reuse GL_WAIT_FAILED 
// reuse GL_TIMEOUT_IGNORED 
// reuse GL_SYNC_FLUSH_COMMANDS_BIT 
// reuse GL_TIMEOUT_IGNORED 
// Reuse tokens from ARB_texture_multisample 
// reuse GL_SAMPLE_POSITION 
// reuse GL_SAMPLE_MASK 
// reuse GL_SAMPLE_MASK_VALUE 
// reuse GL_MAX_SAMPLE_MASK_WORDS 
// reuse GL_TEXTURE_2D_MULTISAMPLE 
// reuse GL_PROXY_TEXTURE_2D_MULTISAMPLE 
// reuse GL_TEXTURE_2D_MULTISAMPLE_ARRAY 
// reuse GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY 
// reuse GL_TEXTURE_BINDING_2D_MULTISAMPLE 
// reuse GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY 
// reuse GL_TEXTURE_SAMPLES 
// reuse GL_TEXTURE_FIXED_SAMPLE_LOCATIONS 
// reuse GL_SAMPLER_2D_MULTISAMPLE 
// reuse GL_INT_SAMPLER_2D_MULTISAMPLE 
// reuse GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE 
// reuse GL_SAMPLER_2D_MULTISAMPLE_ARRAY 
// reuse GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY 
// reuse GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY 
// reuse GL_MAX_COLOR_TEXTURE_SAMPLES 
// reuse GL_MAX_DEPTH_TEXTURE_SAMPLES 
// reuse GL_MAX_INTEGER_SAMPLES 
// Don't need to reuse tokens from ARB_vertex_array_bgra since they're already in 1.2 core 
// OpenGL 3.2 also reuses entry points from these extensions: 
// ARB_draw_elements_base_vertex 
// ARB_provoking_vertex 
// ARB_sync 
// ARB_texture_multisample 
var
  glGetInteger64i_v: procedure(target: GLenum; index: GLuint; data: PGLint64); extdecl;
  glGetBufferParameteri64v: procedure(target: GLenum; pname: GLenum; params: PGLint64); extdecl;
  glProgramParameteri: procedure(_program: GLuint; pname: GLenum; value: GLint); extdecl;
  glFramebufferTexture: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint); extdecl;

function Load_GL_VERSION_3_2(): Boolean;

//**** GL_VERSION_3_3 *****//
// Reuse tokens from ARB_blend_func_extended 
// reuse GL_SRC1_COLOR 
// reuse GL_ONE_MINUS_SRC1_COLOR 
// reuse GL_ONE_MINUS_SRC1_ALPHA 
// reuse GL_MAX_DUAL_SOURCE_DRAW_BUFFERS 
// Would reuse tokens from ARB_explicit_attrib_location, but it has none 
// Reuse tokens from ARB_occlusion_query2 
// reuse GL_ANY_SAMPLES_PASSED 
// Reuse tokens from ARB_sampler_objects 
// reuse GL_SAMPLER_BINDING 
// Would reuse tokens from ARB_shader_bit_encoding, but it has none 
// Reuse tokens from ARB_texture_rgb10_a2ui 
// reuse GL_RGB10_A2UI 
// Reuse tokens from ARB_texture_swizzle 
// reuse GL_TEXTURE_SWIZZLE_R 
// reuse GL_TEXTURE_SWIZZLE_G 
// reuse GL_TEXTURE_SWIZZLE_B 
// reuse GL_TEXTURE_SWIZZLE_A 
// reuse GL_TEXTURE_SWIZZLE_RGBA 
// Reuse tokens from ARB_timer_query 
// reuse GL_TIME_ELAPSED 
// reuse GL_TIMESTAMP 
// Reuse tokens from ARB_vertex_type_2_10_10_10_rev 
// reuse GL_INT_2_10_10_10_REV 
// OpenGL 3.3 also reuses entry points from these extensions: 
// ARB_blend_func_extended 
// ARB_sampler_objects 
// ARB_explicit_attrib_location, but it has none 
// ARB_occlusion_query2 (no entry points) 
// ARB_shader_bit_encoding (no entry points) 
// ARB_texture_rgb10_a2ui (no entry points) 
// ARB_texture_swizzle (no entry points) 
// ARB_timer_query 
// ARB_vertex_type_2_10_10_10_rev 

function Load_GL_VERSION_3_3(): Boolean;

//**** GL_VERSION_4_0 *****//
// Reuse tokens from ARB_draw_indirect 
// reuse GL_DRAW_INDIRECT_BUFFER 
// reuse GL_DRAW_INDIRECT_BUFFER_BINDING 
// Reuse tokens from ARB_gpu_shader5 
// reuse GL_GEOMETRY_SHADER_INVOCATIONS 
// reuse GL_MAX_GEOMETRY_SHADER_INVOCATIONS 
// reuse GL_MIN_FRAGMENT_INTERPOLATION_OFFSET 
// reuse GL_MAX_FRAGMENT_INTERPOLATION_OFFSET 
// reuse GL_FRAGMENT_INTERPOLATION_OFFSET_BITS 
// reuse GL_MAX_VERTEX_STREAMS 
// Reuse tokens from ARB_gpu_shader_fp64 
// reuse GL_DOUBLE_VEC2 
// reuse GL_DOUBLE_VEC3 
// reuse GL_DOUBLE_VEC4 
// reuse GL_DOUBLE_MAT2 
// reuse GL_DOUBLE_MAT3 
// reuse GL_DOUBLE_MAT4 
// reuse GL_DOUBLE_MAT2x3 
// reuse GL_DOUBLE_MAT2x4 
// reuse GL_DOUBLE_MAT3x2 
// reuse GL_DOUBLE_MAT3x4 
// reuse GL_DOUBLE_MAT4x2 
// reuse GL_DOUBLE_MAT4x3 
// Reuse tokens from ARB_shader_subroutine 
// reuse GL_ACTIVE_SUBROUTINES 
// reuse GL_ACTIVE_SUBROUTINE_UNIFORMS 
// reuse GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS 
// reuse GL_ACTIVE_SUBROUTINE_MAX_LENGTH 
// reuse GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH 
// reuse GL_MAX_SUBROUTINES 
// reuse GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS 
// reuse GL_NUM_COMPATIBLE_SUBROUTINES 
// reuse GL_COMPATIBLE_SUBROUTINES 
// Reuse tokens from ARB_tessellation_shader 
// reuse GL_PATCHES 
// reuse GL_PATCH_VERTICES 
// reuse GL_PATCH_DEFAULT_INNER_LEVEL 
// reuse GL_PATCH_DEFAULT_OUTER_LEVEL 
// reuse GL_TESS_CONTROL_OUTPUT_VERTICES 
// reuse GL_TESS_GEN_MODE 
// reuse GL_TESS_GEN_SPACING 
// reuse GL_TESS_GEN_VERTEX_ORDER 
// reuse GL_TESS_GEN_POINT_MODE 
// reuse GL_ISOLINES 
// reuse GL_FRACTIONAL_ODD 
// reuse GL_FRACTIONAL_EVEN 
// reuse GL_MAX_PATCH_VERTICES 
// reuse GL_MAX_TESS_GEN_LEVEL 
// reuse GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS 
// reuse GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS 
// reuse GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS 
// reuse GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS 
// reuse GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS 
// reuse GL_MAX_TESS_PATCH_COMPONENTS 
// reuse GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS 
// reuse GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS 
// reuse GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS 
// reuse GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS 
// reuse GL_MAX_TESS_CONTROL_INPUT_COMPONENTS 
// reuse GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS 
// reuse GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS 
// reuse GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS 
// reuse GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER 
// reuse GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER 
// reuse GL_TESS_EVALUATION_SHADER 
// reuse GL_TESS_CONTROL_SHADER 
// Would reuse tokens from ARB_texture_buffer_object_rgb32, but it has none 
// Reuse tokens from ARB_transform_feedback2 
// reuse GL_TRANSFORM_FEEDBACK 
// reuse GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED 
// reuse GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE 
// reuse GL_TRANSFORM_FEEDBACK_BINDING 
// Reuse tokens from ARB_transform_feedback3 
// reuse GL_MAX_TRANSFORM_FEEDBACK_BUFFERS 
// reuse GL_MAX_VERTEX_STREAMS 
// OpenGL 4.0 also reuses entry points from these extensions: 
// ARB_gpu_shader5 (no entry points) 
// ARB_gpu_shader_fp64 
// ARB_shader_subroutine 
// ARB_tessellation_shader 
// ARB_texture_buffer_object_rgb32 (no entry points) 
// ARB_transform_feedback2 
// ARB_transform_feedback3 

function Load_GL_VERSION_4_0(): Boolean;

//**** GL_ARB_multitexture *****//
const
  GL_TEXTURE0_ARB = $84C0;
  GL_TEXTURE1_ARB = $84C1;
  GL_TEXTURE2_ARB = $84C2;
  GL_TEXTURE3_ARB = $84C3;
  GL_TEXTURE4_ARB = $84C4;
  GL_TEXTURE5_ARB = $84C5;
  GL_TEXTURE6_ARB = $84C6;
  GL_TEXTURE7_ARB = $84C7;
  GL_TEXTURE8_ARB = $84C8;
  GL_TEXTURE9_ARB = $84C9;
  GL_TEXTURE10_ARB = $84CA;
  GL_TEXTURE11_ARB = $84CB;
  GL_TEXTURE12_ARB = $84CC;
  GL_TEXTURE13_ARB = $84CD;
  GL_TEXTURE14_ARB = $84CE;
  GL_TEXTURE15_ARB = $84CF;
  GL_TEXTURE16_ARB = $84D0;
  GL_TEXTURE17_ARB = $84D1;
  GL_TEXTURE18_ARB = $84D2;
  GL_TEXTURE19_ARB = $84D3;
  GL_TEXTURE20_ARB = $84D4;
  GL_TEXTURE21_ARB = $84D5;
  GL_TEXTURE22_ARB = $84D6;
  GL_TEXTURE23_ARB = $84D7;
  GL_TEXTURE24_ARB = $84D8;
  GL_TEXTURE25_ARB = $84D9;
  GL_TEXTURE26_ARB = $84DA;
  GL_TEXTURE27_ARB = $84DB;
  GL_TEXTURE28_ARB = $84DC;
  GL_TEXTURE29_ARB = $84DD;
  GL_TEXTURE30_ARB = $84DE;
  GL_TEXTURE31_ARB = $84DF;
  GL_ACTIVE_TEXTURE_ARB = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE_ARB = $84E1;
  GL_MAX_TEXTURE_UNITS_ARB = $84E2;
var
  glActiveTextureARB: procedure(texture: GLenum); extdecl;
  glClientActiveTextureARB: procedure(texture: GLenum); extdecl;
  glMultiTexCoord1dARB: procedure(target: GLenum; s: GLdouble); extdecl;
  glMultiTexCoord1dvARB: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord1fARB: procedure(target: GLenum; s: GLfloat); extdecl;
  glMultiTexCoord1fvARB: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord1iARB: procedure(target: GLenum; s: GLint); extdecl;
  glMultiTexCoord1ivARB: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord1sARB: procedure(target: GLenum; s: GLshort); extdecl;
  glMultiTexCoord1svARB: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord2dARB: procedure(target: GLenum; s: GLdouble; t: GLdouble); extdecl;
  glMultiTexCoord2dvARB: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord2fARB: procedure(target: GLenum; s: GLfloat; t: GLfloat); extdecl;
  glMultiTexCoord2fvARB: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord2iARB: procedure(target: GLenum; s: GLint; t: GLint); extdecl;
  glMultiTexCoord2ivARB: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord2sARB: procedure(target: GLenum; s: GLshort; t: GLshort); extdecl;
  glMultiTexCoord2svARB: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord3dARB: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble); extdecl;
  glMultiTexCoord3dvARB: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord3fARB: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat); extdecl;
  glMultiTexCoord3fvARB: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord3iARB: procedure(target: GLenum; s: GLint; t: GLint; r: GLint); extdecl;
  glMultiTexCoord3ivARB: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord3sARB: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort); extdecl;
  glMultiTexCoord3svARB: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord4dARB: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble; q: GLdouble); extdecl;
  glMultiTexCoord4dvARB: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord4fARB: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat; q: GLfloat); extdecl;
  glMultiTexCoord4fvARB: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord4iARB: procedure(target: GLenum; s: GLint; t: GLint; r: GLint; q: GLint); extdecl;
  glMultiTexCoord4ivARB: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord4sARB: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort; q: GLshort); extdecl;
  glMultiTexCoord4svARB: procedure(target: GLenum; const v: PGLshort); extdecl;

function Load_GL_ARB_multitexture(): Boolean;

//**** GL_ARB_transpose_matrix *****//
const
  GL_TRANSPOSE_MODELVIEW_MATRIX_ARB = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX_ARB = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX_ARB = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX_ARB = $84E6;
var
  glLoadTransposeMatrixfARB: procedure(const m: PGLfloat); extdecl;
  glLoadTransposeMatrixdARB: procedure(const m: PGLdouble); extdecl;
  glMultTransposeMatrixfARB: procedure(const m: PGLfloat); extdecl;
  glMultTransposeMatrixdARB: procedure(const m: PGLdouble); extdecl;

function Load_GL_ARB_transpose_matrix(): Boolean;

//**** GL_ARB_multisample *****//
const
  GL_MULTISAMPLE_ARB = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB = $809F;
  GL_SAMPLE_COVERAGE_ARB = $80A0;
  GL_SAMPLE_BUFFERS_ARB = $80A8;
  GL_SAMPLES_ARB = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB = $80AB;
  GL_MULTISAMPLE_BIT_ARB = $20000000;
var
  glSampleCoverageARB: procedure(value: GLclampf; invert: GLboolean); extdecl;

function Load_GL_ARB_multisample(): Boolean;

//**** GL_ARB_texture_env_add *****//

function Load_GL_ARB_texture_env_add(): Boolean;

//**** GL_ARB_texture_cube_map *****//
const
  GL_NORMAL_MAP_ARB = $8511;
  GL_REFLECTION_MAP_ARB = $8512;
  GL_TEXTURE_CUBE_MAP_ARB = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_ARB = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARB = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = $851C;

function Load_GL_ARB_texture_cube_map(): Boolean;

//**** GL_ARB_texture_compression *****//
const
  GL_COMPRESSED_ALPHA_ARB = $84E9;
  GL_COMPRESSED_LUMINANCE_ARB = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB = $84EB;
  GL_COMPRESSED_INTENSITY_ARB = $84EC;
  GL_COMPRESSED_RGB_ARB = $84ED;
  GL_COMPRESSED_RGBA_ARB = $84EE;
  GL_TEXTURE_COMPRESSION_HINT_ARB = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB = $86A0;
  GL_TEXTURE_COMPRESSED_ARB = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS_ARB = $86A3;
var
  glCompressedTexImage3DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexImage2DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexImage1DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexSubImage3DARB: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexSubImage2DARB: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glCompressedTexSubImage1DARB: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); extdecl;
  glGetCompressedTexImageARB: procedure(target: GLenum; level: GLint; img: PGLvoid); extdecl;

function Load_GL_ARB_texture_compression(): Boolean;

//**** GL_ARB_texture_border_clamp *****//
const
  GL_CLAMP_TO_BORDER_ARB = $812D;

function Load_GL_ARB_texture_border_clamp(): Boolean;

//**** GL_ARB_point_parameters *****//
const
  GL_POINT_SIZE_MIN_ARB = $8126;
  GL_POINT_SIZE_MAX_ARB = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_ARB = $8128;
  GL_POINT_DISTANCE_ATTENUATION_ARB = $8129;
var
  glPointParameterfARB: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPointParameterfvARB: procedure(pname: GLenum; const params: PGLfloat); extdecl;

function Load_GL_ARB_point_parameters(): Boolean;

//**** GL_ARB_vertex_blend *****//
const
  GL_MAX_VERTEX_UNITS_ARB = $86A4;
  GL_ACTIVE_VERTEX_UNITS_ARB = $86A5;
  GL_WEIGHT_SUM_UNITY_ARB = $86A6;
  GL_VERTEX_BLEND_ARB = $86A7;
  GL_CURRENT_WEIGHT_ARB = $86A8;
  GL_WEIGHT_ARRAY_TYPE_ARB = $86A9;
  GL_WEIGHT_ARRAY_STRIDE_ARB = $86AA;
  GL_WEIGHT_ARRAY_SIZE_ARB = $86AB;
  GL_WEIGHT_ARRAY_POINTER_ARB = $86AC;
  GL_WEIGHT_ARRAY_ARB = $86AD;
  GL_MODELVIEW0_ARB = $1700;
  GL_MODELVIEW1_ARB = $850A;
  GL_MODELVIEW2_ARB = $8722;
  GL_MODELVIEW3_ARB = $8723;
  GL_MODELVIEW4_ARB = $8724;
  GL_MODELVIEW5_ARB = $8725;
  GL_MODELVIEW6_ARB = $8726;
  GL_MODELVIEW7_ARB = $8727;
  GL_MODELVIEW8_ARB = $8728;
  GL_MODELVIEW9_ARB = $8729;
  GL_MODELVIEW10_ARB = $872A;
  GL_MODELVIEW11_ARB = $872B;
  GL_MODELVIEW12_ARB = $872C;
  GL_MODELVIEW13_ARB = $872D;
  GL_MODELVIEW14_ARB = $872E;
  GL_MODELVIEW15_ARB = $872F;
  GL_MODELVIEW16_ARB = $8730;
  GL_MODELVIEW17_ARB = $8731;
  GL_MODELVIEW18_ARB = $8732;
  GL_MODELVIEW19_ARB = $8733;
  GL_MODELVIEW20_ARB = $8734;
  GL_MODELVIEW21_ARB = $8735;
  GL_MODELVIEW22_ARB = $8736;
  GL_MODELVIEW23_ARB = $8737;
  GL_MODELVIEW24_ARB = $8738;
  GL_MODELVIEW25_ARB = $8739;
  GL_MODELVIEW26_ARB = $873A;
  GL_MODELVIEW27_ARB = $873B;
  GL_MODELVIEW28_ARB = $873C;
  GL_MODELVIEW29_ARB = $873D;
  GL_MODELVIEW30_ARB = $873E;
  GL_MODELVIEW31_ARB = $873F;
var
  glWeightbvARB: procedure(size: GLint; const weights: PGLbyte); extdecl;
  glWeightsvARB: procedure(size: GLint; const weights: PGLshort); extdecl;
  glWeightivARB: procedure(size: GLint; const weights: PGLint); extdecl;
  glWeightfvARB: procedure(size: GLint; const weights: PGLfloat); extdecl;
  glWeightdvARB: procedure(size: GLint; const weights: PGLdouble); extdecl;
  glWeightubvARB: procedure(size: GLint; const weights: PGLubyte); extdecl;
  glWeightusvARB: procedure(size: GLint; const weights: PGLushort); extdecl;
  glWeightuivARB: procedure(size: GLint; const weights: PGLuint); extdecl;
  glWeightPointerARB: procedure(size: GLint; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;
  glVertexBlendARB: procedure(count: GLint); extdecl;

function Load_GL_ARB_vertex_blend(): Boolean;

//**** GL_ARB_matrix_palette *****//
const
  GL_MATRIX_PALETTE_ARB = $8840;
  GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB = $8841;
  GL_MAX_PALETTE_MATRICES_ARB = $8842;
  GL_CURRENT_PALETTE_MATRIX_ARB = $8843;
  GL_MATRIX_INDEX_ARRAY_ARB = $8844;
  GL_CURRENT_MATRIX_INDEX_ARB = $8845;
  GL_MATRIX_INDEX_ARRAY_SIZE_ARB = $8846;
  GL_MATRIX_INDEX_ARRAY_TYPE_ARB = $8847;
  GL_MATRIX_INDEX_ARRAY_STRIDE_ARB = $8848;
  GL_MATRIX_INDEX_ARRAY_POINTER_ARB = $8849;
var
  glCurrentPaletteMatrixARB: procedure(index: GLint); extdecl;
  glMatrixIndexubvARB: procedure(size: GLint; const indices: PGLubyte); extdecl;
  glMatrixIndexusvARB: procedure(size: GLint; const indices: PGLushort); extdecl;
  glMatrixIndexuivARB: procedure(size: GLint; const indices: PGLuint); extdecl;
  glMatrixIndexPointerARB: procedure(size: GLint; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;

function Load_GL_ARB_matrix_palette(): Boolean;

//**** GL_ARB_texture_env_combine *****//
const
  GL_COMBINE_ARB = $8570;
  GL_COMBINE_RGB_ARB = $8571;
  GL_COMBINE_ALPHA_ARB = $8572;
  GL_SOURCE0_RGB_ARB = $8580;
  GL_SOURCE1_RGB_ARB = $8581;
  GL_SOURCE2_RGB_ARB = $8582;
  GL_SOURCE0_ALPHA_ARB = $8588;
  GL_SOURCE1_ALPHA_ARB = $8589;
  GL_SOURCE2_ALPHA_ARB = $858A;
  GL_OPERAND0_RGB_ARB = $8590;
  GL_OPERAND1_RGB_ARB = $8591;
  GL_OPERAND2_RGB_ARB = $8592;
  GL_OPERAND0_ALPHA_ARB = $8598;
  GL_OPERAND1_ALPHA_ARB = $8599;
  GL_OPERAND2_ALPHA_ARB = $859A;
  GL_RGB_SCALE_ARB = $8573;
  GL_ADD_SIGNED_ARB = $8574;
  GL_INTERPOLATE_ARB = $8575;
  GL_SUBTRACT_ARB = $84E7;
  GL_CONSTANT_ARB = $8576;
  GL_PRIMARY_COLOR_ARB = $8577;
  GL_PREVIOUS_ARB = $8578;

function Load_GL_ARB_texture_env_combine(): Boolean;

//**** GL_ARB_texture_env_crossbar *****//

function Load_GL_ARB_texture_env_crossbar(): Boolean;

//**** GL_ARB_texture_env_dot3 *****//
const
  GL_DOT3_RGB_ARB = $86AE;
  GL_DOT3_RGBA_ARB = $86AF;

function Load_GL_ARB_texture_env_dot3(): Boolean;

//**** GL_ARB_texture_mirrored_repeat *****//
const
  GL_MIRRORED_REPEAT_ARB = $8370;

function Load_GL_ARB_texture_mirrored_repeat(): Boolean;

//**** GL_ARB_depth_texture *****//
const
  GL_DEPTH_COMPONENT16_ARB = $81A5;
  GL_DEPTH_COMPONENT24_ARB = $81A6;
  GL_DEPTH_COMPONENT32_ARB = $81A7;
  GL_TEXTURE_DEPTH_SIZE_ARB = $884A;
  GL_DEPTH_TEXTURE_MODE_ARB = $884B;

function Load_GL_ARB_depth_texture(): Boolean;

//**** GL_ARB_shadow *****//
const
  GL_TEXTURE_COMPARE_MODE_ARB = $884C;
  GL_TEXTURE_COMPARE_FUNC_ARB = $884D;
  GL_COMPARE_R_TO_TEXTURE_ARB = $884E;

function Load_GL_ARB_shadow(): Boolean;

//**** GL_ARB_shadow_ambient *****//
const
  GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = $80BF;

function Load_GL_ARB_shadow_ambient(): Boolean;

//**** GL_ARB_window_pos *****//
var
  glWindowPos2dARB: procedure(x: GLdouble; y: GLdouble); extdecl;
  glWindowPos2dvARB: procedure(const v: PGLdouble); extdecl;
  glWindowPos2fARB: procedure(x: GLfloat; y: GLfloat); extdecl;
  glWindowPos2fvARB: procedure(const v: PGLfloat); extdecl;
  glWindowPos2iARB: procedure(x: GLint; y: GLint); extdecl;
  glWindowPos2ivARB: procedure(const v: PGLint); extdecl;
  glWindowPos2sARB: procedure(x: GLshort; y: GLshort); extdecl;
  glWindowPos2svARB: procedure(const v: PGLshort); extdecl;
  glWindowPos3dARB: procedure(x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glWindowPos3dvARB: procedure(const v: PGLdouble); extdecl;
  glWindowPos3fARB: procedure(x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glWindowPos3fvARB: procedure(const v: PGLfloat); extdecl;
  glWindowPos3iARB: procedure(x: GLint; y: GLint; z: GLint); extdecl;
  glWindowPos3ivARB: procedure(const v: PGLint); extdecl;
  glWindowPos3sARB: procedure(x: GLshort; y: GLshort; z: GLshort); extdecl;
  glWindowPos3svARB: procedure(const v: PGLshort); extdecl;

function Load_GL_ARB_window_pos(): Boolean;

//**** GL_ARB_vertex_program *****//
const
  GL_COLOR_SUM_ARB = $8458;
  GL_VERTEX_PROGRAM_ARB = $8620;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB = $8625;
  GL_CURRENT_VERTEX_ATTRIB_ARB = $8626;
  GL_PROGRAM_LENGTH_ARB = $8627;
  GL_PROGRAM_STRING_ARB = $8628;
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB = $862E;
  GL_MAX_PROGRAM_MATRICES_ARB = $862F;
  GL_CURRENT_MATRIX_STACK_DEPTH_ARB = $8640;
  GL_CURRENT_MATRIX_ARB = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_ARB = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_ARB = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB = $8645;
  GL_PROGRAM_ERROR_POSITION_ARB = $864B;
  GL_PROGRAM_BINDING_ARB = $8677;
  GL_MAX_VERTEX_ATTRIBS_ARB = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB = $886A;
  GL_PROGRAM_ERROR_STRING_ARB = $8874;
  GL_PROGRAM_FORMAT_ASCII_ARB = $8875;
  GL_PROGRAM_FORMAT_ARB = $8876;
  GL_PROGRAM_INSTRUCTIONS_ARB = $88A0;
  GL_MAX_PROGRAM_INSTRUCTIONS_ARB = $88A1;
  GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A2;
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A3;
  GL_PROGRAM_TEMPORARIES_ARB = $88A4;
  GL_MAX_PROGRAM_TEMPORARIES_ARB = $88A5;
  GL_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A6;
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A7;
  GL_PROGRAM_PARAMETERS_ARB = $88A8;
  GL_MAX_PROGRAM_PARAMETERS_ARB = $88A9;
  GL_PROGRAM_NATIVE_PARAMETERS_ARB = $88AA;
  GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB = $88AB;
  GL_PROGRAM_ATTRIBS_ARB = $88AC;
  GL_MAX_PROGRAM_ATTRIBS_ARB = $88AD;
  GL_PROGRAM_NATIVE_ATTRIBS_ARB = $88AE;
  GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB = $88AF;
  GL_PROGRAM_ADDRESS_REGISTERS_ARB = $88B0;
  GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB = $88B1;
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B2;
  GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B3;
  GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB = $88B4;
  GL_MAX_PROGRAM_ENV_PARAMETERS_ARB = $88B5;
  GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB = $88B6;
  GL_TRANSPOSE_CURRENT_MATRIX_ARB = $88B7;
  GL_MATRIX0_ARB = $88C0;
  GL_MATRIX1_ARB = $88C1;
  GL_MATRIX2_ARB = $88C2;
  GL_MATRIX3_ARB = $88C3;
  GL_MATRIX4_ARB = $88C4;
  GL_MATRIX5_ARB = $88C5;
  GL_MATRIX6_ARB = $88C6;
  GL_MATRIX7_ARB = $88C7;
  GL_MATRIX8_ARB = $88C8;
  GL_MATRIX9_ARB = $88C9;
  GL_MATRIX10_ARB = $88CA;
  GL_MATRIX11_ARB = $88CB;
  GL_MATRIX12_ARB = $88CC;
  GL_MATRIX13_ARB = $88CD;
  GL_MATRIX14_ARB = $88CE;
  GL_MATRIX15_ARB = $88CF;
  GL_MATRIX16_ARB = $88D0;
  GL_MATRIX17_ARB = $88D1;
  GL_MATRIX18_ARB = $88D2;
  GL_MATRIX19_ARB = $88D3;
  GL_MATRIX20_ARB = $88D4;
  GL_MATRIX21_ARB = $88D5;
  GL_MATRIX22_ARB = $88D6;
  GL_MATRIX23_ARB = $88D7;
  GL_MATRIX24_ARB = $88D8;
  GL_MATRIX25_ARB = $88D9;
  GL_MATRIX26_ARB = $88DA;
  GL_MATRIX27_ARB = $88DB;
  GL_MATRIX28_ARB = $88DC;
  GL_MATRIX29_ARB = $88DD;
  GL_MATRIX30_ARB = $88DE;
  GL_MATRIX31_ARB = $88DF;
var
  glVertexAttrib1dARB: procedure(index: GLuint; x: GLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib1dvARB: procedure(index: GLuint; const v: PGLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib1fARB: procedure(index: GLuint; x: GLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib1fvARB: procedure(index: GLuint; const v: PGLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib1sARB: procedure(index: GLuint; x: GLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib1svARB: procedure(index: GLuint; const v: PGLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib2dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib2dvARB: procedure(index: GLuint; const v: PGLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib2fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib2fvARB: procedure(index: GLuint; const v: PGLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib2sARB: procedure(index: GLuint; x: GLshort; y: GLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib2svARB: procedure(index: GLuint; const v: PGLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib3dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib3dvARB: procedure(index: GLuint; const v: PGLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib3fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib3fvARB: procedure(index: GLuint; const v: PGLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib3sARB: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib3svARB: procedure(index: GLuint; const v: PGLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4NbvARB: procedure(index: GLuint; const v: PGLbyte); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4NivARB: procedure(index: GLuint; const v: PGLint); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4NsvARB: procedure(index: GLuint; const v: PGLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4NubARB: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4NubvARB: procedure(index: GLuint; const v: PGLubyte); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4NuivARB: procedure(index: GLuint; const v: PGLuint); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4NusvARB: procedure(index: GLuint; const v: PGLushort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4bvARB: procedure(index: GLuint; const v: PGLbyte); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4dvARB: procedure(index: GLuint; const v: PGLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4fvARB: procedure(index: GLuint; const v: PGLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4ivARB: procedure(index: GLuint; const v: PGLint); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4sARB: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4svARB: procedure(index: GLuint; const v: PGLshort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4ubvARB: procedure(index: GLuint; const v: PGLubyte); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4uivARB: procedure(index: GLuint; const v: PGLuint); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttrib4usvARB: procedure(index: GLuint; const v: PGLushort); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glVertexAttribPointerARB: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const _pointer: PGLvoid); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glEnableVertexAttribArrayARB: procedure(index: GLuint); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glDisableVertexAttribArrayARB: procedure(index: GLuint); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glProgramStringARB: procedure(target: GLenum; format: GLenum; len: GLsizei; const _string: PGLvoid); extdecl; (* Also used in GL_ARB_fragment_program *)
  glBindProgramARB: procedure(target: GLenum; _program: GLuint); extdecl; (* Also used in GL_ARB_fragment_program *)
  glDeleteProgramsARB: procedure(n: GLsizei; const programs: PGLuint); extdecl; (* Also used in GL_ARB_fragment_program *)
  glGenProgramsARB: procedure(n: GLsizei; programs: PGLuint); extdecl; (* Also used in GL_ARB_fragment_program *)
  glProgramEnvParameter4dARB: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl; (* Also used in GL_ARB_fragment_program *)
  glProgramEnvParameter4dvARB: procedure(target: GLenum; index: GLuint; const params: PGLdouble); extdecl; (* Also used in GL_ARB_fragment_program *)
  glProgramEnvParameter4fARB: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl; (* Also used in GL_ARB_fragment_program *)
  glProgramEnvParameter4fvARB: procedure(target: GLenum; index: GLuint; const params: PGLfloat); extdecl; (* Also used in GL_ARB_fragment_program *)
  glProgramLocalParameter4dARB: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl; (* Also used in GL_ARB_fragment_program GL_NV_fragment_program *)
  glProgramLocalParameter4dvARB: procedure(target: GLenum; index: GLuint; const params: PGLdouble); extdecl; (* Also used in GL_ARB_fragment_program GL_NV_fragment_program *)
  glProgramLocalParameter4fARB: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl; (* Also used in GL_ARB_fragment_program GL_NV_fragment_program *)
  glProgramLocalParameter4fvARB: procedure(target: GLenum; index: GLuint; const params: PGLfloat); extdecl; (* Also used in GL_ARB_fragment_program GL_NV_fragment_program *)
  glGetProgramEnvParameterdvARB: procedure(target: GLenum; index: GLuint; params: PGLdouble); extdecl; (* Also used in GL_ARB_fragment_program *)
  glGetProgramEnvParameterfvARB: procedure(target: GLenum; index: GLuint; params: PGLfloat); extdecl; (* Also used in GL_ARB_fragment_program *)
  glGetProgramLocalParameterdvARB: procedure(target: GLenum; index: GLuint; params: PGLdouble); extdecl; (* Also used in GL_ARB_fragment_program GL_NV_fragment_program *)
  glGetProgramLocalParameterfvARB: procedure(target: GLenum; index: GLuint; params: PGLfloat); extdecl; (* Also used in GL_ARB_fragment_program GL_NV_fragment_program *)
  glGetProgramivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl; (* Also used in GL_ARB_fragment_program *)
  glGetProgramStringARB: procedure(target: GLenum; pname: GLenum; _string: PGLvoid); extdecl; (* Also used in GL_ARB_fragment_program *)
  glGetVertexAttribdvARB: procedure(index: GLuint; pname: GLenum; params: PGLdouble); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glGetVertexAttribfvARB: procedure(index: GLuint; pname: GLenum; params: PGLfloat); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glGetVertexAttribivARB: procedure(index: GLuint; pname: GLenum; params: PGLint); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glGetVertexAttribPointervARB: procedure(index: GLuint; pname: GLenum; _pointer: PPGLvoid); extdecl; (* Also used in GL_ARB_vertex_shader *)
  glIsProgramARB: function(_program: GLuint): GLboolean; extdecl; (* Also used in GL_ARB_fragment_program *)

function Load_GL_ARB_vertex_program(): Boolean;

//**** GL_ARB_fragment_program *****//
const
  GL_FRAGMENT_PROGRAM_ARB = $8804;
  GL_PROGRAM_ALU_INSTRUCTIONS_ARB = $8805;
  GL_PROGRAM_TEX_INSTRUCTIONS_ARB = $8806;
  GL_PROGRAM_TEX_INDIRECTIONS_ARB = $8807;
  GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $8808;
  GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $8809;
  GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $880A;
  GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB = $880B;
  GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB = $880C;
  GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB = $880D;
  GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $880E;
  GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $880F;
  GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $8810;
  GL_MAX_TEXTURE_COORDS_ARB = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_ARB = $8872;
// All ARB_fragment_program entry points are shared with ARB_vertex_program. 

function Load_GL_ARB_fragment_program(): Boolean;

//**** GL_ARB_vertex_buffer_object *****//
const
  GL_BUFFER_SIZE_ARB = $8764;
  GL_BUFFER_USAGE_ARB = $8765;
  GL_ARRAY_BUFFER_ARB = $8892;
  GL_ELEMENT_ARRAY_BUFFER_ARB = $8893;
  GL_ARRAY_BUFFER_BINDING_ARB = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING_ARB = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING_ARB = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING_ARB = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING_ARB = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB = $889F;
  GL_READ_ONLY_ARB = $88B8;
  GL_WRITE_ONLY_ARB = $88B9;
  GL_READ_WRITE_ARB = $88BA;
  GL_BUFFER_ACCESS_ARB = $88BB;
  GL_BUFFER_MAPPED_ARB = $88BC;
  GL_BUFFER_MAP_POINTER_ARB = $88BD;
  GL_STREAM_DRAW_ARB = $88E0;
  GL_STREAM_READ_ARB = $88E1;
  GL_STREAM_COPY_ARB = $88E2;
  GL_STATIC_DRAW_ARB = $88E4;
  GL_STATIC_READ_ARB = $88E5;
  GL_STATIC_COPY_ARB = $88E6;
  GL_DYNAMIC_DRAW_ARB = $88E8;
  GL_DYNAMIC_READ_ARB = $88E9;
  GL_DYNAMIC_COPY_ARB = $88EA;
// GL types for handling large vertex buffer objects 
var
  glBindBufferARB: procedure(target: GLenum; buffer: GLuint); extdecl;
  glDeleteBuffersARB: procedure(n: GLsizei; const buffers: PGLuint); extdecl;
  glGenBuffersARB: procedure(n: GLsizei; buffers: PGLuint); extdecl;
  glIsBufferARB: function(buffer: GLuint): GLboolean; extdecl;
  glBufferDataARB: procedure(target: GLenum; size: GLsizeiptrARB; const data: PGLvoid; usage: GLenum); extdecl;
  glBufferSubDataARB: procedure(target: GLenum; offset: GLintptrARB; size: GLsizeiptrARB; const data: PGLvoid); extdecl;
  glGetBufferSubDataARB: procedure(target: GLenum; offset: GLintptrARB; size: GLsizeiptrARB; data: PGLvoid); extdecl;
  glMapBufferARB: function(target: GLenum; access: GLenum): PGLvoid; extdecl;
  glUnmapBufferARB: function(target: GLenum): GLboolean; extdecl;
  glGetBufferParameterivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetBufferPointervARB: procedure(target: GLenum; pname: GLenum; params: PPGLvoid); extdecl;

function Load_GL_ARB_vertex_buffer_object(): Boolean;

//**** GL_ARB_occlusion_query *****//
const
  GL_QUERY_COUNTER_BITS_ARB = $8864;
  GL_CURRENT_QUERY_ARB = $8865;
  GL_QUERY_RESULT_ARB = $8866;
  GL_QUERY_RESULT_AVAILABLE_ARB = $8867;
  GL_SAMPLES_PASSED_ARB = $8914;
var
  glGenQueriesARB: procedure(n: GLsizei; ids: PGLuint); extdecl;
  glDeleteQueriesARB: procedure(n: GLsizei; const ids: PGLuint); extdecl;
  glIsQueryARB: function(id: GLuint): GLboolean; extdecl;
  glBeginQueryARB: procedure(target: GLenum; id: GLuint); extdecl;
  glEndQueryARB: procedure(target: GLenum); extdecl;
  glGetQueryivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetQueryObjectivARB: procedure(id: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetQueryObjectuivARB: procedure(id: GLuint; pname: GLenum; params: PGLuint); extdecl;

function Load_GL_ARB_occlusion_query(): Boolean;

//**** GL_ARB_shader_objects *****//
const
  GL_PROGRAM_OBJECT_ARB = $8B40;
  GL_SHADER_OBJECT_ARB = $8B48;
  GL_OBJECT_TYPE_ARB = $8B4E;
  GL_OBJECT_SUBTYPE_ARB = $8B4F;
  GL_FLOAT_VEC2_ARB = $8B50;
  GL_FLOAT_VEC3_ARB = $8B51;
  GL_FLOAT_VEC4_ARB = $8B52;
  GL_INT_VEC2_ARB = $8B53;
  GL_INT_VEC3_ARB = $8B54;
  GL_INT_VEC4_ARB = $8B55;
  GL_BOOL_ARB = $8B56;
  GL_BOOL_VEC2_ARB = $8B57;
  GL_BOOL_VEC3_ARB = $8B58;
  GL_BOOL_VEC4_ARB = $8B59;
  GL_FLOAT_MAT2_ARB = $8B5A;
  GL_FLOAT_MAT3_ARB = $8B5B;
  GL_FLOAT_MAT4_ARB = $8B5C;
  GL_SAMPLER_1D_ARB = $8B5D;
  GL_SAMPLER_2D_ARB = $8B5E;
  GL_SAMPLER_3D_ARB = $8B5F;
  GL_SAMPLER_CUBE_ARB = $8B60;
  GL_SAMPLER_1D_SHADOW_ARB = $8B61;
  GL_SAMPLER_2D_SHADOW_ARB = $8B62;
  GL_SAMPLER_2D_RECT_ARB = $8B63;
  GL_SAMPLER_2D_RECT_SHADOW_ARB = $8B64;
  GL_OBJECT_DELETE_STATUS_ARB = $8B80;
  GL_OBJECT_COMPILE_STATUS_ARB = $8B81;
  GL_OBJECT_LINK_STATUS_ARB = $8B82;
  GL_OBJECT_VALIDATE_STATUS_ARB = $8B83;
  GL_OBJECT_INFO_LOG_LENGTH_ARB = $8B84;
  GL_OBJECT_ATTACHED_OBJECTS_ARB = $8B85;
  GL_OBJECT_ACTIVE_UNIFORMS_ARB = $8B86;
  GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB = $8B87;
  GL_OBJECT_SHADER_SOURCE_LENGTH_ARB = $8B88;
// GL types for program/shader text and shader object handles 
// GL type for "half" precision (s10e5) float data in host memory 
var
  glDeleteObjectARB: procedure(obj: GLhandleARB); extdecl;
  glGetHandleARB: function(pname: GLenum): GLhandleARB; extdecl;
  glDetachObjectARB: procedure(containerObj: GLhandleARB; attachedObj: GLhandleARB); extdecl;
  glCreateShaderObjectARB: function(shaderType: GLenum): GLhandleARB; extdecl;
  glShaderSourceARB: procedure(shaderObj: GLhandleARB; count: GLsizei; const _string: PPGLcharARB; const length: PGLint); extdecl;
  glCompileShaderARB: procedure(shaderObj: GLhandleARB); extdecl;
  glCreateProgramObjectARB: function(): GLhandleARB; extdecl;
  glAttachObjectARB: procedure(containerObj: GLhandleARB; obj: GLhandleARB); extdecl;
  glLinkProgramARB: procedure(programObj: GLhandleARB); extdecl;
  glUseProgramObjectARB: procedure(programObj: GLhandleARB); extdecl;
  glValidateProgramARB: procedure(programObj: GLhandleARB); extdecl;
  glUniform1fARB: procedure(location: GLint; v0: GLfloat); extdecl;
  glUniform2fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat); extdecl;
  glUniform3fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); extdecl;
  glUniform4fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); extdecl;
  glUniform1iARB: procedure(location: GLint; v0: GLint); extdecl;
  glUniform2iARB: procedure(location: GLint; v0: GLint; v1: GLint); extdecl;
  glUniform3iARB: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); extdecl;
  glUniform4iARB: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); extdecl;
  glUniform1fvARB: procedure(location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glUniform2fvARB: procedure(location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glUniform3fvARB: procedure(location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glUniform4fvARB: procedure(location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glUniform1ivARB: procedure(location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glUniform2ivARB: procedure(location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glUniform3ivARB: procedure(location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glUniform4ivARB: procedure(location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glUniformMatrix2fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix3fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glUniformMatrix4fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glGetObjectParameterfvARB: procedure(obj: GLhandleARB; pname: GLenum; params: PGLfloat); extdecl;
  glGetObjectParameterivARB: procedure(obj: GLhandleARB; pname: GLenum; params: PGLint); extdecl;
  glGetInfoLogARB: procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; infoLog: PGLcharARB); extdecl;
  glGetAttachedObjectsARB: procedure(containerObj: GLhandleARB; maxCount: GLsizei; count: PGLsizei; obj: PGLhandleARB); extdecl;
  glGetUniformLocationARB: function(programObj: GLhandleARB; const name: PGLcharARB): GLint; extdecl;
  glGetActiveUniformARB: procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLcharARB); extdecl;
  glGetUniformfvARB: procedure(programObj: GLhandleARB; location: GLint; params: PGLfloat); extdecl;
  glGetUniformivARB: procedure(programObj: GLhandleARB; location: GLint; params: PGLint); extdecl;
  glGetShaderSourceARB: procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; source: PGLcharARB); extdecl;

function Load_GL_ARB_shader_objects(): Boolean;

//**** GL_ARB_vertex_shader *****//
const
  GL_VERTEX_SHADER_ARB = $8B31;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB = $8B4A;
  GL_MAX_VARYING_FLOATS_ARB = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB = $8B4D;
  GL_OBJECT_ACTIVE_ATTRIBUTES_ARB = $8B89;
  GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB = $8B8A;
var
  glBindAttribLocationARB: procedure(programObj: GLhandleARB; index: GLuint; const name: PGLcharARB); extdecl;
  glGetActiveAttribARB: procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLcharARB); extdecl;
  glGetAttribLocationARB: function(programObj: GLhandleARB; const name: PGLcharARB): GLint; extdecl;

function Load_GL_ARB_vertex_shader(): Boolean;

//**** GL_ARB_fragment_shader *****//
const
  GL_FRAGMENT_SHADER_ARB = $8B30;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB = $8B49;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB = $8B8B;

function Load_GL_ARB_fragment_shader(): Boolean;

//**** GL_ARB_shading_language_100 *****//
const
  GL_SHADING_LANGUAGE_VERSION_ARB = $8B8C;

function Load_GL_ARB_shading_language_100(): Boolean;

//**** GL_ARB_texture_non_power_of_two *****//

function Load_GL_ARB_texture_non_power_of_two(): Boolean;

//**** GL_ARB_point_sprite *****//
const
  GL_POINT_SPRITE_ARB = $8861;
  GL_COORD_REPLACE_ARB = $8862;

function Load_GL_ARB_point_sprite(): Boolean;

//**** GL_ARB_fragment_program_shadow *****//

function Load_GL_ARB_fragment_program_shadow(): Boolean;

//**** GL_ARB_draw_buffers *****//
const
  GL_MAX_DRAW_BUFFERS_ARB = $8824;
  GL_DRAW_BUFFER0_ARB = $8825;
  GL_DRAW_BUFFER1_ARB = $8826;
  GL_DRAW_BUFFER2_ARB = $8827;
  GL_DRAW_BUFFER3_ARB = $8828;
  GL_DRAW_BUFFER4_ARB = $8829;
  GL_DRAW_BUFFER5_ARB = $882A;
  GL_DRAW_BUFFER6_ARB = $882B;
  GL_DRAW_BUFFER7_ARB = $882C;
  GL_DRAW_BUFFER8_ARB = $882D;
  GL_DRAW_BUFFER9_ARB = $882E;
  GL_DRAW_BUFFER10_ARB = $882F;
  GL_DRAW_BUFFER11_ARB = $8830;
  GL_DRAW_BUFFER12_ARB = $8831;
  GL_DRAW_BUFFER13_ARB = $8832;
  GL_DRAW_BUFFER14_ARB = $8833;
  GL_DRAW_BUFFER15_ARB = $8834;
var
  glDrawBuffersARB: procedure(n: GLsizei; const bufs: PGLenum); extdecl;

function Load_GL_ARB_draw_buffers(): Boolean;

//**** GL_ARB_texture_rectangle *****//
const
  GL_TEXTURE_RECTANGLE_ARB = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_ARB = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_ARB = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB = $84F8;

function Load_GL_ARB_texture_rectangle(): Boolean;

//**** GL_ARB_color_buffer_float *****//
const
  GL_RGBA_FLOAT_MODE_ARB = $8820;
  GL_CLAMP_VERTEX_COLOR_ARB = $891A;
  GL_CLAMP_FRAGMENT_COLOR_ARB = $891B;
  GL_CLAMP_READ_COLOR_ARB = $891C;
  GL_FIXED_ONLY_ARB = $891D;
var
  glClampColorARB: procedure(target: GLenum; clamp: GLenum); extdecl;

function Load_GL_ARB_color_buffer_float(): Boolean;

//**** GL_ARB_half_float_pixel *****//
const
  GL_HALF_FLOAT_ARB = $140B;

function Load_GL_ARB_half_float_pixel(): Boolean;

//**** GL_ARB_texture_float *****//
const
  GL_TEXTURE_RED_TYPE_ARB = $8C10;
  GL_TEXTURE_GREEN_TYPE_ARB = $8C11;
  GL_TEXTURE_BLUE_TYPE_ARB = $8C12;
  GL_TEXTURE_ALPHA_TYPE_ARB = $8C13;
  GL_TEXTURE_LUMINANCE_TYPE_ARB = $8C14;
  GL_TEXTURE_INTENSITY_TYPE_ARB = $8C15;
  GL_TEXTURE_DEPTH_TYPE_ARB = $8C16;
  GL_UNSIGNED_NORMALIZED_ARB = $8C17;
  GL_RGBA32F_ARB = $8814;
  GL_RGB32F_ARB = $8815;
  GL_ALPHA32F_ARB = $8816;
  GL_INTENSITY32F_ARB = $8817;
  GL_LUMINANCE32F_ARB = $8818;
  GL_LUMINANCE_ALPHA32F_ARB = $8819;
  GL_RGBA16F_ARB = $881A;
  GL_RGB16F_ARB = $881B;
  GL_ALPHA16F_ARB = $881C;
  GL_INTENSITY16F_ARB = $881D;
  GL_LUMINANCE16F_ARB = $881E;
  GL_LUMINANCE_ALPHA16F_ARB = $881F;

function Load_GL_ARB_texture_float(): Boolean;

//**** GL_ARB_pixel_buffer_object *****//
const
  GL_PIXEL_PACK_BUFFER_ARB = $88EB;
  GL_PIXEL_UNPACK_BUFFER_ARB = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING_ARB = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING_ARB = $88EF;

function Load_GL_ARB_pixel_buffer_object(): Boolean;

//**** GL_ARB_depth_buffer_float *****//
const
  GL_DEPTH_COMPONENT32F = $8CAC;
  GL_DEPTH32F_STENCIL8 = $8CAD;
  GL_FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;

function Load_GL_ARB_depth_buffer_float(): Boolean;

//**** GL_ARB_draw_instanced *****//
var
  glDrawArraysInstancedARB: procedure(mode: GLenum; first: GLint; count: GLsizei; primcount: GLsizei); extdecl;
  glDrawElementsInstancedARB: procedure(mode: GLenum; count: GLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei); extdecl;

function Load_GL_ARB_draw_instanced(): Boolean;

//**** GL_ARB_framebuffer_object *****//
const
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;
  GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
  GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
  GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
  GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
  GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
  GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
  GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
  GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
  GL_FRAMEBUFFER_DEFAULT = $8218;
  GL_FRAMEBUFFER_UNDEFINED = $8219;
  GL_DEPTH_STENCIL_ATTACHMENT = $821A;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_TEXTURE_STENCIL_SIZE = $88F1;
  GL_TEXTURE_RED_TYPE = $8C10;
  GL_TEXTURE_GREEN_TYPE = $8C11;
  GL_TEXTURE_BLUE_TYPE = $8C12;
  GL_TEXTURE_ALPHA_TYPE = $8C13;
  GL_TEXTURE_DEPTH_TYPE = $8C16;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_DRAW_FRAMEBUFFER_BINDING = GL_FRAMEBUFFER_BINDING;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_STENCIL_INDEX1 = $8D46;
  GL_STENCIL_INDEX4 = $8D47;
  GL_STENCIL_INDEX8 = $8D48;
  GL_STENCIL_INDEX16 = $8D49;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;
var
  glIsRenderbuffer: function(renderbuffer: GLuint): GLboolean; extdecl;
  glBindRenderbuffer: procedure(target: GLenum; renderbuffer: GLuint); extdecl;
  glDeleteRenderbuffers: procedure(n: GLsizei; const renderbuffers: PGLuint); extdecl;
  glGenRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); extdecl;
  glRenderbufferStorage: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); extdecl;
  glGetRenderbufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glIsFramebuffer: function(framebuffer: GLuint): GLboolean; extdecl;
  glBindFramebuffer: procedure(target: GLenum; framebuffer: GLuint); extdecl;
  glDeleteFramebuffers: procedure(n: GLsizei; const framebuffers: PGLuint); extdecl;
  glGenFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); extdecl;
  glCheckFramebufferStatus: function(target: GLenum): GLenum; extdecl;
  glFramebufferTexture1D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); extdecl;
  glFramebufferTexture2D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); extdecl;
  glFramebufferTexture3D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); extdecl;
  glFramebufferRenderbuffer: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); extdecl;
  glGetFramebufferAttachmentParameteriv: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGenerateMipmap: procedure(target: GLenum); extdecl;
  glBlitFramebuffer: procedure(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum); extdecl;
  glRenderbufferStorageMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); extdecl;
  glFramebufferTextureLayer: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); extdecl;

function Load_GL_ARB_framebuffer_object(): Boolean;

//**** GL_ARB_framebuffer_sRGB *****//
const
  GL_FRAMEBUFFER_SRGB = $8DB9;

function Load_GL_ARB_framebuffer_sRGB(): Boolean;

//**** GL_ARB_geometry_shader4 *****//
const
  GL_LINES_ADJACENCY_ARB = $000A;
  GL_LINE_STRIP_ADJACENCY_ARB = $000B;
  GL_TRIANGLES_ADJACENCY_ARB = $000C;
  GL_TRIANGLE_STRIP_ADJACENCY_ARB = $000D;
  GL_PROGRAM_POINT_SIZE_ARB = $8642;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB = $8C29;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB = $8DA7;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB = $8DA8;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB = $8DA9;
  GL_GEOMETRY_SHADER_ARB = $8DD9;
  GL_GEOMETRY_VERTICES_OUT_ARB = $8DDA;
  GL_GEOMETRY_INPUT_TYPE_ARB = $8DDB;
  GL_GEOMETRY_OUTPUT_TYPE_ARB = $8DDC;
  GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB = $8DDD;
  GL_MAX_VERTEX_VARYING_COMPONENTS_ARB = $8DDE;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB = $8DE1;
// reuse GL_MAX_VARYING_COMPONENTS 
// reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER 
var
  glProgramParameteriARB: procedure(_program: GLuint; pname: GLenum; value: GLint); extdecl;
  glFramebufferTextureARB: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint); extdecl;
  glFramebufferTextureLayerARB: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); extdecl;
  glFramebufferTextureFaceARB: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; face: GLenum); extdecl;

function Load_GL_ARB_geometry_shader4(): Boolean;

//**** GL_ARB_half_float_vertex *****//
const
  GL_HALF_FLOAT = $140B;

function Load_GL_ARB_half_float_vertex(): Boolean;

//**** GL_ARB_instanced_arrays *****//
const
  GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB = $88FE;
var
  glVertexAttribDivisorARB: procedure(index: GLuint; divisor: GLuint); extdecl;

function Load_GL_ARB_instanced_arrays(): Boolean;

//**** GL_ARB_map_buffer_range *****//
const
  GL_MAP_READ_BIT = $0001;
  GL_MAP_WRITE_BIT = $0002;
  GL_MAP_INVALIDATE_RANGE_BIT = $0004;
  GL_MAP_INVALIDATE_BUFFER_BIT = $0008;
  GL_MAP_FLUSH_EXPLICIT_BIT = $0010;
  GL_MAP_UNSYNCHRONIZED_BIT = $0020;
var
  glMapBufferRange: function(target: GLenum; offset: GLintptr; length: GLsizeiptr; access: GLbitfield): PGLvoid; extdecl;
  glFlushMappedBufferRange: procedure(target: GLenum; offset: GLintptr; length: GLsizeiptr); extdecl;

function Load_GL_ARB_map_buffer_range(): Boolean;

//**** GL_ARB_texture_buffer_object *****//
const
  GL_TEXTURE_BUFFER_ARB = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE_ARB = $8C2B;
  GL_TEXTURE_BINDING_BUFFER_ARB = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT_ARB = $8C2E;
var
  glTexBufferARB: procedure(target: GLenum; internalformat: GLenum; buffer: GLuint); extdecl;

function Load_GL_ARB_texture_buffer_object(): Boolean;

//**** GL_ARB_texture_compression_rgtc *****//
const
  GL_COMPRESSED_RED_RGTC1 = $8DBB;
  GL_COMPRESSED_SIGNED_RED_RGTC1 = $8DBC;
  GL_COMPRESSED_RG_RGTC2 = $8DBD;
  GL_COMPRESSED_SIGNED_RG_RGTC2 = $8DBE;

function Load_GL_ARB_texture_compression_rgtc(): Boolean;

//**** GL_ARB_texture_rg *****//
const
  GL_RG = $8227;
  GL_RG_INTEGER = $8228;
  GL_R8 = $8229;
  GL_R16 = $822A;
  GL_RG8 = $822B;
  GL_RG16 = $822C;
  GL_R16F = $822D;
  GL_R32F = $822E;
  GL_RG16F = $822F;
  GL_RG32F = $8230;
  GL_R8I = $8231;
  GL_R8UI = $8232;
  GL_R16I = $8233;
  GL_R16UI = $8234;
  GL_R32I = $8235;
  GL_R32UI = $8236;
  GL_RG8I = $8237;
  GL_RG8UI = $8238;
  GL_RG16I = $8239;
  GL_RG16UI = $823A;
  GL_RG32I = $823B;
  GL_RG32UI = $823C;

function Load_GL_ARB_texture_rg(): Boolean;

//**** GL_ARB_vertex_array_object *****//
const
  GL_VERTEX_ARRAY_BINDING = $85B5;
var
  glBindVertexArray: procedure(_array: GLuint); extdecl;
  glDeleteVertexArrays: procedure(n: GLsizei; const arrays: PGLuint); extdecl;
  glGenVertexArrays: procedure(n: GLsizei; arrays: PGLuint); extdecl;
  glIsVertexArray: function(_array: GLuint): GLboolean; extdecl;

function Load_GL_ARB_vertex_array_object(): Boolean;

//**** GL_ARB_uniform_buffer_object *****//
const
  GL_UNIFORM_BUFFER = $8A11;
  GL_UNIFORM_BUFFER_BINDING = $8A28;
  GL_UNIFORM_BUFFER_START = $8A29;
  GL_UNIFORM_BUFFER_SIZE = $8A2A;
  GL_MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
  GL_MAX_GEOMETRY_UNIFORM_BLOCKS = $8A2C;
  GL_MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
  GL_MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
  GL_MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
  GL_MAX_UNIFORM_BLOCK_SIZE = $8A30;
  GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
  GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS = $8A32;
  GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
  GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
  GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH = $8A35;
  GL_ACTIVE_UNIFORM_BLOCKS = $8A36;
  GL_UNIFORM_TYPE = $8A37;
  GL_UNIFORM_SIZE = $8A38;
  GL_UNIFORM_NAME_LENGTH = $8A39;
  GL_UNIFORM_BLOCK_INDEX = $8A3A;
  GL_UNIFORM_OFFSET = $8A3B;
  GL_UNIFORM_ARRAY_STRIDE = $8A3C;
  GL_UNIFORM_MATRIX_STRIDE = $8A3D;
  GL_UNIFORM_IS_ROW_MAJOR = $8A3E;
  GL_UNIFORM_BLOCK_BINDING = $8A3F;
  GL_UNIFORM_BLOCK_DATA_SIZE = $8A40;
  GL_UNIFORM_BLOCK_NAME_LENGTH = $8A41;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
  GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
  GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER = $8A45;
  GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
  GL_INVALID_INDEX = DWord($FFFFFFFF);
var
  glGetUniformIndices: procedure(_program: GLuint; uniformCount: GLsizei; const uniformNames: PPGLchar; uniformIndices: PGLuint); extdecl;
  glGetActiveUniformsiv: procedure(_program: GLuint; uniformCount: GLsizei; const uniformIndices: PGLuint; pname: GLenum; params: PGLint); extdecl;
  glGetActiveUniformName: procedure(_program: GLuint; uniformIndex: GLuint; bufSize: GLsizei; length: PGLsizei; uniformName: PGLchar); extdecl;
  glGetUniformBlockIndex: function(_program: GLuint; const uniformBlockName: PGLchar): GLuint; extdecl;
  glGetActiveUniformBlockiv: procedure(_program: GLuint; uniformBlockIndex: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetActiveUniformBlockName: procedure(_program: GLuint; uniformBlockIndex: GLuint; bufSize: GLsizei; length: PGLsizei; uniformBlockName: PGLchar); extdecl;
  glUniformBlockBinding: procedure(_program: GLuint; uniformBlockIndex: GLuint; uniformBlockBinding: GLuint); extdecl;

function Load_GL_ARB_uniform_buffer_object(): Boolean;

//**** GL_ARB_compatibility *****//
// ARB_compatibility just defines tokens from core 3.0 

function Load_GL_ARB_compatibility(): Boolean;

//**** GL_ARB_copy_buffer *****//
const
  GL_COPY_READ_BUFFER = $8F36;
  GL_COPY_WRITE_BUFFER = $8F37;
var
  glCopyBufferSubData: procedure(readTarget: GLenum; writeTarget: GLenum; readOffset: GLintptr; writeOffset: GLintptr; size: GLsizeiptr); extdecl;

function Load_GL_ARB_copy_buffer(): Boolean;

//**** GL_ARB_shader_texture_lod *****//

function Load_GL_ARB_shader_texture_lod(): Boolean;

//**** GL_ARB_depth_clamp *****//
const
  GL_DEPTH_CLAMP = $864F;

function Load_GL_ARB_depth_clamp(): Boolean;

//**** GL_ARB_draw_elements_base_vertex *****//
var
  glDrawElementsBaseVertex: procedure(mode: GLenum; count: GLsizei; _type: GLenum; const indices: PGLvoid; basevertex: GLint); extdecl;
  glDrawRangeElementsBaseVertex: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid; basevertex: GLint); extdecl;
  glDrawElementsInstancedBaseVertex: procedure(mode: GLenum; count: GLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei; basevertex: GLint); extdecl;
  glMultiDrawElementsBaseVertex: procedure(mode: GLenum; const count: PGLsizei; _type: GLenum; const indices: PPGLvoid; primcount: GLsizei; const basevertex: PGLint); extdecl;

function Load_GL_ARB_draw_elements_base_vertex(): Boolean;

//**** GL_ARB_fragment_coord_conventions *****//

function Load_GL_ARB_fragment_coord_conventions(): Boolean;

//**** GL_ARB_provoking_vertex *****//
const
  GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION = $8E4C;
  GL_FIRST_VERTEX_CONVENTION = $8E4D;
  GL_LAST_VERTEX_CONVENTION = $8E4E;
  GL_PROVOKING_VERTEX = $8E4F;
var
  glProvokingVertex: procedure(mode: GLenum); extdecl;

function Load_GL_ARB_provoking_vertex(): Boolean;

//**** GL_ARB_seamless_cube_map *****//
const
  GL_TEXTURE_CUBE_MAP_SEAMLESS = $884F;

function Load_GL_ARB_seamless_cube_map(): Boolean;

//**** GL_ARB_sync *****//
const
  GL_MAX_SERVER_WAIT_TIMEOUT = $9111;
  GL_OBJECT_TYPE = $9112;
  GL_SYNC_CONDITION = $9113;
  GL_SYNC_STATUS = $9114;
  GL_SYNC_FLAGS = $9115;
  GL_SYNC_FENCE = $9116;
  GL_SYNC_GPU_COMMANDS_COMPLETE = $9117;
  GL_UNSIGNALED = $9118;
  GL_SIGNALED = $9119;
  GL_ALREADY_SIGNALED = $911A;
  GL_TIMEOUT_EXPIRED = $911B;
  GL_CONDITION_SATISFIED = $911C;
  GL_WAIT_FAILED = $911D;
  GL_SYNC_FLUSH_COMMANDS_BIT = $00000001;
  GL_TIMEOUT_IGNORED = QWord($FFFFFFFFFFFFFFFF);
var
  glFenceSync: function(condition: GLenum; flags: GLbitfield): GLsync; extdecl;
  glIsSync: function(sync: GLsync): GLboolean; extdecl;
  glDeleteSync: procedure(sync: GLsync); extdecl;
  glClientWaitSync: function(sync: GLsync; flags: GLbitfield; timeout: GLuint64): GLenum; extdecl;
  glWaitSync: procedure(sync: GLsync; flags: GLbitfield; timeout: GLuint64); extdecl;
  glGetInteger64v: procedure(pname: GLenum; params: PGLint64); extdecl;
  glGetSynciv: procedure(sync: GLsync; pname: GLenum; bufSize: GLsizei; length: PGLsizei; values: PGLint); extdecl;

function Load_GL_ARB_sync(): Boolean;

//**** GL_ARB_texture_multisample *****//
const
  GL_SAMPLE_POSITION = $8E50;
  GL_SAMPLE_MASK = $8E51;
  GL_SAMPLE_MASK_VALUE = $8E52;
  GL_MAX_SAMPLE_MASK_WORDS = $8E59;
  GL_TEXTURE_2D_MULTISAMPLE = $9100;
  GL_PROXY_TEXTURE_2D_MULTISAMPLE = $9101;
  GL_TEXTURE_2D_MULTISAMPLE_ARRAY = $9102;
  GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY = $9103;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE = $9104;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY = $9105;
  GL_TEXTURE_SAMPLES = $9106;
  GL_TEXTURE_FIXED_SAMPLE_LOCATIONS = $9107;
  GL_SAMPLER_2D_MULTISAMPLE = $9108;
  GL_INT_SAMPLER_2D_MULTISAMPLE = $9109;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE = $910A;
  GL_SAMPLER_2D_MULTISAMPLE_ARRAY = $910B;
  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910C;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910D;
  GL_MAX_COLOR_TEXTURE_SAMPLES = $910E;
  GL_MAX_DEPTH_TEXTURE_SAMPLES = $910F;
  GL_MAX_INTEGER_SAMPLES = $9110;
var
  glTexImage2DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLint; width: GLsizei; height: GLsizei; fixedsamplelocations: GLboolean); extdecl;
  glTexImage3DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; fixedsamplelocations: GLboolean); extdecl;
  glGetMultisamplefv: procedure(pname: GLenum; index: GLuint; val: PGLfloat); extdecl;
  glSampleMaski: procedure(index: GLuint; mask: GLbitfield); extdecl;

function Load_GL_ARB_texture_multisample(): Boolean;

//**** GL_ARB_vertex_array_bgra *****//
// reuse GL_BGRA 

function Load_GL_ARB_vertex_array_bgra(): Boolean;

//**** GL_ARB_draw_buffers_blend *****//
var
  glBlendEquationi: procedure(buf: GLuint; mode: GLenum); extdecl;
  glBlendEquationSeparatei: procedure(buf: GLuint; modeRGB: GLenum; modeAlpha: GLenum); extdecl;
  glBlendFunci: procedure(buf: GLuint; src: GLenum; dst: GLenum); extdecl;
  glBlendFuncSeparatei: procedure(buf: GLuint; srcRGB: GLenum; dstRGB: GLenum; srcAlpha: GLenum; dstAlpha: GLenum); extdecl;

function Load_GL_ARB_draw_buffers_blend(): Boolean;

//**** GL_ARB_sample_shading *****//
const
  GL_SAMPLE_SHADING = $8C36;
  GL_MIN_SAMPLE_SHADING_VALUE = $8C37;
var
  glMinSampleShading: procedure(value: GLclampf); extdecl;

function Load_GL_ARB_sample_shading(): Boolean;

//**** GL_ARB_texture_cube_map_array *****//
const
  GL_TEXTURE_CUBE_MAP_ARRAY = $9009;
  GL_TEXTURE_BINDING_CUBE_MAP_ARRAY = $900A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARRAY = $900B;
  GL_SAMPLER_CUBE_MAP_ARRAY = $900C;
  GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW = $900D;
  GL_INT_SAMPLER_CUBE_MAP_ARRAY = $900E;
  GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY = $900F;

function Load_GL_ARB_texture_cube_map_array(): Boolean;

//**** GL_ARB_texture_gather *****//
const
  GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB = $8E5E;
  GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB = $8E5F;

function Load_GL_ARB_texture_gather(): Boolean;

//**** GL_ARB_texture_query_lod *****//

function Load_GL_ARB_texture_query_lod(): Boolean;

//**** GL_ARB_shading_language_include *****//
const
  GL_SHADER_INCLUDE_ARB = $8DAE;
  GL_NAMED_STRING_LENGTH_ARB = $8DE9;
  GL_NAMED_STRING_TYPE_ARB = $8DEA;
var
  glNamedStringARB: procedure(_type: GLenum; namelen: GLint; const name: PGLchar; stringlen: GLint; const _string: PGLchar); extdecl;
  glDeleteNamedStringARB: procedure(namelen: GLint; const name: PGLchar); extdecl;
  glCompileShaderIncludeARB: procedure(shader: GLuint; count: GLsizei; const path: PPGLchar; const length: PGLint); extdecl;
  glIsNamedStringARB: function(namelen: GLint; const name: PGLchar): GLboolean; extdecl;
  glGetNamedStringARB: procedure(namelen: GLint; const name: PGLchar; bufSize: GLsizei; stringlen: PGLint; _string: PGLchar); extdecl;
  glGetNamedStringivARB: procedure(namelen: GLint; const name: PGLchar; pname: GLenum; params: PGLint); extdecl;

function Load_GL_ARB_shading_language_include(): Boolean;

//**** GL_ARB_texture_compression_bptc *****//
const
  GL_COMPRESSED_RGBA_BPTC_UNORM_ARB = $8E8C;
  GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB = $8E8D;
  GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB = $8E8E;
  GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB = $8E8F;

function Load_GL_ARB_texture_compression_bptc(): Boolean;

//**** GL_ARB_blend_func_extended *****//
const
  GL_SRC1_COLOR = $88F9;
// reuse GL_SRC1_ALPHA 
const
  GL_ONE_MINUS_SRC1_COLOR = $88FA;
  GL_ONE_MINUS_SRC1_ALPHA = $88FB;
  GL_MAX_DUAL_SOURCE_DRAW_BUFFERS = $88FC;
var
  glBindFragDataLocationIndexed: procedure(_program: GLuint; colorNumber: GLuint; index: GLuint; const name: PGLchar); extdecl;
  glGetFragDataIndex: function(_program: GLuint; const name: PGLchar): GLint; extdecl;

function Load_GL_ARB_blend_func_extended(): Boolean;

//**** GL_ARB_explicit_attrib_location *****//

function Load_GL_ARB_explicit_attrib_location(): Boolean;

//**** GL_ARB_occlusion_query2 *****//
const
  GL_ANY_SAMPLES_PASSED = $8C2F;

function Load_GL_ARB_occlusion_query2(): Boolean;

//**** GL_ARB_sampler_objects *****//
const
  GL_SAMPLER_BINDING = $8919;
var
  glGenSamplers: procedure(count: GLsizei; samplers: PGLuint); extdecl;
  glDeleteSamplers: procedure(count: GLsizei; const samplers: PGLuint); extdecl;
  glIsSampler: function(sampler: GLuint): GLboolean; extdecl;
  glBindSampler: procedure(_unit: GLenum; sampler: GLuint); extdecl;
  glSamplerParameteri: procedure(sampler: GLuint; pname: GLenum; param: GLint); extdecl;
  glSamplerParameteriv: procedure(sampler: GLuint; pname: GLenum; const param: PGLint); extdecl;
  glSamplerParameterf: procedure(sampler: GLuint; pname: GLenum; param: GLfloat); extdecl;
  glSamplerParameterfv: procedure(sampler: GLuint; pname: GLenum; const param: PGLfloat); extdecl;
  glSamplerParameterIiv: procedure(sampler: GLuint; pname: GLenum; const param: PGLint); extdecl;
  glSamplerParameterIuiv: procedure(sampler: GLuint; pname: GLenum; const param: PGLuint); extdecl;
  glGetSamplerParameteriv: procedure(sampler: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetSamplerParameterIiv: procedure(sampler: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetSamplerParameterfv: procedure(sampler: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  //glGetSamplerParameterIfv: procedure(sampler: GLuint; pname: GLenum; params: PGLfloat); extdecl;

function Load_GL_ARB_sampler_objects(): Boolean;

//**** GL_ARB_shader_bit_encoding *****//

function Load_GL_ARB_shader_bit_encoding(): Boolean;

//**** GL_ARB_texture_rgb10_a2ui *****//
const
  GL_RGB10_A2UI = $906F;

function Load_GL_ARB_texture_rgb10_a2ui(): Boolean;

//**** GL_ARB_texture_swizzle *****//
const
  GL_TEXTURE_SWIZZLE_R = $8E42;
  GL_TEXTURE_SWIZZLE_G = $8E43;
  GL_TEXTURE_SWIZZLE_B = $8E44;
  GL_TEXTURE_SWIZZLE_A = $8E45;
  GL_TEXTURE_SWIZZLE_RGBA = $8E46;

function Load_GL_ARB_texture_swizzle(): Boolean;

//**** GL_ARB_timer_query *****//
const
  GL_TIME_ELAPSED = $88BF;
  GL_TIMESTAMP = $8E28;
var
  glQueryCounter: procedure(id: GLuint; target: GLenum); extdecl;
  glGetQueryObjecti64v: procedure(id: GLuint; pname: GLenum; params: PGLint64); extdecl;
  glGetQueryObjectui64v: procedure(id: GLuint; pname: GLenum; params: PGLuint64); extdecl;

function Load_GL_ARB_timer_query(): Boolean;

//**** GL_ARB_vertex_type_2_10_10_10_rev *****//
// reuse GL_UNSIGNED_INT_2_10_10_10_REV 
const
  GL_INT_2_10_10_10_REV = $8D9F;
var
  glVertexP2ui: procedure(_type: GLenum; value: GLuint); extdecl;
  glVertexP2uiv: procedure(_type: GLenum; const value: PGLuint); extdecl;
  glVertexP3ui: procedure(_type: GLenum; value: GLuint); extdecl;
  glVertexP3uiv: procedure(_type: GLenum; const value: PGLuint); extdecl;
  glVertexP4ui: procedure(_type: GLenum; value: GLuint); extdecl;
  glVertexP4uiv: procedure(_type: GLenum; const value: PGLuint); extdecl;
  glTexCoordP1ui: procedure(_type: GLenum; coords: GLuint); extdecl;
  glTexCoordP1uiv: procedure(_type: GLenum; const coords: PGLuint); extdecl;
  glTexCoordP2ui: procedure(_type: GLenum; coords: GLuint); extdecl;
  glTexCoordP2uiv: procedure(_type: GLenum; const coords: PGLuint); extdecl;
  glTexCoordP3ui: procedure(_type: GLenum; coords: GLuint); extdecl;
  glTexCoordP3uiv: procedure(_type: GLenum; const coords: PGLuint); extdecl;
  glTexCoordP4ui: procedure(_type: GLenum; coords: GLuint); extdecl;
  glTexCoordP4uiv: procedure(_type: GLenum; const coords: PGLuint); extdecl;
  glMultiTexCoordP1ui: procedure(texture: GLenum; _type: GLenum; coords: GLuint); extdecl;
  glMultiTexCoordP1uiv: procedure(texture: GLenum; _type: GLenum; const coords: PGLuint); extdecl;
  glMultiTexCoordP2ui: procedure(texture: GLenum; _type: GLenum; coords: GLuint); extdecl;
  glMultiTexCoordP2uiv: procedure(texture: GLenum; _type: GLenum; const coords: PGLuint); extdecl;
  glMultiTexCoordP3ui: procedure(texture: GLenum; _type: GLenum; coords: GLuint); extdecl;
  glMultiTexCoordP3uiv: procedure(texture: GLenum; _type: GLenum; const coords: PGLuint); extdecl;
  glMultiTexCoordP4ui: procedure(texture: GLenum; _type: GLenum; coords: GLuint); extdecl;
  glMultiTexCoordP4uiv: procedure(texture: GLenum; _type: GLenum; const coords: PGLuint); extdecl;
  glNormalP3ui: procedure(_type: GLenum; coords: GLuint); extdecl;
  glNormalP3uiv: procedure(_type: GLenum; const coords: PGLuint); extdecl;
  glColorP3ui: procedure(_type: GLenum; color: GLuint); extdecl;
  glColorP3uiv: procedure(_type: GLenum; const color: PGLuint); extdecl;
  glColorP4ui: procedure(_type: GLenum; color: GLuint); extdecl;
  glColorP4uiv: procedure(_type: GLenum; const color: PGLuint); extdecl;
  glSecondaryColorP3ui: procedure(_type: GLenum; color: GLuint); extdecl;
  glSecondaryColorP3uiv: procedure(_type: GLenum; const color: PGLuint); extdecl;
  glVertexAttribP1ui: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; value: GLuint); extdecl;
  glVertexAttribP1uiv: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; const value: PGLuint); extdecl;
  glVertexAttribP2ui: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; value: GLuint); extdecl;
  glVertexAttribP2uiv: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; const value: PGLuint); extdecl;
  glVertexAttribP3ui: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; value: GLuint); extdecl;
  glVertexAttribP3uiv: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; const value: PGLuint); extdecl;
  glVertexAttribP4ui: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; value: GLuint); extdecl;
  glVertexAttribP4uiv: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; const value: PGLuint); extdecl;

function Load_GL_ARB_vertex_type_2_10_10_10_rev(): Boolean;

//**** GL_ARB_draw_indirect *****//
const
  GL_DRAW_INDIRECT_BUFFER = $8F3F;
  GL_DRAW_INDIRECT_BUFFER_BINDING = $8F43;
var
  glDrawArraysIndirect: procedure(mode: GLenum; const indirect: PGLvoid); extdecl;
  glDrawElementsIndirect: procedure(mode: GLenum; _type: GLenum; const indirect: PGLvoid); extdecl;

function Load_GL_ARB_draw_indirect(): Boolean;

//**** GL_ARB_gpu_shader5 *****//
const
  GL_GEOMETRY_SHADER_INVOCATIONS = $887F;
  GL_MAX_GEOMETRY_SHADER_INVOCATIONS = $8E5A;
  GL_MIN_FRAGMENT_INTERPOLATION_OFFSET = $8E5B;
  GL_MAX_FRAGMENT_INTERPOLATION_OFFSET = $8E5C;
  GL_FRAGMENT_INTERPOLATION_OFFSET_BITS = $8E5D;
  GL_MAX_VERTEX_STREAMS = $8E71;

function Load_GL_ARB_gpu_shader5(): Boolean;

//**** GL_ARB_gpu_shader_fp64 *****//
// reuse GL_DOUBLE 
const
  GL_DOUBLE_VEC2 = $8FFC;
  GL_DOUBLE_VEC3 = $8FFD;
  GL_DOUBLE_VEC4 = $8FFE;
  GL_DOUBLE_MAT2 = $8F46;
  GL_DOUBLE_MAT3 = $8F47;
  GL_DOUBLE_MAT4 = $8F48;
  GL_DOUBLE_MAT2x3 = $8F49;
  GL_DOUBLE_MAT2x4 = $8F4A;
  GL_DOUBLE_MAT3x2 = $8F4B;
  GL_DOUBLE_MAT3x4 = $8F4C;
  GL_DOUBLE_MAT4x2 = $8F4D;
  GL_DOUBLE_MAT4x3 = $8F4E;
var
  glUniform1d: procedure(location: GLint; x: GLdouble); extdecl;
  glUniform2d: procedure(location: GLint; x: GLdouble; y: GLdouble); extdecl;
  glUniform3d: procedure(location: GLint; x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glUniform4d: procedure(location: GLint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glUniform1dv: procedure(location: GLint; count: GLsizei; const value: PGLdouble); extdecl;
  glUniform2dv: procedure(location: GLint; count: GLsizei; const value: PGLdouble); extdecl;
  glUniform3dv: procedure(location: GLint; count: GLsizei; const value: PGLdouble); extdecl;
  glUniform4dv: procedure(location: GLint; count: GLsizei; const value: PGLdouble); extdecl;
  glUniformMatrix2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glUniformMatrix3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glUniformMatrix4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glUniformMatrix2x3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glUniformMatrix2x4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glUniformMatrix3x2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glUniformMatrix3x4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glUniformMatrix4x2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glUniformMatrix4x3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glGetUniformdv: procedure(_program: GLuint; location: GLint; params: PGLdouble); extdecl;
  glProgramUniform1dEXT: procedure(_program: GLuint; location: GLint; x: GLdouble); extdecl;
  glProgramUniform2dEXT: procedure(_program: GLuint; location: GLint; x: GLdouble; y: GLdouble); extdecl;
  glProgramUniform3dEXT: procedure(_program: GLuint; location: GLint; x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glProgramUniform4dEXT: procedure(_program: GLuint; location: GLint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glProgramUniform1dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLdouble); extdecl;
  glProgramUniform2dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLdouble); extdecl;
  glProgramUniform3dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLdouble); extdecl;
  glProgramUniform4dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLdouble); extdecl;
  glProgramUniformMatrix2dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glProgramUniformMatrix3dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glProgramUniformMatrix4dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glProgramUniformMatrix2x3dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glProgramUniformMatrix2x4dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glProgramUniformMatrix3x2dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glProgramUniformMatrix3x4dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glProgramUniformMatrix4x2dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;
  glProgramUniformMatrix4x3dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); extdecl;

function Load_GL_ARB_gpu_shader_fp64(): Boolean;

//**** GL_ARB_shader_subroutine *****//
const
  GL_ACTIVE_SUBROUTINES = $8DE5;
  GL_ACTIVE_SUBROUTINE_UNIFORMS = $8DE6;
  GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS = $8E47;
  GL_ACTIVE_SUBROUTINE_MAX_LENGTH = $8E48;
  GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH = $8E49;
  GL_MAX_SUBROUTINES = $8DE7;
  GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS = $8DE8;
  GL_NUM_COMPATIBLE_SUBROUTINES = $8E4A;
  GL_COMPATIBLE_SUBROUTINES = $8E4B;
// reuse GL_UNIFORM_SIZE 
// reuse GL_UNIFORM_NAME_LENGTH 
var
  glGetSubroutineUniformLocation: function(_program: GLuint; shadertype: GLenum; const name: PGLchar): GLint; extdecl;
  glGetSubroutineIndex: function(_program: GLuint; shadertype: GLenum; const name: PGLchar): GLuint; extdecl;
  glGetActiveSubroutineUniformiv: procedure(_program: GLuint; shadertype: GLenum; index: GLuint; pname: GLenum; values: PGLint); extdecl;
  glGetActiveSubroutineUniformName: procedure(_program: GLuint; shadertype: GLenum; index: GLuint; bufsize: GLsizei; length: PGLsizei; name: PGLchar); extdecl;
  glGetActiveSubroutineName: procedure(_program: GLuint; shadertype: GLenum; index: GLuint; bufsize: GLsizei; length: PGLsizei; name: PGLchar); extdecl;
  glUniformSubroutinesuiv: procedure(shadertype: GLenum; count: GLsizei; const indices: PGLuint); extdecl;
  glGetUniformSubroutineuiv: procedure(shadertype: GLenum; location: GLint; params: PGLuint); extdecl;
  glGetProgramStageiv: procedure(_program: GLuint; shadertype: GLenum; pname: GLenum; values: PGLint); extdecl;

function Load_GL_ARB_shader_subroutine(): Boolean;

//**** GL_ARB_tessellation_shader *****//
const
  GL_PATCHES = $000E;
  GL_PATCH_VERTICES = $8E72;
  GL_PATCH_DEFAULT_INNER_LEVEL = $8E73;
  GL_PATCH_DEFAULT_OUTER_LEVEL = $8E74;
  GL_TESS_CONTROL_OUTPUT_VERTICES = $8E75;
  GL_TESS_GEN_MODE = $8E76;
  GL_TESS_GEN_SPACING = $8E77;
  GL_TESS_GEN_VERTEX_ORDER = $8E78;
  GL_TESS_GEN_POINT_MODE = $8E79;
// reuse GL_TRIANGLES 
// reuse GL_QUADS 
const
  GL_ISOLINES = $8E7A;
// reuse GL_EQUAL 
const
  GL_FRACTIONAL_ODD = $8E7B;
  GL_FRACTIONAL_EVEN = $8E7C;
// reuse GL_CCW 
// reuse GL_CW 
const
  GL_MAX_PATCH_VERTICES = $8E7D;
  GL_MAX_TESS_GEN_LEVEL = $8E7E;
  GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS = $8E7F;
  GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E80;
  GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS = $8E81;
  GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS = $8E82;
  GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS = $8E83;
  GL_MAX_TESS_PATCH_COMPONENTS = $8E84;
  GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS = $8E85;
  GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS = $8E86;
  GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS = $8E89;
  GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS = $8E8A;
  GL_MAX_TESS_CONTROL_INPUT_COMPONENTS = $886C;
  GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS = $886D;
  GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS = $8E1E;
  GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E1F;
  GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER = $84F0;
  GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER = $84F1;
  GL_TESS_EVALUATION_SHADER = $8E87;
  GL_TESS_CONTROL_SHADER = $8E88;
var
  glPatchParameteri: procedure(pname: GLenum; value: GLint); extdecl;
  glPatchParameterfv: procedure(pname: GLenum; const values: PGLfloat); extdecl;

function Load_GL_ARB_tessellation_shader(): Boolean;

//**** GL_ARB_texture_buffer_object_rgb32 *****//
// reuse GL_RGB32F 
// reuse GL_RGB32UI 
// reuse GL_RGB32I 

function Load_GL_ARB_texture_buffer_object_rgb32(): Boolean;

//**** GL_ARB_transform_feedback2 *****//
const
  GL_TRANSFORM_FEEDBACK = $8E22;
  GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED = $8E23;
  GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE = $8E24;
  GL_TRANSFORM_FEEDBACK_BINDING = $8E25;
var
  glBindTransformFeedback: procedure(target: GLenum; id: GLuint); extdecl;
  glDeleteTransformFeedbacks: procedure(n: GLsizei; const ids: PGLuint); extdecl;
  glGenTransformFeedbacks: procedure(n: GLsizei; ids: PGLuint); extdecl;
  glIsTransformFeedback: function(id: GLuint): GLboolean; extdecl;
  glPauseTransformFeedback: procedure(); extdecl;
  glResumeTransformFeedback: procedure(); extdecl;
  glDrawTransformFeedback: procedure(mode: GLenum; id: GLuint); extdecl;

function Load_GL_ARB_transform_feedback2(): Boolean;

//**** GL_ARB_transform_feedback3 *****//
const
  GL_MAX_TRANSFORM_FEEDBACK_BUFFERS = $8E70;
var
  glDrawTransformFeedbackStream: procedure(mode: GLenum; id: GLuint; stream: GLuint); extdecl;
  glBeginQueryIndexed: procedure(target: GLenum; index: GLuint; id: GLuint); extdecl;
  glEndQueryIndexed: procedure(target: GLenum; index: GLuint); extdecl;
  glGetQueryIndexediv: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLint); extdecl;

function Load_GL_ARB_transform_feedback3(): Boolean;

//**** GL_EXT_abgr *****//
const
  GL_ABGR_EXT = $8000;

function Load_GL_EXT_abgr(): Boolean;

//**** GL_EXT_blend_color *****//
const
  GL_CONSTANT_COLOR_EXT = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT = $8002;
  GL_CONSTANT_ALPHA_EXT = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT = $8004;
  GL_BLEND_COLOR_EXT = $8005;
var
  glBlendColorEXT: procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); extdecl;

function Load_GL_EXT_blend_color(): Boolean;

//**** GL_EXT_polygon_offset *****//
const
  GL_POLYGON_OFFSET_EXT = $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT = $8038;
  GL_POLYGON_OFFSET_BIAS_EXT = $8039;
var
  glPolygonOffsetEXT: procedure(factor: GLfloat; bias: GLfloat); extdecl;

function Load_GL_EXT_polygon_offset(): Boolean;

//**** GL_EXT_texture *****//
const
  GL_ALPHA4_EXT = $803B;
  GL_ALPHA8_EXT = $803C;
  GL_ALPHA12_EXT = $803D;
  GL_ALPHA16_EXT = $803E;
  GL_LUMINANCE4_EXT = $803F;
  GL_LUMINANCE8_EXT = $8040;
  GL_LUMINANCE12_EXT = $8041;
  GL_LUMINANCE16_EXT = $8042;
  GL_LUMINANCE4_ALPHA4_EXT = $8043;
  GL_LUMINANCE6_ALPHA2_EXT = $8044;
  GL_LUMINANCE8_ALPHA8_EXT = $8045;
  GL_LUMINANCE12_ALPHA4_EXT = $8046;
  GL_LUMINANCE12_ALPHA12_EXT = $8047;
  GL_LUMINANCE16_ALPHA16_EXT = $8048;
  GL_INTENSITY_EXT = $8049;
  GL_INTENSITY4_EXT = $804A;
  GL_INTENSITY8_EXT = $804B;
  GL_INTENSITY12_EXT = $804C;
  GL_INTENSITY16_EXT = $804D;
  GL_RGB2_EXT = $804E;
  GL_RGB4_EXT = $804F;
  GL_RGB5_EXT = $8050;
  GL_RGB8_EXT = $8051;
  GL_RGB10_EXT = $8052;
  GL_RGB12_EXT = $8053;
  GL_RGB16_EXT = $8054;
  GL_RGBA2_EXT = $8055;
  GL_RGBA4_EXT = $8056;
  GL_RGB5_A1_EXT = $8057;
  GL_RGBA8_EXT = $8058;
  GL_RGB10_A2_EXT = $8059;
  GL_RGBA12_EXT = $805A;
  GL_RGBA16_EXT = $805B;
  GL_TEXTURE_RED_SIZE_EXT = $805C;
  GL_TEXTURE_GREEN_SIZE_EXT = $805D;
  GL_TEXTURE_BLUE_SIZE_EXT = $805E;
  GL_TEXTURE_ALPHA_SIZE_EXT = $805F;
  GL_TEXTURE_LUMINANCE_SIZE_EXT = $8060;
  GL_TEXTURE_INTENSITY_SIZE_EXT = $8061;
  GL_REPLACE_EXT = $8062;
  GL_PROXY_TEXTURE_1D_EXT = $8063;
  GL_PROXY_TEXTURE_2D_EXT = $8064;
  GL_TEXTURE_TOO_LARGE_EXT = $8065;

function Load_GL_EXT_texture(): Boolean;

//**** GL_EXT_texture3D *****//
const
  GL_PACK_SKIP_IMAGES_EXT = $806B;
  GL_PACK_IMAGE_HEIGHT_EXT = $806C;
  GL_UNPACK_SKIP_IMAGES_EXT = $806D;
  GL_UNPACK_IMAGE_HEIGHT_EXT = $806E;
  GL_TEXTURE_3D_EXT = $806F;
  GL_PROXY_TEXTURE_3D_EXT = $8070;
  GL_TEXTURE_DEPTH_EXT = $8071;
  GL_TEXTURE_WRAP_R_EXT = $8072;
  GL_MAX_3D_TEXTURE_SIZE_EXT = $8073;
var
  glTexImage3DEXT: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glTexSubImage3DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl; (* Also used in GL_EXT_subtexture *)

function Load_GL_EXT_texture3D(): Boolean;

//**** GL_SGIS_texture_filter4 *****//
const
  GL_FILTER4_SGIS = $8146;
  GL_TEXTURE_FILTER4_SIZE_SGIS = $8147;
var
  glGetTexFilterFuncSGIS: procedure(target: GLenum; filter: GLenum; weights: PGLfloat); extdecl;
  glTexFilterFuncSGIS: procedure(target: GLenum; filter: GLenum; n: GLsizei; const weights: PGLfloat); extdecl;

function Load_GL_SGIS_texture_filter4(): Boolean;

//**** GL_EXT_subtexture *****//
var
  glTexSubImage1DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glTexSubImage2DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;

function Load_GL_EXT_subtexture(): Boolean;

//**** GL_EXT_copy_texture *****//
var
  glCopyTexImage1DEXT: procedure(target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; border: GLint); extdecl;
  glCopyTexImage2DEXT: procedure(target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); extdecl;
  glCopyTexSubImage1DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; x: GLint; y: GLint; width: GLsizei); extdecl;
  glCopyTexSubImage2DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); extdecl;
  glCopyTexSubImage3DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); extdecl;

function Load_GL_EXT_copy_texture(): Boolean;

//**** GL_EXT_histogram *****//
const
  GL_HISTOGRAM_EXT = $8024;
  GL_PROXY_HISTOGRAM_EXT = $8025;
  GL_HISTOGRAM_WIDTH_EXT = $8026;
  GL_HISTOGRAM_FORMAT_EXT = $8027;
  GL_HISTOGRAM_RED_SIZE_EXT = $8028;
  GL_HISTOGRAM_GREEN_SIZE_EXT = $8029;
  GL_HISTOGRAM_BLUE_SIZE_EXT = $802A;
  GL_HISTOGRAM_ALPHA_SIZE_EXT = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE_EXT = $802C;
  GL_HISTOGRAM_SINK_EXT = $802D;
  GL_MINMAX_EXT = $802E;
  GL_MINMAX_FORMAT_EXT = $802F;
  GL_MINMAX_SINK_EXT = $8030;
  GL_TABLE_TOO_LARGE_EXT = $8031;
var
  glGetHistogramEXT: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: PGLvoid); extdecl;
  glGetHistogramParameterfvEXT: procedure(target: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetHistogramParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetMinmaxEXT: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: PGLvoid); extdecl;
  glGetMinmaxParameterfvEXT: procedure(target: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetMinmaxParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glHistogramEXT: procedure(target: GLenum; width: GLsizei; internalformat: GLenum; sink: GLboolean); extdecl;
  glMinmaxEXT: procedure(target: GLenum; internalformat: GLenum; sink: GLboolean); extdecl;
  glResetHistogramEXT: procedure(target: GLenum); extdecl;
  glResetMinmaxEXT: procedure(target: GLenum); extdecl;

function Load_GL_EXT_histogram(): Boolean;

//**** GL_EXT_convolution *****//
const
  GL_CONVOLUTION_1D_EXT = $8010;
  GL_CONVOLUTION_2D_EXT = $8011;
  GL_SEPARABLE_2D_EXT = $8012;
  GL_CONVOLUTION_BORDER_MODE_EXT = $8013;
  GL_CONVOLUTION_FILTER_SCALE_EXT = $8014;
  GL_CONVOLUTION_FILTER_BIAS_EXT = $8015;
  GL_REDUCE_EXT = $8016;
  GL_CONVOLUTION_FORMAT_EXT = $8017;
  GL_CONVOLUTION_WIDTH_EXT = $8018;
  GL_CONVOLUTION_HEIGHT_EXT = $8019;
  GL_MAX_CONVOLUTION_WIDTH_EXT = $801A;
  GL_MAX_CONVOLUTION_HEIGHT_EXT = $801B;
  GL_POST_CONVOLUTION_RED_SCALE_EXT = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE_EXT = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE_EXT = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE_EXT = $801F;
  GL_POST_CONVOLUTION_RED_BIAS_EXT = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS_EXT = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS_EXT = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS_EXT = $8023;
var
  glConvolutionFilter1DEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const image: PGLvoid); extdecl;
  glConvolutionFilter2DEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const image: PGLvoid); extdecl;
  glConvolutionParameterfEXT: procedure(target: GLenum; pname: GLenum; params: GLfloat); extdecl;
  glConvolutionParameterfvEXT: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glConvolutionParameteriEXT: procedure(target: GLenum; pname: GLenum; params: GLint); extdecl;
  glConvolutionParameterivEXT: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glCopyConvolutionFilter1DEXT: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); extdecl;
  glCopyConvolutionFilter2DEXT: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei); extdecl;
  glGetConvolutionFilterEXT: procedure(target: GLenum; format: GLenum; _type: GLenum; image: PGLvoid); extdecl;
  glGetConvolutionParameterfvEXT: procedure(target: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetConvolutionParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetSeparableFilterEXT: procedure(target: GLenum; format: GLenum; _type: GLenum; row: PGLvoid; column: PGLvoid; span: PGLvoid); extdecl;
  glSeparableFilter2DEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const row: PGLvoid; const column: PGLvoid); extdecl;

function Load_GL_EXT_convolution(): Boolean;

//**** GL_SGI_color_matrix *****//
const
  GL_COLOR_MATRIX_SGI = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH_SGI = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE_SGI = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS_SGI = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI = $80BB;

function Load_GL_SGI_color_matrix(): Boolean;

//**** GL_SGI_color_table *****//
const
  GL_COLOR_TABLE_SGI = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE_SGI = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D2;
  GL_PROXY_COLOR_TABLE_SGI = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D5;
  GL_COLOR_TABLE_SCALE_SGI = $80D6;
  GL_COLOR_TABLE_BIAS_SGI = $80D7;
  GL_COLOR_TABLE_FORMAT_SGI = $80D8;
  GL_COLOR_TABLE_WIDTH_SGI = $80D9;
  GL_COLOR_TABLE_RED_SIZE_SGI = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_SGI = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_SGI = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_SGI = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_SGI = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_SGI = $80DF;
var
  glColorTableSGI: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const table: PGLvoid); extdecl;
  glColorTableParameterfvSGI: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glColorTableParameterivSGI: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glCopyColorTableSGI: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); extdecl;
  glGetColorTableSGI: procedure(target: GLenum; format: GLenum; _type: GLenum; table: PGLvoid); extdecl;
  glGetColorTableParameterfvSGI: procedure(target: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetColorTableParameterivSGI: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;

function Load_GL_SGI_color_table(): Boolean;

//**** GL_SGIS_pixel_texture *****//
const
  GL_PIXEL_TEXTURE_SGIS = $8353;
  GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS = $8354;
  GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS = $8355;
  GL_PIXEL_GROUP_COLOR_SGIS = $8356;
var
  glPixelTexGenParameteriSGIS: procedure(pname: GLenum; param: GLint); extdecl;
  glPixelTexGenParameterivSGIS: procedure(pname: GLenum; const params: PGLint); extdecl;
  glPixelTexGenParameterfSGIS: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPixelTexGenParameterfvSGIS: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glGetPixelTexGenParameterivSGIS: procedure(pname: GLenum; params: PGLint); extdecl;
  glGetPixelTexGenParameterfvSGIS: procedure(pname: GLenum; params: PGLfloat); extdecl;

function Load_GL_SGIS_pixel_texture(): Boolean;

//**** GL_SGIX_pixel_texture *****//
const
  GL_PIXEL_TEX_GEN_SGIX = $8139;
  GL_PIXEL_TEX_GEN_MODE_SGIX = $832B;
var
  glPixelTexGenSGIX: procedure(mode: GLenum); extdecl;

function Load_GL_SGIX_pixel_texture(): Boolean;

//**** GL_SGIS_texture4D *****//
const
  GL_PACK_SKIP_VOLUMES_SGIS = $8130;
  GL_PACK_IMAGE_DEPTH_SGIS = $8131;
  GL_UNPACK_SKIP_VOLUMES_SGIS = $8132;
  GL_UNPACK_IMAGE_DEPTH_SGIS = $8133;
  GL_TEXTURE_4D_SGIS = $8134;
  GL_PROXY_TEXTURE_4D_SGIS = $8135;
  GL_TEXTURE_4DSIZE_SGIS = $8136;
  GL_TEXTURE_WRAP_Q_SGIS = $8137;
  GL_MAX_4D_TEXTURE_SIZE_SGIS = $8138;
  GL_TEXTURE_4D_BINDING_SGIS = $814F;
var
  glTexImage4DSGIS: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; size4d: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glTexSubImage4DSGIS: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; woffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; size4d: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;

function Load_GL_SGIS_texture4D(): Boolean;

//**** GL_SGI_texture_color_table *****//
const
  GL_TEXTURE_COLOR_TABLE_SGI = $80BC;
  GL_PROXY_TEXTURE_COLOR_TABLE_SGI = $80BD;

function Load_GL_SGI_texture_color_table(): Boolean;

//**** GL_EXT_cmyka *****//
const
  GL_CMYK_EXT = $800C;
  GL_CMYKA_EXT = $800D;
  GL_PACK_CMYK_HINT_EXT = $800E;
  GL_UNPACK_CMYK_HINT_EXT = $800F;

function Load_GL_EXT_cmyka(): Boolean;

//**** GL_EXT_texture_object *****//
const
  GL_TEXTURE_PRIORITY_EXT = $8066;
  GL_TEXTURE_RESIDENT_EXT = $8067;
  GL_TEXTURE_1D_BINDING_EXT = $8068;
  GL_TEXTURE_2D_BINDING_EXT = $8069;
  GL_TEXTURE_3D_BINDING_EXT = $806A;
var
  glAreTexturesResidentEXT: function(n: GLsizei; const textures: PGLuint; residences: PGLboolean): GLboolean; extdecl;
  glBindTextureEXT: procedure(target: GLenum; texture: GLuint); extdecl;
  glDeleteTexturesEXT: procedure(n: GLsizei; const textures: PGLuint); extdecl;
  glGenTexturesEXT: procedure(n: GLsizei; textures: PGLuint); extdecl;
  glIsTextureEXT: function(texture: GLuint): GLboolean; extdecl;
  glPrioritizeTexturesEXT: procedure(n: GLsizei; const textures: PGLuint; const priorities: PGLclampf); extdecl;

function Load_GL_EXT_texture_object(): Boolean;

//**** GL_SGIS_detail_texture *****//
const
  GL_DETAIL_TEXTURE_2D_SGIS = $8095;
  GL_DETAIL_TEXTURE_2D_BINDING_SGIS = $8096;
  GL_LINEAR_DETAIL_SGIS = $8097;
  GL_LINEAR_DETAIL_ALPHA_SGIS = $8098;
  GL_LINEAR_DETAIL_COLOR_SGIS = $8099;
  GL_DETAIL_TEXTURE_LEVEL_SGIS = $809A;
  GL_DETAIL_TEXTURE_MODE_SGIS = $809B;
  GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS = $809C;
var
  glDetailTexFuncSGIS: procedure(target: GLenum; n: GLsizei; const points: PGLfloat); extdecl;
  glGetDetailTexFuncSGIS: procedure(target: GLenum; points: PGLfloat); extdecl;

function Load_GL_SGIS_detail_texture(): Boolean;

//**** GL_SGIS_sharpen_texture *****//
const
  GL_LINEAR_SHARPEN_SGIS = $80AD;
  GL_LINEAR_SHARPEN_ALPHA_SGIS = $80AE;
  GL_LINEAR_SHARPEN_COLOR_SGIS = $80AF;
  GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS = $80B0;
var
  glSharpenTexFuncSGIS: procedure(target: GLenum; n: GLsizei; const points: PGLfloat); extdecl;
  glGetSharpenTexFuncSGIS: procedure(target: GLenum; points: PGLfloat); extdecl;

function Load_GL_SGIS_sharpen_texture(): Boolean;

//**** GL_EXT_packed_pixels *****//
const
  GL_UNSIGNED_BYTE_3_3_2_EXT = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4_EXT = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1_EXT = $8034;
  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
  GL_UNSIGNED_INT_10_10_10_2_EXT = $8036;

function Load_GL_EXT_packed_pixels(): Boolean;

//**** GL_SGIS_texture_lod *****//
const
  GL_TEXTURE_MIN_LOD_SGIS = $813A;
  GL_TEXTURE_MAX_LOD_SGIS = $813B;
  GL_TEXTURE_BASE_LEVEL_SGIS = $813C;
  GL_TEXTURE_MAX_LEVEL_SGIS = $813D;

function Load_GL_SGIS_texture_lod(): Boolean;

//**** GL_SGIS_multisample *****//
const
  GL_MULTISAMPLE_SGIS = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_SGIS = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_SGIS = $809F;
  GL_SAMPLE_MASK_SGIS = $80A0;
  GL_1PASS_SGIS = $80A1;
  GL_2PASS_0_SGIS = $80A2;
  GL_2PASS_1_SGIS = $80A3;
  GL_4PASS_0_SGIS = $80A4;
  GL_4PASS_1_SGIS = $80A5;
  GL_4PASS_2_SGIS = $80A6;
  GL_4PASS_3_SGIS = $80A7;
  GL_SAMPLE_BUFFERS_SGIS = $80A8;
  GL_SAMPLES_SGIS = $80A9;
  GL_SAMPLE_MASK_VALUE_SGIS = $80AA;
  GL_SAMPLE_MASK_INVERT_SGIS = $80AB;
  GL_SAMPLE_PATTERN_SGIS = $80AC;
var
  glSampleMaskSGIS: procedure(value: GLclampf; invert: GLboolean); extdecl;
  glSamplePatternSGIS: procedure(pattern: GLenum); extdecl;

function Load_GL_SGIS_multisample(): Boolean;

//**** GL_EXT_rescale_normal *****//
const
  GL_RESCALE_NORMAL_EXT = $803A;

function Load_GL_EXT_rescale_normal(): Boolean;

//**** GL_EXT_vertex_array *****//
const
  GL_VERTEX_ARRAY_EXT = $8074;
  GL_NORMAL_ARRAY_EXT = $8075;
  GL_COLOR_ARRAY_EXT = $8076;
  GL_INDEX_ARRAY_EXT = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT = $8078;
  GL_EDGE_FLAG_ARRAY_EXT = $8079;
  GL_VERTEX_ARRAY_SIZE_EXT = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT = $8080;
  GL_COLOR_ARRAY_SIZE_EXT = $8081;
  GL_COLOR_ARRAY_TYPE_EXT = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT = $8083;
  GL_COLOR_ARRAY_COUNT_EXT = $8084;
  GL_INDEX_ARRAY_TYPE_EXT = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT = $8086;
  GL_INDEX_ARRAY_COUNT_EXT = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT = $808F;
  GL_COLOR_ARRAY_POINTER_EXT = $8090;
  GL_INDEX_ARRAY_POINTER_EXT = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT = $8093;
var
  glArrayElementEXT: procedure(i: GLint); extdecl;
  glColorPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; count: GLsizei; const _pointer: PGLvoid); extdecl;
  glDrawArraysEXT: procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  glEdgeFlagPointerEXT: procedure(stride: GLsizei; count: GLsizei; const _pointer: PGLboolean); extdecl;
  glGetPointervEXT: procedure(pname: GLenum; params: PPGLvoid); extdecl;
  glIndexPointerEXT: procedure(_type: GLenum; stride: GLsizei; count: GLsizei; const _pointer: PGLvoid); extdecl;
  glNormalPointerEXT: procedure(_type: GLenum; stride: GLsizei; count: GLsizei; const _pointer: PGLvoid); extdecl;
  glTexCoordPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; count: GLsizei; const _pointer: PGLvoid); extdecl;
  glVertexPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; count: GLsizei; const _pointer: PGLvoid); extdecl;

function Load_GL_EXT_vertex_array(): Boolean;

//**** GL_EXT_misc_attribute *****//

function Load_GL_EXT_misc_attribute(): Boolean;

//**** GL_SGIS_generate_mipmap *****//
const
  GL_GENERATE_MIPMAP_SGIS = $8191;
  GL_GENERATE_MIPMAP_HINT_SGIS = $8192;

function Load_GL_SGIS_generate_mipmap(): Boolean;

//**** GL_SGIX_clipmap *****//
const
  GL_LINEAR_CLIPMAP_LINEAR_SGIX = $8170;
  GL_TEXTURE_CLIPMAP_CENTER_SGIX = $8171;
  GL_TEXTURE_CLIPMAP_FRAME_SGIX = $8172;
  GL_TEXTURE_CLIPMAP_OFFSET_SGIX = $8173;
  GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX = $8174;
  GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX = $8175;
  GL_TEXTURE_CLIPMAP_DEPTH_SGIX = $8176;
  GL_MAX_CLIPMAP_DEPTH_SGIX = $8177;
  GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX = $8178;
  GL_NEAREST_CLIPMAP_NEAREST_SGIX = $844D;
  GL_NEAREST_CLIPMAP_LINEAR_SGIX = $844E;
  GL_LINEAR_CLIPMAP_NEAREST_SGIX = $844F;

function Load_GL_SGIX_clipmap(): Boolean;

//**** GL_SGIX_shadow *****//
const
  GL_TEXTURE_COMPARE_SGIX = $819A;
  GL_TEXTURE_COMPARE_OPERATOR_SGIX = $819B;
  GL_TEXTURE_LEQUAL_R_SGIX = $819C;
  GL_TEXTURE_GEQUAL_R_SGIX = $819D;

function Load_GL_SGIX_shadow(): Boolean;

//**** GL_SGIS_texture_edge_clamp *****//
const
  GL_CLAMP_TO_EDGE_SGIS = $812F;

function Load_GL_SGIS_texture_edge_clamp(): Boolean;

//**** GL_SGIS_texture_border_clamp *****//
const
  GL_CLAMP_TO_BORDER_SGIS = $812D;

function Load_GL_SGIS_texture_border_clamp(): Boolean;

//**** GL_EXT_blend_minmax *****//
const
  GL_FUNC_ADD_EXT = $8006;
  GL_MIN_EXT = $8007;
  GL_MAX_EXT = $8008;
  GL_BLEND_EQUATION_EXT = $8009;
var
  glBlendEquationEXT: procedure(mode: GLenum); extdecl;

function Load_GL_EXT_blend_minmax(): Boolean;

//**** GL_EXT_blend_subtract *****//
const
  GL_FUNC_SUBTRACT_EXT = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT = $800B;

function Load_GL_EXT_blend_subtract(): Boolean;

//**** GL_EXT_blend_logic_op *****//

function Load_GL_EXT_blend_logic_op(): Boolean;

//**** GL_SGIX_interlace *****//
const
  GL_INTERLACE_SGIX = $8094;

function Load_GL_SGIX_interlace(): Boolean;

//**** GL_SGIX_pixel_tiles *****//
const
  GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX = $813E;
  GL_PIXEL_TILE_CACHE_INCREMENT_SGIX = $813F;
  GL_PIXEL_TILE_WIDTH_SGIX = $8140;
  GL_PIXEL_TILE_HEIGHT_SGIX = $8141;
  GL_PIXEL_TILE_GRID_WIDTH_SGIX = $8142;
  GL_PIXEL_TILE_GRID_HEIGHT_SGIX = $8143;
  GL_PIXEL_TILE_GRID_DEPTH_SGIX = $8144;
  GL_PIXEL_TILE_CACHE_SIZE_SGIX = $8145;

function Load_GL_SGIX_pixel_tiles(): Boolean;

//**** GL_SGIS_texture_select *****//
const
  GL_DUAL_ALPHA4_SGIS = $8110;
  GL_DUAL_ALPHA8_SGIS = $8111;
  GL_DUAL_ALPHA12_SGIS = $8112;
  GL_DUAL_ALPHA16_SGIS = $8113;
  GL_DUAL_LUMINANCE4_SGIS = $8114;
  GL_DUAL_LUMINANCE8_SGIS = $8115;
  GL_DUAL_LUMINANCE12_SGIS = $8116;
  GL_DUAL_LUMINANCE16_SGIS = $8117;
  GL_DUAL_INTENSITY4_SGIS = $8118;
  GL_DUAL_INTENSITY8_SGIS = $8119;
  GL_DUAL_INTENSITY12_SGIS = $811A;
  GL_DUAL_INTENSITY16_SGIS = $811B;
  GL_DUAL_LUMINANCE_ALPHA4_SGIS = $811C;
  GL_DUAL_LUMINANCE_ALPHA8_SGIS = $811D;
  GL_QUAD_ALPHA4_SGIS = $811E;
  GL_QUAD_ALPHA8_SGIS = $811F;
  GL_QUAD_LUMINANCE4_SGIS = $8120;
  GL_QUAD_LUMINANCE8_SGIS = $8121;
  GL_QUAD_INTENSITY4_SGIS = $8122;
  GL_QUAD_INTENSITY8_SGIS = $8123;
  GL_DUAL_TEXTURE_SELECT_SGIS = $8124;
  GL_QUAD_TEXTURE_SELECT_SGIS = $8125;

function Load_GL_SGIS_texture_select(): Boolean;

//**** GL_SGIX_sprite *****//
const
  GL_SPRITE_SGIX = $8148;
  GL_SPRITE_MODE_SGIX = $8149;
  GL_SPRITE_AXIS_SGIX = $814A;
  GL_SPRITE_TRANSLATION_SGIX = $814B;
  GL_SPRITE_AXIAL_SGIX = $814C;
  GL_SPRITE_OBJECT_ALIGNED_SGIX = $814D;
  GL_SPRITE_EYE_ALIGNED_SGIX = $814E;
var
  glSpriteParameterfSGIX: procedure(pname: GLenum; param: GLfloat); extdecl;
  glSpriteParameterfvSGIX: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glSpriteParameteriSGIX: procedure(pname: GLenum; param: GLint); extdecl;
  glSpriteParameterivSGIX: procedure(pname: GLenum; const params: PGLint); extdecl;

function Load_GL_SGIX_sprite(): Boolean;

//**** GL_SGIX_texture_multi_buffer *****//
const
  GL_TEXTURE_MULTI_BUFFER_HINT_SGIX = $812E;

function Load_GL_SGIX_texture_multi_buffer(): Boolean;

//**** GL_EXT_point_parameters *****//
const
  GL_POINT_SIZE_MIN_EXT = $8126;
  GL_POINT_SIZE_MAX_EXT = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT = $8128;
  GL_DISTANCE_ATTENUATION_EXT = $8129;
var
  glPointParameterfEXT: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPointParameterfvEXT: procedure(pname: GLenum; const params: PGLfloat); extdecl;

function Load_GL_EXT_point_parameters(): Boolean;

//**** GL_SGIS_point_parameters *****//
const
  GL_POINT_SIZE_MIN_SGIS = $8126;
  GL_POINT_SIZE_MAX_SGIS = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_SGIS = $8128;
  GL_DISTANCE_ATTENUATION_SGIS = $8129;
var
  glPointParameterfSGIS: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPointParameterfvSGIS: procedure(pname: GLenum; const params: PGLfloat); extdecl;

function Load_GL_SGIS_point_parameters(): Boolean;

//**** GL_SGIX_instruments *****//
const
  GL_INSTRUMENT_BUFFER_POINTER_SGIX = $8180;
  GL_INSTRUMENT_MEASUREMENTS_SGIX = $8181;
var
  glGetInstrumentsSGIX: function(): GLint; extdecl;
  glInstrumentsBufferSGIX: procedure(size: GLsizei; buffer: PGLint); extdecl;
  glPollInstrumentsSGIX: function(marker_p: PGLint): GLint; extdecl;
  glReadInstrumentsSGIX: procedure(marker: GLint); extdecl;
  glStartInstrumentsSGIX: procedure(); extdecl;
  glStopInstrumentsSGIX: procedure(marker: GLint); extdecl;

function Load_GL_SGIX_instruments(): Boolean;

//**** GL_SGIX_texture_scale_bias *****//
const
  GL_POST_TEXTURE_FILTER_BIAS_SGIX = $8179;
  GL_POST_TEXTURE_FILTER_SCALE_SGIX = $817A;
  GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX = $817B;
  GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX = $817C;

function Load_GL_SGIX_texture_scale_bias(): Boolean;

//**** GL_SGIX_framezoom *****//
const
  GL_FRAMEZOOM_SGIX = $818B;
  GL_FRAMEZOOM_FACTOR_SGIX = $818C;
  GL_MAX_FRAMEZOOM_FACTOR_SGIX = $818D;
var
  glFrameZoomSGIX: procedure(factor: GLint); extdecl;

function Load_GL_SGIX_framezoom(): Boolean;

//**** GL_SGIX_tag_sample_buffer *****//
var
  glTagSampleBufferSGIX: procedure(); extdecl;

function Load_GL_SGIX_tag_sample_buffer(): Boolean;

//**** GL_FfdMaskSGIX *****//
const
  GL_TEXTURE_DEFORMATION_BIT_SGIX = $00000001;
  GL_GEOMETRY_DEFORMATION_BIT_SGIX = $00000002;

function Load_GL_FfdMaskSGIX(): Boolean;

//**** GL_SGIX_polynomial_ffd *****//
const
  GL_GEOMETRY_DEFORMATION_SGIX = $8194;
  GL_TEXTURE_DEFORMATION_SGIX = $8195;
  GL_DEFORMATIONS_MASK_SGIX = $8196;
  GL_MAX_DEFORMATION_ORDER_SGIX = $8197;
var
  glDeformationMap3dSGIX: procedure(target: GLenum; u1: GLdouble; u2: GLdouble; ustride: GLint; uorder: GLint; v1: GLdouble; v2: GLdouble; vstride: GLint; vorder: GLint; w1: GLdouble; w2: GLdouble; wstride: GLint; worder: GLint; const points: PGLdouble); extdecl;
  glDeformationMap3fSGIX: procedure(target: GLenum; u1: GLfloat; u2: GLfloat; ustride: GLint; uorder: GLint; v1: GLfloat; v2: GLfloat; vstride: GLint; vorder: GLint; w1: GLfloat; w2: GLfloat; wstride: GLint; worder: GLint; const points: PGLfloat); extdecl;
  glDeformSGIX: procedure(mask: GLbitfield); extdecl;
  glLoadIdentityDeformationMapSGIX: procedure(mask: GLbitfield); extdecl;

function Load_GL_SGIX_polynomial_ffd(): Boolean;

//**** GL_SGIX_reference_plane *****//
const
  GL_REFERENCE_PLANE_SGIX = $817D;
  GL_REFERENCE_PLANE_EQUATION_SGIX = $817E;
var
  glReferencePlaneSGIX: procedure(const equation: PGLdouble); extdecl;

function Load_GL_SGIX_reference_plane(): Boolean;

//**** GL_SGIX_flush_raster *****//
var
  glFlushRasterSGIX: procedure(); extdecl;

function Load_GL_SGIX_flush_raster(): Boolean;

//**** GL_SGIX_depth_texture *****//
const
  GL_DEPTH_COMPONENT16_SGIX = $81A5;
  GL_DEPTH_COMPONENT24_SGIX = $81A6;
  GL_DEPTH_COMPONENT32_SGIX = $81A7;

function Load_GL_SGIX_depth_texture(): Boolean;

//**** GL_SGIS_fog_function *****//
const
  GL_FOG_FUNC_SGIS = $812A;
  GL_FOG_FUNC_POINTS_SGIS = $812B;
  GL_MAX_FOG_FUNC_POINTS_SGIS = $812C;
var
  glFogFuncSGIS: procedure(n: GLsizei; const points: PGLfloat); extdecl;
  glGetFogFuncSGIS: procedure(points: PGLfloat); extdecl;

function Load_GL_SGIS_fog_function(): Boolean;

//**** GL_SGIX_fog_offset *****//
const
  GL_FOG_OFFSET_SGIX = $8198;
  GL_FOG_OFFSET_VALUE_SGIX = $8199;

function Load_GL_SGIX_fog_offset(): Boolean;

//**** GL_HP_image_transform *****//
const
  GL_IMAGE_SCALE_X_HP = $8155;
  GL_IMAGE_SCALE_Y_HP = $8156;
  GL_IMAGE_TRANSLATE_X_HP = $8157;
  GL_IMAGE_TRANSLATE_Y_HP = $8158;
  GL_IMAGE_ROTATE_ANGLE_HP = $8159;
  GL_IMAGE_ROTATE_ORIGIN_X_HP = $815A;
  GL_IMAGE_ROTATE_ORIGIN_Y_HP = $815B;
  GL_IMAGE_MAG_FILTER_HP = $815C;
  GL_IMAGE_MIN_FILTER_HP = $815D;
  GL_IMAGE_CUBIC_WEIGHT_HP = $815E;
  GL_CUBIC_HP = $815F;
  GL_AVERAGE_HP = $8160;
  GL_IMAGE_TRANSFORM_2D_HP = $8161;
  GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP = $8162;
  GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP = $8163;
var
  glImageTransformParameteriHP: procedure(target: GLenum; pname: GLenum; param: GLint); extdecl;
  glImageTransformParameterfHP: procedure(target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glImageTransformParameterivHP: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glImageTransformParameterfvHP: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glGetImageTransformParameterivHP: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetImageTransformParameterfvHP: procedure(target: GLenum; pname: GLenum; params: PGLfloat); extdecl;

function Load_GL_HP_image_transform(): Boolean;

//**** GL_HP_convolution_border_modes *****//
const
  GL_IGNORE_BORDER_HP = $8150;
  GL_CONSTANT_BORDER_HP = $8151;
  GL_REPLICATE_BORDER_HP = $8153;
  GL_CONVOLUTION_BORDER_COLOR_HP = $8154;

function Load_GL_HP_convolution_border_modes(): Boolean;

//**** GL_INGR_palette_buffer *****//

function Load_GL_INGR_palette_buffer(): Boolean;

//**** GL_SGIX_texture_add_env *****//
const
  GL_TEXTURE_ENV_BIAS_SGIX = $80BE;

function Load_GL_SGIX_texture_add_env(): Boolean;

//**** GL_EXT_color_subtable *****//
var
  glColorSubTableEXT: procedure(target: GLenum; start: GLsizei; count: GLsizei; format: GLenum; _type: GLenum; const data: PGLvoid); extdecl; (* Also used in GL_EXT_paletted_texture *)
  glCopyColorSubTableEXT: procedure(target: GLenum; start: GLsizei; x: GLint; y: GLint; width: GLsizei); extdecl;

function Load_GL_EXT_color_subtable(): Boolean;

//**** GL_PGI_vertex_hints *****//
const
  GL_VERTEX_DATA_HINT_PGI = $1A22A;
  GL_VERTEX_CONSISTENT_HINT_PGI = $1A22B;
  GL_MATERIAL_SIDE_HINT_PGI = $1A22C;
  GL_MAX_VERTEX_HINT_PGI = $1A22D;
  GL_COLOR3_BIT_PGI = $00010000;
  GL_COLOR4_BIT_PGI = $00020000;
  GL_EDGEFLAG_BIT_PGI = $00040000;
  GL_INDEX_BIT_PGI = $00080000;
  GL_MAT_AMBIENT_BIT_PGI = $00100000;
  GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI = $00200000;
  GL_MAT_DIFFUSE_BIT_PGI = $00400000;
  GL_MAT_EMISSION_BIT_PGI = $00800000;
  GL_MAT_COLOR_INDEXES_BIT_PGI = $01000000;
  GL_MAT_SHININESS_BIT_PGI = $02000000;
  GL_MAT_SPECULAR_BIT_PGI = $04000000;
  GL_NORMAL_BIT_PGI = $08000000;
  GL_TEXCOORD1_BIT_PGI = $10000000;
  GL_TEXCOORD2_BIT_PGI = $20000000;
  GL_TEXCOORD3_BIT_PGI = $40000000;
  GL_TEXCOORD4_BIT_PGI = $80000000;
  GL_VERTEX23_BIT_PGI = $00000004;
  GL_VERTEX4_BIT_PGI = $00000008;

function Load_GL_PGI_vertex_hints(): Boolean;

//**** GL_PGI_misc_hints *****//
const
  GL_PREFER_DOUBLEBUFFER_HINT_PGI = $1A1F8;
  GL_CONSERVE_MEMORY_HINT_PGI = $1A1FD;
  GL_RECLAIM_MEMORY_HINT_PGI = $1A1FE;
  GL_NATIVE_GRAPHICS_HANDLE_PGI = $1A202;
  GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI = $1A203;
  GL_NATIVE_GRAPHICS_END_HINT_PGI = $1A204;
  GL_ALWAYS_FAST_HINT_PGI = $1A20C;
  GL_ALWAYS_SOFT_HINT_PGI = $1A20D;
  GL_ALLOW_DRAW_OBJ_HINT_PGI = $1A20E;
  GL_ALLOW_DRAW_WIN_HINT_PGI = $1A20F;
  GL_ALLOW_DRAW_FRG_HINT_PGI = $1A210;
  GL_ALLOW_DRAW_MEM_HINT_PGI = $1A211;
  GL_STRICT_DEPTHFUNC_HINT_PGI = $1A216;
  GL_STRICT_LIGHTING_HINT_PGI = $1A217;
  GL_STRICT_SCISSOR_HINT_PGI = $1A218;
  GL_FULL_STIPPLE_HINT_PGI = $1A219;
  GL_CLIP_NEAR_HINT_PGI = $1A220;
  GL_CLIP_FAR_HINT_PGI = $1A221;
  GL_WIDE_LINE_HINT_PGI = $1A222;
  GL_BACK_NORMALS_HINT_PGI = $1A223;
var
  glHintPGI: procedure(target: GLenum; mode: GLint); extdecl;

function Load_GL_PGI_misc_hints(): Boolean;

//**** GL_EXT_paletted_texture *****//
const
  GL_COLOR_INDEX1_EXT = $80E2;
  GL_COLOR_INDEX2_EXT = $80E3;
  GL_COLOR_INDEX4_EXT = $80E4;
  GL_COLOR_INDEX8_EXT = $80E5;
  GL_COLOR_INDEX12_EXT = $80E6;
  GL_COLOR_INDEX16_EXT = $80E7;
  GL_TEXTURE_INDEX_SIZE_EXT = $80ED;
var
  glColorTableEXT: procedure(target: GLenum; internalFormat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const table: PGLvoid); extdecl;
  glGetColorTableEXT: procedure(target: GLenum; format: GLenum; _type: GLenum; data: PGLvoid); extdecl;
  glGetColorTableParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetColorTableParameterfvEXT: procedure(target: GLenum; pname: GLenum; params: PGLfloat); extdecl;

function Load_GL_EXT_paletted_texture(): Boolean;

//**** GL_EXT_clip_volume_hint *****//
const
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT = $80F0;

function Load_GL_EXT_clip_volume_hint(): Boolean;

//**** GL_SGIX_list_priority *****//
const
  GL_LIST_PRIORITY_SGIX = $8182;
var
  glGetListParameterfvSGIX: procedure(list: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetListParameterivSGIX: procedure(list: GLuint; pname: GLenum; params: PGLint); extdecl;
  glListParameterfSGIX: procedure(list: GLuint; pname: GLenum; param: GLfloat); extdecl;
  glListParameterfvSGIX: procedure(list: GLuint; pname: GLenum; const params: PGLfloat); extdecl;
  glListParameteriSGIX: procedure(list: GLuint; pname: GLenum; param: GLint); extdecl;
  glListParameterivSGIX: procedure(list: GLuint; pname: GLenum; const params: PGLint); extdecl;

function Load_GL_SGIX_list_priority(): Boolean;

//**** GL_SGIX_ir_instrument1 *****//
const
  GL_IR_INSTRUMENT1_SGIX = $817F;

function Load_GL_SGIX_ir_instrument1(): Boolean;

//**** GL_SGIX_calligraphic_fragment *****//
const
  GL_CALLIGRAPHIC_FRAGMENT_SGIX = $8183;

function Load_GL_SGIX_calligraphic_fragment(): Boolean;

//**** GL_SGIX_texture_lod_bias *****//
const
  GL_TEXTURE_LOD_BIAS_S_SGIX = $818E;
  GL_TEXTURE_LOD_BIAS_T_SGIX = $818F;
  GL_TEXTURE_LOD_BIAS_R_SGIX = $8190;

function Load_GL_SGIX_texture_lod_bias(): Boolean;

//**** GL_SGIX_shadow_ambient *****//
const
  GL_SHADOW_AMBIENT_SGIX = $80BF;

function Load_GL_SGIX_shadow_ambient(): Boolean;

//**** GL_EXT_index_texture *****//

function Load_GL_EXT_index_texture(): Boolean;

//**** GL_EXT_index_material *****//
const
  GL_INDEX_MATERIAL_EXT = $81B8;
  GL_INDEX_MATERIAL_PARAMETER_EXT = $81B9;
  GL_INDEX_MATERIAL_FACE_EXT = $81BA;
var
  glIndexMaterialEXT: procedure(face: GLenum; mode: GLenum); extdecl;

function Load_GL_EXT_index_material(): Boolean;

//**** GL_EXT_index_func *****//
const
  GL_INDEX_TEST_EXT = $81B5;
  GL_INDEX_TEST_FUNC_EXT = $81B6;
  GL_INDEX_TEST_REF_EXT = $81B7;
var
  glIndexFuncEXT: procedure(func: GLenum; ref: GLclampf); extdecl;

function Load_GL_EXT_index_func(): Boolean;

//**** GL_EXT_index_array_formats *****//
const
  GL_IUI_V2F_EXT = $81AD;
  GL_IUI_V3F_EXT = $81AE;
  GL_IUI_N3F_V2F_EXT = $81AF;
  GL_IUI_N3F_V3F_EXT = $81B0;
  GL_T2F_IUI_V2F_EXT = $81B1;
  GL_T2F_IUI_V3F_EXT = $81B2;
  GL_T2F_IUI_N3F_V2F_EXT = $81B3;
  GL_T2F_IUI_N3F_V3F_EXT = $81B4;

function Load_GL_EXT_index_array_formats(): Boolean;

//**** GL_EXT_compiled_vertex_array *****//
const
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT = $81A8;
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT = $81A9;
var
  glLockArraysEXT: procedure(first: GLint; count: GLsizei); extdecl;
  glUnlockArraysEXT: procedure(); extdecl;

function Load_GL_EXT_compiled_vertex_array(): Boolean;

//**** GL_EXT_cull_vertex *****//
const
  GL_CULL_VERTEX_EXT = $81AA;
  GL_CULL_VERTEX_EYE_POSITION_EXT = $81AB;
  GL_CULL_VERTEX_OBJECT_POSITION_EXT = $81AC;
var
  glCullParameterdvEXT: procedure(pname: GLenum; params: PGLdouble); extdecl;
  glCullParameterfvEXT: procedure(pname: GLenum; params: PGLfloat); extdecl;

function Load_GL_EXT_cull_vertex(): Boolean;

//**** GL_SGIX_ycrcb *****//
const
  GL_YCRCB_422_SGIX = $81BB;
  GL_YCRCB_444_SGIX = $81BC;

function Load_GL_SGIX_ycrcb(): Boolean;

//**** GL_SGIX_fragment_lighting *****//
const
  GL_FRAGMENT_LIGHTING_SGIX = $8400;
  GL_FRAGMENT_COLOR_MATERIAL_SGIX = $8401;
  GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX = $8402;
  GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX = $8403;
  GL_MAX_FRAGMENT_LIGHTS_SGIX = $8404;
  GL_MAX_ACTIVE_LIGHTS_SGIX = $8405;
  GL_CURRENT_RASTER_NORMAL_SGIX = $8406;
  GL_LIGHT_ENV_MODE_SGIX = $8407;
  GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX = $8408;
  GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX = $8409;
  GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX = $840A;
  GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX = $840B;
  GL_FRAGMENT_LIGHT0_SGIX = $840C;
  GL_FRAGMENT_LIGHT1_SGIX = $840D;
  GL_FRAGMENT_LIGHT2_SGIX = $840E;
  GL_FRAGMENT_LIGHT3_SGIX = $840F;
  GL_FRAGMENT_LIGHT4_SGIX = $8410;
  GL_FRAGMENT_LIGHT5_SGIX = $8411;
  GL_FRAGMENT_LIGHT6_SGIX = $8412;
  GL_FRAGMENT_LIGHT7_SGIX = $8413;
var
  glFragmentColorMaterialSGIX: procedure(face: GLenum; mode: GLenum); extdecl;
  glFragmentLightfSGIX: procedure(light: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glFragmentLightfvSGIX: procedure(light: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glFragmentLightiSGIX: procedure(light: GLenum; pname: GLenum; param: GLint); extdecl;
  glFragmentLightivSGIX: procedure(light: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glFragmentLightModelfSGIX: procedure(pname: GLenum; param: GLfloat); extdecl;
  glFragmentLightModelfvSGIX: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glFragmentLightModeliSGIX: procedure(pname: GLenum; param: GLint); extdecl;
  glFragmentLightModelivSGIX: procedure(pname: GLenum; const params: PGLint); extdecl;
  glFragmentMaterialfSGIX: procedure(face: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glFragmentMaterialfvSGIX: procedure(face: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glFragmentMaterialiSGIX: procedure(face: GLenum; pname: GLenum; param: GLint); extdecl;
  glFragmentMaterialivSGIX: procedure(face: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glGetFragmentLightfvSGIX: procedure(light: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetFragmentLightivSGIX: procedure(light: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetFragmentMaterialfvSGIX: procedure(face: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetFragmentMaterialivSGIX: procedure(face: GLenum; pname: GLenum; params: PGLint); extdecl;
  glLightEnviSGIX: procedure(pname: GLenum; param: GLint); extdecl;

function Load_GL_SGIX_fragment_lighting(): Boolean;

//**** GL_IBM_rasterpos_clip *****//
const
  GL_RASTER_POSITION_UNCLIPPED_IBM = $19262;

function Load_GL_IBM_rasterpos_clip(): Boolean;

//**** GL_HP_texture_lighting *****//
const
  GL_TEXTURE_LIGHTING_MODE_HP = $8167;
  GL_TEXTURE_POST_SPECULAR_HP = $8168;
  GL_TEXTURE_PRE_SPECULAR_HP = $8169;

function Load_GL_HP_texture_lighting(): Boolean;

//**** GL_EXT_draw_range_elements *****//
const
  GL_MAX_ELEMENTS_VERTICES_EXT = $80E8;
  GL_MAX_ELEMENTS_INDICES_EXT = $80E9;
var
  glDrawRangeElementsEXT: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid); extdecl;

function Load_GL_EXT_draw_range_elements(): Boolean;

//**** GL_WIN_phong_shading *****//
const
  GL_PHONG_WIN = $80EA;
  GL_PHONG_HINT_WIN = $80EB;

function Load_GL_WIN_phong_shading(): Boolean;

//**** GL_WIN_specular_fog *****//
const
  GL_FOG_SPECULAR_TEXTURE_WIN = $80EC;

function Load_GL_WIN_specular_fog(): Boolean;

//**** GL_EXT_light_texture *****//
const
  GL_FRAGMENT_MATERIAL_EXT = $8349;
  GL_FRAGMENT_NORMAL_EXT = $834A;
  GL_FRAGMENT_COLOR_EXT = $834C;
  GL_ATTENUATION_EXT = $834D;
  GL_SHADOW_ATTENUATION_EXT = $834E;
  GL_TEXTURE_APPLICATION_MODE_EXT = $834F;
  GL_TEXTURE_LIGHT_EXT = $8350;
  GL_TEXTURE_MATERIAL_FACE_EXT = $8351;
  GL_TEXTURE_MATERIAL_PARAMETER_EXT = $8352;
// reuse GL_FRAGMENT_DEPTH_EXT 
var
  glApplyTextureEXT: procedure(mode: GLenum); extdecl;
  glTextureLightEXT: procedure(pname: GLenum); extdecl;
  glTextureMaterialEXT: procedure(face: GLenum; mode: GLenum); extdecl;

function Load_GL_EXT_light_texture(): Boolean;

//**** GL_SGIX_blend_alpha_minmax *****//
const
  GL_ALPHA_MIN_SGIX = $8320;
  GL_ALPHA_MAX_SGIX = $8321;

function Load_GL_SGIX_blend_alpha_minmax(): Boolean;

//**** GL_SGIX_impact_pixel_texture *****//
const
  GL_PIXEL_TEX_GEN_Q_CEILING_SGIX = $8184;
  GL_PIXEL_TEX_GEN_Q_ROUND_SGIX = $8185;
  GL_PIXEL_TEX_GEN_Q_FLOOR_SGIX = $8186;
  GL_PIXEL_TEX_GEN_ALPHA_REPLACE_SGIX = $8187;
  GL_PIXEL_TEX_GEN_ALPHA_NO_REPLACE_SGIX = $8188;
  GL_PIXEL_TEX_GEN_ALPHA_LS_SGIX = $8189;
  GL_PIXEL_TEX_GEN_ALPHA_MS_SGIX = $818A;

function Load_GL_SGIX_impact_pixel_texture(): Boolean;

//**** GL_EXT_bgra *****//
const
  GL_BGR_EXT = $80E0;
  GL_BGRA_EXT = $80E1;

function Load_GL_EXT_bgra(): Boolean;

//**** GL_SGIX_async *****//
const
  GL_ASYNC_MARKER_SGIX = $8329;
var
  glAsyncMarkerSGIX: procedure(marker: GLuint); extdecl;
  glFinishAsyncSGIX: function(markerp: PGLuint): GLint; extdecl;
  glPollAsyncSGIX: function(markerp: PGLuint): GLint; extdecl;
  glGenAsyncMarkersSGIX: function(range: GLsizei): GLuint; extdecl;
  glDeleteAsyncMarkersSGIX: procedure(marker: GLuint; range: GLsizei); extdecl;
  glIsAsyncMarkerSGIX: function(marker: GLuint): GLboolean; extdecl;

function Load_GL_SGIX_async(): Boolean;

//**** GL_SGIX_async_pixel *****//
const
  GL_ASYNC_TEX_IMAGE_SGIX = $835C;
  GL_ASYNC_DRAW_PIXELS_SGIX = $835D;
  GL_ASYNC_READ_PIXELS_SGIX = $835E;
  GL_MAX_ASYNC_TEX_IMAGE_SGIX = $835F;
  GL_MAX_ASYNC_DRAW_PIXELS_SGIX = $8360;
  GL_MAX_ASYNC_READ_PIXELS_SGIX = $8361;

function Load_GL_SGIX_async_pixel(): Boolean;

//**** GL_SGIX_async_histogram *****//
const
  GL_ASYNC_HISTOGRAM_SGIX = $832C;
  GL_MAX_ASYNC_HISTOGRAM_SGIX = $832D;

function Load_GL_SGIX_async_histogram(): Boolean;

//**** GL_INTEL_texture_scissor *****//

function Load_GL_INTEL_texture_scissor(): Boolean;

//**** GL_INTEL_parallel_arrays *****//
const
  GL_PARALLEL_ARRAYS_INTEL = $83F4;
  GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL = $83F5;
  GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL = $83F6;
  GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL = $83F7;
  GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL = $83F8;
var
  glVertexPointervINTEL: procedure(size: GLint; _type: GLenum; const _pointer: PPGLvoid); extdecl;
  glNormalPointervINTEL: procedure(_type: GLenum; const _pointer: PPGLvoid); extdecl;
  glColorPointervINTEL: procedure(size: GLint; _type: GLenum; const _pointer: PPGLvoid); extdecl;
  glTexCoordPointervINTEL: procedure(size: GLint; _type: GLenum; const _pointer: PPGLvoid); extdecl;

function Load_GL_INTEL_parallel_arrays(): Boolean;

//**** GL_HP_occlusion_test *****//
const
  GL_OCCLUSION_TEST_HP = $8165;
  GL_OCCLUSION_TEST_RESULT_HP = $8166;

function Load_GL_HP_occlusion_test(): Boolean;

//**** GL_EXT_pixel_transform *****//
const
  GL_PIXEL_TRANSFORM_2D_EXT = $8330;
  GL_PIXEL_MAG_FILTER_EXT = $8331;
  GL_PIXEL_MIN_FILTER_EXT = $8332;
  GL_PIXEL_CUBIC_WEIGHT_EXT = $8333;
  GL_CUBIC_EXT = $8334;
  GL_AVERAGE_EXT = $8335;
  GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT = $8336;
  GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT = $8337;
  GL_PIXEL_TRANSFORM_2D_MATRIX_EXT = $8338;
var
  glPixelTransformParameteriEXT: procedure(target: GLenum; pname: GLenum; param: GLint); extdecl;
  glPixelTransformParameterfEXT: procedure(target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glPixelTransformParameterivEXT: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glPixelTransformParameterfvEXT: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;

function Load_GL_EXT_pixel_transform(): Boolean;

//**** GL_EXT_pixel_transform_color_table *****//

function Load_GL_EXT_pixel_transform_color_table(): Boolean;

//**** GL_EXT_shared_texture_palette *****//
const
  GL_SHARED_TEXTURE_PALETTE_EXT = $81FB;

function Load_GL_EXT_shared_texture_palette(): Boolean;

//**** GL_EXT_separate_specular_color *****//
const
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT = $81F8;
  GL_SINGLE_COLOR_EXT = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT = $81FA;

function Load_GL_EXT_separate_specular_color(): Boolean;

//**** GL_EXT_secondary_color *****//
const
  GL_COLOR_SUM_EXT = $8458;
  GL_CURRENT_SECONDARY_COLOR_EXT = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT = $845D;
  GL_SECONDARY_COLOR_ARRAY_EXT = $845E;
var
  glSecondaryColor3bEXT: procedure(red: GLbyte; green: GLbyte; blue: GLbyte); extdecl;
  glSecondaryColor3bvEXT: procedure(const v: PGLbyte); extdecl;
  glSecondaryColor3dEXT: procedure(red: GLdouble; green: GLdouble; blue: GLdouble); extdecl;
  glSecondaryColor3dvEXT: procedure(const v: PGLdouble); extdecl;
  glSecondaryColor3fEXT: procedure(red: GLfloat; green: GLfloat; blue: GLfloat); extdecl;
  glSecondaryColor3fvEXT: procedure(const v: PGLfloat); extdecl;
  glSecondaryColor3iEXT: procedure(red: GLint; green: GLint; blue: GLint); extdecl;
  glSecondaryColor3ivEXT: procedure(const v: PGLint); extdecl;
  glSecondaryColor3sEXT: procedure(red: GLshort; green: GLshort; blue: GLshort); extdecl;
  glSecondaryColor3svEXT: procedure(const v: PGLshort); extdecl;
  glSecondaryColor3ubEXT: procedure(red: GLubyte; green: GLubyte; blue: GLubyte); extdecl;
  glSecondaryColor3ubvEXT: procedure(const v: PGLubyte); extdecl;
  glSecondaryColor3uiEXT: procedure(red: GLuint; green: GLuint; blue: GLuint); extdecl;
  glSecondaryColor3uivEXT: procedure(const v: PGLuint); extdecl;
  glSecondaryColor3usEXT: procedure(red: GLushort; green: GLushort; blue: GLushort); extdecl;
  glSecondaryColor3usvEXT: procedure(const v: PGLushort); extdecl;
  glSecondaryColorPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;

function Load_GL_EXT_secondary_color(): Boolean;

//**** GL_EXT_texture_perturb_normal *****//
const
  GL_PERTURB_EXT = $85AE;
  GL_TEXTURE_NORMAL_EXT = $85AF;
var
  glTextureNormalEXT: procedure(mode: GLenum); extdecl;

function Load_GL_EXT_texture_perturb_normal(): Boolean;

//**** GL_EXT_multi_draw_arrays *****//
var
  glMultiDrawArraysEXT: procedure(mode: GLenum; first: PGLint; count: PGLsizei; primcount: GLsizei); extdecl;
  glMultiDrawElementsEXT: procedure(mode: GLenum; const count: PGLsizei; _type: GLenum; const indices: PPGLvoid; primcount: GLsizei); extdecl;

function Load_GL_EXT_multi_draw_arrays(): Boolean;

//**** GL_EXT_fog_coord *****//
const
  GL_FOG_COORDINATE_SOURCE_EXT = $8450;
  GL_FOG_COORDINATE_EXT = $8451;
  GL_FRAGMENT_DEPTH_EXT = $8452;
  GL_CURRENT_FOG_COORDINATE_EXT = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT = $8456;
  GL_FOG_COORDINATE_ARRAY_EXT = $8457;
var
  glFogCoordfEXT: procedure(coord: GLfloat); extdecl;
  glFogCoordfvEXT: procedure(const coord: PGLfloat); extdecl;
  glFogCoorddEXT: procedure(coord: GLdouble); extdecl;
  glFogCoorddvEXT: procedure(const coord: PGLdouble); extdecl;
  glFogCoordPointerEXT: procedure(_type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;

function Load_GL_EXT_fog_coord(): Boolean;

//**** GL_REND_screen_coordinates *****//
const
  GL_SCREEN_COORDINATES_REND = $8490;
  GL_INVERTED_SCREEN_W_REND = $8491;

function Load_GL_REND_screen_coordinates(): Boolean;

//**** GL_EXT_coordinate_frame *****//
const
  GL_TANGENT_ARRAY_EXT = $8439;
  GL_BINORMAL_ARRAY_EXT = $843A;
  GL_CURRENT_TANGENT_EXT = $843B;
  GL_CURRENT_BINORMAL_EXT = $843C;
  GL_TANGENT_ARRAY_TYPE_EXT = $843E;
  GL_TANGENT_ARRAY_STRIDE_EXT = $843F;
  GL_BINORMAL_ARRAY_TYPE_EXT = $8440;
  GL_BINORMAL_ARRAY_STRIDE_EXT = $8441;
  GL_TANGENT_ARRAY_POINTER_EXT = $8442;
  GL_BINORMAL_ARRAY_POINTER_EXT = $8443;
  GL_MAP1_TANGENT_EXT = $8444;
  GL_MAP2_TANGENT_EXT = $8445;
  GL_MAP1_BINORMAL_EXT = $8446;
  GL_MAP2_BINORMAL_EXT = $8447;
var
  glTangent3bEXT: procedure(tx: GLbyte; ty: GLbyte; tz: GLbyte); extdecl;
  glTangent3bvEXT: procedure(const v: PGLbyte); extdecl;
  glTangent3dEXT: procedure(tx: GLdouble; ty: GLdouble; tz: GLdouble); extdecl;
  glTangent3dvEXT: procedure(const v: PGLdouble); extdecl;
  glTangent3fEXT: procedure(tx: GLfloat; ty: GLfloat; tz: GLfloat); extdecl;
  glTangent3fvEXT: procedure(const v: PGLfloat); extdecl;
  glTangent3iEXT: procedure(tx: GLint; ty: GLint; tz: GLint); extdecl;
  glTangent3ivEXT: procedure(const v: PGLint); extdecl;
  glTangent3sEXT: procedure(tx: GLshort; ty: GLshort; tz: GLshort); extdecl;
  glTangent3svEXT: procedure(const v: PGLshort); extdecl;
  glBinormal3bEXT: procedure(bx: GLbyte; by: GLbyte; bz: GLbyte); extdecl;
  glBinormal3bvEXT: procedure(const v: PGLbyte); extdecl;
  glBinormal3dEXT: procedure(bx: GLdouble; by: GLdouble; bz: GLdouble); extdecl;
  glBinormal3dvEXT: procedure(const v: PGLdouble); extdecl;
  glBinormal3fEXT: procedure(bx: GLfloat; by: GLfloat; bz: GLfloat); extdecl;
  glBinormal3fvEXT: procedure(const v: PGLfloat); extdecl;
  glBinormal3iEXT: procedure(bx: GLint; by: GLint; bz: GLint); extdecl;
  glBinormal3ivEXT: procedure(const v: PGLint); extdecl;
  glBinormal3sEXT: procedure(bx: GLshort; by: GLshort; bz: GLshort); extdecl;
  glBinormal3svEXT: procedure(const v: PGLshort); extdecl;
  glTangentPointerEXT: procedure(_type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;
  glBinormalPointerEXT: procedure(_type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;

function Load_GL_EXT_coordinate_frame(): Boolean;

//**** GL_EXT_texture_env_combine *****//
const
  GL_COMBINE_EXT = $8570;
  GL_COMBINE_RGB_EXT = $8571;
  GL_COMBINE_ALPHA_EXT = $8572;
  GL_RGB_SCALE_EXT = $8573;
  GL_ADD_SIGNED_EXT = $8574;
  GL_INTERPOLATE_EXT = $8575;
  GL_CONSTANT_EXT = $8576;
  GL_PRIMARY_COLOR_EXT = $8577;
  GL_PREVIOUS_EXT = $8578;
  GL_SOURCE0_RGB_EXT = $8580;
  GL_SOURCE1_RGB_EXT = $8581;
  GL_SOURCE2_RGB_EXT = $8582;
  GL_SOURCE0_ALPHA_EXT = $8588;
  GL_SOURCE1_ALPHA_EXT = $8589;
  GL_SOURCE2_ALPHA_EXT = $858A;
  GL_OPERAND0_RGB_EXT = $8590;
  GL_OPERAND1_RGB_EXT = $8591;
  GL_OPERAND2_RGB_EXT = $8592;
  GL_OPERAND0_ALPHA_EXT = $8598;
  GL_OPERAND1_ALPHA_EXT = $8599;
  GL_OPERAND2_ALPHA_EXT = $859A;

function Load_GL_EXT_texture_env_combine(): Boolean;

//**** GL_APPLE_specular_vector *****//
const
  GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE = $85B0;

function Load_GL_APPLE_specular_vector(): Boolean;

//**** GL_APPLE_transform_hint *****//
const
  GL_TRANSFORM_HINT_APPLE = $85B1;

function Load_GL_APPLE_transform_hint(): Boolean;

//**** GL_SGIX_fog_scale *****//
const
  GL_FOG_SCALE_SGIX = $81FC;
  GL_FOG_SCALE_VALUE_SGIX = $81FD;

function Load_GL_SGIX_fog_scale(): Boolean;

//**** GL_SUNX_constant_data *****//
const
  GL_UNPACK_CONSTANT_DATA_SUNX = $81D5;
  GL_TEXTURE_CONSTANT_DATA_SUNX = $81D6;
var
  glFinishTextureSUNX: procedure(); extdecl;

function Load_GL_SUNX_constant_data(): Boolean;

//**** GL_SUN_global_alpha *****//
const
  GL_GLOBAL_ALPHA_SUN = $81D9;
  GL_GLOBAL_ALPHA_FACTOR_SUN = $81DA;
var
  glGlobalAlphaFactorbSUN: procedure(factor: GLbyte); extdecl;
  glGlobalAlphaFactorsSUN: procedure(factor: GLshort); extdecl;
  glGlobalAlphaFactoriSUN: procedure(factor: GLint); extdecl;
  glGlobalAlphaFactorfSUN: procedure(factor: GLfloat); extdecl;
  glGlobalAlphaFactordSUN: procedure(factor: GLdouble); extdecl;
  glGlobalAlphaFactorubSUN: procedure(factor: GLubyte); extdecl;
  glGlobalAlphaFactorusSUN: procedure(factor: GLushort); extdecl;
  glGlobalAlphaFactoruiSUN: procedure(factor: GLuint); extdecl;

function Load_GL_SUN_global_alpha(): Boolean;

//**** GL_SUN_triangle_list *****//
const
  GL_RESTART_SUN = $0001;
  GL_REPLACE_MIDDLE_SUN = $0002;
  GL_REPLACE_OLDEST_SUN = $0003;
  GL_TRIANGLE_LIST_SUN = $81D7;
  GL_REPLACEMENT_CODE_SUN = $81D8;
  GL_REPLACEMENT_CODE_ARRAY_SUN = $85C0;
  GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN = $85C1;
  GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN = $85C2;
  GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN = $85C3;
  GL_R1UI_V3F_SUN = $85C4;
  GL_R1UI_C4UB_V3F_SUN = $85C5;
  GL_R1UI_C3F_V3F_SUN = $85C6;
  GL_R1UI_N3F_V3F_SUN = $85C7;
  GL_R1UI_C4F_N3F_V3F_SUN = $85C8;
  GL_R1UI_T2F_V3F_SUN = $85C9;
  GL_R1UI_T2F_N3F_V3F_SUN = $85CA;
  GL_R1UI_T2F_C4F_N3F_V3F_SUN = $85CB;
var
  glReplacementCodeuiSUN: procedure(code: GLuint); extdecl;
  glReplacementCodeusSUN: procedure(code: GLushort); extdecl;
  glReplacementCodeubSUN: procedure(code: GLubyte); extdecl;
  glReplacementCodeuivSUN: procedure(const code: PGLuint); extdecl;
  glReplacementCodeusvSUN: procedure(const code: PGLushort); extdecl;
  glReplacementCodeubvSUN: procedure(const code: PGLubyte); extdecl;
  glReplacementCodePointerSUN: procedure(_type: GLenum; stride: GLsizei; const _pointer: PPGLvoid); extdecl;

function Load_GL_SUN_triangle_list(): Boolean;

//**** GL_SUN_vertex *****//
var
  glColor4ubVertex2fSUN: procedure(r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat); extdecl;
  glColor4ubVertex2fvSUN: procedure(const c: PGLubyte; const v: PGLfloat); extdecl;
  glColor4ubVertex3fSUN: procedure(r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glColor4ubVertex3fvSUN: procedure(const c: PGLubyte; const v: PGLfloat); extdecl;
  glColor3fVertex3fSUN: procedure(r: GLfloat; g: GLfloat; b: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glColor3fVertex3fvSUN: procedure(const c: PGLfloat; const v: PGLfloat); extdecl;
  glNormal3fVertex3fSUN: procedure(nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glNormal3fVertex3fvSUN: procedure(const n: PGLfloat; const v: PGLfloat); extdecl;
  glColor4fNormal3fVertex3fSUN: procedure(r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glColor4fNormal3fVertex3fvSUN: procedure(const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord2fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fVertex3fvSUN: procedure(const tc: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord4fVertex4fSUN: procedure(s: GLfloat; t: GLfloat; p: GLfloat; q: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glTexCoord4fVertex4fvSUN: procedure(const tc: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord2fColor4ubVertex3fSUN: procedure(s: GLfloat; t: GLfloat; r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fColor4ubVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLubyte; const v: PGLfloat); extdecl;
  glTexCoord2fColor3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fColor3fVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord2fNormal3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fNormal3fVertex3fvSUN: procedure(const tc: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord2fColor4fNormal3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord4fColor4fNormal3fVertex4fSUN: procedure(s: GLfloat; t: GLfloat; p: GLfloat; q: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glTexCoord4fColor4fNormal3fVertex4fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiVertex3fSUN: procedure(rc: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiVertex3fvSUN: procedure(const rc: PGLuint; const v: PGLfloat); extdecl;
  glReplacementCodeuiColor4ubVertex3fSUN: procedure(rc: GLuint; r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiColor4ubVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLubyte; const v: PGLfloat); extdecl;
  glReplacementCodeuiColor3fVertex3fSUN: procedure(rc: GLuint; r: GLfloat; g: GLfloat; b: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiColor3fVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiNormal3fVertex3fSUN: procedure(rc: GLuint; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const n: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiColor4fNormal3fVertex3fSUN: procedure(rc: GLuint; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiTexCoord2fVertex3fSUN: procedure(rc: GLuint; s: GLfloat; t: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiTexCoord2fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN: procedure(rc: GLuint; s: GLfloat; t: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN: procedure(rc: GLuint; s: GLfloat; t: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;

function Load_GL_SUN_vertex(): Boolean;

//**** GL_EXT_blend_func_separate *****//
const
  GL_BLEND_DST_RGB_EXT = $80C8;
  GL_BLEND_SRC_RGB_EXT = $80C9;
  GL_BLEND_DST_ALPHA_EXT = $80CA;
  GL_BLEND_SRC_ALPHA_EXT = $80CB;
var
  glBlendFuncSeparateEXT: procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); extdecl;

function Load_GL_EXT_blend_func_separate(): Boolean;

//**** GL_INGR_color_clamp *****//
const
  GL_RED_MIN_CLAMP_INGR = $8560;
  GL_GREEN_MIN_CLAMP_INGR = $8561;
  GL_BLUE_MIN_CLAMP_INGR = $8562;
  GL_ALPHA_MIN_CLAMP_INGR = $8563;
  GL_RED_MAX_CLAMP_INGR = $8564;
  GL_GREEN_MAX_CLAMP_INGR = $8565;
  GL_BLUE_MAX_CLAMP_INGR = $8566;
  GL_ALPHA_MAX_CLAMP_INGR = $8567;

function Load_GL_INGR_color_clamp(): Boolean;

//**** GL_INGR_interlace_read *****//
const
  GL_INTERLACE_READ_INGR = $8568;

function Load_GL_INGR_interlace_read(): Boolean;

//**** GL_EXT_stencil_wrap *****//
const
  GL_INCR_WRAP_EXT = $8507;
  GL_DECR_WRAP_EXT = $8508;

function Load_GL_EXT_stencil_wrap(): Boolean;

//**** GL_EXT_422_pixels *****//
const
  GL_422_EXT = $80CC;
  GL_422_REV_EXT = $80CD;
  GL_422_AVERAGE_EXT = $80CE;
  GL_422_REV_AVERAGE_EXT = $80CF;

function Load_GL_EXT_422_pixels(): Boolean;

//**** GL_NV_texgen_reflection *****//
const
  GL_NORMAL_MAP_NV = $8511;
  GL_REFLECTION_MAP_NV = $8512;

function Load_GL_NV_texgen_reflection(): Boolean;

//**** GL_EXT_texture_cube_map *****//
const
  GL_NORMAL_MAP_EXT = $8511;
  GL_REFLECTION_MAP_EXT = $8512;
  GL_TEXTURE_CUBE_MAP_EXT = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_EXT = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_EXT = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT = $851C;

function Load_GL_EXT_texture_cube_map(): Boolean;

//**** GL_SUN_convolution_border_modes *****//
const
  GL_WRAP_BORDER_SUN = $81D4;

function Load_GL_SUN_convolution_border_modes(): Boolean;

//**** GL_EXT_texture_env_add *****//

function Load_GL_EXT_texture_env_add(): Boolean;

//**** GL_EXT_texture_lod_bias *****//
const
  GL_MAX_TEXTURE_LOD_BIAS_EXT = $84FD;
  GL_TEXTURE_FILTER_CONTROL_EXT = $8500;
  GL_TEXTURE_LOD_BIAS_EXT = $8501;

function Load_GL_EXT_texture_lod_bias(): Boolean;

//**** GL_EXT_texture_filter_anisotropic *****//
const
  GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;

function Load_GL_EXT_texture_filter_anisotropic(): Boolean;

//**** GL_EXT_vertex_weighting *****//
const
  GL_MODELVIEW0_STACK_DEPTH_EXT = GL_MODELVIEW_STACK_DEPTH;
  GL_MODELVIEW1_STACK_DEPTH_EXT = $8502;
  GL_MODELVIEW0_MATRIX_EXT = GL_MODELVIEW_MATRIX;
  GL_MODELVIEW1_MATRIX_EXT = $8506;
  GL_VERTEX_WEIGHTING_EXT = $8509;
  GL_MODELVIEW0_EXT = GL_MODELVIEW;
  GL_MODELVIEW1_EXT = $850A;
  GL_CURRENT_VERTEX_WEIGHT_EXT = $850B;
  GL_VERTEX_WEIGHT_ARRAY_EXT = $850C;
  GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT = $850D;
  GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT = $850E;
  GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT = $850F;
  GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT = $8510;
var
  glVertexWeightfEXT: procedure(weight: GLfloat); extdecl;
  glVertexWeightfvEXT: procedure(const weight: PGLfloat); extdecl;
  glVertexWeightPointerEXT: procedure(size: GLsizei; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;

function Load_GL_EXT_vertex_weighting(): Boolean;

//**** GL_NV_light_max_exponent *****//
const
  GL_MAX_SHININESS_NV = $8504;
  GL_MAX_SPOT_EXPONENT_NV = $8505;

function Load_GL_NV_light_max_exponent(): Boolean;

//**** GL_NV_vertex_array_range *****//
const
  GL_VERTEX_ARRAY_RANGE_NV = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV = $851E;
  GL_VERTEX_ARRAY_RANGE_VALID_NV = $851F;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_NV = $8521;
var
  glFlushVertexArrayRangeNV: procedure(); extdecl;
  glVertexArrayRangeNV: procedure(length: GLsizei; const _pointer: PGLvoid); extdecl;

function Load_GL_NV_vertex_array_range(): Boolean;

//**** GL_NV_register_combiners *****//
const
  GL_REGISTER_COMBINERS_NV = $8522;
  GL_VARIABLE_A_NV = $8523;
  GL_VARIABLE_B_NV = $8524;
  GL_VARIABLE_C_NV = $8525;
  GL_VARIABLE_D_NV = $8526;
  GL_VARIABLE_E_NV = $8527;
  GL_VARIABLE_F_NV = $8528;
  GL_VARIABLE_G_NV = $8529;
  GL_CONSTANT_COLOR0_NV = $852A;
  GL_CONSTANT_COLOR1_NV = $852B;
  GL_PRIMARY_COLOR_NV = $852C;
  GL_SECONDARY_COLOR_NV = $852D;
  GL_SPARE0_NV = $852E;
  GL_SPARE1_NV = $852F;
  GL_DISCARD_NV = $8530;
  GL_E_TIMES_F_NV = $8531;
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV = $8532;
  GL_UNSIGNED_IDENTITY_NV = $8536;
  GL_UNSIGNED_INVERT_NV = $8537;
  GL_EXPAND_NORMAL_NV = $8538;
  GL_EXPAND_NEGATE_NV = $8539;
  GL_HALF_BIAS_NORMAL_NV = $853A;
  GL_HALF_BIAS_NEGATE_NV = $853B;
  GL_SIGNED_IDENTITY_NV = $853C;
  GL_SIGNED_NEGATE_NV = $853D;
  GL_SCALE_BY_TWO_NV = $853E;
  GL_SCALE_BY_FOUR_NV = $853F;
  GL_SCALE_BY_ONE_HALF_NV = $8540;
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV = $8541;
  GL_COMBINER_INPUT_NV = $8542;
  GL_COMBINER_MAPPING_NV = $8543;
  GL_COMBINER_COMPONENT_USAGE_NV = $8544;
  GL_COMBINER_AB_DOT_PRODUCT_NV = $8545;
  GL_COMBINER_CD_DOT_PRODUCT_NV = $8546;
  GL_COMBINER_MUX_SUM_NV = $8547;
  GL_COMBINER_SCALE_NV = $8548;
  GL_COMBINER_BIAS_NV = $8549;
  GL_COMBINER_AB_OUTPUT_NV = $854A;
  GL_COMBINER_CD_OUTPUT_NV = $854B;
  GL_COMBINER_SUM_OUTPUT_NV = $854C;
  GL_MAX_GENERAL_COMBINERS_NV = $854D;
  GL_NUM_GENERAL_COMBINERS_NV = $854E;
  GL_COLOR_SUM_CLAMP_NV = $854F;
  GL_COMBINER0_NV = $8550;
  GL_COMBINER1_NV = $8551;
  GL_COMBINER2_NV = $8552;
  GL_COMBINER3_NV = $8553;
  GL_COMBINER4_NV = $8554;
  GL_COMBINER5_NV = $8555;
  GL_COMBINER6_NV = $8556;
  GL_COMBINER7_NV = $8557;
// reuse GL_TEXTURE0_ARB 
// reuse GL_TEXTURE1_ARB 
// reuse GL_ZERO 
// reuse GL_NONE 
// reuse GL_FOG 
var
  glCombinerParameterfvNV: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glCombinerParameterfNV: procedure(pname: GLenum; param: GLfloat); extdecl;
  glCombinerParameterivNV: procedure(pname: GLenum; const params: PGLint); extdecl;
  glCombinerParameteriNV: procedure(pname: GLenum; param: GLint); extdecl;
  glCombinerInputNV: procedure(stage: GLenum; portion: GLenum; variable: GLenum; input: GLenum; mapping: GLenum; componentUsage: GLenum); extdecl;
  glCombinerOutputNV: procedure(stage: GLenum; portion: GLenum; abOutput: GLenum; cdOutput: GLenum; sumOutput: GLenum; scale: GLenum; bias: GLenum; abDotProduct: GLboolean; cdDotProduct: GLboolean; muxSum: GLboolean); extdecl;
  glFinalCombinerInputNV: procedure(variable: GLenum; input: GLenum; mapping: GLenum; componentUsage: GLenum); extdecl;
  glGetCombinerInputParameterfvNV: procedure(stage: GLenum; portion: GLenum; variable: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetCombinerInputParameterivNV: procedure(stage: GLenum; portion: GLenum; variable: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetCombinerOutputParameterfvNV: procedure(stage: GLenum; portion: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetCombinerOutputParameterivNV: procedure(stage: GLenum; portion: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetFinalCombinerInputParameterfvNV: procedure(variable: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetFinalCombinerInputParameterivNV: procedure(variable: GLenum; pname: GLenum; params: PGLint); extdecl;

function Load_GL_NV_register_combiners(): Boolean;

//**** GL_NV_fog_distance *****//
const
  GL_FOG_DISTANCE_MODE_NV = $855A;
  GL_EYE_RADIAL_NV = $855B;
  GL_EYE_PLANE_ABSOLUTE_NV = $855C;
// reuse GL_EYE_PLANE 

function Load_GL_NV_fog_distance(): Boolean;

//**** GL_NV_texgen_emboss *****//
const
  GL_EMBOSS_LIGHT_NV = $855D;
  GL_EMBOSS_CONSTANT_NV = $855E;
  GL_EMBOSS_MAP_NV = $855F;

function Load_GL_NV_texgen_emboss(): Boolean;

//**** GL_NV_blend_square *****//

function Load_GL_NV_blend_square(): Boolean;

//**** GL_NV_texture_env_combine4 *****//
const
  GL_COMBINE4_NV = $8503;
  GL_SOURCE3_RGB_NV = $8583;
  GL_SOURCE3_ALPHA_NV = $858B;
  GL_OPERAND3_RGB_NV = $8593;
  GL_OPERAND3_ALPHA_NV = $859B;

function Load_GL_NV_texture_env_combine4(): Boolean;

//**** GL_MESA_resize_buffers *****//
var
  glResizeBuffersMESA: procedure(); extdecl;

function Load_GL_MESA_resize_buffers(): Boolean;

//**** GL_MESA_window_pos *****//
var
  glWindowPos2dMESA: procedure(x: GLdouble; y: GLdouble); extdecl;
  glWindowPos2dvMESA: procedure(const v: PGLdouble); extdecl;
  glWindowPos2fMESA: procedure(x: GLfloat; y: GLfloat); extdecl;
  glWindowPos2fvMESA: procedure(const v: PGLfloat); extdecl;
  glWindowPos2iMESA: procedure(x: GLint; y: GLint); extdecl;
  glWindowPos2ivMESA: procedure(const v: PGLint); extdecl;
  glWindowPos2sMESA: procedure(x: GLshort; y: GLshort); extdecl;
  glWindowPos2svMESA: procedure(const v: PGLshort); extdecl;
  glWindowPos3dMESA: procedure(x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glWindowPos3dvMESA: procedure(const v: PGLdouble); extdecl;
  glWindowPos3fMESA: procedure(x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glWindowPos3fvMESA: procedure(const v: PGLfloat); extdecl;
  glWindowPos3iMESA: procedure(x: GLint; y: GLint; z: GLint); extdecl;
  glWindowPos3ivMESA: procedure(const v: PGLint); extdecl;
  glWindowPos3sMESA: procedure(x: GLshort; y: GLshort; z: GLshort); extdecl;
  glWindowPos3svMESA: procedure(const v: PGLshort); extdecl;
  glWindowPos4dMESA: procedure(x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glWindowPos4dvMESA: procedure(const v: PGLdouble); extdecl;
  glWindowPos4fMESA: procedure(x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glWindowPos4fvMESA: procedure(const v: PGLfloat); extdecl;
  glWindowPos4iMESA: procedure(x: GLint; y: GLint; z: GLint; w: GLint); extdecl;
  glWindowPos4ivMESA: procedure(const v: PGLint); extdecl;
  glWindowPos4sMESA: procedure(x: GLshort; y: GLshort; z: GLshort; w: GLshort); extdecl;
  glWindowPos4svMESA: procedure(const v: PGLshort); extdecl;

function Load_GL_MESA_window_pos(): Boolean;

//**** GL_EXT_texture_compression_s3tc *****//
const
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;

function Load_GL_EXT_texture_compression_s3tc(): Boolean;

//**** GL_IBM_cull_vertex *****//
const
  GL_CULL_VERTEX_IBM = 103050;

function Load_GL_IBM_cull_vertex(): Boolean;

//**** GL_IBM_multimode_draw_arrays *****//
var
  glMultiModeDrawArraysIBM: procedure(const mode: PGLenum; const first: PGLint; const count: PGLsizei; primcount: GLsizei; modestride: GLint); extdecl;
  glMultiModeDrawElementsIBM: procedure(const mode: PGLenum; const count: PGLsizei; _type: GLenum; const indices: PPGLvoid; primcount: GLsizei; modestride: GLint); extdecl;

function Load_GL_IBM_multimode_draw_arrays(): Boolean;

//**** GL_IBM_vertex_array_lists *****//
const
  GL_VERTEX_ARRAY_LIST_IBM = 103070;
  GL_NORMAL_ARRAY_LIST_IBM = 103071;
  GL_COLOR_ARRAY_LIST_IBM = 103072;
  GL_INDEX_ARRAY_LIST_IBM = 103073;
  GL_TEXTURE_COORD_ARRAY_LIST_IBM = 103074;
  GL_EDGE_FLAG_ARRAY_LIST_IBM = 103075;
  GL_FOG_COORDINATE_ARRAY_LIST_IBM = 103076;
  GL_SECONDARY_COLOR_ARRAY_LIST_IBM = 103077;
  GL_VERTEX_ARRAY_LIST_STRIDE_IBM = 103080;
  GL_NORMAL_ARRAY_LIST_STRIDE_IBM = 103081;
  GL_COLOR_ARRAY_LIST_STRIDE_IBM = 103082;
  GL_INDEX_ARRAY_LIST_STRIDE_IBM = 103083;
  GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM = 103084;
  GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM = 103085;
  GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM = 103086;
  GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM = 103087;
var
  glColorPointerListIBM: procedure(size: GLint; _type: GLenum; stride: GLint; const _pointer: PPGLvoid; ptrstride: GLint); extdecl;
  glSecondaryColorPointerListIBM: procedure(size: GLint; _type: GLenum; stride: GLint; const _pointer: PPGLvoid; ptrstride: GLint); extdecl;
  glEdgeFlagPointerListIBM: procedure(stride: GLint; const _pointer: PPGLboolean; ptrstride: GLint); extdecl;
  glFogCoordPointerListIBM: procedure(_type: GLenum; stride: GLint; const _pointer: PPGLvoid; ptrstride: GLint); extdecl;
  glIndexPointerListIBM: procedure(_type: GLenum; stride: GLint; const _pointer: PPGLvoid; ptrstride: GLint); extdecl;
  glNormalPointerListIBM: procedure(_type: GLenum; stride: GLint; const _pointer: PPGLvoid; ptrstride: GLint); extdecl;
  glTexCoordPointerListIBM: procedure(size: GLint; _type: GLenum; stride: GLint; const _pointer: PPGLvoid; ptrstride: GLint); extdecl;
  glVertexPointerListIBM: procedure(size: GLint; _type: GLenum; stride: GLint; const _pointer: PPGLvoid; ptrstride: GLint); extdecl;

function Load_GL_IBM_vertex_array_lists(): Boolean;

//**** GL_SGIX_subsample *****//
const
  GL_PACK_SUBSAMPLE_RATE_SGIX = $85A0;
  GL_UNPACK_SUBSAMPLE_RATE_SGIX = $85A1;
  GL_PIXEL_SUBSAMPLE_4444_SGIX = $85A2;
  GL_PIXEL_SUBSAMPLE_2424_SGIX = $85A3;
  GL_PIXEL_SUBSAMPLE_4242_SGIX = $85A4;

function Load_GL_SGIX_subsample(): Boolean;

//**** GL_SGIX_ycrcb_subsample *****//

function Load_GL_SGIX_ycrcb_subsample(): Boolean;

//**** GL_SGIX_ycrcba *****//
const
  GL_YCRCB_SGIX = $8318;
  GL_YCRCBA_SGIX = $8319;

function Load_GL_SGIX_ycrcba(): Boolean;

//**** GL_SGI_depth_pass_instrument *****//
const
  GL_DEPTH_PASS_INSTRUMENT_SGIX = $8310;
  GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX = $8311;
  GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX = $8312;

function Load_GL_SGI_depth_pass_instrument(): Boolean;

//**** GL_3DFX_texture_compression_FXT1 *****//
const
  GL_COMPRESSED_RGB_FXT1_3DFX = $86B0;
  GL_COMPRESSED_RGBA_FXT1_3DFX = $86B1;

function Load_GL_3DFX_texture_compression_FXT1(): Boolean;

//**** GL_3DFX_multisample *****//
const
  GL_MULTISAMPLE_3DFX = $86B2;
  GL_SAMPLE_BUFFERS_3DFX = $86B3;
  GL_SAMPLES_3DFX = $86B4;
  GL_MULTISAMPLE_BIT_3DFX = $20000000;

function Load_GL_3DFX_multisample(): Boolean;

//**** GL_3DFX_tbuffer *****//
var
  glTbufferMask3DFX: procedure(mask: GLuint); extdecl;

function Load_GL_3DFX_tbuffer(): Boolean;

//**** GL_EXT_multisample *****//
const
  GL_MULTISAMPLE_EXT = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_EXT = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_EXT = $809F;
  GL_SAMPLE_MASK_EXT = $80A0;
  GL_1PASS_EXT = $80A1;
  GL_2PASS_0_EXT = $80A2;
  GL_2PASS_1_EXT = $80A3;
  GL_4PASS_0_EXT = $80A4;
  GL_4PASS_1_EXT = $80A5;
  GL_4PASS_2_EXT = $80A6;
  GL_4PASS_3_EXT = $80A7;
  GL_SAMPLE_BUFFERS_EXT = $80A8;
  GL_SAMPLES_EXT = $80A9;
  GL_SAMPLE_MASK_VALUE_EXT = $80AA;
  GL_SAMPLE_MASK_INVERT_EXT = $80AB;
  GL_SAMPLE_PATTERN_EXT = $80AC;
  GL_MULTISAMPLE_BIT_EXT = $20000000;
var
  glSampleMaskEXT: procedure(value: GLclampf; invert: GLboolean); extdecl;
  glSamplePatternEXT: procedure(pattern: GLenum); extdecl;

function Load_GL_EXT_multisample(): Boolean;

//**** GL_SGIX_vertex_preclip *****//
const
  GL_VERTEX_PRECLIP_SGIX = $83EE;
  GL_VERTEX_PRECLIP_HINT_SGIX = $83EF;

function Load_GL_SGIX_vertex_preclip(): Boolean;

//**** GL_SGIX_convolution_accuracy *****//
const
  GL_CONVOLUTION_HINT_SGIX = $8316;

function Load_GL_SGIX_convolution_accuracy(): Boolean;

//**** GL_SGIX_resample *****//
const
  GL_PACK_RESAMPLE_SGIX = $842C;
  GL_UNPACK_RESAMPLE_SGIX = $842D;
  GL_RESAMPLE_REPLICATE_SGIX = $842E;
  GL_RESAMPLE_ZERO_FILL_SGIX = $842F;
  GL_RESAMPLE_DECIMATE_SGIX = $8430;

function Load_GL_SGIX_resample(): Boolean;

//**** GL_SGIS_point_line_texgen *****//
const
  GL_EYE_DISTANCE_TO_POINT_SGIS = $81F0;
  GL_OBJECT_DISTANCE_TO_POINT_SGIS = $81F1;
  GL_EYE_DISTANCE_TO_LINE_SGIS = $81F2;
  GL_OBJECT_DISTANCE_TO_LINE_SGIS = $81F3;
  GL_EYE_POINT_SGIS = $81F4;
  GL_OBJECT_POINT_SGIS = $81F5;
  GL_EYE_LINE_SGIS = $81F6;
  GL_OBJECT_LINE_SGIS = $81F7;

function Load_GL_SGIS_point_line_texgen(): Boolean;

//**** GL_SGIS_texture_color_mask *****//
const
  GL_TEXTURE_COLOR_WRITEMASK_SGIS = $81EF;
var
  glTextureColorMaskSGIS: procedure(red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean); extdecl;

function Load_GL_SGIS_texture_color_mask(): Boolean;

//**** GL_EXT_texture_env_dot3 *****//
const
  GL_DOT3_RGB_EXT = $8740;
  GL_DOT3_RGBA_EXT = $8741;

function Load_GL_EXT_texture_env_dot3(): Boolean;

//**** GL_ATI_texture_mirror_once *****//
const
  GL_MIRROR_CLAMP_ATI = $8742;
  GL_MIRROR_CLAMP_TO_EDGE_ATI = $8743;

function Load_GL_ATI_texture_mirror_once(): Boolean;

//**** GL_NV_fence *****//
const
  GL_ALL_COMPLETED_NV = $84F2;
  GL_FENCE_STATUS_NV = $84F3;
  GL_FENCE_CONDITION_NV = $84F4;
var
  glDeleteFencesNV: procedure(n: GLsizei; const fences: PGLuint); extdecl;
  glGenFencesNV: procedure(n: GLsizei; fences: PGLuint); extdecl;
  glIsFenceNV: function(fence: GLuint): GLboolean; extdecl;
  glTestFenceNV: function(fence: GLuint): GLboolean; extdecl;
  glGetFenceivNV: procedure(fence: GLuint; pname: GLenum; params: PGLint); extdecl;
  glFinishFenceNV: procedure(fence: GLuint); extdecl;
  glSetFenceNV: procedure(fence: GLuint; condition: GLenum); extdecl;

function Load_GL_NV_fence(): Boolean;

//**** GL_IBM_texture_mirrored_repeat *****//
const
  GL_MIRRORED_REPEAT_IBM = $8370;

function Load_GL_IBM_texture_mirrored_repeat(): Boolean;

//**** GL_NV_evaluators *****//
const
  GL_EVAL_2D_NV = $86C0;
  GL_EVAL_TRIANGULAR_2D_NV = $86C1;
  GL_MAP_TESSELLATION_NV = $86C2;
  GL_MAP_ATTRIB_U_ORDER_NV = $86C3;
  GL_MAP_ATTRIB_V_ORDER_NV = $86C4;
  GL_EVAL_FRACTIONAL_TESSELLATION_NV = $86C5;
  GL_EVAL_VERTEX_ATTRIB0_NV = $86C6;
  GL_EVAL_VERTEX_ATTRIB1_NV = $86C7;
  GL_EVAL_VERTEX_ATTRIB2_NV = $86C8;
  GL_EVAL_VERTEX_ATTRIB3_NV = $86C9;
  GL_EVAL_VERTEX_ATTRIB4_NV = $86CA;
  GL_EVAL_VERTEX_ATTRIB5_NV = $86CB;
  GL_EVAL_VERTEX_ATTRIB6_NV = $86CC;
  GL_EVAL_VERTEX_ATTRIB7_NV = $86CD;
  GL_EVAL_VERTEX_ATTRIB8_NV = $86CE;
  GL_EVAL_VERTEX_ATTRIB9_NV = $86CF;
  GL_EVAL_VERTEX_ATTRIB10_NV = $86D0;
  GL_EVAL_VERTEX_ATTRIB11_NV = $86D1;
  GL_EVAL_VERTEX_ATTRIB12_NV = $86D2;
  GL_EVAL_VERTEX_ATTRIB13_NV = $86D3;
  GL_EVAL_VERTEX_ATTRIB14_NV = $86D4;
  GL_EVAL_VERTEX_ATTRIB15_NV = $86D5;
  GL_MAX_MAP_TESSELLATION_NV = $86D6;
  GL_MAX_RATIONAL_EVAL_ORDER_NV = $86D7;
var
  glMapControlPointsNV: procedure(target: GLenum; index: GLuint; _type: GLenum; ustride: GLsizei; vstride: GLsizei; uorder: GLint; vorder: GLint; _packed: GLboolean; const points: PGLvoid); extdecl;
  glMapParameterivNV: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glMapParameterfvNV: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glGetMapControlPointsNV: procedure(target: GLenum; index: GLuint; _type: GLenum; ustride: GLsizei; vstride: GLsizei; _packed: GLboolean; points: PGLvoid); extdecl;
  glGetMapParameterivNV: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetMapParameterfvNV: procedure(target: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetMapAttribParameterivNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetMapAttribParameterfvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glEvalMapsNV: procedure(target: GLenum; mode: GLenum); extdecl;

function Load_GL_NV_evaluators(): Boolean;

//**** GL_NV_packed_depth_stencil *****//
const
  GL_DEPTH_STENCIL_NV = $84F9;
  GL_UNSIGNED_INT_24_8_NV = $84FA;

function Load_GL_NV_packed_depth_stencil(): Boolean;

//**** GL_NV_register_combiners2 *****//
const
  GL_PER_STAGE_CONSTANTS_NV = $8535;
var
  glCombinerStageParameterfvNV: procedure(stage: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glGetCombinerStageParameterfvNV: procedure(stage: GLenum; pname: GLenum; params: PGLfloat); extdecl;

function Load_GL_NV_register_combiners2(): Boolean;

//**** GL_NV_texture_compression_vtc *****//

function Load_GL_NV_texture_compression_vtc(): Boolean;

//**** GL_NV_texture_rectangle *****//
const
  GL_TEXTURE_RECTANGLE_NV = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_NV = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_NV = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_NV = $84F8;

function Load_GL_NV_texture_rectangle(): Boolean;

//**** GL_NV_texture_shader *****//
const
  GL_OFFSET_TEXTURE_RECTANGLE_NV = $864C;
  GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV = $864D;
  GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV = $864E;
  GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV = $86D9;
  GL_UNSIGNED_INT_S8_S8_8_8_NV = $86DA;
  GL_UNSIGNED_INT_8_8_S8_S8_REV_NV = $86DB;
  GL_DSDT_MAG_INTENSITY_NV = $86DC;
  GL_SHADER_CONSISTENT_NV = $86DD;
  GL_TEXTURE_SHADER_NV = $86DE;
  GL_SHADER_OPERATION_NV = $86DF;
  GL_CULL_MODES_NV = $86E0;
  GL_OFFSET_TEXTURE_MATRIX_NV = $86E1;
  GL_OFFSET_TEXTURE_SCALE_NV = $86E2;
  GL_OFFSET_TEXTURE_BIAS_NV = $86E3;
  GL_OFFSET_TEXTURE_2D_MATRIX_NV = GL_OFFSET_TEXTURE_MATRIX_NV;
  GL_OFFSET_TEXTURE_2D_SCALE_NV = GL_OFFSET_TEXTURE_SCALE_NV;
  GL_OFFSET_TEXTURE_2D_BIAS_NV = GL_OFFSET_TEXTURE_BIAS_NV;
  GL_PREVIOUS_TEXTURE_INPUT_NV = $86E4;
  GL_CONST_EYE_NV = $86E5;
  GL_PASS_THROUGH_NV = $86E6;
  GL_CULL_FRAGMENT_NV = $86E7;
  GL_OFFSET_TEXTURE_2D_NV = $86E8;
  GL_DEPENDENT_AR_TEXTURE_2D_NV = $86E9;
  GL_DEPENDENT_GB_TEXTURE_2D_NV = $86EA;
  GL_DOT_PRODUCT_NV = $86EC;
  GL_DOT_PRODUCT_DEPTH_REPLACE_NV = $86ED;
  GL_DOT_PRODUCT_TEXTURE_2D_NV = $86EE;
  GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV = $86F0;
  GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV = $86F1;
  GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV = $86F2;
  GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV = $86F3;
  GL_HILO_NV = $86F4;
  GL_DSDT_NV = $86F5;
  GL_DSDT_MAG_NV = $86F6;
  GL_DSDT_MAG_VIB_NV = $86F7;
  GL_HILO16_NV = $86F8;
  GL_SIGNED_HILO_NV = $86F9;
  GL_SIGNED_HILO16_NV = $86FA;
  GL_SIGNED_RGBA_NV = $86FB;
  GL_SIGNED_RGBA8_NV = $86FC;
  GL_SIGNED_RGB_NV = $86FE;
  GL_SIGNED_RGB8_NV = $86FF;
  GL_SIGNED_LUMINANCE_NV = $8701;
  GL_SIGNED_LUMINANCE8_NV = $8702;
  GL_SIGNED_LUMINANCE_ALPHA_NV = $8703;
  GL_SIGNED_LUMINANCE8_ALPHA8_NV = $8704;
  GL_SIGNED_ALPHA_NV = $8705;
  GL_SIGNED_ALPHA8_NV = $8706;
  GL_SIGNED_INTENSITY_NV = $8707;
  GL_SIGNED_INTENSITY8_NV = $8708;
  GL_DSDT8_NV = $8709;
  GL_DSDT8_MAG8_NV = $870A;
  GL_DSDT8_MAG8_INTENSITY8_NV = $870B;
  GL_SIGNED_RGB_UNSIGNED_ALPHA_NV = $870C;
  GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV = $870D;
  GL_HI_SCALE_NV = $870E;
  GL_LO_SCALE_NV = $870F;
  GL_DS_SCALE_NV = $8710;
  GL_DT_SCALE_NV = $8711;
  GL_MAGNITUDE_SCALE_NV = $8712;
  GL_VIBRANCE_SCALE_NV = $8713;
  GL_HI_BIAS_NV = $8714;
  GL_LO_BIAS_NV = $8715;
  GL_DS_BIAS_NV = $8716;
  GL_DT_BIAS_NV = $8717;
  GL_MAGNITUDE_BIAS_NV = $8718;
  GL_VIBRANCE_BIAS_NV = $8719;
  GL_TEXTURE_BORDER_VALUES_NV = $871A;
  GL_TEXTURE_HI_SIZE_NV = $871B;
  GL_TEXTURE_LO_SIZE_NV = $871C;
  GL_TEXTURE_DS_SIZE_NV = $871D;
  GL_TEXTURE_DT_SIZE_NV = $871E;
  GL_TEXTURE_MAG_SIZE_NV = $871F;

function Load_GL_NV_texture_shader(): Boolean;

//**** GL_NV_texture_shader2 *****//
const
  GL_DOT_PRODUCT_TEXTURE_3D_NV = $86EF;

function Load_GL_NV_texture_shader2(): Boolean;

//**** GL_NV_vertex_array_range2 *****//
const
  GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV = $8533;

function Load_GL_NV_vertex_array_range2(): Boolean;

//**** GL_NV_vertex_program *****//
const
  GL_VERTEX_PROGRAM_NV = $8620;
  GL_VERTEX_STATE_PROGRAM_NV = $8621;
  GL_ATTRIB_ARRAY_SIZE_NV = $8623;
  GL_ATTRIB_ARRAY_STRIDE_NV = $8624;
  GL_ATTRIB_ARRAY_TYPE_NV = $8625;
  GL_CURRENT_ATTRIB_NV = $8626;
  GL_PROGRAM_LENGTH_NV = $8627;
  GL_PROGRAM_STRING_NV = $8628;
  GL_MODELVIEW_PROJECTION_NV = $8629;
  GL_IDENTITY_NV = $862A;
  GL_INVERSE_NV = $862B;
  GL_TRANSPOSE_NV = $862C;
  GL_INVERSE_TRANSPOSE_NV = $862D;
  GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV = $862E;
  GL_MAX_TRACK_MATRICES_NV = $862F;
  GL_MATRIX0_NV = $8630;
  GL_MATRIX1_NV = $8631;
  GL_MATRIX2_NV = $8632;
  GL_MATRIX3_NV = $8633;
  GL_MATRIX4_NV = $8634;
  GL_MATRIX5_NV = $8635;
  GL_MATRIX6_NV = $8636;
  GL_MATRIX7_NV = $8637;
  GL_CURRENT_MATRIX_STACK_DEPTH_NV = $8640;
  GL_CURRENT_MATRIX_NV = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_NV = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_NV = $8643;
  GL_PROGRAM_PARAMETER_NV = $8644;
  GL_ATTRIB_ARRAY_POINTER_NV = $8645;
  GL_PROGRAM_TARGET_NV = $8646;
  GL_PROGRAM_RESIDENT_NV = $8647;
  GL_TRACK_MATRIX_NV = $8648;
  GL_TRACK_MATRIX_TRANSFORM_NV = $8649;
  GL_VERTEX_PROGRAM_BINDING_NV = $864A;
  GL_PROGRAM_ERROR_POSITION_NV = $864B;
  GL_VERTEX_ATTRIB_ARRAY0_NV = $8650;
  GL_VERTEX_ATTRIB_ARRAY1_NV = $8651;
  GL_VERTEX_ATTRIB_ARRAY2_NV = $8652;
  GL_VERTEX_ATTRIB_ARRAY3_NV = $8653;
  GL_VERTEX_ATTRIB_ARRAY4_NV = $8654;
  GL_VERTEX_ATTRIB_ARRAY5_NV = $8655;
  GL_VERTEX_ATTRIB_ARRAY6_NV = $8656;
  GL_VERTEX_ATTRIB_ARRAY7_NV = $8657;
  GL_VERTEX_ATTRIB_ARRAY8_NV = $8658;
  GL_VERTEX_ATTRIB_ARRAY9_NV = $8659;
  GL_VERTEX_ATTRIB_ARRAY10_NV = $865A;
  GL_VERTEX_ATTRIB_ARRAY11_NV = $865B;
  GL_VERTEX_ATTRIB_ARRAY12_NV = $865C;
  GL_VERTEX_ATTRIB_ARRAY13_NV = $865D;
  GL_VERTEX_ATTRIB_ARRAY14_NV = $865E;
  GL_VERTEX_ATTRIB_ARRAY15_NV = $865F;
  GL_MAP1_VERTEX_ATTRIB0_4_NV = $8660;
  GL_MAP1_VERTEX_ATTRIB1_4_NV = $8661;
  GL_MAP1_VERTEX_ATTRIB2_4_NV = $8662;
  GL_MAP1_VERTEX_ATTRIB3_4_NV = $8663;
  GL_MAP1_VERTEX_ATTRIB4_4_NV = $8664;
  GL_MAP1_VERTEX_ATTRIB5_4_NV = $8665;
  GL_MAP1_VERTEX_ATTRIB6_4_NV = $8666;
  GL_MAP1_VERTEX_ATTRIB7_4_NV = $8667;
  GL_MAP1_VERTEX_ATTRIB8_4_NV = $8668;
  GL_MAP1_VERTEX_ATTRIB9_4_NV = $8669;
  GL_MAP1_VERTEX_ATTRIB10_4_NV = $866A;
  GL_MAP1_VERTEX_ATTRIB11_4_NV = $866B;
  GL_MAP1_VERTEX_ATTRIB12_4_NV = $866C;
  GL_MAP1_VERTEX_ATTRIB13_4_NV = $866D;
  GL_MAP1_VERTEX_ATTRIB14_4_NV = $866E;
  GL_MAP1_VERTEX_ATTRIB15_4_NV = $866F;
  GL_MAP2_VERTEX_ATTRIB0_4_NV = $8670;
  GL_MAP2_VERTEX_ATTRIB1_4_NV = $8671;
  GL_MAP2_VERTEX_ATTRIB2_4_NV = $8672;
  GL_MAP2_VERTEX_ATTRIB3_4_NV = $8673;
  GL_MAP2_VERTEX_ATTRIB4_4_NV = $8674;
  GL_MAP2_VERTEX_ATTRIB5_4_NV = $8675;
  GL_MAP2_VERTEX_ATTRIB6_4_NV = $8676;
  GL_MAP2_VERTEX_ATTRIB7_4_NV = $8677;
  GL_MAP2_VERTEX_ATTRIB8_4_NV = $8678;
  GL_MAP2_VERTEX_ATTRIB9_4_NV = $8679;
  GL_MAP2_VERTEX_ATTRIB10_4_NV = $867A;
  GL_MAP2_VERTEX_ATTRIB11_4_NV = $867B;
  GL_MAP2_VERTEX_ATTRIB12_4_NV = $867C;
  GL_MAP2_VERTEX_ATTRIB13_4_NV = $867D;
  GL_MAP2_VERTEX_ATTRIB14_4_NV = $867E;
  GL_MAP2_VERTEX_ATTRIB15_4_NV = $867F;
var
  glAreProgramsResidentNV: function(n: GLsizei; const programs: PGLuint; residences: PGLboolean): GLboolean; extdecl;
  glBindProgramNV: procedure(target: GLenum; id: GLuint); extdecl;
  glDeleteProgramsNV: procedure(n: GLsizei; const programs: PGLuint); extdecl;
  glExecuteProgramNV: procedure(target: GLenum; id: GLuint; const params: PGLfloat); extdecl;
  glGenProgramsNV: procedure(n: GLsizei; programs: PGLuint); extdecl;
  glGetProgramParameterdvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLdouble); extdecl;
  glGetProgramParameterfvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetProgramivNV: procedure(id: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetProgramStringNV: procedure(id: GLuint; pname: GLenum; _program: PGLubyte); extdecl;
  glGetTrackMatrixivNV: procedure(target: GLenum; address: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVertexAttribdvNV: procedure(index: GLuint; pname: GLenum; params: PGLdouble); extdecl;
  glGetVertexAttribfvNV: procedure(index: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetVertexAttribivNV: procedure(index: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVertexAttribPointervNV: procedure(index: GLuint; pname: GLenum; _pointer: PPGLvoid); extdecl;
  glIsProgramNV: function(id: GLuint): GLboolean; extdecl;
  glLoadProgramNV: procedure(target: GLenum; id: GLuint; len: GLsizei; const _program: PGLubyte); extdecl;
  glProgramParameter4dNV: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glProgramParameter4dvNV: procedure(target: GLenum; index: GLuint; const v: PGLdouble); extdecl;
  glProgramParameter4fNV: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glProgramParameter4fvNV: procedure(target: GLenum; index: GLuint; const v: PGLfloat); extdecl;
  glProgramParameters4dvNV: procedure(target: GLenum; index: GLuint; count: GLuint; const v: PGLdouble); extdecl;
  glProgramParameters4fvNV: procedure(target: GLenum; index: GLuint; count: GLuint; const v: PGLfloat); extdecl;
  glRequestResidentProgramsNV: procedure(n: GLsizei; const programs: PGLuint); extdecl;
  glTrackMatrixNV: procedure(target: GLenum; address: GLuint; matrix: GLenum; transform: GLenum); extdecl;
  glVertexAttribPointerNV: procedure(index: GLuint; fsize: GLint; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;
  glVertexAttrib1dNV: procedure(index: GLuint; x: GLdouble); extdecl;
  glVertexAttrib1dvNV: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib1fNV: procedure(index: GLuint; x: GLfloat); extdecl;
  glVertexAttrib1fvNV: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib1sNV: procedure(index: GLuint; x: GLshort); extdecl;
  glVertexAttrib1svNV: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib2dNV: procedure(index: GLuint; x: GLdouble; y: GLdouble); extdecl;
  glVertexAttrib2dvNV: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib2fNV: procedure(index: GLuint; x: GLfloat; y: GLfloat); extdecl;
  glVertexAttrib2fvNV: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib2sNV: procedure(index: GLuint; x: GLshort; y: GLshort); extdecl;
  glVertexAttrib2svNV: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib3dNV: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glVertexAttrib3dvNV: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib3fNV: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glVertexAttrib3fvNV: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib3sNV: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); extdecl;
  glVertexAttrib3svNV: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4dNV: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glVertexAttrib4dvNV: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib4fNV: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glVertexAttrib4fvNV: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib4sNV: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); extdecl;
  glVertexAttrib4svNV: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4ubNV: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); extdecl;
  glVertexAttrib4ubvNV: procedure(index: GLuint; const v: PGLubyte); extdecl;
  glVertexAttribs1dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glVertexAttribs1fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glVertexAttribs1svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); extdecl;
  glVertexAttribs2dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glVertexAttribs2fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glVertexAttribs2svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); extdecl;
  glVertexAttribs3dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glVertexAttribs3fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glVertexAttribs3svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); extdecl;
  glVertexAttribs4dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glVertexAttribs4fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glVertexAttribs4svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); extdecl;
  glVertexAttribs4ubvNV: procedure(index: GLuint; count: GLsizei; const v: PGLubyte); extdecl;

function Load_GL_NV_vertex_program(): Boolean;

//**** GL_SGIX_texture_coordinate_clamp *****//
const
  GL_TEXTURE_MAX_CLAMP_S_SGIX = $8369;
  GL_TEXTURE_MAX_CLAMP_T_SGIX = $836A;
  GL_TEXTURE_MAX_CLAMP_R_SGIX = $836B;

function Load_GL_SGIX_texture_coordinate_clamp(): Boolean;

//**** GL_SGIX_scalebias_hint *****//
const
  GL_SCALEBIAS_HINT_SGIX = $8322;

function Load_GL_SGIX_scalebias_hint(): Boolean;

//**** GL_OML_interlace *****//
const
  GL_INTERLACE_OML = $8980;
  GL_INTERLACE_READ_OML = $8981;

function Load_GL_OML_interlace(): Boolean;

//**** GL_OML_subsample *****//
const
  GL_FORMAT_SUBSAMPLE_24_24_OML = $8982;
  GL_FORMAT_SUBSAMPLE_244_244_OML = $8983;

function Load_GL_OML_subsample(): Boolean;

//**** GL_OML_resample *****//
const
  GL_PACK_RESAMPLE_OML = $8984;
  GL_UNPACK_RESAMPLE_OML = $8985;
  GL_RESAMPLE_REPLICATE_OML = $8986;
  GL_RESAMPLE_ZERO_FILL_OML = $8987;
  GL_RESAMPLE_AVERAGE_OML = $8988;
  GL_RESAMPLE_DECIMATE_OML = $8989;

function Load_GL_OML_resample(): Boolean;

//**** GL_NV_copy_depth_to_color *****//
const
  GL_DEPTH_STENCIL_TO_RGBA_NV = $886E;
  GL_DEPTH_STENCIL_TO_BGRA_NV = $886F;

function Load_GL_NV_copy_depth_to_color(): Boolean;

//**** GL_ATI_envmap_bumpmap *****//
const
  GL_BUMP_ROT_MATRIX_ATI = $8775;
  GL_BUMP_ROT_MATRIX_SIZE_ATI = $8776;
  GL_BUMP_NUM_TEX_UNITS_ATI = $8777;
  GL_BUMP_TEX_UNITS_ATI = $8778;
  GL_DUDV_ATI = $8779;
  GL_DU8DV8_ATI = $877A;
  GL_BUMP_ENVMAP_ATI = $877B;
  GL_BUMP_TARGET_ATI = $877C;
var
  glTexBumpParameterivATI: procedure(pname: GLenum; const param: PGLint); extdecl;
  glTexBumpParameterfvATI: procedure(pname: GLenum; const param: PGLfloat); extdecl;
  glGetTexBumpParameterivATI: procedure(pname: GLenum; param: PGLint); extdecl;
  glGetTexBumpParameterfvATI: procedure(pname: GLenum; param: PGLfloat); extdecl;

function Load_GL_ATI_envmap_bumpmap(): Boolean;

//**** GL_ATI_fragment_shader *****//
const
  GL_FRAGMENT_SHADER_ATI = $8920;
  GL_REG_0_ATI = $8921;
  GL_REG_1_ATI = $8922;
  GL_REG_2_ATI = $8923;
  GL_REG_3_ATI = $8924;
  GL_REG_4_ATI = $8925;
  GL_REG_5_ATI = $8926;
  GL_REG_6_ATI = $8927;
  GL_REG_7_ATI = $8928;
  GL_REG_8_ATI = $8929;
  GL_REG_9_ATI = $892A;
  GL_REG_10_ATI = $892B;
  GL_REG_11_ATI = $892C;
  GL_REG_12_ATI = $892D;
  GL_REG_13_ATI = $892E;
  GL_REG_14_ATI = $892F;
  GL_REG_15_ATI = $8930;
  GL_REG_16_ATI = $8931;
  GL_REG_17_ATI = $8932;
  GL_REG_18_ATI = $8933;
  GL_REG_19_ATI = $8934;
  GL_REG_20_ATI = $8935;
  GL_REG_21_ATI = $8936;
  GL_REG_22_ATI = $8937;
  GL_REG_23_ATI = $8938;
  GL_REG_24_ATI = $8939;
  GL_REG_25_ATI = $893A;
  GL_REG_26_ATI = $893B;
  GL_REG_27_ATI = $893C;
  GL_REG_28_ATI = $893D;
  GL_REG_29_ATI = $893E;
  GL_REG_30_ATI = $893F;
  GL_REG_31_ATI = $8940;
  GL_CON_0_ATI = $8941;
  GL_CON_1_ATI = $8942;
  GL_CON_2_ATI = $8943;
  GL_CON_3_ATI = $8944;
  GL_CON_4_ATI = $8945;
  GL_CON_5_ATI = $8946;
  GL_CON_6_ATI = $8947;
  GL_CON_7_ATI = $8948;
  GL_CON_8_ATI = $8949;
  GL_CON_9_ATI = $894A;
  GL_CON_10_ATI = $894B;
  GL_CON_11_ATI = $894C;
  GL_CON_12_ATI = $894D;
  GL_CON_13_ATI = $894E;
  GL_CON_14_ATI = $894F;
  GL_CON_15_ATI = $8950;
  GL_CON_16_ATI = $8951;
  GL_CON_17_ATI = $8952;
  GL_CON_18_ATI = $8953;
  GL_CON_19_ATI = $8954;
  GL_CON_20_ATI = $8955;
  GL_CON_21_ATI = $8956;
  GL_CON_22_ATI = $8957;
  GL_CON_23_ATI = $8958;
  GL_CON_24_ATI = $8959;
  GL_CON_25_ATI = $895A;
  GL_CON_26_ATI = $895B;
  GL_CON_27_ATI = $895C;
  GL_CON_28_ATI = $895D;
  GL_CON_29_ATI = $895E;
  GL_CON_30_ATI = $895F;
  GL_CON_31_ATI = $8960;
  GL_MOV_ATI = $8961;
  GL_ADD_ATI = $8963;
  GL_MUL_ATI = $8964;
  GL_SUB_ATI = $8965;
  GL_DOT3_ATI = $8966;
  GL_DOT4_ATI = $8967;
  GL_MAD_ATI = $8968;
  GL_LERP_ATI = $8969;
  GL_CND_ATI = $896A;
  GL_CND0_ATI = $896B;
  GL_DOT2_ADD_ATI = $896C;
  GL_SECONDARY_INTERPOLATOR_ATI = $896D;
  GL_NUM_FRAGMENT_REGISTERS_ATI = $896E;
  GL_NUM_FRAGMENT_CONSTANTS_ATI = $896F;
  GL_NUM_PASSES_ATI = $8970;
  GL_NUM_INSTRUCTIONS_PER_PASS_ATI = $8971;
  GL_NUM_INSTRUCTIONS_TOTAL_ATI = $8972;
  GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI = $8973;
  GL_NUM_LOOPBACK_COMPONENTS_ATI = $8974;
  GL_COLOR_ALPHA_PAIRING_ATI = $8975;
  GL_SWIZZLE_STR_ATI = $8976;
  GL_SWIZZLE_STQ_ATI = $8977;
  GL_SWIZZLE_STR_DR_ATI = $8978;
  GL_SWIZZLE_STQ_DQ_ATI = $8979;
  GL_SWIZZLE_STRQ_ATI = $897A;
  GL_SWIZZLE_STRQ_DQ_ATI = $897B;
  GL_RED_BIT_ATI = $00000001;
  GL_GREEN_BIT_ATI = $00000002;
  GL_BLUE_BIT_ATI = $00000004;
  GL_2X_BIT_ATI = $00000001;
  GL_4X_BIT_ATI = $00000002;
  GL_8X_BIT_ATI = $00000004;
  GL_HALF_BIT_ATI = $00000008;
  GL_QUARTER_BIT_ATI = $00000010;
  GL_EIGHTH_BIT_ATI = $00000020;
  GL_SATURATE_BIT_ATI = $00000040;
  GL_COMP_BIT_ATI = $00000002;
  GL_NEGATE_BIT_ATI = $00000004;
  GL_BIAS_BIT_ATI = $00000008;
var
  glGenFragmentShadersATI: function(range: GLuint): GLuint; extdecl;
  glBindFragmentShaderATI: procedure(id: GLuint); extdecl;
  glDeleteFragmentShaderATI: procedure(id: GLuint); extdecl;
  glBeginFragmentShaderATI: procedure(); extdecl;
  glEndFragmentShaderATI: procedure(); extdecl;
  glPassTexCoordATI: procedure(dst: GLuint; coord: GLuint; swizzle: GLenum); extdecl;
  glSampleMapATI: procedure(dst: GLuint; interp: GLuint; swizzle: GLenum); extdecl;
  glColorFragmentOp1ATI: procedure(op: GLenum; dst: GLuint; dstMask: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint); extdecl;
  glColorFragmentOp2ATI: procedure(op: GLenum; dst: GLuint; dstMask: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint; arg2: GLuint; arg2Rep: GLuint; arg2Mod: GLuint); extdecl;
  glColorFragmentOp3ATI: procedure(op: GLenum; dst: GLuint; dstMask: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint; arg2: GLuint; arg2Rep: GLuint; arg2Mod: GLuint; arg3: GLuint; arg3Rep: GLuint; arg3Mod: GLuint); extdecl;
  glAlphaFragmentOp1ATI: procedure(op: GLenum; dst: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint); extdecl;
  glAlphaFragmentOp2ATI: procedure(op: GLenum; dst: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint; arg2: GLuint; arg2Rep: GLuint; arg2Mod: GLuint); extdecl;
  glAlphaFragmentOp3ATI: procedure(op: GLenum; dst: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint; arg2: GLuint; arg2Rep: GLuint; arg2Mod: GLuint; arg3: GLuint; arg3Rep: GLuint; arg3Mod: GLuint); extdecl;
  glSetFragmentShaderConstantATI: procedure(dst: GLuint; const value: PGLfloat); extdecl;

function Load_GL_ATI_fragment_shader(): Boolean;

//**** GL_ATI_pn_triangles *****//
const
  GL_PN_TRIANGLES_ATI = $87F0;
  GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI = $87F1;
  GL_PN_TRIANGLES_POINT_MODE_ATI = $87F2;
  GL_PN_TRIANGLES_NORMAL_MODE_ATI = $87F3;
  GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI = $87F4;
  GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI = $87F5;
  GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI = $87F6;
  GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI = $87F7;
  GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI = $87F8;
var
  glPNTrianglesiATI: procedure(pname: GLenum; param: GLint); extdecl;
  glPNTrianglesfATI: procedure(pname: GLenum; param: GLfloat); extdecl;

function Load_GL_ATI_pn_triangles(): Boolean;

//**** GL_ATI_vertex_array_object *****//
const
  GL_STATIC_ATI = $8760;
  GL_DYNAMIC_ATI = $8761;
  GL_PRESERVE_ATI = $8762;
  GL_DISCARD_ATI = $8763;
  GL_OBJECT_BUFFER_SIZE_ATI = $8764;
  GL_OBJECT_BUFFER_USAGE_ATI = $8765;
  GL_ARRAY_OBJECT_BUFFER_ATI = $8766;
  GL_ARRAY_OBJECT_OFFSET_ATI = $8767;
var
  glNewObjectBufferATI: function(size: GLsizei; const _pointer: PGLvoid; usage: GLenum): GLuint; extdecl;
  glIsObjectBufferATI: function(buffer: GLuint): GLboolean; extdecl;
  glUpdateObjectBufferATI: procedure(buffer: GLuint; offset: GLuint; size: GLsizei; const _pointer: PGLvoid; preserve: GLenum); extdecl;
  glGetObjectBufferfvATI: procedure(buffer: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetObjectBufferivATI: procedure(buffer: GLuint; pname: GLenum; params: PGLint); extdecl;
  glFreeObjectBufferATI: procedure(buffer: GLuint); extdecl;
  glArrayObjectATI: procedure(_array: GLenum; size: GLint; _type: GLenum; stride: GLsizei; buffer: GLuint; offset: GLuint); extdecl;
  glGetArrayObjectfvATI: procedure(_array: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetArrayObjectivATI: procedure(_array: GLenum; pname: GLenum; params: PGLint); extdecl;
  glVariantArrayObjectATI: procedure(id: GLuint; _type: GLenum; stride: GLsizei; buffer: GLuint; offset: GLuint); extdecl;
  glGetVariantArrayObjectfvATI: procedure(id: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetVariantArrayObjectivATI: procedure(id: GLuint; pname: GLenum; params: PGLint); extdecl;

function Load_GL_ATI_vertex_array_object(): Boolean;

//**** GL_EXT_vertex_shader *****//
const
  GL_VERTEX_SHADER_EXT = $8780;
  GL_VERTEX_SHADER_BINDING_EXT = $8781;
  GL_OP_INDEX_EXT = $8782;
  GL_OP_NEGATE_EXT = $8783;
  GL_OP_DOT3_EXT = $8784;
  GL_OP_DOT4_EXT = $8785;
  GL_OP_MUL_EXT = $8786;
  GL_OP_ADD_EXT = $8787;
  GL_OP_MADD_EXT = $8788;
  GL_OP_FRAC_EXT = $8789;
  GL_OP_MAX_EXT = $878A;
  GL_OP_MIN_EXT = $878B;
  GL_OP_SET_GE_EXT = $878C;
  GL_OP_SET_LT_EXT = $878D;
  GL_OP_CLAMP_EXT = $878E;
  GL_OP_FLOOR_EXT = $878F;
  GL_OP_ROUND_EXT = $8790;
  GL_OP_EXP_BASE_2_EXT = $8791;
  GL_OP_LOG_BASE_2_EXT = $8792;
  GL_OP_POWER_EXT = $8793;
  GL_OP_RECIP_EXT = $8794;
  GL_OP_RECIP_SQRT_EXT = $8795;
  GL_OP_SUB_EXT = $8796;
  GL_OP_CROSS_PRODUCT_EXT = $8797;
  GL_OP_MULTIPLY_MATRIX_EXT = $8798;
  GL_OP_MOV_EXT = $8799;
  GL_OUTPUT_VERTEX_EXT = $879A;
  GL_OUTPUT_COLOR0_EXT = $879B;
  GL_OUTPUT_COLOR1_EXT = $879C;
  GL_OUTPUT_TEXTURE_COORD0_EXT = $879D;
  GL_OUTPUT_TEXTURE_COORD1_EXT = $879E;
  GL_OUTPUT_TEXTURE_COORD2_EXT = $879F;
  GL_OUTPUT_TEXTURE_COORD3_EXT = $87A0;
  GL_OUTPUT_TEXTURE_COORD4_EXT = $87A1;
  GL_OUTPUT_TEXTURE_COORD5_EXT = $87A2;
  GL_OUTPUT_TEXTURE_COORD6_EXT = $87A3;
  GL_OUTPUT_TEXTURE_COORD7_EXT = $87A4;
  GL_OUTPUT_TEXTURE_COORD8_EXT = $87A5;
  GL_OUTPUT_TEXTURE_COORD9_EXT = $87A6;
  GL_OUTPUT_TEXTURE_COORD10_EXT = $87A7;
  GL_OUTPUT_TEXTURE_COORD11_EXT = $87A8;
  GL_OUTPUT_TEXTURE_COORD12_EXT = $87A9;
  GL_OUTPUT_TEXTURE_COORD13_EXT = $87AA;
  GL_OUTPUT_TEXTURE_COORD14_EXT = $87AB;
  GL_OUTPUT_TEXTURE_COORD15_EXT = $87AC;
  GL_OUTPUT_TEXTURE_COORD16_EXT = $87AD;
  GL_OUTPUT_TEXTURE_COORD17_EXT = $87AE;
  GL_OUTPUT_TEXTURE_COORD18_EXT = $87AF;
  GL_OUTPUT_TEXTURE_COORD19_EXT = $87B0;
  GL_OUTPUT_TEXTURE_COORD20_EXT = $87B1;
  GL_OUTPUT_TEXTURE_COORD21_EXT = $87B2;
  GL_OUTPUT_TEXTURE_COORD22_EXT = $87B3;
  GL_OUTPUT_TEXTURE_COORD23_EXT = $87B4;
  GL_OUTPUT_TEXTURE_COORD24_EXT = $87B5;
  GL_OUTPUT_TEXTURE_COORD25_EXT = $87B6;
  GL_OUTPUT_TEXTURE_COORD26_EXT = $87B7;
  GL_OUTPUT_TEXTURE_COORD27_EXT = $87B8;
  GL_OUTPUT_TEXTURE_COORD28_EXT = $87B9;
  GL_OUTPUT_TEXTURE_COORD29_EXT = $87BA;
  GL_OUTPUT_TEXTURE_COORD30_EXT = $87BB;
  GL_OUTPUT_TEXTURE_COORD31_EXT = $87BC;
  GL_OUTPUT_FOG_EXT = $87BD;
  GL_SCALAR_EXT = $87BE;
  GL_VECTOR_EXT = $87BF;
  GL_MATRIX_EXT = $87C0;
  GL_VARIANT_EXT = $87C1;
  GL_INVARIANT_EXT = $87C2;
  GL_LOCAL_CONSTANT_EXT = $87C3;
  GL_LOCAL_EXT = $87C4;
  GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT = $87C5;
  GL_MAX_VERTEX_SHADER_VARIANTS_EXT = $87C6;
  GL_MAX_VERTEX_SHADER_INVARIANTS_EXT = $87C7;
  GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87C8;
  GL_MAX_VERTEX_SHADER_LOCALS_EXT = $87C9;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT = $87CA;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT = $87CB;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87CC;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT = $87CD;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT = $87CE;
  GL_VERTEX_SHADER_INSTRUCTIONS_EXT = $87CF;
  GL_VERTEX_SHADER_VARIANTS_EXT = $87D0;
  GL_VERTEX_SHADER_INVARIANTS_EXT = $87D1;
  GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87D2;
  GL_VERTEX_SHADER_LOCALS_EXT = $87D3;
  GL_VERTEX_SHADER_OPTIMIZED_EXT = $87D4;
  GL_X_EXT = $87D5;
  GL_Y_EXT = $87D6;
  GL_Z_EXT = $87D7;
  GL_W_EXT = $87D8;
  GL_NEGATIVE_X_EXT = $87D9;
  GL_NEGATIVE_Y_EXT = $87DA;
  GL_NEGATIVE_Z_EXT = $87DB;
  GL_NEGATIVE_W_EXT = $87DC;
  GL_ZERO_EXT = $87DD;
  GL_ONE_EXT = $87DE;
  GL_NEGATIVE_ONE_EXT = $87DF;
  GL_NORMALIZED_RANGE_EXT = $87E0;
  GL_FULL_RANGE_EXT = $87E1;
  GL_CURRENT_VERTEX_EXT = $87E2;
  GL_MVP_MATRIX_EXT = $87E3;
  GL_VARIANT_VALUE_EXT = $87E4;
  GL_VARIANT_DATATYPE_EXT = $87E5;
  GL_VARIANT_ARRAY_STRIDE_EXT = $87E6;
  GL_VARIANT_ARRAY_TYPE_EXT = $87E7;
  GL_VARIANT_ARRAY_EXT = $87E8;
  GL_VARIANT_ARRAY_POINTER_EXT = $87E9;
  GL_INVARIANT_VALUE_EXT = $87EA;
  GL_INVARIANT_DATATYPE_EXT = $87EB;
  GL_LOCAL_CONSTANT_VALUE_EXT = $87EC;
  GL_LOCAL_CONSTANT_DATATYPE_EXT = $87ED;
var
  glBeginVertexShaderEXT: procedure(); extdecl;
  glEndVertexShaderEXT: procedure(); extdecl;
  glBindVertexShaderEXT: procedure(id: GLuint); extdecl;
  glGenVertexShadersEXT: function(range: GLuint): GLuint; extdecl;
  glDeleteVertexShaderEXT: procedure(id: GLuint); extdecl;
  glShaderOp1EXT: procedure(op: GLenum; res: GLuint; arg1: GLuint); extdecl;
  glShaderOp2EXT: procedure(op: GLenum; res: GLuint; arg1: GLuint; arg2: GLuint); extdecl;
  glShaderOp3EXT: procedure(op: GLenum; res: GLuint; arg1: GLuint; arg2: GLuint; arg3: GLuint); extdecl;
  glSwizzleEXT: procedure(res: GLuint; _in: GLuint; outX: GLenum; outY: GLenum; outZ: GLenum; outW: GLenum); extdecl;
  glWriteMaskEXT: procedure(res: GLuint; _in: GLuint; outX: GLenum; outY: GLenum; outZ: GLenum; outW: GLenum); extdecl;
  glInsertComponentEXT: procedure(res: GLuint; src: GLuint; num: GLuint); extdecl;
  glExtractComponentEXT: procedure(res: GLuint; src: GLuint; num: GLuint); extdecl;
  glGenSymbolsEXT: function(datatype: GLenum; storagetype: GLenum; range: GLenum; components: GLuint): GLuint; extdecl;
  glSetInvariantEXT: procedure(id: GLuint; _type: GLenum; const addr: PGLvoid); extdecl;
  glSetLocalConstantEXT: procedure(id: GLuint; _type: GLenum; const addr: PGLvoid); extdecl;
  glVariantbvEXT: procedure(id: GLuint; const addr: PGLbyte); extdecl;
  glVariantsvEXT: procedure(id: GLuint; const addr: PGLshort); extdecl;
  glVariantivEXT: procedure(id: GLuint; const addr: PGLint); extdecl;
  glVariantfvEXT: procedure(id: GLuint; const addr: PGLfloat); extdecl;
  glVariantdvEXT: procedure(id: GLuint; const addr: PGLdouble); extdecl;
  glVariantubvEXT: procedure(id: GLuint; const addr: PGLubyte); extdecl;
  glVariantusvEXT: procedure(id: GLuint; const addr: PGLushort); extdecl;
  glVariantuivEXT: procedure(id: GLuint; const addr: PGLuint); extdecl;
  glVariantPointerEXT: procedure(id: GLuint; _type: GLenum; stride: GLuint; const addr: PGLvoid); extdecl;
  glEnableVariantClientStateEXT: procedure(id: GLuint); extdecl;
  glDisableVariantClientStateEXT: procedure(id: GLuint); extdecl;
  glBindLightParameterEXT: function(light: GLenum; value: GLenum): GLuint; extdecl;
  glBindMaterialParameterEXT: function(face: GLenum; value: GLenum): GLuint; extdecl;
  glBindTexGenParameterEXT: function(_unit: GLenum; coord: GLenum; value: GLenum): GLuint; extdecl;
  glBindTextureUnitParameterEXT: function(_unit: GLenum; value: GLenum): GLuint; extdecl;
  glBindParameterEXT: function(value: GLenum): GLuint; extdecl;
  glIsVariantEnabledEXT: function(id: GLuint; cap: GLenum): GLboolean; extdecl;
  glGetVariantBooleanvEXT: procedure(id: GLuint; value: GLenum; data: PGLboolean); extdecl;
  glGetVariantIntegervEXT: procedure(id: GLuint; value: GLenum; data: PGLint); extdecl;
  glGetVariantFloatvEXT: procedure(id: GLuint; value: GLenum; data: PGLfloat); extdecl;
  glGetVariantPointervEXT: procedure(id: GLuint; value: GLenum; data: PPGLvoid); extdecl;
  glGetInvariantBooleanvEXT: procedure(id: GLuint; value: GLenum; data: PGLboolean); extdecl;
  glGetInvariantIntegervEXT: procedure(id: GLuint; value: GLenum; data: PGLint); extdecl;
  glGetInvariantFloatvEXT: procedure(id: GLuint; value: GLenum; data: PGLfloat); extdecl;
  glGetLocalConstantBooleanvEXT: procedure(id: GLuint; value: GLenum; data: PGLboolean); extdecl;
  glGetLocalConstantIntegervEXT: procedure(id: GLuint; value: GLenum; data: PGLint); extdecl;
  glGetLocalConstantFloatvEXT: procedure(id: GLuint; value: GLenum; data: PGLfloat); extdecl;

function Load_GL_EXT_vertex_shader(): Boolean;

//**** GL_ATI_vertex_streams *****//
const
  GL_MAX_VERTEX_STREAMS_ATI = $876B;
  GL_VERTEX_STREAM0_ATI = $876C;
  GL_VERTEX_STREAM1_ATI = $876D;
  GL_VERTEX_STREAM2_ATI = $876E;
  GL_VERTEX_STREAM3_ATI = $876F;
  GL_VERTEX_STREAM4_ATI = $8770;
  GL_VERTEX_STREAM5_ATI = $8771;
  GL_VERTEX_STREAM6_ATI = $8772;
  GL_VERTEX_STREAM7_ATI = $8773;
  GL_VERTEX_SOURCE_ATI = $8774;
var
  glVertexStream1sATI: procedure(stream: GLenum; x: GLshort); extdecl;
  glVertexStream1svATI: procedure(stream: GLenum; const coords: PGLshort); extdecl;
  glVertexStream1iATI: procedure(stream: GLenum; x: GLint); extdecl;
  glVertexStream1ivATI: procedure(stream: GLenum; const coords: PGLint); extdecl;
  glVertexStream1fATI: procedure(stream: GLenum; x: GLfloat); extdecl;
  glVertexStream1fvATI: procedure(stream: GLenum; const coords: PGLfloat); extdecl;
  glVertexStream1dATI: procedure(stream: GLenum; x: GLdouble); extdecl;
  glVertexStream1dvATI: procedure(stream: GLenum; const coords: PGLdouble); extdecl;
  glVertexStream2sATI: procedure(stream: GLenum; x: GLshort; y: GLshort); extdecl;
  glVertexStream2svATI: procedure(stream: GLenum; const coords: PGLshort); extdecl;
  glVertexStream2iATI: procedure(stream: GLenum; x: GLint; y: GLint); extdecl;
  glVertexStream2ivATI: procedure(stream: GLenum; const coords: PGLint); extdecl;
  glVertexStream2fATI: procedure(stream: GLenum; x: GLfloat; y: GLfloat); extdecl;
  glVertexStream2fvATI: procedure(stream: GLenum; const coords: PGLfloat); extdecl;
  glVertexStream2dATI: procedure(stream: GLenum; x: GLdouble; y: GLdouble); extdecl;
  glVertexStream2dvATI: procedure(stream: GLenum; const coords: PGLdouble); extdecl;
  glVertexStream3sATI: procedure(stream: GLenum; x: GLshort; y: GLshort; z: GLshort); extdecl;
  glVertexStream3svATI: procedure(stream: GLenum; const coords: PGLshort); extdecl;
  glVertexStream3iATI: procedure(stream: GLenum; x: GLint; y: GLint; z: GLint); extdecl;
  glVertexStream3ivATI: procedure(stream: GLenum; const coords: PGLint); extdecl;
  glVertexStream3fATI: procedure(stream: GLenum; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glVertexStream3fvATI: procedure(stream: GLenum; const coords: PGLfloat); extdecl;
  glVertexStream3dATI: procedure(stream: GLenum; x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glVertexStream3dvATI: procedure(stream: GLenum; const coords: PGLdouble); extdecl;
  glVertexStream4sATI: procedure(stream: GLenum; x: GLshort; y: GLshort; z: GLshort; w: GLshort); extdecl;
  glVertexStream4svATI: procedure(stream: GLenum; const coords: PGLshort); extdecl;
  glVertexStream4iATI: procedure(stream: GLenum; x: GLint; y: GLint; z: GLint; w: GLint); extdecl;
  glVertexStream4ivATI: procedure(stream: GLenum; const coords: PGLint); extdecl;
  glVertexStream4fATI: procedure(stream: GLenum; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glVertexStream4fvATI: procedure(stream: GLenum; const coords: PGLfloat); extdecl;
  glVertexStream4dATI: procedure(stream: GLenum; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glVertexStream4dvATI: procedure(stream: GLenum; const coords: PGLdouble); extdecl;
  glNormalStream3bATI: procedure(stream: GLenum; nx: GLbyte; ny: GLbyte; nz: GLbyte); extdecl;
  glNormalStream3bvATI: procedure(stream: GLenum; const coords: PGLbyte); extdecl;
  glNormalStream3sATI: procedure(stream: GLenum; nx: GLshort; ny: GLshort; nz: GLshort); extdecl;
  glNormalStream3svATI: procedure(stream: GLenum; const coords: PGLshort); extdecl;
  glNormalStream3iATI: procedure(stream: GLenum; nx: GLint; ny: GLint; nz: GLint); extdecl;
  glNormalStream3ivATI: procedure(stream: GLenum; const coords: PGLint); extdecl;
  glNormalStream3fATI: procedure(stream: GLenum; nx: GLfloat; ny: GLfloat; nz: GLfloat); extdecl;
  glNormalStream3fvATI: procedure(stream: GLenum; const coords: PGLfloat); extdecl;
  glNormalStream3dATI: procedure(stream: GLenum; nx: GLdouble; ny: GLdouble; nz: GLdouble); extdecl;
  glNormalStream3dvATI: procedure(stream: GLenum; const coords: PGLdouble); extdecl;
  glClientActiveVertexStreamATI: procedure(stream: GLenum); extdecl;
  glVertexBlendEnviATI: procedure(pname: GLenum; param: GLint); extdecl;
  glVertexBlendEnvfATI: procedure(pname: GLenum; param: GLfloat); extdecl;

function Load_GL_ATI_vertex_streams(): Boolean;

//**** GL_ATI_element_array *****//
const
  GL_ELEMENT_ARRAY_ATI = $8768;
  GL_ELEMENT_ARRAY_TYPE_ATI = $8769;
  GL_ELEMENT_ARRAY_POINTER_ATI = $876A;
var
  glElementPointerATI: procedure(_type: GLenum; const _pointer: PGLvoid); extdecl;
  glDrawElementArrayATI: procedure(mode: GLenum; count: GLsizei); extdecl;
  glDrawRangeElementArrayATI: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei); extdecl;

function Load_GL_ATI_element_array(): Boolean;

//**** GL_SUN_mesh_array *****//
const
  GL_QUAD_MESH_SUN = $8614;
  GL_TRIANGLE_MESH_SUN = $8615;
var
  glDrawMeshArraysSUN: procedure(mode: GLenum; first: GLint; count: GLsizei; width: GLsizei); extdecl;

function Load_GL_SUN_mesh_array(): Boolean;

//**** GL_SUN_slice_accum *****//
const
  GL_SLICE_ACCUM_SUN = $85CC;

function Load_GL_SUN_slice_accum(): Boolean;

//**** GL_NV_multisample_filter_hint *****//
const
  GL_MULTISAMPLE_FILTER_HINT_NV = $8534;

function Load_GL_NV_multisample_filter_hint(): Boolean;

//**** GL_NV_depth_clamp *****//
const
  GL_DEPTH_CLAMP_NV = $864F;

function Load_GL_NV_depth_clamp(): Boolean;

//**** GL_NV_occlusion_query *****//
const
  GL_PIXEL_COUNTER_BITS_NV = $8864;
  GL_CURRENT_OCCLUSION_QUERY_ID_NV = $8865;
  GL_PIXEL_COUNT_NV = $8866;
  GL_PIXEL_COUNT_AVAILABLE_NV = $8867;
var
  glGenOcclusionQueriesNV: procedure(n: GLsizei; ids: PGLuint); extdecl;
  glDeleteOcclusionQueriesNV: procedure(n: GLsizei; const ids: PGLuint); extdecl;
  glIsOcclusionQueryNV: function(id: GLuint): GLboolean; extdecl;
  glBeginOcclusionQueryNV: procedure(id: GLuint); extdecl;
  glEndOcclusionQueryNV: procedure(); extdecl;
  glGetOcclusionQueryivNV: procedure(id: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetOcclusionQueryuivNV: procedure(id: GLuint; pname: GLenum; params: PGLuint); extdecl;

function Load_GL_NV_occlusion_query(): Boolean;

//**** GL_NV_point_sprite *****//
const
  GL_POINT_SPRITE_NV = $8861;
  GL_COORD_REPLACE_NV = $8862;
  GL_POINT_SPRITE_R_MODE_NV = $8863;
var
  glPointParameteriNV: procedure(pname: GLenum; param: GLint); extdecl;
  glPointParameterivNV: procedure(pname: GLenum; const params: PGLint); extdecl;

function Load_GL_NV_point_sprite(): Boolean;

//**** GL_NV_texture_shader3 *****//
const
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV = $8850;
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV = $8851;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8852;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV = $8853;
  GL_OFFSET_HILO_TEXTURE_2D_NV = $8854;
  GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV = $8855;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV = $8856;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8857;
  GL_DEPENDENT_HILO_TEXTURE_2D_NV = $8858;
  GL_DEPENDENT_RGB_TEXTURE_3D_NV = $8859;
  GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV = $885A;
  GL_DOT_PRODUCT_PASS_THROUGH_NV = $885B;
  GL_DOT_PRODUCT_TEXTURE_1D_NV = $885C;
  GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV = $885D;
  GL_HILO8_NV = $885E;
  GL_SIGNED_HILO8_NV = $885F;
  GL_FORCE_BLUE_TO_ONE_NV = $8860;

function Load_GL_NV_texture_shader3(): Boolean;

//**** GL_NV_vertex_program1_1 *****//

function Load_GL_NV_vertex_program1_1(): Boolean;

//**** GL_EXT_shadow_funcs *****//

function Load_GL_EXT_shadow_funcs(): Boolean;

//**** GL_EXT_stencil_two_side *****//
const
  GL_STENCIL_TEST_TWO_SIDE_EXT = $8910;
  GL_ACTIVE_STENCIL_FACE_EXT = $8911;
var
  glActiveStencilFaceEXT: procedure(face: GLenum); extdecl;

function Load_GL_EXT_stencil_two_side(): Boolean;

//**** GL_ATI_text_fragment_shader *****//
const
  GL_TEXT_FRAGMENT_SHADER_ATI = $8200;

function Load_GL_ATI_text_fragment_shader(): Boolean;

//**** GL_APPLE_client_storage *****//
const
  GL_UNPACK_CLIENT_STORAGE_APPLE = $85B2;

function Load_GL_APPLE_client_storage(): Boolean;

//**** GL_APPLE_element_array *****//
const
  GL_ELEMENT_ARRAY_APPLE = $8768;
  GL_ELEMENT_ARRAY_TYPE_APPLE = $8769;
  GL_ELEMENT_ARRAY_POINTER_APPLE = $876A;
var
  glElementPointerAPPLE: procedure(_type: GLenum; const _pointer: PGLvoid); extdecl;
  glDrawElementArrayAPPLE: procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  glDrawRangeElementArrayAPPLE: procedure(mode: GLenum; start: GLuint; _end: GLuint; first: GLint; count: GLsizei); extdecl;
  glMultiDrawElementArrayAPPLE: procedure(mode: GLenum; const first: PGLint; const count: PGLsizei; primcount: GLsizei); extdecl;
  glMultiDrawRangeElementArrayAPPLE: procedure(mode: GLenum; start: GLuint; _end: GLuint; const first: PGLint; const count: PGLsizei; primcount: GLsizei); extdecl;

function Load_GL_APPLE_element_array(): Boolean;

//**** GL_APPLE_fence *****//
const
  GL_DRAW_PIXELS_APPLE = $8A0A;
  GL_FENCE_APPLE = $8A0B;
var
  glGenFencesAPPLE: procedure(n: GLsizei; fences: PGLuint); extdecl;
  glDeleteFencesAPPLE: procedure(n: GLsizei; const fences: PGLuint); extdecl;
  glSetFenceAPPLE: procedure(fence: GLuint); extdecl;
  glIsFenceAPPLE: function(fence: GLuint): GLboolean; extdecl;
  glTestFenceAPPLE: function(fence: GLuint): GLboolean; extdecl;
  glFinishFenceAPPLE: procedure(fence: GLuint); extdecl;
  glTestObjectAPPLE: function(_object: GLenum; name: GLuint): GLboolean; extdecl;
  glFinishObjectAPPLE: procedure(_object: GLenum; name: GLint); extdecl;

function Load_GL_APPLE_fence(): Boolean;

//**** GL_APPLE_vertex_array_object *****//
const
  GL_VERTEX_ARRAY_BINDING_APPLE = $85B5;
var
  glBindVertexArrayAPPLE: procedure(_array: GLuint); extdecl;
  glDeleteVertexArraysAPPLE: procedure(n: GLsizei; const arrays: PGLuint); extdecl;
  glGenVertexArraysAPPLE: procedure(n: GLsizei; arrays: PGLuint); extdecl;
  glIsVertexArrayAPPLE: function(_array: GLuint): GLboolean; extdecl;

function Load_GL_APPLE_vertex_array_object(): Boolean;

//**** GL_APPLE_vertex_array_range *****//
const
  GL_VERTEX_ARRAY_RANGE_APPLE = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE = $851E;
  GL_VERTEX_ARRAY_STORAGE_HINT_APPLE = $851F;
  GL_VERTEX_ARRAY_RANGE_POINTER_APPLE = $8521;
  GL_STORAGE_CACHED_APPLE = $85BE;
  GL_STORAGE_SHARED_APPLE = $85BF;
var
  glVertexArrayRangeAPPLE: procedure(length: GLsizei; _pointer: PGLvoid); extdecl;
  glFlushVertexArrayRangeAPPLE: procedure(length: GLsizei; _pointer: PGLvoid); extdecl;
  glVertexArrayParameteriAPPLE: procedure(pname: GLenum; param: GLint); extdecl;

function Load_GL_APPLE_vertex_array_range(): Boolean;

//**** GL_APPLE_ycbcr_422 *****//
const
  GL_YCBCR_422_APPLE = $85B9;
  GL_UNSIGNED_SHORT_8_8_APPLE = $85BA;
  GL_UNSIGNED_SHORT_8_8_REV_APPLE = $85BB;

function Load_GL_APPLE_ycbcr_422(): Boolean;

//**** GL_S3_s3tc *****//
const
  GL_RGB_S3TC = $83A0;
  GL_RGB4_S3TC = $83A1;
  GL_RGBA_S3TC = $83A2;
  GL_RGBA4_S3TC = $83A3;

function Load_GL_S3_s3tc(): Boolean;

//**** GL_ATI_draw_buffers *****//
const
  GL_MAX_DRAW_BUFFERS_ATI = $8824;
  GL_DRAW_BUFFER0_ATI = $8825;
  GL_DRAW_BUFFER1_ATI = $8826;
  GL_DRAW_BUFFER2_ATI = $8827;
  GL_DRAW_BUFFER3_ATI = $8828;
  GL_DRAW_BUFFER4_ATI = $8829;
  GL_DRAW_BUFFER5_ATI = $882A;
  GL_DRAW_BUFFER6_ATI = $882B;
  GL_DRAW_BUFFER7_ATI = $882C;
  GL_DRAW_BUFFER8_ATI = $882D;
  GL_DRAW_BUFFER9_ATI = $882E;
  GL_DRAW_BUFFER10_ATI = $882F;
  GL_DRAW_BUFFER11_ATI = $8830;
  GL_DRAW_BUFFER12_ATI = $8831;
  GL_DRAW_BUFFER13_ATI = $8832;
  GL_DRAW_BUFFER14_ATI = $8833;
  GL_DRAW_BUFFER15_ATI = $8834;
var
  glDrawBuffersATI: procedure(n: GLsizei; const bufs: PGLenum); extdecl;

function Load_GL_ATI_draw_buffers(): Boolean;

//**** GL_ATI_pixel_format_float *****//
const
  GL_TYPE_RGBA_FLOAT_ATI = $8820;
  GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI = $8835;
(* This is really a WGL extension, but defines some associated GL enums.
 * ATI does not export "GL_ATI_pixel_format_float" in the GL_EXTENSIONS string.
 
*)

function Load_GL_ATI_pixel_format_float(): Boolean;

//**** GL_ATI_texture_env_combine3 *****//
const
  GL_MODULATE_ADD_ATI = $8744;
  GL_MODULATE_SIGNED_ADD_ATI = $8745;
  GL_MODULATE_SUBTRACT_ATI = $8746;

function Load_GL_ATI_texture_env_combine3(): Boolean;

//**** GL_ATI_texture_float *****//
const
  GL_RGBA_FLOAT32_ATI = $8814;
  GL_RGB_FLOAT32_ATI = $8815;
  GL_ALPHA_FLOAT32_ATI = $8816;
  GL_INTENSITY_FLOAT32_ATI = $8817;
  GL_LUMINANCE_FLOAT32_ATI = $8818;
  GL_LUMINANCE_ALPHA_FLOAT32_ATI = $8819;
  GL_RGBA_FLOAT16_ATI = $881A;
  GL_RGB_FLOAT16_ATI = $881B;
  GL_ALPHA_FLOAT16_ATI = $881C;
  GL_INTENSITY_FLOAT16_ATI = $881D;
  GL_LUMINANCE_FLOAT16_ATI = $881E;
  GL_LUMINANCE_ALPHA_FLOAT16_ATI = $881F;

function Load_GL_ATI_texture_float(): Boolean;

//**** GL_NV_float_buffer *****//
const
  GL_FLOAT_R_NV = $8880;
  GL_FLOAT_RG_NV = $8881;
  GL_FLOAT_RGB_NV = $8882;
  GL_FLOAT_RGBA_NV = $8883;
  GL_FLOAT_R16_NV = $8884;
  GL_FLOAT_R32_NV = $8885;
  GL_FLOAT_RG16_NV = $8886;
  GL_FLOAT_RG32_NV = $8887;
  GL_FLOAT_RGB16_NV = $8888;
  GL_FLOAT_RGB32_NV = $8889;
  GL_FLOAT_RGBA16_NV = $888A;
  GL_FLOAT_RGBA32_NV = $888B;
  GL_TEXTURE_FLOAT_COMPONENTS_NV = $888C;
  GL_FLOAT_CLEAR_COLOR_VALUE_NV = $888D;
  GL_FLOAT_RGBA_MODE_NV = $888E;

function Load_GL_NV_float_buffer(): Boolean;

//**** GL_NV_fragment_program *****//
const
  GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV = $8868;
  GL_FRAGMENT_PROGRAM_NV = $8870;
  GL_MAX_TEXTURE_COORDS_NV = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_NV = $8872;
  GL_FRAGMENT_PROGRAM_BINDING_NV = $8873;
  GL_PROGRAM_ERROR_STRING_NV = $8874;
// Some NV_fragment_program entry points are shared with ARB_vertex_program. 
var
  glProgramNamedParameter4fNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glProgramNamedParameter4dNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glProgramNamedParameter4fvNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; const v: PGLfloat); extdecl;
  glProgramNamedParameter4dvNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; const v: PGLdouble); extdecl;
  glGetProgramNamedParameterfvNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; params: PGLfloat); extdecl;
  glGetProgramNamedParameterdvNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; params: PGLdouble); extdecl;

function Load_GL_NV_fragment_program(): Boolean;

//**** GL_NV_half_float *****//
const
  GL_HALF_FLOAT_NV = $140B;
var
  glVertex2hNV: procedure(x: GLhalfNV; y: GLhalfNV); extdecl;
  glVertex2hvNV: procedure(const v: PGLhalfNV); extdecl;
  glVertex3hNV: procedure(x: GLhalfNV; y: GLhalfNV; z: GLhalfNV); extdecl;
  glVertex3hvNV: procedure(const v: PGLhalfNV); extdecl;
  glVertex4hNV: procedure(x: GLhalfNV; y: GLhalfNV; z: GLhalfNV; w: GLhalfNV); extdecl;
  glVertex4hvNV: procedure(const v: PGLhalfNV); extdecl;
  glNormal3hNV: procedure(nx: GLhalfNV; ny: GLhalfNV; nz: GLhalfNV); extdecl;
  glNormal3hvNV: procedure(const v: PGLhalfNV); extdecl;
  glColor3hNV: procedure(red: GLhalfNV; green: GLhalfNV; blue: GLhalfNV); extdecl;
  glColor3hvNV: procedure(const v: PGLhalfNV); extdecl;
  glColor4hNV: procedure(red: GLhalfNV; green: GLhalfNV; blue: GLhalfNV; alpha: GLhalfNV); extdecl;
  glColor4hvNV: procedure(const v: PGLhalfNV); extdecl;
  glTexCoord1hNV: procedure(s: GLhalfNV); extdecl;
  glTexCoord1hvNV: procedure(const v: PGLhalfNV); extdecl;
  glTexCoord2hNV: procedure(s: GLhalfNV; t: GLhalfNV); extdecl;
  glTexCoord2hvNV: procedure(const v: PGLhalfNV); extdecl;
  glTexCoord3hNV: procedure(s: GLhalfNV; t: GLhalfNV; r: GLhalfNV); extdecl;
  glTexCoord3hvNV: procedure(const v: PGLhalfNV); extdecl;
  glTexCoord4hNV: procedure(s: GLhalfNV; t: GLhalfNV; r: GLhalfNV; q: GLhalfNV); extdecl;
  glTexCoord4hvNV: procedure(const v: PGLhalfNV); extdecl;
  glMultiTexCoord1hNV: procedure(target: GLenum; s: GLhalfNV); extdecl;
  glMultiTexCoord1hvNV: procedure(target: GLenum; const v: PGLhalfNV); extdecl;
  glMultiTexCoord2hNV: procedure(target: GLenum; s: GLhalfNV; t: GLhalfNV); extdecl;
  glMultiTexCoord2hvNV: procedure(target: GLenum; const v: PGLhalfNV); extdecl;
  glMultiTexCoord3hNV: procedure(target: GLenum; s: GLhalfNV; t: GLhalfNV; r: GLhalfNV); extdecl;
  glMultiTexCoord3hvNV: procedure(target: GLenum; const v: PGLhalfNV); extdecl;
  glMultiTexCoord4hNV: procedure(target: GLenum; s: GLhalfNV; t: GLhalfNV; r: GLhalfNV; q: GLhalfNV); extdecl;
  glMultiTexCoord4hvNV: procedure(target: GLenum; const v: PGLhalfNV); extdecl;
  glFogCoordhNV: procedure(fog: GLhalfNV); extdecl;
  glFogCoordhvNV: procedure(const fog: PGLhalfNV); extdecl;
  glSecondaryColor3hNV: procedure(red: GLhalfNV; green: GLhalfNV; blue: GLhalfNV); extdecl;
  glSecondaryColor3hvNV: procedure(const v: PGLhalfNV); extdecl;
  glVertexWeighthNV: procedure(weight: GLhalfNV); extdecl;
  glVertexWeighthvNV: procedure(const weight: PGLhalfNV); extdecl;
  glVertexAttrib1hNV: procedure(index: GLuint; x: GLhalfNV); extdecl;
  glVertexAttrib1hvNV: procedure(index: GLuint; const v: PGLhalfNV); extdecl;
  glVertexAttrib2hNV: procedure(index: GLuint; x: GLhalfNV; y: GLhalfNV); extdecl;
  glVertexAttrib2hvNV: procedure(index: GLuint; const v: PGLhalfNV); extdecl;
  glVertexAttrib3hNV: procedure(index: GLuint; x: GLhalfNV; y: GLhalfNV; z: GLhalfNV); extdecl;
  glVertexAttrib3hvNV: procedure(index: GLuint; const v: PGLhalfNV); extdecl;
  glVertexAttrib4hNV: procedure(index: GLuint; x: GLhalfNV; y: GLhalfNV; z: GLhalfNV; w: GLhalfNV); extdecl;
  glVertexAttrib4hvNV: procedure(index: GLuint; const v: PGLhalfNV); extdecl;
  glVertexAttribs1hvNV: procedure(index: GLuint; n: GLsizei; const v: PGLhalfNV); extdecl;
  glVertexAttribs2hvNV: procedure(index: GLuint; n: GLsizei; const v: PGLhalfNV); extdecl;
  glVertexAttribs3hvNV: procedure(index: GLuint; n: GLsizei; const v: PGLhalfNV); extdecl;
  glVertexAttribs4hvNV: procedure(index: GLuint; n: GLsizei; const v: PGLhalfNV); extdecl;

function Load_GL_NV_half_float(): Boolean;

//**** GL_NV_pixel_data_range *****//
const
  GL_WRITE_PIXEL_DATA_RANGE_NV = $8878;
  GL_READ_PIXEL_DATA_RANGE_NV = $8879;
  GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV = $887A;
  GL_READ_PIXEL_DATA_RANGE_LENGTH_NV = $887B;
  GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV = $887C;
  GL_READ_PIXEL_DATA_RANGE_POINTER_NV = $887D;
var
  glPixelDataRangeNV: procedure(target: GLenum; length: GLsizei; _pointer: PGLvoid); extdecl;
  glFlushPixelDataRangeNV: procedure(target: GLenum); extdecl;

function Load_GL_NV_pixel_data_range(): Boolean;

//**** GL_NV_primitive_restart *****//
const
  GL_PRIMITIVE_RESTART_NV = $8558;
  GL_PRIMITIVE_RESTART_INDEX_NV = $8559;
var
  glPrimitiveRestartNV: procedure(); extdecl;
  glPrimitiveRestartIndexNV: procedure(index: GLuint); extdecl;

function Load_GL_NV_primitive_restart(): Boolean;

//**** GL_NV_texture_expand_normal *****//
const
  GL_TEXTURE_UNSIGNED_REMAP_MODE_NV = $888F;

function Load_GL_NV_texture_expand_normal(): Boolean;

//**** GL_NV_vertex_program2 *****//

function Load_GL_NV_vertex_program2(): Boolean;

//**** GL_ATI_map_object_buffer *****//
var
  glMapObjectBufferATI: function(buffer: GLuint): PGLvoid; extdecl;
  glUnmapObjectBufferATI: procedure(buffer: GLuint); extdecl;

function Load_GL_ATI_map_object_buffer(): Boolean;

//**** GL_ATI_separate_stencil *****//
const
  GL_STENCIL_BACK_FUNC_ATI = $8800;
  GL_STENCIL_BACK_FAIL_ATI = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI = $8803;
var
  glStencilOpSeparateATI: procedure(face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); extdecl;
  glStencilFuncSeparateATI: procedure(frontfunc: GLenum; backfunc: GLenum; ref: GLint; mask: GLuint); extdecl;

function Load_GL_ATI_separate_stencil(): Boolean;

//**** GL_ATI_vertex_attrib_array_object *****//
var
  glVertexAttribArrayObjectATI: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; buffer: GLuint; offset: GLuint); extdecl;
  glGetVertexAttribArrayObjectfvATI: procedure(index: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetVertexAttribArrayObjectivATI: procedure(index: GLuint; pname: GLenum; params: PGLint); extdecl;

function Load_GL_ATI_vertex_attrib_array_object(): Boolean;

//**** GL_OES_read_format *****//
const
  GL_IMPLEMENTATION_COLOR_READ_TYPE_OES = $8B9A;
  GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES = $8B9B;

function Load_GL_OES_read_format(): Boolean;

//**** GL_EXT_depth_bounds_test *****//
const
  GL_DEPTH_BOUNDS_TEST_EXT = $8890;
  GL_DEPTH_BOUNDS_EXT = $8891;
var
  glDepthBoundsEXT: procedure(zmin: GLclampd; zmax: GLclampd); extdecl;

function Load_GL_EXT_depth_bounds_test(): Boolean;

//**** GL_EXT_texture_mirror_clamp *****//
const
  GL_MIRROR_CLAMP_EXT = $8742;
  GL_MIRROR_CLAMP_TO_EDGE_EXT = $8743;
  GL_MIRROR_CLAMP_TO_BORDER_EXT = $8912;

function Load_GL_EXT_texture_mirror_clamp(): Boolean;

//**** GL_EXT_blend_equation_separate *****//
const
  GL_BLEND_EQUATION_RGB_EXT = $8009;
  GL_BLEND_EQUATION_ALPHA_EXT = $883D;
var
  glBlendEquationSeparateEXT: procedure(modeRGB: GLenum; modeAlpha: GLenum); extdecl;

function Load_GL_EXT_blend_equation_separate(): Boolean;

//**** GL_MESA_pack_invert *****//
const
  GL_PACK_INVERT_MESA = $8758;

function Load_GL_MESA_pack_invert(): Boolean;

//**** GL_MESA_ycbcr_texture *****//
const
  GL_UNSIGNED_SHORT_8_8_MESA = $85BA;
  GL_UNSIGNED_SHORT_8_8_REV_MESA = $85BB;
  GL_YCBCR_MESA = $8757;

function Load_GL_MESA_ycbcr_texture(): Boolean;

//**** GL_EXT_pixel_buffer_object *****//
const
  GL_PIXEL_PACK_BUFFER_EXT = $88EB;
  GL_PIXEL_UNPACK_BUFFER_EXT = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING_EXT = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING_EXT = $88EF;

function Load_GL_EXT_pixel_buffer_object(): Boolean;

//**** GL_NV_fragment_program_option *****//

function Load_GL_NV_fragment_program_option(): Boolean;

//**** GL_NV_fragment_program2 *****//
const
  GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV = $88F4;
  GL_MAX_PROGRAM_CALL_DEPTH_NV = $88F5;
  GL_MAX_PROGRAM_IF_DEPTH_NV = $88F6;
  GL_MAX_PROGRAM_LOOP_DEPTH_NV = $88F7;
  GL_MAX_PROGRAM_LOOP_COUNT_NV = $88F8;

function Load_GL_NV_fragment_program2(): Boolean;

//**** GL_NV_vertex_program2_option *****//
// reuse GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV 
// reuse GL_MAX_PROGRAM_CALL_DEPTH_NV 

function Load_GL_NV_vertex_program2_option(): Boolean;

//**** GL_NV_vertex_program3 *****//
// reuse GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB 

function Load_GL_NV_vertex_program3(): Boolean;

//**** GL_EXT_framebuffer_object *****//
const
  GL_INVALID_FRAMEBUFFER_OPERATION_EXT = $0506;
  GL_MAX_RENDERBUFFER_SIZE_EXT = $84E8;
  GL_FRAMEBUFFER_BINDING_EXT = $8CA6;
  GL_RENDERBUFFER_BINDING_EXT = $8CA7;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT = $8CD4;
  GL_FRAMEBUFFER_COMPLETE_EXT = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT = $8CD9;
  GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT = $8CDA;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED_EXT = $8CDD;
  GL_MAX_COLOR_ATTACHMENTS_EXT = $8CDF;
  GL_COLOR_ATTACHMENT0_EXT = $8CE0;
  GL_COLOR_ATTACHMENT1_EXT = $8CE1;
  GL_COLOR_ATTACHMENT2_EXT = $8CE2;
  GL_COLOR_ATTACHMENT3_EXT = $8CE3;
  GL_COLOR_ATTACHMENT4_EXT = $8CE4;
  GL_COLOR_ATTACHMENT5_EXT = $8CE5;
  GL_COLOR_ATTACHMENT6_EXT = $8CE6;
  GL_COLOR_ATTACHMENT7_EXT = $8CE7;
  GL_COLOR_ATTACHMENT8_EXT = $8CE8;
  GL_COLOR_ATTACHMENT9_EXT = $8CE9;
  GL_COLOR_ATTACHMENT10_EXT = $8CEA;
  GL_COLOR_ATTACHMENT11_EXT = $8CEB;
  GL_COLOR_ATTACHMENT12_EXT = $8CEC;
  GL_COLOR_ATTACHMENT13_EXT = $8CED;
  GL_COLOR_ATTACHMENT14_EXT = $8CEE;
  GL_COLOR_ATTACHMENT15_EXT = $8CEF;
  GL_DEPTH_ATTACHMENT_EXT = $8D00;
  GL_STENCIL_ATTACHMENT_EXT = $8D20;
  GL_FRAMEBUFFER_EXT = $8D40;
  GL_RENDERBUFFER_EXT = $8D41;
  GL_RENDERBUFFER_WIDTH_EXT = $8D42;
  GL_RENDERBUFFER_HEIGHT_EXT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT_EXT = $8D44;
  GL_STENCIL_INDEX1_EXT = $8D46;
  GL_STENCIL_INDEX4_EXT = $8D47;
  GL_STENCIL_INDEX8_EXT = $8D48;
  GL_STENCIL_INDEX16_EXT = $8D49;
  GL_RENDERBUFFER_RED_SIZE_EXT = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE_EXT = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE_EXT = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE_EXT = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE_EXT = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE_EXT = $8D55;
var
  glIsRenderbufferEXT: function(renderbuffer: GLuint): GLboolean; extdecl;
  glBindRenderbufferEXT: procedure(target: GLenum; renderbuffer: GLuint); extdecl;
  glDeleteRenderbuffersEXT: procedure(n: GLsizei; const renderbuffers: PGLuint); extdecl;
  glGenRenderbuffersEXT: procedure(n: GLsizei; renderbuffers: PGLuint); extdecl;
  glRenderbufferStorageEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); extdecl;
  glGetRenderbufferParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glIsFramebufferEXT: function(framebuffer: GLuint): GLboolean; extdecl;
  glBindFramebufferEXT: procedure(target: GLenum; framebuffer: GLuint); extdecl;
  glDeleteFramebuffersEXT: procedure(n: GLsizei; const framebuffers: PGLuint); extdecl;
  glGenFramebuffersEXT: procedure(n: GLsizei; framebuffers: PGLuint); extdecl;
  glCheckFramebufferStatusEXT: function(target: GLenum): GLenum; extdecl;
  glFramebufferTexture1DEXT: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); extdecl;
  glFramebufferTexture2DEXT: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); extdecl;
  glFramebufferTexture3DEXT: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); extdecl;
  glFramebufferRenderbufferEXT: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); extdecl;
  glGetFramebufferAttachmentParameterivEXT: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGenerateMipmapEXT: procedure(target: GLenum); extdecl;

function Load_GL_EXT_framebuffer_object(): Boolean;

//**** GL_GREMEDY_string_marker *****//
var
  glStringMarkerGREMEDY: procedure(len: GLsizei; const _string: PGLvoid); extdecl;

function Load_GL_GREMEDY_string_marker(): Boolean;

//**** GL_EXT_packed_depth_stencil *****//
const
  GL_DEPTH_STENCIL_EXT = $84F9;
  GL_UNSIGNED_INT_24_8_EXT = $84FA;
  GL_DEPTH24_STENCIL8_EXT = $88F0;
  GL_TEXTURE_STENCIL_SIZE_EXT = $88F1;

function Load_GL_EXT_packed_depth_stencil(): Boolean;

//**** GL_EXT_stencil_clear_tag *****//
const
  GL_STENCIL_TAG_BITS_EXT = $88F2;
  GL_STENCIL_CLEAR_TAG_VALUE_EXT = $88F3;
var
  glStencilClearTagEXT: procedure(stencilTagBits: GLsizei; stencilClearTag: GLuint); extdecl;

function Load_GL_EXT_stencil_clear_tag(): Boolean;

//**** GL_EXT_texture_sRGB *****//
const
  GL_SRGB_EXT = $8C40;
  GL_SRGB8_EXT = $8C41;
  GL_SRGB_ALPHA_EXT = $8C42;
  GL_SRGB8_ALPHA8_EXT = $8C43;
  GL_SLUMINANCE_ALPHA_EXT = $8C44;
  GL_SLUMINANCE8_ALPHA8_EXT = $8C45;
  GL_SLUMINANCE_EXT = $8C46;
  GL_SLUMINANCE8_EXT = $8C47;
  GL_COMPRESSED_SRGB_EXT = $8C48;
  GL_COMPRESSED_SRGB_ALPHA_EXT = $8C49;
  GL_COMPRESSED_SLUMINANCE_EXT = $8C4A;
  GL_COMPRESSED_SLUMINANCE_ALPHA_EXT = $8C4B;
  GL_COMPRESSED_SRGB_S3TC_DXT1_EXT = $8C4C;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = $8C4D;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = $8C4E;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = $8C4F;

function Load_GL_EXT_texture_sRGB(): Boolean;

//**** GL_EXT_framebuffer_blit *****//
const
  GL_READ_FRAMEBUFFER_EXT = $8CA8;
  GL_DRAW_FRAMEBUFFER_EXT = $8CA9;
  GL_DRAW_FRAMEBUFFER_BINDING_EXT = GL_FRAMEBUFFER_BINDING_EXT;
  GL_READ_FRAMEBUFFER_BINDING_EXT = $8CAA;
var
  glBlitFramebufferEXT: procedure(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum); extdecl;

function Load_GL_EXT_framebuffer_blit(): Boolean;

//**** GL_EXT_framebuffer_multisample *****//
const
  GL_RENDERBUFFER_SAMPLES_EXT = $8CAB;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_EXT = $8D56;
  GL_MAX_SAMPLES_EXT = $8D57;
var
  glRenderbufferStorageMultisampleEXT: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); extdecl;

function Load_GL_EXT_framebuffer_multisample(): Boolean;

//**** GL_MESAX_texture_stack *****//
const
  GL_TEXTURE_1D_STACK_MESAX = $8759;
  GL_TEXTURE_2D_STACK_MESAX = $875A;
  GL_PROXY_TEXTURE_1D_STACK_MESAX = $875B;
  GL_PROXY_TEXTURE_2D_STACK_MESAX = $875C;
  GL_TEXTURE_1D_STACK_BINDING_MESAX = $875D;
  GL_TEXTURE_2D_STACK_BINDING_MESAX = $875E;

function Load_GL_MESAX_texture_stack(): Boolean;

//**** GL_EXT_timer_query *****//
const
  GL_TIME_ELAPSED_EXT = $88BF;
var
  glGetQueryObjecti64vEXT: procedure(id: GLuint; pname: GLenum; params: PGLint64EXT); extdecl;
  glGetQueryObjectui64vEXT: procedure(id: GLuint; pname: GLenum; params: PGLuint64EXT); extdecl;

function Load_GL_EXT_timer_query(): Boolean;

//**** GL_EXT_gpu_program_parameters *****//
var
  glProgramEnvParameters4fvEXT: procedure(target: GLenum; index: GLuint; count: GLsizei; const params: PGLfloat); extdecl;
  glProgramLocalParameters4fvEXT: procedure(target: GLenum; index: GLuint; count: GLsizei; const params: PGLfloat); extdecl;

function Load_GL_EXT_gpu_program_parameters(): Boolean;

//**** GL_APPLE_flush_buffer_range *****//
const
  GL_BUFFER_SERIALIZED_MODIFY_APPLE = $8A12;
  GL_BUFFER_FLUSHING_UNMAP_APPLE = $8A13;
var
  glBufferParameteriAPPLE: procedure(target: GLenum; pname: GLenum; param: GLint); extdecl;
  glFlushMappedBufferRangeAPPLE: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr); extdecl;

function Load_GL_APPLE_flush_buffer_range(): Boolean;

//**** GL_NV_gpu_program4 *****//
const
  GL_MIN_PROGRAM_TEXEL_OFFSET_NV = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET_NV = $8905;
  GL_PROGRAM_ATTRIB_COMPONENTS_NV = $8906;
  GL_PROGRAM_RESULT_COMPONENTS_NV = $8907;
  GL_MAX_PROGRAM_ATTRIB_COMPONENTS_NV = $8908;
  GL_MAX_PROGRAM_RESULT_COMPONENTS_NV = $8909;
  GL_MAX_PROGRAM_GENERIC_ATTRIBS_NV = $8DA5;
  GL_MAX_PROGRAM_GENERIC_RESULTS_NV = $8DA6;
var
  glProgramLocalParameterI4iNV: procedure(target: GLenum; index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); extdecl;
  glProgramLocalParameterI4ivNV: procedure(target: GLenum; index: GLuint; const params: PGLint); extdecl;
  glProgramLocalParametersI4ivNV: procedure(target: GLenum; index: GLuint; count: GLsizei; const params: PGLint); extdecl;
  glProgramLocalParameterI4uiNV: procedure(target: GLenum; index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); extdecl;
  glProgramLocalParameterI4uivNV: procedure(target: GLenum; index: GLuint; const params: PGLuint); extdecl;
  glProgramLocalParametersI4uivNV: procedure(target: GLenum; index: GLuint; count: GLsizei; const params: PGLuint); extdecl;
  glProgramEnvParameterI4iNV: procedure(target: GLenum; index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); extdecl;
  glProgramEnvParameterI4ivNV: procedure(target: GLenum; index: GLuint; const params: PGLint); extdecl;
  glProgramEnvParametersI4ivNV: procedure(target: GLenum; index: GLuint; count: GLsizei; const params: PGLint); extdecl;
  glProgramEnvParameterI4uiNV: procedure(target: GLenum; index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); extdecl;
  glProgramEnvParameterI4uivNV: procedure(target: GLenum; index: GLuint; const params: PGLuint); extdecl;
  glProgramEnvParametersI4uivNV: procedure(target: GLenum; index: GLuint; count: GLsizei; const params: PGLuint); extdecl;
  glGetProgramLocalParameterIivNV: procedure(target: GLenum; index: GLuint; params: PGLint); extdecl;
  glGetProgramLocalParameterIuivNV: procedure(target: GLenum; index: GLuint; params: PGLuint); extdecl;
  glGetProgramEnvParameterIivNV: procedure(target: GLenum; index: GLuint; params: PGLint); extdecl;
  glGetProgramEnvParameterIuivNV: procedure(target: GLenum; index: GLuint; params: PGLuint); extdecl;

function Load_GL_NV_gpu_program4(): Boolean;

//**** GL_NV_geometry_program4 *****//
const
  GL_LINES_ADJACENCY_EXT = $000A;
  GL_LINE_STRIP_ADJACENCY_EXT = $000B;
  GL_TRIANGLES_ADJACENCY_EXT = $000C;
  GL_TRIANGLE_STRIP_ADJACENCY_EXT = $000D;
  GL_GEOMETRY_PROGRAM_NV = $8C26;
  GL_MAX_PROGRAM_OUTPUT_VERTICES_NV = $8C27;
  GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV = $8C28;
  GL_GEOMETRY_VERTICES_OUT_EXT = $8DDA;
  GL_GEOMETRY_INPUT_TYPE_EXT = $8DDB;
  GL_GEOMETRY_OUTPUT_TYPE_EXT = $8DDC;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT = $8C29;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT = $8DA7;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT = $8DA8;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT = $8DA9;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT = $8CD4;
  GL_PROGRAM_POINT_SIZE_EXT = $8642;
var
  glProgramVertexLimitNV: procedure(target: GLenum; limit: GLint); extdecl;
  glFramebufferTextureEXT: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint); extdecl; (* Also used in GL_EXT_geometry_shader4 *)
  glFramebufferTextureLayerEXT: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); extdecl; (* Also used in GL_EXT_geometry_shader4 GL_EXT_texture_array *)
  glFramebufferTextureFaceEXT: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; face: GLenum); extdecl; (* Also used in GL_EXT_geometry_shader4 *)

function Load_GL_NV_geometry_program4(): Boolean;

//**** GL_EXT_geometry_shader4 *****//
const
  GL_GEOMETRY_SHADER_EXT = $8DD9;
// reuse GL_GEOMETRY_VERTICES_OUT_EXT 
// reuse GL_GEOMETRY_INPUT_TYPE_EXT 
// reuse GL_GEOMETRY_OUTPUT_TYPE_EXT 
// reuse GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT 
const
  GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT = $8DDD;
  GL_MAX_VERTEX_VARYING_COMPONENTS_EXT = $8DDE;
  GL_MAX_VARYING_COMPONENTS_EXT = $8B4B;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_EXT = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES_EXT = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_EXT = $8DE1;
// reuse GL_LINES_ADJACENCY_EXT 
// reuse GL_LINE_STRIP_ADJACENCY_EXT 
// reuse GL_TRIANGLES_ADJACENCY_EXT 
// reuse GL_TRIANGLE_STRIP_ADJACENCY_EXT 
// reuse GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT 
// reuse GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT 
// reuse GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT 
// reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT 
// reuse GL_PROGRAM_POINT_SIZE_EXT 
var
  glProgramParameteriEXT: procedure(_program: GLuint; pname: GLenum; value: GLint); extdecl;

function Load_GL_EXT_geometry_shader4(): Boolean;

//**** GL_NV_vertex_program4 *****//
const
  GL_VERTEX_ATTRIB_ARRAY_INTEGER_NV = $88FD;
var
  glVertexAttribI1iEXT: procedure(index: GLuint; x: GLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI2iEXT: procedure(index: GLuint; x: GLint; y: GLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI3iEXT: procedure(index: GLuint; x: GLint; y: GLint; z: GLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI4iEXT: procedure(index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI1uiEXT: procedure(index: GLuint; x: GLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI2uiEXT: procedure(index: GLuint; x: GLuint; y: GLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI3uiEXT: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI4uiEXT: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI1ivEXT: procedure(index: GLuint; const v: PGLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI2ivEXT: procedure(index: GLuint; const v: PGLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI3ivEXT: procedure(index: GLuint; const v: PGLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI4ivEXT: procedure(index: GLuint; const v: PGLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI1uivEXT: procedure(index: GLuint; const v: PGLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI2uivEXT: procedure(index: GLuint; const v: PGLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI3uivEXT: procedure(index: GLuint; const v: PGLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI4uivEXT: procedure(index: GLuint; const v: PGLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI4bvEXT: procedure(index: GLuint; const v: PGLbyte); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI4svEXT: procedure(index: GLuint; const v: PGLshort); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI4ubvEXT: procedure(index: GLuint; const v: PGLubyte); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribI4usvEXT: procedure(index: GLuint; const v: PGLushort); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glVertexAttribIPointerEXT: procedure(index: GLuint; size: GLint; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glGetVertexAttribIivEXT: procedure(index: GLuint; pname: GLenum; params: PGLint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)
  glGetVertexAttribIuivEXT: procedure(index: GLuint; pname: GLenum; params: PGLuint); extdecl; (* Also used in GL_EXT_gpu_shader4 *)

function Load_GL_NV_vertex_program4(): Boolean;

//**** GL_EXT_gpu_shader4 *****//
const
  GL_SAMPLER_1D_ARRAY_EXT = $8DC0;
  GL_SAMPLER_2D_ARRAY_EXT = $8DC1;
  GL_SAMPLER_BUFFER_EXT = $8DC2;
  GL_SAMPLER_1D_ARRAY_SHADOW_EXT = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW_EXT = $8DC4;
  GL_SAMPLER_CUBE_SHADOW_EXT = $8DC5;
  GL_UNSIGNED_INT_VEC2_EXT = $8DC6;
  GL_UNSIGNED_INT_VEC3_EXT = $8DC7;
  GL_UNSIGNED_INT_VEC4_EXT = $8DC8;
  GL_INT_SAMPLER_1D_EXT = $8DC9;
  GL_INT_SAMPLER_2D_EXT = $8DCA;
  GL_INT_SAMPLER_3D_EXT = $8DCB;
  GL_INT_SAMPLER_CUBE_EXT = $8DCC;
  GL_INT_SAMPLER_2D_RECT_EXT = $8DCD;
  GL_INT_SAMPLER_1D_ARRAY_EXT = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY_EXT = $8DCF;
  GL_INT_SAMPLER_BUFFER_EXT = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_1D_EXT = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D_EXT = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D_EXT = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE_EXT = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT = $8DD5;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT = $8DD7;
  GL_UNSIGNED_INT_SAMPLER_BUFFER_EXT = $8DD8;
var
  glGetUniformuivEXT: procedure(_program: GLuint; location: GLint; params: PGLuint); extdecl;
  glBindFragDataLocationEXT: procedure(_program: GLuint; color: GLuint; const name: PGLchar); extdecl;
  glGetFragDataLocationEXT: function(_program: GLuint; const name: PGLchar): GLint; extdecl;
  glUniform1uiEXT: procedure(location: GLint; v0: GLuint); extdecl;
  glUniform2uiEXT: procedure(location: GLint; v0: GLuint; v1: GLuint); extdecl;
  glUniform3uiEXT: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); extdecl;
  glUniform4uiEXT: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); extdecl;
  glUniform1uivEXT: procedure(location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glUniform2uivEXT: procedure(location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glUniform3uivEXT: procedure(location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glUniform4uivEXT: procedure(location: GLint; count: GLsizei; const value: PGLuint); extdecl;

function Load_GL_EXT_gpu_shader4(): Boolean;

//**** GL_EXT_draw_instanced *****//
var
  glDrawArraysInstancedEXT: procedure(mode: GLenum; start: GLint; count: GLsizei; primcount: GLsizei); extdecl;
  glDrawElementsInstancedEXT: procedure(mode: GLenum; count: GLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei); extdecl;

function Load_GL_EXT_draw_instanced(): Boolean;

//**** GL_EXT_packed_float *****//
const
  GL_R11F_G11F_B10F_EXT = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV_EXT = $8C3B;
  GL_RGBA_SIGNED_COMPONENTS_EXT = $8C3C;

function Load_GL_EXT_packed_float(): Boolean;

//**** GL_EXT_texture_array *****//
const
  GL_TEXTURE_1D_ARRAY_EXT = $8C18;
  GL_PROXY_TEXTURE_1D_ARRAY_EXT = $8C19;
  GL_TEXTURE_2D_ARRAY_EXT = $8C1A;
  GL_PROXY_TEXTURE_2D_ARRAY_EXT = $8C1B;
  GL_TEXTURE_BINDING_1D_ARRAY_EXT = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY_EXT = $8C1D;
  GL_MAX_ARRAY_TEXTURE_LAYERS_EXT = $88FF;
  GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT = $884E;
// reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT 

function Load_GL_EXT_texture_array(): Boolean;

//**** GL_EXT_texture_buffer_object *****//
const
  GL_TEXTURE_BUFFER_EXT = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE_EXT = $8C2B;
  GL_TEXTURE_BINDING_BUFFER_EXT = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING_EXT = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT_EXT = $8C2E;
var
  glTexBufferEXT: procedure(target: GLenum; internalformat: GLenum; buffer: GLuint); extdecl;

function Load_GL_EXT_texture_buffer_object(): Boolean;

//**** GL_EXT_texture_compression_latc *****//
const
  GL_COMPRESSED_LUMINANCE_LATC1_EXT = $8C70;
  GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT = $8C71;
  GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT = $8C72;
  GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT = $8C73;

function Load_GL_EXT_texture_compression_latc(): Boolean;

//**** GL_EXT_texture_compression_rgtc *****//
const
  GL_COMPRESSED_RED_RGTC1_EXT = $8DBB;
  GL_COMPRESSED_SIGNED_RED_RGTC1_EXT = $8DBC;
  GL_COMPRESSED_RED_GREEN_RGTC2_EXT = $8DBD;
  GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT = $8DBE;

function Load_GL_EXT_texture_compression_rgtc(): Boolean;

//**** GL_EXT_texture_shared_exponent *****//
const
  GL_RGB9_E5_EXT = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV_EXT = $8C3E;
  GL_TEXTURE_SHARED_SIZE_EXT = $8C3F;

function Load_GL_EXT_texture_shared_exponent(): Boolean;

//**** GL_NV_depth_buffer_float *****//
const
  GL_DEPTH_COMPONENT32F_NV = $8DAB;
  GL_DEPTH32F_STENCIL8_NV = $8DAC;
  GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV = $8DAD;
  GL_DEPTH_BUFFER_FLOAT_MODE_NV = $8DAF;
var
  glDepthRangedNV: procedure(zNear: GLdouble; zFar: GLdouble); extdecl;
  glClearDepthdNV: procedure(depth: GLdouble); extdecl;
  glDepthBoundsdNV: procedure(zmin: GLdouble; zmax: GLdouble); extdecl;

function Load_GL_NV_depth_buffer_float(): Boolean;

//**** GL_NV_fragment_program4 *****//

function Load_GL_NV_fragment_program4(): Boolean;

//**** GL_NV_framebuffer_multisample_coverage *****//
const
  GL_RENDERBUFFER_COVERAGE_SAMPLES_NV = $8CAB;
  GL_RENDERBUFFER_COLOR_SAMPLES_NV = $8E10;
  GL_MAX_MULTISAMPLE_COVERAGE_MODES_NV = $8E11;
  GL_MULTISAMPLE_COVERAGE_MODES_NV = $8E12;
var
  glRenderbufferStorageMultisampleCoverageNV: procedure(target: GLenum; coverageSamples: GLsizei; colorSamples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); extdecl;

function Load_GL_NV_framebuffer_multisample_coverage(): Boolean;

//**** GL_EXT_framebuffer_sRGB *****//
const
  GL_FRAMEBUFFER_SRGB_EXT = $8DB9;
  GL_FRAMEBUFFER_SRGB_CAPABLE_EXT = $8DBA;

function Load_GL_EXT_framebuffer_sRGB(): Boolean;

//**** GL_NV_geometry_shader4 *****//

function Load_GL_NV_geometry_shader4(): Boolean;

//**** GL_NV_parameter_buffer_object *****//
const
  GL_MAX_PROGRAM_PARAMETER_BUFFER_BINDINGS_NV = $8DA0;
  GL_MAX_PROGRAM_PARAMETER_BUFFER_SIZE_NV = $8DA1;
  GL_VERTEX_PROGRAM_PARAMETER_BUFFER_NV = $8DA2;
  GL_GEOMETRY_PROGRAM_PARAMETER_BUFFER_NV = $8DA3;
  GL_FRAGMENT_PROGRAM_PARAMETER_BUFFER_NV = $8DA4;
var
  glProgramBufferParametersfvNV: procedure(target: GLenum; buffer: GLuint; index: GLuint; count: GLsizei; const params: PGLfloat); extdecl;
  glProgramBufferParametersIivNV: procedure(target: GLenum; buffer: GLuint; index: GLuint; count: GLsizei; const params: PGLint); extdecl;
  glProgramBufferParametersIuivNV: procedure(target: GLenum; buffer: GLuint; index: GLuint; count: GLsizei; const params: PGLuint); extdecl;

function Load_GL_NV_parameter_buffer_object(): Boolean;

//**** GL_EXT_draw_buffers2 *****//
var
  glColorMaskIndexedEXT: procedure(index: GLuint; r: GLboolean; g: GLboolean; b: GLboolean; a: GLboolean); extdecl;
  glGetBooleanIndexedvEXT: procedure(target: GLenum; index: GLuint; data: PGLboolean); extdecl; (* Also used in GL_NV_transform_feedback GL_EXT_transform_feedback GL_NV_explicit_multisample *)
  glGetIntegerIndexedvEXT: procedure(target: GLenum; index: GLuint; data: PGLint); extdecl; (* Also used in GL_NV_parameter_buffer_object GL_NV_transform_feedback GL_EXT_transform_feedback GL_NV_explicit_multisample *)
  glEnableIndexedEXT: procedure(target: GLenum; index: GLuint); extdecl;
  glDisableIndexedEXT: procedure(target: GLenum; index: GLuint); extdecl;
  glIsEnabledIndexedEXT: function(target: GLenum; index: GLuint): GLboolean; extdecl;

function Load_GL_EXT_draw_buffers2(): Boolean;

//**** GL_NV_transform_feedback *****//
const
  GL_BACK_PRIMARY_COLOR_NV = $8C77;
  GL_BACK_SECONDARY_COLOR_NV = $8C78;
  GL_TEXTURE_COORD_NV = $8C79;
  GL_CLIP_DISTANCE_NV = $8C7A;
  GL_VERTEX_ID_NV = $8C7B;
  GL_PRIMITIVE_ID_NV = $8C7C;
  GL_GENERIC_ATTRIB_NV = $8C7D;
  GL_TRANSFORM_FEEDBACK_ATTRIBS_NV = $8C7E;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV = $8C7F;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV = $8C80;
  GL_ACTIVE_VARYINGS_NV = $8C81;
  GL_ACTIVE_VARYING_MAX_LENGTH_NV = $8C82;
  GL_TRANSFORM_FEEDBACK_VARYINGS_NV = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_START_NV = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV = $8C85;
  GL_TRANSFORM_FEEDBACK_RECORD_NV = $8C86;
  GL_PRIMITIVES_GENERATED_NV = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV = $8C88;
  GL_RASTERIZER_DISCARD_NV = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_ATTRIBS_NV = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV = $8C8B;
  GL_INTERLEAVED_ATTRIBS_NV = $8C8C;
  GL_SEPARATE_ATTRIBS_NV = $8C8D;
  GL_TRANSFORM_FEEDBACK_BUFFER_NV = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV = $8C8F;
var
  glBeginTransformFeedbackNV: procedure(primitiveMode: GLenum); extdecl;
  glEndTransformFeedbackNV: procedure(); extdecl;
  glTransformFeedbackAttribsNV: procedure(count: GLuint; const attribs: PGLint; bufferMode: GLenum); extdecl;
  glBindBufferRangeNV: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); extdecl; (* Also used in GL_NV_parameter_buffer_object *)
  glBindBufferOffsetNV: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: GLintptr); extdecl; (* Also used in GL_NV_parameter_buffer_object *)
  glBindBufferBaseNV: procedure(target: GLenum; index: GLuint; buffer: GLuint); extdecl; (* Also used in GL_NV_parameter_buffer_object *)
  glTransformFeedbackVaryingsNV: procedure(_program: GLuint; count: GLsizei; const varyings: PPGLchar; bufferMode: GLenum); extdecl;
  glActiveVaryingNV: procedure(_program: GLuint; const name: PGLchar); extdecl;
  glGetVaryingLocationNV: function(_program: GLuint; const name: PGLchar): GLint; extdecl;
  glGetActiveVaryingNV: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLsizei; _type: PGLenum; name: PGLchar); extdecl;
  glGetTransformFeedbackVaryingNV: procedure(_program: GLuint; index: GLuint; location: PGLint); extdecl;

function Load_GL_NV_transform_feedback(): Boolean;

//**** GL_EXT_bindable_uniform *****//
const
  GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT = $8DE2;
  GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT = $8DE3;
  GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT = $8DE4;
  GL_MAX_BINDABLE_UNIFORM_SIZE_EXT = $8DED;
  GL_UNIFORM_BUFFER_EXT = $8DEE;
  GL_UNIFORM_BUFFER_BINDING_EXT = $8DEF;
var
  glUniformBufferEXT: procedure(_program: GLuint; location: GLint; buffer: GLuint); extdecl;
  glGetUniformBufferSizeEXT: function(_program: GLuint; location: GLint): GLint; extdecl;
  glGetUniformOffsetEXT: function(_program: GLuint; location: GLint): GLintptr; extdecl;

function Load_GL_EXT_bindable_uniform(): Boolean;

//**** GL_EXT_texture_integer *****//
const
  GL_RGBA32UI_EXT = $8D70;
  GL_RGB32UI_EXT = $8D71;
  GL_ALPHA32UI_EXT = $8D72;
  GL_INTENSITY32UI_EXT = $8D73;
  GL_LUMINANCE32UI_EXT = $8D74;
  GL_LUMINANCE_ALPHA32UI_EXT = $8D75;
  GL_RGBA16UI_EXT = $8D76;
  GL_RGB16UI_EXT = $8D77;
  GL_ALPHA16UI_EXT = $8D78;
  GL_INTENSITY16UI_EXT = $8D79;
  GL_LUMINANCE16UI_EXT = $8D7A;
  GL_LUMINANCE_ALPHA16UI_EXT = $8D7B;
  GL_RGBA8UI_EXT = $8D7C;
  GL_RGB8UI_EXT = $8D7D;
  GL_ALPHA8UI_EXT = $8D7E;
  GL_INTENSITY8UI_EXT = $8D7F;
  GL_LUMINANCE8UI_EXT = $8D80;
  GL_LUMINANCE_ALPHA8UI_EXT = $8D81;
  GL_RGBA32I_EXT = $8D82;
  GL_RGB32I_EXT = $8D83;
  GL_ALPHA32I_EXT = $8D84;
  GL_INTENSITY32I_EXT = $8D85;
  GL_LUMINANCE32I_EXT = $8D86;
  GL_LUMINANCE_ALPHA32I_EXT = $8D87;
  GL_RGBA16I_EXT = $8D88;
  GL_RGB16I_EXT = $8D89;
  GL_ALPHA16I_EXT = $8D8A;
  GL_INTENSITY16I_EXT = $8D8B;
  GL_LUMINANCE16I_EXT = $8D8C;
  GL_LUMINANCE_ALPHA16I_EXT = $8D8D;
  GL_RGBA8I_EXT = $8D8E;
  GL_RGB8I_EXT = $8D8F;
  GL_ALPHA8I_EXT = $8D90;
  GL_INTENSITY8I_EXT = $8D91;
  GL_LUMINANCE8I_EXT = $8D92;
  GL_LUMINANCE_ALPHA8I_EXT = $8D93;
  GL_RED_INTEGER_EXT = $8D94;
  GL_GREEN_INTEGER_EXT = $8D95;
  GL_BLUE_INTEGER_EXT = $8D96;
  GL_ALPHA_INTEGER_EXT = $8D97;
  GL_RGB_INTEGER_EXT = $8D98;
  GL_RGBA_INTEGER_EXT = $8D99;
  GL_BGR_INTEGER_EXT = $8D9A;
  GL_BGRA_INTEGER_EXT = $8D9B;
  GL_LUMINANCE_INTEGER_EXT = $8D9C;
  GL_LUMINANCE_ALPHA_INTEGER_EXT = $8D9D;
  GL_RGBA_INTEGER_MODE_EXT = $8D9E;
var
  glTexParameterIivEXT: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexParameterIuivEXT: procedure(target: GLenum; pname: GLenum; const params: PGLuint); extdecl;
  glGetTexParameterIivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetTexParameterIuivEXT: procedure(target: GLenum; pname: GLenum; params: PGLuint); extdecl;
  glClearColorIiEXT: procedure(red: GLint; green: GLint; blue: GLint; alpha: GLint); extdecl;
  glClearColorIuiEXT: procedure(red: GLuint; green: GLuint; blue: GLuint; alpha: GLuint); extdecl;

function Load_GL_EXT_texture_integer(): Boolean;

//**** GL_GREMEDY_frame_terminator *****//
var
  glFrameTerminatorGREMEDY: procedure(); extdecl;

function Load_GL_GREMEDY_frame_terminator(): Boolean;

//**** GL_NV_conditional_render *****//
const
  GL_QUERY_WAIT_NV = $8E13;
  GL_QUERY_NO_WAIT_NV = $8E14;
  GL_QUERY_BY_REGION_WAIT_NV = $8E15;
  GL_QUERY_BY_REGION_NO_WAIT_NV = $8E16;
var
  glBeginConditionalRenderNV: procedure(id: GLuint; mode: GLenum); extdecl;
  glEndConditionalRenderNV: procedure(); extdecl;

function Load_GL_NV_conditional_render(): Boolean;

//**** GL_NV_present_video *****//
const
  GL_FRAME_NV = $8E26;
  GL_FIELDS_NV = $8E27;
  GL_CURRENT_TIME_NV = $8E28;
  GL_NUM_FILL_STREAMS_NV = $8E29;
  GL_PRESENT_TIME_NV = $8E2A;
  GL_PRESENT_DURATION_NV = $8E2B;
var
  glPresentFrameKeyedNV: procedure(video_slot: GLuint; minPresentTime: GLuint64EXT; beginPresentTimeId: GLuint; presentDurationId: GLuint; _type: GLenum; target0: GLenum; fill0: GLuint; key0: GLuint; target1: GLenum; fill1: GLuint; key1: GLuint); extdecl;
  glPresentFrameDualFillNV: procedure(video_slot: GLuint; minPresentTime: GLuint64EXT; beginPresentTimeId: GLuint; presentDurationId: GLuint; _type: GLenum; target0: GLenum; fill0: GLuint; target1: GLenum; fill1: GLuint; target2: GLenum; fill2: GLuint; target3: GLenum; fill3: GLuint); extdecl;
  glGetVideoivNV: procedure(video_slot: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVideouivNV: procedure(video_slot: GLuint; pname: GLenum; params: PGLuint); extdecl;
  glGetVideoi64vNV: procedure(video_slot: GLuint; pname: GLenum; params: PGLint64EXT); extdecl;
  glGetVideoui64vNV: procedure(video_slot: GLuint; pname: GLenum; params: PGLuint64EXT); extdecl;

function Load_GL_NV_present_video(): Boolean;

//**** GL_EXT_transform_feedback *****//
const
  GL_TRANSFORM_FEEDBACK_BUFFER_EXT = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT = $8C85;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT = $8C8F;
  GL_INTERLEAVED_ATTRIBS_EXT = $8C8C;
  GL_SEPARATE_ATTRIBS_EXT = $8C8D;
  GL_PRIMITIVES_GENERATED_EXT = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT = $8C88;
  GL_RASTERIZER_DISCARD_EXT = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT = $8C8B;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS_EXT = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT = $8C7F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT = $8C76;
var
  glBeginTransformFeedbackEXT: procedure(primitiveMode: GLenum); extdecl;
  glEndTransformFeedbackEXT: procedure(); extdecl;
  glBindBufferRangeEXT: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); extdecl;
  glBindBufferOffsetEXT: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: GLintptr); extdecl;
  glBindBufferBaseEXT: procedure(target: GLenum; index: GLuint; buffer: GLuint); extdecl;
  glTransformFeedbackVaryingsEXT: procedure(_program: GLuint; count: GLsizei; const varyings: PPGLchar; bufferMode: GLenum); extdecl;
  glGetTransformFeedbackVaryingEXT: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLsizei; _type: PGLenum; name: PGLchar); extdecl;

function Load_GL_EXT_transform_feedback(): Boolean;

//**** GL_EXT_direct_state_access *****//
const
  GL_PROGRAM_MATRIX_EXT = $8E2D;
  GL_TRANSPOSE_PROGRAM_MATRIX_EXT = $8E2E;
  GL_PROGRAM_MATRIX_STACK_DEPTH_EXT = $8E2F;
var
  glClientAttribDefaultEXT: procedure(mask: GLbitfield); extdecl;
  glPushClientAttribDefaultEXT: procedure(mask: GLbitfield); extdecl;
  glMatrixLoadfEXT: procedure(mode: GLenum; const m: PGLfloat); extdecl;
  glMatrixLoaddEXT: procedure(mode: GLenum; const m: PGLdouble); extdecl;
  glMatrixMultfEXT: procedure(mode: GLenum; const m: PGLfloat); extdecl;
  glMatrixMultdEXT: procedure(mode: GLenum; const m: PGLdouble); extdecl;
  glMatrixLoadIdentityEXT: procedure(mode: GLenum); extdecl;
  glMatrixRotatefEXT: procedure(mode: GLenum; angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glMatrixRotatedEXT: procedure(mode: GLenum; angle: GLdouble; x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glMatrixScalefEXT: procedure(mode: GLenum; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glMatrixScaledEXT: procedure(mode: GLenum; x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glMatrixTranslatefEXT: procedure(mode: GLenum; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glMatrixTranslatedEXT: procedure(mode: GLenum; x: GLdouble; y: GLdouble; z: GLdouble); extdecl;
  glMatrixFrustumEXT: procedure(mode: GLenum; left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble); extdecl;
  glMatrixOrthoEXT: procedure(mode: GLenum; left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble); extdecl;
  glMatrixPopEXT: procedure(mode: GLenum); extdecl;
  glMatrixPushEXT: procedure(mode: GLenum); extdecl;
  glMatrixLoadTransposefEXT: procedure(mode: GLenum; const m: PGLfloat); extdecl;
  glMatrixLoadTransposedEXT: procedure(mode: GLenum; const m: PGLdouble); extdecl;
  glMatrixMultTransposefEXT: procedure(mode: GLenum; const m: PGLfloat); extdecl;
  glMatrixMultTransposedEXT: procedure(mode: GLenum; const m: PGLdouble); extdecl;
  glTextureParameterfEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glTextureParameterfvEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glTextureParameteriEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; param: GLint); extdecl;
  glTextureParameterivEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTextureImage1DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glTextureImage2DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glTextureSubImage1DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glTextureSubImage2DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glCopyTextureImage1DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; border: GLint); extdecl;
  glCopyTextureImage2DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); extdecl;
  glCopyTextureSubImage1DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; x: GLint; y: GLint; width: GLsizei); extdecl;
  glCopyTextureSubImage2DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); extdecl;
  glGetTextureImageEXT: procedure(texture: GLuint; target: GLenum; level: GLint; format: GLenum; _type: GLenum; pixels: PGLvoid); extdecl;
  glGetTextureParameterfvEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetTextureParameterivEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetTextureLevelParameterfvEXT: procedure(texture: GLuint; target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); extdecl;
  glGetTextureLevelParameterivEXT: procedure(texture: GLuint; target: GLenum; level: GLint; pname: GLenum; params: PGLint); extdecl;
  glTextureImage3DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glTextureSubImage3DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glCopyTextureSubImage3DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); extdecl;
  glMultiTexParameterfEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glMultiTexParameterfvEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glMultiTexParameteriEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLint); extdecl;
  glMultiTexParameterivEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glMultiTexImage1DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glMultiTexImage2DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glMultiTexSubImage1DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glMultiTexSubImage2DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glCopyMultiTexImage1DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; border: GLint); extdecl;
  glCopyMultiTexImage2DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); extdecl;
  glCopyMultiTexSubImage1DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; x: GLint; y: GLint; width: GLsizei); extdecl;
  glCopyMultiTexSubImage2DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); extdecl;
  glGetMultiTexImageEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; format: GLenum; _type: GLenum; pixels: PGLvoid); extdecl;
  glGetMultiTexParameterfvEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetMultiTexParameterivEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetMultiTexLevelParameterfvEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); extdecl;
  glGetMultiTexLevelParameterivEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; pname: GLenum; params: PGLint); extdecl;
  glMultiTexImage3DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glMultiTexSubImage3DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); extdecl;
  glCopyMultiTexSubImage3DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); extdecl;
  glBindMultiTextureEXT: procedure(texunit: GLenum; target: GLenum; texture: GLuint); extdecl;
  glEnableClientStateIndexedEXT: procedure(_array: GLenum; index: GLuint); extdecl;
  glDisableClientStateIndexedEXT: procedure(_array: GLenum; index: GLuint); extdecl;
  glMultiTexCoordPointerEXT: procedure(texunit: GLenum; size: GLint; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); extdecl;
  glMultiTexEnvfEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glMultiTexEnvfvEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glMultiTexEnviEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLint); extdecl;
  glMultiTexEnvivEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glMultiTexGendEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; param: GLdouble); extdecl;
  glMultiTexGendvEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; const params: PGLdouble); extdecl;
  glMultiTexGenfEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glMultiTexGenfvEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glMultiTexGeniEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; param: GLint); extdecl;
  glMultiTexGenivEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glGetMultiTexEnvfvEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetMultiTexEnvivEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetMultiTexGendvEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; params: PGLdouble); extdecl;
  glGetMultiTexGenfvEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; params: PGLfloat); extdecl;
  glGetMultiTexGenivEXT: procedure(texunit: GLenum; coord: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetFloatIndexedvEXT: procedure(target: GLenum; index: GLuint; data: PGLfloat); extdecl;
  glGetDoubleIndexedvEXT: procedure(target: GLenum; index: GLuint; data: PGLdouble); extdecl;
  glGetPointerIndexedvEXT: procedure(target: GLenum; index: GLuint; data: PPGLvoid); extdecl;
  glCompressedTextureImage3DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedTextureImage2DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedTextureImage1DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedTextureSubImage3DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedTextureSubImage2DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedTextureSubImage1DEXT: procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glGetCompressedTextureImageEXT: procedure(texture: GLuint; target: GLenum; lod: GLint; img: PGLvoid); extdecl;
  glCompressedMultiTexImage3DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedMultiTexImage2DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedMultiTexImage1DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedMultiTexSubImage3DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedMultiTexSubImage2DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glCompressedMultiTexSubImage1DEXT: procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); extdecl;
  glGetCompressedMultiTexImageEXT: procedure(texunit: GLenum; target: GLenum; lod: GLint; img: PGLvoid); extdecl;
  glNamedProgramStringEXT: procedure(_program: GLuint; target: GLenum; format: GLenum; len: GLsizei; const _string: PGLvoid); extdecl;
  glNamedProgramLocalParameter4dEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); extdecl;
  glNamedProgramLocalParameter4dvEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; const params: PGLdouble); extdecl;
  glNamedProgramLocalParameter4fEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glNamedProgramLocalParameter4fvEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; const params: PGLfloat); extdecl;
  glGetNamedProgramLocalParameterdvEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; params: PGLdouble); extdecl;
  glGetNamedProgramLocalParameterfvEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; params: PGLfloat); extdecl;
  glGetNamedProgramivEXT: procedure(_program: GLuint; target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetNamedProgramStringEXT: procedure(_program: GLuint; target: GLenum; pname: GLenum; _string: PGLvoid); extdecl;
  glNamedProgramLocalParameters4fvEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; count: GLsizei; const params: PGLfloat); extdecl;
  glNamedProgramLocalParameterI4iEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); extdecl;
  glNamedProgramLocalParameterI4ivEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; const params: PGLint); extdecl;
  glNamedProgramLocalParametersI4ivEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; count: GLsizei; const params: PGLint); extdecl;
  glNamedProgramLocalParameterI4uiEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); extdecl;
  glNamedProgramLocalParameterI4uivEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; const params: PGLuint); extdecl;
  glNamedProgramLocalParametersI4uivEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; count: GLsizei; const params: PGLuint); extdecl;
  glGetNamedProgramLocalParameterIivEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; params: PGLint); extdecl;
  glGetNamedProgramLocalParameterIuivEXT: procedure(_program: GLuint; target: GLenum; index: GLuint; params: PGLuint); extdecl;
  glTextureParameterIivEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTextureParameterIuivEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLuint); extdecl;
  glGetTextureParameterIivEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetTextureParameterIuivEXT: procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLuint); extdecl;
  glMultiTexParameterIivEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glMultiTexParameterIuivEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLuint); extdecl;
  glGetMultiTexParameterIivEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGetMultiTexParameterIuivEXT: procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLuint); extdecl;
  glProgramUniform1fEXT: procedure(_program: GLuint; location: GLint; v0: GLfloat); extdecl;
  glProgramUniform2fEXT: procedure(_program: GLuint; location: GLint; v0: GLfloat; v1: GLfloat); extdecl;
  glProgramUniform3fEXT: procedure(_program: GLuint; location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); extdecl;
  glProgramUniform4fEXT: procedure(_program: GLuint; location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); extdecl;
  glProgramUniform1iEXT: procedure(_program: GLuint; location: GLint; v0: GLint); extdecl;
  glProgramUniform2iEXT: procedure(_program: GLuint; location: GLint; v0: GLint; v1: GLint); extdecl;
  glProgramUniform3iEXT: procedure(_program: GLuint; location: GLint; v0: GLint; v1: GLint; v2: GLint); extdecl;
  glProgramUniform4iEXT: procedure(_program: GLuint; location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); extdecl;
  glProgramUniform1fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glProgramUniform2fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glProgramUniform3fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glProgramUniform4fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLfloat); extdecl;
  glProgramUniform1ivEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glProgramUniform2ivEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glProgramUniform3ivEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glProgramUniform4ivEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLint); extdecl;
  glProgramUniformMatrix2fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniformMatrix3fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniformMatrix4fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniformMatrix2x3fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniformMatrix3x2fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniformMatrix2x4fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniformMatrix4x2fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniformMatrix3x4fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniformMatrix4x3fvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); extdecl;
  glProgramUniform1uiEXT: procedure(_program: GLuint; location: GLint; v0: GLuint); extdecl;
  glProgramUniform2uiEXT: procedure(_program: GLuint; location: GLint; v0: GLuint; v1: GLuint); extdecl;
  glProgramUniform3uiEXT: procedure(_program: GLuint; location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); extdecl;
  glProgramUniform4uiEXT: procedure(_program: GLuint; location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); extdecl;
  glProgramUniform1uivEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glProgramUniform2uivEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glProgramUniform3uivEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glProgramUniform4uivEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLuint); extdecl;
  glNamedBufferDataEXT: procedure(buffer: GLuint; size: GLsizeiptr; const data: PGLvoid; usage: GLenum); extdecl;
  glNamedBufferSubDataEXT: procedure(buffer: GLuint; offset: GLintptr; size: GLsizeiptr; const data: PGLvoid); extdecl;
  glMapNamedBufferEXT: function(buffer: GLuint; access: GLenum): PGLvoid; extdecl;
  glUnmapNamedBufferEXT: function(buffer: GLuint): GLboolean; extdecl;
  glGetNamedBufferParameterivEXT: procedure(buffer: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetNamedBufferPointervEXT: procedure(buffer: GLuint; pname: GLenum; params: PPGLvoid); extdecl;
  glGetNamedBufferSubDataEXT: procedure(buffer: GLuint; offset: GLintptr; size: GLsizeiptr; data: PGLvoid); extdecl;
  glTextureBufferEXT: procedure(texture: GLuint; target: GLenum; internalformat: GLenum; buffer: GLuint); extdecl;
  glMultiTexBufferEXT: procedure(texunit: GLenum; target: GLenum; internalformat: GLenum; buffer: GLuint); extdecl;
  glNamedRenderbufferStorageEXT: procedure(renderbuffer: GLuint; internalformat: GLenum; width: GLsizei; height: GLsizei); extdecl;
  glGetNamedRenderbufferParameterivEXT: procedure(renderbuffer: GLuint; pname: GLenum; params: PGLint); extdecl;
  glCheckNamedFramebufferStatusEXT: function(framebuffer: GLuint; target: GLenum): GLenum; extdecl;
  glNamedFramebufferTexture1DEXT: procedure(framebuffer: GLuint; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); extdecl;
  glNamedFramebufferTexture2DEXT: procedure(framebuffer: GLuint; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); extdecl;
  glNamedFramebufferTexture3DEXT: procedure(framebuffer: GLuint; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); extdecl;
  glNamedFramebufferRenderbufferEXT: procedure(framebuffer: GLuint; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); extdecl;
  glGetNamedFramebufferAttachmentParameterivEXT: procedure(framebuffer: GLuint; attachment: GLenum; pname: GLenum; params: PGLint); extdecl;
  glGenerateTextureMipmapEXT: procedure(texture: GLuint; target: GLenum); extdecl;
  glGenerateMultiTexMipmapEXT: procedure(texunit: GLenum; target: GLenum); extdecl;
  glFramebufferDrawBufferEXT: procedure(framebuffer: GLuint; mode: GLenum); extdecl;
  glFramebufferDrawBuffersEXT: procedure(framebuffer: GLuint; n: GLsizei; const bufs: PGLenum); extdecl;
  glFramebufferReadBufferEXT: procedure(framebuffer: GLuint; mode: GLenum); extdecl;
  glGetFramebufferParameterivEXT: procedure(framebuffer: GLuint; pname: GLenum; params: PGLint); extdecl;
  glNamedRenderbufferStorageMultisampleEXT: procedure(renderbuffer: GLuint; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); extdecl;
  glNamedRenderbufferStorageMultisampleCoverageEXT: procedure(renderbuffer: GLuint; coverageSamples: GLsizei; colorSamples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); extdecl;
  glNamedFramebufferTextureEXT: procedure(framebuffer: GLuint; attachment: GLenum; texture: GLuint; level: GLint); extdecl;
  glNamedFramebufferTextureLayerEXT: procedure(framebuffer: GLuint; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); extdecl;
  glNamedFramebufferTextureFaceEXT: procedure(framebuffer: GLuint; attachment: GLenum; texture: GLuint; level: GLint; face: GLenum); extdecl;
  glTextureRenderbufferEXT: procedure(texture: GLuint; target: GLenum; renderbuffer: GLuint); extdecl;
  glMultiTexRenderbufferEXT: procedure(texunit: GLenum; target: GLenum; renderbuffer: GLuint); extdecl;

function Load_GL_EXT_direct_state_access(): Boolean;

//**** GL_EXT_vertex_array_bgra *****//
// reuse GL_BGRA 

function Load_GL_EXT_vertex_array_bgra(): Boolean;

//**** GL_EXT_texture_swizzle *****//
const
  GL_TEXTURE_SWIZZLE_R_EXT = $8E42;
  GL_TEXTURE_SWIZZLE_G_EXT = $8E43;
  GL_TEXTURE_SWIZZLE_B_EXT = $8E44;
  GL_TEXTURE_SWIZZLE_A_EXT = $8E45;
  GL_TEXTURE_SWIZZLE_RGBA_EXT = $8E46;

function Load_GL_EXT_texture_swizzle(): Boolean;

//**** GL_NV_explicit_multisample *****//
const
  GL_SAMPLE_POSITION_NV = $8E50;
  GL_SAMPLE_MASK_NV = $8E51;
  GL_SAMPLE_MASK_VALUE_NV = $8E52;
  GL_TEXTURE_BINDING_RENDERBUFFER_NV = $8E53;
  GL_TEXTURE_RENDERBUFFER_DATA_STORE_BINDING_NV = $8E54;
  GL_TEXTURE_RENDERBUFFER_NV = $8E55;
  GL_SAMPLER_RENDERBUFFER_NV = $8E56;
  GL_INT_SAMPLER_RENDERBUFFER_NV = $8E57;
  GL_UNSIGNED_INT_SAMPLER_RENDERBUFFER_NV = $8E58;
  GL_MAX_SAMPLE_MASK_WORDS_NV = $8E59;
var
  glGetMultisamplefvNV: procedure(pname: GLenum; index: GLuint; val: PGLfloat); extdecl;
  glSampleMaskIndexedNV: procedure(index: GLuint; mask: GLbitfield); extdecl;
  glTexRenderbufferNV: procedure(target: GLenum; renderbuffer: GLuint); extdecl;

function Load_GL_NV_explicit_multisample(): Boolean;

//**** GL_NV_transform_feedback2 *****//
const
  GL_TRANSFORM_FEEDBACK_NV = $8E22;
  GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED_NV = $8E23;
  GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE_NV = $8E24;
  GL_TRANSFORM_FEEDBACK_BINDING_NV = $8E25;
var
  glBindTransformFeedbackNV: procedure(target: GLenum; id: GLuint); extdecl;
  glDeleteTransformFeedbacksNV: procedure(n: GLsizei; const ids: PGLuint); extdecl;
  glGenTransformFeedbacksNV: procedure(n: GLsizei; ids: PGLuint); extdecl;
  glIsTransformFeedbackNV: function(id: GLuint): GLboolean; extdecl;
  glPauseTransformFeedbackNV: procedure(); extdecl;
  glResumeTransformFeedbackNV: procedure(); extdecl;
  glDrawTransformFeedbackNV: procedure(mode: GLenum; id: GLuint); extdecl;

function Load_GL_NV_transform_feedback2(): Boolean;

//**** GL_ATI_meminfo *****//
const
  GL_VBO_FREE_MEMORY_ATI = $87FB;
  GL_TEXTURE_FREE_MEMORY_ATI = $87FC;
  GL_RENDERBUFFER_FREE_MEMORY_ATI = $87FD;

function Load_GL_ATI_meminfo(): Boolean;

//**** GL_AMD_performance_monitor *****//
const
  GL_COUNTER_TYPE_AMD = $8BC0;
  GL_COUNTER_RANGE_AMD = $8BC1;
  GL_UNSIGNED_INT64_AMD = $8BC2;
  GL_PERCENTAGE_AMD = $8BC3;
  GL_PERFMON_RESULT_AVAILABLE_AMD = $8BC4;
  GL_PERFMON_RESULT_SIZE_AMD = $8BC5;
  GL_PERFMON_RESULT_AMD = $8BC6;
var
  glGetPerfMonitorGroupsAMD: procedure(numGroups: PGLint; groupsSize: GLsizei; groups: PGLuint); extdecl;
  glGetPerfMonitorCountersAMD: procedure(group: GLuint; numCounters: PGLint; maxActiveCounters: PGLint; counterSize: GLsizei; counters: PGLuint); extdecl;
  glGetPerfMonitorGroupStringAMD: procedure(group: GLuint; bufSize: GLsizei; length: PGLsizei; groupString: PGLchar); extdecl;
  glGetPerfMonitorCounterStringAMD: procedure(group: GLuint; counter: GLuint; bufSize: GLsizei; length: PGLsizei; counterString: PGLchar); extdecl;
  glGetPerfMonitorCounterInfoAMD: procedure(group: GLuint; counter: GLuint; pname: GLenum; data: Pointer); extdecl;
  glGenPerfMonitorsAMD: procedure(n: GLsizei; monitors: PGLuint); extdecl;
  glDeletePerfMonitorsAMD: procedure(n: GLsizei; monitors: PGLuint); extdecl;
  glSelectPerfMonitorCountersAMD: procedure(monitor: GLuint; enable: GLboolean; group: GLuint; numCounters: GLint; counterList: PGLuint); extdecl;
  glBeginPerfMonitorAMD: procedure(monitor: GLuint); extdecl;
  glEndPerfMonitorAMD: procedure(monitor: GLuint); extdecl;
  glGetPerfMonitorCounterDataAMD: procedure(monitor: GLuint; pname: GLenum; dataSize: GLsizei; data: PGLuint; bytesWritten: PGLint); extdecl;

function Load_GL_AMD_performance_monitor(): Boolean;

//**** GL_AMD_texture_texture4 *****//

function Load_GL_AMD_texture_texture4(): Boolean;

//**** GL_AMD_vertex_shader_tesselator *****//
const
  GL_SAMPLER_BUFFER_AMD = $9001;
  GL_INT_SAMPLER_BUFFER_AMD = $9002;
  GL_UNSIGNED_INT_SAMPLER_BUFFER_AMD = $9003;
  GL_TESSELLATION_MODE_AMD = $9004;
  GL_TESSELLATION_FACTOR_AMD = $9005;
  GL_DISCRETE_AMD = $9006;
  GL_CONTINUOUS_AMD = $9007;
var
  glTessellationFactorAMD: procedure(factor: GLfloat); extdecl;
  glTessellationModeAMD: procedure(mode: GLenum); extdecl;

function Load_GL_AMD_vertex_shader_tesselator(): Boolean;

//**** GL_EXT_provoking_vertex *****//
const
  GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION_EXT = $8E4C;
  GL_FIRST_VERTEX_CONVENTION_EXT = $8E4D;
  GL_LAST_VERTEX_CONVENTION_EXT = $8E4E;
  GL_PROVOKING_VERTEX_EXT = $8E4F;
var
  glProvokingVertexEXT: procedure(mode: GLenum); extdecl;

function Load_GL_EXT_provoking_vertex(): Boolean;

//**** GL_EXT_texture_snorm *****//
const
  GL_ALPHA_SNORM = $9010;
  GL_LUMINANCE_SNORM = $9011;
  GL_LUMINANCE_ALPHA_SNORM = $9012;
  GL_INTENSITY_SNORM = $9013;
  GL_ALPHA8_SNORM = $9014;
  GL_LUMINANCE8_SNORM = $9015;
  GL_LUMINANCE8_ALPHA8_SNORM = $9016;
  GL_INTENSITY8_SNORM = $9017;
  GL_ALPHA16_SNORM = $9018;
  GL_LUMINANCE16_SNORM = $9019;
  GL_LUMINANCE16_ALPHA16_SNORM = $901A;
  GL_INTENSITY16_SNORM = $901B;
// reuse GL_RED_SNORM 
// reuse GL_RG_SNORM 
// reuse GL_RGB_SNORM 
// reuse GL_RGBA_SNORM 
// reuse GL_R8_SNORM 
// reuse GL_RG8_SNORM 
// reuse GL_RGB8_SNORM 
// reuse GL_RGBA8_SNORM 
// reuse GL_R16_SNORM 
// reuse GL_RG16_SNORM 
// reuse GL_RGB16_SNORM 
// reuse GL_RGBA16_SNORM 
// reuse GL_SIGNED_NORMALIZED 

function Load_GL_EXT_texture_snorm(): Boolean;

//**** GL_AMD_draw_buffers_blend *****//
var
  glBlendFuncIndexedAMD: procedure(buf: GLuint; src: GLenum; dst: GLenum); extdecl;
  glBlendFuncSeparateIndexedAMD: procedure(buf: GLuint; srcRGB: GLenum; dstRGB: GLenum; srcAlpha: GLenum; dstAlpha: GLenum); extdecl;
  glBlendEquationIndexedAMD: procedure(buf: GLuint; mode: GLenum); extdecl;
  glBlendEquationSeparateIndexedAMD: procedure(buf: GLuint; modeRGB: GLenum; modeAlpha: GLenum); extdecl;

function Load_GL_AMD_draw_buffers_blend(): Boolean;

//**** GL_APPLE_texture_range *****//
const
  GL_TEXTURE_RANGE_LENGTH_APPLE = $85B7;
  GL_TEXTURE_RANGE_POINTER_APPLE = $85B8;
  GL_TEXTURE_STORAGE_HINT_APPLE = $85BC;
  GL_STORAGE_PRIVATE_APPLE = $85BD;
// reuse GL_STORAGE_CACHED_APPLE 
// reuse GL_STORAGE_SHARED_APPLE 
var
  glTextureRangeAPPLE: procedure(target: GLenum; length: GLsizei; const _pointer: PGLvoid); extdecl;
  glGetTexParameterPointervAPPLE: procedure(target: GLenum; pname: GLenum; params: PPGLvoid); extdecl;

function Load_GL_APPLE_texture_range(): Boolean;

//**** GL_APPLE_float_pixels *****//
const
  GL_HALF_APPLE = $140B;
  GL_RGBA_FLOAT32_APPLE = $8814;
  GL_RGB_FLOAT32_APPLE = $8815;
  GL_ALPHA_FLOAT32_APPLE = $8816;
  GL_INTENSITY_FLOAT32_APPLE = $8817;
  GL_LUMINANCE_FLOAT32_APPLE = $8818;
  GL_LUMINANCE_ALPHA_FLOAT32_APPLE = $8819;
  GL_RGBA_FLOAT16_APPLE = $881A;
  GL_RGB_FLOAT16_APPLE = $881B;
  GL_ALPHA_FLOAT16_APPLE = $881C;
  GL_INTENSITY_FLOAT16_APPLE = $881D;
  GL_LUMINANCE_FLOAT16_APPLE = $881E;
  GL_LUMINANCE_ALPHA_FLOAT16_APPLE = $881F;
  GL_COLOR_FLOAT_APPLE = $8A0F;

function Load_GL_APPLE_float_pixels(): Boolean;

//**** GL_APPLE_vertex_program_evaluators *****//
const
  GL_VERTEX_ATTRIB_MAP1_APPLE = $8A00;
  GL_VERTEX_ATTRIB_MAP2_APPLE = $8A01;
  GL_VERTEX_ATTRIB_MAP1_SIZE_APPLE = $8A02;
  GL_VERTEX_ATTRIB_MAP1_COEFF_APPLE = $8A03;
  GL_VERTEX_ATTRIB_MAP1_ORDER_APPLE = $8A04;
  GL_VERTEX_ATTRIB_MAP1_DOMAIN_APPLE = $8A05;
  GL_VERTEX_ATTRIB_MAP2_SIZE_APPLE = $8A06;
  GL_VERTEX_ATTRIB_MAP2_COEFF_APPLE = $8A07;
  GL_VERTEX_ATTRIB_MAP2_ORDER_APPLE = $8A08;
  GL_VERTEX_ATTRIB_MAP2_DOMAIN_APPLE = $8A09;
var
  glEnableVertexAttribAPPLE: procedure(index: GLuint; pname: GLenum); extdecl;
  glDisableVertexAttribAPPLE: procedure(index: GLuint; pname: GLenum); extdecl;
  glIsVertexAttribEnabledAPPLE: function(index: GLuint; pname: GLenum): GLboolean; extdecl;
  glMapVertexAttrib1dAPPLE: procedure(index: GLuint; size: GLuint; u1: GLdouble; u2: GLdouble; stride: GLint; order: GLint; const points: PGLdouble); extdecl;
  glMapVertexAttrib1fAPPLE: procedure(index: GLuint; size: GLuint; u1: GLfloat; u2: GLfloat; stride: GLint; order: GLint; const points: PGLfloat); extdecl;
  glMapVertexAttrib2dAPPLE: procedure(index: GLuint; size: GLuint; u1: GLdouble; u2: GLdouble; ustride: GLint; uorder: GLint; v1: GLdouble; v2: GLdouble; vstride: GLint; vorder: GLint; const points: PGLdouble); extdecl;
  glMapVertexAttrib2fAPPLE: procedure(index: GLuint; size: GLuint; u1: GLfloat; u2: GLfloat; ustride: GLint; uorder: GLint; v1: GLfloat; v2: GLfloat; vstride: GLint; vorder: GLint; const points: PGLfloat); extdecl;

function Load_GL_APPLE_vertex_program_evaluators(): Boolean;

//**** GL_APPLE_aux_depth_stencil *****//
const
  GL_AUX_DEPTH_STENCIL_APPLE = $8A14;

function Load_GL_APPLE_aux_depth_stencil(): Boolean;

//**** GL_APPLE_object_purgeable *****//
const
  GL_BUFFER_OBJECT_APPLE = $85B3;
  GL_RELEASED_APPLE = $8A19;
  GL_VOLATILE_APPLE = $8A1A;
  GL_RETAINED_APPLE = $8A1B;
  GL_UNDEFINED_APPLE = $8A1C;
  GL_PURGEABLE_APPLE = $8A1D;
var
  glObjectPurgeableAPPLE: function(objectType: GLenum; name: GLuint; option: GLenum): GLenum; extdecl;
  glObjectUnpurgeableAPPLE: function(objectType: GLenum; name: GLuint; option: GLenum): GLenum; extdecl;
  glGetObjectParameterivAPPLE: procedure(objectType: GLenum; name: GLuint; pname: GLenum; params: PGLint); extdecl;

function Load_GL_APPLE_object_purgeable(): Boolean;

//**** GL_APPLE_row_bytes *****//
const
  GL_PACK_ROW_BYTES_APPLE = $8A15;
  GL_UNPACK_ROW_BYTES_APPLE = $8A16;

function Load_GL_APPLE_row_bytes(): Boolean;

//**** GL_APPLE_rgb_422 *****//
const
  GL_RGB_422_APPLE = $8A1F;
// reuse GL_UNSIGNED_SHORT_8_8_APPLE 
// reuse GL_UNSIGNED_SHORT_8_8_REV_APPLE 

function Load_GL_APPLE_rgb_422(): Boolean;

//**** GL_NV_video_capture *****//
const
  GL_VIDEO_BUFFER_NV = $9020;
  GL_VIDEO_BUFFER_BINDING_NV = $9021;
  GL_FIELD_UPPER_NV = $9022;
  GL_FIELD_LOWER_NV = $9023;
  GL_NUM_VIDEO_CAPTURE_STREAMS_NV = $9024;
  GL_NEXT_VIDEO_CAPTURE_BUFFER_STATUS_NV = $9025;
  GL_VIDEO_CAPTURE_TO_422_SUPPORTED_NV = $9026;
  GL_LAST_VIDEO_CAPTURE_STATUS_NV = $9027;
  GL_VIDEO_BUFFER_PITCH_NV = $9028;
  GL_VIDEO_COLOR_CONVERSION_MATRIX_NV = $9029;
  GL_VIDEO_COLOR_CONVERSION_MAX_NV = $902A;
  GL_VIDEO_COLOR_CONVERSION_MIN_NV = $902B;
  GL_VIDEO_COLOR_CONVERSION_OFFSET_NV = $902C;
  GL_VIDEO_BUFFER_INTERNAL_FORMAT_NV = $902D;
  GL_PARTIAL_SUCCESS_NV = $902E;
  GL_SUCCESS_NV = $902F;
  GL_FAILURE_NV = $9030;
  GL_YCBYCR8_422_NV = $9031;
  GL_YCBAYCR8A_4224_NV = $9032;
  GL_Z6Y10Z6CB10Z6Y10Z6CR10_422_NV = $9033;
  GL_Z6Y10Z6CB10Z6A10Z6Y10Z6CR10Z6A10_4224_NV = $9034;
  GL_Z4Y12Z4CB12Z4Y12Z4CR12_422_NV = $9035;
  GL_Z4Y12Z4CB12Z4A12Z4Y12Z4CR12Z4A12_4224_NV = $9036;
  GL_Z4Y12Z4CB12Z4CR12_444_NV = $9037;
  GL_VIDEO_CAPTURE_FRAME_WIDTH_NV = $9038;
  GL_VIDEO_CAPTURE_FRAME_HEIGHT_NV = $9039;
  GL_VIDEO_CAPTURE_FIELD_UPPER_HEIGHT_NV = $903A;
  GL_VIDEO_CAPTURE_FIELD_LOWER_HEIGHT_NV = $903B;
  GL_VIDEO_CAPTURE_SURFACE_ORIGIN_NV = $903C;
var
  glBeginVideoCaptureNV: procedure(video_capture_slot: GLuint); extdecl;
  glBindVideoCaptureStreamBufferNV: procedure(video_capture_slot: GLuint; stream: GLuint; frame_region: GLenum; offset: GLintptrARB); extdecl;
  glBindVideoCaptureStreamTextureNV: procedure(video_capture_slot: GLuint; stream: GLuint; frame_region: GLenum; target: GLenum; texture: GLuint); extdecl;
  glEndVideoCaptureNV: procedure(video_capture_slot: GLuint); extdecl;
  glGetVideoCaptureivNV: procedure(video_capture_slot: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVideoCaptureStreamivNV: procedure(video_capture_slot: GLuint; stream: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVideoCaptureStreamfvNV: procedure(video_capture_slot: GLuint; stream: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetVideoCaptureStreamdvNV: procedure(video_capture_slot: GLuint; stream: GLuint; pname: GLenum; params: PGLdouble); extdecl;
  glVideoCaptureNV: function(video_capture_slot: GLuint; sequence_num: PGLuint; capture_time: PGLuint64EXT): GLenum; extdecl;
  glVideoCaptureStreamParameterivNV: procedure(video_capture_slot: GLuint; stream: GLuint; pname: GLenum; const params: PGLint); extdecl;
  glVideoCaptureStreamParameterfvNV: procedure(video_capture_slot: GLuint; stream: GLuint; pname: GLenum; const params: PGLfloat); extdecl;
  glVideoCaptureStreamParameterdvNV: procedure(video_capture_slot: GLuint; stream: GLuint; pname: GLenum; const params: PGLdouble); extdecl;

function Load_GL_NV_video_capture(): Boolean;

//**** GL_NV_copy_image *****//
var
  glCopyImageSubDataNV: procedure(srcName: GLuint; srcTarget: GLenum; srcLevel: GLint; srcX: GLint; srcY: GLint; srcZ: GLint; dstName: GLuint; dstTarget: GLenum; dstLevel: GLint; dstX: GLint; dstY: GLint; dstZ: GLint; width: GLsizei; height: GLsizei; depth: GLsizei); extdecl;

function Load_GL_NV_copy_image(): Boolean;

//**** GL_EXT_separate_shader_objects *****//
const
  GL_ACTIVE_PROGRAM_EXT = $8B8D;
var
  glUseShaderProgramEXT: procedure(_type: GLenum; _program: GLuint); extdecl;
  glActiveProgramEXT: procedure(_program: GLuint); extdecl;
  glCreateShaderProgramEXT: function(_type: GLenum; const _string: PGLchar): GLuint; extdecl;

function Load_GL_EXT_separate_shader_objects(): Boolean;

//**** GL_NV_parameter_buffer_object2 *****//

function Load_GL_NV_parameter_buffer_object2(): Boolean;

//**** GL_NV_shader_buffer_load *****//
const
  GL_BUFFER_GPU_ADDRESS_NV = $8F1D;
  GL_GPU_ADDRESS_NV = $8F34;
  GL_MAX_SHADER_BUFFER_ADDRESS_NV = $8F35;
var
  glMakeBufferResidentNV: procedure(target: GLenum; access: GLenum); extdecl;
  glMakeBufferNonResidentNV: procedure(target: GLenum); extdecl;
  glIsBufferResidentNV: function(target: GLenum): GLboolean; extdecl;
  glMakeNamedBufferResidentNV: procedure(buffer: GLuint; access: GLenum); extdecl;
  glMakeNamedBufferNonResidentNV: procedure(buffer: GLuint); extdecl;
  glIsNamedBufferResidentNV: function(buffer: GLuint): GLboolean; extdecl;
  glGetBufferParameterui64vNV: procedure(target: GLenum; pname: GLenum; params: PGLuint64EXT); extdecl;
  glGetNamedBufferParameterui64vNV: procedure(buffer: GLuint; pname: GLenum; params: PGLuint64EXT); extdecl;
  glGetIntegerui64vNV: procedure(value: GLenum; result: PGLuint64EXT); extdecl;
  glUniformui64NV: procedure(location: GLint; value: GLuint64EXT); extdecl;
  glUniformui64vNV: procedure(location: GLint; count: GLsizei; const value: PGLuint64EXT); extdecl;
  glGetUniformui64vNV: procedure(_program: GLuint; location: GLint; params: PGLuint64EXT); extdecl;
  glProgramUniformui64NV: procedure(_program: GLuint; location: GLint; value: GLuint64EXT); extdecl;
  glProgramUniformui64vNV: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLuint64EXT); extdecl;

function Load_GL_NV_shader_buffer_load(): Boolean;

//**** GL_NV_vertex_buffer_unified_memory *****//
const
  GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV = $8F1E;
  GL_ELEMENT_ARRAY_UNIFIED_NV = $8F1F;
  GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV = $8F20;
  GL_VERTEX_ARRAY_ADDRESS_NV = $8F21;
  GL_NORMAL_ARRAY_ADDRESS_NV = $8F22;
  GL_COLOR_ARRAY_ADDRESS_NV = $8F23;
  GL_INDEX_ARRAY_ADDRESS_NV = $8F24;
  GL_TEXTURE_COORD_ARRAY_ADDRESS_NV = $8F25;
  GL_EDGE_FLAG_ARRAY_ADDRESS_NV = $8F26;
  GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV = $8F27;
  GL_FOG_COORD_ARRAY_ADDRESS_NV = $8F28;
  GL_ELEMENT_ARRAY_ADDRESS_NV = $8F29;
  GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV = $8F2A;
  GL_VERTEX_ARRAY_LENGTH_NV = $8F2B;
  GL_NORMAL_ARRAY_LENGTH_NV = $8F2C;
  GL_COLOR_ARRAY_LENGTH_NV = $8F2D;
  GL_INDEX_ARRAY_LENGTH_NV = $8F2E;
  GL_TEXTURE_COORD_ARRAY_LENGTH_NV = $8F2F;
  GL_EDGE_FLAG_ARRAY_LENGTH_NV = $8F30;
  GL_SECONDARY_COLOR_ARRAY_LENGTH_NV = $8F31;
  GL_FOG_COORD_ARRAY_LENGTH_NV = $8F32;
  GL_ELEMENT_ARRAY_LENGTH_NV = $8F33;
var
  glBufferAddressRangeNV: procedure(pname: GLenum; index: GLuint; address: GLuint64EXT; length: GLsizeiptr); extdecl;
  glVertexFormatNV: procedure(size: GLint; _type: GLenum; stride: GLsizei); extdecl;
  glNormalFormatNV: procedure(_type: GLenum; stride: GLsizei); extdecl;
  glColorFormatNV: procedure(size: GLint; _type: GLenum; stride: GLsizei); extdecl;
  glIndexFormatNV: procedure(_type: GLenum; stride: GLsizei); extdecl;
  glTexCoordFormatNV: procedure(size: GLint; _type: GLenum; stride: GLsizei); extdecl;
  glEdgeFlagFormatNV: procedure(stride: GLsizei); extdecl;
  glSecondaryColorFormatNV: procedure(size: GLint; _type: GLenum; stride: GLsizei); extdecl;
  glFogCoordFormatNV: procedure(_type: GLenum; stride: GLsizei); extdecl;
  glVertexAttribFormatNV: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei); extdecl;
  glVertexAttribIFormatNV: procedure(index: GLuint; size: GLint; _type: GLenum; stride: GLsizei); extdecl;
  glGetIntegerui64i_vNV: procedure(value: GLenum; index: GLuint; result: PGLuint64EXT); extdecl;

function Load_GL_NV_vertex_buffer_unified_memory(): Boolean;

//**** GL_NV_texture_barrier *****//
var
  glTextureBarrierNV: procedure(); extdecl;

function Load_GL_NV_texture_barrier(): Boolean;

//**** GL_AMD_shader_stencil_export *****//

function Load_GL_AMD_shader_stencil_export(): Boolean;

//**** GL_AMD_seamless_cubemap_per_texture *****//
// reuse GL_TEXTURE_CUBE_MAP_SEAMLESS_ARB 
//***********************************************************

function Load_GL_AMD_seamless_cubemap_per_texture(): Boolean;

//**** GL_SGIX_texture_select *****//

function Load_GL_SGIX_texture_select(): Boolean;

//**** GL_INGR_blend_func_separate *****//
var
  glBlendFuncSeparateINGR: procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); extdecl;

function Load_GL_INGR_blend_func_separate(): Boolean;

//**** GL_SGIX_depth_pass_instrument *****//

function Load_GL_SGIX_depth_pass_instrument(): Boolean;

//**** GL_SGIX_igloo_interface *****//
var
  glIglooInterfaceSGIX: procedure(pname: GLenum; const params: PGLvoid); extdecl;

function Load_GL_SGIX_igloo_interface(): Boolean;

 

implementation

{$IFNDEF Windows}
function wglGetProcAddress(proc: PChar): Pointer;
begin
  Result := GetProcAddress(LibGL, proc);
end;
{$ENDIF}

function glext_ExtensionSupported(const extension: String; const searchIn: String): Boolean;
var
  extensions: string;
  start: PChar;
  where, terminator: PChar;
  n: GLint;
  i: GLint;
begin
  if not Assigned(glGetStringi) then
    Pointer(glGetStringi) := wglGetProcAddress('glGetStringi');

  if not Assigned(glGetStringi) then Exit(false);

  if extension = '' then
  begin
    Exit(False);
  end;

  glGetIntegerv(GL_NUM_EXTENSIONS, @n);

  Result := false;

  for i := 0 to n -1 do
  begin
    extensions :=PChar(glGetStringi(GL_EXTENSIONS, i));
    if SameText(extension, extensions) then
    begin
      exit(true);
    end;
  end;

end;



function Load_GL_VERSION_1_2(): Boolean;
begin
  Result := False;
  Pointer(glBlendColor) := wglGetProcAddress('glBlendColor');
  if not Assigned(glBlendColor) then Exit;
  Pointer(glBlendEquation) := wglGetProcAddress('glBlendEquation');
  if not Assigned(glBlendEquation) then Exit;
  Pointer(glDrawRangeElements) := wglGetProcAddress('glDrawRangeElements');
  if not Assigned(glDrawRangeElements) then Exit;
  Pointer(glTexImage3D) := wglGetProcAddress('glTexImage3D');
  if not Assigned(glTexImage3D) then Exit;
  Pointer(glTexSubImage3D) := wglGetProcAddress('glTexSubImage3D');
  if not Assigned(glTexSubImage3D) then Exit;
  Pointer(glCopyTexSubImage3D) := wglGetProcAddress('glCopyTexSubImage3D');
  if not Assigned(glCopyTexSubImage3D) then Exit;
  Result := True;
end;

function Load_GL_ARB_imaging(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_imaging', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_VERSION_1_3(): Boolean;
begin
  Result := False;
  Pointer(glActiveTexture) := wglGetProcAddress('glActiveTexture');
  if not Assigned(glActiveTexture) then Exit;
  Pointer(glSampleCoverage) := wglGetProcAddress('glSampleCoverage');
  if not Assigned(glSampleCoverage) then Exit;
  Pointer(glCompressedTexImage3D) := wglGetProcAddress('glCompressedTexImage3D');
  if not Assigned(glCompressedTexImage3D) then Exit;
  Pointer(glCompressedTexImage2D) := wglGetProcAddress('glCompressedTexImage2D');
  if not Assigned(glCompressedTexImage2D) then Exit;
  Pointer(glCompressedTexImage1D) := wglGetProcAddress('glCompressedTexImage1D');
  if not Assigned(glCompressedTexImage1D) then Exit;
  Pointer(glCompressedTexSubImage3D) := wglGetProcAddress('glCompressedTexSubImage3D');
  if not Assigned(glCompressedTexSubImage3D) then Exit;
  Pointer(glCompressedTexSubImage2D) := wglGetProcAddress('glCompressedTexSubImage2D');
  if not Assigned(glCompressedTexSubImage2D) then Exit;
  Pointer(glCompressedTexSubImage1D) := wglGetProcAddress('glCompressedTexSubImage1D');
  if not Assigned(glCompressedTexSubImage1D) then Exit;
  Pointer(glGetCompressedTexImage) := wglGetProcAddress('glGetCompressedTexImage');
  if not Assigned(glGetCompressedTexImage) then Exit;
  Result := Load_GL_VERSION_1_2();
end;

function Load_GL_VERSION_1_4(): Boolean;
begin
  Result := False;
  Pointer(glBlendFuncSeparate) := wglGetProcAddress('glBlendFuncSeparate');
  if not Assigned(glBlendFuncSeparate) then Exit;
  Pointer(glMultiDrawArrays) := wglGetProcAddress('glMultiDrawArrays');
  if not Assigned(glMultiDrawArrays) then Exit;
  Pointer(glMultiDrawElements) := wglGetProcAddress('glMultiDrawElements');
  if not Assigned(glMultiDrawElements) then Exit;
  Pointer(glPointParameterf) := wglGetProcAddress('glPointParameterf');
  if not Assigned(glPointParameterf) then Exit;
  Pointer(glPointParameterfv) := wglGetProcAddress('glPointParameterfv');
  if not Assigned(glPointParameterfv) then Exit;
  Pointer(glPointParameteri) := wglGetProcAddress('glPointParameteri');
  if not Assigned(glPointParameteri) then Exit;
  Pointer(glPointParameteriv) := wglGetProcAddress('glPointParameteriv');
  if not Assigned(glPointParameteriv) then Exit;
  Result := Load_GL_VERSION_1_3();
end;

function Load_GL_VERSION_1_5(): Boolean;
begin
  Result := False;
  Pointer(glGenQueries) := wglGetProcAddress('glGenQueries');
  if not Assigned(glGenQueries) then Exit;
  Pointer(glDeleteQueries) := wglGetProcAddress('glDeleteQueries');
  if not Assigned(glDeleteQueries) then Exit;
  Pointer(glIsQuery) := wglGetProcAddress('glIsQuery');
  if not Assigned(glIsQuery) then Exit;
  Pointer(glBeginQuery) := wglGetProcAddress('glBeginQuery');
  if not Assigned(glBeginQuery) then Exit;
  Pointer(glEndQuery) := wglGetProcAddress('glEndQuery');
  if not Assigned(glEndQuery) then Exit;
  Pointer(glGetQueryiv) := wglGetProcAddress('glGetQueryiv');
  if not Assigned(glGetQueryiv) then Exit;
  Pointer(glGetQueryObjectiv) := wglGetProcAddress('glGetQueryObjectiv');
  if not Assigned(glGetQueryObjectiv) then Exit;
  Pointer(glGetQueryObjectuiv) := wglGetProcAddress('glGetQueryObjectuiv');
  if not Assigned(glGetQueryObjectuiv) then Exit;
  Pointer(glBindBuffer) := wglGetProcAddress('glBindBuffer');
  if not Assigned(glBindBuffer) then Exit;
  Pointer(glDeleteBuffers) := wglGetProcAddress('glDeleteBuffers');
  if not Assigned(glDeleteBuffers) then Exit;
  Pointer(glGenBuffers) := wglGetProcAddress('glGenBuffers');
  if not Assigned(glGenBuffers) then Exit;
  Pointer(glIsBuffer) := wglGetProcAddress('glIsBuffer');
  if not Assigned(glIsBuffer) then Exit;
  Pointer(glBufferData) := wglGetProcAddress('glBufferData');
  if not Assigned(glBufferData) then Exit;
  Pointer(glBufferSubData) := wglGetProcAddress('glBufferSubData');
  if not Assigned(glBufferSubData) then Exit;
  Pointer(glGetBufferSubData) := wglGetProcAddress('glGetBufferSubData');
  if not Assigned(glGetBufferSubData) then Exit;
  Pointer(glMapBuffer) := wglGetProcAddress('glMapBuffer');
  if not Assigned(glMapBuffer) then Exit;
  Pointer(glUnmapBuffer) := wglGetProcAddress('glUnmapBuffer');
  if not Assigned(glUnmapBuffer) then Exit;
  Pointer(glGetBufferParameteriv) := wglGetProcAddress('glGetBufferParameteriv');
  if not Assigned(glGetBufferParameteriv) then Exit;
  Pointer(glGetBufferPointerv) := wglGetProcAddress('glGetBufferPointerv');
  if not Assigned(glGetBufferPointerv) then Exit;
  Result := Load_GL_VERSION_1_4();
end;

function Load_GL_VERSION_2_0(): Boolean;
begin
  Result := False;
  Pointer(glBlendEquationSeparate) := wglGetProcAddress('glBlendEquationSeparate');
  if not Assigned(glBlendEquationSeparate) then Exit;
  Pointer(glDrawBuffers) := wglGetProcAddress('glDrawBuffers');
  if not Assigned(glDrawBuffers) then Exit;
  Pointer(glStencilOpSeparate) := wglGetProcAddress('glStencilOpSeparate');
  if not Assigned(glStencilOpSeparate) then Exit;
  Pointer(glStencilFuncSeparate) := wglGetProcAddress('glStencilFuncSeparate');
  if not Assigned(glStencilFuncSeparate) then Exit;
  Pointer(glStencilMaskSeparate) := wglGetProcAddress('glStencilMaskSeparate');
  if not Assigned(glStencilMaskSeparate) then Exit;
  Pointer(glAttachShader) := wglGetProcAddress('glAttachShader');
  if not Assigned(glAttachShader) then Exit;
  Pointer(glBindAttribLocation) := wglGetProcAddress('glBindAttribLocation');
  if not Assigned(glBindAttribLocation) then Exit;
  Pointer(glCompileShader) := wglGetProcAddress('glCompileShader');
  if not Assigned(glCompileShader) then Exit;
  Pointer(glCreateProgram) := wglGetProcAddress('glCreateProgram');
  if not Assigned(glCreateProgram) then Exit;
  Pointer(glCreateShader) := wglGetProcAddress('glCreateShader');
  if not Assigned(glCreateShader) then Exit;
  Pointer(glDeleteProgram) := wglGetProcAddress('glDeleteProgram');
  if not Assigned(glDeleteProgram) then Exit;
  Pointer(glDeleteShader) := wglGetProcAddress('glDeleteShader');
  if not Assigned(glDeleteShader) then Exit;
  Pointer(glDetachShader) := wglGetProcAddress('glDetachShader');
  if not Assigned(glDetachShader) then Exit;
  Pointer(glDisableVertexAttribArray) := wglGetProcAddress('glDisableVertexAttribArray');
  if not Assigned(glDisableVertexAttribArray) then Exit;
  Pointer(glEnableVertexAttribArray) := wglGetProcAddress('glEnableVertexAttribArray');
  if not Assigned(glEnableVertexAttribArray) then Exit;
  Pointer(glGetActiveAttrib) := wglGetProcAddress('glGetActiveAttrib');
  if not Assigned(glGetActiveAttrib) then Exit;
  Pointer(glGetActiveUniform) := wglGetProcAddress('glGetActiveUniform');
  if not Assigned(glGetActiveUniform) then Exit;
  Pointer(glGetAttachedShaders) := wglGetProcAddress('glGetAttachedShaders');
  if not Assigned(glGetAttachedShaders) then Exit;
  Pointer(glGetAttribLocation) := wglGetProcAddress('glGetAttribLocation');
  if not Assigned(glGetAttribLocation) then Exit;
  Pointer(glGetProgramiv) := wglGetProcAddress('glGetProgramiv');
  if not Assigned(glGetProgramiv) then Exit;
  Pointer(glGetProgramInfoLog) := wglGetProcAddress('glGetProgramInfoLog');
  if not Assigned(glGetProgramInfoLog) then Exit;
  Pointer(glGetShaderiv) := wglGetProcAddress('glGetShaderiv');
  if not Assigned(glGetShaderiv) then Exit;
  Pointer(glGetShaderInfoLog) := wglGetProcAddress('glGetShaderInfoLog');
  if not Assigned(glGetShaderInfoLog) then Exit;
  Pointer(glGetShaderSource) := wglGetProcAddress('glGetShaderSource');
  if not Assigned(glGetShaderSource) then Exit;
  Pointer(glGetUniformLocation) := wglGetProcAddress('glGetUniformLocation');
  if not Assigned(glGetUniformLocation) then Exit;
  Pointer(glGetUniformfv) := wglGetProcAddress('glGetUniformfv');
  if not Assigned(glGetUniformfv) then Exit;
  Pointer(glGetUniformiv) := wglGetProcAddress('glGetUniformiv');
  if not Assigned(glGetUniformiv) then Exit;
  Pointer(glGetVertexAttribdv) := wglGetProcAddress('glGetVertexAttribdv');
  if not Assigned(glGetVertexAttribdv) then Exit;
  Pointer(glGetVertexAttribfv) := wglGetProcAddress('glGetVertexAttribfv');
  if not Assigned(glGetVertexAttribfv) then Exit;
  Pointer(glGetVertexAttribiv) := wglGetProcAddress('glGetVertexAttribiv');
  if not Assigned(glGetVertexAttribiv) then Exit;
  Pointer(glGetVertexAttribPointerv) := wglGetProcAddress('glGetVertexAttribPointerv');
  if not Assigned(glGetVertexAttribPointerv) then Exit;
  Pointer(glIsProgram) := wglGetProcAddress('glIsProgram');
  if not Assigned(glIsProgram) then Exit;
  Pointer(glIsShader) := wglGetProcAddress('glIsShader');
  if not Assigned(glIsShader) then Exit;
  Pointer(glLinkProgram) := wglGetProcAddress('glLinkProgram');
  if not Assigned(glLinkProgram) then Exit;
  Pointer(glShaderSource) := wglGetProcAddress('glShaderSource');
  if not Assigned(glShaderSource) then Exit;
  Pointer(glUseProgram) := wglGetProcAddress('glUseProgram');
  if not Assigned(glUseProgram) then Exit;
  Pointer(glUniform1f) := wglGetProcAddress('glUniform1f');
  if not Assigned(glUniform1f) then Exit;
  Pointer(glUniform2f) := wglGetProcAddress('glUniform2f');
  if not Assigned(glUniform2f) then Exit;
  Pointer(glUniform3f) := wglGetProcAddress('glUniform3f');
  if not Assigned(glUniform3f) then Exit;
  Pointer(glUniform4f) := wglGetProcAddress('glUniform4f');
  if not Assigned(glUniform4f) then Exit;
  Pointer(glUniform1i) := wglGetProcAddress('glUniform1i');
  if not Assigned(glUniform1i) then Exit;
  Pointer(glUniform2i) := wglGetProcAddress('glUniform2i');
  if not Assigned(glUniform2i) then Exit;
  Pointer(glUniform3i) := wglGetProcAddress('glUniform3i');
  if not Assigned(glUniform3i) then Exit;
  Pointer(glUniform4i) := wglGetProcAddress('glUniform4i');
  if not Assigned(glUniform4i) then Exit;
  Pointer(glUniform1fv) := wglGetProcAddress('glUniform1fv');
  if not Assigned(glUniform1fv) then Exit;
  Pointer(glUniform2fv) := wglGetProcAddress('glUniform2fv');
  if not Assigned(glUniform2fv) then Exit;
  Pointer(glUniform3fv) := wglGetProcAddress('glUniform3fv');
  if not Assigned(glUniform3fv) then Exit;
  Pointer(glUniform4fv) := wglGetProcAddress('glUniform4fv');
  if not Assigned(glUniform4fv) then Exit;
  Pointer(glUniform1iv) := wglGetProcAddress('glUniform1iv');
  if not Assigned(glUniform1iv) then Exit;
  Pointer(glUniform2iv) := wglGetProcAddress('glUniform2iv');
  if not Assigned(glUniform2iv) then Exit;
  Pointer(glUniform3iv) := wglGetProcAddress('glUniform3iv');
  if not Assigned(glUniform3iv) then Exit;
  Pointer(glUniform4iv) := wglGetProcAddress('glUniform4iv');
  if not Assigned(glUniform4iv) then Exit;
  Pointer(glUniformMatrix2fv) := wglGetProcAddress('glUniformMatrix2fv');
  if not Assigned(glUniformMatrix2fv) then Exit;
  Pointer(glUniformMatrix3fv) := wglGetProcAddress('glUniformMatrix3fv');
  if not Assigned(glUniformMatrix3fv) then Exit;
  Pointer(glUniformMatrix4fv) := wglGetProcAddress('glUniformMatrix4fv');
  if not Assigned(glUniformMatrix4fv) then Exit;
  Pointer(glValidateProgram) := wglGetProcAddress('glValidateProgram');
  if not Assigned(glValidateProgram) then Exit;
  Pointer(glVertexAttrib1d) := wglGetProcAddress('glVertexAttrib1d');
  if not Assigned(glVertexAttrib1d) then Exit;
  Pointer(glVertexAttrib1dv) := wglGetProcAddress('glVertexAttrib1dv');
  if not Assigned(glVertexAttrib1dv) then Exit;
  Pointer(glVertexAttrib1f) := wglGetProcAddress('glVertexAttrib1f');
  if not Assigned(glVertexAttrib1f) then Exit;
  Pointer(glVertexAttrib1fv) := wglGetProcAddress('glVertexAttrib1fv');
  if not Assigned(glVertexAttrib1fv) then Exit;
  Pointer(glVertexAttrib1s) := wglGetProcAddress('glVertexAttrib1s');
  if not Assigned(glVertexAttrib1s) then Exit;
  Pointer(glVertexAttrib1sv) := wglGetProcAddress('glVertexAttrib1sv');
  if not Assigned(glVertexAttrib1sv) then Exit;
  Pointer(glVertexAttrib2d) := wglGetProcAddress('glVertexAttrib2d');
  if not Assigned(glVertexAttrib2d) then Exit;
  Pointer(glVertexAttrib2dv) := wglGetProcAddress('glVertexAttrib2dv');
  if not Assigned(glVertexAttrib2dv) then Exit;
  Pointer(glVertexAttrib2f) := wglGetProcAddress('glVertexAttrib2f');
  if not Assigned(glVertexAttrib2f) then Exit;
  Pointer(glVertexAttrib2fv) := wglGetProcAddress('glVertexAttrib2fv');
  if not Assigned(glVertexAttrib2fv) then Exit;
  Pointer(glVertexAttrib2s) := wglGetProcAddress('glVertexAttrib2s');
  if not Assigned(glVertexAttrib2s) then Exit;
  Pointer(glVertexAttrib2sv) := wglGetProcAddress('glVertexAttrib2sv');
  if not Assigned(glVertexAttrib2sv) then Exit;
  Pointer(glVertexAttrib3d) := wglGetProcAddress('glVertexAttrib3d');
  if not Assigned(glVertexAttrib3d) then Exit;
  Pointer(glVertexAttrib3dv) := wglGetProcAddress('glVertexAttrib3dv');
  if not Assigned(glVertexAttrib3dv) then Exit;
  Pointer(glVertexAttrib3f) := wglGetProcAddress('glVertexAttrib3f');
  if not Assigned(glVertexAttrib3f) then Exit;
  Pointer(glVertexAttrib3fv) := wglGetProcAddress('glVertexAttrib3fv');
  if not Assigned(glVertexAttrib3fv) then Exit;
  Pointer(glVertexAttrib3s) := wglGetProcAddress('glVertexAttrib3s');
  if not Assigned(glVertexAttrib3s) then Exit;
  Pointer(glVertexAttrib3sv) := wglGetProcAddress('glVertexAttrib3sv');
  if not Assigned(glVertexAttrib3sv) then Exit;
  Pointer(glVertexAttrib4Nbv) := wglGetProcAddress('glVertexAttrib4Nbv');
  if not Assigned(glVertexAttrib4Nbv) then Exit;
  Pointer(glVertexAttrib4Niv) := wglGetProcAddress('glVertexAttrib4Niv');
  if not Assigned(glVertexAttrib4Niv) then Exit;
  Pointer(glVertexAttrib4Nsv) := wglGetProcAddress('glVertexAttrib4Nsv');
  if not Assigned(glVertexAttrib4Nsv) then Exit;
  Pointer(glVertexAttrib4Nub) := wglGetProcAddress('glVertexAttrib4Nub');
  if not Assigned(glVertexAttrib4Nub) then Exit;
  Pointer(glVertexAttrib4Nubv) := wglGetProcAddress('glVertexAttrib4Nubv');
  if not Assigned(glVertexAttrib4Nubv) then Exit;
  Pointer(glVertexAttrib4Nuiv) := wglGetProcAddress('glVertexAttrib4Nuiv');
  if not Assigned(glVertexAttrib4Nuiv) then Exit;
  Pointer(glVertexAttrib4Nusv) := wglGetProcAddress('glVertexAttrib4Nusv');
  if not Assigned(glVertexAttrib4Nusv) then Exit;
  Pointer(glVertexAttrib4bv) := wglGetProcAddress('glVertexAttrib4bv');
  if not Assigned(glVertexAttrib4bv) then Exit;
  Pointer(glVertexAttrib4d) := wglGetProcAddress('glVertexAttrib4d');
  if not Assigned(glVertexAttrib4d) then Exit;
  Pointer(glVertexAttrib4dv) := wglGetProcAddress('glVertexAttrib4dv');
  if not Assigned(glVertexAttrib4dv) then Exit;
  Pointer(glVertexAttrib4f) := wglGetProcAddress('glVertexAttrib4f');
  if not Assigned(glVertexAttrib4f) then Exit;
  Pointer(glVertexAttrib4fv) := wglGetProcAddress('glVertexAttrib4fv');
  if not Assigned(glVertexAttrib4fv) then Exit;
  Pointer(glVertexAttrib4iv) := wglGetProcAddress('glVertexAttrib4iv');
  if not Assigned(glVertexAttrib4iv) then Exit;
  Pointer(glVertexAttrib4s) := wglGetProcAddress('glVertexAttrib4s');
  if not Assigned(glVertexAttrib4s) then Exit;
  Pointer(glVertexAttrib4sv) := wglGetProcAddress('glVertexAttrib4sv');
  if not Assigned(glVertexAttrib4sv) then Exit;
  Pointer(glVertexAttrib4ubv) := wglGetProcAddress('glVertexAttrib4ubv');
  if not Assigned(glVertexAttrib4ubv) then Exit;
  Pointer(glVertexAttrib4uiv) := wglGetProcAddress('glVertexAttrib4uiv');
  if not Assigned(glVertexAttrib4uiv) then Exit;
  Pointer(glVertexAttrib4usv) := wglGetProcAddress('glVertexAttrib4usv');
  if not Assigned(glVertexAttrib4usv) then Exit;
  Pointer(glVertexAttribPointer) := wglGetProcAddress('glVertexAttribPointer');
  if not Assigned(glVertexAttribPointer) then Exit;
  Result := Load_GL_VERSION_1_5();
end;

function Load_GL_VERSION_2_1(): Boolean;
begin
  Result := False;
  Pointer(glUniformMatrix2x3fv) := wglGetProcAddress('glUniformMatrix2x3fv');
  if not Assigned(glUniformMatrix2x3fv) then Exit;
  Pointer(glUniformMatrix3x2fv) := wglGetProcAddress('glUniformMatrix3x2fv');
  if not Assigned(glUniformMatrix3x2fv) then Exit;
  Pointer(glUniformMatrix2x4fv) := wglGetProcAddress('glUniformMatrix2x4fv');
  if not Assigned(glUniformMatrix2x4fv) then Exit;
  Pointer(glUniformMatrix4x2fv) := wglGetProcAddress('glUniformMatrix4x2fv');
  if not Assigned(glUniformMatrix4x2fv) then Exit;
  Pointer(glUniformMatrix3x4fv) := wglGetProcAddress('glUniformMatrix3x4fv');
  if not Assigned(glUniformMatrix3x4fv) then Exit;
  Pointer(glUniformMatrix4x3fv) := wglGetProcAddress('glUniformMatrix4x3fv');
  if not Assigned(glUniformMatrix4x3fv) then Exit;
  Result := Load_GL_VERSION_2_0();
end;

function Load_GL_VERSION_3_0(): Boolean;
begin
  Result := False;
  Pointer(glColorMaski) := wglGetProcAddress('glColorMaski');
  if not Assigned(glColorMaski) then Exit;
  Pointer(glGetBooleani_v) := wglGetProcAddress('glGetBooleani_v');
  if not Assigned(glGetBooleani_v) then Exit;
  Pointer(glGetIntegeri_v) := wglGetProcAddress('glGetIntegeri_v');
  if not Assigned(glGetIntegeri_v) then Exit;
  Pointer(glEnablei) := wglGetProcAddress('glEnablei');
  if not Assigned(glEnablei) then Exit;
  Pointer(glDisablei) := wglGetProcAddress('glDisablei');
  if not Assigned(glDisablei) then Exit;
  Pointer(glIsEnabledi) := wglGetProcAddress('glIsEnabledi');
  if not Assigned(glIsEnabledi) then Exit;
  Pointer(glBeginTransformFeedback) := wglGetProcAddress('glBeginTransformFeedback');
  if not Assigned(glBeginTransformFeedback) then Exit;
  Pointer(glEndTransformFeedback) := wglGetProcAddress('glEndTransformFeedback');
  if not Assigned(glEndTransformFeedback) then Exit;
  Pointer(glBindBufferRange) := wglGetProcAddress('glBindBufferRange');
  if not Assigned(glBindBufferRange) then Exit;
  Pointer(glBindBufferBase) := wglGetProcAddress('glBindBufferBase');
  if not Assigned(glBindBufferBase) then Exit;
  Pointer(glTransformFeedbackVaryings) := wglGetProcAddress('glTransformFeedbackVaryings');
  if not Assigned(glTransformFeedbackVaryings) then Exit;
  Pointer(glGetTransformFeedbackVarying) := wglGetProcAddress('glGetTransformFeedbackVarying');
  if not Assigned(glGetTransformFeedbackVarying) then Exit;
  Pointer(glClampColor) := wglGetProcAddress('glClampColor');
  if not Assigned(glClampColor) then Exit;
  Pointer(glBeginConditionalRender) := wglGetProcAddress('glBeginConditionalRender');
  if not Assigned(glBeginConditionalRender) then Exit;
  Pointer(glEndConditionalRender) := wglGetProcAddress('glEndConditionalRender');
  if not Assigned(glEndConditionalRender) then Exit;
  Pointer(glVertexAttribIPointer) := wglGetProcAddress('glVertexAttribIPointer');
  if not Assigned(glVertexAttribIPointer) then Exit;
  Pointer(glGetVertexAttribIiv) := wglGetProcAddress('glGetVertexAttribIiv');
  if not Assigned(glGetVertexAttribIiv) then Exit;
  Pointer(glGetVertexAttribIuiv) := wglGetProcAddress('glGetVertexAttribIuiv');
  if not Assigned(glGetVertexAttribIuiv) then Exit;
  Pointer(glVertexAttribI1i) := wglGetProcAddress('glVertexAttribI1i');
  if not Assigned(glVertexAttribI1i) then Exit;
  Pointer(glVertexAttribI2i) := wglGetProcAddress('glVertexAttribI2i');
  if not Assigned(glVertexAttribI2i) then Exit;
  Pointer(glVertexAttribI3i) := wglGetProcAddress('glVertexAttribI3i');
  if not Assigned(glVertexAttribI3i) then Exit;
  Pointer(glVertexAttribI4i) := wglGetProcAddress('glVertexAttribI4i');
  if not Assigned(glVertexAttribI4i) then Exit;
  Pointer(glVertexAttribI1ui) := wglGetProcAddress('glVertexAttribI1ui');
  if not Assigned(glVertexAttribI1ui) then Exit;
  Pointer(glVertexAttribI2ui) := wglGetProcAddress('glVertexAttribI2ui');
  if not Assigned(glVertexAttribI2ui) then Exit;
  Pointer(glVertexAttribI3ui) := wglGetProcAddress('glVertexAttribI3ui');
  if not Assigned(glVertexAttribI3ui) then Exit;
  Pointer(glVertexAttribI4ui) := wglGetProcAddress('glVertexAttribI4ui');
  if not Assigned(glVertexAttribI4ui) then Exit;
  Pointer(glVertexAttribI1iv) := wglGetProcAddress('glVertexAttribI1iv');
  if not Assigned(glVertexAttribI1iv) then Exit;
  Pointer(glVertexAttribI2iv) := wglGetProcAddress('glVertexAttribI2iv');
  if not Assigned(glVertexAttribI2iv) then Exit;
  Pointer(glVertexAttribI3iv) := wglGetProcAddress('glVertexAttribI3iv');
  if not Assigned(glVertexAttribI3iv) then Exit;
  Pointer(glVertexAttribI4iv) := wglGetProcAddress('glVertexAttribI4iv');
  if not Assigned(glVertexAttribI4iv) then Exit;
  Pointer(glVertexAttribI1uiv) := wglGetProcAddress('glVertexAttribI1uiv');
  if not Assigned(glVertexAttribI1uiv) then Exit;
  Pointer(glVertexAttribI2uiv) := wglGetProcAddress('glVertexAttribI2uiv');
  if not Assigned(glVertexAttribI2uiv) then Exit;
  Pointer(glVertexAttribI3uiv) := wglGetProcAddress('glVertexAttribI3uiv');
  if not Assigned(glVertexAttribI3uiv) then Exit;
  Pointer(glVertexAttribI4uiv) := wglGetProcAddress('glVertexAttribI4uiv');
  if not Assigned(glVertexAttribI4uiv) then Exit;
  Pointer(glVertexAttribI4bv) := wglGetProcAddress('glVertexAttribI4bv');
  if not Assigned(glVertexAttribI4bv) then Exit;
  Pointer(glVertexAttribI4sv) := wglGetProcAddress('glVertexAttribI4sv');
  if not Assigned(glVertexAttribI4sv) then Exit;
  Pointer(glVertexAttribI4ubv) := wglGetProcAddress('glVertexAttribI4ubv');
  if not Assigned(glVertexAttribI4ubv) then Exit;
  Pointer(glVertexAttribI4usv) := wglGetProcAddress('glVertexAttribI4usv');
  if not Assigned(glVertexAttribI4usv) then Exit;
  Pointer(glGetUniformuiv) := wglGetProcAddress('glGetUniformuiv');
  if not Assigned(glGetUniformuiv) then Exit;
  Pointer(glBindFragDataLocation) := wglGetProcAddress('glBindFragDataLocation');
  if not Assigned(glBindFragDataLocation) then Exit;
  Pointer(glGetFragDataLocation) := wglGetProcAddress('glGetFragDataLocation');
  if not Assigned(glGetFragDataLocation) then Exit;
  Pointer(glUniform1ui) := wglGetProcAddress('glUniform1ui');
  if not Assigned(glUniform1ui) then Exit;
  Pointer(glUniform2ui) := wglGetProcAddress('glUniform2ui');
  if not Assigned(glUniform2ui) then Exit;
  Pointer(glUniform3ui) := wglGetProcAddress('glUniform3ui');
  if not Assigned(glUniform3ui) then Exit;
  Pointer(glUniform4ui) := wglGetProcAddress('glUniform4ui');
  if not Assigned(glUniform4ui) then Exit;
  Pointer(glUniform1uiv) := wglGetProcAddress('glUniform1uiv');
  if not Assigned(glUniform1uiv) then Exit;
  Pointer(glUniform2uiv) := wglGetProcAddress('glUniform2uiv');
  if not Assigned(glUniform2uiv) then Exit;
  Pointer(glUniform3uiv) := wglGetProcAddress('glUniform3uiv');
  if not Assigned(glUniform3uiv) then Exit;
  Pointer(glUniform4uiv) := wglGetProcAddress('glUniform4uiv');
  if not Assigned(glUniform4uiv) then Exit;
  Pointer(glTexParameterIiv) := wglGetProcAddress('glTexParameterIiv');
  if not Assigned(glTexParameterIiv) then Exit;
  Pointer(glTexParameterIuiv) := wglGetProcAddress('glTexParameterIuiv');
  if not Assigned(glTexParameterIuiv) then Exit;
  Pointer(glGetTexParameterIiv) := wglGetProcAddress('glGetTexParameterIiv');
  if not Assigned(glGetTexParameterIiv) then Exit;
  Pointer(glGetTexParameterIuiv) := wglGetProcAddress('glGetTexParameterIuiv');
  if not Assigned(glGetTexParameterIuiv) then Exit;
  Pointer(glClearBufferiv) := wglGetProcAddress('glClearBufferiv');
  if not Assigned(glClearBufferiv) then Exit;
  Pointer(glClearBufferuiv) := wglGetProcAddress('glClearBufferuiv');
  if not Assigned(glClearBufferuiv) then Exit;
  Pointer(glClearBufferfv) := wglGetProcAddress('glClearBufferfv');
  if not Assigned(glClearBufferfv) then Exit;
  Pointer(glClearBufferfi) := wglGetProcAddress('glClearBufferfi');
  if not Assigned(glClearBufferfi) then Exit;
  Pointer(glGetStringi) := wglGetProcAddress('glGetStringi');
  if not Assigned(glGetStringi) then Exit;
  if not Load_GL_ARB_framebuffer_object() then Exit;
  if not Load_GL_ARB_map_buffer_range() then Exit;
  if not Load_GL_ARB_vertex_array_object() then Exit;
  Result := Load_GL_VERSION_2_1();
end;

function Load_GL_VERSION_3_1(): Boolean;
begin
  Result := False;
  Pointer(glDrawArraysInstanced) := wglGetProcAddress('glDrawArraysInstanced');
  if not Assigned(glDrawArraysInstanced) then Exit;
  Pointer(glDrawElementsInstanced) := wglGetProcAddress('glDrawElementsInstanced');
  if not Assigned(glDrawElementsInstanced) then Exit;
  Pointer(glTexBuffer) := wglGetProcAddress('glTexBuffer');
  if not Assigned(glTexBuffer) then Exit;
  Pointer(glPrimitiveRestartIndex) := wglGetProcAddress('glPrimitiveRestartIndex');
  if not Assigned(glPrimitiveRestartIndex) then Exit;
  if not Load_GL_ARB_copy_buffer() then Exit;
  if not Load_GL_ARB_uniform_buffer_object() then Exit;
  Result := Load_GL_VERSION_3_0();
end;

function Load_GL_VERSION_3_2(): Boolean;
begin
  Result := False;
  Pointer(glGetInteger64i_v) := wglGetProcAddress('glGetInteger64i_v');
  if not Assigned(glGetInteger64i_v) then Exit;
  Pointer(glGetBufferParameteri64v) := wglGetProcAddress('glGetBufferParameteri64v');
  if not Assigned(glGetBufferParameteri64v) then Exit;
  Pointer(glProgramParameteri) := wglGetProcAddress('glProgramParameteri');
  if not Assigned(glProgramParameteri) then Exit;
  Pointer(glFramebufferTexture) := wglGetProcAddress('glFramebufferTexture');
  if not Assigned(glFramebufferTexture) then Exit;
  if not Load_GL_ARB_draw_elements_base_vertex() then Exit;
  if not Load_GL_ARB_provoking_vertex() then Exit;
  if not Load_GL_ARB_sync() then Exit;
  if not Load_GL_ARB_texture_multisample() then Exit;
  Result := Load_GL_VERSION_3_1();
end;

function Load_GL_VERSION_3_3(): Boolean;
begin
  Result := False;
  if not Load_GL_ARB_blend_func_extended() then Exit;
  if not Load_GL_ARB_sampler_objects() then Exit;
  if not Load_GL_ARB_timer_query() then Exit;
  if not Load_GL_ARB_vertex_type_2_10_10_10_rev() then Exit;
  Result := Load_GL_VERSION_3_2();
end;

function Load_GL_VERSION_4_0(): Boolean;
begin
  Result := False;
  if not Load_GL_ARB_gpu_shader_fp64() then Exit;
  if not Load_GL_ARB_shader_subroutine() then Exit;
  if not Load_GL_ARB_tessellation_shader() then Exit;
  if not Load_GL_ARB_transform_feedback2() then Exit;
  if not Load_GL_ARB_transform_feedback3() then Exit;
  Result := Load_GL_VERSION_3_3();
end;

function Load_GL_ARB_multitexture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_multitexture', extstring) then
  begin
    Pointer(glActiveTextureARB) := wglGetProcAddress('glActiveTextureARB');
    if not Assigned(glActiveTextureARB) then Exit;
    Pointer(glClientActiveTextureARB) := wglGetProcAddress('glClientActiveTextureARB');
    if not Assigned(glClientActiveTextureARB) then Exit;
    Pointer(glMultiTexCoord1dARB) := wglGetProcAddress('glMultiTexCoord1dARB');
    if not Assigned(glMultiTexCoord1dARB) then Exit;
    Pointer(glMultiTexCoord1dvARB) := wglGetProcAddress('glMultiTexCoord1dvARB');
    if not Assigned(glMultiTexCoord1dvARB) then Exit;
    Pointer(glMultiTexCoord1fARB) := wglGetProcAddress('glMultiTexCoord1fARB');
    if not Assigned(glMultiTexCoord1fARB) then Exit;
    Pointer(glMultiTexCoord1fvARB) := wglGetProcAddress('glMultiTexCoord1fvARB');
    if not Assigned(glMultiTexCoord1fvARB) then Exit;
    Pointer(glMultiTexCoord1iARB) := wglGetProcAddress('glMultiTexCoord1iARB');
    if not Assigned(glMultiTexCoord1iARB) then Exit;
    Pointer(glMultiTexCoord1ivARB) := wglGetProcAddress('glMultiTexCoord1ivARB');
    if not Assigned(glMultiTexCoord1ivARB) then Exit;
    Pointer(glMultiTexCoord1sARB) := wglGetProcAddress('glMultiTexCoord1sARB');
    if not Assigned(glMultiTexCoord1sARB) then Exit;
    Pointer(glMultiTexCoord1svARB) := wglGetProcAddress('glMultiTexCoord1svARB');
    if not Assigned(glMultiTexCoord1svARB) then Exit;
    Pointer(glMultiTexCoord2dARB) := wglGetProcAddress('glMultiTexCoord2dARB');
    if not Assigned(glMultiTexCoord2dARB) then Exit;
    Pointer(glMultiTexCoord2dvARB) := wglGetProcAddress('glMultiTexCoord2dvARB');
    if not Assigned(glMultiTexCoord2dvARB) then Exit;
    Pointer(glMultiTexCoord2fARB) := wglGetProcAddress('glMultiTexCoord2fARB');
    if not Assigned(glMultiTexCoord2fARB) then Exit;
    Pointer(glMultiTexCoord2fvARB) := wglGetProcAddress('glMultiTexCoord2fvARB');
    if not Assigned(glMultiTexCoord2fvARB) then Exit;
    Pointer(glMultiTexCoord2iARB) := wglGetProcAddress('glMultiTexCoord2iARB');
    if not Assigned(glMultiTexCoord2iARB) then Exit;
    Pointer(glMultiTexCoord2ivARB) := wglGetProcAddress('glMultiTexCoord2ivARB');
    if not Assigned(glMultiTexCoord2ivARB) then Exit;
    Pointer(glMultiTexCoord2sARB) := wglGetProcAddress('glMultiTexCoord2sARB');
    if not Assigned(glMultiTexCoord2sARB) then Exit;
    Pointer(glMultiTexCoord2svARB) := wglGetProcAddress('glMultiTexCoord2svARB');
    if not Assigned(glMultiTexCoord2svARB) then Exit;
    Pointer(glMultiTexCoord3dARB) := wglGetProcAddress('glMultiTexCoord3dARB');
    if not Assigned(glMultiTexCoord3dARB) then Exit;
    Pointer(glMultiTexCoord3dvARB) := wglGetProcAddress('glMultiTexCoord3dvARB');
    if not Assigned(glMultiTexCoord3dvARB) then Exit;
    Pointer(glMultiTexCoord3fARB) := wglGetProcAddress('glMultiTexCoord3fARB');
    if not Assigned(glMultiTexCoord3fARB) then Exit;
    Pointer(glMultiTexCoord3fvARB) := wglGetProcAddress('glMultiTexCoord3fvARB');
    if not Assigned(glMultiTexCoord3fvARB) then Exit;
    Pointer(glMultiTexCoord3iARB) := wglGetProcAddress('glMultiTexCoord3iARB');
    if not Assigned(glMultiTexCoord3iARB) then Exit;
    Pointer(glMultiTexCoord3ivARB) := wglGetProcAddress('glMultiTexCoord3ivARB');
    if not Assigned(glMultiTexCoord3ivARB) then Exit;
    Pointer(glMultiTexCoord3sARB) := wglGetProcAddress('glMultiTexCoord3sARB');
    if not Assigned(glMultiTexCoord3sARB) then Exit;
    Pointer(glMultiTexCoord3svARB) := wglGetProcAddress('glMultiTexCoord3svARB');
    if not Assigned(glMultiTexCoord3svARB) then Exit;
    Pointer(glMultiTexCoord4dARB) := wglGetProcAddress('glMultiTexCoord4dARB');
    if not Assigned(glMultiTexCoord4dARB) then Exit;
    Pointer(glMultiTexCoord4dvARB) := wglGetProcAddress('glMultiTexCoord4dvARB');
    if not Assigned(glMultiTexCoord4dvARB) then Exit;
    Pointer(glMultiTexCoord4fARB) := wglGetProcAddress('glMultiTexCoord4fARB');
    if not Assigned(glMultiTexCoord4fARB) then Exit;
    Pointer(glMultiTexCoord4fvARB) := wglGetProcAddress('glMultiTexCoord4fvARB');
    if not Assigned(glMultiTexCoord4fvARB) then Exit;
    Pointer(glMultiTexCoord4iARB) := wglGetProcAddress('glMultiTexCoord4iARB');
    if not Assigned(glMultiTexCoord4iARB) then Exit;
    Pointer(glMultiTexCoord4ivARB) := wglGetProcAddress('glMultiTexCoord4ivARB');
    if not Assigned(glMultiTexCoord4ivARB) then Exit;
    Pointer(glMultiTexCoord4sARB) := wglGetProcAddress('glMultiTexCoord4sARB');
    if not Assigned(glMultiTexCoord4sARB) then Exit;
    Pointer(glMultiTexCoord4svARB) := wglGetProcAddress('glMultiTexCoord4svARB');
    if not Assigned(glMultiTexCoord4svARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_transpose_matrix(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_transpose_matrix', extstring) then
  begin
    Pointer(glLoadTransposeMatrixfARB) := wglGetProcAddress('glLoadTransposeMatrixfARB');
    if not Assigned(glLoadTransposeMatrixfARB) then Exit;
    Pointer(glLoadTransposeMatrixdARB) := wglGetProcAddress('glLoadTransposeMatrixdARB');
    if not Assigned(glLoadTransposeMatrixdARB) then Exit;
    Pointer(glMultTransposeMatrixfARB) := wglGetProcAddress('glMultTransposeMatrixfARB');
    if not Assigned(glMultTransposeMatrixfARB) then Exit;
    Pointer(glMultTransposeMatrixdARB) := wglGetProcAddress('glMultTransposeMatrixdARB');
    if not Assigned(glMultTransposeMatrixdARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_multisample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_multisample', extstring) then
  begin
    Pointer(glSampleCoverageARB) := wglGetProcAddress('glSampleCoverageARB');
    if not Assigned(glSampleCoverageARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_env_add(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_env_add', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_cube_map(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_cube_map', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_compression(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_compression', extstring) then
  begin
    Pointer(glCompressedTexImage3DARB) := wglGetProcAddress('glCompressedTexImage3DARB');
    if not Assigned(glCompressedTexImage3DARB) then Exit;
    Pointer(glCompressedTexImage2DARB) := wglGetProcAddress('glCompressedTexImage2DARB');
    if not Assigned(glCompressedTexImage2DARB) then Exit;
    Pointer(glCompressedTexImage1DARB) := wglGetProcAddress('glCompressedTexImage1DARB');
    if not Assigned(glCompressedTexImage1DARB) then Exit;
    Pointer(glCompressedTexSubImage3DARB) := wglGetProcAddress('glCompressedTexSubImage3DARB');
    if not Assigned(glCompressedTexSubImage3DARB) then Exit;
    Pointer(glCompressedTexSubImage2DARB) := wglGetProcAddress('glCompressedTexSubImage2DARB');
    if not Assigned(glCompressedTexSubImage2DARB) then Exit;
    Pointer(glCompressedTexSubImage1DARB) := wglGetProcAddress('glCompressedTexSubImage1DARB');
    if not Assigned(glCompressedTexSubImage1DARB) then Exit;
    Pointer(glGetCompressedTexImageARB) := wglGetProcAddress('glGetCompressedTexImageARB');
    if not Assigned(glGetCompressedTexImageARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_border_clamp(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_border_clamp', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_point_parameters(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_point_parameters', extstring) then
  begin
    Pointer(glPointParameterfARB) := wglGetProcAddress('glPointParameterfARB');
    if not Assigned(glPointParameterfARB) then Exit;
    Pointer(glPointParameterfvARB) := wglGetProcAddress('glPointParameterfvARB');
    if not Assigned(glPointParameterfvARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_blend(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_blend', extstring) then
  begin
    Pointer(glWeightbvARB) := wglGetProcAddress('glWeightbvARB');
    if not Assigned(glWeightbvARB) then Exit;
    Pointer(glWeightsvARB) := wglGetProcAddress('glWeightsvARB');
    if not Assigned(glWeightsvARB) then Exit;
    Pointer(glWeightivARB) := wglGetProcAddress('glWeightivARB');
    if not Assigned(glWeightivARB) then Exit;
    Pointer(glWeightfvARB) := wglGetProcAddress('glWeightfvARB');
    if not Assigned(glWeightfvARB) then Exit;
    Pointer(glWeightdvARB) := wglGetProcAddress('glWeightdvARB');
    if not Assigned(glWeightdvARB) then Exit;
    Pointer(glWeightubvARB) := wglGetProcAddress('glWeightubvARB');
    if not Assigned(glWeightubvARB) then Exit;
    Pointer(glWeightusvARB) := wglGetProcAddress('glWeightusvARB');
    if not Assigned(glWeightusvARB) then Exit;
    Pointer(glWeightuivARB) := wglGetProcAddress('glWeightuivARB');
    if not Assigned(glWeightuivARB) then Exit;
    Pointer(glWeightPointerARB) := wglGetProcAddress('glWeightPointerARB');
    if not Assigned(glWeightPointerARB) then Exit;
    Pointer(glVertexBlendARB) := wglGetProcAddress('glVertexBlendARB');
    if not Assigned(glVertexBlendARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_matrix_palette(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_matrix_palette', extstring) then
  begin
    Pointer(glCurrentPaletteMatrixARB) := wglGetProcAddress('glCurrentPaletteMatrixARB');
    if not Assigned(glCurrentPaletteMatrixARB) then Exit;
    Pointer(glMatrixIndexubvARB) := wglGetProcAddress('glMatrixIndexubvARB');
    if not Assigned(glMatrixIndexubvARB) then Exit;
    Pointer(glMatrixIndexusvARB) := wglGetProcAddress('glMatrixIndexusvARB');
    if not Assigned(glMatrixIndexusvARB) then Exit;
    Pointer(glMatrixIndexuivARB) := wglGetProcAddress('glMatrixIndexuivARB');
    if not Assigned(glMatrixIndexuivARB) then Exit;
    Pointer(glMatrixIndexPointerARB) := wglGetProcAddress('glMatrixIndexPointerARB');
    if not Assigned(glMatrixIndexPointerARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_env_combine(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_env_combine', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_env_crossbar(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_env_crossbar', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_env_dot3(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_env_dot3', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_mirrored_repeat(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_mirrored_repeat', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_depth_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_depth_texture', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_shadow(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shadow', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_shadow_ambient(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shadow_ambient', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_window_pos(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_window_pos', extstring) then
  begin
    Pointer(glWindowPos2dARB) := wglGetProcAddress('glWindowPos2dARB');
    if not Assigned(glWindowPos2dARB) then Exit;
    Pointer(glWindowPos2dvARB) := wglGetProcAddress('glWindowPos2dvARB');
    if not Assigned(glWindowPos2dvARB) then Exit;
    Pointer(glWindowPos2fARB) := wglGetProcAddress('glWindowPos2fARB');
    if not Assigned(glWindowPos2fARB) then Exit;
    Pointer(glWindowPos2fvARB) := wglGetProcAddress('glWindowPos2fvARB');
    if not Assigned(glWindowPos2fvARB) then Exit;
    Pointer(glWindowPos2iARB) := wglGetProcAddress('glWindowPos2iARB');
    if not Assigned(glWindowPos2iARB) then Exit;
    Pointer(glWindowPos2ivARB) := wglGetProcAddress('glWindowPos2ivARB');
    if not Assigned(glWindowPos2ivARB) then Exit;
    Pointer(glWindowPos2sARB) := wglGetProcAddress('glWindowPos2sARB');
    if not Assigned(glWindowPos2sARB) then Exit;
    Pointer(glWindowPos2svARB) := wglGetProcAddress('glWindowPos2svARB');
    if not Assigned(glWindowPos2svARB) then Exit;
    Pointer(glWindowPos3dARB) := wglGetProcAddress('glWindowPos3dARB');
    if not Assigned(glWindowPos3dARB) then Exit;
    Pointer(glWindowPos3dvARB) := wglGetProcAddress('glWindowPos3dvARB');
    if not Assigned(glWindowPos3dvARB) then Exit;
    Pointer(glWindowPos3fARB) := wglGetProcAddress('glWindowPos3fARB');
    if not Assigned(glWindowPos3fARB) then Exit;
    Pointer(glWindowPos3fvARB) := wglGetProcAddress('glWindowPos3fvARB');
    if not Assigned(glWindowPos3fvARB) then Exit;
    Pointer(glWindowPos3iARB) := wglGetProcAddress('glWindowPos3iARB');
    if not Assigned(glWindowPos3iARB) then Exit;
    Pointer(glWindowPos3ivARB) := wglGetProcAddress('glWindowPos3ivARB');
    if not Assigned(glWindowPos3ivARB) then Exit;
    Pointer(glWindowPos3sARB) := wglGetProcAddress('glWindowPos3sARB');
    if not Assigned(glWindowPos3sARB) then Exit;
    Pointer(glWindowPos3svARB) := wglGetProcAddress('glWindowPos3svARB');
    if not Assigned(glWindowPos3svARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_program(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_program', extstring) then
  begin
    Pointer(glVertexAttrib1dARB) := wglGetProcAddress('glVertexAttrib1dARB');
    if not Assigned(glVertexAttrib1dARB) then Exit;
    Pointer(glVertexAttrib1dvARB) := wglGetProcAddress('glVertexAttrib1dvARB');
    if not Assigned(glVertexAttrib1dvARB) then Exit;
    Pointer(glVertexAttrib1fARB) := wglGetProcAddress('glVertexAttrib1fARB');
    if not Assigned(glVertexAttrib1fARB) then Exit;
    Pointer(glVertexAttrib1fvARB) := wglGetProcAddress('glVertexAttrib1fvARB');
    if not Assigned(glVertexAttrib1fvARB) then Exit;
    Pointer(glVertexAttrib1sARB) := wglGetProcAddress('glVertexAttrib1sARB');
    if not Assigned(glVertexAttrib1sARB) then Exit;
    Pointer(glVertexAttrib1svARB) := wglGetProcAddress('glVertexAttrib1svARB');
    if not Assigned(glVertexAttrib1svARB) then Exit;
    Pointer(glVertexAttrib2dARB) := wglGetProcAddress('glVertexAttrib2dARB');
    if not Assigned(glVertexAttrib2dARB) then Exit;
    Pointer(glVertexAttrib2dvARB) := wglGetProcAddress('glVertexAttrib2dvARB');
    if not Assigned(glVertexAttrib2dvARB) then Exit;
    Pointer(glVertexAttrib2fARB) := wglGetProcAddress('glVertexAttrib2fARB');
    if not Assigned(glVertexAttrib2fARB) then Exit;
    Pointer(glVertexAttrib2fvARB) := wglGetProcAddress('glVertexAttrib2fvARB');
    if not Assigned(glVertexAttrib2fvARB) then Exit;
    Pointer(glVertexAttrib2sARB) := wglGetProcAddress('glVertexAttrib2sARB');
    if not Assigned(glVertexAttrib2sARB) then Exit;
    Pointer(glVertexAttrib2svARB) := wglGetProcAddress('glVertexAttrib2svARB');
    if not Assigned(glVertexAttrib2svARB) then Exit;
    Pointer(glVertexAttrib3dARB) := wglGetProcAddress('glVertexAttrib3dARB');
    if not Assigned(glVertexAttrib3dARB) then Exit;
    Pointer(glVertexAttrib3dvARB) := wglGetProcAddress('glVertexAttrib3dvARB');
    if not Assigned(glVertexAttrib3dvARB) then Exit;
    Pointer(glVertexAttrib3fARB) := wglGetProcAddress('glVertexAttrib3fARB');
    if not Assigned(glVertexAttrib3fARB) then Exit;
    Pointer(glVertexAttrib3fvARB) := wglGetProcAddress('glVertexAttrib3fvARB');
    if not Assigned(glVertexAttrib3fvARB) then Exit;
    Pointer(glVertexAttrib3sARB) := wglGetProcAddress('glVertexAttrib3sARB');
    if not Assigned(glVertexAttrib3sARB) then Exit;
    Pointer(glVertexAttrib3svARB) := wglGetProcAddress('glVertexAttrib3svARB');
    if not Assigned(glVertexAttrib3svARB) then Exit;
    Pointer(glVertexAttrib4NbvARB) := wglGetProcAddress('glVertexAttrib4NbvARB');
    if not Assigned(glVertexAttrib4NbvARB) then Exit;
    Pointer(glVertexAttrib4NivARB) := wglGetProcAddress('glVertexAttrib4NivARB');
    if not Assigned(glVertexAttrib4NivARB) then Exit;
    Pointer(glVertexAttrib4NsvARB) := wglGetProcAddress('glVertexAttrib4NsvARB');
    if not Assigned(glVertexAttrib4NsvARB) then Exit;
    Pointer(glVertexAttrib4NubARB) := wglGetProcAddress('glVertexAttrib4NubARB');
    if not Assigned(glVertexAttrib4NubARB) then Exit;
    Pointer(glVertexAttrib4NubvARB) := wglGetProcAddress('glVertexAttrib4NubvARB');
    if not Assigned(glVertexAttrib4NubvARB) then Exit;
    Pointer(glVertexAttrib4NuivARB) := wglGetProcAddress('glVertexAttrib4NuivARB');
    if not Assigned(glVertexAttrib4NuivARB) then Exit;
    Pointer(glVertexAttrib4NusvARB) := wglGetProcAddress('glVertexAttrib4NusvARB');
    if not Assigned(glVertexAttrib4NusvARB) then Exit;
    Pointer(glVertexAttrib4bvARB) := wglGetProcAddress('glVertexAttrib4bvARB');
    if not Assigned(glVertexAttrib4bvARB) then Exit;
    Pointer(glVertexAttrib4dARB) := wglGetProcAddress('glVertexAttrib4dARB');
    if not Assigned(glVertexAttrib4dARB) then Exit;
    Pointer(glVertexAttrib4dvARB) := wglGetProcAddress('glVertexAttrib4dvARB');
    if not Assigned(glVertexAttrib4dvARB) then Exit;
    Pointer(glVertexAttrib4fARB) := wglGetProcAddress('glVertexAttrib4fARB');
    if not Assigned(glVertexAttrib4fARB) then Exit;
    Pointer(glVertexAttrib4fvARB) := wglGetProcAddress('glVertexAttrib4fvARB');
    if not Assigned(glVertexAttrib4fvARB) then Exit;
    Pointer(glVertexAttrib4ivARB) := wglGetProcAddress('glVertexAttrib4ivARB');
    if not Assigned(glVertexAttrib4ivARB) then Exit;
    Pointer(glVertexAttrib4sARB) := wglGetProcAddress('glVertexAttrib4sARB');
    if not Assigned(glVertexAttrib4sARB) then Exit;
    Pointer(glVertexAttrib4svARB) := wglGetProcAddress('glVertexAttrib4svARB');
    if not Assigned(glVertexAttrib4svARB) then Exit;
    Pointer(glVertexAttrib4ubvARB) := wglGetProcAddress('glVertexAttrib4ubvARB');
    if not Assigned(glVertexAttrib4ubvARB) then Exit;
    Pointer(glVertexAttrib4uivARB) := wglGetProcAddress('glVertexAttrib4uivARB');
    if not Assigned(glVertexAttrib4uivARB) then Exit;
    Pointer(glVertexAttrib4usvARB) := wglGetProcAddress('glVertexAttrib4usvARB');
    if not Assigned(glVertexAttrib4usvARB) then Exit;
    Pointer(glVertexAttribPointerARB) := wglGetProcAddress('glVertexAttribPointerARB');
    if not Assigned(glVertexAttribPointerARB) then Exit;
    Pointer(glEnableVertexAttribArrayARB) := wglGetProcAddress('glEnableVertexAttribArrayARB');
    if not Assigned(glEnableVertexAttribArrayARB) then Exit;
    Pointer(glDisableVertexAttribArrayARB) := wglGetProcAddress('glDisableVertexAttribArrayARB');
    if not Assigned(glDisableVertexAttribArrayARB) then Exit;
    Pointer(glProgramStringARB) := wglGetProcAddress('glProgramStringARB');
    if not Assigned(glProgramStringARB) then Exit;
    Pointer(glBindProgramARB) := wglGetProcAddress('glBindProgramARB');
    if not Assigned(glBindProgramARB) then Exit;
    Pointer(glDeleteProgramsARB) := wglGetProcAddress('glDeleteProgramsARB');
    if not Assigned(glDeleteProgramsARB) then Exit;
    Pointer(glGenProgramsARB) := wglGetProcAddress('glGenProgramsARB');
    if not Assigned(glGenProgramsARB) then Exit;
    Pointer(glProgramEnvParameter4dARB) := wglGetProcAddress('glProgramEnvParameter4dARB');
    if not Assigned(glProgramEnvParameter4dARB) then Exit;
    Pointer(glProgramEnvParameter4dvARB) := wglGetProcAddress('glProgramEnvParameter4dvARB');
    if not Assigned(glProgramEnvParameter4dvARB) then Exit;
    Pointer(glProgramEnvParameter4fARB) := wglGetProcAddress('glProgramEnvParameter4fARB');
    if not Assigned(glProgramEnvParameter4fARB) then Exit;
    Pointer(glProgramEnvParameter4fvARB) := wglGetProcAddress('glProgramEnvParameter4fvARB');
    if not Assigned(glProgramEnvParameter4fvARB) then Exit;
    Pointer(glProgramLocalParameter4dARB) := wglGetProcAddress('glProgramLocalParameter4dARB');
    if not Assigned(glProgramLocalParameter4dARB) then Exit;
    Pointer(glProgramLocalParameter4dvARB) := wglGetProcAddress('glProgramLocalParameter4dvARB');
    if not Assigned(glProgramLocalParameter4dvARB) then Exit;
    Pointer(glProgramLocalParameter4fARB) := wglGetProcAddress('glProgramLocalParameter4fARB');
    if not Assigned(glProgramLocalParameter4fARB) then Exit;
    Pointer(glProgramLocalParameter4fvARB) := wglGetProcAddress('glProgramLocalParameter4fvARB');
    if not Assigned(glProgramLocalParameter4fvARB) then Exit;
    Pointer(glGetProgramEnvParameterdvARB) := wglGetProcAddress('glGetProgramEnvParameterdvARB');
    if not Assigned(glGetProgramEnvParameterdvARB) then Exit;
    Pointer(glGetProgramEnvParameterfvARB) := wglGetProcAddress('glGetProgramEnvParameterfvARB');
    if not Assigned(glGetProgramEnvParameterfvARB) then Exit;
    Pointer(glGetProgramLocalParameterdvARB) := wglGetProcAddress('glGetProgramLocalParameterdvARB');
    if not Assigned(glGetProgramLocalParameterdvARB) then Exit;
    Pointer(glGetProgramLocalParameterfvARB) := wglGetProcAddress('glGetProgramLocalParameterfvARB');
    if not Assigned(glGetProgramLocalParameterfvARB) then Exit;
    Pointer(glGetProgramivARB) := wglGetProcAddress('glGetProgramivARB');
    if not Assigned(glGetProgramivARB) then Exit;
    Pointer(glGetProgramStringARB) := wglGetProcAddress('glGetProgramStringARB');
    if not Assigned(glGetProgramStringARB) then Exit;
    Pointer(glGetVertexAttribdvARB) := wglGetProcAddress('glGetVertexAttribdvARB');
    if not Assigned(glGetVertexAttribdvARB) then Exit;
    Pointer(glGetVertexAttribfvARB) := wglGetProcAddress('glGetVertexAttribfvARB');
    if not Assigned(glGetVertexAttribfvARB) then Exit;
    Pointer(glGetVertexAttribivARB) := wglGetProcAddress('glGetVertexAttribivARB');
    if not Assigned(glGetVertexAttribivARB) then Exit;
    Pointer(glGetVertexAttribPointervARB) := wglGetProcAddress('glGetVertexAttribPointervARB');
    if not Assigned(glGetVertexAttribPointervARB) then Exit;
    Pointer(glIsProgramARB) := wglGetProcAddress('glIsProgramARB');
    if not Assigned(glIsProgramARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_fragment_program(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_fragment_program', extstring) then
  begin
    (* Shared entry points *)
    Pointer(glProgramStringARB) := wglGetProcAddress('glProgramStringARB');
    if not Assigned(glProgramStringARB) then Exit;
    Pointer(glBindProgramARB) := wglGetProcAddress('glBindProgramARB');
    if not Assigned(glBindProgramARB) then Exit;
    Pointer(glDeleteProgramsARB) := wglGetProcAddress('glDeleteProgramsARB');
    if not Assigned(glDeleteProgramsARB) then Exit;
    Pointer(glGenProgramsARB) := wglGetProcAddress('glGenProgramsARB');
    if not Assigned(glGenProgramsARB) then Exit;
    Pointer(glProgramEnvParameter4dARB) := wglGetProcAddress('glProgramEnvParameter4dARB');
    if not Assigned(glProgramEnvParameter4dARB) then Exit;
    Pointer(glProgramEnvParameter4dvARB) := wglGetProcAddress('glProgramEnvParameter4dvARB');
    if not Assigned(glProgramEnvParameter4dvARB) then Exit;
    Pointer(glProgramEnvParameter4fARB) := wglGetProcAddress('glProgramEnvParameter4fARB');
    if not Assigned(glProgramEnvParameter4fARB) then Exit;
    Pointer(glProgramEnvParameter4fvARB) := wglGetProcAddress('glProgramEnvParameter4fvARB');
    if not Assigned(glProgramEnvParameter4fvARB) then Exit;
    Pointer(glProgramLocalParameter4dARB) := wglGetProcAddress('glProgramLocalParameter4dARB');
    if not Assigned(glProgramLocalParameter4dARB) then Exit;
    Pointer(glProgramLocalParameter4dvARB) := wglGetProcAddress('glProgramLocalParameter4dvARB');
    if not Assigned(glProgramLocalParameter4dvARB) then Exit;
    Pointer(glProgramLocalParameter4fARB) := wglGetProcAddress('glProgramLocalParameter4fARB');
    if not Assigned(glProgramLocalParameter4fARB) then Exit;
    Pointer(glProgramLocalParameter4fvARB) := wglGetProcAddress('glProgramLocalParameter4fvARB');
    if not Assigned(glProgramLocalParameter4fvARB) then Exit;
    Pointer(glGetProgramEnvParameterdvARB) := wglGetProcAddress('glGetProgramEnvParameterdvARB');
    if not Assigned(glGetProgramEnvParameterdvARB) then Exit;
    Pointer(glGetProgramEnvParameterfvARB) := wglGetProcAddress('glGetProgramEnvParameterfvARB');
    if not Assigned(glGetProgramEnvParameterfvARB) then Exit;
    Pointer(glGetProgramLocalParameterdvARB) := wglGetProcAddress('glGetProgramLocalParameterdvARB');
    if not Assigned(glGetProgramLocalParameterdvARB) then Exit;
    Pointer(glGetProgramLocalParameterfvARB) := wglGetProcAddress('glGetProgramLocalParameterfvARB');
    if not Assigned(glGetProgramLocalParameterfvARB) then Exit;
    Pointer(glGetProgramivARB) := wglGetProcAddress('glGetProgramivARB');
    if not Assigned(glGetProgramivARB) then Exit;
    Pointer(glGetProgramStringARB) := wglGetProcAddress('glGetProgramStringARB');
    if not Assigned(glGetProgramStringARB) then Exit;
    Pointer(glIsProgramARB) := wglGetProcAddress('glIsProgramARB');
    if not Assigned(glIsProgramARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_buffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_buffer_object', extstring) then
  begin
    Pointer(glBindBufferARB) := wglGetProcAddress('glBindBufferARB');
    if not Assigned(glBindBufferARB) then Exit;
    Pointer(glDeleteBuffersARB) := wglGetProcAddress('glDeleteBuffersARB');
    if not Assigned(glDeleteBuffersARB) then Exit;
    Pointer(glGenBuffersARB) := wglGetProcAddress('glGenBuffersARB');
    if not Assigned(glGenBuffersARB) then Exit;
    Pointer(glIsBufferARB) := wglGetProcAddress('glIsBufferARB');
    if not Assigned(glIsBufferARB) then Exit;
    Pointer(glBufferDataARB) := wglGetProcAddress('glBufferDataARB');
    if not Assigned(glBufferDataARB) then Exit;
    Pointer(glBufferSubDataARB) := wglGetProcAddress('glBufferSubDataARB');
    if not Assigned(glBufferSubDataARB) then Exit;
    Pointer(glGetBufferSubDataARB) := wglGetProcAddress('glGetBufferSubDataARB');
    if not Assigned(glGetBufferSubDataARB) then Exit;
    Pointer(glMapBufferARB) := wglGetProcAddress('glMapBufferARB');
    if not Assigned(glMapBufferARB) then Exit;
    Pointer(glUnmapBufferARB) := wglGetProcAddress('glUnmapBufferARB');
    if not Assigned(glUnmapBufferARB) then Exit;
    Pointer(glGetBufferParameterivARB) := wglGetProcAddress('glGetBufferParameterivARB');
    if not Assigned(glGetBufferParameterivARB) then Exit;
    Pointer(glGetBufferPointervARB) := wglGetProcAddress('glGetBufferPointervARB');
    if not Assigned(glGetBufferPointervARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_occlusion_query(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_occlusion_query', extstring) then
  begin
    Pointer(glGenQueriesARB) := wglGetProcAddress('glGenQueriesARB');
    if not Assigned(glGenQueriesARB) then Exit;
    Pointer(glDeleteQueriesARB) := wglGetProcAddress('glDeleteQueriesARB');
    if not Assigned(glDeleteQueriesARB) then Exit;
    Pointer(glIsQueryARB) := wglGetProcAddress('glIsQueryARB');
    if not Assigned(glIsQueryARB) then Exit;
    Pointer(glBeginQueryARB) := wglGetProcAddress('glBeginQueryARB');
    if not Assigned(glBeginQueryARB) then Exit;
    Pointer(glEndQueryARB) := wglGetProcAddress('glEndQueryARB');
    if not Assigned(glEndQueryARB) then Exit;
    Pointer(glGetQueryivARB) := wglGetProcAddress('glGetQueryivARB');
    if not Assigned(glGetQueryivARB) then Exit;
    Pointer(glGetQueryObjectivARB) := wglGetProcAddress('glGetQueryObjectivARB');
    if not Assigned(glGetQueryObjectivARB) then Exit;
    Pointer(glGetQueryObjectuivARB) := wglGetProcAddress('glGetQueryObjectuivARB');
    if not Assigned(glGetQueryObjectuivARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_shader_objects(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shader_objects', extstring) then
  begin
    Pointer(glDeleteObjectARB) := wglGetProcAddress('glDeleteObjectARB');
    if not Assigned(glDeleteObjectARB) then Exit;
    Pointer(glGetHandleARB) := wglGetProcAddress('glGetHandleARB');
    if not Assigned(glGetHandleARB) then Exit;
    Pointer(glDetachObjectARB) := wglGetProcAddress('glDetachObjectARB');
    if not Assigned(glDetachObjectARB) then Exit;
    Pointer(glCreateShaderObjectARB) := wglGetProcAddress('glCreateShaderObjectARB');
    if not Assigned(glCreateShaderObjectARB) then Exit;
    Pointer(glShaderSourceARB) := wglGetProcAddress('glShaderSourceARB');
    if not Assigned(glShaderSourceARB) then Exit;
    Pointer(glCompileShaderARB) := wglGetProcAddress('glCompileShaderARB');
    if not Assigned(glCompileShaderARB) then Exit;
    Pointer(glCreateProgramObjectARB) := wglGetProcAddress('glCreateProgramObjectARB');
    if not Assigned(glCreateProgramObjectARB) then Exit;
    Pointer(glAttachObjectARB) := wglGetProcAddress('glAttachObjectARB');
    if not Assigned(glAttachObjectARB) then Exit;
    Pointer(glLinkProgramARB) := wglGetProcAddress('glLinkProgramARB');
    if not Assigned(glLinkProgramARB) then Exit;
    Pointer(glUseProgramObjectARB) := wglGetProcAddress('glUseProgramObjectARB');
    if not Assigned(glUseProgramObjectARB) then Exit;
    Pointer(glValidateProgramARB) := wglGetProcAddress('glValidateProgramARB');
    if not Assigned(glValidateProgramARB) then Exit;
    Pointer(glUniform1fARB) := wglGetProcAddress('glUniform1fARB');
    if not Assigned(glUniform1fARB) then Exit;
    Pointer(glUniform2fARB) := wglGetProcAddress('glUniform2fARB');
    if not Assigned(glUniform2fARB) then Exit;
    Pointer(glUniform3fARB) := wglGetProcAddress('glUniform3fARB');
    if not Assigned(glUniform3fARB) then Exit;
    Pointer(glUniform4fARB) := wglGetProcAddress('glUniform4fARB');
    if not Assigned(glUniform4fARB) then Exit;
    Pointer(glUniform1iARB) := wglGetProcAddress('glUniform1iARB');
    if not Assigned(glUniform1iARB) then Exit;
    Pointer(glUniform2iARB) := wglGetProcAddress('glUniform2iARB');
    if not Assigned(glUniform2iARB) then Exit;
    Pointer(glUniform3iARB) := wglGetProcAddress('glUniform3iARB');
    if not Assigned(glUniform3iARB) then Exit;
    Pointer(glUniform4iARB) := wglGetProcAddress('glUniform4iARB');
    if not Assigned(glUniform4iARB) then Exit;
    Pointer(glUniform1fvARB) := wglGetProcAddress('glUniform1fvARB');
    if not Assigned(glUniform1fvARB) then Exit;
    Pointer(glUniform2fvARB) := wglGetProcAddress('glUniform2fvARB');
    if not Assigned(glUniform2fvARB) then Exit;
    Pointer(glUniform3fvARB) := wglGetProcAddress('glUniform3fvARB');
    if not Assigned(glUniform3fvARB) then Exit;
    Pointer(glUniform4fvARB) := wglGetProcAddress('glUniform4fvARB');
    if not Assigned(glUniform4fvARB) then Exit;
    Pointer(glUniform1ivARB) := wglGetProcAddress('glUniform1ivARB');
    if not Assigned(glUniform1ivARB) then Exit;
    Pointer(glUniform2ivARB) := wglGetProcAddress('glUniform2ivARB');
    if not Assigned(glUniform2ivARB) then Exit;
    Pointer(glUniform3ivARB) := wglGetProcAddress('glUniform3ivARB');
    if not Assigned(glUniform3ivARB) then Exit;
    Pointer(glUniform4ivARB) := wglGetProcAddress('glUniform4ivARB');
    if not Assigned(glUniform4ivARB) then Exit;
    Pointer(glUniformMatrix2fvARB) := wglGetProcAddress('glUniformMatrix2fvARB');
    if not Assigned(glUniformMatrix2fvARB) then Exit;
    Pointer(glUniformMatrix3fvARB) := wglGetProcAddress('glUniformMatrix3fvARB');
    if not Assigned(glUniformMatrix3fvARB) then Exit;
    Pointer(glUniformMatrix4fvARB) := wglGetProcAddress('glUniformMatrix4fvARB');
    if not Assigned(glUniformMatrix4fvARB) then Exit;
    Pointer(glGetObjectParameterfvARB) := wglGetProcAddress('glGetObjectParameterfvARB');
    if not Assigned(glGetObjectParameterfvARB) then Exit;
    Pointer(glGetObjectParameterivARB) := wglGetProcAddress('glGetObjectParameterivARB');
    if not Assigned(glGetObjectParameterivARB) then Exit;
    Pointer(glGetInfoLogARB) := wglGetProcAddress('glGetInfoLogARB');
    if not Assigned(glGetInfoLogARB) then Exit;
    Pointer(glGetAttachedObjectsARB) := wglGetProcAddress('glGetAttachedObjectsARB');
    if not Assigned(glGetAttachedObjectsARB) then Exit;
    Pointer(glGetUniformLocationARB) := wglGetProcAddress('glGetUniformLocationARB');
    if not Assigned(glGetUniformLocationARB) then Exit;
    Pointer(glGetActiveUniformARB) := wglGetProcAddress('glGetActiveUniformARB');
    if not Assigned(glGetActiveUniformARB) then Exit;
    Pointer(glGetUniformfvARB) := wglGetProcAddress('glGetUniformfvARB');
    if not Assigned(glGetUniformfvARB) then Exit;
    Pointer(glGetUniformivARB) := wglGetProcAddress('glGetUniformivARB');
    if not Assigned(glGetUniformivARB) then Exit;
    Pointer(glGetShaderSourceARB) := wglGetProcAddress('glGetShaderSourceARB');
    if not Assigned(glGetShaderSourceARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_shader(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_shader', extstring) then
  begin
    Pointer(glBindAttribLocationARB) := wglGetProcAddress('glBindAttribLocationARB');
    if not Assigned(glBindAttribLocationARB) then Exit;
    Pointer(glGetActiveAttribARB) := wglGetProcAddress('glGetActiveAttribARB');
    if not Assigned(glGetActiveAttribARB) then Exit;
    Pointer(glGetAttribLocationARB) := wglGetProcAddress('glGetAttribLocationARB');
    if not Assigned(glGetAttribLocationARB) then Exit;
    (* Shared entry points *)
    Pointer(glVertexAttrib1fARB) := wglGetProcAddress('glVertexAttrib1fARB');
    if not Assigned(glVertexAttrib1fARB) then Exit;
    Pointer(glVertexAttrib1sARB) := wglGetProcAddress('glVertexAttrib1sARB');
    if not Assigned(glVertexAttrib1sARB) then Exit;
    Pointer(glVertexAttrib1dARB) := wglGetProcAddress('glVertexAttrib1dARB');
    if not Assigned(glVertexAttrib1dARB) then Exit;
    Pointer(glVertexAttrib2fARB) := wglGetProcAddress('glVertexAttrib2fARB');
    if not Assigned(glVertexAttrib2fARB) then Exit;
    Pointer(glVertexAttrib2sARB) := wglGetProcAddress('glVertexAttrib2sARB');
    if not Assigned(glVertexAttrib2sARB) then Exit;
    Pointer(glVertexAttrib2dARB) := wglGetProcAddress('glVertexAttrib2dARB');
    if not Assigned(glVertexAttrib2dARB) then Exit;
    Pointer(glVertexAttrib3fARB) := wglGetProcAddress('glVertexAttrib3fARB');
    if not Assigned(glVertexAttrib3fARB) then Exit;
    Pointer(glVertexAttrib3sARB) := wglGetProcAddress('glVertexAttrib3sARB');
    if not Assigned(glVertexAttrib3sARB) then Exit;
    Pointer(glVertexAttrib3dARB) := wglGetProcAddress('glVertexAttrib3dARB');
    if not Assigned(glVertexAttrib3dARB) then Exit;
    Pointer(glVertexAttrib4fARB) := wglGetProcAddress('glVertexAttrib4fARB');
    if not Assigned(glVertexAttrib4fARB) then Exit;
    Pointer(glVertexAttrib4sARB) := wglGetProcAddress('glVertexAttrib4sARB');
    if not Assigned(glVertexAttrib4sARB) then Exit;
    Pointer(glVertexAttrib4dARB) := wglGetProcAddress('glVertexAttrib4dARB');
    if not Assigned(glVertexAttrib4dARB) then Exit;
    Pointer(glVertexAttrib4NubARB) := wglGetProcAddress('glVertexAttrib4NubARB');
    if not Assigned(glVertexAttrib4NubARB) then Exit;
    Pointer(glVertexAttrib1fvARB) := wglGetProcAddress('glVertexAttrib1fvARB');
    if not Assigned(glVertexAttrib1fvARB) then Exit;
    Pointer(glVertexAttrib1svARB) := wglGetProcAddress('glVertexAttrib1svARB');
    if not Assigned(glVertexAttrib1svARB) then Exit;
    Pointer(glVertexAttrib1dvARB) := wglGetProcAddress('glVertexAttrib1dvARB');
    if not Assigned(glVertexAttrib1dvARB) then Exit;
    Pointer(glVertexAttrib2fvARB) := wglGetProcAddress('glVertexAttrib2fvARB');
    if not Assigned(glVertexAttrib2fvARB) then Exit;
    Pointer(glVertexAttrib2svARB) := wglGetProcAddress('glVertexAttrib2svARB');
    if not Assigned(glVertexAttrib2svARB) then Exit;
    Pointer(glVertexAttrib2dvARB) := wglGetProcAddress('glVertexAttrib2dvARB');
    if not Assigned(glVertexAttrib2dvARB) then Exit;
    Pointer(glVertexAttrib3fvARB) := wglGetProcAddress('glVertexAttrib3fvARB');
    if not Assigned(glVertexAttrib3fvARB) then Exit;
    Pointer(glVertexAttrib3svARB) := wglGetProcAddress('glVertexAttrib3svARB');
    if not Assigned(glVertexAttrib3svARB) then Exit;
    Pointer(glVertexAttrib3dvARB) := wglGetProcAddress('glVertexAttrib3dvARB');
    if not Assigned(glVertexAttrib3dvARB) then Exit;
    Pointer(glVertexAttrib4fvARB) := wglGetProcAddress('glVertexAttrib4fvARB');
    if not Assigned(glVertexAttrib4fvARB) then Exit;
    Pointer(glVertexAttrib4svARB) := wglGetProcAddress('glVertexAttrib4svARB');
    if not Assigned(glVertexAttrib4svARB) then Exit;
    Pointer(glVertexAttrib4dvARB) := wglGetProcAddress('glVertexAttrib4dvARB');
    if not Assigned(glVertexAttrib4dvARB) then Exit;
    Pointer(glVertexAttrib4ivARB) := wglGetProcAddress('glVertexAttrib4ivARB');
    if not Assigned(glVertexAttrib4ivARB) then Exit;
    Pointer(glVertexAttrib4bvARB) := wglGetProcAddress('glVertexAttrib4bvARB');
    if not Assigned(glVertexAttrib4bvARB) then Exit;
    Pointer(glVertexAttrib4ubvARB) := wglGetProcAddress('glVertexAttrib4ubvARB');
    if not Assigned(glVertexAttrib4ubvARB) then Exit;
    Pointer(glVertexAttrib4usvARB) := wglGetProcAddress('glVertexAttrib4usvARB');
    if not Assigned(glVertexAttrib4usvARB) then Exit;
    Pointer(glVertexAttrib4uivARB) := wglGetProcAddress('glVertexAttrib4uivARB');
    if not Assigned(glVertexAttrib4uivARB) then Exit;
    Pointer(glVertexAttrib4NbvARB) := wglGetProcAddress('glVertexAttrib4NbvARB');
    if not Assigned(glVertexAttrib4NbvARB) then Exit;
    Pointer(glVertexAttrib4NsvARB) := wglGetProcAddress('glVertexAttrib4NsvARB');
    if not Assigned(glVertexAttrib4NsvARB) then Exit;
    Pointer(glVertexAttrib4NivARB) := wglGetProcAddress('glVertexAttrib4NivARB');
    if not Assigned(glVertexAttrib4NivARB) then Exit;
    Pointer(glVertexAttrib4NubvARB) := wglGetProcAddress('glVertexAttrib4NubvARB');
    if not Assigned(glVertexAttrib4NubvARB) then Exit;
    Pointer(glVertexAttrib4NusvARB) := wglGetProcAddress('glVertexAttrib4NusvARB');
    if not Assigned(glVertexAttrib4NusvARB) then Exit;
    Pointer(glVertexAttrib4NuivARB) := wglGetProcAddress('glVertexAttrib4NuivARB');
    if not Assigned(glVertexAttrib4NuivARB) then Exit;
    Pointer(glVertexAttribPointerARB) := wglGetProcAddress('glVertexAttribPointerARB');
    if not Assigned(glVertexAttribPointerARB) then Exit;
    Pointer(glEnableVertexAttribArrayARB) := wglGetProcAddress('glEnableVertexAttribArrayARB');
    if not Assigned(glEnableVertexAttribArrayARB) then Exit;
    Pointer(glDisableVertexAttribArrayARB) := wglGetProcAddress('glDisableVertexAttribArrayARB');
    if not Assigned(glDisableVertexAttribArrayARB) then Exit;
    Pointer(glGetVertexAttribdvARB) := wglGetProcAddress('glGetVertexAttribdvARB');
    if not Assigned(glGetVertexAttribdvARB) then Exit;
    Pointer(glGetVertexAttribfvARB) := wglGetProcAddress('glGetVertexAttribfvARB');
    if not Assigned(glGetVertexAttribfvARB) then Exit;
    Pointer(glGetVertexAttribivARB) := wglGetProcAddress('glGetVertexAttribivARB');
    if not Assigned(glGetVertexAttribivARB) then Exit;
    Pointer(glGetVertexAttribPointervARB) := wglGetProcAddress('glGetVertexAttribPointervARB');
    if not Assigned(glGetVertexAttribPointervARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_fragment_shader(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_fragment_shader', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_shading_language_100(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shading_language_100', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_non_power_of_two(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_non_power_of_two', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_point_sprite(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_point_sprite', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_fragment_program_shadow(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_fragment_program_shadow', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_draw_buffers(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_draw_buffers', extstring) then
  begin
    Pointer(glDrawBuffersARB) := wglGetProcAddress('glDrawBuffersARB');
    if not Assigned(glDrawBuffersARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_rectangle(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_rectangle', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_color_buffer_float(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_color_buffer_float', extstring) then
  begin
    Pointer(glClampColorARB) := wglGetProcAddress('glClampColorARB');
    if not Assigned(glClampColorARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_half_float_pixel(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_half_float_pixel', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_float(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_float', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_pixel_buffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_pixel_buffer_object', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_depth_buffer_float(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_depth_buffer_float', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_draw_instanced(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_draw_instanced', extstring) then
  begin
    Pointer(glDrawArraysInstancedARB) := wglGetProcAddress('glDrawArraysInstancedARB');
    if not Assigned(glDrawArraysInstancedARB) then Exit;
    Pointer(glDrawElementsInstancedARB) := wglGetProcAddress('glDrawElementsInstancedARB');
    if not Assigned(glDrawElementsInstancedARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_framebuffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_framebuffer_object', extstring) then
  begin
    Pointer(glIsRenderbuffer) := wglGetProcAddress('glIsRenderbuffer');
    if not Assigned(glIsRenderbuffer) then Exit;
    Pointer(glBindRenderbuffer) := wglGetProcAddress('glBindRenderbuffer');
    if not Assigned(glBindRenderbuffer) then Exit;
    Pointer(glDeleteRenderbuffers) := wglGetProcAddress('glDeleteRenderbuffers');
    if not Assigned(glDeleteRenderbuffers) then Exit;
    Pointer(glGenRenderbuffers) := wglGetProcAddress('glGenRenderbuffers');
    if not Assigned(glGenRenderbuffers) then Exit;
    Pointer(glRenderbufferStorage) := wglGetProcAddress('glRenderbufferStorage');
    if not Assigned(glRenderbufferStorage) then Exit;
    Pointer(glGetRenderbufferParameteriv) := wglGetProcAddress('glGetRenderbufferParameteriv');
    if not Assigned(glGetRenderbufferParameteriv) then Exit;
    Pointer(glIsFramebuffer) := wglGetProcAddress('glIsFramebuffer');
    if not Assigned(glIsFramebuffer) then Exit;
    Pointer(glBindFramebuffer) := wglGetProcAddress('glBindFramebuffer');
    if not Assigned(glBindFramebuffer) then Exit;
    Pointer(glDeleteFramebuffers) := wglGetProcAddress('glDeleteFramebuffers');
    if not Assigned(glDeleteFramebuffers) then Exit;
    Pointer(glGenFramebuffers) := wglGetProcAddress('glGenFramebuffers');
    if not Assigned(glGenFramebuffers) then Exit;
    Pointer(glCheckFramebufferStatus) := wglGetProcAddress('glCheckFramebufferStatus');
    if not Assigned(glCheckFramebufferStatus) then Exit;
    Pointer(glFramebufferTexture1D) := wglGetProcAddress('glFramebufferTexture1D');
    if not Assigned(glFramebufferTexture1D) then Exit;
    Pointer(glFramebufferTexture2D) := wglGetProcAddress('glFramebufferTexture2D');
    if not Assigned(glFramebufferTexture2D) then Exit;
    Pointer(glFramebufferTexture3D) := wglGetProcAddress('glFramebufferTexture3D');
    if not Assigned(glFramebufferTexture3D) then Exit;
    Pointer(glFramebufferRenderbuffer) := wglGetProcAddress('glFramebufferRenderbuffer');
    if not Assigned(glFramebufferRenderbuffer) then Exit;
    Pointer(glGetFramebufferAttachmentParameteriv) := wglGetProcAddress('glGetFramebufferAttachmentParameteriv');
    if not Assigned(glGetFramebufferAttachmentParameteriv) then Exit;
    Pointer(glGenerateMipmap) := wglGetProcAddress('glGenerateMipmap');
    if not Assigned(glGenerateMipmap) then Exit;
    Pointer(glBlitFramebuffer) := wglGetProcAddress('glBlitFramebuffer');
    if not Assigned(glBlitFramebuffer) then Exit;
    Pointer(glRenderbufferStorageMultisample) := wglGetProcAddress('glRenderbufferStorageMultisample');
    if not Assigned(glRenderbufferStorageMultisample) then Exit;
    Pointer(glFramebufferTextureLayer) := wglGetProcAddress('glFramebufferTextureLayer');
    if not Assigned(glFramebufferTextureLayer) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_framebuffer_sRGB(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_framebuffer_sRGB', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_geometry_shader4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_geometry_shader4', extstring) then
  begin
    Pointer(glProgramParameteriARB) := wglGetProcAddress('glProgramParameteriARB');
    if not Assigned(glProgramParameteriARB) then Exit;
    Pointer(glFramebufferTextureARB) := wglGetProcAddress('glFramebufferTextureARB');
    if not Assigned(glFramebufferTextureARB) then Exit;
    Pointer(glFramebufferTextureLayerARB) := wglGetProcAddress('glFramebufferTextureLayerARB');
    if not Assigned(glFramebufferTextureLayerARB) then Exit;
    Pointer(glFramebufferTextureFaceARB) := wglGetProcAddress('glFramebufferTextureFaceARB');
    if not Assigned(glFramebufferTextureFaceARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_half_float_vertex(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_half_float_vertex', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_instanced_arrays(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_instanced_arrays', extstring) then
  begin
    Pointer(glVertexAttribDivisorARB) := wglGetProcAddress('glVertexAttribDivisorARB');
    if not Assigned(glVertexAttribDivisorARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_map_buffer_range(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_map_buffer_range', extstring) then
  begin
    Pointer(glMapBufferRange) := wglGetProcAddress('glMapBufferRange');
    if not Assigned(glMapBufferRange) then Exit;
    Pointer(glFlushMappedBufferRange) := wglGetProcAddress('glFlushMappedBufferRange');
    if not Assigned(glFlushMappedBufferRange) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_buffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_buffer_object', extstring) then
  begin
    Pointer(glTexBufferARB) := wglGetProcAddress('glTexBufferARB');
    if not Assigned(glTexBufferARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_compression_rgtc(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_compression_rgtc', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_rg(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_rg', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_array_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_array_object', extstring) then
  begin
    Pointer(glBindVertexArray) := wglGetProcAddress('glBindVertexArray');
    if not Assigned(glBindVertexArray) then Exit;
    Pointer(glDeleteVertexArrays) := wglGetProcAddress('glDeleteVertexArrays');
    if not Assigned(glDeleteVertexArrays) then Exit;
    Pointer(glGenVertexArrays) := wglGetProcAddress('glGenVertexArrays');
    if not Assigned(glGenVertexArrays) then Exit;
    Pointer(glIsVertexArray) := wglGetProcAddress('glIsVertexArray');
    if not Assigned(glIsVertexArray) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_uniform_buffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_uniform_buffer_object', extstring) then
  begin
    Pointer(glGetUniformIndices) := wglGetProcAddress('glGetUniformIndices');
    if not Assigned(glGetUniformIndices) then Exit;
    Pointer(glGetActiveUniformsiv) := wglGetProcAddress('glGetActiveUniformsiv');
    if not Assigned(glGetActiveUniformsiv) then Exit;
    Pointer(glGetActiveUniformName) := wglGetProcAddress('glGetActiveUniformName');
    if not Assigned(glGetActiveUniformName) then Exit;
    Pointer(glGetUniformBlockIndex) := wglGetProcAddress('glGetUniformBlockIndex');
    if not Assigned(glGetUniformBlockIndex) then Exit;
    Pointer(glGetActiveUniformBlockiv) := wglGetProcAddress('glGetActiveUniformBlockiv');
    if not Assigned(glGetActiveUniformBlockiv) then Exit;
    Pointer(glGetActiveUniformBlockName) := wglGetProcAddress('glGetActiveUniformBlockName');
    if not Assigned(glGetActiveUniformBlockName) then Exit;
    Pointer(glUniformBlockBinding) := wglGetProcAddress('glUniformBlockBinding');
    if not Assigned(glUniformBlockBinding) then Exit;
    (* Shared entry points *)
    Pointer(glBindBufferRange) := wglGetProcAddress('glBindBufferRange');
    if not Assigned(glBindBufferRange) then Exit;
    Pointer(glBindBufferBase) := wglGetProcAddress('glBindBufferBase');
    if not Assigned(glBindBufferBase) then Exit;
    Pointer(glGetIntegeri_v) := wglGetProcAddress('glGetIntegeri_v');
    if not Assigned(glGetIntegeri_v) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_compatibility(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_compatibility', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_copy_buffer(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_copy_buffer', extstring) then
  begin
    Pointer(glCopyBufferSubData) := wglGetProcAddress('glCopyBufferSubData');
    if not Assigned(glCopyBufferSubData) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_shader_texture_lod(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shader_texture_lod', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_depth_clamp(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_depth_clamp', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_draw_elements_base_vertex(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_draw_elements_base_vertex', extstring) then
  begin
    Pointer(glDrawElementsBaseVertex) := wglGetProcAddress('glDrawElementsBaseVertex');
    if not Assigned(glDrawElementsBaseVertex) then Exit;
    Pointer(glDrawRangeElementsBaseVertex) := wglGetProcAddress('glDrawRangeElementsBaseVertex');
    if not Assigned(glDrawRangeElementsBaseVertex) then Exit;
    Pointer(glDrawElementsInstancedBaseVertex) := wglGetProcAddress('glDrawElementsInstancedBaseVertex');
    if not Assigned(glDrawElementsInstancedBaseVertex) then Exit;
    Pointer(glMultiDrawElementsBaseVertex) := wglGetProcAddress('glMultiDrawElementsBaseVertex');
    if not Assigned(glMultiDrawElementsBaseVertex) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_fragment_coord_conventions(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_fragment_coord_conventions', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_provoking_vertex(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_provoking_vertex', extstring) then
  begin
    Pointer(glProvokingVertex) := wglGetProcAddress('glProvokingVertex');
    if not Assigned(glProvokingVertex) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_seamless_cube_map(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_seamless_cube_map', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_sync(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_sync', extstring) then
  begin
    Pointer(glFenceSync) := wglGetProcAddress('glFenceSync');
    if not Assigned(glFenceSync) then Exit;
    Pointer(glIsSync) := wglGetProcAddress('glIsSync');
    if not Assigned(glIsSync) then Exit;
    Pointer(glDeleteSync) := wglGetProcAddress('glDeleteSync');
    if not Assigned(glDeleteSync) then Exit;
    Pointer(glClientWaitSync) := wglGetProcAddress('glClientWaitSync');
    if not Assigned(glClientWaitSync) then Exit;
    Pointer(glWaitSync) := wglGetProcAddress('glWaitSync');
    if not Assigned(glWaitSync) then Exit;
    Pointer(glGetInteger64v) := wglGetProcAddress('glGetInteger64v');
    if not Assigned(glGetInteger64v) then Exit;
    Pointer(glGetSynciv) := wglGetProcAddress('glGetSynciv');
    if not Assigned(glGetSynciv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_multisample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_multisample', extstring) then
  begin
    Pointer(glTexImage2DMultisample) := wglGetProcAddress('glTexImage2DMultisample');
    if not Assigned(glTexImage2DMultisample) then Exit;
    Pointer(glTexImage3DMultisample) := wglGetProcAddress('glTexImage3DMultisample');
    if not Assigned(glTexImage3DMultisample) then Exit;
    Pointer(glGetMultisamplefv) := wglGetProcAddress('glGetMultisamplefv');
    if not Assigned(glGetMultisamplefv) then Exit;
    Pointer(glSampleMaski) := wglGetProcAddress('glSampleMaski');
    if not Assigned(glSampleMaski) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_array_bgra(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_array_bgra', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_draw_buffers_blend(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_draw_buffers_blend', extstring) then
  begin
    Pointer(glBlendEquationi) := wglGetProcAddress('glBlendEquationi');
    if not Assigned(glBlendEquationi) then Exit;
    Pointer(glBlendEquationSeparatei) := wglGetProcAddress('glBlendEquationSeparatei');
    if not Assigned(glBlendEquationSeparatei) then Exit;
    Pointer(glBlendFunci) := wglGetProcAddress('glBlendFunci');
    if not Assigned(glBlendFunci) then Exit;
    Pointer(glBlendFuncSeparatei) := wglGetProcAddress('glBlendFuncSeparatei');
    if not Assigned(glBlendFuncSeparatei) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_sample_shading(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_sample_shading', extstring) then
  begin
    Pointer(glMinSampleShading) := wglGetProcAddress('glMinSampleShading');
    if not Assigned(glMinSampleShading) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_cube_map_array(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_cube_map_array', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_gather(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_gather', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_query_lod(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_query_lod', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_shading_language_include(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shading_language_include', extstring) then
  begin
    Pointer(glNamedStringARB) := wglGetProcAddress('glNamedStringARB');
    if not Assigned(glNamedStringARB) then Exit;
    Pointer(glDeleteNamedStringARB) := wglGetProcAddress('glDeleteNamedStringARB');
    if not Assigned(glDeleteNamedStringARB) then Exit;
    Pointer(glCompileShaderIncludeARB) := wglGetProcAddress('glCompileShaderIncludeARB');
    if not Assigned(glCompileShaderIncludeARB) then Exit;
    Pointer(glIsNamedStringARB) := wglGetProcAddress('glIsNamedStringARB');
    if not Assigned(glIsNamedStringARB) then Exit;
    Pointer(glGetNamedStringARB) := wglGetProcAddress('glGetNamedStringARB');
    if not Assigned(glGetNamedStringARB) then Exit;
    Pointer(glGetNamedStringivARB) := wglGetProcAddress('glGetNamedStringivARB');
    if not Assigned(glGetNamedStringivARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_compression_bptc(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_compression_bptc', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_blend_func_extended(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_blend_func_extended', extstring) then
  begin
    Pointer(glBindFragDataLocationIndexed) := wglGetProcAddress('glBindFragDataLocationIndexed');
    if not Assigned(glBindFragDataLocationIndexed) then Exit;
    Pointer(glGetFragDataIndex) := wglGetProcAddress('glGetFragDataIndex');
    if not Assigned(glGetFragDataIndex) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_explicit_attrib_location(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_explicit_attrib_location', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_occlusion_query2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_occlusion_query2', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_sampler_objects(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_sampler_objects', extstring) then
  begin
    Pointer(glGenSamplers) := wglGetProcAddress('glGenSamplers');
    if not Assigned(glGenSamplers) then Exit;
    Pointer(glDeleteSamplers) := wglGetProcAddress('glDeleteSamplers');
    if not Assigned(glDeleteSamplers) then Exit;
    Pointer(glIsSampler) := wglGetProcAddress('glIsSampler');
    if not Assigned(glIsSampler) then Exit;
    Pointer(glBindSampler) := wglGetProcAddress('glBindSampler');
    if not Assigned(glBindSampler) then Exit;
    Pointer(glSamplerParameteri) := wglGetProcAddress('glSamplerParameteri');
    if not Assigned(glSamplerParameteri) then Exit;
    Pointer(glSamplerParameteriv) := wglGetProcAddress('glSamplerParameteriv');
    if not Assigned(glSamplerParameteriv) then Exit;
    Pointer(glSamplerParameterf) := wglGetProcAddress('glSamplerParameterf');
    if not Assigned(glSamplerParameterf) then Exit;
    Pointer(glSamplerParameterfv) := wglGetProcAddress('glSamplerParameterfv');
    if not Assigned(glSamplerParameterfv) then Exit;
    Pointer(glSamplerParameterIiv) := wglGetProcAddress('glSamplerParameterIiv');
    if not Assigned(glSamplerParameterIiv) then Exit;
    Pointer(glSamplerParameterIuiv) := wglGetProcAddress('glSamplerParameterIuiv');
    if not Assigned(glSamplerParameterIuiv) then Exit;
    Pointer(glGetSamplerParameteriv) := wglGetProcAddress('glGetSamplerParameteriv');
    if not Assigned(glGetSamplerParameteriv) then Exit;
    Pointer(glGetSamplerParameterIiv) := wglGetProcAddress('glGetSamplerParameterIiv');
    if not Assigned(glGetSamplerParameterIiv) then Exit;
    Pointer(glGetSamplerParameterfv) := wglGetProcAddress('glGetSamplerParameterfv');
    if not Assigned(glGetSamplerParameterfv) then Exit;
    //Pointer(glGetSamplerParameterIfv) := wglGetProcAddress('glGetSamplerParameterIfv');
    //if not Assigned(glGetSamplerParameterIfv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_shader_bit_encoding(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shader_bit_encoding', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_rgb10_a2ui(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_rgb10_a2ui', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_texture_swizzle(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_swizzle', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_timer_query(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_timer_query', extstring) then
  begin
    Pointer(glQueryCounter) := wglGetProcAddress('glQueryCounter');
    if not Assigned(glQueryCounter) then Exit;
    Pointer(glGetQueryObjecti64v) := wglGetProcAddress('glGetQueryObjecti64v');
    if not Assigned(glGetQueryObjecti64v) then Exit;
    Pointer(glGetQueryObjectui64v) := wglGetProcAddress('glGetQueryObjectui64v');
    if not Assigned(glGetQueryObjectui64v) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_type_2_10_10_10_rev(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_type_2_10_10_10_rev', extstring) then
  begin
    Pointer(glVertexP2ui) := wglGetProcAddress('glVertexP2ui');
    if not Assigned(glVertexP2ui) then Exit;
    Pointer(glVertexP2uiv) := wglGetProcAddress('glVertexP2uiv');
    if not Assigned(glVertexP2uiv) then Exit;
    Pointer(glVertexP3ui) := wglGetProcAddress('glVertexP3ui');
    if not Assigned(glVertexP3ui) then Exit;
    Pointer(glVertexP3uiv) := wglGetProcAddress('glVertexP3uiv');
    if not Assigned(glVertexP3uiv) then Exit;
    Pointer(glVertexP4ui) := wglGetProcAddress('glVertexP4ui');
    if not Assigned(glVertexP4ui) then Exit;
    Pointer(glVertexP4uiv) := wglGetProcAddress('glVertexP4uiv');
    if not Assigned(glVertexP4uiv) then Exit;
    Pointer(glTexCoordP1ui) := wglGetProcAddress('glTexCoordP1ui');
    if not Assigned(glTexCoordP1ui) then Exit;
    Pointer(glTexCoordP1uiv) := wglGetProcAddress('glTexCoordP1uiv');
    if not Assigned(glTexCoordP1uiv) then Exit;
    Pointer(glTexCoordP2ui) := wglGetProcAddress('glTexCoordP2ui');
    if not Assigned(glTexCoordP2ui) then Exit;
    Pointer(glTexCoordP2uiv) := wglGetProcAddress('glTexCoordP2uiv');
    if not Assigned(glTexCoordP2uiv) then Exit;
    Pointer(glTexCoordP3ui) := wglGetProcAddress('glTexCoordP3ui');
    if not Assigned(glTexCoordP3ui) then Exit;
    Pointer(glTexCoordP3uiv) := wglGetProcAddress('glTexCoordP3uiv');
    if not Assigned(glTexCoordP3uiv) then Exit;
    Pointer(glTexCoordP4ui) := wglGetProcAddress('glTexCoordP4ui');
    if not Assigned(glTexCoordP4ui) then Exit;
    Pointer(glTexCoordP4uiv) := wglGetProcAddress('glTexCoordP4uiv');
    if not Assigned(glTexCoordP4uiv) then Exit;
    Pointer(glMultiTexCoordP1ui) := wglGetProcAddress('glMultiTexCoordP1ui');
    if not Assigned(glMultiTexCoordP1ui) then Exit;
    Pointer(glMultiTexCoordP1uiv) := wglGetProcAddress('glMultiTexCoordP1uiv');
    if not Assigned(glMultiTexCoordP1uiv) then Exit;
    Pointer(glMultiTexCoordP2ui) := wglGetProcAddress('glMultiTexCoordP2ui');
    if not Assigned(glMultiTexCoordP2ui) then Exit;
    Pointer(glMultiTexCoordP2uiv) := wglGetProcAddress('glMultiTexCoordP2uiv');
    if not Assigned(glMultiTexCoordP2uiv) then Exit;
    Pointer(glMultiTexCoordP3ui) := wglGetProcAddress('glMultiTexCoordP3ui');
    if not Assigned(glMultiTexCoordP3ui) then Exit;
    Pointer(glMultiTexCoordP3uiv) := wglGetProcAddress('glMultiTexCoordP3uiv');
    if not Assigned(glMultiTexCoordP3uiv) then Exit;
    Pointer(glMultiTexCoordP4ui) := wglGetProcAddress('glMultiTexCoordP4ui');
    if not Assigned(glMultiTexCoordP4ui) then Exit;
    Pointer(glMultiTexCoordP4uiv) := wglGetProcAddress('glMultiTexCoordP4uiv');
    if not Assigned(glMultiTexCoordP4uiv) then Exit;
    Pointer(glNormalP3ui) := wglGetProcAddress('glNormalP3ui');
    if not Assigned(glNormalP3ui) then Exit;
    Pointer(glNormalP3uiv) := wglGetProcAddress('glNormalP3uiv');
    if not Assigned(glNormalP3uiv) then Exit;
    Pointer(glColorP3ui) := wglGetProcAddress('glColorP3ui');
    if not Assigned(glColorP3ui) then Exit;
    Pointer(glColorP3uiv) := wglGetProcAddress('glColorP3uiv');
    if not Assigned(glColorP3uiv) then Exit;
    Pointer(glColorP4ui) := wglGetProcAddress('glColorP4ui');
    if not Assigned(glColorP4ui) then Exit;
    Pointer(glColorP4uiv) := wglGetProcAddress('glColorP4uiv');
    if not Assigned(glColorP4uiv) then Exit;
    Pointer(glSecondaryColorP3ui) := wglGetProcAddress('glSecondaryColorP3ui');
    if not Assigned(glSecondaryColorP3ui) then Exit;
    Pointer(glSecondaryColorP3uiv) := wglGetProcAddress('glSecondaryColorP3uiv');
    if not Assigned(glSecondaryColorP3uiv) then Exit;
    Pointer(glVertexAttribP1ui) := wglGetProcAddress('glVertexAttribP1ui');
    if not Assigned(glVertexAttribP1ui) then Exit;
    Pointer(glVertexAttribP1uiv) := wglGetProcAddress('glVertexAttribP1uiv');
    if not Assigned(glVertexAttribP1uiv) then Exit;
    Pointer(glVertexAttribP2ui) := wglGetProcAddress('glVertexAttribP2ui');
    if not Assigned(glVertexAttribP2ui) then Exit;
    Pointer(glVertexAttribP2uiv) := wglGetProcAddress('glVertexAttribP2uiv');
    if not Assigned(glVertexAttribP2uiv) then Exit;
    Pointer(glVertexAttribP3ui) := wglGetProcAddress('glVertexAttribP3ui');
    if not Assigned(glVertexAttribP3ui) then Exit;
    Pointer(glVertexAttribP3uiv) := wglGetProcAddress('glVertexAttribP3uiv');
    if not Assigned(glVertexAttribP3uiv) then Exit;
    Pointer(glVertexAttribP4ui) := wglGetProcAddress('glVertexAttribP4ui');
    if not Assigned(glVertexAttribP4ui) then Exit;
    Pointer(glVertexAttribP4uiv) := wglGetProcAddress('glVertexAttribP4uiv');
    if not Assigned(glVertexAttribP4uiv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_draw_indirect(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_draw_indirect', extstring) then
  begin
    Pointer(glDrawArraysIndirect) := wglGetProcAddress('glDrawArraysIndirect');
    if not Assigned(glDrawArraysIndirect) then Exit;
    Pointer(glDrawElementsIndirect) := wglGetProcAddress('glDrawElementsIndirect');
    if not Assigned(glDrawElementsIndirect) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_gpu_shader5(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_gpu_shader5', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_gpu_shader_fp64(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_gpu_shader_fp64', extstring) then
  begin
    Pointer(glUniform1d) := wglGetProcAddress('glUniform1d');
    if not Assigned(glUniform1d) then Exit;
    Pointer(glUniform2d) := wglGetProcAddress('glUniform2d');
    if not Assigned(glUniform2d) then Exit;
    Pointer(glUniform3d) := wglGetProcAddress('glUniform3d');
    if not Assigned(glUniform3d) then Exit;
    Pointer(glUniform4d) := wglGetProcAddress('glUniform4d');
    if not Assigned(glUniform4d) then Exit;
    Pointer(glUniform1dv) := wglGetProcAddress('glUniform1dv');
    if not Assigned(glUniform1dv) then Exit;
    Pointer(glUniform2dv) := wglGetProcAddress('glUniform2dv');
    if not Assigned(glUniform2dv) then Exit;
    Pointer(glUniform3dv) := wglGetProcAddress('glUniform3dv');
    if not Assigned(glUniform3dv) then Exit;
    Pointer(glUniform4dv) := wglGetProcAddress('glUniform4dv');
    if not Assigned(glUniform4dv) then Exit;
    Pointer(glUniformMatrix2dv) := wglGetProcAddress('glUniformMatrix2dv');
    if not Assigned(glUniformMatrix2dv) then Exit;
    Pointer(glUniformMatrix3dv) := wglGetProcAddress('glUniformMatrix3dv');
    if not Assigned(glUniformMatrix3dv) then Exit;
    Pointer(glUniformMatrix4dv) := wglGetProcAddress('glUniformMatrix4dv');
    if not Assigned(glUniformMatrix4dv) then Exit;
    Pointer(glUniformMatrix2x3dv) := wglGetProcAddress('glUniformMatrix2x3dv');
    if not Assigned(glUniformMatrix2x3dv) then Exit;
    Pointer(glUniformMatrix2x4dv) := wglGetProcAddress('glUniformMatrix2x4dv');
    if not Assigned(glUniformMatrix2x4dv) then Exit;
    Pointer(glUniformMatrix3x2dv) := wglGetProcAddress('glUniformMatrix3x2dv');
    if not Assigned(glUniformMatrix3x2dv) then Exit;
    Pointer(glUniformMatrix3x4dv) := wglGetProcAddress('glUniformMatrix3x4dv');
    if not Assigned(glUniformMatrix3x4dv) then Exit;
    Pointer(glUniformMatrix4x2dv) := wglGetProcAddress('glUniformMatrix4x2dv');
    if not Assigned(glUniformMatrix4x2dv) then Exit;
    Pointer(glUniformMatrix4x3dv) := wglGetProcAddress('glUniformMatrix4x3dv');
    if not Assigned(glUniformMatrix4x3dv) then Exit;
    Pointer(glGetUniformdv) := wglGetProcAddress('glGetUniformdv');
    if not Assigned(glGetUniformdv) then Exit;
    Pointer(glProgramUniform1dEXT) := wglGetProcAddress('glProgramUniform1dEXT');
    if not Assigned(glProgramUniform1dEXT) then Exit;
    Pointer(glProgramUniform2dEXT) := wglGetProcAddress('glProgramUniform2dEXT');
    if not Assigned(glProgramUniform2dEXT) then Exit;
    Pointer(glProgramUniform3dEXT) := wglGetProcAddress('glProgramUniform3dEXT');
    if not Assigned(glProgramUniform3dEXT) then Exit;
    Pointer(glProgramUniform4dEXT) := wglGetProcAddress('glProgramUniform4dEXT');
    if not Assigned(glProgramUniform4dEXT) then Exit;
    Pointer(glProgramUniform1dvEXT) := wglGetProcAddress('glProgramUniform1dvEXT');
    if not Assigned(glProgramUniform1dvEXT) then Exit;
    Pointer(glProgramUniform2dvEXT) := wglGetProcAddress('glProgramUniform2dvEXT');
    if not Assigned(glProgramUniform2dvEXT) then Exit;
    Pointer(glProgramUniform3dvEXT) := wglGetProcAddress('glProgramUniform3dvEXT');
    if not Assigned(glProgramUniform3dvEXT) then Exit;
    Pointer(glProgramUniform4dvEXT) := wglGetProcAddress('glProgramUniform4dvEXT');
    if not Assigned(glProgramUniform4dvEXT) then Exit;
    Pointer(glProgramUniformMatrix2dvEXT) := wglGetProcAddress('glProgramUniformMatrix2dvEXT');
    if not Assigned(glProgramUniformMatrix2dvEXT) then Exit;
    Pointer(glProgramUniformMatrix3dvEXT) := wglGetProcAddress('glProgramUniformMatrix3dvEXT');
    if not Assigned(glProgramUniformMatrix3dvEXT) then Exit;
    Pointer(glProgramUniformMatrix4dvEXT) := wglGetProcAddress('glProgramUniformMatrix4dvEXT');
    if not Assigned(glProgramUniformMatrix4dvEXT) then Exit;
    Pointer(glProgramUniformMatrix2x3dvEXT) := wglGetProcAddress('glProgramUniformMatrix2x3dvEXT');
    if not Assigned(glProgramUniformMatrix2x3dvEXT) then Exit;
    Pointer(glProgramUniformMatrix2x4dvEXT) := wglGetProcAddress('glProgramUniformMatrix2x4dvEXT');
    if not Assigned(glProgramUniformMatrix2x4dvEXT) then Exit;
    Pointer(glProgramUniformMatrix3x2dvEXT) := wglGetProcAddress('glProgramUniformMatrix3x2dvEXT');
    if not Assigned(glProgramUniformMatrix3x2dvEXT) then Exit;
    Pointer(glProgramUniformMatrix3x4dvEXT) := wglGetProcAddress('glProgramUniformMatrix3x4dvEXT');
    if not Assigned(glProgramUniformMatrix3x4dvEXT) then Exit;
    Pointer(glProgramUniformMatrix4x2dvEXT) := wglGetProcAddress('glProgramUniformMatrix4x2dvEXT');
    if not Assigned(glProgramUniformMatrix4x2dvEXT) then Exit;
    Pointer(glProgramUniformMatrix4x3dvEXT) := wglGetProcAddress('glProgramUniformMatrix4x3dvEXT');
    if not Assigned(glProgramUniformMatrix4x3dvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_shader_subroutine(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shader_subroutine', extstring) then
  begin
    Pointer(glGetSubroutineUniformLocation) := wglGetProcAddress('glGetSubroutineUniformLocation');
    if not Assigned(glGetSubroutineUniformLocation) then Exit;
    Pointer(glGetSubroutineIndex) := wglGetProcAddress('glGetSubroutineIndex');
    if not Assigned(glGetSubroutineIndex) then Exit;
    Pointer(glGetActiveSubroutineUniformiv) := wglGetProcAddress('glGetActiveSubroutineUniformiv');
    if not Assigned(glGetActiveSubroutineUniformiv) then Exit;
    Pointer(glGetActiveSubroutineUniformName) := wglGetProcAddress('glGetActiveSubroutineUniformName');
    if not Assigned(glGetActiveSubroutineUniformName) then Exit;
    Pointer(glGetActiveSubroutineName) := wglGetProcAddress('glGetActiveSubroutineName');
    if not Assigned(glGetActiveSubroutineName) then Exit;
    Pointer(glUniformSubroutinesuiv) := wglGetProcAddress('glUniformSubroutinesuiv');
    if not Assigned(glUniformSubroutinesuiv) then Exit;
    Pointer(glGetUniformSubroutineuiv) := wglGetProcAddress('glGetUniformSubroutineuiv');
    if not Assigned(glGetUniformSubroutineuiv) then Exit;
    Pointer(glGetProgramStageiv) := wglGetProcAddress('glGetProgramStageiv');
    if not Assigned(glGetProgramStageiv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_tessellation_shader(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_tessellation_shader', extstring) then
  begin
    Pointer(glPatchParameteri) := wglGetProcAddress('glPatchParameteri');
    if not Assigned(glPatchParameteri) then Exit;
    Pointer(glPatchParameterfv) := wglGetProcAddress('glPatchParameterfv');
    if not Assigned(glPatchParameterfv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_buffer_object_rgb32(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_buffer_object_rgb32', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ARB_transform_feedback2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_transform_feedback2', extstring) then
  begin
    Pointer(glBindTransformFeedback) := wglGetProcAddress('glBindTransformFeedback');
    if not Assigned(glBindTransformFeedback) then Exit;
    Pointer(glDeleteTransformFeedbacks) := wglGetProcAddress('glDeleteTransformFeedbacks');
    if not Assigned(glDeleteTransformFeedbacks) then Exit;
    Pointer(glGenTransformFeedbacks) := wglGetProcAddress('glGenTransformFeedbacks');
    if not Assigned(glGenTransformFeedbacks) then Exit;
    Pointer(glIsTransformFeedback) := wglGetProcAddress('glIsTransformFeedback');
    if not Assigned(glIsTransformFeedback) then Exit;
    Pointer(glPauseTransformFeedback) := wglGetProcAddress('glPauseTransformFeedback');
    if not Assigned(glPauseTransformFeedback) then Exit;
    Pointer(glResumeTransformFeedback) := wglGetProcAddress('glResumeTransformFeedback');
    if not Assigned(glResumeTransformFeedback) then Exit;
    Pointer(glDrawTransformFeedback) := wglGetProcAddress('glDrawTransformFeedback');
    if not Assigned(glDrawTransformFeedback) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_transform_feedback3(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_transform_feedback3', extstring) then
  begin
    Pointer(glDrawTransformFeedbackStream) := wglGetProcAddress('glDrawTransformFeedbackStream');
    if not Assigned(glDrawTransformFeedbackStream) then Exit;
    Pointer(glBeginQueryIndexed) := wglGetProcAddress('glBeginQueryIndexed');
    if not Assigned(glBeginQueryIndexed) then Exit;
    Pointer(glEndQueryIndexed) := wglGetProcAddress('glEndQueryIndexed');
    if not Assigned(glEndQueryIndexed) then Exit;
    Pointer(glGetQueryIndexediv) := wglGetProcAddress('glGetQueryIndexediv');
    if not Assigned(glGetQueryIndexediv) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_abgr(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_abgr', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_blend_color(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_color', extstring) then
  begin
    Pointer(glBlendColorEXT) := wglGetProcAddress('glBlendColorEXT');
    if not Assigned(glBlendColorEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_polygon_offset(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_polygon_offset', extstring) then
  begin
    Pointer(glPolygonOffsetEXT) := wglGetProcAddress('glPolygonOffsetEXT');
    if not Assigned(glPolygonOffsetEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture3D(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture3D', extstring) then
  begin
    Pointer(glTexImage3DEXT) := wglGetProcAddress('glTexImage3DEXT');
    if not Assigned(glTexImage3DEXT) then Exit;
    Pointer(glTexSubImage3DEXT) := wglGetProcAddress('glTexSubImage3DEXT');
    if not Assigned(glTexSubImage3DEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIS_texture_filter4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_filter4', extstring) then
  begin
    Pointer(glGetTexFilterFuncSGIS) := wglGetProcAddress('glGetTexFilterFuncSGIS');
    if not Assigned(glGetTexFilterFuncSGIS) then Exit;
    Pointer(glTexFilterFuncSGIS) := wglGetProcAddress('glTexFilterFuncSGIS');
    if not Assigned(glTexFilterFuncSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_subtexture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_subtexture', extstring) then
  begin
    Pointer(glTexSubImage1DEXT) := wglGetProcAddress('glTexSubImage1DEXT');
    if not Assigned(glTexSubImage1DEXT) then Exit;
    Pointer(glTexSubImage2DEXT) := wglGetProcAddress('glTexSubImage2DEXT');
    if not Assigned(glTexSubImage2DEXT) then Exit;
    (* Shared entry points *)
    Pointer(glTexSubImage3DEXT) := wglGetProcAddress('glTexSubImage3DEXT');
    if not Assigned(glTexSubImage3DEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_copy_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_copy_texture', extstring) then
  begin
    Pointer(glCopyTexImage1DEXT) := wglGetProcAddress('glCopyTexImage1DEXT');
    if not Assigned(glCopyTexImage1DEXT) then Exit;
    Pointer(glCopyTexImage2DEXT) := wglGetProcAddress('glCopyTexImage2DEXT');
    if not Assigned(glCopyTexImage2DEXT) then Exit;
    Pointer(glCopyTexSubImage1DEXT) := wglGetProcAddress('glCopyTexSubImage1DEXT');
    if not Assigned(glCopyTexSubImage1DEXT) then Exit;
    Pointer(glCopyTexSubImage2DEXT) := wglGetProcAddress('glCopyTexSubImage2DEXT');
    if not Assigned(glCopyTexSubImage2DEXT) then Exit;
    Pointer(glCopyTexSubImage3DEXT) := wglGetProcAddress('glCopyTexSubImage3DEXT');
    if not Assigned(glCopyTexSubImage3DEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_histogram(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_histogram', extstring) then
  begin
    Pointer(glGetHistogramEXT) := wglGetProcAddress('glGetHistogramEXT');
    if not Assigned(glGetHistogramEXT) then Exit;
    Pointer(glGetHistogramParameterfvEXT) := wglGetProcAddress('glGetHistogramParameterfvEXT');
    if not Assigned(glGetHistogramParameterfvEXT) then Exit;
    Pointer(glGetHistogramParameterivEXT) := wglGetProcAddress('glGetHistogramParameterivEXT');
    if not Assigned(glGetHistogramParameterivEXT) then Exit;
    Pointer(glGetMinmaxEXT) := wglGetProcAddress('glGetMinmaxEXT');
    if not Assigned(glGetMinmaxEXT) then Exit;
    Pointer(glGetMinmaxParameterfvEXT) := wglGetProcAddress('glGetMinmaxParameterfvEXT');
    if not Assigned(glGetMinmaxParameterfvEXT) then Exit;
    Pointer(glGetMinmaxParameterivEXT) := wglGetProcAddress('glGetMinmaxParameterivEXT');
    if not Assigned(glGetMinmaxParameterivEXT) then Exit;
    Pointer(glHistogramEXT) := wglGetProcAddress('glHistogramEXT');
    if not Assigned(glHistogramEXT) then Exit;
    Pointer(glMinmaxEXT) := wglGetProcAddress('glMinmaxEXT');
    if not Assigned(glMinmaxEXT) then Exit;
    Pointer(glResetHistogramEXT) := wglGetProcAddress('glResetHistogramEXT');
    if not Assigned(glResetHistogramEXT) then Exit;
    Pointer(glResetMinmaxEXT) := wglGetProcAddress('glResetMinmaxEXT');
    if not Assigned(glResetMinmaxEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_convolution(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_convolution', extstring) then
  begin
    Pointer(glConvolutionFilter1DEXT) := wglGetProcAddress('glConvolutionFilter1DEXT');
    if not Assigned(glConvolutionFilter1DEXT) then Exit;
    Pointer(glConvolutionFilter2DEXT) := wglGetProcAddress('glConvolutionFilter2DEXT');
    if not Assigned(glConvolutionFilter2DEXT) then Exit;
    Pointer(glConvolutionParameterfEXT) := wglGetProcAddress('glConvolutionParameterfEXT');
    if not Assigned(glConvolutionParameterfEXT) then Exit;
    Pointer(glConvolutionParameterfvEXT) := wglGetProcAddress('glConvolutionParameterfvEXT');
    if not Assigned(glConvolutionParameterfvEXT) then Exit;
    Pointer(glConvolutionParameteriEXT) := wglGetProcAddress('glConvolutionParameteriEXT');
    if not Assigned(glConvolutionParameteriEXT) then Exit;
    Pointer(glConvolutionParameterivEXT) := wglGetProcAddress('glConvolutionParameterivEXT');
    if not Assigned(glConvolutionParameterivEXT) then Exit;
    Pointer(glCopyConvolutionFilter1DEXT) := wglGetProcAddress('glCopyConvolutionFilter1DEXT');
    if not Assigned(glCopyConvolutionFilter1DEXT) then Exit;
    Pointer(glCopyConvolutionFilter2DEXT) := wglGetProcAddress('glCopyConvolutionFilter2DEXT');
    if not Assigned(glCopyConvolutionFilter2DEXT) then Exit;
    Pointer(glGetConvolutionFilterEXT) := wglGetProcAddress('glGetConvolutionFilterEXT');
    if not Assigned(glGetConvolutionFilterEXT) then Exit;
    Pointer(glGetConvolutionParameterfvEXT) := wglGetProcAddress('glGetConvolutionParameterfvEXT');
    if not Assigned(glGetConvolutionParameterfvEXT) then Exit;
    Pointer(glGetConvolutionParameterivEXT) := wglGetProcAddress('glGetConvolutionParameterivEXT');
    if not Assigned(glGetConvolutionParameterivEXT) then Exit;
    Pointer(glGetSeparableFilterEXT) := wglGetProcAddress('glGetSeparableFilterEXT');
    if not Assigned(glGetSeparableFilterEXT) then Exit;
    Pointer(glSeparableFilter2DEXT) := wglGetProcAddress('glSeparableFilter2DEXT');
    if not Assigned(glSeparableFilter2DEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGI_color_matrix(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGI_color_matrix', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGI_color_table(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGI_color_table', extstring) then
  begin
    Pointer(glColorTableSGI) := wglGetProcAddress('glColorTableSGI');
    if not Assigned(glColorTableSGI) then Exit;
    Pointer(glColorTableParameterfvSGI) := wglGetProcAddress('glColorTableParameterfvSGI');
    if not Assigned(glColorTableParameterfvSGI) then Exit;
    Pointer(glColorTableParameterivSGI) := wglGetProcAddress('glColorTableParameterivSGI');
    if not Assigned(glColorTableParameterivSGI) then Exit;
    Pointer(glCopyColorTableSGI) := wglGetProcAddress('glCopyColorTableSGI');
    if not Assigned(glCopyColorTableSGI) then Exit;
    Pointer(glGetColorTableSGI) := wglGetProcAddress('glGetColorTableSGI');
    if not Assigned(glGetColorTableSGI) then Exit;
    Pointer(glGetColorTableParameterfvSGI) := wglGetProcAddress('glGetColorTableParameterfvSGI');
    if not Assigned(glGetColorTableParameterfvSGI) then Exit;
    Pointer(glGetColorTableParameterivSGI) := wglGetProcAddress('glGetColorTableParameterivSGI');
    if not Assigned(glGetColorTableParameterivSGI) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIS_pixel_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_pixel_texture', extstring) then
  begin
    Pointer(glPixelTexGenParameteriSGIS) := wglGetProcAddress('glPixelTexGenParameteriSGIS');
    if not Assigned(glPixelTexGenParameteriSGIS) then Exit;
    Pointer(glPixelTexGenParameterivSGIS) := wglGetProcAddress('glPixelTexGenParameterivSGIS');
    if not Assigned(glPixelTexGenParameterivSGIS) then Exit;
    Pointer(glPixelTexGenParameterfSGIS) := wglGetProcAddress('glPixelTexGenParameterfSGIS');
    if not Assigned(glPixelTexGenParameterfSGIS) then Exit;
    Pointer(glPixelTexGenParameterfvSGIS) := wglGetProcAddress('glPixelTexGenParameterfvSGIS');
    if not Assigned(glPixelTexGenParameterfvSGIS) then Exit;
    Pointer(glGetPixelTexGenParameterivSGIS) := wglGetProcAddress('glGetPixelTexGenParameterivSGIS');
    if not Assigned(glGetPixelTexGenParameterivSGIS) then Exit;
    Pointer(glGetPixelTexGenParameterfvSGIS) := wglGetProcAddress('glGetPixelTexGenParameterfvSGIS');
    if not Assigned(glGetPixelTexGenParameterfvSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_pixel_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_pixel_texture', extstring) then
  begin
    Pointer(glPixelTexGenSGIX) := wglGetProcAddress('glPixelTexGenSGIX');
    if not Assigned(glPixelTexGenSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIS_texture4D(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture4D', extstring) then
  begin
    Pointer(glTexImage4DSGIS) := wglGetProcAddress('glTexImage4DSGIS');
    if not Assigned(glTexImage4DSGIS) then Exit;
    Pointer(glTexSubImage4DSGIS) := wglGetProcAddress('glTexSubImage4DSGIS');
    if not Assigned(glTexSubImage4DSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGI_texture_color_table(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGI_texture_color_table', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_cmyka(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_cmyka', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_object', extstring) then
  begin
    Pointer(glAreTexturesResidentEXT) := wglGetProcAddress('glAreTexturesResidentEXT');
    if not Assigned(glAreTexturesResidentEXT) then Exit;
    Pointer(glBindTextureEXT) := wglGetProcAddress('glBindTextureEXT');
    if not Assigned(glBindTextureEXT) then Exit;
    Pointer(glDeleteTexturesEXT) := wglGetProcAddress('glDeleteTexturesEXT');
    if not Assigned(glDeleteTexturesEXT) then Exit;
    Pointer(glGenTexturesEXT) := wglGetProcAddress('glGenTexturesEXT');
    if not Assigned(glGenTexturesEXT) then Exit;
    Pointer(glIsTextureEXT) := wglGetProcAddress('glIsTextureEXT');
    if not Assigned(glIsTextureEXT) then Exit;
    Pointer(glPrioritizeTexturesEXT) := wglGetProcAddress('glPrioritizeTexturesEXT');
    if not Assigned(glPrioritizeTexturesEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIS_detail_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_detail_texture', extstring) then
  begin
    Pointer(glDetailTexFuncSGIS) := wglGetProcAddress('glDetailTexFuncSGIS');
    if not Assigned(glDetailTexFuncSGIS) then Exit;
    Pointer(glGetDetailTexFuncSGIS) := wglGetProcAddress('glGetDetailTexFuncSGIS');
    if not Assigned(glGetDetailTexFuncSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIS_sharpen_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_sharpen_texture', extstring) then
  begin
    Pointer(glSharpenTexFuncSGIS) := wglGetProcAddress('glSharpenTexFuncSGIS');
    if not Assigned(glSharpenTexFuncSGIS) then Exit;
    Pointer(glGetSharpenTexFuncSGIS) := wglGetProcAddress('glGetSharpenTexFuncSGIS');
    if not Assigned(glGetSharpenTexFuncSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_packed_pixels(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_packed_pixels', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_texture_lod(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_lod', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_multisample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_multisample', extstring) then
  begin
    Pointer(glSampleMaskSGIS) := wglGetProcAddress('glSampleMaskSGIS');
    if not Assigned(glSampleMaskSGIS) then Exit;
    Pointer(glSamplePatternSGIS) := wglGetProcAddress('glSamplePatternSGIS');
    if not Assigned(glSamplePatternSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_rescale_normal(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_rescale_normal', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_vertex_array(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_vertex_array', extstring) then
  begin
    Pointer(glArrayElementEXT) := wglGetProcAddress('glArrayElementEXT');
    if not Assigned(glArrayElementEXT) then Exit;
    Pointer(glColorPointerEXT) := wglGetProcAddress('glColorPointerEXT');
    if not Assigned(glColorPointerEXT) then Exit;
    Pointer(glDrawArraysEXT) := wglGetProcAddress('glDrawArraysEXT');
    if not Assigned(glDrawArraysEXT) then Exit;
    Pointer(glEdgeFlagPointerEXT) := wglGetProcAddress('glEdgeFlagPointerEXT');
    if not Assigned(glEdgeFlagPointerEXT) then Exit;
    Pointer(glGetPointervEXT) := wglGetProcAddress('glGetPointervEXT');
    if not Assigned(glGetPointervEXT) then Exit;
    Pointer(glIndexPointerEXT) := wglGetProcAddress('glIndexPointerEXT');
    if not Assigned(glIndexPointerEXT) then Exit;
    Pointer(glNormalPointerEXT) := wglGetProcAddress('glNormalPointerEXT');
    if not Assigned(glNormalPointerEXT) then Exit;
    Pointer(glTexCoordPointerEXT) := wglGetProcAddress('glTexCoordPointerEXT');
    if not Assigned(glTexCoordPointerEXT) then Exit;
    Pointer(glVertexPointerEXT) := wglGetProcAddress('glVertexPointerEXT');
    if not Assigned(glVertexPointerEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_misc_attribute(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_misc_attribute', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_generate_mipmap(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_generate_mipmap', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_clipmap(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_clipmap', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_shadow(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_shadow', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_texture_edge_clamp(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_edge_clamp', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_texture_border_clamp(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_border_clamp', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_blend_minmax(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_minmax', extstring) then
  begin
    Pointer(glBlendEquationEXT) := wglGetProcAddress('glBlendEquationEXT');
    if not Assigned(glBlendEquationEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_blend_subtract(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_subtract', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_blend_logic_op(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_logic_op', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_interlace(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_interlace', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_pixel_tiles(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_pixel_tiles', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_texture_select(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_select', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_sprite(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_sprite', extstring) then
  begin
    Pointer(glSpriteParameterfSGIX) := wglGetProcAddress('glSpriteParameterfSGIX');
    if not Assigned(glSpriteParameterfSGIX) then Exit;
    Pointer(glSpriteParameterfvSGIX) := wglGetProcAddress('glSpriteParameterfvSGIX');
    if not Assigned(glSpriteParameterfvSGIX) then Exit;
    Pointer(glSpriteParameteriSGIX) := wglGetProcAddress('glSpriteParameteriSGIX');
    if not Assigned(glSpriteParameteriSGIX) then Exit;
    Pointer(glSpriteParameterivSGIX) := wglGetProcAddress('glSpriteParameterivSGIX');
    if not Assigned(glSpriteParameterivSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_texture_multi_buffer(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_texture_multi_buffer', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_point_parameters(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_point_parameters', extstring) then
  begin
    Pointer(glPointParameterfEXT) := wglGetProcAddress('glPointParameterfEXT');
    if not Assigned(glPointParameterfEXT) then Exit;
    Pointer(glPointParameterfvEXT) := wglGetProcAddress('glPointParameterfvEXT');
    if not Assigned(glPointParameterfvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIS_point_parameters(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_point_parameters', extstring) then
  begin
    Pointer(glPointParameterfSGIS) := wglGetProcAddress('glPointParameterfSGIS');
    if not Assigned(glPointParameterfSGIS) then Exit;
    Pointer(glPointParameterfvSGIS) := wglGetProcAddress('glPointParameterfvSGIS');
    if not Assigned(glPointParameterfvSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_instruments(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_instruments', extstring) then
  begin
    Pointer(glGetInstrumentsSGIX) := wglGetProcAddress('glGetInstrumentsSGIX');
    if not Assigned(glGetInstrumentsSGIX) then Exit;
    Pointer(glInstrumentsBufferSGIX) := wglGetProcAddress('glInstrumentsBufferSGIX');
    if not Assigned(glInstrumentsBufferSGIX) then Exit;
    Pointer(glPollInstrumentsSGIX) := wglGetProcAddress('glPollInstrumentsSGIX');
    if not Assigned(glPollInstrumentsSGIX) then Exit;
    Pointer(glReadInstrumentsSGIX) := wglGetProcAddress('glReadInstrumentsSGIX');
    if not Assigned(glReadInstrumentsSGIX) then Exit;
    Pointer(glStartInstrumentsSGIX) := wglGetProcAddress('glStartInstrumentsSGIX');
    if not Assigned(glStartInstrumentsSGIX) then Exit;
    Pointer(glStopInstrumentsSGIX) := wglGetProcAddress('glStopInstrumentsSGIX');
    if not Assigned(glStopInstrumentsSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_texture_scale_bias(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_texture_scale_bias', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_framezoom(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_framezoom', extstring) then
  begin
    Pointer(glFrameZoomSGIX) := wglGetProcAddress('glFrameZoomSGIX');
    if not Assigned(glFrameZoomSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_tag_sample_buffer(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_tag_sample_buffer', extstring) then
  begin
    Pointer(glTagSampleBufferSGIX) := wglGetProcAddress('glTagSampleBufferSGIX');
    if not Assigned(glTagSampleBufferSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_FfdMaskSGIX(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_FfdMaskSGIX', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_polynomial_ffd(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_polynomial_ffd', extstring) then
  begin
    Pointer(glDeformationMap3dSGIX) := wglGetProcAddress('glDeformationMap3dSGIX');
    if not Assigned(glDeformationMap3dSGIX) then Exit;
    Pointer(glDeformationMap3fSGIX) := wglGetProcAddress('glDeformationMap3fSGIX');
    if not Assigned(glDeformationMap3fSGIX) then Exit;
    Pointer(glDeformSGIX) := wglGetProcAddress('glDeformSGIX');
    if not Assigned(glDeformSGIX) then Exit;
    Pointer(glLoadIdentityDeformationMapSGIX) := wglGetProcAddress('glLoadIdentityDeformationMapSGIX');
    if not Assigned(glLoadIdentityDeformationMapSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_reference_plane(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_reference_plane', extstring) then
  begin
    Pointer(glReferencePlaneSGIX) := wglGetProcAddress('glReferencePlaneSGIX');
    if not Assigned(glReferencePlaneSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_flush_raster(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_flush_raster', extstring) then
  begin
    Pointer(glFlushRasterSGIX) := wglGetProcAddress('glFlushRasterSGIX');
    if not Assigned(glFlushRasterSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_depth_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_depth_texture', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_fog_function(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_fog_function', extstring) then
  begin
    Pointer(glFogFuncSGIS) := wglGetProcAddress('glFogFuncSGIS');
    if not Assigned(glFogFuncSGIS) then Exit;
    Pointer(glGetFogFuncSGIS) := wglGetProcAddress('glGetFogFuncSGIS');
    if not Assigned(glGetFogFuncSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_fog_offset(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_fog_offset', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_HP_image_transform(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_HP_image_transform', extstring) then
  begin
    Pointer(glImageTransformParameteriHP) := wglGetProcAddress('glImageTransformParameteriHP');
    if not Assigned(glImageTransformParameteriHP) then Exit;
    Pointer(glImageTransformParameterfHP) := wglGetProcAddress('glImageTransformParameterfHP');
    if not Assigned(glImageTransformParameterfHP) then Exit;
    Pointer(glImageTransformParameterivHP) := wglGetProcAddress('glImageTransformParameterivHP');
    if not Assigned(glImageTransformParameterivHP) then Exit;
    Pointer(glImageTransformParameterfvHP) := wglGetProcAddress('glImageTransformParameterfvHP');
    if not Assigned(glImageTransformParameterfvHP) then Exit;
    Pointer(glGetImageTransformParameterivHP) := wglGetProcAddress('glGetImageTransformParameterivHP');
    if not Assigned(glGetImageTransformParameterivHP) then Exit;
    Pointer(glGetImageTransformParameterfvHP) := wglGetProcAddress('glGetImageTransformParameterfvHP');
    if not Assigned(glGetImageTransformParameterfvHP) then Exit;
    Result := True;
  end;
end;

function Load_GL_HP_convolution_border_modes(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_HP_convolution_border_modes', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_INGR_palette_buffer(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_INGR_palette_buffer', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_texture_add_env(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_texture_add_env', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_color_subtable(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_color_subtable', extstring) then
  begin
    Pointer(glColorSubTableEXT) := wglGetProcAddress('glColorSubTableEXT');
    if not Assigned(glColorSubTableEXT) then Exit;
    Pointer(glCopyColorSubTableEXT) := wglGetProcAddress('glCopyColorSubTableEXT');
    if not Assigned(glCopyColorSubTableEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_PGI_vertex_hints(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_PGI_vertex_hints', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_PGI_misc_hints(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_PGI_misc_hints', extstring) then
  begin
    Pointer(glHintPGI) := wglGetProcAddress('glHintPGI');
    if not Assigned(glHintPGI) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_paletted_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_paletted_texture', extstring) then
  begin
    Pointer(glColorTableEXT) := wglGetProcAddress('glColorTableEXT');
    if not Assigned(glColorTableEXT) then Exit;
    Pointer(glGetColorTableEXT) := wglGetProcAddress('glGetColorTableEXT');
    if not Assigned(glGetColorTableEXT) then Exit;
    Pointer(glGetColorTableParameterivEXT) := wglGetProcAddress('glGetColorTableParameterivEXT');
    if not Assigned(glGetColorTableParameterivEXT) then Exit;
    Pointer(glGetColorTableParameterfvEXT) := wglGetProcAddress('glGetColorTableParameterfvEXT');
    if not Assigned(glGetColorTableParameterfvEXT) then Exit;
    (* Shared entry points *)
    Pointer(glColorSubTableEXT) := wglGetProcAddress('glColorSubTableEXT');
    if not Assigned(glColorSubTableEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_clip_volume_hint(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_clip_volume_hint', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_list_priority(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_list_priority', extstring) then
  begin
    Pointer(glGetListParameterfvSGIX) := wglGetProcAddress('glGetListParameterfvSGIX');
    if not Assigned(glGetListParameterfvSGIX) then Exit;
    Pointer(glGetListParameterivSGIX) := wglGetProcAddress('glGetListParameterivSGIX');
    if not Assigned(glGetListParameterivSGIX) then Exit;
    Pointer(glListParameterfSGIX) := wglGetProcAddress('glListParameterfSGIX');
    if not Assigned(glListParameterfSGIX) then Exit;
    Pointer(glListParameterfvSGIX) := wglGetProcAddress('glListParameterfvSGIX');
    if not Assigned(glListParameterfvSGIX) then Exit;
    Pointer(glListParameteriSGIX) := wglGetProcAddress('glListParameteriSGIX');
    if not Assigned(glListParameteriSGIX) then Exit;
    Pointer(glListParameterivSGIX) := wglGetProcAddress('glListParameterivSGIX');
    if not Assigned(glListParameterivSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_ir_instrument1(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_ir_instrument1', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_calligraphic_fragment(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_calligraphic_fragment', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_texture_lod_bias(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_texture_lod_bias', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_shadow_ambient(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_shadow_ambient', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_index_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_index_texture', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_index_material(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_index_material', extstring) then
  begin
    Pointer(glIndexMaterialEXT) := wglGetProcAddress('glIndexMaterialEXT');
    if not Assigned(glIndexMaterialEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_index_func(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_index_func', extstring) then
  begin
    Pointer(glIndexFuncEXT) := wglGetProcAddress('glIndexFuncEXT');
    if not Assigned(glIndexFuncEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_index_array_formats(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_index_array_formats', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_compiled_vertex_array(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_compiled_vertex_array', extstring) then
  begin
    Pointer(glLockArraysEXT) := wglGetProcAddress('glLockArraysEXT');
    if not Assigned(glLockArraysEXT) then Exit;
    Pointer(glUnlockArraysEXT) := wglGetProcAddress('glUnlockArraysEXT');
    if not Assigned(glUnlockArraysEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_cull_vertex(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_cull_vertex', extstring) then
  begin
    Pointer(glCullParameterdvEXT) := wglGetProcAddress('glCullParameterdvEXT');
    if not Assigned(glCullParameterdvEXT) then Exit;
    Pointer(glCullParameterfvEXT) := wglGetProcAddress('glCullParameterfvEXT');
    if not Assigned(glCullParameterfvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_ycrcb(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_ycrcb', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_fragment_lighting(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_fragment_lighting', extstring) then
  begin
    Pointer(glFragmentColorMaterialSGIX) := wglGetProcAddress('glFragmentColorMaterialSGIX');
    if not Assigned(glFragmentColorMaterialSGIX) then Exit;
    Pointer(glFragmentLightfSGIX) := wglGetProcAddress('glFragmentLightfSGIX');
    if not Assigned(glFragmentLightfSGIX) then Exit;
    Pointer(glFragmentLightfvSGIX) := wglGetProcAddress('glFragmentLightfvSGIX');
    if not Assigned(glFragmentLightfvSGIX) then Exit;
    Pointer(glFragmentLightiSGIX) := wglGetProcAddress('glFragmentLightiSGIX');
    if not Assigned(glFragmentLightiSGIX) then Exit;
    Pointer(glFragmentLightivSGIX) := wglGetProcAddress('glFragmentLightivSGIX');
    if not Assigned(glFragmentLightivSGIX) then Exit;
    Pointer(glFragmentLightModelfSGIX) := wglGetProcAddress('glFragmentLightModelfSGIX');
    if not Assigned(glFragmentLightModelfSGIX) then Exit;
    Pointer(glFragmentLightModelfvSGIX) := wglGetProcAddress('glFragmentLightModelfvSGIX');
    if not Assigned(glFragmentLightModelfvSGIX) then Exit;
    Pointer(glFragmentLightModeliSGIX) := wglGetProcAddress('glFragmentLightModeliSGIX');
    if not Assigned(glFragmentLightModeliSGIX) then Exit;
    Pointer(glFragmentLightModelivSGIX) := wglGetProcAddress('glFragmentLightModelivSGIX');
    if not Assigned(glFragmentLightModelivSGIX) then Exit;
    Pointer(glFragmentMaterialfSGIX) := wglGetProcAddress('glFragmentMaterialfSGIX');
    if not Assigned(glFragmentMaterialfSGIX) then Exit;
    Pointer(glFragmentMaterialfvSGIX) := wglGetProcAddress('glFragmentMaterialfvSGIX');
    if not Assigned(glFragmentMaterialfvSGIX) then Exit;
    Pointer(glFragmentMaterialiSGIX) := wglGetProcAddress('glFragmentMaterialiSGIX');
    if not Assigned(glFragmentMaterialiSGIX) then Exit;
    Pointer(glFragmentMaterialivSGIX) := wglGetProcAddress('glFragmentMaterialivSGIX');
    if not Assigned(glFragmentMaterialivSGIX) then Exit;
    Pointer(glGetFragmentLightfvSGIX) := wglGetProcAddress('glGetFragmentLightfvSGIX');
    if not Assigned(glGetFragmentLightfvSGIX) then Exit;
    Pointer(glGetFragmentLightivSGIX) := wglGetProcAddress('glGetFragmentLightivSGIX');
    if not Assigned(glGetFragmentLightivSGIX) then Exit;
    Pointer(glGetFragmentMaterialfvSGIX) := wglGetProcAddress('glGetFragmentMaterialfvSGIX');
    if not Assigned(glGetFragmentMaterialfvSGIX) then Exit;
    Pointer(glGetFragmentMaterialivSGIX) := wglGetProcAddress('glGetFragmentMaterialivSGIX');
    if not Assigned(glGetFragmentMaterialivSGIX) then Exit;
    Pointer(glLightEnviSGIX) := wglGetProcAddress('glLightEnviSGIX');
    if not Assigned(glLightEnviSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_IBM_rasterpos_clip(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_rasterpos_clip', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_HP_texture_lighting(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_HP_texture_lighting', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_draw_range_elements(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_draw_range_elements', extstring) then
  begin
    Pointer(glDrawRangeElementsEXT) := wglGetProcAddress('glDrawRangeElementsEXT');
    if not Assigned(glDrawRangeElementsEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_WIN_phong_shading(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_WIN_phong_shading', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_WIN_specular_fog(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_WIN_specular_fog', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_light_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_light_texture', extstring) then
  begin
    Pointer(glApplyTextureEXT) := wglGetProcAddress('glApplyTextureEXT');
    if not Assigned(glApplyTextureEXT) then Exit;
    Pointer(glTextureLightEXT) := wglGetProcAddress('glTextureLightEXT');
    if not Assigned(glTextureLightEXT) then Exit;
    Pointer(glTextureMaterialEXT) := wglGetProcAddress('glTextureMaterialEXT');
    if not Assigned(glTextureMaterialEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_blend_alpha_minmax(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_blend_alpha_minmax', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_impact_pixel_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_impact_pixel_texture', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_bgra(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_bgra', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_async(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_async', extstring) then
  begin
    Pointer(glAsyncMarkerSGIX) := wglGetProcAddress('glAsyncMarkerSGIX');
    if not Assigned(glAsyncMarkerSGIX) then Exit;
    Pointer(glFinishAsyncSGIX) := wglGetProcAddress('glFinishAsyncSGIX');
    if not Assigned(glFinishAsyncSGIX) then Exit;
    Pointer(glPollAsyncSGIX) := wglGetProcAddress('glPollAsyncSGIX');
    if not Assigned(glPollAsyncSGIX) then Exit;
    Pointer(glGenAsyncMarkersSGIX) := wglGetProcAddress('glGenAsyncMarkersSGIX');
    if not Assigned(glGenAsyncMarkersSGIX) then Exit;
    Pointer(glDeleteAsyncMarkersSGIX) := wglGetProcAddress('glDeleteAsyncMarkersSGIX');
    if not Assigned(glDeleteAsyncMarkersSGIX) then Exit;
    Pointer(glIsAsyncMarkerSGIX) := wglGetProcAddress('glIsAsyncMarkerSGIX');
    if not Assigned(glIsAsyncMarkerSGIX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_async_pixel(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_async_pixel', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_async_histogram(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_async_histogram', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_INTEL_texture_scissor(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_INTEL_texture_scissor', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_INTEL_parallel_arrays(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_INTEL_parallel_arrays', extstring) then
  begin
    Pointer(glVertexPointervINTEL) := wglGetProcAddress('glVertexPointervINTEL');
    if not Assigned(glVertexPointervINTEL) then Exit;
    Pointer(glNormalPointervINTEL) := wglGetProcAddress('glNormalPointervINTEL');
    if not Assigned(glNormalPointervINTEL) then Exit;
    Pointer(glColorPointervINTEL) := wglGetProcAddress('glColorPointervINTEL');
    if not Assigned(glColorPointervINTEL) then Exit;
    Pointer(glTexCoordPointervINTEL) := wglGetProcAddress('glTexCoordPointervINTEL');
    if not Assigned(glTexCoordPointervINTEL) then Exit;
    Result := True;
  end;
end;

function Load_GL_HP_occlusion_test(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_HP_occlusion_test', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_pixel_transform(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_pixel_transform', extstring) then
  begin
    Pointer(glPixelTransformParameteriEXT) := wglGetProcAddress('glPixelTransformParameteriEXT');
    if not Assigned(glPixelTransformParameteriEXT) then Exit;
    Pointer(glPixelTransformParameterfEXT) := wglGetProcAddress('glPixelTransformParameterfEXT');
    if not Assigned(glPixelTransformParameterfEXT) then Exit;
    Pointer(glPixelTransformParameterivEXT) := wglGetProcAddress('glPixelTransformParameterivEXT');
    if not Assigned(glPixelTransformParameterivEXT) then Exit;
    Pointer(glPixelTransformParameterfvEXT) := wglGetProcAddress('glPixelTransformParameterfvEXT');
    if not Assigned(glPixelTransformParameterfvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_pixel_transform_color_table(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_pixel_transform_color_table', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_shared_texture_palette(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_shared_texture_palette', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_separate_specular_color(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_separate_specular_color', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_secondary_color(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_secondary_color', extstring) then
  begin
    Pointer(glSecondaryColor3bEXT) := wglGetProcAddress('glSecondaryColor3bEXT');
    if not Assigned(glSecondaryColor3bEXT) then Exit;
    Pointer(glSecondaryColor3bvEXT) := wglGetProcAddress('glSecondaryColor3bvEXT');
    if not Assigned(glSecondaryColor3bvEXT) then Exit;
    Pointer(glSecondaryColor3dEXT) := wglGetProcAddress('glSecondaryColor3dEXT');
    if not Assigned(glSecondaryColor3dEXT) then Exit;
    Pointer(glSecondaryColor3dvEXT) := wglGetProcAddress('glSecondaryColor3dvEXT');
    if not Assigned(glSecondaryColor3dvEXT) then Exit;
    Pointer(glSecondaryColor3fEXT) := wglGetProcAddress('glSecondaryColor3fEXT');
    if not Assigned(glSecondaryColor3fEXT) then Exit;
    Pointer(glSecondaryColor3fvEXT) := wglGetProcAddress('glSecondaryColor3fvEXT');
    if not Assigned(glSecondaryColor3fvEXT) then Exit;
    Pointer(glSecondaryColor3iEXT) := wglGetProcAddress('glSecondaryColor3iEXT');
    if not Assigned(glSecondaryColor3iEXT) then Exit;
    Pointer(glSecondaryColor3ivEXT) := wglGetProcAddress('glSecondaryColor3ivEXT');
    if not Assigned(glSecondaryColor3ivEXT) then Exit;
    Pointer(glSecondaryColor3sEXT) := wglGetProcAddress('glSecondaryColor3sEXT');
    if not Assigned(glSecondaryColor3sEXT) then Exit;
    Pointer(glSecondaryColor3svEXT) := wglGetProcAddress('glSecondaryColor3svEXT');
    if not Assigned(glSecondaryColor3svEXT) then Exit;
    Pointer(glSecondaryColor3ubEXT) := wglGetProcAddress('glSecondaryColor3ubEXT');
    if not Assigned(glSecondaryColor3ubEXT) then Exit;
    Pointer(glSecondaryColor3ubvEXT) := wglGetProcAddress('glSecondaryColor3ubvEXT');
    if not Assigned(glSecondaryColor3ubvEXT) then Exit;
    Pointer(glSecondaryColor3uiEXT) := wglGetProcAddress('glSecondaryColor3uiEXT');
    if not Assigned(glSecondaryColor3uiEXT) then Exit;
    Pointer(glSecondaryColor3uivEXT) := wglGetProcAddress('glSecondaryColor3uivEXT');
    if not Assigned(glSecondaryColor3uivEXT) then Exit;
    Pointer(glSecondaryColor3usEXT) := wglGetProcAddress('glSecondaryColor3usEXT');
    if not Assigned(glSecondaryColor3usEXT) then Exit;
    Pointer(glSecondaryColor3usvEXT) := wglGetProcAddress('glSecondaryColor3usvEXT');
    if not Assigned(glSecondaryColor3usvEXT) then Exit;
    Pointer(glSecondaryColorPointerEXT) := wglGetProcAddress('glSecondaryColorPointerEXT');
    if not Assigned(glSecondaryColorPointerEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_perturb_normal(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_perturb_normal', extstring) then
  begin
    Pointer(glTextureNormalEXT) := wglGetProcAddress('glTextureNormalEXT');
    if not Assigned(glTextureNormalEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_multi_draw_arrays(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_multi_draw_arrays', extstring) then
  begin
    Pointer(glMultiDrawArraysEXT) := wglGetProcAddress('glMultiDrawArraysEXT');
    if not Assigned(glMultiDrawArraysEXT) then Exit;
    Pointer(glMultiDrawElementsEXT) := wglGetProcAddress('glMultiDrawElementsEXT');
    if not Assigned(glMultiDrawElementsEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_fog_coord(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_fog_coord', extstring) then
  begin
    Pointer(glFogCoordfEXT) := wglGetProcAddress('glFogCoordfEXT');
    if not Assigned(glFogCoordfEXT) then Exit;
    Pointer(glFogCoordfvEXT) := wglGetProcAddress('glFogCoordfvEXT');
    if not Assigned(glFogCoordfvEXT) then Exit;
    Pointer(glFogCoorddEXT) := wglGetProcAddress('glFogCoorddEXT');
    if not Assigned(glFogCoorddEXT) then Exit;
    Pointer(glFogCoorddvEXT) := wglGetProcAddress('glFogCoorddvEXT');
    if not Assigned(glFogCoorddvEXT) then Exit;
    Pointer(glFogCoordPointerEXT) := wglGetProcAddress('glFogCoordPointerEXT');
    if not Assigned(glFogCoordPointerEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_REND_screen_coordinates(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_REND_screen_coordinates', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_coordinate_frame(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_coordinate_frame', extstring) then
  begin
    Pointer(glTangent3bEXT) := wglGetProcAddress('glTangent3bEXT');
    if not Assigned(glTangent3bEXT) then Exit;
    Pointer(glTangent3bvEXT) := wglGetProcAddress('glTangent3bvEXT');
    if not Assigned(glTangent3bvEXT) then Exit;
    Pointer(glTangent3dEXT) := wglGetProcAddress('glTangent3dEXT');
    if not Assigned(glTangent3dEXT) then Exit;
    Pointer(glTangent3dvEXT) := wglGetProcAddress('glTangent3dvEXT');
    if not Assigned(glTangent3dvEXT) then Exit;
    Pointer(glTangent3fEXT) := wglGetProcAddress('glTangent3fEXT');
    if not Assigned(glTangent3fEXT) then Exit;
    Pointer(glTangent3fvEXT) := wglGetProcAddress('glTangent3fvEXT');
    if not Assigned(glTangent3fvEXT) then Exit;
    Pointer(glTangent3iEXT) := wglGetProcAddress('glTangent3iEXT');
    if not Assigned(glTangent3iEXT) then Exit;
    Pointer(glTangent3ivEXT) := wglGetProcAddress('glTangent3ivEXT');
    if not Assigned(glTangent3ivEXT) then Exit;
    Pointer(glTangent3sEXT) := wglGetProcAddress('glTangent3sEXT');
    if not Assigned(glTangent3sEXT) then Exit;
    Pointer(glTangent3svEXT) := wglGetProcAddress('glTangent3svEXT');
    if not Assigned(glTangent3svEXT) then Exit;
    Pointer(glBinormal3bEXT) := wglGetProcAddress('glBinormal3bEXT');
    if not Assigned(glBinormal3bEXT) then Exit;
    Pointer(glBinormal3bvEXT) := wglGetProcAddress('glBinormal3bvEXT');
    if not Assigned(glBinormal3bvEXT) then Exit;
    Pointer(glBinormal3dEXT) := wglGetProcAddress('glBinormal3dEXT');
    if not Assigned(glBinormal3dEXT) then Exit;
    Pointer(glBinormal3dvEXT) := wglGetProcAddress('glBinormal3dvEXT');
    if not Assigned(glBinormal3dvEXT) then Exit;
    Pointer(glBinormal3fEXT) := wglGetProcAddress('glBinormal3fEXT');
    if not Assigned(glBinormal3fEXT) then Exit;
    Pointer(glBinormal3fvEXT) := wglGetProcAddress('glBinormal3fvEXT');
    if not Assigned(glBinormal3fvEXT) then Exit;
    Pointer(glBinormal3iEXT) := wglGetProcAddress('glBinormal3iEXT');
    if not Assigned(glBinormal3iEXT) then Exit;
    Pointer(glBinormal3ivEXT) := wglGetProcAddress('glBinormal3ivEXT');
    if not Assigned(glBinormal3ivEXT) then Exit;
    Pointer(glBinormal3sEXT) := wglGetProcAddress('glBinormal3sEXT');
    if not Assigned(glBinormal3sEXT) then Exit;
    Pointer(glBinormal3svEXT) := wglGetProcAddress('glBinormal3svEXT');
    if not Assigned(glBinormal3svEXT) then Exit;
    Pointer(glTangentPointerEXT) := wglGetProcAddress('glTangentPointerEXT');
    if not Assigned(glTangentPointerEXT) then Exit;
    Pointer(glBinormalPointerEXT) := wglGetProcAddress('glBinormalPointerEXT');
    if not Assigned(glBinormalPointerEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_env_combine(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_env_combine', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_APPLE_specular_vector(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_specular_vector', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_APPLE_transform_hint(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_transform_hint', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_fog_scale(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_fog_scale', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SUNX_constant_data(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SUNX_constant_data', extstring) then
  begin
    Pointer(glFinishTextureSUNX) := wglGetProcAddress('glFinishTextureSUNX');
    if not Assigned(glFinishTextureSUNX) then Exit;
    Result := True;
  end;
end;

function Load_GL_SUN_global_alpha(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SUN_global_alpha', extstring) then
  begin
    Pointer(glGlobalAlphaFactorbSUN) := wglGetProcAddress('glGlobalAlphaFactorbSUN');
    if not Assigned(glGlobalAlphaFactorbSUN) then Exit;
    Pointer(glGlobalAlphaFactorsSUN) := wglGetProcAddress('glGlobalAlphaFactorsSUN');
    if not Assigned(glGlobalAlphaFactorsSUN) then Exit;
    Pointer(glGlobalAlphaFactoriSUN) := wglGetProcAddress('glGlobalAlphaFactoriSUN');
    if not Assigned(glGlobalAlphaFactoriSUN) then Exit;
    Pointer(glGlobalAlphaFactorfSUN) := wglGetProcAddress('glGlobalAlphaFactorfSUN');
    if not Assigned(glGlobalAlphaFactorfSUN) then Exit;
    Pointer(glGlobalAlphaFactordSUN) := wglGetProcAddress('glGlobalAlphaFactordSUN');
    if not Assigned(glGlobalAlphaFactordSUN) then Exit;
    Pointer(glGlobalAlphaFactorubSUN) := wglGetProcAddress('glGlobalAlphaFactorubSUN');
    if not Assigned(glGlobalAlphaFactorubSUN) then Exit;
    Pointer(glGlobalAlphaFactorusSUN) := wglGetProcAddress('glGlobalAlphaFactorusSUN');
    if not Assigned(glGlobalAlphaFactorusSUN) then Exit;
    Pointer(glGlobalAlphaFactoruiSUN) := wglGetProcAddress('glGlobalAlphaFactoruiSUN');
    if not Assigned(glGlobalAlphaFactoruiSUN) then Exit;
    Result := True;
  end;
end;

function Load_GL_SUN_triangle_list(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SUN_triangle_list', extstring) then
  begin
    Pointer(glReplacementCodeuiSUN) := wglGetProcAddress('glReplacementCodeuiSUN');
    if not Assigned(glReplacementCodeuiSUN) then Exit;
    Pointer(glReplacementCodeusSUN) := wglGetProcAddress('glReplacementCodeusSUN');
    if not Assigned(glReplacementCodeusSUN) then Exit;
    Pointer(glReplacementCodeubSUN) := wglGetProcAddress('glReplacementCodeubSUN');
    if not Assigned(glReplacementCodeubSUN) then Exit;
    Pointer(glReplacementCodeuivSUN) := wglGetProcAddress('glReplacementCodeuivSUN');
    if not Assigned(glReplacementCodeuivSUN) then Exit;
    Pointer(glReplacementCodeusvSUN) := wglGetProcAddress('glReplacementCodeusvSUN');
    if not Assigned(glReplacementCodeusvSUN) then Exit;
    Pointer(glReplacementCodeubvSUN) := wglGetProcAddress('glReplacementCodeubvSUN');
    if not Assigned(glReplacementCodeubvSUN) then Exit;
    Pointer(glReplacementCodePointerSUN) := wglGetProcAddress('glReplacementCodePointerSUN');
    if not Assigned(glReplacementCodePointerSUN) then Exit;
    Result := True;
  end;
end;

function Load_GL_SUN_vertex(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SUN_vertex', extstring) then
  begin
    Pointer(glColor4ubVertex2fSUN) := wglGetProcAddress('glColor4ubVertex2fSUN');
    if not Assigned(glColor4ubVertex2fSUN) then Exit;
    Pointer(glColor4ubVertex2fvSUN) := wglGetProcAddress('glColor4ubVertex2fvSUN');
    if not Assigned(glColor4ubVertex2fvSUN) then Exit;
    Pointer(glColor4ubVertex3fSUN) := wglGetProcAddress('glColor4ubVertex3fSUN');
    if not Assigned(glColor4ubVertex3fSUN) then Exit;
    Pointer(glColor4ubVertex3fvSUN) := wglGetProcAddress('glColor4ubVertex3fvSUN');
    if not Assigned(glColor4ubVertex3fvSUN) then Exit;
    Pointer(glColor3fVertex3fSUN) := wglGetProcAddress('glColor3fVertex3fSUN');
    if not Assigned(glColor3fVertex3fSUN) then Exit;
    Pointer(glColor3fVertex3fvSUN) := wglGetProcAddress('glColor3fVertex3fvSUN');
    if not Assigned(glColor3fVertex3fvSUN) then Exit;
    Pointer(glNormal3fVertex3fSUN) := wglGetProcAddress('glNormal3fVertex3fSUN');
    if not Assigned(glNormal3fVertex3fSUN) then Exit;
    Pointer(glNormal3fVertex3fvSUN) := wglGetProcAddress('glNormal3fVertex3fvSUN');
    if not Assigned(glNormal3fVertex3fvSUN) then Exit;
    Pointer(glColor4fNormal3fVertex3fSUN) := wglGetProcAddress('glColor4fNormal3fVertex3fSUN');
    if not Assigned(glColor4fNormal3fVertex3fSUN) then Exit;
    Pointer(glColor4fNormal3fVertex3fvSUN) := wglGetProcAddress('glColor4fNormal3fVertex3fvSUN');
    if not Assigned(glColor4fNormal3fVertex3fvSUN) then Exit;
    Pointer(glTexCoord2fVertex3fSUN) := wglGetProcAddress('glTexCoord2fVertex3fSUN');
    if not Assigned(glTexCoord2fVertex3fSUN) then Exit;
    Pointer(glTexCoord2fVertex3fvSUN) := wglGetProcAddress('glTexCoord2fVertex3fvSUN');
    if not Assigned(glTexCoord2fVertex3fvSUN) then Exit;
    Pointer(glTexCoord4fVertex4fSUN) := wglGetProcAddress('glTexCoord4fVertex4fSUN');
    if not Assigned(glTexCoord4fVertex4fSUN) then Exit;
    Pointer(glTexCoord4fVertex4fvSUN) := wglGetProcAddress('glTexCoord4fVertex4fvSUN');
    if not Assigned(glTexCoord4fVertex4fvSUN) then Exit;
    Pointer(glTexCoord2fColor4ubVertex3fSUN) := wglGetProcAddress('glTexCoord2fColor4ubVertex3fSUN');
    if not Assigned(glTexCoord2fColor4ubVertex3fSUN) then Exit;
    Pointer(glTexCoord2fColor4ubVertex3fvSUN) := wglGetProcAddress('glTexCoord2fColor4ubVertex3fvSUN');
    if not Assigned(glTexCoord2fColor4ubVertex3fvSUN) then Exit;
    Pointer(glTexCoord2fColor3fVertex3fSUN) := wglGetProcAddress('glTexCoord2fColor3fVertex3fSUN');
    if not Assigned(glTexCoord2fColor3fVertex3fSUN) then Exit;
    Pointer(glTexCoord2fColor3fVertex3fvSUN) := wglGetProcAddress('glTexCoord2fColor3fVertex3fvSUN');
    if not Assigned(glTexCoord2fColor3fVertex3fvSUN) then Exit;
    Pointer(glTexCoord2fNormal3fVertex3fSUN) := wglGetProcAddress('glTexCoord2fNormal3fVertex3fSUN');
    if not Assigned(glTexCoord2fNormal3fVertex3fSUN) then Exit;
    Pointer(glTexCoord2fNormal3fVertex3fvSUN) := wglGetProcAddress('glTexCoord2fNormal3fVertex3fvSUN');
    if not Assigned(glTexCoord2fNormal3fVertex3fvSUN) then Exit;
    Pointer(glTexCoord2fColor4fNormal3fVertex3fSUN) := wglGetProcAddress('glTexCoord2fColor4fNormal3fVertex3fSUN');
    if not Assigned(glTexCoord2fColor4fNormal3fVertex3fSUN) then Exit;
    Pointer(glTexCoord2fColor4fNormal3fVertex3fvSUN) := wglGetProcAddress('glTexCoord2fColor4fNormal3fVertex3fvSUN');
    if not Assigned(glTexCoord2fColor4fNormal3fVertex3fvSUN) then Exit;
    Pointer(glTexCoord4fColor4fNormal3fVertex4fSUN) := wglGetProcAddress('glTexCoord4fColor4fNormal3fVertex4fSUN');
    if not Assigned(glTexCoord4fColor4fNormal3fVertex4fSUN) then Exit;
    Pointer(glTexCoord4fColor4fNormal3fVertex4fvSUN) := wglGetProcAddress('glTexCoord4fColor4fNormal3fVertex4fvSUN');
    if not Assigned(glTexCoord4fColor4fNormal3fVertex4fvSUN) then Exit;
    Pointer(glReplacementCodeuiVertex3fSUN) := wglGetProcAddress('glReplacementCodeuiVertex3fSUN');
    if not Assigned(glReplacementCodeuiVertex3fSUN) then Exit;
    Pointer(glReplacementCodeuiVertex3fvSUN) := wglGetProcAddress('glReplacementCodeuiVertex3fvSUN');
    if not Assigned(glReplacementCodeuiVertex3fvSUN) then Exit;
    Pointer(glReplacementCodeuiColor4ubVertex3fSUN) := wglGetProcAddress('glReplacementCodeuiColor4ubVertex3fSUN');
    if not Assigned(glReplacementCodeuiColor4ubVertex3fSUN) then Exit;
    Pointer(glReplacementCodeuiColor4ubVertex3fvSUN) := wglGetProcAddress('glReplacementCodeuiColor4ubVertex3fvSUN');
    if not Assigned(glReplacementCodeuiColor4ubVertex3fvSUN) then Exit;
    Pointer(glReplacementCodeuiColor3fVertex3fSUN) := wglGetProcAddress('glReplacementCodeuiColor3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiColor3fVertex3fSUN) then Exit;
    Pointer(glReplacementCodeuiColor3fVertex3fvSUN) := wglGetProcAddress('glReplacementCodeuiColor3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiColor3fVertex3fvSUN) then Exit;
    Pointer(glReplacementCodeuiNormal3fVertex3fSUN) := wglGetProcAddress('glReplacementCodeuiNormal3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiNormal3fVertex3fSUN) then Exit;
    Pointer(glReplacementCodeuiNormal3fVertex3fvSUN) := wglGetProcAddress('glReplacementCodeuiNormal3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiNormal3fVertex3fvSUN) then Exit;
    Pointer(glReplacementCodeuiColor4fNormal3fVertex3fSUN) := wglGetProcAddress('glReplacementCodeuiColor4fNormal3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiColor4fNormal3fVertex3fSUN) then Exit;
    Pointer(glReplacementCodeuiColor4fNormal3fVertex3fvSUN) := wglGetProcAddress('glReplacementCodeuiColor4fNormal3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiColor4fNormal3fVertex3fvSUN) then Exit;
    Pointer(glReplacementCodeuiTexCoord2fVertex3fSUN) := wglGetProcAddress('glReplacementCodeuiTexCoord2fVertex3fSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fVertex3fSUN) then Exit;
    Pointer(glReplacementCodeuiTexCoord2fVertex3fvSUN) := wglGetProcAddress('glReplacementCodeuiTexCoord2fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fVertex3fvSUN) then Exit;
    Pointer(glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN) := wglGetProcAddress('glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN) then Exit;
    Pointer(glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN) := wglGetProcAddress('glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN) then Exit;
    Pointer(glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN) := wglGetProcAddress('glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN) then Exit;
    Pointer(glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN) := wglGetProcAddress('glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_blend_func_separate(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_func_separate', extstring) then
  begin
    Pointer(glBlendFuncSeparateEXT) := wglGetProcAddress('glBlendFuncSeparateEXT');
    if not Assigned(glBlendFuncSeparateEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_INGR_color_clamp(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_INGR_color_clamp', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_INGR_interlace_read(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_INGR_interlace_read', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_stencil_wrap(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_stencil_wrap', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_422_pixels(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_422_pixels', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_texgen_reflection(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texgen_reflection', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_cube_map(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_cube_map', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SUN_convolution_border_modes(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SUN_convolution_border_modes', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_env_add(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_env_add', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_lod_bias(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_lod_bias', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_filter_anisotropic(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_filter_anisotropic', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_vertex_weighting(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_vertex_weighting', extstring) then
  begin
    Pointer(glVertexWeightfEXT) := wglGetProcAddress('glVertexWeightfEXT');
    if not Assigned(glVertexWeightfEXT) then Exit;
    Pointer(glVertexWeightfvEXT) := wglGetProcAddress('glVertexWeightfvEXT');
    if not Assigned(glVertexWeightfvEXT) then Exit;
    Pointer(glVertexWeightPointerEXT) := wglGetProcAddress('glVertexWeightPointerEXT');
    if not Assigned(glVertexWeightPointerEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_light_max_exponent(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_light_max_exponent', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_vertex_array_range(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_array_range', extstring) then
  begin
    Pointer(glFlushVertexArrayRangeNV) := wglGetProcAddress('glFlushVertexArrayRangeNV');
    if not Assigned(glFlushVertexArrayRangeNV) then Exit;
    Pointer(glVertexArrayRangeNV) := wglGetProcAddress('glVertexArrayRangeNV');
    if not Assigned(glVertexArrayRangeNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_register_combiners(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_register_combiners', extstring) then
  begin
    Pointer(glCombinerParameterfvNV) := wglGetProcAddress('glCombinerParameterfvNV');
    if not Assigned(glCombinerParameterfvNV) then Exit;
    Pointer(glCombinerParameterfNV) := wglGetProcAddress('glCombinerParameterfNV');
    if not Assigned(glCombinerParameterfNV) then Exit;
    Pointer(glCombinerParameterivNV) := wglGetProcAddress('glCombinerParameterivNV');
    if not Assigned(glCombinerParameterivNV) then Exit;
    Pointer(glCombinerParameteriNV) := wglGetProcAddress('glCombinerParameteriNV');
    if not Assigned(glCombinerParameteriNV) then Exit;
    Pointer(glCombinerInputNV) := wglGetProcAddress('glCombinerInputNV');
    if not Assigned(glCombinerInputNV) then Exit;
    Pointer(glCombinerOutputNV) := wglGetProcAddress('glCombinerOutputNV');
    if not Assigned(glCombinerOutputNV) then Exit;
    Pointer(glFinalCombinerInputNV) := wglGetProcAddress('glFinalCombinerInputNV');
    if not Assigned(glFinalCombinerInputNV) then Exit;
    Pointer(glGetCombinerInputParameterfvNV) := wglGetProcAddress('glGetCombinerInputParameterfvNV');
    if not Assigned(glGetCombinerInputParameterfvNV) then Exit;
    Pointer(glGetCombinerInputParameterivNV) := wglGetProcAddress('glGetCombinerInputParameterivNV');
    if not Assigned(glGetCombinerInputParameterivNV) then Exit;
    Pointer(glGetCombinerOutputParameterfvNV) := wglGetProcAddress('glGetCombinerOutputParameterfvNV');
    if not Assigned(glGetCombinerOutputParameterfvNV) then Exit;
    Pointer(glGetCombinerOutputParameterivNV) := wglGetProcAddress('glGetCombinerOutputParameterivNV');
    if not Assigned(glGetCombinerOutputParameterivNV) then Exit;
    Pointer(glGetFinalCombinerInputParameterfvNV) := wglGetProcAddress('glGetFinalCombinerInputParameterfvNV');
    if not Assigned(glGetFinalCombinerInputParameterfvNV) then Exit;
    Pointer(glGetFinalCombinerInputParameterivNV) := wglGetProcAddress('glGetFinalCombinerInputParameterivNV');
    if not Assigned(glGetFinalCombinerInputParameterivNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_fog_distance(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fog_distance', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_texgen_emboss(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texgen_emboss', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_blend_square(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_blend_square', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_texture_env_combine4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_env_combine4', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_MESA_resize_buffers(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESA_resize_buffers', extstring) then
  begin
    Pointer(glResizeBuffersMESA) := wglGetProcAddress('glResizeBuffersMESA');
    if not Assigned(glResizeBuffersMESA) then Exit;
    Result := True;
  end;
end;

function Load_GL_MESA_window_pos(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESA_window_pos', extstring) then
  begin
    Pointer(glWindowPos2dMESA) := wglGetProcAddress('glWindowPos2dMESA');
    if not Assigned(glWindowPos2dMESA) then Exit;
    Pointer(glWindowPos2dvMESA) := wglGetProcAddress('glWindowPos2dvMESA');
    if not Assigned(glWindowPos2dvMESA) then Exit;
    Pointer(glWindowPos2fMESA) := wglGetProcAddress('glWindowPos2fMESA');
    if not Assigned(glWindowPos2fMESA) then Exit;
    Pointer(glWindowPos2fvMESA) := wglGetProcAddress('glWindowPos2fvMESA');
    if not Assigned(glWindowPos2fvMESA) then Exit;
    Pointer(glWindowPos2iMESA) := wglGetProcAddress('glWindowPos2iMESA');
    if not Assigned(glWindowPos2iMESA) then Exit;
    Pointer(glWindowPos2ivMESA) := wglGetProcAddress('glWindowPos2ivMESA');
    if not Assigned(glWindowPos2ivMESA) then Exit;
    Pointer(glWindowPos2sMESA) := wglGetProcAddress('glWindowPos2sMESA');
    if not Assigned(glWindowPos2sMESA) then Exit;
    Pointer(glWindowPos2svMESA) := wglGetProcAddress('glWindowPos2svMESA');
    if not Assigned(glWindowPos2svMESA) then Exit;
    Pointer(glWindowPos3dMESA) := wglGetProcAddress('glWindowPos3dMESA');
    if not Assigned(glWindowPos3dMESA) then Exit;
    Pointer(glWindowPos3dvMESA) := wglGetProcAddress('glWindowPos3dvMESA');
    if not Assigned(glWindowPos3dvMESA) then Exit;
    Pointer(glWindowPos3fMESA) := wglGetProcAddress('glWindowPos3fMESA');
    if not Assigned(glWindowPos3fMESA) then Exit;
    Pointer(glWindowPos3fvMESA) := wglGetProcAddress('glWindowPos3fvMESA');
    if not Assigned(glWindowPos3fvMESA) then Exit;
    Pointer(glWindowPos3iMESA) := wglGetProcAddress('glWindowPos3iMESA');
    if not Assigned(glWindowPos3iMESA) then Exit;
    Pointer(glWindowPos3ivMESA) := wglGetProcAddress('glWindowPos3ivMESA');
    if not Assigned(glWindowPos3ivMESA) then Exit;
    Pointer(glWindowPos3sMESA) := wglGetProcAddress('glWindowPos3sMESA');
    if not Assigned(glWindowPos3sMESA) then Exit;
    Pointer(glWindowPos3svMESA) := wglGetProcAddress('glWindowPos3svMESA');
    if not Assigned(glWindowPos3svMESA) then Exit;
    Pointer(glWindowPos4dMESA) := wglGetProcAddress('glWindowPos4dMESA');
    if not Assigned(glWindowPos4dMESA) then Exit;
    Pointer(glWindowPos4dvMESA) := wglGetProcAddress('glWindowPos4dvMESA');
    if not Assigned(glWindowPos4dvMESA) then Exit;
    Pointer(glWindowPos4fMESA) := wglGetProcAddress('glWindowPos4fMESA');
    if not Assigned(glWindowPos4fMESA) then Exit;
    Pointer(glWindowPos4fvMESA) := wglGetProcAddress('glWindowPos4fvMESA');
    if not Assigned(glWindowPos4fvMESA) then Exit;
    Pointer(glWindowPos4iMESA) := wglGetProcAddress('glWindowPos4iMESA');
    if not Assigned(glWindowPos4iMESA) then Exit;
    Pointer(glWindowPos4ivMESA) := wglGetProcAddress('glWindowPos4ivMESA');
    if not Assigned(glWindowPos4ivMESA) then Exit;
    Pointer(glWindowPos4sMESA) := wglGetProcAddress('glWindowPos4sMESA');
    if not Assigned(glWindowPos4sMESA) then Exit;
    Pointer(glWindowPos4svMESA) := wglGetProcAddress('glWindowPos4svMESA');
    if not Assigned(glWindowPos4svMESA) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_compression_s3tc(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_compression_s3tc', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_IBM_cull_vertex(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_cull_vertex', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_IBM_multimode_draw_arrays(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_multimode_draw_arrays', extstring) then
  begin
    Pointer(glMultiModeDrawArraysIBM) := wglGetProcAddress('glMultiModeDrawArraysIBM');
    if not Assigned(glMultiModeDrawArraysIBM) then Exit;
    Pointer(glMultiModeDrawElementsIBM) := wglGetProcAddress('glMultiModeDrawElementsIBM');
    if not Assigned(glMultiModeDrawElementsIBM) then Exit;
    Result := True;
  end;
end;

function Load_GL_IBM_vertex_array_lists(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_vertex_array_lists', extstring) then
  begin
    Pointer(glColorPointerListIBM) := wglGetProcAddress('glColorPointerListIBM');
    if not Assigned(glColorPointerListIBM) then Exit;
    Pointer(glSecondaryColorPointerListIBM) := wglGetProcAddress('glSecondaryColorPointerListIBM');
    if not Assigned(glSecondaryColorPointerListIBM) then Exit;
    Pointer(glEdgeFlagPointerListIBM) := wglGetProcAddress('glEdgeFlagPointerListIBM');
    if not Assigned(glEdgeFlagPointerListIBM) then Exit;
    Pointer(glFogCoordPointerListIBM) := wglGetProcAddress('glFogCoordPointerListIBM');
    if not Assigned(glFogCoordPointerListIBM) then Exit;
    Pointer(glIndexPointerListIBM) := wglGetProcAddress('glIndexPointerListIBM');
    if not Assigned(glIndexPointerListIBM) then Exit;
    Pointer(glNormalPointerListIBM) := wglGetProcAddress('glNormalPointerListIBM');
    if not Assigned(glNormalPointerListIBM) then Exit;
    Pointer(glTexCoordPointerListIBM) := wglGetProcAddress('glTexCoordPointerListIBM');
    if not Assigned(glTexCoordPointerListIBM) then Exit;
    Pointer(glVertexPointerListIBM) := wglGetProcAddress('glVertexPointerListIBM');
    if not Assigned(glVertexPointerListIBM) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_subsample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_subsample', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_ycrcb_subsample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_ycrcb_subsample', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_ycrcba(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_ycrcba', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGI_depth_pass_instrument(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGI_depth_pass_instrument', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_3DFX_texture_compression_FXT1(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_3DFX_texture_compression_FXT1', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_3DFX_multisample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_3DFX_multisample', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_3DFX_tbuffer(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_3DFX_tbuffer', extstring) then
  begin
    Pointer(glTbufferMask3DFX) := wglGetProcAddress('glTbufferMask3DFX');
    if not Assigned(glTbufferMask3DFX) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_multisample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_multisample', extstring) then
  begin
    Pointer(glSampleMaskEXT) := wglGetProcAddress('glSampleMaskEXT');
    if not Assigned(glSampleMaskEXT) then Exit;
    Pointer(glSamplePatternEXT) := wglGetProcAddress('glSamplePatternEXT');
    if not Assigned(glSamplePatternEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_vertex_preclip(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_vertex_preclip', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_convolution_accuracy(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_convolution_accuracy', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_resample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_resample', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_point_line_texgen(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_point_line_texgen', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIS_texture_color_mask(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_color_mask', extstring) then
  begin
    Pointer(glTextureColorMaskSGIS) := wglGetProcAddress('glTextureColorMaskSGIS');
    if not Assigned(glTextureColorMaskSGIS) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_env_dot3(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_env_dot3', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ATI_texture_mirror_once(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_texture_mirror_once', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_fence(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fence', extstring) then
  begin
    Pointer(glDeleteFencesNV) := wglGetProcAddress('glDeleteFencesNV');
    if not Assigned(glDeleteFencesNV) then Exit;
    Pointer(glGenFencesNV) := wglGetProcAddress('glGenFencesNV');
    if not Assigned(glGenFencesNV) then Exit;
    Pointer(glIsFenceNV) := wglGetProcAddress('glIsFenceNV');
    if not Assigned(glIsFenceNV) then Exit;
    Pointer(glTestFenceNV) := wglGetProcAddress('glTestFenceNV');
    if not Assigned(glTestFenceNV) then Exit;
    Pointer(glGetFenceivNV) := wglGetProcAddress('glGetFenceivNV');
    if not Assigned(glGetFenceivNV) then Exit;
    Pointer(glFinishFenceNV) := wglGetProcAddress('glFinishFenceNV');
    if not Assigned(glFinishFenceNV) then Exit;
    Pointer(glSetFenceNV) := wglGetProcAddress('glSetFenceNV');
    if not Assigned(glSetFenceNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_IBM_texture_mirrored_repeat(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_texture_mirrored_repeat', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_evaluators(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_evaluators', extstring) then
  begin
    Pointer(glMapControlPointsNV) := wglGetProcAddress('glMapControlPointsNV');
    if not Assigned(glMapControlPointsNV) then Exit;
    Pointer(glMapParameterivNV) := wglGetProcAddress('glMapParameterivNV');
    if not Assigned(glMapParameterivNV) then Exit;
    Pointer(glMapParameterfvNV) := wglGetProcAddress('glMapParameterfvNV');
    if not Assigned(glMapParameterfvNV) then Exit;
    Pointer(glGetMapControlPointsNV) := wglGetProcAddress('glGetMapControlPointsNV');
    if not Assigned(glGetMapControlPointsNV) then Exit;
    Pointer(glGetMapParameterivNV) := wglGetProcAddress('glGetMapParameterivNV');
    if not Assigned(glGetMapParameterivNV) then Exit;
    Pointer(glGetMapParameterfvNV) := wglGetProcAddress('glGetMapParameterfvNV');
    if not Assigned(glGetMapParameterfvNV) then Exit;
    Pointer(glGetMapAttribParameterivNV) := wglGetProcAddress('glGetMapAttribParameterivNV');
    if not Assigned(glGetMapAttribParameterivNV) then Exit;
    Pointer(glGetMapAttribParameterfvNV) := wglGetProcAddress('glGetMapAttribParameterfvNV');
    if not Assigned(glGetMapAttribParameterfvNV) then Exit;
    Pointer(glEvalMapsNV) := wglGetProcAddress('glEvalMapsNV');
    if not Assigned(glEvalMapsNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_packed_depth_stencil(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_packed_depth_stencil', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_register_combiners2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_register_combiners2', extstring) then
  begin
    Pointer(glCombinerStageParameterfvNV) := wglGetProcAddress('glCombinerStageParameterfvNV');
    if not Assigned(glCombinerStageParameterfvNV) then Exit;
    Pointer(glGetCombinerStageParameterfvNV) := wglGetProcAddress('glGetCombinerStageParameterfvNV');
    if not Assigned(glGetCombinerStageParameterfvNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_texture_compression_vtc(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_compression_vtc', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_texture_rectangle(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_rectangle', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_texture_shader(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_shader', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_texture_shader2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_shader2', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_vertex_array_range2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_array_range2', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_vertex_program(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program', extstring) then
  begin
    Pointer(glAreProgramsResidentNV) := wglGetProcAddress('glAreProgramsResidentNV');
    if not Assigned(glAreProgramsResidentNV) then Exit;
    Pointer(glBindProgramNV) := wglGetProcAddress('glBindProgramNV');
    if not Assigned(glBindProgramNV) then Exit;
    Pointer(glDeleteProgramsNV) := wglGetProcAddress('glDeleteProgramsNV');
    if not Assigned(glDeleteProgramsNV) then Exit;
    Pointer(glExecuteProgramNV) := wglGetProcAddress('glExecuteProgramNV');
    if not Assigned(glExecuteProgramNV) then Exit;
    Pointer(glGenProgramsNV) := wglGetProcAddress('glGenProgramsNV');
    if not Assigned(glGenProgramsNV) then Exit;
    Pointer(glGetProgramParameterdvNV) := wglGetProcAddress('glGetProgramParameterdvNV');
    if not Assigned(glGetProgramParameterdvNV) then Exit;
    Pointer(glGetProgramParameterfvNV) := wglGetProcAddress('glGetProgramParameterfvNV');
    if not Assigned(glGetProgramParameterfvNV) then Exit;
    Pointer(glGetProgramivNV) := wglGetProcAddress('glGetProgramivNV');
    if not Assigned(glGetProgramivNV) then Exit;
    Pointer(glGetProgramStringNV) := wglGetProcAddress('glGetProgramStringNV');
    if not Assigned(glGetProgramStringNV) then Exit;
    Pointer(glGetTrackMatrixivNV) := wglGetProcAddress('glGetTrackMatrixivNV');
    if not Assigned(glGetTrackMatrixivNV) then Exit;
    Pointer(glGetVertexAttribdvNV) := wglGetProcAddress('glGetVertexAttribdvNV');
    if not Assigned(glGetVertexAttribdvNV) then Exit;
    Pointer(glGetVertexAttribfvNV) := wglGetProcAddress('glGetVertexAttribfvNV');
    if not Assigned(glGetVertexAttribfvNV) then Exit;
    Pointer(glGetVertexAttribivNV) := wglGetProcAddress('glGetVertexAttribivNV');
    if not Assigned(glGetVertexAttribivNV) then Exit;
    Pointer(glGetVertexAttribPointervNV) := wglGetProcAddress('glGetVertexAttribPointervNV');
    if not Assigned(glGetVertexAttribPointervNV) then Exit;
    Pointer(glIsProgramNV) := wglGetProcAddress('glIsProgramNV');
    if not Assigned(glIsProgramNV) then Exit;
    Pointer(glLoadProgramNV) := wglGetProcAddress('glLoadProgramNV');
    if not Assigned(glLoadProgramNV) then Exit;
    Pointer(glProgramParameter4dNV) := wglGetProcAddress('glProgramParameter4dNV');
    if not Assigned(glProgramParameter4dNV) then Exit;
    Pointer(glProgramParameter4dvNV) := wglGetProcAddress('glProgramParameter4dvNV');
    if not Assigned(glProgramParameter4dvNV) then Exit;
    Pointer(glProgramParameter4fNV) := wglGetProcAddress('glProgramParameter4fNV');
    if not Assigned(glProgramParameter4fNV) then Exit;
    Pointer(glProgramParameter4fvNV) := wglGetProcAddress('glProgramParameter4fvNV');
    if not Assigned(glProgramParameter4fvNV) then Exit;
    Pointer(glProgramParameters4dvNV) := wglGetProcAddress('glProgramParameters4dvNV');
    if not Assigned(glProgramParameters4dvNV) then Exit;
    Pointer(glProgramParameters4fvNV) := wglGetProcAddress('glProgramParameters4fvNV');
    if not Assigned(glProgramParameters4fvNV) then Exit;
    Pointer(glRequestResidentProgramsNV) := wglGetProcAddress('glRequestResidentProgramsNV');
    if not Assigned(glRequestResidentProgramsNV) then Exit;
    Pointer(glTrackMatrixNV) := wglGetProcAddress('glTrackMatrixNV');
    if not Assigned(glTrackMatrixNV) then Exit;
    Pointer(glVertexAttribPointerNV) := wglGetProcAddress('glVertexAttribPointerNV');
    if not Assigned(glVertexAttribPointerNV) then Exit;
    Pointer(glVertexAttrib1dNV) := wglGetProcAddress('glVertexAttrib1dNV');
    if not Assigned(glVertexAttrib1dNV) then Exit;
    Pointer(glVertexAttrib1dvNV) := wglGetProcAddress('glVertexAttrib1dvNV');
    if not Assigned(glVertexAttrib1dvNV) then Exit;
    Pointer(glVertexAttrib1fNV) := wglGetProcAddress('glVertexAttrib1fNV');
    if not Assigned(glVertexAttrib1fNV) then Exit;
    Pointer(glVertexAttrib1fvNV) := wglGetProcAddress('glVertexAttrib1fvNV');
    if not Assigned(glVertexAttrib1fvNV) then Exit;
    Pointer(glVertexAttrib1sNV) := wglGetProcAddress('glVertexAttrib1sNV');
    if not Assigned(glVertexAttrib1sNV) then Exit;
    Pointer(glVertexAttrib1svNV) := wglGetProcAddress('glVertexAttrib1svNV');
    if not Assigned(glVertexAttrib1svNV) then Exit;
    Pointer(glVertexAttrib2dNV) := wglGetProcAddress('glVertexAttrib2dNV');
    if not Assigned(glVertexAttrib2dNV) then Exit;
    Pointer(glVertexAttrib2dvNV) := wglGetProcAddress('glVertexAttrib2dvNV');
    if not Assigned(glVertexAttrib2dvNV) then Exit;
    Pointer(glVertexAttrib2fNV) := wglGetProcAddress('glVertexAttrib2fNV');
    if not Assigned(glVertexAttrib2fNV) then Exit;
    Pointer(glVertexAttrib2fvNV) := wglGetProcAddress('glVertexAttrib2fvNV');
    if not Assigned(glVertexAttrib2fvNV) then Exit;
    Pointer(glVertexAttrib2sNV) := wglGetProcAddress('glVertexAttrib2sNV');
    if not Assigned(glVertexAttrib2sNV) then Exit;
    Pointer(glVertexAttrib2svNV) := wglGetProcAddress('glVertexAttrib2svNV');
    if not Assigned(glVertexAttrib2svNV) then Exit;
    Pointer(glVertexAttrib3dNV) := wglGetProcAddress('glVertexAttrib3dNV');
    if not Assigned(glVertexAttrib3dNV) then Exit;
    Pointer(glVertexAttrib3dvNV) := wglGetProcAddress('glVertexAttrib3dvNV');
    if not Assigned(glVertexAttrib3dvNV) then Exit;
    Pointer(glVertexAttrib3fNV) := wglGetProcAddress('glVertexAttrib3fNV');
    if not Assigned(glVertexAttrib3fNV) then Exit;
    Pointer(glVertexAttrib3fvNV) := wglGetProcAddress('glVertexAttrib3fvNV');
    if not Assigned(glVertexAttrib3fvNV) then Exit;
    Pointer(glVertexAttrib3sNV) := wglGetProcAddress('glVertexAttrib3sNV');
    if not Assigned(glVertexAttrib3sNV) then Exit;
    Pointer(glVertexAttrib3svNV) := wglGetProcAddress('glVertexAttrib3svNV');
    if not Assigned(glVertexAttrib3svNV) then Exit;
    Pointer(glVertexAttrib4dNV) := wglGetProcAddress('glVertexAttrib4dNV');
    if not Assigned(glVertexAttrib4dNV) then Exit;
    Pointer(glVertexAttrib4dvNV) := wglGetProcAddress('glVertexAttrib4dvNV');
    if not Assigned(glVertexAttrib4dvNV) then Exit;
    Pointer(glVertexAttrib4fNV) := wglGetProcAddress('glVertexAttrib4fNV');
    if not Assigned(glVertexAttrib4fNV) then Exit;
    Pointer(glVertexAttrib4fvNV) := wglGetProcAddress('glVertexAttrib4fvNV');
    if not Assigned(glVertexAttrib4fvNV) then Exit;
    Pointer(glVertexAttrib4sNV) := wglGetProcAddress('glVertexAttrib4sNV');
    if not Assigned(glVertexAttrib4sNV) then Exit;
    Pointer(glVertexAttrib4svNV) := wglGetProcAddress('glVertexAttrib4svNV');
    if not Assigned(glVertexAttrib4svNV) then Exit;
    Pointer(glVertexAttrib4ubNV) := wglGetProcAddress('glVertexAttrib4ubNV');
    if not Assigned(glVertexAttrib4ubNV) then Exit;
    Pointer(glVertexAttrib4ubvNV) := wglGetProcAddress('glVertexAttrib4ubvNV');
    if not Assigned(glVertexAttrib4ubvNV) then Exit;
    Pointer(glVertexAttribs1dvNV) := wglGetProcAddress('glVertexAttribs1dvNV');
    if not Assigned(glVertexAttribs1dvNV) then Exit;
    Pointer(glVertexAttribs1fvNV) := wglGetProcAddress('glVertexAttribs1fvNV');
    if not Assigned(glVertexAttribs1fvNV) then Exit;
    Pointer(glVertexAttribs1svNV) := wglGetProcAddress('glVertexAttribs1svNV');
    if not Assigned(glVertexAttribs1svNV) then Exit;
    Pointer(glVertexAttribs2dvNV) := wglGetProcAddress('glVertexAttribs2dvNV');
    if not Assigned(glVertexAttribs2dvNV) then Exit;
    Pointer(glVertexAttribs2fvNV) := wglGetProcAddress('glVertexAttribs2fvNV');
    if not Assigned(glVertexAttribs2fvNV) then Exit;
    Pointer(glVertexAttribs2svNV) := wglGetProcAddress('glVertexAttribs2svNV');
    if not Assigned(glVertexAttribs2svNV) then Exit;
    Pointer(glVertexAttribs3dvNV) := wglGetProcAddress('glVertexAttribs3dvNV');
    if not Assigned(glVertexAttribs3dvNV) then Exit;
    Pointer(glVertexAttribs3fvNV) := wglGetProcAddress('glVertexAttribs3fvNV');
    if not Assigned(glVertexAttribs3fvNV) then Exit;
    Pointer(glVertexAttribs3svNV) := wglGetProcAddress('glVertexAttribs3svNV');
    if not Assigned(glVertexAttribs3svNV) then Exit;
    Pointer(glVertexAttribs4dvNV) := wglGetProcAddress('glVertexAttribs4dvNV');
    if not Assigned(glVertexAttribs4dvNV) then Exit;
    Pointer(glVertexAttribs4fvNV) := wglGetProcAddress('glVertexAttribs4fvNV');
    if not Assigned(glVertexAttribs4fvNV) then Exit;
    Pointer(glVertexAttribs4svNV) := wglGetProcAddress('glVertexAttribs4svNV');
    if not Assigned(glVertexAttribs4svNV) then Exit;
    Pointer(glVertexAttribs4ubvNV) := wglGetProcAddress('glVertexAttribs4ubvNV');
    if not Assigned(glVertexAttribs4ubvNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_texture_coordinate_clamp(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_texture_coordinate_clamp', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_scalebias_hint(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_scalebias_hint', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_OML_interlace(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_OML_interlace', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_OML_subsample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_OML_subsample', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_OML_resample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_OML_resample', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_copy_depth_to_color(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_copy_depth_to_color', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ATI_envmap_bumpmap(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_envmap_bumpmap', extstring) then
  begin
    Pointer(glTexBumpParameterivATI) := wglGetProcAddress('glTexBumpParameterivATI');
    if not Assigned(glTexBumpParameterivATI) then Exit;
    Pointer(glTexBumpParameterfvATI) := wglGetProcAddress('glTexBumpParameterfvATI');
    if not Assigned(glTexBumpParameterfvATI) then Exit;
    Pointer(glGetTexBumpParameterivATI) := wglGetProcAddress('glGetTexBumpParameterivATI');
    if not Assigned(glGetTexBumpParameterivATI) then Exit;
    Pointer(glGetTexBumpParameterfvATI) := wglGetProcAddress('glGetTexBumpParameterfvATI');
    if not Assigned(glGetTexBumpParameterfvATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_fragment_shader(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_fragment_shader', extstring) then
  begin
    Pointer(glGenFragmentShadersATI) := wglGetProcAddress('glGenFragmentShadersATI');
    if not Assigned(glGenFragmentShadersATI) then Exit;
    Pointer(glBindFragmentShaderATI) := wglGetProcAddress('glBindFragmentShaderATI');
    if not Assigned(glBindFragmentShaderATI) then Exit;
    Pointer(glDeleteFragmentShaderATI) := wglGetProcAddress('glDeleteFragmentShaderATI');
    if not Assigned(glDeleteFragmentShaderATI) then Exit;
    Pointer(glBeginFragmentShaderATI) := wglGetProcAddress('glBeginFragmentShaderATI');
    if not Assigned(glBeginFragmentShaderATI) then Exit;
    Pointer(glEndFragmentShaderATI) := wglGetProcAddress('glEndFragmentShaderATI');
    if not Assigned(glEndFragmentShaderATI) then Exit;
    Pointer(glPassTexCoordATI) := wglGetProcAddress('glPassTexCoordATI');
    if not Assigned(glPassTexCoordATI) then Exit;
    Pointer(glSampleMapATI) := wglGetProcAddress('glSampleMapATI');
    if not Assigned(glSampleMapATI) then Exit;
    Pointer(glColorFragmentOp1ATI) := wglGetProcAddress('glColorFragmentOp1ATI');
    if not Assigned(glColorFragmentOp1ATI) then Exit;
    Pointer(glColorFragmentOp2ATI) := wglGetProcAddress('glColorFragmentOp2ATI');
    if not Assigned(glColorFragmentOp2ATI) then Exit;
    Pointer(glColorFragmentOp3ATI) := wglGetProcAddress('glColorFragmentOp3ATI');
    if not Assigned(glColorFragmentOp3ATI) then Exit;
    Pointer(glAlphaFragmentOp1ATI) := wglGetProcAddress('glAlphaFragmentOp1ATI');
    if not Assigned(glAlphaFragmentOp1ATI) then Exit;
    Pointer(glAlphaFragmentOp2ATI) := wglGetProcAddress('glAlphaFragmentOp2ATI');
    if not Assigned(glAlphaFragmentOp2ATI) then Exit;
    Pointer(glAlphaFragmentOp3ATI) := wglGetProcAddress('glAlphaFragmentOp3ATI');
    if not Assigned(glAlphaFragmentOp3ATI) then Exit;
    Pointer(glSetFragmentShaderConstantATI) := wglGetProcAddress('glSetFragmentShaderConstantATI');
    if not Assigned(glSetFragmentShaderConstantATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_pn_triangles(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_pn_triangles', extstring) then
  begin
    Pointer(glPNTrianglesiATI) := wglGetProcAddress('glPNTrianglesiATI');
    if not Assigned(glPNTrianglesiATI) then Exit;
    Pointer(glPNTrianglesfATI) := wglGetProcAddress('glPNTrianglesfATI');
    if not Assigned(glPNTrianglesfATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_vertex_array_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_vertex_array_object', extstring) then
  begin
    Pointer(glNewObjectBufferATI) := wglGetProcAddress('glNewObjectBufferATI');
    if not Assigned(glNewObjectBufferATI) then Exit;
    Pointer(glIsObjectBufferATI) := wglGetProcAddress('glIsObjectBufferATI');
    if not Assigned(glIsObjectBufferATI) then Exit;
    Pointer(glUpdateObjectBufferATI) := wglGetProcAddress('glUpdateObjectBufferATI');
    if not Assigned(glUpdateObjectBufferATI) then Exit;
    Pointer(glGetObjectBufferfvATI) := wglGetProcAddress('glGetObjectBufferfvATI');
    if not Assigned(glGetObjectBufferfvATI) then Exit;
    Pointer(glGetObjectBufferivATI) := wglGetProcAddress('glGetObjectBufferivATI');
    if not Assigned(glGetObjectBufferivATI) then Exit;
    Pointer(glFreeObjectBufferATI) := wglGetProcAddress('glFreeObjectBufferATI');
    if not Assigned(glFreeObjectBufferATI) then Exit;
    Pointer(glArrayObjectATI) := wglGetProcAddress('glArrayObjectATI');
    if not Assigned(glArrayObjectATI) then Exit;
    Pointer(glGetArrayObjectfvATI) := wglGetProcAddress('glGetArrayObjectfvATI');
    if not Assigned(glGetArrayObjectfvATI) then Exit;
    Pointer(glGetArrayObjectivATI) := wglGetProcAddress('glGetArrayObjectivATI');
    if not Assigned(glGetArrayObjectivATI) then Exit;
    Pointer(glVariantArrayObjectATI) := wglGetProcAddress('glVariantArrayObjectATI');
    if not Assigned(glVariantArrayObjectATI) then Exit;
    Pointer(glGetVariantArrayObjectfvATI) := wglGetProcAddress('glGetVariantArrayObjectfvATI');
    if not Assigned(glGetVariantArrayObjectfvATI) then Exit;
    Pointer(glGetVariantArrayObjectivATI) := wglGetProcAddress('glGetVariantArrayObjectivATI');
    if not Assigned(glGetVariantArrayObjectivATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_vertex_shader(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_vertex_shader', extstring) then
  begin
    Pointer(glBeginVertexShaderEXT) := wglGetProcAddress('glBeginVertexShaderEXT');
    if not Assigned(glBeginVertexShaderEXT) then Exit;
    Pointer(glEndVertexShaderEXT) := wglGetProcAddress('glEndVertexShaderEXT');
    if not Assigned(glEndVertexShaderEXT) then Exit;
    Pointer(glBindVertexShaderEXT) := wglGetProcAddress('glBindVertexShaderEXT');
    if not Assigned(glBindVertexShaderEXT) then Exit;
    Pointer(glGenVertexShadersEXT) := wglGetProcAddress('glGenVertexShadersEXT');
    if not Assigned(glGenVertexShadersEXT) then Exit;
    Pointer(glDeleteVertexShaderEXT) := wglGetProcAddress('glDeleteVertexShaderEXT');
    if not Assigned(glDeleteVertexShaderEXT) then Exit;
    Pointer(glShaderOp1EXT) := wglGetProcAddress('glShaderOp1EXT');
    if not Assigned(glShaderOp1EXT) then Exit;
    Pointer(glShaderOp2EXT) := wglGetProcAddress('glShaderOp2EXT');
    if not Assigned(glShaderOp2EXT) then Exit;
    Pointer(glShaderOp3EXT) := wglGetProcAddress('glShaderOp3EXT');
    if not Assigned(glShaderOp3EXT) then Exit;
    Pointer(glSwizzleEXT) := wglGetProcAddress('glSwizzleEXT');
    if not Assigned(glSwizzleEXT) then Exit;
    Pointer(glWriteMaskEXT) := wglGetProcAddress('glWriteMaskEXT');
    if not Assigned(glWriteMaskEXT) then Exit;
    Pointer(glInsertComponentEXT) := wglGetProcAddress('glInsertComponentEXT');
    if not Assigned(glInsertComponentEXT) then Exit;
    Pointer(glExtractComponentEXT) := wglGetProcAddress('glExtractComponentEXT');
    if not Assigned(glExtractComponentEXT) then Exit;
    Pointer(glGenSymbolsEXT) := wglGetProcAddress('glGenSymbolsEXT');
    if not Assigned(glGenSymbolsEXT) then Exit;
    Pointer(glSetInvariantEXT) := wglGetProcAddress('glSetInvariantEXT');
    if not Assigned(glSetInvariantEXT) then Exit;
    Pointer(glSetLocalConstantEXT) := wglGetProcAddress('glSetLocalConstantEXT');
    if not Assigned(glSetLocalConstantEXT) then Exit;
    Pointer(glVariantbvEXT) := wglGetProcAddress('glVariantbvEXT');
    if not Assigned(glVariantbvEXT) then Exit;
    Pointer(glVariantsvEXT) := wglGetProcAddress('glVariantsvEXT');
    if not Assigned(glVariantsvEXT) then Exit;
    Pointer(glVariantivEXT) := wglGetProcAddress('glVariantivEXT');
    if not Assigned(glVariantivEXT) then Exit;
    Pointer(glVariantfvEXT) := wglGetProcAddress('glVariantfvEXT');
    if not Assigned(glVariantfvEXT) then Exit;
    Pointer(glVariantdvEXT) := wglGetProcAddress('glVariantdvEXT');
    if not Assigned(glVariantdvEXT) then Exit;
    Pointer(glVariantubvEXT) := wglGetProcAddress('glVariantubvEXT');
    if not Assigned(glVariantubvEXT) then Exit;
    Pointer(glVariantusvEXT) := wglGetProcAddress('glVariantusvEXT');
    if not Assigned(glVariantusvEXT) then Exit;
    Pointer(glVariantuivEXT) := wglGetProcAddress('glVariantuivEXT');
    if not Assigned(glVariantuivEXT) then Exit;
    Pointer(glVariantPointerEXT) := wglGetProcAddress('glVariantPointerEXT');
    if not Assigned(glVariantPointerEXT) then Exit;
    Pointer(glEnableVariantClientStateEXT) := wglGetProcAddress('glEnableVariantClientStateEXT');
    if not Assigned(glEnableVariantClientStateEXT) then Exit;
    Pointer(glDisableVariantClientStateEXT) := wglGetProcAddress('glDisableVariantClientStateEXT');
    if not Assigned(glDisableVariantClientStateEXT) then Exit;
    Pointer(glBindLightParameterEXT) := wglGetProcAddress('glBindLightParameterEXT');
    if not Assigned(glBindLightParameterEXT) then Exit;
    Pointer(glBindMaterialParameterEXT) := wglGetProcAddress('glBindMaterialParameterEXT');
    if not Assigned(glBindMaterialParameterEXT) then Exit;
    Pointer(glBindTexGenParameterEXT) := wglGetProcAddress('glBindTexGenParameterEXT');
    if not Assigned(glBindTexGenParameterEXT) then Exit;
    Pointer(glBindTextureUnitParameterEXT) := wglGetProcAddress('glBindTextureUnitParameterEXT');
    if not Assigned(glBindTextureUnitParameterEXT) then Exit;
    Pointer(glBindParameterEXT) := wglGetProcAddress('glBindParameterEXT');
    if not Assigned(glBindParameterEXT) then Exit;
    Pointer(glIsVariantEnabledEXT) := wglGetProcAddress('glIsVariantEnabledEXT');
    if not Assigned(glIsVariantEnabledEXT) then Exit;
    Pointer(glGetVariantBooleanvEXT) := wglGetProcAddress('glGetVariantBooleanvEXT');
    if not Assigned(glGetVariantBooleanvEXT) then Exit;
    Pointer(glGetVariantIntegervEXT) := wglGetProcAddress('glGetVariantIntegervEXT');
    if not Assigned(glGetVariantIntegervEXT) then Exit;
    Pointer(glGetVariantFloatvEXT) := wglGetProcAddress('glGetVariantFloatvEXT');
    if not Assigned(glGetVariantFloatvEXT) then Exit;
    Pointer(glGetVariantPointervEXT) := wglGetProcAddress('glGetVariantPointervEXT');
    if not Assigned(glGetVariantPointervEXT) then Exit;
    Pointer(glGetInvariantBooleanvEXT) := wglGetProcAddress('glGetInvariantBooleanvEXT');
    if not Assigned(glGetInvariantBooleanvEXT) then Exit;
    Pointer(glGetInvariantIntegervEXT) := wglGetProcAddress('glGetInvariantIntegervEXT');
    if not Assigned(glGetInvariantIntegervEXT) then Exit;
    Pointer(glGetInvariantFloatvEXT) := wglGetProcAddress('glGetInvariantFloatvEXT');
    if not Assigned(glGetInvariantFloatvEXT) then Exit;
    Pointer(glGetLocalConstantBooleanvEXT) := wglGetProcAddress('glGetLocalConstantBooleanvEXT');
    if not Assigned(glGetLocalConstantBooleanvEXT) then Exit;
    Pointer(glGetLocalConstantIntegervEXT) := wglGetProcAddress('glGetLocalConstantIntegervEXT');
    if not Assigned(glGetLocalConstantIntegervEXT) then Exit;
    Pointer(glGetLocalConstantFloatvEXT) := wglGetProcAddress('glGetLocalConstantFloatvEXT');
    if not Assigned(glGetLocalConstantFloatvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_vertex_streams(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_vertex_streams', extstring) then
  begin
    Pointer(glVertexStream1sATI) := wglGetProcAddress('glVertexStream1sATI');
    if not Assigned(glVertexStream1sATI) then Exit;
    Pointer(glVertexStream1svATI) := wglGetProcAddress('glVertexStream1svATI');
    if not Assigned(glVertexStream1svATI) then Exit;
    Pointer(glVertexStream1iATI) := wglGetProcAddress('glVertexStream1iATI');
    if not Assigned(glVertexStream1iATI) then Exit;
    Pointer(glVertexStream1ivATI) := wglGetProcAddress('glVertexStream1ivATI');
    if not Assigned(glVertexStream1ivATI) then Exit;
    Pointer(glVertexStream1fATI) := wglGetProcAddress('glVertexStream1fATI');
    if not Assigned(glVertexStream1fATI) then Exit;
    Pointer(glVertexStream1fvATI) := wglGetProcAddress('glVertexStream1fvATI');
    if not Assigned(glVertexStream1fvATI) then Exit;
    Pointer(glVertexStream1dATI) := wglGetProcAddress('glVertexStream1dATI');
    if not Assigned(glVertexStream1dATI) then Exit;
    Pointer(glVertexStream1dvATI) := wglGetProcAddress('glVertexStream1dvATI');
    if not Assigned(glVertexStream1dvATI) then Exit;
    Pointer(glVertexStream2sATI) := wglGetProcAddress('glVertexStream2sATI');
    if not Assigned(glVertexStream2sATI) then Exit;
    Pointer(glVertexStream2svATI) := wglGetProcAddress('glVertexStream2svATI');
    if not Assigned(glVertexStream2svATI) then Exit;
    Pointer(glVertexStream2iATI) := wglGetProcAddress('glVertexStream2iATI');
    if not Assigned(glVertexStream2iATI) then Exit;
    Pointer(glVertexStream2ivATI) := wglGetProcAddress('glVertexStream2ivATI');
    if not Assigned(glVertexStream2ivATI) then Exit;
    Pointer(glVertexStream2fATI) := wglGetProcAddress('glVertexStream2fATI');
    if not Assigned(glVertexStream2fATI) then Exit;
    Pointer(glVertexStream2fvATI) := wglGetProcAddress('glVertexStream2fvATI');
    if not Assigned(glVertexStream2fvATI) then Exit;
    Pointer(glVertexStream2dATI) := wglGetProcAddress('glVertexStream2dATI');
    if not Assigned(glVertexStream2dATI) then Exit;
    Pointer(glVertexStream2dvATI) := wglGetProcAddress('glVertexStream2dvATI');
    if not Assigned(glVertexStream2dvATI) then Exit;
    Pointer(glVertexStream3sATI) := wglGetProcAddress('glVertexStream3sATI');
    if not Assigned(glVertexStream3sATI) then Exit;
    Pointer(glVertexStream3svATI) := wglGetProcAddress('glVertexStream3svATI');
    if not Assigned(glVertexStream3svATI) then Exit;
    Pointer(glVertexStream3iATI) := wglGetProcAddress('glVertexStream3iATI');
    if not Assigned(glVertexStream3iATI) then Exit;
    Pointer(glVertexStream3ivATI) := wglGetProcAddress('glVertexStream3ivATI');
    if not Assigned(glVertexStream3ivATI) then Exit;
    Pointer(glVertexStream3fATI) := wglGetProcAddress('glVertexStream3fATI');
    if not Assigned(glVertexStream3fATI) then Exit;
    Pointer(glVertexStream3fvATI) := wglGetProcAddress('glVertexStream3fvATI');
    if not Assigned(glVertexStream3fvATI) then Exit;
    Pointer(glVertexStream3dATI) := wglGetProcAddress('glVertexStream3dATI');
    if not Assigned(glVertexStream3dATI) then Exit;
    Pointer(glVertexStream3dvATI) := wglGetProcAddress('glVertexStream3dvATI');
    if not Assigned(glVertexStream3dvATI) then Exit;
    Pointer(glVertexStream4sATI) := wglGetProcAddress('glVertexStream4sATI');
    if not Assigned(glVertexStream4sATI) then Exit;
    Pointer(glVertexStream4svATI) := wglGetProcAddress('glVertexStream4svATI');
    if not Assigned(glVertexStream4svATI) then Exit;
    Pointer(glVertexStream4iATI) := wglGetProcAddress('glVertexStream4iATI');
    if not Assigned(glVertexStream4iATI) then Exit;
    Pointer(glVertexStream4ivATI) := wglGetProcAddress('glVertexStream4ivATI');
    if not Assigned(glVertexStream4ivATI) then Exit;
    Pointer(glVertexStream4fATI) := wglGetProcAddress('glVertexStream4fATI');
    if not Assigned(glVertexStream4fATI) then Exit;
    Pointer(glVertexStream4fvATI) := wglGetProcAddress('glVertexStream4fvATI');
    if not Assigned(glVertexStream4fvATI) then Exit;
    Pointer(glVertexStream4dATI) := wglGetProcAddress('glVertexStream4dATI');
    if not Assigned(glVertexStream4dATI) then Exit;
    Pointer(glVertexStream4dvATI) := wglGetProcAddress('glVertexStream4dvATI');
    if not Assigned(glVertexStream4dvATI) then Exit;
    Pointer(glNormalStream3bATI) := wglGetProcAddress('glNormalStream3bATI');
    if not Assigned(glNormalStream3bATI) then Exit;
    Pointer(glNormalStream3bvATI) := wglGetProcAddress('glNormalStream3bvATI');
    if not Assigned(glNormalStream3bvATI) then Exit;
    Pointer(glNormalStream3sATI) := wglGetProcAddress('glNormalStream3sATI');
    if not Assigned(glNormalStream3sATI) then Exit;
    Pointer(glNormalStream3svATI) := wglGetProcAddress('glNormalStream3svATI');
    if not Assigned(glNormalStream3svATI) then Exit;
    Pointer(glNormalStream3iATI) := wglGetProcAddress('glNormalStream3iATI');
    if not Assigned(glNormalStream3iATI) then Exit;
    Pointer(glNormalStream3ivATI) := wglGetProcAddress('glNormalStream3ivATI');
    if not Assigned(glNormalStream3ivATI) then Exit;
    Pointer(glNormalStream3fATI) := wglGetProcAddress('glNormalStream3fATI');
    if not Assigned(glNormalStream3fATI) then Exit;
    Pointer(glNormalStream3fvATI) := wglGetProcAddress('glNormalStream3fvATI');
    if not Assigned(glNormalStream3fvATI) then Exit;
    Pointer(glNormalStream3dATI) := wglGetProcAddress('glNormalStream3dATI');
    if not Assigned(glNormalStream3dATI) then Exit;
    Pointer(glNormalStream3dvATI) := wglGetProcAddress('glNormalStream3dvATI');
    if not Assigned(glNormalStream3dvATI) then Exit;
    Pointer(glClientActiveVertexStreamATI) := wglGetProcAddress('glClientActiveVertexStreamATI');
    if not Assigned(glClientActiveVertexStreamATI) then Exit;
    Pointer(glVertexBlendEnviATI) := wglGetProcAddress('glVertexBlendEnviATI');
    if not Assigned(glVertexBlendEnviATI) then Exit;
    Pointer(glVertexBlendEnvfATI) := wglGetProcAddress('glVertexBlendEnvfATI');
    if not Assigned(glVertexBlendEnvfATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_element_array(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_element_array', extstring) then
  begin
    Pointer(glElementPointerATI) := wglGetProcAddress('glElementPointerATI');
    if not Assigned(glElementPointerATI) then Exit;
    Pointer(glDrawElementArrayATI) := wglGetProcAddress('glDrawElementArrayATI');
    if not Assigned(glDrawElementArrayATI) then Exit;
    Pointer(glDrawRangeElementArrayATI) := wglGetProcAddress('glDrawRangeElementArrayATI');
    if not Assigned(glDrawRangeElementArrayATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_SUN_mesh_array(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SUN_mesh_array', extstring) then
  begin
    Pointer(glDrawMeshArraysSUN) := wglGetProcAddress('glDrawMeshArraysSUN');
    if not Assigned(glDrawMeshArraysSUN) then Exit;
    Result := True;
  end;
end;

function Load_GL_SUN_slice_accum(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SUN_slice_accum', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_multisample_filter_hint(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_multisample_filter_hint', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_depth_clamp(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_depth_clamp', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_occlusion_query(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_occlusion_query', extstring) then
  begin
    Pointer(glGenOcclusionQueriesNV) := wglGetProcAddress('glGenOcclusionQueriesNV');
    if not Assigned(glGenOcclusionQueriesNV) then Exit;
    Pointer(glDeleteOcclusionQueriesNV) := wglGetProcAddress('glDeleteOcclusionQueriesNV');
    if not Assigned(glDeleteOcclusionQueriesNV) then Exit;
    Pointer(glIsOcclusionQueryNV) := wglGetProcAddress('glIsOcclusionQueryNV');
    if not Assigned(glIsOcclusionQueryNV) then Exit;
    Pointer(glBeginOcclusionQueryNV) := wglGetProcAddress('glBeginOcclusionQueryNV');
    if not Assigned(glBeginOcclusionQueryNV) then Exit;
    Pointer(glEndOcclusionQueryNV) := wglGetProcAddress('glEndOcclusionQueryNV');
    if not Assigned(glEndOcclusionQueryNV) then Exit;
    Pointer(glGetOcclusionQueryivNV) := wglGetProcAddress('glGetOcclusionQueryivNV');
    if not Assigned(glGetOcclusionQueryivNV) then Exit;
    Pointer(glGetOcclusionQueryuivNV) := wglGetProcAddress('glGetOcclusionQueryuivNV');
    if not Assigned(glGetOcclusionQueryuivNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_point_sprite(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_point_sprite', extstring) then
  begin
    Pointer(glPointParameteriNV) := wglGetProcAddress('glPointParameteriNV');
    if not Assigned(glPointParameteriNV) then Exit;
    Pointer(glPointParameterivNV) := wglGetProcAddress('glPointParameterivNV');
    if not Assigned(glPointParameterivNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_texture_shader3(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_shader3', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_vertex_program1_1(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program1_1', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_shadow_funcs(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_shadow_funcs', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_stencil_two_side(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_stencil_two_side', extstring) then
  begin
    Pointer(glActiveStencilFaceEXT) := wglGetProcAddress('glActiveStencilFaceEXT');
    if not Assigned(glActiveStencilFaceEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_text_fragment_shader(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_text_fragment_shader', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_APPLE_client_storage(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_client_storage', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_APPLE_element_array(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_element_array', extstring) then
  begin
    Pointer(glElementPointerAPPLE) := wglGetProcAddress('glElementPointerAPPLE');
    if not Assigned(glElementPointerAPPLE) then Exit;
    Pointer(glDrawElementArrayAPPLE) := wglGetProcAddress('glDrawElementArrayAPPLE');
    if not Assigned(glDrawElementArrayAPPLE) then Exit;
    Pointer(glDrawRangeElementArrayAPPLE) := wglGetProcAddress('glDrawRangeElementArrayAPPLE');
    if not Assigned(glDrawRangeElementArrayAPPLE) then Exit;
    Pointer(glMultiDrawElementArrayAPPLE) := wglGetProcAddress('glMultiDrawElementArrayAPPLE');
    if not Assigned(glMultiDrawElementArrayAPPLE) then Exit;
    Pointer(glMultiDrawRangeElementArrayAPPLE) := wglGetProcAddress('glMultiDrawRangeElementArrayAPPLE');
    if not Assigned(glMultiDrawRangeElementArrayAPPLE) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_fence(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_fence', extstring) then
  begin
    Pointer(glGenFencesAPPLE) := wglGetProcAddress('glGenFencesAPPLE');
    if not Assigned(glGenFencesAPPLE) then Exit;
    Pointer(glDeleteFencesAPPLE) := wglGetProcAddress('glDeleteFencesAPPLE');
    if not Assigned(glDeleteFencesAPPLE) then Exit;
    Pointer(glSetFenceAPPLE) := wglGetProcAddress('glSetFenceAPPLE');
    if not Assigned(glSetFenceAPPLE) then Exit;
    Pointer(glIsFenceAPPLE) := wglGetProcAddress('glIsFenceAPPLE');
    if not Assigned(glIsFenceAPPLE) then Exit;
    Pointer(glTestFenceAPPLE) := wglGetProcAddress('glTestFenceAPPLE');
    if not Assigned(glTestFenceAPPLE) then Exit;
    Pointer(glFinishFenceAPPLE) := wglGetProcAddress('glFinishFenceAPPLE');
    if not Assigned(glFinishFenceAPPLE) then Exit;
    Pointer(glTestObjectAPPLE) := wglGetProcAddress('glTestObjectAPPLE');
    if not Assigned(glTestObjectAPPLE) then Exit;
    Pointer(glFinishObjectAPPLE) := wglGetProcAddress('glFinishObjectAPPLE');
    if not Assigned(glFinishObjectAPPLE) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_vertex_array_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_vertex_array_object', extstring) then
  begin
    Pointer(glBindVertexArrayAPPLE) := wglGetProcAddress('glBindVertexArrayAPPLE');
    if not Assigned(glBindVertexArrayAPPLE) then Exit;
    Pointer(glDeleteVertexArraysAPPLE) := wglGetProcAddress('glDeleteVertexArraysAPPLE');
    if not Assigned(glDeleteVertexArraysAPPLE) then Exit;
    Pointer(glGenVertexArraysAPPLE) := wglGetProcAddress('glGenVertexArraysAPPLE');
    if not Assigned(glGenVertexArraysAPPLE) then Exit;
    Pointer(glIsVertexArrayAPPLE) := wglGetProcAddress('glIsVertexArrayAPPLE');
    if not Assigned(glIsVertexArrayAPPLE) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_vertex_array_range(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_vertex_array_range', extstring) then
  begin
    Pointer(glVertexArrayRangeAPPLE) := wglGetProcAddress('glVertexArrayRangeAPPLE');
    if not Assigned(glVertexArrayRangeAPPLE) then Exit;
    Pointer(glFlushVertexArrayRangeAPPLE) := wglGetProcAddress('glFlushVertexArrayRangeAPPLE');
    if not Assigned(glFlushVertexArrayRangeAPPLE) then Exit;
    Pointer(glVertexArrayParameteriAPPLE) := wglGetProcAddress('glVertexArrayParameteriAPPLE');
    if not Assigned(glVertexArrayParameteriAPPLE) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_ycbcr_422(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_ycbcr_422', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_S3_s3tc(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_S3_s3tc', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ATI_draw_buffers(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_draw_buffers', extstring) then
  begin
    Pointer(glDrawBuffersATI) := wglGetProcAddress('glDrawBuffersATI');
    if not Assigned(glDrawBuffersATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_pixel_format_float(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_pixel_format_float', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ATI_texture_env_combine3(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_texture_env_combine3', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ATI_texture_float(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_texture_float', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_float_buffer(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_float_buffer', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_fragment_program(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fragment_program', extstring) then
  begin
    Pointer(glProgramNamedParameter4fNV) := wglGetProcAddress('glProgramNamedParameter4fNV');
    if not Assigned(glProgramNamedParameter4fNV) then Exit;
    Pointer(glProgramNamedParameter4dNV) := wglGetProcAddress('glProgramNamedParameter4dNV');
    if not Assigned(glProgramNamedParameter4dNV) then Exit;
    Pointer(glProgramNamedParameter4fvNV) := wglGetProcAddress('glProgramNamedParameter4fvNV');
    if not Assigned(glProgramNamedParameter4fvNV) then Exit;
    Pointer(glProgramNamedParameter4dvNV) := wglGetProcAddress('glProgramNamedParameter4dvNV');
    if not Assigned(glProgramNamedParameter4dvNV) then Exit;
    Pointer(glGetProgramNamedParameterfvNV) := wglGetProcAddress('glGetProgramNamedParameterfvNV');
    if not Assigned(glGetProgramNamedParameterfvNV) then Exit;
    Pointer(glGetProgramNamedParameterdvNV) := wglGetProcAddress('glGetProgramNamedParameterdvNV');
    if not Assigned(glGetProgramNamedParameterdvNV) then Exit;
    (* Shared entry points *)
    Pointer(glProgramLocalParameter4dARB) := wglGetProcAddress('glProgramLocalParameter4dARB');
    if not Assigned(glProgramLocalParameter4dARB) then Exit;
    Pointer(glProgramLocalParameter4dvARB) := wglGetProcAddress('glProgramLocalParameter4dvARB');
    if not Assigned(glProgramLocalParameter4dvARB) then Exit;
    Pointer(glProgramLocalParameter4fARB) := wglGetProcAddress('glProgramLocalParameter4fARB');
    if not Assigned(glProgramLocalParameter4fARB) then Exit;
    Pointer(glProgramLocalParameter4fvARB) := wglGetProcAddress('glProgramLocalParameter4fvARB');
    if not Assigned(glProgramLocalParameter4fvARB) then Exit;
    Pointer(glGetProgramLocalParameterdvARB) := wglGetProcAddress('glGetProgramLocalParameterdvARB');
    if not Assigned(glGetProgramLocalParameterdvARB) then Exit;
    Pointer(glGetProgramLocalParameterfvARB) := wglGetProcAddress('glGetProgramLocalParameterfvARB');
    if not Assigned(glGetProgramLocalParameterfvARB) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_half_float(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_half_float', extstring) then
  begin
    Pointer(glVertex2hNV) := wglGetProcAddress('glVertex2hNV');
    if not Assigned(glVertex2hNV) then Exit;
    Pointer(glVertex2hvNV) := wglGetProcAddress('glVertex2hvNV');
    if not Assigned(glVertex2hvNV) then Exit;
    Pointer(glVertex3hNV) := wglGetProcAddress('glVertex3hNV');
    if not Assigned(glVertex3hNV) then Exit;
    Pointer(glVertex3hvNV) := wglGetProcAddress('glVertex3hvNV');
    if not Assigned(glVertex3hvNV) then Exit;
    Pointer(glVertex4hNV) := wglGetProcAddress('glVertex4hNV');
    if not Assigned(glVertex4hNV) then Exit;
    Pointer(glVertex4hvNV) := wglGetProcAddress('glVertex4hvNV');
    if not Assigned(glVertex4hvNV) then Exit;
    Pointer(glNormal3hNV) := wglGetProcAddress('glNormal3hNV');
    if not Assigned(glNormal3hNV) then Exit;
    Pointer(glNormal3hvNV) := wglGetProcAddress('glNormal3hvNV');
    if not Assigned(glNormal3hvNV) then Exit;
    Pointer(glColor3hNV) := wglGetProcAddress('glColor3hNV');
    if not Assigned(glColor3hNV) then Exit;
    Pointer(glColor3hvNV) := wglGetProcAddress('glColor3hvNV');
    if not Assigned(glColor3hvNV) then Exit;
    Pointer(glColor4hNV) := wglGetProcAddress('glColor4hNV');
    if not Assigned(glColor4hNV) then Exit;
    Pointer(glColor4hvNV) := wglGetProcAddress('glColor4hvNV');
    if not Assigned(glColor4hvNV) then Exit;
    Pointer(glTexCoord1hNV) := wglGetProcAddress('glTexCoord1hNV');
    if not Assigned(glTexCoord1hNV) then Exit;
    Pointer(glTexCoord1hvNV) := wglGetProcAddress('glTexCoord1hvNV');
    if not Assigned(glTexCoord1hvNV) then Exit;
    Pointer(glTexCoord2hNV) := wglGetProcAddress('glTexCoord2hNV');
    if not Assigned(glTexCoord2hNV) then Exit;
    Pointer(glTexCoord2hvNV) := wglGetProcAddress('glTexCoord2hvNV');
    if not Assigned(glTexCoord2hvNV) then Exit;
    Pointer(glTexCoord3hNV) := wglGetProcAddress('glTexCoord3hNV');
    if not Assigned(glTexCoord3hNV) then Exit;
    Pointer(glTexCoord3hvNV) := wglGetProcAddress('glTexCoord3hvNV');
    if not Assigned(glTexCoord3hvNV) then Exit;
    Pointer(glTexCoord4hNV) := wglGetProcAddress('glTexCoord4hNV');
    if not Assigned(glTexCoord4hNV) then Exit;
    Pointer(glTexCoord4hvNV) := wglGetProcAddress('glTexCoord4hvNV');
    if not Assigned(glTexCoord4hvNV) then Exit;
    Pointer(glMultiTexCoord1hNV) := wglGetProcAddress('glMultiTexCoord1hNV');
    if not Assigned(glMultiTexCoord1hNV) then Exit;
    Pointer(glMultiTexCoord1hvNV) := wglGetProcAddress('glMultiTexCoord1hvNV');
    if not Assigned(glMultiTexCoord1hvNV) then Exit;
    Pointer(glMultiTexCoord2hNV) := wglGetProcAddress('glMultiTexCoord2hNV');
    if not Assigned(glMultiTexCoord2hNV) then Exit;
    Pointer(glMultiTexCoord2hvNV) := wglGetProcAddress('glMultiTexCoord2hvNV');
    if not Assigned(glMultiTexCoord2hvNV) then Exit;
    Pointer(glMultiTexCoord3hNV) := wglGetProcAddress('glMultiTexCoord3hNV');
    if not Assigned(glMultiTexCoord3hNV) then Exit;
    Pointer(glMultiTexCoord3hvNV) := wglGetProcAddress('glMultiTexCoord3hvNV');
    if not Assigned(glMultiTexCoord3hvNV) then Exit;
    Pointer(glMultiTexCoord4hNV) := wglGetProcAddress('glMultiTexCoord4hNV');
    if not Assigned(glMultiTexCoord4hNV) then Exit;
    Pointer(glMultiTexCoord4hvNV) := wglGetProcAddress('glMultiTexCoord4hvNV');
    if not Assigned(glMultiTexCoord4hvNV) then Exit;
    Pointer(glFogCoordhNV) := wglGetProcAddress('glFogCoordhNV');
    if not Assigned(glFogCoordhNV) then Exit;
    Pointer(glFogCoordhvNV) := wglGetProcAddress('glFogCoordhvNV');
    if not Assigned(glFogCoordhvNV) then Exit;
    Pointer(glSecondaryColor3hNV) := wglGetProcAddress('glSecondaryColor3hNV');
    if not Assigned(glSecondaryColor3hNV) then Exit;
    Pointer(glSecondaryColor3hvNV) := wglGetProcAddress('glSecondaryColor3hvNV');
    if not Assigned(glSecondaryColor3hvNV) then Exit;
    Pointer(glVertexWeighthNV) := wglGetProcAddress('glVertexWeighthNV');
    if not Assigned(glVertexWeighthNV) then Exit;
    Pointer(glVertexWeighthvNV) := wglGetProcAddress('glVertexWeighthvNV');
    if not Assigned(glVertexWeighthvNV) then Exit;
    Pointer(glVertexAttrib1hNV) := wglGetProcAddress('glVertexAttrib1hNV');
    if not Assigned(glVertexAttrib1hNV) then Exit;
    Pointer(glVertexAttrib1hvNV) := wglGetProcAddress('glVertexAttrib1hvNV');
    if not Assigned(glVertexAttrib1hvNV) then Exit;
    Pointer(glVertexAttrib2hNV) := wglGetProcAddress('glVertexAttrib2hNV');
    if not Assigned(glVertexAttrib2hNV) then Exit;
    Pointer(glVertexAttrib2hvNV) := wglGetProcAddress('glVertexAttrib2hvNV');
    if not Assigned(glVertexAttrib2hvNV) then Exit;
    Pointer(glVertexAttrib3hNV) := wglGetProcAddress('glVertexAttrib3hNV');
    if not Assigned(glVertexAttrib3hNV) then Exit;
    Pointer(glVertexAttrib3hvNV) := wglGetProcAddress('glVertexAttrib3hvNV');
    if not Assigned(glVertexAttrib3hvNV) then Exit;
    Pointer(glVertexAttrib4hNV) := wglGetProcAddress('glVertexAttrib4hNV');
    if not Assigned(glVertexAttrib4hNV) then Exit;
    Pointer(glVertexAttrib4hvNV) := wglGetProcAddress('glVertexAttrib4hvNV');
    if not Assigned(glVertexAttrib4hvNV) then Exit;
    Pointer(glVertexAttribs1hvNV) := wglGetProcAddress('glVertexAttribs1hvNV');
    if not Assigned(glVertexAttribs1hvNV) then Exit;
    Pointer(glVertexAttribs2hvNV) := wglGetProcAddress('glVertexAttribs2hvNV');
    if not Assigned(glVertexAttribs2hvNV) then Exit;
    Pointer(glVertexAttribs3hvNV) := wglGetProcAddress('glVertexAttribs3hvNV');
    if not Assigned(glVertexAttribs3hvNV) then Exit;
    Pointer(glVertexAttribs4hvNV) := wglGetProcAddress('glVertexAttribs4hvNV');
    if not Assigned(glVertexAttribs4hvNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_pixel_data_range(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_pixel_data_range', extstring) then
  begin
    Pointer(glPixelDataRangeNV) := wglGetProcAddress('glPixelDataRangeNV');
    if not Assigned(glPixelDataRangeNV) then Exit;
    Pointer(glFlushPixelDataRangeNV) := wglGetProcAddress('glFlushPixelDataRangeNV');
    if not Assigned(glFlushPixelDataRangeNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_primitive_restart(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_primitive_restart', extstring) then
  begin
    Pointer(glPrimitiveRestartNV) := wglGetProcAddress('glPrimitiveRestartNV');
    if not Assigned(glPrimitiveRestartNV) then Exit;
    Pointer(glPrimitiveRestartIndexNV) := wglGetProcAddress('glPrimitiveRestartIndexNV');
    if not Assigned(glPrimitiveRestartIndexNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_texture_expand_normal(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_expand_normal', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_vertex_program2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program2', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_ATI_map_object_buffer(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_map_object_buffer', extstring) then
  begin
    Pointer(glMapObjectBufferATI) := wglGetProcAddress('glMapObjectBufferATI');
    if not Assigned(glMapObjectBufferATI) then Exit;
    Pointer(glUnmapObjectBufferATI) := wglGetProcAddress('glUnmapObjectBufferATI');
    if not Assigned(glUnmapObjectBufferATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_separate_stencil(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_separate_stencil', extstring) then
  begin
    Pointer(glStencilOpSeparateATI) := wglGetProcAddress('glStencilOpSeparateATI');
    if not Assigned(glStencilOpSeparateATI) then Exit;
    Pointer(glStencilFuncSeparateATI) := wglGetProcAddress('glStencilFuncSeparateATI');
    if not Assigned(glStencilFuncSeparateATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_vertex_attrib_array_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_vertex_attrib_array_object', extstring) then
  begin
    Pointer(glVertexAttribArrayObjectATI) := wglGetProcAddress('glVertexAttribArrayObjectATI');
    if not Assigned(glVertexAttribArrayObjectATI) then Exit;
    Pointer(glGetVertexAttribArrayObjectfvATI) := wglGetProcAddress('glGetVertexAttribArrayObjectfvATI');
    if not Assigned(glGetVertexAttribArrayObjectfvATI) then Exit;
    Pointer(glGetVertexAttribArrayObjectivATI) := wglGetProcAddress('glGetVertexAttribArrayObjectivATI');
    if not Assigned(glGetVertexAttribArrayObjectivATI) then Exit;
    Result := True;
  end;
end;

function Load_GL_OES_read_format(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_OES_read_format', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_depth_bounds_test(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_depth_bounds_test', extstring) then
  begin
    Pointer(glDepthBoundsEXT) := wglGetProcAddress('glDepthBoundsEXT');
    if not Assigned(glDepthBoundsEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_mirror_clamp(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_mirror_clamp', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_blend_equation_separate(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_equation_separate', extstring) then
  begin
    Pointer(glBlendEquationSeparateEXT) := wglGetProcAddress('glBlendEquationSeparateEXT');
    if not Assigned(glBlendEquationSeparateEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_MESA_pack_invert(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESA_pack_invert', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_MESA_ycbcr_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESA_ycbcr_texture', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_pixel_buffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_pixel_buffer_object', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_fragment_program_option(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fragment_program_option', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_fragment_program2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fragment_program2', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_vertex_program2_option(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program2_option', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_vertex_program3(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program3', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_framebuffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_framebuffer_object', extstring) then
  begin
    Pointer(glIsRenderbufferEXT) := wglGetProcAddress('glIsRenderbufferEXT');
    if not Assigned(glIsRenderbufferEXT) then Exit;
    Pointer(glBindRenderbufferEXT) := wglGetProcAddress('glBindRenderbufferEXT');
    if not Assigned(glBindRenderbufferEXT) then Exit;
    Pointer(glDeleteRenderbuffersEXT) := wglGetProcAddress('glDeleteRenderbuffersEXT');
    if not Assigned(glDeleteRenderbuffersEXT) then Exit;
    Pointer(glGenRenderbuffersEXT) := wglGetProcAddress('glGenRenderbuffersEXT');
    if not Assigned(glGenRenderbuffersEXT) then Exit;
    Pointer(glRenderbufferStorageEXT) := wglGetProcAddress('glRenderbufferStorageEXT');
    if not Assigned(glRenderbufferStorageEXT) then Exit;
    Pointer(glGetRenderbufferParameterivEXT) := wglGetProcAddress('glGetRenderbufferParameterivEXT');
    if not Assigned(glGetRenderbufferParameterivEXT) then Exit;
    Pointer(glIsFramebufferEXT) := wglGetProcAddress('glIsFramebufferEXT');
    if not Assigned(glIsFramebufferEXT) then Exit;
    Pointer(glBindFramebufferEXT) := wglGetProcAddress('glBindFramebufferEXT');
    if not Assigned(glBindFramebufferEXT) then Exit;
    Pointer(glDeleteFramebuffersEXT) := wglGetProcAddress('glDeleteFramebuffersEXT');
    if not Assigned(glDeleteFramebuffersEXT) then Exit;
    Pointer(glGenFramebuffersEXT) := wglGetProcAddress('glGenFramebuffersEXT');
    if not Assigned(glGenFramebuffersEXT) then Exit;
    Pointer(glCheckFramebufferStatusEXT) := wglGetProcAddress('glCheckFramebufferStatusEXT');
    if not Assigned(glCheckFramebufferStatusEXT) then Exit;
    Pointer(glFramebufferTexture1DEXT) := wglGetProcAddress('glFramebufferTexture1DEXT');
    if not Assigned(glFramebufferTexture1DEXT) then Exit;
    Pointer(glFramebufferTexture2DEXT) := wglGetProcAddress('glFramebufferTexture2DEXT');
    if not Assigned(glFramebufferTexture2DEXT) then Exit;
    Pointer(glFramebufferTexture3DEXT) := wglGetProcAddress('glFramebufferTexture3DEXT');
    if not Assigned(glFramebufferTexture3DEXT) then Exit;
    Pointer(glFramebufferRenderbufferEXT) := wglGetProcAddress('glFramebufferRenderbufferEXT');
    if not Assigned(glFramebufferRenderbufferEXT) then Exit;
    Pointer(glGetFramebufferAttachmentParameterivEXT) := wglGetProcAddress('glGetFramebufferAttachmentParameterivEXT');
    if not Assigned(glGetFramebufferAttachmentParameterivEXT) then Exit;
    Pointer(glGenerateMipmapEXT) := wglGetProcAddress('glGenerateMipmapEXT');
    if not Assigned(glGenerateMipmapEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_GREMEDY_string_marker(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_GREMEDY_string_marker', extstring) then
  begin
    Pointer(glStringMarkerGREMEDY) := wglGetProcAddress('glStringMarkerGREMEDY');
    if not Assigned(glStringMarkerGREMEDY) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_packed_depth_stencil(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_packed_depth_stencil', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_stencil_clear_tag(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_stencil_clear_tag', extstring) then
  begin
    Pointer(glStencilClearTagEXT) := wglGetProcAddress('glStencilClearTagEXT');
    if not Assigned(glStencilClearTagEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_sRGB(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_sRGB', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_framebuffer_blit(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_framebuffer_blit', extstring) then
  begin
    Pointer(glBlitFramebufferEXT) := wglGetProcAddress('glBlitFramebufferEXT');
    if not Assigned(glBlitFramebufferEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_framebuffer_multisample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_framebuffer_multisample', extstring) then
  begin
    Pointer(glRenderbufferStorageMultisampleEXT) := wglGetProcAddress('glRenderbufferStorageMultisampleEXT');
    if not Assigned(glRenderbufferStorageMultisampleEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_MESAX_texture_stack(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESAX_texture_stack', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_timer_query(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_timer_query', extstring) then
  begin
    Pointer(glGetQueryObjecti64vEXT) := wglGetProcAddress('glGetQueryObjecti64vEXT');
    if not Assigned(glGetQueryObjecti64vEXT) then Exit;
    Pointer(glGetQueryObjectui64vEXT) := wglGetProcAddress('glGetQueryObjectui64vEXT');
    if not Assigned(glGetQueryObjectui64vEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_gpu_program_parameters(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_gpu_program_parameters', extstring) then
  begin
    Pointer(glProgramEnvParameters4fvEXT) := wglGetProcAddress('glProgramEnvParameters4fvEXT');
    if not Assigned(glProgramEnvParameters4fvEXT) then Exit;
    Pointer(glProgramLocalParameters4fvEXT) := wglGetProcAddress('glProgramLocalParameters4fvEXT');
    if not Assigned(glProgramLocalParameters4fvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_flush_buffer_range(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_flush_buffer_range', extstring) then
  begin
    Pointer(glBufferParameteriAPPLE) := wglGetProcAddress('glBufferParameteriAPPLE');
    if not Assigned(glBufferParameteriAPPLE) then Exit;
    Pointer(glFlushMappedBufferRangeAPPLE) := wglGetProcAddress('glFlushMappedBufferRangeAPPLE');
    if not Assigned(glFlushMappedBufferRangeAPPLE) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_gpu_program4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_gpu_program4', extstring) then
  begin
    Pointer(glProgramLocalParameterI4iNV) := wglGetProcAddress('glProgramLocalParameterI4iNV');
    if not Assigned(glProgramLocalParameterI4iNV) then Exit;
    Pointer(glProgramLocalParameterI4ivNV) := wglGetProcAddress('glProgramLocalParameterI4ivNV');
    if not Assigned(glProgramLocalParameterI4ivNV) then Exit;
    Pointer(glProgramLocalParametersI4ivNV) := wglGetProcAddress('glProgramLocalParametersI4ivNV');
    if not Assigned(glProgramLocalParametersI4ivNV) then Exit;
    Pointer(glProgramLocalParameterI4uiNV) := wglGetProcAddress('glProgramLocalParameterI4uiNV');
    if not Assigned(glProgramLocalParameterI4uiNV) then Exit;
    Pointer(glProgramLocalParameterI4uivNV) := wglGetProcAddress('glProgramLocalParameterI4uivNV');
    if not Assigned(glProgramLocalParameterI4uivNV) then Exit;
    Pointer(glProgramLocalParametersI4uivNV) := wglGetProcAddress('glProgramLocalParametersI4uivNV');
    if not Assigned(glProgramLocalParametersI4uivNV) then Exit;
    Pointer(glProgramEnvParameterI4iNV) := wglGetProcAddress('glProgramEnvParameterI4iNV');
    if not Assigned(glProgramEnvParameterI4iNV) then Exit;
    Pointer(glProgramEnvParameterI4ivNV) := wglGetProcAddress('glProgramEnvParameterI4ivNV');
    if not Assigned(glProgramEnvParameterI4ivNV) then Exit;
    Pointer(glProgramEnvParametersI4ivNV) := wglGetProcAddress('glProgramEnvParametersI4ivNV');
    if not Assigned(glProgramEnvParametersI4ivNV) then Exit;
    Pointer(glProgramEnvParameterI4uiNV) := wglGetProcAddress('glProgramEnvParameterI4uiNV');
    if not Assigned(glProgramEnvParameterI4uiNV) then Exit;
    Pointer(glProgramEnvParameterI4uivNV) := wglGetProcAddress('glProgramEnvParameterI4uivNV');
    if not Assigned(glProgramEnvParameterI4uivNV) then Exit;
    Pointer(glProgramEnvParametersI4uivNV) := wglGetProcAddress('glProgramEnvParametersI4uivNV');
    if not Assigned(glProgramEnvParametersI4uivNV) then Exit;
    Pointer(glGetProgramLocalParameterIivNV) := wglGetProcAddress('glGetProgramLocalParameterIivNV');
    if not Assigned(glGetProgramLocalParameterIivNV) then Exit;
    Pointer(glGetProgramLocalParameterIuivNV) := wglGetProcAddress('glGetProgramLocalParameterIuivNV');
    if not Assigned(glGetProgramLocalParameterIuivNV) then Exit;
    Pointer(glGetProgramEnvParameterIivNV) := wglGetProcAddress('glGetProgramEnvParameterIivNV');
    if not Assigned(glGetProgramEnvParameterIivNV) then Exit;
    Pointer(glGetProgramEnvParameterIuivNV) := wglGetProcAddress('glGetProgramEnvParameterIuivNV');
    if not Assigned(glGetProgramEnvParameterIuivNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_geometry_program4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_geometry_program4', extstring) then
  begin
    Pointer(glProgramVertexLimitNV) := wglGetProcAddress('glProgramVertexLimitNV');
    if not Assigned(glProgramVertexLimitNV) then Exit;
    Pointer(glFramebufferTextureEXT) := wglGetProcAddress('glFramebufferTextureEXT');
    if not Assigned(glFramebufferTextureEXT) then Exit;
    Pointer(glFramebufferTextureLayerEXT) := wglGetProcAddress('glFramebufferTextureLayerEXT');
    if not Assigned(glFramebufferTextureLayerEXT) then Exit;
    Pointer(glFramebufferTextureFaceEXT) := wglGetProcAddress('glFramebufferTextureFaceEXT');
    if not Assigned(glFramebufferTextureFaceEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_geometry_shader4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_geometry_shader4', extstring) then
  begin
    Pointer(glProgramParameteriEXT) := wglGetProcAddress('glProgramParameteriEXT');
    if not Assigned(glProgramParameteriEXT) then Exit;
    (* Shared entry points *)
    Pointer(glFramebufferTextureEXT) := wglGetProcAddress('glFramebufferTextureEXT');
    if not Assigned(glFramebufferTextureEXT) then Exit;
    Pointer(glFramebufferTextureLayerEXT) := wglGetProcAddress('glFramebufferTextureLayerEXT');
    if not Assigned(glFramebufferTextureLayerEXT) then Exit;
    Pointer(glFramebufferTextureFaceEXT) := wglGetProcAddress('glFramebufferTextureFaceEXT');
    if not Assigned(glFramebufferTextureFaceEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_vertex_program4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program4', extstring) then
  begin
    Pointer(glVertexAttribI1iEXT) := wglGetProcAddress('glVertexAttribI1iEXT');
    if not Assigned(glVertexAttribI1iEXT) then Exit;
    Pointer(glVertexAttribI2iEXT) := wglGetProcAddress('glVertexAttribI2iEXT');
    if not Assigned(glVertexAttribI2iEXT) then Exit;
    Pointer(glVertexAttribI3iEXT) := wglGetProcAddress('glVertexAttribI3iEXT');
    if not Assigned(glVertexAttribI3iEXT) then Exit;
    Pointer(glVertexAttribI4iEXT) := wglGetProcAddress('glVertexAttribI4iEXT');
    if not Assigned(glVertexAttribI4iEXT) then Exit;
    Pointer(glVertexAttribI1uiEXT) := wglGetProcAddress('glVertexAttribI1uiEXT');
    if not Assigned(glVertexAttribI1uiEXT) then Exit;
    Pointer(glVertexAttribI2uiEXT) := wglGetProcAddress('glVertexAttribI2uiEXT');
    if not Assigned(glVertexAttribI2uiEXT) then Exit;
    Pointer(glVertexAttribI3uiEXT) := wglGetProcAddress('glVertexAttribI3uiEXT');
    if not Assigned(glVertexAttribI3uiEXT) then Exit;
    Pointer(glVertexAttribI4uiEXT) := wglGetProcAddress('glVertexAttribI4uiEXT');
    if not Assigned(glVertexAttribI4uiEXT) then Exit;
    Pointer(glVertexAttribI1ivEXT) := wglGetProcAddress('glVertexAttribI1ivEXT');
    if not Assigned(glVertexAttribI1ivEXT) then Exit;
    Pointer(glVertexAttribI2ivEXT) := wglGetProcAddress('glVertexAttribI2ivEXT');
    if not Assigned(glVertexAttribI2ivEXT) then Exit;
    Pointer(glVertexAttribI3ivEXT) := wglGetProcAddress('glVertexAttribI3ivEXT');
    if not Assigned(glVertexAttribI3ivEXT) then Exit;
    Pointer(glVertexAttribI4ivEXT) := wglGetProcAddress('glVertexAttribI4ivEXT');
    if not Assigned(glVertexAttribI4ivEXT) then Exit;
    Pointer(glVertexAttribI1uivEXT) := wglGetProcAddress('glVertexAttribI1uivEXT');
    if not Assigned(glVertexAttribI1uivEXT) then Exit;
    Pointer(glVertexAttribI2uivEXT) := wglGetProcAddress('glVertexAttribI2uivEXT');
    if not Assigned(glVertexAttribI2uivEXT) then Exit;
    Pointer(glVertexAttribI3uivEXT) := wglGetProcAddress('glVertexAttribI3uivEXT');
    if not Assigned(glVertexAttribI3uivEXT) then Exit;
    Pointer(glVertexAttribI4uivEXT) := wglGetProcAddress('glVertexAttribI4uivEXT');
    if not Assigned(glVertexAttribI4uivEXT) then Exit;
    Pointer(glVertexAttribI4bvEXT) := wglGetProcAddress('glVertexAttribI4bvEXT');
    if not Assigned(glVertexAttribI4bvEXT) then Exit;
    Pointer(glVertexAttribI4svEXT) := wglGetProcAddress('glVertexAttribI4svEXT');
    if not Assigned(glVertexAttribI4svEXT) then Exit;
    Pointer(glVertexAttribI4ubvEXT) := wglGetProcAddress('glVertexAttribI4ubvEXT');
    if not Assigned(glVertexAttribI4ubvEXT) then Exit;
    Pointer(glVertexAttribI4usvEXT) := wglGetProcAddress('glVertexAttribI4usvEXT');
    if not Assigned(glVertexAttribI4usvEXT) then Exit;
    Pointer(glVertexAttribIPointerEXT) := wglGetProcAddress('glVertexAttribIPointerEXT');
    if not Assigned(glVertexAttribIPointerEXT) then Exit;
    Pointer(glGetVertexAttribIivEXT) := wglGetProcAddress('glGetVertexAttribIivEXT');
    if not Assigned(glGetVertexAttribIivEXT) then Exit;
    Pointer(glGetVertexAttribIuivEXT) := wglGetProcAddress('glGetVertexAttribIuivEXT');
    if not Assigned(glGetVertexAttribIuivEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_gpu_shader4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_gpu_shader4', extstring) then
  begin
    Pointer(glGetUniformuivEXT) := wglGetProcAddress('glGetUniformuivEXT');
    if not Assigned(glGetUniformuivEXT) then Exit;
    Pointer(glBindFragDataLocationEXT) := wglGetProcAddress('glBindFragDataLocationEXT');
    if not Assigned(glBindFragDataLocationEXT) then Exit;
    Pointer(glGetFragDataLocationEXT) := wglGetProcAddress('glGetFragDataLocationEXT');
    if not Assigned(glGetFragDataLocationEXT) then Exit;
    Pointer(glUniform1uiEXT) := wglGetProcAddress('glUniform1uiEXT');
    if not Assigned(glUniform1uiEXT) then Exit;
    Pointer(glUniform2uiEXT) := wglGetProcAddress('glUniform2uiEXT');
    if not Assigned(glUniform2uiEXT) then Exit;
    Pointer(glUniform3uiEXT) := wglGetProcAddress('glUniform3uiEXT');
    if not Assigned(glUniform3uiEXT) then Exit;
    Pointer(glUniform4uiEXT) := wglGetProcAddress('glUniform4uiEXT');
    if not Assigned(glUniform4uiEXT) then Exit;
    Pointer(glUniform1uivEXT) := wglGetProcAddress('glUniform1uivEXT');
    if not Assigned(glUniform1uivEXT) then Exit;
    Pointer(glUniform2uivEXT) := wglGetProcAddress('glUniform2uivEXT');
    if not Assigned(glUniform2uivEXT) then Exit;
    Pointer(glUniform3uivEXT) := wglGetProcAddress('glUniform3uivEXT');
    if not Assigned(glUniform3uivEXT) then Exit;
    Pointer(glUniform4uivEXT) := wglGetProcAddress('glUniform4uivEXT');
    if not Assigned(glUniform4uivEXT) then Exit;
    (* Shared entry points *)
    Pointer(glVertexAttribI1iEXT) := wglGetProcAddress('glVertexAttribI1iEXT');
    if not Assigned(glVertexAttribI1iEXT) then Exit;
    Pointer(glVertexAttribI2iEXT) := wglGetProcAddress('glVertexAttribI2iEXT');
    if not Assigned(glVertexAttribI2iEXT) then Exit;
    Pointer(glVertexAttribI3iEXT) := wglGetProcAddress('glVertexAttribI3iEXT');
    if not Assigned(glVertexAttribI3iEXT) then Exit;
    Pointer(glVertexAttribI4iEXT) := wglGetProcAddress('glVertexAttribI4iEXT');
    if not Assigned(glVertexAttribI4iEXT) then Exit;
    Pointer(glVertexAttribI1uiEXT) := wglGetProcAddress('glVertexAttribI1uiEXT');
    if not Assigned(glVertexAttribI1uiEXT) then Exit;
    Pointer(glVertexAttribI2uiEXT) := wglGetProcAddress('glVertexAttribI2uiEXT');
    if not Assigned(glVertexAttribI2uiEXT) then Exit;
    Pointer(glVertexAttribI3uiEXT) := wglGetProcAddress('glVertexAttribI3uiEXT');
    if not Assigned(glVertexAttribI3uiEXT) then Exit;
    Pointer(glVertexAttribI4uiEXT) := wglGetProcAddress('glVertexAttribI4uiEXT');
    if not Assigned(glVertexAttribI4uiEXT) then Exit;
    Pointer(glVertexAttribI1ivEXT) := wglGetProcAddress('glVertexAttribI1ivEXT');
    if not Assigned(glVertexAttribI1ivEXT) then Exit;
    Pointer(glVertexAttribI2ivEXT) := wglGetProcAddress('glVertexAttribI2ivEXT');
    if not Assigned(glVertexAttribI2ivEXT) then Exit;
    Pointer(glVertexAttribI3ivEXT) := wglGetProcAddress('glVertexAttribI3ivEXT');
    if not Assigned(glVertexAttribI3ivEXT) then Exit;
    Pointer(glVertexAttribI4ivEXT) := wglGetProcAddress('glVertexAttribI4ivEXT');
    if not Assigned(glVertexAttribI4ivEXT) then Exit;
    Pointer(glVertexAttribI1uivEXT) := wglGetProcAddress('glVertexAttribI1uivEXT');
    if not Assigned(glVertexAttribI1uivEXT) then Exit;
    Pointer(glVertexAttribI2uivEXT) := wglGetProcAddress('glVertexAttribI2uivEXT');
    if not Assigned(glVertexAttribI2uivEXT) then Exit;
    Pointer(glVertexAttribI3uivEXT) := wglGetProcAddress('glVertexAttribI3uivEXT');
    if not Assigned(glVertexAttribI3uivEXT) then Exit;
    Pointer(glVertexAttribI4uivEXT) := wglGetProcAddress('glVertexAttribI4uivEXT');
    if not Assigned(glVertexAttribI4uivEXT) then Exit;
    Pointer(glVertexAttribI4bvEXT) := wglGetProcAddress('glVertexAttribI4bvEXT');
    if not Assigned(glVertexAttribI4bvEXT) then Exit;
    Pointer(glVertexAttribI4svEXT) := wglGetProcAddress('glVertexAttribI4svEXT');
    if not Assigned(glVertexAttribI4svEXT) then Exit;
    Pointer(glVertexAttribI4ubvEXT) := wglGetProcAddress('glVertexAttribI4ubvEXT');
    if not Assigned(glVertexAttribI4ubvEXT) then Exit;
    Pointer(glVertexAttribI4usvEXT) := wglGetProcAddress('glVertexAttribI4usvEXT');
    if not Assigned(glVertexAttribI4usvEXT) then Exit;
    Pointer(glVertexAttribIPointerEXT) := wglGetProcAddress('glVertexAttribIPointerEXT');
    if not Assigned(glVertexAttribIPointerEXT) then Exit;
    Pointer(glGetVertexAttribIivEXT) := wglGetProcAddress('glGetVertexAttribIivEXT');
    if not Assigned(glGetVertexAttribIivEXT) then Exit;
    Pointer(glGetVertexAttribIuivEXT) := wglGetProcAddress('glGetVertexAttribIuivEXT');
    if not Assigned(glGetVertexAttribIuivEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_draw_instanced(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_draw_instanced', extstring) then
  begin
    Pointer(glDrawArraysInstancedEXT) := wglGetProcAddress('glDrawArraysInstancedEXT');
    if not Assigned(glDrawArraysInstancedEXT) then Exit;
    Pointer(glDrawElementsInstancedEXT) := wglGetProcAddress('glDrawElementsInstancedEXT');
    if not Assigned(glDrawElementsInstancedEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_packed_float(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_packed_float', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_array(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_array', extstring) then
  begin
    (* Shared entry points *)
    Pointer(glFramebufferTextureLayerEXT) := wglGetProcAddress('glFramebufferTextureLayerEXT');
    if not Assigned(glFramebufferTextureLayerEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_buffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_buffer_object', extstring) then
  begin
    Pointer(glTexBufferEXT) := wglGetProcAddress('glTexBufferEXT');
    if not Assigned(glTexBufferEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_compression_latc(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_compression_latc', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_compression_rgtc(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_compression_rgtc', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_shared_exponent(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_shared_exponent', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_depth_buffer_float(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_depth_buffer_float', extstring) then
  begin
    Pointer(glDepthRangedNV) := wglGetProcAddress('glDepthRangedNV');
    if not Assigned(glDepthRangedNV) then Exit;
    Pointer(glClearDepthdNV) := wglGetProcAddress('glClearDepthdNV');
    if not Assigned(glClearDepthdNV) then Exit;
    Pointer(glDepthBoundsdNV) := wglGetProcAddress('glDepthBoundsdNV');
    if not Assigned(glDepthBoundsdNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_fragment_program4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fragment_program4', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_framebuffer_multisample_coverage(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_framebuffer_multisample_coverage', extstring) then
  begin
    Pointer(glRenderbufferStorageMultisampleCoverageNV) := wglGetProcAddress('glRenderbufferStorageMultisampleCoverageNV');
    if not Assigned(glRenderbufferStorageMultisampleCoverageNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_framebuffer_sRGB(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_framebuffer_sRGB', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_geometry_shader4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_geometry_shader4', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_parameter_buffer_object(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_parameter_buffer_object', extstring) then
  begin
    Pointer(glProgramBufferParametersfvNV) := wglGetProcAddress('glProgramBufferParametersfvNV');
    if not Assigned(glProgramBufferParametersfvNV) then Exit;
    Pointer(glProgramBufferParametersIivNV) := wglGetProcAddress('glProgramBufferParametersIivNV');
    if not Assigned(glProgramBufferParametersIivNV) then Exit;
    Pointer(glProgramBufferParametersIuivNV) := wglGetProcAddress('glProgramBufferParametersIuivNV');
    if not Assigned(glProgramBufferParametersIuivNV) then Exit;
    (* Shared entry points *)
    Pointer(glBindBufferRangeNV) := wglGetProcAddress('glBindBufferRangeNV');
    if not Assigned(glBindBufferRangeNV) then Exit;
    Pointer(glBindBufferOffsetNV) := wglGetProcAddress('glBindBufferOffsetNV');
    if not Assigned(glBindBufferOffsetNV) then Exit;
    Pointer(glBindBufferBaseNV) := wglGetProcAddress('glBindBufferBaseNV');
    if not Assigned(glBindBufferBaseNV) then Exit;
    Pointer(glGetIntegerIndexedvEXT) := wglGetProcAddress('glGetIntegerIndexedvEXT');
    if not Assigned(glGetIntegerIndexedvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_draw_buffers2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_draw_buffers2', extstring) then
  begin
    Pointer(glColorMaskIndexedEXT) := wglGetProcAddress('glColorMaskIndexedEXT');
    if not Assigned(glColorMaskIndexedEXT) then Exit;
    Pointer(glGetBooleanIndexedvEXT) := wglGetProcAddress('glGetBooleanIndexedvEXT');
    if not Assigned(glGetBooleanIndexedvEXT) then Exit;
    Pointer(glGetIntegerIndexedvEXT) := wglGetProcAddress('glGetIntegerIndexedvEXT');
    if not Assigned(glGetIntegerIndexedvEXT) then Exit;
    Pointer(glEnableIndexedEXT) := wglGetProcAddress('glEnableIndexedEXT');
    if not Assigned(glEnableIndexedEXT) then Exit;
    Pointer(glDisableIndexedEXT) := wglGetProcAddress('glDisableIndexedEXT');
    if not Assigned(glDisableIndexedEXT) then Exit;
    Pointer(glIsEnabledIndexedEXT) := wglGetProcAddress('glIsEnabledIndexedEXT');
    if not Assigned(glIsEnabledIndexedEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_transform_feedback(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_transform_feedback', extstring) then
  begin
    Pointer(glBeginTransformFeedbackNV) := wglGetProcAddress('glBeginTransformFeedbackNV');
    if not Assigned(glBeginTransformFeedbackNV) then Exit;
    Pointer(glEndTransformFeedbackNV) := wglGetProcAddress('glEndTransformFeedbackNV');
    if not Assigned(glEndTransformFeedbackNV) then Exit;
    Pointer(glTransformFeedbackAttribsNV) := wglGetProcAddress('glTransformFeedbackAttribsNV');
    if not Assigned(glTransformFeedbackAttribsNV) then Exit;
    Pointer(glBindBufferRangeNV) := wglGetProcAddress('glBindBufferRangeNV');
    if not Assigned(glBindBufferRangeNV) then Exit;
    Pointer(glBindBufferOffsetNV) := wglGetProcAddress('glBindBufferOffsetNV');
    if not Assigned(glBindBufferOffsetNV) then Exit;
    Pointer(glBindBufferBaseNV) := wglGetProcAddress('glBindBufferBaseNV');
    if not Assigned(glBindBufferBaseNV) then Exit;
    Pointer(glTransformFeedbackVaryingsNV) := wglGetProcAddress('glTransformFeedbackVaryingsNV');
    if not Assigned(glTransformFeedbackVaryingsNV) then Exit;
    Pointer(glActiveVaryingNV) := wglGetProcAddress('glActiveVaryingNV');
    if not Assigned(glActiveVaryingNV) then Exit;
    Pointer(glGetVaryingLocationNV) := wglGetProcAddress('glGetVaryingLocationNV');
    if not Assigned(glGetVaryingLocationNV) then Exit;
    Pointer(glGetActiveVaryingNV) := wglGetProcAddress('glGetActiveVaryingNV');
    if not Assigned(glGetActiveVaryingNV) then Exit;
    Pointer(glGetTransformFeedbackVaryingNV) := wglGetProcAddress('glGetTransformFeedbackVaryingNV');
    if not Assigned(glGetTransformFeedbackVaryingNV) then Exit;
    (* Shared entry points *)
    Pointer(glGetIntegerIndexedvEXT) := wglGetProcAddress('glGetIntegerIndexedvEXT');
    if not Assigned(glGetIntegerIndexedvEXT) then Exit;
    Pointer(glGetBooleanIndexedvEXT) := wglGetProcAddress('glGetBooleanIndexedvEXT');
    if not Assigned(glGetBooleanIndexedvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_bindable_uniform(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_bindable_uniform', extstring) then
  begin
    Pointer(glUniformBufferEXT) := wglGetProcAddress('glUniformBufferEXT');
    if not Assigned(glUniformBufferEXT) then Exit;
    Pointer(glGetUniformBufferSizeEXT) := wglGetProcAddress('glGetUniformBufferSizeEXT');
    if not Assigned(glGetUniformBufferSizeEXT) then Exit;
    Pointer(glGetUniformOffsetEXT) := wglGetProcAddress('glGetUniformOffsetEXT');
    if not Assigned(glGetUniformOffsetEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_integer(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_integer', extstring) then
  begin
    Pointer(glTexParameterIivEXT) := wglGetProcAddress('glTexParameterIivEXT');
    if not Assigned(glTexParameterIivEXT) then Exit;
    Pointer(glTexParameterIuivEXT) := wglGetProcAddress('glTexParameterIuivEXT');
    if not Assigned(glTexParameterIuivEXT) then Exit;
    Pointer(glGetTexParameterIivEXT) := wglGetProcAddress('glGetTexParameterIivEXT');
    if not Assigned(glGetTexParameterIivEXT) then Exit;
    Pointer(glGetTexParameterIuivEXT) := wglGetProcAddress('glGetTexParameterIuivEXT');
    if not Assigned(glGetTexParameterIuivEXT) then Exit;
    Pointer(glClearColorIiEXT) := wglGetProcAddress('glClearColorIiEXT');
    if not Assigned(glClearColorIiEXT) then Exit;
    Pointer(glClearColorIuiEXT) := wglGetProcAddress('glClearColorIuiEXT');
    if not Assigned(glClearColorIuiEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_GREMEDY_frame_terminator(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_GREMEDY_frame_terminator', extstring) then
  begin
    Pointer(glFrameTerminatorGREMEDY) := wglGetProcAddress('glFrameTerminatorGREMEDY');
    if not Assigned(glFrameTerminatorGREMEDY) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_conditional_render(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_conditional_render', extstring) then
  begin
    Pointer(glBeginConditionalRenderNV) := wglGetProcAddress('glBeginConditionalRenderNV');
    if not Assigned(glBeginConditionalRenderNV) then Exit;
    Pointer(glEndConditionalRenderNV) := wglGetProcAddress('glEndConditionalRenderNV');
    if not Assigned(glEndConditionalRenderNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_present_video(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_present_video', extstring) then
  begin
    Pointer(glPresentFrameKeyedNV) := wglGetProcAddress('glPresentFrameKeyedNV');
    if not Assigned(glPresentFrameKeyedNV) then Exit;
    Pointer(glPresentFrameDualFillNV) := wglGetProcAddress('glPresentFrameDualFillNV');
    if not Assigned(glPresentFrameDualFillNV) then Exit;
    Pointer(glGetVideoivNV) := wglGetProcAddress('glGetVideoivNV');
    if not Assigned(glGetVideoivNV) then Exit;
    Pointer(glGetVideouivNV) := wglGetProcAddress('glGetVideouivNV');
    if not Assigned(glGetVideouivNV) then Exit;
    Pointer(glGetVideoi64vNV) := wglGetProcAddress('glGetVideoi64vNV');
    if not Assigned(glGetVideoi64vNV) then Exit;
    Pointer(glGetVideoui64vNV) := wglGetProcAddress('glGetVideoui64vNV');
    if not Assigned(glGetVideoui64vNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_transform_feedback(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_transform_feedback', extstring) then
  begin
    Pointer(glBeginTransformFeedbackEXT) := wglGetProcAddress('glBeginTransformFeedbackEXT');
    if not Assigned(glBeginTransformFeedbackEXT) then Exit;
    Pointer(glEndTransformFeedbackEXT) := wglGetProcAddress('glEndTransformFeedbackEXT');
    if not Assigned(glEndTransformFeedbackEXT) then Exit;
    Pointer(glBindBufferRangeEXT) := wglGetProcAddress('glBindBufferRangeEXT');
    if not Assigned(glBindBufferRangeEXT) then Exit;
    Pointer(glBindBufferOffsetEXT) := wglGetProcAddress('glBindBufferOffsetEXT');
    if not Assigned(glBindBufferOffsetEXT) then Exit;
    Pointer(glBindBufferBaseEXT) := wglGetProcAddress('glBindBufferBaseEXT');
    if not Assigned(glBindBufferBaseEXT) then Exit;
    Pointer(glTransformFeedbackVaryingsEXT) := wglGetProcAddress('glTransformFeedbackVaryingsEXT');
    if not Assigned(glTransformFeedbackVaryingsEXT) then Exit;
    Pointer(glGetTransformFeedbackVaryingEXT) := wglGetProcAddress('glGetTransformFeedbackVaryingEXT');
    if not Assigned(glGetTransformFeedbackVaryingEXT) then Exit;
    (* Shared entry points *)
    Pointer(glGetIntegerIndexedvEXT) := wglGetProcAddress('glGetIntegerIndexedvEXT');
    if not Assigned(glGetIntegerIndexedvEXT) then Exit;
    Pointer(glGetBooleanIndexedvEXT) := wglGetProcAddress('glGetBooleanIndexedvEXT');
    if not Assigned(glGetBooleanIndexedvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_direct_state_access(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_direct_state_access', extstring) then
  begin
    Pointer(glClientAttribDefaultEXT) := wglGetProcAddress('glClientAttribDefaultEXT');
    if not Assigned(glClientAttribDefaultEXT) then Exit;
    Pointer(glPushClientAttribDefaultEXT) := wglGetProcAddress('glPushClientAttribDefaultEXT');
    if not Assigned(glPushClientAttribDefaultEXT) then Exit;
    Pointer(glMatrixLoadfEXT) := wglGetProcAddress('glMatrixLoadfEXT');
    if not Assigned(glMatrixLoadfEXT) then Exit;
    Pointer(glMatrixLoaddEXT) := wglGetProcAddress('glMatrixLoaddEXT');
    if not Assigned(glMatrixLoaddEXT) then Exit;
    Pointer(glMatrixMultfEXT) := wglGetProcAddress('glMatrixMultfEXT');
    if not Assigned(glMatrixMultfEXT) then Exit;
    Pointer(glMatrixMultdEXT) := wglGetProcAddress('glMatrixMultdEXT');
    if not Assigned(glMatrixMultdEXT) then Exit;
    Pointer(glMatrixLoadIdentityEXT) := wglGetProcAddress('glMatrixLoadIdentityEXT');
    if not Assigned(glMatrixLoadIdentityEXT) then Exit;
    Pointer(glMatrixRotatefEXT) := wglGetProcAddress('glMatrixRotatefEXT');
    if not Assigned(glMatrixRotatefEXT) then Exit;
    Pointer(glMatrixRotatedEXT) := wglGetProcAddress('glMatrixRotatedEXT');
    if not Assigned(glMatrixRotatedEXT) then Exit;
    Pointer(glMatrixScalefEXT) := wglGetProcAddress('glMatrixScalefEXT');
    if not Assigned(glMatrixScalefEXT) then Exit;
    Pointer(glMatrixScaledEXT) := wglGetProcAddress('glMatrixScaledEXT');
    if not Assigned(glMatrixScaledEXT) then Exit;
    Pointer(glMatrixTranslatefEXT) := wglGetProcAddress('glMatrixTranslatefEXT');
    if not Assigned(glMatrixTranslatefEXT) then Exit;
    Pointer(glMatrixTranslatedEXT) := wglGetProcAddress('glMatrixTranslatedEXT');
    if not Assigned(glMatrixTranslatedEXT) then Exit;
    Pointer(glMatrixFrustumEXT) := wglGetProcAddress('glMatrixFrustumEXT');
    if not Assigned(glMatrixFrustumEXT) then Exit;
    Pointer(glMatrixOrthoEXT) := wglGetProcAddress('glMatrixOrthoEXT');
    if not Assigned(glMatrixOrthoEXT) then Exit;
    Pointer(glMatrixPopEXT) := wglGetProcAddress('glMatrixPopEXT');
    if not Assigned(glMatrixPopEXT) then Exit;
    Pointer(glMatrixPushEXT) := wglGetProcAddress('glMatrixPushEXT');
    if not Assigned(glMatrixPushEXT) then Exit;
    Pointer(glMatrixLoadTransposefEXT) := wglGetProcAddress('glMatrixLoadTransposefEXT');
    if not Assigned(glMatrixLoadTransposefEXT) then Exit;
    Pointer(glMatrixLoadTransposedEXT) := wglGetProcAddress('glMatrixLoadTransposedEXT');
    if not Assigned(glMatrixLoadTransposedEXT) then Exit;
    Pointer(glMatrixMultTransposefEXT) := wglGetProcAddress('glMatrixMultTransposefEXT');
    if not Assigned(glMatrixMultTransposefEXT) then Exit;
    Pointer(glMatrixMultTransposedEXT) := wglGetProcAddress('glMatrixMultTransposedEXT');
    if not Assigned(glMatrixMultTransposedEXT) then Exit;
    Pointer(glTextureParameterfEXT) := wglGetProcAddress('glTextureParameterfEXT');
    if not Assigned(glTextureParameterfEXT) then Exit;
    Pointer(glTextureParameterfvEXT) := wglGetProcAddress('glTextureParameterfvEXT');
    if not Assigned(glTextureParameterfvEXT) then Exit;
    Pointer(glTextureParameteriEXT) := wglGetProcAddress('glTextureParameteriEXT');
    if not Assigned(glTextureParameteriEXT) then Exit;
    Pointer(glTextureParameterivEXT) := wglGetProcAddress('glTextureParameterivEXT');
    if not Assigned(glTextureParameterivEXT) then Exit;
    Pointer(glTextureImage1DEXT) := wglGetProcAddress('glTextureImage1DEXT');
    if not Assigned(glTextureImage1DEXT) then Exit;
    Pointer(glTextureImage2DEXT) := wglGetProcAddress('glTextureImage2DEXT');
    if not Assigned(glTextureImage2DEXT) then Exit;
    Pointer(glTextureSubImage1DEXT) := wglGetProcAddress('glTextureSubImage1DEXT');
    if not Assigned(glTextureSubImage1DEXT) then Exit;
    Pointer(glTextureSubImage2DEXT) := wglGetProcAddress('glTextureSubImage2DEXT');
    if not Assigned(glTextureSubImage2DEXT) then Exit;
    Pointer(glCopyTextureImage1DEXT) := wglGetProcAddress('glCopyTextureImage1DEXT');
    if not Assigned(glCopyTextureImage1DEXT) then Exit;
    Pointer(glCopyTextureImage2DEXT) := wglGetProcAddress('glCopyTextureImage2DEXT');
    if not Assigned(glCopyTextureImage2DEXT) then Exit;
    Pointer(glCopyTextureSubImage1DEXT) := wglGetProcAddress('glCopyTextureSubImage1DEXT');
    if not Assigned(glCopyTextureSubImage1DEXT) then Exit;
    Pointer(glCopyTextureSubImage2DEXT) := wglGetProcAddress('glCopyTextureSubImage2DEXT');
    if not Assigned(glCopyTextureSubImage2DEXT) then Exit;
    Pointer(glGetTextureImageEXT) := wglGetProcAddress('glGetTextureImageEXT');
    if not Assigned(glGetTextureImageEXT) then Exit;
    Pointer(glGetTextureParameterfvEXT) := wglGetProcAddress('glGetTextureParameterfvEXT');
    if not Assigned(glGetTextureParameterfvEXT) then Exit;
    Pointer(glGetTextureParameterivEXT) := wglGetProcAddress('glGetTextureParameterivEXT');
    if not Assigned(glGetTextureParameterivEXT) then Exit;
    Pointer(glGetTextureLevelParameterfvEXT) := wglGetProcAddress('glGetTextureLevelParameterfvEXT');
    if not Assigned(glGetTextureLevelParameterfvEXT) then Exit;
    Pointer(glGetTextureLevelParameterivEXT) := wglGetProcAddress('glGetTextureLevelParameterivEXT');
    if not Assigned(glGetTextureLevelParameterivEXT) then Exit;
    Pointer(glTextureImage3DEXT) := wglGetProcAddress('glTextureImage3DEXT');
    if not Assigned(glTextureImage3DEXT) then Exit;
    Pointer(glTextureSubImage3DEXT) := wglGetProcAddress('glTextureSubImage3DEXT');
    if not Assigned(glTextureSubImage3DEXT) then Exit;
    Pointer(glCopyTextureSubImage3DEXT) := wglGetProcAddress('glCopyTextureSubImage3DEXT');
    if not Assigned(glCopyTextureSubImage3DEXT) then Exit;
    Pointer(glMultiTexParameterfEXT) := wglGetProcAddress('glMultiTexParameterfEXT');
    if not Assigned(glMultiTexParameterfEXT) then Exit;
    Pointer(glMultiTexParameterfvEXT) := wglGetProcAddress('glMultiTexParameterfvEXT');
    if not Assigned(glMultiTexParameterfvEXT) then Exit;
    Pointer(glMultiTexParameteriEXT) := wglGetProcAddress('glMultiTexParameteriEXT');
    if not Assigned(glMultiTexParameteriEXT) then Exit;
    Pointer(glMultiTexParameterivEXT) := wglGetProcAddress('glMultiTexParameterivEXT');
    if not Assigned(glMultiTexParameterivEXT) then Exit;
    Pointer(glMultiTexImage1DEXT) := wglGetProcAddress('glMultiTexImage1DEXT');
    if not Assigned(glMultiTexImage1DEXT) then Exit;
    Pointer(glMultiTexImage2DEXT) := wglGetProcAddress('glMultiTexImage2DEXT');
    if not Assigned(glMultiTexImage2DEXT) then Exit;
    Pointer(glMultiTexSubImage1DEXT) := wglGetProcAddress('glMultiTexSubImage1DEXT');
    if not Assigned(glMultiTexSubImage1DEXT) then Exit;
    Pointer(glMultiTexSubImage2DEXT) := wglGetProcAddress('glMultiTexSubImage2DEXT');
    if not Assigned(glMultiTexSubImage2DEXT) then Exit;
    Pointer(glCopyMultiTexImage1DEXT) := wglGetProcAddress('glCopyMultiTexImage1DEXT');
    if not Assigned(glCopyMultiTexImage1DEXT) then Exit;
    Pointer(glCopyMultiTexImage2DEXT) := wglGetProcAddress('glCopyMultiTexImage2DEXT');
    if not Assigned(glCopyMultiTexImage2DEXT) then Exit;
    Pointer(glCopyMultiTexSubImage1DEXT) := wglGetProcAddress('glCopyMultiTexSubImage1DEXT');
    if not Assigned(glCopyMultiTexSubImage1DEXT) then Exit;
    Pointer(glCopyMultiTexSubImage2DEXT) := wglGetProcAddress('glCopyMultiTexSubImage2DEXT');
    if not Assigned(glCopyMultiTexSubImage2DEXT) then Exit;
    Pointer(glGetMultiTexImageEXT) := wglGetProcAddress('glGetMultiTexImageEXT');
    if not Assigned(glGetMultiTexImageEXT) then Exit;
    Pointer(glGetMultiTexParameterfvEXT) := wglGetProcAddress('glGetMultiTexParameterfvEXT');
    if not Assigned(glGetMultiTexParameterfvEXT) then Exit;
    Pointer(glGetMultiTexParameterivEXT) := wglGetProcAddress('glGetMultiTexParameterivEXT');
    if not Assigned(glGetMultiTexParameterivEXT) then Exit;
    Pointer(glGetMultiTexLevelParameterfvEXT) := wglGetProcAddress('glGetMultiTexLevelParameterfvEXT');
    if not Assigned(glGetMultiTexLevelParameterfvEXT) then Exit;
    Pointer(glGetMultiTexLevelParameterivEXT) := wglGetProcAddress('glGetMultiTexLevelParameterivEXT');
    if not Assigned(glGetMultiTexLevelParameterivEXT) then Exit;
    Pointer(glMultiTexImage3DEXT) := wglGetProcAddress('glMultiTexImage3DEXT');
    if not Assigned(glMultiTexImage3DEXT) then Exit;
    Pointer(glMultiTexSubImage3DEXT) := wglGetProcAddress('glMultiTexSubImage3DEXT');
    if not Assigned(glMultiTexSubImage3DEXT) then Exit;
    Pointer(glCopyMultiTexSubImage3DEXT) := wglGetProcAddress('glCopyMultiTexSubImage3DEXT');
    if not Assigned(glCopyMultiTexSubImage3DEXT) then Exit;
    Pointer(glBindMultiTextureEXT) := wglGetProcAddress('glBindMultiTextureEXT');
    if not Assigned(glBindMultiTextureEXT) then Exit;
    Pointer(glEnableClientStateIndexedEXT) := wglGetProcAddress('glEnableClientStateIndexedEXT');
    if not Assigned(glEnableClientStateIndexedEXT) then Exit;
    Pointer(glDisableClientStateIndexedEXT) := wglGetProcAddress('glDisableClientStateIndexedEXT');
    if not Assigned(glDisableClientStateIndexedEXT) then Exit;
    Pointer(glMultiTexCoordPointerEXT) := wglGetProcAddress('glMultiTexCoordPointerEXT');
    if not Assigned(glMultiTexCoordPointerEXT) then Exit;
    Pointer(glMultiTexEnvfEXT) := wglGetProcAddress('glMultiTexEnvfEXT');
    if not Assigned(glMultiTexEnvfEXT) then Exit;
    Pointer(glMultiTexEnvfvEXT) := wglGetProcAddress('glMultiTexEnvfvEXT');
    if not Assigned(glMultiTexEnvfvEXT) then Exit;
    Pointer(glMultiTexEnviEXT) := wglGetProcAddress('glMultiTexEnviEXT');
    if not Assigned(glMultiTexEnviEXT) then Exit;
    Pointer(glMultiTexEnvivEXT) := wglGetProcAddress('glMultiTexEnvivEXT');
    if not Assigned(glMultiTexEnvivEXT) then Exit;
    Pointer(glMultiTexGendEXT) := wglGetProcAddress('glMultiTexGendEXT');
    if not Assigned(glMultiTexGendEXT) then Exit;
    Pointer(glMultiTexGendvEXT) := wglGetProcAddress('glMultiTexGendvEXT');
    if not Assigned(glMultiTexGendvEXT) then Exit;
    Pointer(glMultiTexGenfEXT) := wglGetProcAddress('glMultiTexGenfEXT');
    if not Assigned(glMultiTexGenfEXT) then Exit;
    Pointer(glMultiTexGenfvEXT) := wglGetProcAddress('glMultiTexGenfvEXT');
    if not Assigned(glMultiTexGenfvEXT) then Exit;
    Pointer(glMultiTexGeniEXT) := wglGetProcAddress('glMultiTexGeniEXT');
    if not Assigned(glMultiTexGeniEXT) then Exit;
    Pointer(glMultiTexGenivEXT) := wglGetProcAddress('glMultiTexGenivEXT');
    if not Assigned(glMultiTexGenivEXT) then Exit;
    Pointer(glGetMultiTexEnvfvEXT) := wglGetProcAddress('glGetMultiTexEnvfvEXT');
    if not Assigned(glGetMultiTexEnvfvEXT) then Exit;
    Pointer(glGetMultiTexEnvivEXT) := wglGetProcAddress('glGetMultiTexEnvivEXT');
    if not Assigned(glGetMultiTexEnvivEXT) then Exit;
    Pointer(glGetMultiTexGendvEXT) := wglGetProcAddress('glGetMultiTexGendvEXT');
    if not Assigned(glGetMultiTexGendvEXT) then Exit;
    Pointer(glGetMultiTexGenfvEXT) := wglGetProcAddress('glGetMultiTexGenfvEXT');
    if not Assigned(glGetMultiTexGenfvEXT) then Exit;
    Pointer(glGetMultiTexGenivEXT) := wglGetProcAddress('glGetMultiTexGenivEXT');
    if not Assigned(glGetMultiTexGenivEXT) then Exit;
    Pointer(glGetFloatIndexedvEXT) := wglGetProcAddress('glGetFloatIndexedvEXT');
    if not Assigned(glGetFloatIndexedvEXT) then Exit;
    Pointer(glGetDoubleIndexedvEXT) := wglGetProcAddress('glGetDoubleIndexedvEXT');
    if not Assigned(glGetDoubleIndexedvEXT) then Exit;
    Pointer(glGetPointerIndexedvEXT) := wglGetProcAddress('glGetPointerIndexedvEXT');
    if not Assigned(glGetPointerIndexedvEXT) then Exit;
    Pointer(glCompressedTextureImage3DEXT) := wglGetProcAddress('glCompressedTextureImage3DEXT');
    if not Assigned(glCompressedTextureImage3DEXT) then Exit;
    Pointer(glCompressedTextureImage2DEXT) := wglGetProcAddress('glCompressedTextureImage2DEXT');
    if not Assigned(glCompressedTextureImage2DEXT) then Exit;
    Pointer(glCompressedTextureImage1DEXT) := wglGetProcAddress('glCompressedTextureImage1DEXT');
    if not Assigned(glCompressedTextureImage1DEXT) then Exit;
    Pointer(glCompressedTextureSubImage3DEXT) := wglGetProcAddress('glCompressedTextureSubImage3DEXT');
    if not Assigned(glCompressedTextureSubImage3DEXT) then Exit;
    Pointer(glCompressedTextureSubImage2DEXT) := wglGetProcAddress('glCompressedTextureSubImage2DEXT');
    if not Assigned(glCompressedTextureSubImage2DEXT) then Exit;
    Pointer(glCompressedTextureSubImage1DEXT) := wglGetProcAddress('glCompressedTextureSubImage1DEXT');
    if not Assigned(glCompressedTextureSubImage1DEXT) then Exit;
    Pointer(glGetCompressedTextureImageEXT) := wglGetProcAddress('glGetCompressedTextureImageEXT');
    if not Assigned(glGetCompressedTextureImageEXT) then Exit;
    Pointer(glCompressedMultiTexImage3DEXT) := wglGetProcAddress('glCompressedMultiTexImage3DEXT');
    if not Assigned(glCompressedMultiTexImage3DEXT) then Exit;
    Pointer(glCompressedMultiTexImage2DEXT) := wglGetProcAddress('glCompressedMultiTexImage2DEXT');
    if not Assigned(glCompressedMultiTexImage2DEXT) then Exit;
    Pointer(glCompressedMultiTexImage1DEXT) := wglGetProcAddress('glCompressedMultiTexImage1DEXT');
    if not Assigned(glCompressedMultiTexImage1DEXT) then Exit;
    Pointer(glCompressedMultiTexSubImage3DEXT) := wglGetProcAddress('glCompressedMultiTexSubImage3DEXT');
    if not Assigned(glCompressedMultiTexSubImage3DEXT) then Exit;
    Pointer(glCompressedMultiTexSubImage2DEXT) := wglGetProcAddress('glCompressedMultiTexSubImage2DEXT');
    if not Assigned(glCompressedMultiTexSubImage2DEXT) then Exit;
    Pointer(glCompressedMultiTexSubImage1DEXT) := wglGetProcAddress('glCompressedMultiTexSubImage1DEXT');
    if not Assigned(glCompressedMultiTexSubImage1DEXT) then Exit;
    Pointer(glGetCompressedMultiTexImageEXT) := wglGetProcAddress('glGetCompressedMultiTexImageEXT');
    if not Assigned(glGetCompressedMultiTexImageEXT) then Exit;
    Pointer(glNamedProgramStringEXT) := wglGetProcAddress('glNamedProgramStringEXT');
    if not Assigned(glNamedProgramStringEXT) then Exit;
    Pointer(glNamedProgramLocalParameter4dEXT) := wglGetProcAddress('glNamedProgramLocalParameter4dEXT');
    if not Assigned(glNamedProgramLocalParameter4dEXT) then Exit;
    Pointer(glNamedProgramLocalParameter4dvEXT) := wglGetProcAddress('glNamedProgramLocalParameter4dvEXT');
    if not Assigned(glNamedProgramLocalParameter4dvEXT) then Exit;
    Pointer(glNamedProgramLocalParameter4fEXT) := wglGetProcAddress('glNamedProgramLocalParameter4fEXT');
    if not Assigned(glNamedProgramLocalParameter4fEXT) then Exit;
    Pointer(glNamedProgramLocalParameter4fvEXT) := wglGetProcAddress('glNamedProgramLocalParameter4fvEXT');
    if not Assigned(glNamedProgramLocalParameter4fvEXT) then Exit;
    Pointer(glGetNamedProgramLocalParameterdvEXT) := wglGetProcAddress('glGetNamedProgramLocalParameterdvEXT');
    if not Assigned(glGetNamedProgramLocalParameterdvEXT) then Exit;
    Pointer(glGetNamedProgramLocalParameterfvEXT) := wglGetProcAddress('glGetNamedProgramLocalParameterfvEXT');
    if not Assigned(glGetNamedProgramLocalParameterfvEXT) then Exit;
    Pointer(glGetNamedProgramivEXT) := wglGetProcAddress('glGetNamedProgramivEXT');
    if not Assigned(glGetNamedProgramivEXT) then Exit;
    Pointer(glGetNamedProgramStringEXT) := wglGetProcAddress('glGetNamedProgramStringEXT');
    if not Assigned(glGetNamedProgramStringEXT) then Exit;
    Pointer(glNamedProgramLocalParameters4fvEXT) := wglGetProcAddress('glNamedProgramLocalParameters4fvEXT');
    if not Assigned(glNamedProgramLocalParameters4fvEXT) then Exit;
    Pointer(glNamedProgramLocalParameterI4iEXT) := wglGetProcAddress('glNamedProgramLocalParameterI4iEXT');
    if not Assigned(glNamedProgramLocalParameterI4iEXT) then Exit;
    Pointer(glNamedProgramLocalParameterI4ivEXT) := wglGetProcAddress('glNamedProgramLocalParameterI4ivEXT');
    if not Assigned(glNamedProgramLocalParameterI4ivEXT) then Exit;
    Pointer(glNamedProgramLocalParametersI4ivEXT) := wglGetProcAddress('glNamedProgramLocalParametersI4ivEXT');
    if not Assigned(glNamedProgramLocalParametersI4ivEXT) then Exit;
    Pointer(glNamedProgramLocalParameterI4uiEXT) := wglGetProcAddress('glNamedProgramLocalParameterI4uiEXT');
    if not Assigned(glNamedProgramLocalParameterI4uiEXT) then Exit;
    Pointer(glNamedProgramLocalParameterI4uivEXT) := wglGetProcAddress('glNamedProgramLocalParameterI4uivEXT');
    if not Assigned(glNamedProgramLocalParameterI4uivEXT) then Exit;
    Pointer(glNamedProgramLocalParametersI4uivEXT) := wglGetProcAddress('glNamedProgramLocalParametersI4uivEXT');
    if not Assigned(glNamedProgramLocalParametersI4uivEXT) then Exit;
    Pointer(glGetNamedProgramLocalParameterIivEXT) := wglGetProcAddress('glGetNamedProgramLocalParameterIivEXT');
    if not Assigned(glGetNamedProgramLocalParameterIivEXT) then Exit;
    Pointer(glGetNamedProgramLocalParameterIuivEXT) := wglGetProcAddress('glGetNamedProgramLocalParameterIuivEXT');
    if not Assigned(glGetNamedProgramLocalParameterIuivEXT) then Exit;
    Pointer(glTextureParameterIivEXT) := wglGetProcAddress('glTextureParameterIivEXT');
    if not Assigned(glTextureParameterIivEXT) then Exit;
    Pointer(glTextureParameterIuivEXT) := wglGetProcAddress('glTextureParameterIuivEXT');
    if not Assigned(glTextureParameterIuivEXT) then Exit;
    Pointer(glGetTextureParameterIivEXT) := wglGetProcAddress('glGetTextureParameterIivEXT');
    if not Assigned(glGetTextureParameterIivEXT) then Exit;
    Pointer(glGetTextureParameterIuivEXT) := wglGetProcAddress('glGetTextureParameterIuivEXT');
    if not Assigned(glGetTextureParameterIuivEXT) then Exit;
    Pointer(glMultiTexParameterIivEXT) := wglGetProcAddress('glMultiTexParameterIivEXT');
    if not Assigned(glMultiTexParameterIivEXT) then Exit;
    Pointer(glMultiTexParameterIuivEXT) := wglGetProcAddress('glMultiTexParameterIuivEXT');
    if not Assigned(glMultiTexParameterIuivEXT) then Exit;
    Pointer(glGetMultiTexParameterIivEXT) := wglGetProcAddress('glGetMultiTexParameterIivEXT');
    if not Assigned(glGetMultiTexParameterIivEXT) then Exit;
    Pointer(glGetMultiTexParameterIuivEXT) := wglGetProcAddress('glGetMultiTexParameterIuivEXT');
    if not Assigned(glGetMultiTexParameterIuivEXT) then Exit;
    Pointer(glProgramUniform1fEXT) := wglGetProcAddress('glProgramUniform1fEXT');
    if not Assigned(glProgramUniform1fEXT) then Exit;
    Pointer(glProgramUniform2fEXT) := wglGetProcAddress('glProgramUniform2fEXT');
    if not Assigned(glProgramUniform2fEXT) then Exit;
    Pointer(glProgramUniform3fEXT) := wglGetProcAddress('glProgramUniform3fEXT');
    if not Assigned(glProgramUniform3fEXT) then Exit;
    Pointer(glProgramUniform4fEXT) := wglGetProcAddress('glProgramUniform4fEXT');
    if not Assigned(glProgramUniform4fEXT) then Exit;
    Pointer(glProgramUniform1iEXT) := wglGetProcAddress('glProgramUniform1iEXT');
    if not Assigned(glProgramUniform1iEXT) then Exit;
    Pointer(glProgramUniform2iEXT) := wglGetProcAddress('glProgramUniform2iEXT');
    if not Assigned(glProgramUniform2iEXT) then Exit;
    Pointer(glProgramUniform3iEXT) := wglGetProcAddress('glProgramUniform3iEXT');
    if not Assigned(glProgramUniform3iEXT) then Exit;
    Pointer(glProgramUniform4iEXT) := wglGetProcAddress('glProgramUniform4iEXT');
    if not Assigned(glProgramUniform4iEXT) then Exit;
    Pointer(glProgramUniform1fvEXT) := wglGetProcAddress('glProgramUniform1fvEXT');
    if not Assigned(glProgramUniform1fvEXT) then Exit;
    Pointer(glProgramUniform2fvEXT) := wglGetProcAddress('glProgramUniform2fvEXT');
    if not Assigned(glProgramUniform2fvEXT) then Exit;
    Pointer(glProgramUniform3fvEXT) := wglGetProcAddress('glProgramUniform3fvEXT');
    if not Assigned(glProgramUniform3fvEXT) then Exit;
    Pointer(glProgramUniform4fvEXT) := wglGetProcAddress('glProgramUniform4fvEXT');
    if not Assigned(glProgramUniform4fvEXT) then Exit;
    Pointer(glProgramUniform1ivEXT) := wglGetProcAddress('glProgramUniform1ivEXT');
    if not Assigned(glProgramUniform1ivEXT) then Exit;
    Pointer(glProgramUniform2ivEXT) := wglGetProcAddress('glProgramUniform2ivEXT');
    if not Assigned(glProgramUniform2ivEXT) then Exit;
    Pointer(glProgramUniform3ivEXT) := wglGetProcAddress('glProgramUniform3ivEXT');
    if not Assigned(glProgramUniform3ivEXT) then Exit;
    Pointer(glProgramUniform4ivEXT) := wglGetProcAddress('glProgramUniform4ivEXT');
    if not Assigned(glProgramUniform4ivEXT) then Exit;
    Pointer(glProgramUniformMatrix2fvEXT) := wglGetProcAddress('glProgramUniformMatrix2fvEXT');
    if not Assigned(glProgramUniformMatrix2fvEXT) then Exit;
    Pointer(glProgramUniformMatrix3fvEXT) := wglGetProcAddress('glProgramUniformMatrix3fvEXT');
    if not Assigned(glProgramUniformMatrix3fvEXT) then Exit;
    Pointer(glProgramUniformMatrix4fvEXT) := wglGetProcAddress('glProgramUniformMatrix4fvEXT');
    if not Assigned(glProgramUniformMatrix4fvEXT) then Exit;
    Pointer(glProgramUniformMatrix2x3fvEXT) := wglGetProcAddress('glProgramUniformMatrix2x3fvEXT');
    if not Assigned(glProgramUniformMatrix2x3fvEXT) then Exit;
    Pointer(glProgramUniformMatrix3x2fvEXT) := wglGetProcAddress('glProgramUniformMatrix3x2fvEXT');
    if not Assigned(glProgramUniformMatrix3x2fvEXT) then Exit;
    Pointer(glProgramUniformMatrix2x4fvEXT) := wglGetProcAddress('glProgramUniformMatrix2x4fvEXT');
    if not Assigned(glProgramUniformMatrix2x4fvEXT) then Exit;
    Pointer(glProgramUniformMatrix4x2fvEXT) := wglGetProcAddress('glProgramUniformMatrix4x2fvEXT');
    if not Assigned(glProgramUniformMatrix4x2fvEXT) then Exit;
    Pointer(glProgramUniformMatrix3x4fvEXT) := wglGetProcAddress('glProgramUniformMatrix3x4fvEXT');
    if not Assigned(glProgramUniformMatrix3x4fvEXT) then Exit;
    Pointer(glProgramUniformMatrix4x3fvEXT) := wglGetProcAddress('glProgramUniformMatrix4x3fvEXT');
    if not Assigned(glProgramUniformMatrix4x3fvEXT) then Exit;
    Pointer(glProgramUniform1uiEXT) := wglGetProcAddress('glProgramUniform1uiEXT');
    if not Assigned(glProgramUniform1uiEXT) then Exit;
    Pointer(glProgramUniform2uiEXT) := wglGetProcAddress('glProgramUniform2uiEXT');
    if not Assigned(glProgramUniform2uiEXT) then Exit;
    Pointer(glProgramUniform3uiEXT) := wglGetProcAddress('glProgramUniform3uiEXT');
    if not Assigned(glProgramUniform3uiEXT) then Exit;
    Pointer(glProgramUniform4uiEXT) := wglGetProcAddress('glProgramUniform4uiEXT');
    if not Assigned(glProgramUniform4uiEXT) then Exit;
    Pointer(glProgramUniform1uivEXT) := wglGetProcAddress('glProgramUniform1uivEXT');
    if not Assigned(glProgramUniform1uivEXT) then Exit;
    Pointer(glProgramUniform2uivEXT) := wglGetProcAddress('glProgramUniform2uivEXT');
    if not Assigned(glProgramUniform2uivEXT) then Exit;
    Pointer(glProgramUniform3uivEXT) := wglGetProcAddress('glProgramUniform3uivEXT');
    if not Assigned(glProgramUniform3uivEXT) then Exit;
    Pointer(glProgramUniform4uivEXT) := wglGetProcAddress('glProgramUniform4uivEXT');
    if not Assigned(glProgramUniform4uivEXT) then Exit;
    Pointer(glNamedBufferDataEXT) := wglGetProcAddress('glNamedBufferDataEXT');
    if not Assigned(glNamedBufferDataEXT) then Exit;
    Pointer(glNamedBufferSubDataEXT) := wglGetProcAddress('glNamedBufferSubDataEXT');
    if not Assigned(glNamedBufferSubDataEXT) then Exit;
    Pointer(glMapNamedBufferEXT) := wglGetProcAddress('glMapNamedBufferEXT');
    if not Assigned(glMapNamedBufferEXT) then Exit;
    Pointer(glUnmapNamedBufferEXT) := wglGetProcAddress('glUnmapNamedBufferEXT');
    if not Assigned(glUnmapNamedBufferEXT) then Exit;
    Pointer(glGetNamedBufferParameterivEXT) := wglGetProcAddress('glGetNamedBufferParameterivEXT');
    if not Assigned(glGetNamedBufferParameterivEXT) then Exit;
    Pointer(glGetNamedBufferPointervEXT) := wglGetProcAddress('glGetNamedBufferPointervEXT');
    if not Assigned(glGetNamedBufferPointervEXT) then Exit;
    Pointer(glGetNamedBufferSubDataEXT) := wglGetProcAddress('glGetNamedBufferSubDataEXT');
    if not Assigned(glGetNamedBufferSubDataEXT) then Exit;
    Pointer(glTextureBufferEXT) := wglGetProcAddress('glTextureBufferEXT');
    if not Assigned(glTextureBufferEXT) then Exit;
    Pointer(glMultiTexBufferEXT) := wglGetProcAddress('glMultiTexBufferEXT');
    if not Assigned(glMultiTexBufferEXT) then Exit;
    Pointer(glNamedRenderbufferStorageEXT) := wglGetProcAddress('glNamedRenderbufferStorageEXT');
    if not Assigned(glNamedRenderbufferStorageEXT) then Exit;
    Pointer(glGetNamedRenderbufferParameterivEXT) := wglGetProcAddress('glGetNamedRenderbufferParameterivEXT');
    if not Assigned(glGetNamedRenderbufferParameterivEXT) then Exit;
    Pointer(glCheckNamedFramebufferStatusEXT) := wglGetProcAddress('glCheckNamedFramebufferStatusEXT');
    if not Assigned(glCheckNamedFramebufferStatusEXT) then Exit;
    Pointer(glNamedFramebufferTexture1DEXT) := wglGetProcAddress('glNamedFramebufferTexture1DEXT');
    if not Assigned(glNamedFramebufferTexture1DEXT) then Exit;
    Pointer(glNamedFramebufferTexture2DEXT) := wglGetProcAddress('glNamedFramebufferTexture2DEXT');
    if not Assigned(glNamedFramebufferTexture2DEXT) then Exit;
    Pointer(glNamedFramebufferTexture3DEXT) := wglGetProcAddress('glNamedFramebufferTexture3DEXT');
    if not Assigned(glNamedFramebufferTexture3DEXT) then Exit;
    Pointer(glNamedFramebufferRenderbufferEXT) := wglGetProcAddress('glNamedFramebufferRenderbufferEXT');
    if not Assigned(glNamedFramebufferRenderbufferEXT) then Exit;
    Pointer(glGetNamedFramebufferAttachmentParameterivEXT) := wglGetProcAddress('glGetNamedFramebufferAttachmentParameterivEXT');
    if not Assigned(glGetNamedFramebufferAttachmentParameterivEXT) then Exit;
    Pointer(glGenerateTextureMipmapEXT) := wglGetProcAddress('glGenerateTextureMipmapEXT');
    if not Assigned(glGenerateTextureMipmapEXT) then Exit;
    Pointer(glGenerateMultiTexMipmapEXT) := wglGetProcAddress('glGenerateMultiTexMipmapEXT');
    if not Assigned(glGenerateMultiTexMipmapEXT) then Exit;
    Pointer(glFramebufferDrawBufferEXT) := wglGetProcAddress('glFramebufferDrawBufferEXT');
    if not Assigned(glFramebufferDrawBufferEXT) then Exit;
    Pointer(glFramebufferDrawBuffersEXT) := wglGetProcAddress('glFramebufferDrawBuffersEXT');
    if not Assigned(glFramebufferDrawBuffersEXT) then Exit;
    Pointer(glFramebufferReadBufferEXT) := wglGetProcAddress('glFramebufferReadBufferEXT');
    if not Assigned(glFramebufferReadBufferEXT) then Exit;
    Pointer(glGetFramebufferParameterivEXT) := wglGetProcAddress('glGetFramebufferParameterivEXT');
    if not Assigned(glGetFramebufferParameterivEXT) then Exit;
    Pointer(glNamedRenderbufferStorageMultisampleEXT) := wglGetProcAddress('glNamedRenderbufferStorageMultisampleEXT');
    if not Assigned(glNamedRenderbufferStorageMultisampleEXT) then Exit;
    Pointer(glNamedRenderbufferStorageMultisampleCoverageEXT) := wglGetProcAddress('glNamedRenderbufferStorageMultisampleCoverageEXT');
    if not Assigned(glNamedRenderbufferStorageMultisampleCoverageEXT) then Exit;
    Pointer(glNamedFramebufferTextureEXT) := wglGetProcAddress('glNamedFramebufferTextureEXT');
    if not Assigned(glNamedFramebufferTextureEXT) then Exit;
    Pointer(glNamedFramebufferTextureLayerEXT) := wglGetProcAddress('glNamedFramebufferTextureLayerEXT');
    if not Assigned(glNamedFramebufferTextureLayerEXT) then Exit;
    Pointer(glNamedFramebufferTextureFaceEXT) := wglGetProcAddress('glNamedFramebufferTextureFaceEXT');
    if not Assigned(glNamedFramebufferTextureFaceEXT) then Exit;
    Pointer(glTextureRenderbufferEXT) := wglGetProcAddress('glTextureRenderbufferEXT');
    if not Assigned(glTextureRenderbufferEXT) then Exit;
    Pointer(glMultiTexRenderbufferEXT) := wglGetProcAddress('glMultiTexRenderbufferEXT');
    if not Assigned(glMultiTexRenderbufferEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_vertex_array_bgra(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_vertex_array_bgra', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_EXT_texture_swizzle(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_swizzle', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_explicit_multisample(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_explicit_multisample', extstring) then
  begin
    Pointer(glGetMultisamplefvNV) := wglGetProcAddress('glGetMultisamplefvNV');
    if not Assigned(glGetMultisamplefvNV) then Exit;
    Pointer(glSampleMaskIndexedNV) := wglGetProcAddress('glSampleMaskIndexedNV');
    if not Assigned(glSampleMaskIndexedNV) then Exit;
    Pointer(glTexRenderbufferNV) := wglGetProcAddress('glTexRenderbufferNV');
    if not Assigned(glTexRenderbufferNV) then Exit;
    (* Shared entry points *)
    Pointer(glGetBooleanIndexedvEXT) := wglGetProcAddress('glGetBooleanIndexedvEXT');
    if not Assigned(glGetBooleanIndexedvEXT) then Exit;
    Pointer(glGetIntegerIndexedvEXT) := wglGetProcAddress('glGetIntegerIndexedvEXT');
    if not Assigned(glGetIntegerIndexedvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_transform_feedback2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_transform_feedback2', extstring) then
  begin
    Pointer(glBindTransformFeedbackNV) := wglGetProcAddress('glBindTransformFeedbackNV');
    if not Assigned(glBindTransformFeedbackNV) then Exit;
    Pointer(glDeleteTransformFeedbacksNV) := wglGetProcAddress('glDeleteTransformFeedbacksNV');
    if not Assigned(glDeleteTransformFeedbacksNV) then Exit;
    Pointer(glGenTransformFeedbacksNV) := wglGetProcAddress('glGenTransformFeedbacksNV');
    if not Assigned(glGenTransformFeedbacksNV) then Exit;
    Pointer(glIsTransformFeedbackNV) := wglGetProcAddress('glIsTransformFeedbackNV');
    if not Assigned(glIsTransformFeedbackNV) then Exit;
    Pointer(glPauseTransformFeedbackNV) := wglGetProcAddress('glPauseTransformFeedbackNV');
    if not Assigned(glPauseTransformFeedbackNV) then Exit;
    Pointer(glResumeTransformFeedbackNV) := wglGetProcAddress('glResumeTransformFeedbackNV');
    if not Assigned(glResumeTransformFeedbackNV) then Exit;
    Pointer(glDrawTransformFeedbackNV) := wglGetProcAddress('glDrawTransformFeedbackNV');
    if not Assigned(glDrawTransformFeedbackNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_ATI_meminfo(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_meminfo', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_AMD_performance_monitor(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_AMD_performance_monitor', extstring) then
  begin
    Pointer(glGetPerfMonitorGroupsAMD) := wglGetProcAddress('glGetPerfMonitorGroupsAMD');
    if not Assigned(glGetPerfMonitorGroupsAMD) then Exit;
    Pointer(glGetPerfMonitorCountersAMD) := wglGetProcAddress('glGetPerfMonitorCountersAMD');
    if not Assigned(glGetPerfMonitorCountersAMD) then Exit;
    Pointer(glGetPerfMonitorGroupStringAMD) := wglGetProcAddress('glGetPerfMonitorGroupStringAMD');
    if not Assigned(glGetPerfMonitorGroupStringAMD) then Exit;
    Pointer(glGetPerfMonitorCounterStringAMD) := wglGetProcAddress('glGetPerfMonitorCounterStringAMD');
    if not Assigned(glGetPerfMonitorCounterStringAMD) then Exit;
    Pointer(glGetPerfMonitorCounterInfoAMD) := wglGetProcAddress('glGetPerfMonitorCounterInfoAMD');
    if not Assigned(glGetPerfMonitorCounterInfoAMD) then Exit;
    Pointer(glGenPerfMonitorsAMD) := wglGetProcAddress('glGenPerfMonitorsAMD');
    if not Assigned(glGenPerfMonitorsAMD) then Exit;
    Pointer(glDeletePerfMonitorsAMD) := wglGetProcAddress('glDeletePerfMonitorsAMD');
    if not Assigned(glDeletePerfMonitorsAMD) then Exit;
    Pointer(glSelectPerfMonitorCountersAMD) := wglGetProcAddress('glSelectPerfMonitorCountersAMD');
    if not Assigned(glSelectPerfMonitorCountersAMD) then Exit;
    Pointer(glBeginPerfMonitorAMD) := wglGetProcAddress('glBeginPerfMonitorAMD');
    if not Assigned(glBeginPerfMonitorAMD) then Exit;
    Pointer(glEndPerfMonitorAMD) := wglGetProcAddress('glEndPerfMonitorAMD');
    if not Assigned(glEndPerfMonitorAMD) then Exit;
    Pointer(glGetPerfMonitorCounterDataAMD) := wglGetProcAddress('glGetPerfMonitorCounterDataAMD');
    if not Assigned(glGetPerfMonitorCounterDataAMD) then Exit;
    Result := True;
  end;
end;

function Load_GL_AMD_texture_texture4(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_AMD_texture_texture4', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_AMD_vertex_shader_tesselator(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_AMD_vertex_shader_tesselator', extstring) then
  begin
    Pointer(glTessellationFactorAMD) := wglGetProcAddress('glTessellationFactorAMD');
    if not Assigned(glTessellationFactorAMD) then Exit;
    Pointer(glTessellationModeAMD) := wglGetProcAddress('glTessellationModeAMD');
    if not Assigned(glTessellationModeAMD) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_provoking_vertex(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_provoking_vertex', extstring) then
  begin
    Pointer(glProvokingVertexEXT) := wglGetProcAddress('glProvokingVertexEXT');
    if not Assigned(glProvokingVertexEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_texture_snorm(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_snorm', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_AMD_draw_buffers_blend(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_AMD_draw_buffers_blend', extstring) then
  begin
    Pointer(glBlendFuncIndexedAMD) := wglGetProcAddress('glBlendFuncIndexedAMD');
    if not Assigned(glBlendFuncIndexedAMD) then Exit;
    Pointer(glBlendFuncSeparateIndexedAMD) := wglGetProcAddress('glBlendFuncSeparateIndexedAMD');
    if not Assigned(glBlendFuncSeparateIndexedAMD) then Exit;
    Pointer(glBlendEquationIndexedAMD) := wglGetProcAddress('glBlendEquationIndexedAMD');
    if not Assigned(glBlendEquationIndexedAMD) then Exit;
    Pointer(glBlendEquationSeparateIndexedAMD) := wglGetProcAddress('glBlendEquationSeparateIndexedAMD');
    if not Assigned(glBlendEquationSeparateIndexedAMD) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_texture_range(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_texture_range', extstring) then
  begin
    Pointer(glTextureRangeAPPLE) := wglGetProcAddress('glTextureRangeAPPLE');
    if not Assigned(glTextureRangeAPPLE) then Exit;
    Pointer(glGetTexParameterPointervAPPLE) := wglGetProcAddress('glGetTexParameterPointervAPPLE');
    if not Assigned(glGetTexParameterPointervAPPLE) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_float_pixels(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_float_pixels', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_APPLE_vertex_program_evaluators(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_vertex_program_evaluators', extstring) then
  begin
    Pointer(glEnableVertexAttribAPPLE) := wglGetProcAddress('glEnableVertexAttribAPPLE');
    if not Assigned(glEnableVertexAttribAPPLE) then Exit;
    Pointer(glDisableVertexAttribAPPLE) := wglGetProcAddress('glDisableVertexAttribAPPLE');
    if not Assigned(glDisableVertexAttribAPPLE) then Exit;
    Pointer(glIsVertexAttribEnabledAPPLE) := wglGetProcAddress('glIsVertexAttribEnabledAPPLE');
    if not Assigned(glIsVertexAttribEnabledAPPLE) then Exit;
    Pointer(glMapVertexAttrib1dAPPLE) := wglGetProcAddress('glMapVertexAttrib1dAPPLE');
    if not Assigned(glMapVertexAttrib1dAPPLE) then Exit;
    Pointer(glMapVertexAttrib1fAPPLE) := wglGetProcAddress('glMapVertexAttrib1fAPPLE');
    if not Assigned(glMapVertexAttrib1fAPPLE) then Exit;
    Pointer(glMapVertexAttrib2dAPPLE) := wglGetProcAddress('glMapVertexAttrib2dAPPLE');
    if not Assigned(glMapVertexAttrib2dAPPLE) then Exit;
    Pointer(glMapVertexAttrib2fAPPLE) := wglGetProcAddress('glMapVertexAttrib2fAPPLE');
    if not Assigned(glMapVertexAttrib2fAPPLE) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_aux_depth_stencil(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_aux_depth_stencil', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_APPLE_object_purgeable(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_object_purgeable', extstring) then
  begin
    Pointer(glObjectPurgeableAPPLE) := wglGetProcAddress('glObjectPurgeableAPPLE');
    if not Assigned(glObjectPurgeableAPPLE) then Exit;
    Pointer(glObjectUnpurgeableAPPLE) := wglGetProcAddress('glObjectUnpurgeableAPPLE');
    if not Assigned(glObjectUnpurgeableAPPLE) then Exit;
    Pointer(glGetObjectParameterivAPPLE) := wglGetProcAddress('glGetObjectParameterivAPPLE');
    if not Assigned(glGetObjectParameterivAPPLE) then Exit;
    Result := True;
  end;
end;

function Load_GL_APPLE_row_bytes(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_row_bytes', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_APPLE_rgb_422(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_rgb_422', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_video_capture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_video_capture', extstring) then
  begin
    Pointer(glBeginVideoCaptureNV) := wglGetProcAddress('glBeginVideoCaptureNV');
    if not Assigned(glBeginVideoCaptureNV) then Exit;
    Pointer(glBindVideoCaptureStreamBufferNV) := wglGetProcAddress('glBindVideoCaptureStreamBufferNV');
    if not Assigned(glBindVideoCaptureStreamBufferNV) then Exit;
    Pointer(glBindVideoCaptureStreamTextureNV) := wglGetProcAddress('glBindVideoCaptureStreamTextureNV');
    if not Assigned(glBindVideoCaptureStreamTextureNV) then Exit;
    Pointer(glEndVideoCaptureNV) := wglGetProcAddress('glEndVideoCaptureNV');
    if not Assigned(glEndVideoCaptureNV) then Exit;
    Pointer(glGetVideoCaptureivNV) := wglGetProcAddress('glGetVideoCaptureivNV');
    if not Assigned(glGetVideoCaptureivNV) then Exit;
    Pointer(glGetVideoCaptureStreamivNV) := wglGetProcAddress('glGetVideoCaptureStreamivNV');
    if not Assigned(glGetVideoCaptureStreamivNV) then Exit;
    Pointer(glGetVideoCaptureStreamfvNV) := wglGetProcAddress('glGetVideoCaptureStreamfvNV');
    if not Assigned(glGetVideoCaptureStreamfvNV) then Exit;
    Pointer(glGetVideoCaptureStreamdvNV) := wglGetProcAddress('glGetVideoCaptureStreamdvNV');
    if not Assigned(glGetVideoCaptureStreamdvNV) then Exit;
    Pointer(glVideoCaptureNV) := wglGetProcAddress('glVideoCaptureNV');
    if not Assigned(glVideoCaptureNV) then Exit;
    Pointer(glVideoCaptureStreamParameterivNV) := wglGetProcAddress('glVideoCaptureStreamParameterivNV');
    if not Assigned(glVideoCaptureStreamParameterivNV) then Exit;
    Pointer(glVideoCaptureStreamParameterfvNV) := wglGetProcAddress('glVideoCaptureStreamParameterfvNV');
    if not Assigned(glVideoCaptureStreamParameterfvNV) then Exit;
    Pointer(glVideoCaptureStreamParameterdvNV) := wglGetProcAddress('glVideoCaptureStreamParameterdvNV');
    if not Assigned(glVideoCaptureStreamParameterdvNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_copy_image(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_copy_image', extstring) then
  begin
    Pointer(glCopyImageSubDataNV) := wglGetProcAddress('glCopyImageSubDataNV');
    if not Assigned(glCopyImageSubDataNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_separate_shader_objects(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_separate_shader_objects', extstring) then
  begin
    Pointer(glUseShaderProgramEXT) := wglGetProcAddress('glUseShaderProgramEXT');
    if not Assigned(glUseShaderProgramEXT) then Exit;
    Pointer(glActiveProgramEXT) := wglGetProcAddress('glActiveProgramEXT');
    if not Assigned(glActiveProgramEXT) then Exit;
    Pointer(glCreateShaderProgramEXT) := wglGetProcAddress('glCreateShaderProgramEXT');
    if not Assigned(glCreateShaderProgramEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_parameter_buffer_object2(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_parameter_buffer_object2', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_NV_shader_buffer_load(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_shader_buffer_load', extstring) then
  begin
    Pointer(glMakeBufferResidentNV) := wglGetProcAddress('glMakeBufferResidentNV');
    if not Assigned(glMakeBufferResidentNV) then Exit;
    Pointer(glMakeBufferNonResidentNV) := wglGetProcAddress('glMakeBufferNonResidentNV');
    if not Assigned(glMakeBufferNonResidentNV) then Exit;
    Pointer(glIsBufferResidentNV) := wglGetProcAddress('glIsBufferResidentNV');
    if not Assigned(glIsBufferResidentNV) then Exit;
    Pointer(glMakeNamedBufferResidentNV) := wglGetProcAddress('glMakeNamedBufferResidentNV');
    if not Assigned(glMakeNamedBufferResidentNV) then Exit;
    Pointer(glMakeNamedBufferNonResidentNV) := wglGetProcAddress('glMakeNamedBufferNonResidentNV');
    if not Assigned(glMakeNamedBufferNonResidentNV) then Exit;
    Pointer(glIsNamedBufferResidentNV) := wglGetProcAddress('glIsNamedBufferResidentNV');
    if not Assigned(glIsNamedBufferResidentNV) then Exit;
    Pointer(glGetBufferParameterui64vNV) := wglGetProcAddress('glGetBufferParameterui64vNV');
    if not Assigned(glGetBufferParameterui64vNV) then Exit;
    Pointer(glGetNamedBufferParameterui64vNV) := wglGetProcAddress('glGetNamedBufferParameterui64vNV');
    if not Assigned(glGetNamedBufferParameterui64vNV) then Exit;
    Pointer(glGetIntegerui64vNV) := wglGetProcAddress('glGetIntegerui64vNV');
    if not Assigned(glGetIntegerui64vNV) then Exit;
    Pointer(glUniformui64NV) := wglGetProcAddress('glUniformui64NV');
    if not Assigned(glUniformui64NV) then Exit;
    Pointer(glUniformui64vNV) := wglGetProcAddress('glUniformui64vNV');
    if not Assigned(glUniformui64vNV) then Exit;
    Pointer(glGetUniformui64vNV) := wglGetProcAddress('glGetUniformui64vNV');
    if not Assigned(glGetUniformui64vNV) then Exit;
    Pointer(glProgramUniformui64NV) := wglGetProcAddress('glProgramUniformui64NV');
    if not Assigned(glProgramUniformui64NV) then Exit;
    Pointer(glProgramUniformui64vNV) := wglGetProcAddress('glProgramUniformui64vNV');
    if not Assigned(glProgramUniformui64vNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_vertex_buffer_unified_memory(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_buffer_unified_memory', extstring) then
  begin
    Pointer(glBufferAddressRangeNV) := wglGetProcAddress('glBufferAddressRangeNV');
    if not Assigned(glBufferAddressRangeNV) then Exit;
    Pointer(glVertexFormatNV) := wglGetProcAddress('glVertexFormatNV');
    if not Assigned(glVertexFormatNV) then Exit;
    Pointer(glNormalFormatNV) := wglGetProcAddress('glNormalFormatNV');
    if not Assigned(glNormalFormatNV) then Exit;
    Pointer(glColorFormatNV) := wglGetProcAddress('glColorFormatNV');
    if not Assigned(glColorFormatNV) then Exit;
    Pointer(glIndexFormatNV) := wglGetProcAddress('glIndexFormatNV');
    if not Assigned(glIndexFormatNV) then Exit;
    Pointer(glTexCoordFormatNV) := wglGetProcAddress('glTexCoordFormatNV');
    if not Assigned(glTexCoordFormatNV) then Exit;
    Pointer(glEdgeFlagFormatNV) := wglGetProcAddress('glEdgeFlagFormatNV');
    if not Assigned(glEdgeFlagFormatNV) then Exit;
    Pointer(glSecondaryColorFormatNV) := wglGetProcAddress('glSecondaryColorFormatNV');
    if not Assigned(glSecondaryColorFormatNV) then Exit;
    Pointer(glFogCoordFormatNV) := wglGetProcAddress('glFogCoordFormatNV');
    if not Assigned(glFogCoordFormatNV) then Exit;
    Pointer(glVertexAttribFormatNV) := wglGetProcAddress('glVertexAttribFormatNV');
    if not Assigned(glVertexAttribFormatNV) then Exit;
    Pointer(glVertexAttribIFormatNV) := wglGetProcAddress('glVertexAttribIFormatNV');
    if not Assigned(glVertexAttribIFormatNV) then Exit;
    Pointer(glGetIntegerui64i_vNV) := wglGetProcAddress('glGetIntegerui64i_vNV');
    if not Assigned(glGetIntegerui64i_vNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_NV_texture_barrier(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_barrier', extstring) then
  begin
    Pointer(glTextureBarrierNV) := wglGetProcAddress('glTextureBarrierNV');
    if not Assigned(glTextureBarrierNV) then Exit;
    Result := True;
  end;
end;

function Load_GL_AMD_shader_stencil_export(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_AMD_shader_stencil_export', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_AMD_seamless_cubemap_per_texture(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_AMD_seamless_cubemap_per_texture', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_texture_select(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_texture_select', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_INGR_blend_func_separate(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_INGR_blend_func_separate', extstring) then
  begin
    Pointer(glBlendFuncSeparateINGR) := wglGetProcAddress('glBlendFuncSeparateINGR');
    if not Assigned(glBlendFuncSeparateINGR) then Exit;
    Result := True;
  end;
end;

function Load_GL_SGIX_depth_pass_instrument(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_depth_pass_instrument', extstring) then
  begin
    Result := True;
  end;
end;

function Load_GL_SGIX_igloo_interface(): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_igloo_interface', extstring) then
  begin
    Pointer(glIglooInterfaceSGIX) := wglGetProcAddress('glIglooInterfaceSGIX');
    if not Assigned(glIglooInterfaceSGIX) then Exit;
    Result := True;
  end;
end;
function glext_LoadExtension(const ext: String): Boolean;
begin
  Result := False;

  if ext = 'GL_VERSION_1_2' then Result := Load_GL_VERSION_1_2()
  else if ext = 'GL_ARB_imaging' then Result := Load_GL_ARB_imaging()
  else if ext = 'GL_VERSION_1_3' then Result := Load_GL_VERSION_1_3()
  else if ext = 'GL_VERSION_1_4' then Result := Load_GL_VERSION_1_4()
  else if ext = 'GL_VERSION_1_5' then Result := Load_GL_VERSION_1_5()
  else if ext = 'GL_VERSION_2_0' then Result := Load_GL_VERSION_2_0()
  else if ext = 'GL_VERSION_2_1' then Result := Load_GL_VERSION_2_1()
  else if ext = 'GL_VERSION_3_0' then Result := Load_GL_VERSION_3_0()
  else if ext = 'GL_VERSION_3_1' then Result := Load_GL_VERSION_3_1()
  else if ext = 'GL_VERSION_3_2' then Result := Load_GL_VERSION_3_2()
  else if ext = 'GL_VERSION_3_3' then Result := Load_GL_VERSION_3_3()
  else if ext = 'GL_VERSION_4_0' then Result := Load_GL_VERSION_4_0()
  else if ext = 'GL_ARB_multitexture' then Result := Load_GL_ARB_multitexture()
  else if ext = 'GL_ARB_transpose_matrix' then Result := Load_GL_ARB_transpose_matrix()
  else if ext = 'GL_ARB_multisample' then Result := Load_GL_ARB_multisample()
  else if ext = 'GL_ARB_texture_env_add' then Result := Load_GL_ARB_texture_env_add()
  else if ext = 'GL_ARB_texture_cube_map' then Result := Load_GL_ARB_texture_cube_map()
  else if ext = 'GL_ARB_texture_compression' then Result := Load_GL_ARB_texture_compression()
  else if ext = 'GL_ARB_texture_border_clamp' then Result := Load_GL_ARB_texture_border_clamp()
  else if ext = 'GL_ARB_point_parameters' then Result := Load_GL_ARB_point_parameters()
  else if ext = 'GL_ARB_vertex_blend' then Result := Load_GL_ARB_vertex_blend()
  else if ext = 'GL_ARB_matrix_palette' then Result := Load_GL_ARB_matrix_palette()
  else if ext = 'GL_ARB_texture_env_combine' then Result := Load_GL_ARB_texture_env_combine()
  else if ext = 'GL_ARB_texture_env_crossbar' then Result := Load_GL_ARB_texture_env_crossbar()
  else if ext = 'GL_ARB_texture_env_dot3' then Result := Load_GL_ARB_texture_env_dot3()
  else if ext = 'GL_ARB_texture_mirrored_repeat' then Result := Load_GL_ARB_texture_mirrored_repeat()
  else if ext = 'GL_ARB_depth_texture' then Result := Load_GL_ARB_depth_texture()
  else if ext = 'GL_ARB_shadow' then Result := Load_GL_ARB_shadow()
  else if ext = 'GL_ARB_shadow_ambient' then Result := Load_GL_ARB_shadow_ambient()
  else if ext = 'GL_ARB_window_pos' then Result := Load_GL_ARB_window_pos()
  else if ext = 'GL_ARB_vertex_program' then Result := Load_GL_ARB_vertex_program()
  else if ext = 'GL_ARB_fragment_program' then Result := Load_GL_ARB_fragment_program()
  else if ext = 'GL_ARB_vertex_buffer_object' then Result := Load_GL_ARB_vertex_buffer_object()
  else if ext = 'GL_ARB_occlusion_query' then Result := Load_GL_ARB_occlusion_query()
  else if ext = 'GL_ARB_shader_objects' then Result := Load_GL_ARB_shader_objects()
  else if ext = 'GL_ARB_vertex_shader' then Result := Load_GL_ARB_vertex_shader()
  else if ext = 'GL_ARB_fragment_shader' then Result := Load_GL_ARB_fragment_shader()
  else if ext = 'GL_ARB_shading_language_100' then Result := Load_GL_ARB_shading_language_100()
  else if ext = 'GL_ARB_texture_non_power_of_two' then Result := Load_GL_ARB_texture_non_power_of_two()
  else if ext = 'GL_ARB_point_sprite' then Result := Load_GL_ARB_point_sprite()
  else if ext = 'GL_ARB_fragment_program_shadow' then Result := Load_GL_ARB_fragment_program_shadow()
  else if ext = 'GL_ARB_draw_buffers' then Result := Load_GL_ARB_draw_buffers()
  else if ext = 'GL_ARB_texture_rectangle' then Result := Load_GL_ARB_texture_rectangle()
  else if ext = 'GL_ARB_color_buffer_float' then Result := Load_GL_ARB_color_buffer_float()
  else if ext = 'GL_ARB_half_float_pixel' then Result := Load_GL_ARB_half_float_pixel()
  else if ext = 'GL_ARB_texture_float' then Result := Load_GL_ARB_texture_float()
  else if ext = 'GL_ARB_pixel_buffer_object' then Result := Load_GL_ARB_pixel_buffer_object()
  else if ext = 'GL_ARB_depth_buffer_float' then Result := Load_GL_ARB_depth_buffer_float()
  else if ext = 'GL_ARB_draw_instanced' then Result := Load_GL_ARB_draw_instanced()
  else if ext = 'GL_ARB_framebuffer_object' then Result := Load_GL_ARB_framebuffer_object()
  else if ext = 'GL_ARB_framebuffer_sRGB' then Result := Load_GL_ARB_framebuffer_sRGB()
  else if ext = 'GL_ARB_geometry_shader4' then Result := Load_GL_ARB_geometry_shader4()
  else if ext = 'GL_ARB_half_float_vertex' then Result := Load_GL_ARB_half_float_vertex()
  else if ext = 'GL_ARB_instanced_arrays' then Result := Load_GL_ARB_instanced_arrays()
  else if ext = 'GL_ARB_map_buffer_range' then Result := Load_GL_ARB_map_buffer_range()
  else if ext = 'GL_ARB_texture_buffer_object' then Result := Load_GL_ARB_texture_buffer_object()
  else if ext = 'GL_ARB_texture_compression_rgtc' then Result := Load_GL_ARB_texture_compression_rgtc()
  else if ext = 'GL_ARB_texture_rg' then Result := Load_GL_ARB_texture_rg()
  else if ext = 'GL_ARB_vertex_array_object' then Result := Load_GL_ARB_vertex_array_object()
  else if ext = 'GL_ARB_uniform_buffer_object' then Result := Load_GL_ARB_uniform_buffer_object()
  else if ext = 'GL_ARB_compatibility' then Result := Load_GL_ARB_compatibility()
  else if ext = 'GL_ARB_copy_buffer' then Result := Load_GL_ARB_copy_buffer()
  else if ext = 'GL_ARB_shader_texture_lod' then Result := Load_GL_ARB_shader_texture_lod()
  else if ext = 'GL_ARB_depth_clamp' then Result := Load_GL_ARB_depth_clamp()
  else if ext = 'GL_ARB_draw_elements_base_vertex' then Result := Load_GL_ARB_draw_elements_base_vertex()
  else if ext = 'GL_ARB_fragment_coord_conventions' then Result := Load_GL_ARB_fragment_coord_conventions()
  else if ext = 'GL_ARB_provoking_vertex' then Result := Load_GL_ARB_provoking_vertex()
  else if ext = 'GL_ARB_seamless_cube_map' then Result := Load_GL_ARB_seamless_cube_map()
  else if ext = 'GL_ARB_sync' then Result := Load_GL_ARB_sync()
  else if ext = 'GL_ARB_texture_multisample' then Result := Load_GL_ARB_texture_multisample()
  else if ext = 'GL_ARB_vertex_array_bgra' then Result := Load_GL_ARB_vertex_array_bgra()
  else if ext = 'GL_ARB_draw_buffers_blend' then Result := Load_GL_ARB_draw_buffers_blend()
  else if ext = 'GL_ARB_sample_shading' then Result := Load_GL_ARB_sample_shading()
  else if ext = 'GL_ARB_texture_cube_map_array' then Result := Load_GL_ARB_texture_cube_map_array()
  else if ext = 'GL_ARB_texture_gather' then Result := Load_GL_ARB_texture_gather()
  else if ext = 'GL_ARB_texture_query_lod' then Result := Load_GL_ARB_texture_query_lod()
  else if ext = 'GL_ARB_shading_language_include' then Result := Load_GL_ARB_shading_language_include()
  else if ext = 'GL_ARB_texture_compression_bptc' then Result := Load_GL_ARB_texture_compression_bptc()
  else if ext = 'GL_ARB_blend_func_extended' then Result := Load_GL_ARB_blend_func_extended()
  else if ext = 'GL_ARB_explicit_attrib_location' then Result := Load_GL_ARB_explicit_attrib_location()
  else if ext = 'GL_ARB_occlusion_query2' then Result := Load_GL_ARB_occlusion_query2()
  else if ext = 'GL_ARB_sampler_objects' then Result := Load_GL_ARB_sampler_objects()
  else if ext = 'GL_ARB_shader_bit_encoding' then Result := Load_GL_ARB_shader_bit_encoding()
  else if ext = 'GL_ARB_texture_rgb10_a2ui' then Result := Load_GL_ARB_texture_rgb10_a2ui()
  else if ext = 'GL_ARB_texture_swizzle' then Result := Load_GL_ARB_texture_swizzle()
  else if ext = 'GL_ARB_timer_query' then Result := Load_GL_ARB_timer_query()
  else if ext = 'GL_ARB_vertex_type_2_10_10_10_rev' then Result := Load_GL_ARB_vertex_type_2_10_10_10_rev()
  else if ext = 'GL_ARB_draw_indirect' then Result := Load_GL_ARB_draw_indirect()
  else if ext = 'GL_ARB_gpu_shader5' then Result := Load_GL_ARB_gpu_shader5()
  else if ext = 'GL_ARB_gpu_shader_fp64' then Result := Load_GL_ARB_gpu_shader_fp64()
  else if ext = 'GL_ARB_shader_subroutine' then Result := Load_GL_ARB_shader_subroutine()
  else if ext = 'GL_ARB_tessellation_shader' then Result := Load_GL_ARB_tessellation_shader()
  else if ext = 'GL_ARB_texture_buffer_object_rgb32' then Result := Load_GL_ARB_texture_buffer_object_rgb32()
  else if ext = 'GL_ARB_transform_feedback2' then Result := Load_GL_ARB_transform_feedback2()
  else if ext = 'GL_ARB_transform_feedback3' then Result := Load_GL_ARB_transform_feedback3()
  else if ext = 'GL_EXT_abgr' then Result := Load_GL_EXT_abgr()
  else if ext = 'GL_EXT_blend_color' then Result := Load_GL_EXT_blend_color()
  else if ext = 'GL_EXT_polygon_offset' then Result := Load_GL_EXT_polygon_offset()
  else if ext = 'GL_EXT_texture' then Result := Load_GL_EXT_texture()
  else if ext = 'GL_EXT_texture3D' then Result := Load_GL_EXT_texture3D()
  else if ext = 'GL_SGIS_texture_filter4' then Result := Load_GL_SGIS_texture_filter4()
  else if ext = 'GL_EXT_subtexture' then Result := Load_GL_EXT_subtexture()
  else if ext = 'GL_EXT_copy_texture' then Result := Load_GL_EXT_copy_texture()
  else if ext = 'GL_EXT_histogram' then Result := Load_GL_EXT_histogram()
  else if ext = 'GL_EXT_convolution' then Result := Load_GL_EXT_convolution()
  else if ext = 'GL_SGI_color_matrix' then Result := Load_GL_SGI_color_matrix()
  else if ext = 'GL_SGI_color_table' then Result := Load_GL_SGI_color_table()
  else if ext = 'GL_SGIS_pixel_texture' then Result := Load_GL_SGIS_pixel_texture()
  else if ext = 'GL_SGIX_pixel_texture' then Result := Load_GL_SGIX_pixel_texture()
  else if ext = 'GL_SGIS_texture4D' then Result := Load_GL_SGIS_texture4D()
  else if ext = 'GL_SGI_texture_color_table' then Result := Load_GL_SGI_texture_color_table()
  else if ext = 'GL_EXT_cmyka' then Result := Load_GL_EXT_cmyka()
  else if ext = 'GL_EXT_texture_object' then Result := Load_GL_EXT_texture_object()
  else if ext = 'GL_SGIS_detail_texture' then Result := Load_GL_SGIS_detail_texture()
  else if ext = 'GL_SGIS_sharpen_texture' then Result := Load_GL_SGIS_sharpen_texture()
  else if ext = 'GL_EXT_packed_pixels' then Result := Load_GL_EXT_packed_pixels()
  else if ext = 'GL_SGIS_texture_lod' then Result := Load_GL_SGIS_texture_lod()
  else if ext = 'GL_SGIS_multisample' then Result := Load_GL_SGIS_multisample()
  else if ext = 'GL_EXT_rescale_normal' then Result := Load_GL_EXT_rescale_normal()
  else if ext = 'GL_EXT_vertex_array' then Result := Load_GL_EXT_vertex_array()
  else if ext = 'GL_EXT_misc_attribute' then Result := Load_GL_EXT_misc_attribute()
  else if ext = 'GL_SGIS_generate_mipmap' then Result := Load_GL_SGIS_generate_mipmap()
  else if ext = 'GL_SGIX_clipmap' then Result := Load_GL_SGIX_clipmap()
  else if ext = 'GL_SGIX_shadow' then Result := Load_GL_SGIX_shadow()
  else if ext = 'GL_SGIS_texture_edge_clamp' then Result := Load_GL_SGIS_texture_edge_clamp()
  else if ext = 'GL_SGIS_texture_border_clamp' then Result := Load_GL_SGIS_texture_border_clamp()
  else if ext = 'GL_EXT_blend_minmax' then Result := Load_GL_EXT_blend_minmax()
  else if ext = 'GL_EXT_blend_subtract' then Result := Load_GL_EXT_blend_subtract()
  else if ext = 'GL_EXT_blend_logic_op' then Result := Load_GL_EXT_blend_logic_op()
  else if ext = 'GL_SGIX_interlace' then Result := Load_GL_SGIX_interlace()
  else if ext = 'GL_SGIX_pixel_tiles' then Result := Load_GL_SGIX_pixel_tiles()
  else if ext = 'GL_SGIS_texture_select' then Result := Load_GL_SGIS_texture_select()
  else if ext = 'GL_SGIX_sprite' then Result := Load_GL_SGIX_sprite()
  else if ext = 'GL_SGIX_texture_multi_buffer' then Result := Load_GL_SGIX_texture_multi_buffer()
  else if ext = 'GL_EXT_point_parameters' then Result := Load_GL_EXT_point_parameters()
  else if ext = 'GL_SGIS_point_parameters' then Result := Load_GL_SGIS_point_parameters()
  else if ext = 'GL_SGIX_instruments' then Result := Load_GL_SGIX_instruments()
  else if ext = 'GL_SGIX_texture_scale_bias' then Result := Load_GL_SGIX_texture_scale_bias()
  else if ext = 'GL_SGIX_framezoom' then Result := Load_GL_SGIX_framezoom()
  else if ext = 'GL_SGIX_tag_sample_buffer' then Result := Load_GL_SGIX_tag_sample_buffer()
  else if ext = 'GL_FfdMaskSGIX' then Result := Load_GL_FfdMaskSGIX()
  else if ext = 'GL_SGIX_polynomial_ffd' then Result := Load_GL_SGIX_polynomial_ffd()
  else if ext = 'GL_SGIX_reference_plane' then Result := Load_GL_SGIX_reference_plane()
  else if ext = 'GL_SGIX_flush_raster' then Result := Load_GL_SGIX_flush_raster()
  else if ext = 'GL_SGIX_depth_texture' then Result := Load_GL_SGIX_depth_texture()
  else if ext = 'GL_SGIS_fog_function' then Result := Load_GL_SGIS_fog_function()
  else if ext = 'GL_SGIX_fog_offset' then Result := Load_GL_SGIX_fog_offset()
  else if ext = 'GL_HP_image_transform' then Result := Load_GL_HP_image_transform()
  else if ext = 'GL_HP_convolution_border_modes' then Result := Load_GL_HP_convolution_border_modes()
  else if ext = 'GL_INGR_palette_buffer' then Result := Load_GL_INGR_palette_buffer()
  else if ext = 'GL_SGIX_texture_add_env' then Result := Load_GL_SGIX_texture_add_env()
  else if ext = 'GL_EXT_color_subtable' then Result := Load_GL_EXT_color_subtable()
  else if ext = 'GL_PGI_vertex_hints' then Result := Load_GL_PGI_vertex_hints()
  else if ext = 'GL_PGI_misc_hints' then Result := Load_GL_PGI_misc_hints()
  else if ext = 'GL_EXT_paletted_texture' then Result := Load_GL_EXT_paletted_texture()
  else if ext = 'GL_EXT_clip_volume_hint' then Result := Load_GL_EXT_clip_volume_hint()
  else if ext = 'GL_SGIX_list_priority' then Result := Load_GL_SGIX_list_priority()
  else if ext = 'GL_SGIX_ir_instrument1' then Result := Load_GL_SGIX_ir_instrument1()
  else if ext = 'GL_SGIX_calligraphic_fragment' then Result := Load_GL_SGIX_calligraphic_fragment()
  else if ext = 'GL_SGIX_texture_lod_bias' then Result := Load_GL_SGIX_texture_lod_bias()
  else if ext = 'GL_SGIX_shadow_ambient' then Result := Load_GL_SGIX_shadow_ambient()
  else if ext = 'GL_EXT_index_texture' then Result := Load_GL_EXT_index_texture()
  else if ext = 'GL_EXT_index_material' then Result := Load_GL_EXT_index_material()
  else if ext = 'GL_EXT_index_func' then Result := Load_GL_EXT_index_func()
  else if ext = 'GL_EXT_index_array_formats' then Result := Load_GL_EXT_index_array_formats()
  else if ext = 'GL_EXT_compiled_vertex_array' then Result := Load_GL_EXT_compiled_vertex_array()
  else if ext = 'GL_EXT_cull_vertex' then Result := Load_GL_EXT_cull_vertex()
  else if ext = 'GL_SGIX_ycrcb' then Result := Load_GL_SGIX_ycrcb()
  else if ext = 'GL_SGIX_fragment_lighting' then Result := Load_GL_SGIX_fragment_lighting()
  else if ext = 'GL_IBM_rasterpos_clip' then Result := Load_GL_IBM_rasterpos_clip()
  else if ext = 'GL_HP_texture_lighting' then Result := Load_GL_HP_texture_lighting()
  else if ext = 'GL_EXT_draw_range_elements' then Result := Load_GL_EXT_draw_range_elements()
  else if ext = 'GL_WIN_phong_shading' then Result := Load_GL_WIN_phong_shading()
  else if ext = 'GL_WIN_specular_fog' then Result := Load_GL_WIN_specular_fog()
  else if ext = 'GL_EXT_light_texture' then Result := Load_GL_EXT_light_texture()
  else if ext = 'GL_SGIX_blend_alpha_minmax' then Result := Load_GL_SGIX_blend_alpha_minmax()
  else if ext = 'GL_SGIX_impact_pixel_texture' then Result := Load_GL_SGIX_impact_pixel_texture()
  else if ext = 'GL_EXT_bgra' then Result := Load_GL_EXT_bgra()
  else if ext = 'GL_SGIX_async' then Result := Load_GL_SGIX_async()
  else if ext = 'GL_SGIX_async_pixel' then Result := Load_GL_SGIX_async_pixel()
  else if ext = 'GL_SGIX_async_histogram' then Result := Load_GL_SGIX_async_histogram()
  else if ext = 'GL_INTEL_texture_scissor' then Result := Load_GL_INTEL_texture_scissor()
  else if ext = 'GL_INTEL_parallel_arrays' then Result := Load_GL_INTEL_parallel_arrays()
  else if ext = 'GL_HP_occlusion_test' then Result := Load_GL_HP_occlusion_test()
  else if ext = 'GL_EXT_pixel_transform' then Result := Load_GL_EXT_pixel_transform()
  else if ext = 'GL_EXT_pixel_transform_color_table' then Result := Load_GL_EXT_pixel_transform_color_table()
  else if ext = 'GL_EXT_shared_texture_palette' then Result := Load_GL_EXT_shared_texture_palette()
  else if ext = 'GL_EXT_separate_specular_color' then Result := Load_GL_EXT_separate_specular_color()
  else if ext = 'GL_EXT_secondary_color' then Result := Load_GL_EXT_secondary_color()
  else if ext = 'GL_EXT_texture_perturb_normal' then Result := Load_GL_EXT_texture_perturb_normal()
  else if ext = 'GL_EXT_multi_draw_arrays' then Result := Load_GL_EXT_multi_draw_arrays()
  else if ext = 'GL_EXT_fog_coord' then Result := Load_GL_EXT_fog_coord()
  else if ext = 'GL_REND_screen_coordinates' then Result := Load_GL_REND_screen_coordinates()
  else if ext = 'GL_EXT_coordinate_frame' then Result := Load_GL_EXT_coordinate_frame()
  else if ext = 'GL_EXT_texture_env_combine' then Result := Load_GL_EXT_texture_env_combine()
  else if ext = 'GL_APPLE_specular_vector' then Result := Load_GL_APPLE_specular_vector()
  else if ext = 'GL_APPLE_transform_hint' then Result := Load_GL_APPLE_transform_hint()
  else if ext = 'GL_SGIX_fog_scale' then Result := Load_GL_SGIX_fog_scale()
  else if ext = 'GL_SUNX_constant_data' then Result := Load_GL_SUNX_constant_data()
  else if ext = 'GL_SUN_global_alpha' then Result := Load_GL_SUN_global_alpha()
  else if ext = 'GL_SUN_triangle_list' then Result := Load_GL_SUN_triangle_list()
  else if ext = 'GL_SUN_vertex' then Result := Load_GL_SUN_vertex()
  else if ext = 'GL_EXT_blend_func_separate' then Result := Load_GL_EXT_blend_func_separate()
  else if ext = 'GL_INGR_color_clamp' then Result := Load_GL_INGR_color_clamp()
  else if ext = 'GL_INGR_interlace_read' then Result := Load_GL_INGR_interlace_read()
  else if ext = 'GL_EXT_stencil_wrap' then Result := Load_GL_EXT_stencil_wrap()
  else if ext = 'GL_EXT_422_pixels' then Result := Load_GL_EXT_422_pixels()
  else if ext = 'GL_NV_texgen_reflection' then Result := Load_GL_NV_texgen_reflection()
  else if ext = 'GL_EXT_texture_cube_map' then Result := Load_GL_EXT_texture_cube_map()
  else if ext = 'GL_SUN_convolution_border_modes' then Result := Load_GL_SUN_convolution_border_modes()
  else if ext = 'GL_EXT_texture_env_add' then Result := Load_GL_EXT_texture_env_add()
  else if ext = 'GL_EXT_texture_lod_bias' then Result := Load_GL_EXT_texture_lod_bias()
  else if ext = 'GL_EXT_texture_filter_anisotropic' then Result := Load_GL_EXT_texture_filter_anisotropic()
  else if ext = 'GL_EXT_vertex_weighting' then Result := Load_GL_EXT_vertex_weighting()
  else if ext = 'GL_NV_light_max_exponent' then Result := Load_GL_NV_light_max_exponent()
  else if ext = 'GL_NV_vertex_array_range' then Result := Load_GL_NV_vertex_array_range()
  else if ext = 'GL_NV_register_combiners' then Result := Load_GL_NV_register_combiners()
  else if ext = 'GL_NV_fog_distance' then Result := Load_GL_NV_fog_distance()
  else if ext = 'GL_NV_texgen_emboss' then Result := Load_GL_NV_texgen_emboss()
  else if ext = 'GL_NV_blend_square' then Result := Load_GL_NV_blend_square()
  else if ext = 'GL_NV_texture_env_combine4' then Result := Load_GL_NV_texture_env_combine4()
  else if ext = 'GL_MESA_resize_buffers' then Result := Load_GL_MESA_resize_buffers()
  else if ext = 'GL_MESA_window_pos' then Result := Load_GL_MESA_window_pos()
  else if ext = 'GL_EXT_texture_compression_s3tc' then Result := Load_GL_EXT_texture_compression_s3tc()
  else if ext = 'GL_IBM_cull_vertex' then Result := Load_GL_IBM_cull_vertex()
  else if ext = 'GL_IBM_multimode_draw_arrays' then Result := Load_GL_IBM_multimode_draw_arrays()
  else if ext = 'GL_IBM_vertex_array_lists' then Result := Load_GL_IBM_vertex_array_lists()
  else if ext = 'GL_SGIX_subsample' then Result := Load_GL_SGIX_subsample()
  else if ext = 'GL_SGIX_ycrcb_subsample' then Result := Load_GL_SGIX_ycrcb_subsample()
  else if ext = 'GL_SGIX_ycrcba' then Result := Load_GL_SGIX_ycrcba()
  else if ext = 'GL_SGI_depth_pass_instrument' then Result := Load_GL_SGI_depth_pass_instrument()
  else if ext = 'GL_3DFX_texture_compression_FXT1' then Result := Load_GL_3DFX_texture_compression_FXT1()
  else if ext = 'GL_3DFX_multisample' then Result := Load_GL_3DFX_multisample()
  else if ext = 'GL_3DFX_tbuffer' then Result := Load_GL_3DFX_tbuffer()
  else if ext = 'GL_EXT_multisample' then Result := Load_GL_EXT_multisample()
  else if ext = 'GL_SGIX_vertex_preclip' then Result := Load_GL_SGIX_vertex_preclip()
  else if ext = 'GL_SGIX_convolution_accuracy' then Result := Load_GL_SGIX_convolution_accuracy()
  else if ext = 'GL_SGIX_resample' then Result := Load_GL_SGIX_resample()
  else if ext = 'GL_SGIS_point_line_texgen' then Result := Load_GL_SGIS_point_line_texgen()
  else if ext = 'GL_SGIS_texture_color_mask' then Result := Load_GL_SGIS_texture_color_mask()
  else if ext = 'GL_EXT_texture_env_dot3' then Result := Load_GL_EXT_texture_env_dot3()
  else if ext = 'GL_ATI_texture_mirror_once' then Result := Load_GL_ATI_texture_mirror_once()
  else if ext = 'GL_NV_fence' then Result := Load_GL_NV_fence()
  else if ext = 'GL_IBM_texture_mirrored_repeat' then Result := Load_GL_IBM_texture_mirrored_repeat()
  else if ext = 'GL_NV_evaluators' then Result := Load_GL_NV_evaluators()
  else if ext = 'GL_NV_packed_depth_stencil' then Result := Load_GL_NV_packed_depth_stencil()
  else if ext = 'GL_NV_register_combiners2' then Result := Load_GL_NV_register_combiners2()
  else if ext = 'GL_NV_texture_compression_vtc' then Result := Load_GL_NV_texture_compression_vtc()
  else if ext = 'GL_NV_texture_rectangle' then Result := Load_GL_NV_texture_rectangle()
  else if ext = 'GL_NV_texture_shader' then Result := Load_GL_NV_texture_shader()
  else if ext = 'GL_NV_texture_shader2' then Result := Load_GL_NV_texture_shader2()
  else if ext = 'GL_NV_vertex_array_range2' then Result := Load_GL_NV_vertex_array_range2()
  else if ext = 'GL_NV_vertex_program' then Result := Load_GL_NV_vertex_program()
  else if ext = 'GL_SGIX_texture_coordinate_clamp' then Result := Load_GL_SGIX_texture_coordinate_clamp()
  else if ext = 'GL_SGIX_scalebias_hint' then Result := Load_GL_SGIX_scalebias_hint()
  else if ext = 'GL_OML_interlace' then Result := Load_GL_OML_interlace()
  else if ext = 'GL_OML_subsample' then Result := Load_GL_OML_subsample()
  else if ext = 'GL_OML_resample' then Result := Load_GL_OML_resample()
  else if ext = 'GL_NV_copy_depth_to_color' then Result := Load_GL_NV_copy_depth_to_color()
  else if ext = 'GL_ATI_envmap_bumpmap' then Result := Load_GL_ATI_envmap_bumpmap()
  else if ext = 'GL_ATI_fragment_shader' then Result := Load_GL_ATI_fragment_shader()
  else if ext = 'GL_ATI_pn_triangles' then Result := Load_GL_ATI_pn_triangles()
  else if ext = 'GL_ATI_vertex_array_object' then Result := Load_GL_ATI_vertex_array_object()
  else if ext = 'GL_EXT_vertex_shader' then Result := Load_GL_EXT_vertex_shader()
  else if ext = 'GL_ATI_vertex_streams' then Result := Load_GL_ATI_vertex_streams()
  else if ext = 'GL_ATI_element_array' then Result := Load_GL_ATI_element_array()
  else if ext = 'GL_SUN_mesh_array' then Result := Load_GL_SUN_mesh_array()
  else if ext = 'GL_SUN_slice_accum' then Result := Load_GL_SUN_slice_accum()
  else if ext = 'GL_NV_multisample_filter_hint' then Result := Load_GL_NV_multisample_filter_hint()
  else if ext = 'GL_NV_depth_clamp' then Result := Load_GL_NV_depth_clamp()
  else if ext = 'GL_NV_occlusion_query' then Result := Load_GL_NV_occlusion_query()
  else if ext = 'GL_NV_point_sprite' then Result := Load_GL_NV_point_sprite()
  else if ext = 'GL_NV_texture_shader3' then Result := Load_GL_NV_texture_shader3()
  else if ext = 'GL_NV_vertex_program1_1' then Result := Load_GL_NV_vertex_program1_1()
  else if ext = 'GL_EXT_shadow_funcs' then Result := Load_GL_EXT_shadow_funcs()
  else if ext = 'GL_EXT_stencil_two_side' then Result := Load_GL_EXT_stencil_two_side()
  else if ext = 'GL_ATI_text_fragment_shader' then Result := Load_GL_ATI_text_fragment_shader()
  else if ext = 'GL_APPLE_client_storage' then Result := Load_GL_APPLE_client_storage()
  else if ext = 'GL_APPLE_element_array' then Result := Load_GL_APPLE_element_array()
  else if ext = 'GL_APPLE_fence' then Result := Load_GL_APPLE_fence()
  else if ext = 'GL_APPLE_vertex_array_object' then Result := Load_GL_APPLE_vertex_array_object()
  else if ext = 'GL_APPLE_vertex_array_range' then Result := Load_GL_APPLE_vertex_array_range()
  else if ext = 'GL_APPLE_ycbcr_422' then Result := Load_GL_APPLE_ycbcr_422()
  else if ext = 'GL_S3_s3tc' then Result := Load_GL_S3_s3tc()
  else if ext = 'GL_ATI_draw_buffers' then Result := Load_GL_ATI_draw_buffers()
  else if ext = 'GL_ATI_pixel_format_float' then Result := Load_GL_ATI_pixel_format_float()
  else if ext = 'GL_ATI_texture_env_combine3' then Result := Load_GL_ATI_texture_env_combine3()
  else if ext = 'GL_ATI_texture_float' then Result := Load_GL_ATI_texture_float()
  else if ext = 'GL_NV_float_buffer' then Result := Load_GL_NV_float_buffer()
  else if ext = 'GL_NV_fragment_program' then Result := Load_GL_NV_fragment_program()
  else if ext = 'GL_NV_half_float' then Result := Load_GL_NV_half_float()
  else if ext = 'GL_NV_pixel_data_range' then Result := Load_GL_NV_pixel_data_range()
  else if ext = 'GL_NV_primitive_restart' then Result := Load_GL_NV_primitive_restart()
  else if ext = 'GL_NV_texture_expand_normal' then Result := Load_GL_NV_texture_expand_normal()
  else if ext = 'GL_NV_vertex_program2' then Result := Load_GL_NV_vertex_program2()
  else if ext = 'GL_ATI_map_object_buffer' then Result := Load_GL_ATI_map_object_buffer()
  else if ext = 'GL_ATI_separate_stencil' then Result := Load_GL_ATI_separate_stencil()
  else if ext = 'GL_ATI_vertex_attrib_array_object' then Result := Load_GL_ATI_vertex_attrib_array_object()
  else if ext = 'GL_OES_read_format' then Result := Load_GL_OES_read_format()
  else if ext = 'GL_EXT_depth_bounds_test' then Result := Load_GL_EXT_depth_bounds_test()
  else if ext = 'GL_EXT_texture_mirror_clamp' then Result := Load_GL_EXT_texture_mirror_clamp()
  else if ext = 'GL_EXT_blend_equation_separate' then Result := Load_GL_EXT_blend_equation_separate()
  else if ext = 'GL_MESA_pack_invert' then Result := Load_GL_MESA_pack_invert()
  else if ext = 'GL_MESA_ycbcr_texture' then Result := Load_GL_MESA_ycbcr_texture()
  else if ext = 'GL_EXT_pixel_buffer_object' then Result := Load_GL_EXT_pixel_buffer_object()
  else if ext = 'GL_NV_fragment_program_option' then Result := Load_GL_NV_fragment_program_option()
  else if ext = 'GL_NV_fragment_program2' then Result := Load_GL_NV_fragment_program2()
  else if ext = 'GL_NV_vertex_program2_option' then Result := Load_GL_NV_vertex_program2_option()
  else if ext = 'GL_NV_vertex_program3' then Result := Load_GL_NV_vertex_program3()
  else if ext = 'GL_EXT_framebuffer_object' then Result := Load_GL_EXT_framebuffer_object()
  else if ext = 'GL_GREMEDY_string_marker' then Result := Load_GL_GREMEDY_string_marker()
  else if ext = 'GL_EXT_packed_depth_stencil' then Result := Load_GL_EXT_packed_depth_stencil()
  else if ext = 'GL_EXT_stencil_clear_tag' then Result := Load_GL_EXT_stencil_clear_tag()
  else if ext = 'GL_EXT_texture_sRGB' then Result := Load_GL_EXT_texture_sRGB()
  else if ext = 'GL_EXT_framebuffer_blit' then Result := Load_GL_EXT_framebuffer_blit()
  else if ext = 'GL_EXT_framebuffer_multisample' then Result := Load_GL_EXT_framebuffer_multisample()
  else if ext = 'GL_MESAX_texture_stack' then Result := Load_GL_MESAX_texture_stack()
  else if ext = 'GL_EXT_timer_query' then Result := Load_GL_EXT_timer_query()
  else if ext = 'GL_EXT_gpu_program_parameters' then Result := Load_GL_EXT_gpu_program_parameters()
  else if ext = 'GL_APPLE_flush_buffer_range' then Result := Load_GL_APPLE_flush_buffer_range()
  else if ext = 'GL_NV_gpu_program4' then Result := Load_GL_NV_gpu_program4()
  else if ext = 'GL_NV_geometry_program4' then Result := Load_GL_NV_geometry_program4()
  else if ext = 'GL_EXT_geometry_shader4' then Result := Load_GL_EXT_geometry_shader4()
  else if ext = 'GL_NV_vertex_program4' then Result := Load_GL_NV_vertex_program4()
  else if ext = 'GL_EXT_gpu_shader4' then Result := Load_GL_EXT_gpu_shader4()
  else if ext = 'GL_EXT_draw_instanced' then Result := Load_GL_EXT_draw_instanced()
  else if ext = 'GL_EXT_packed_float' then Result := Load_GL_EXT_packed_float()
  else if ext = 'GL_EXT_texture_array' then Result := Load_GL_EXT_texture_array()
  else if ext = 'GL_EXT_texture_buffer_object' then Result := Load_GL_EXT_texture_buffer_object()
  else if ext = 'GL_EXT_texture_compression_latc' then Result := Load_GL_EXT_texture_compression_latc()
  else if ext = 'GL_EXT_texture_compression_rgtc' then Result := Load_GL_EXT_texture_compression_rgtc()
  else if ext = 'GL_EXT_texture_shared_exponent' then Result := Load_GL_EXT_texture_shared_exponent()
  else if ext = 'GL_NV_depth_buffer_float' then Result := Load_GL_NV_depth_buffer_float()
  else if ext = 'GL_NV_fragment_program4' then Result := Load_GL_NV_fragment_program4()
  else if ext = 'GL_NV_framebuffer_multisample_coverage' then Result := Load_GL_NV_framebuffer_multisample_coverage()
  else if ext = 'GL_EXT_framebuffer_sRGB' then Result := Load_GL_EXT_framebuffer_sRGB()
  else if ext = 'GL_NV_geometry_shader4' then Result := Load_GL_NV_geometry_shader4()
  else if ext = 'GL_NV_parameter_buffer_object' then Result := Load_GL_NV_parameter_buffer_object()
  else if ext = 'GL_EXT_draw_buffers2' then Result := Load_GL_EXT_draw_buffers2()
  else if ext = 'GL_NV_transform_feedback' then Result := Load_GL_NV_transform_feedback()
  else if ext = 'GL_EXT_bindable_uniform' then Result := Load_GL_EXT_bindable_uniform()
  else if ext = 'GL_EXT_texture_integer' then Result := Load_GL_EXT_texture_integer()
  else if ext = 'GL_GREMEDY_frame_terminator' then Result := Load_GL_GREMEDY_frame_terminator()
  else if ext = 'GL_NV_conditional_render' then Result := Load_GL_NV_conditional_render()
  else if ext = 'GL_NV_present_video' then Result := Load_GL_NV_present_video()
  else if ext = 'GL_EXT_transform_feedback' then Result := Load_GL_EXT_transform_feedback()
  else if ext = 'GL_EXT_direct_state_access' then Result := Load_GL_EXT_direct_state_access()
  else if ext = 'GL_EXT_vertex_array_bgra' then Result := Load_GL_EXT_vertex_array_bgra()
  else if ext = 'GL_EXT_texture_swizzle' then Result := Load_GL_EXT_texture_swizzle()
  else if ext = 'GL_NV_explicit_multisample' then Result := Load_GL_NV_explicit_multisample()
  else if ext = 'GL_NV_transform_feedback2' then Result := Load_GL_NV_transform_feedback2()
  else if ext = 'GL_ATI_meminfo' then Result := Load_GL_ATI_meminfo()
  else if ext = 'GL_AMD_performance_monitor' then Result := Load_GL_AMD_performance_monitor()
  else if ext = 'GL_AMD_texture_texture4' then Result := Load_GL_AMD_texture_texture4()
  else if ext = 'GL_AMD_vertex_shader_tesselator' then Result := Load_GL_AMD_vertex_shader_tesselator()
  else if ext = 'GL_EXT_provoking_vertex' then Result := Load_GL_EXT_provoking_vertex()
  else if ext = 'GL_EXT_texture_snorm' then Result := Load_GL_EXT_texture_snorm()
  else if ext = 'GL_AMD_draw_buffers_blend' then Result := Load_GL_AMD_draw_buffers_blend()
  else if ext = 'GL_APPLE_texture_range' then Result := Load_GL_APPLE_texture_range()
  else if ext = 'GL_APPLE_float_pixels' then Result := Load_GL_APPLE_float_pixels()
  else if ext = 'GL_APPLE_vertex_program_evaluators' then Result := Load_GL_APPLE_vertex_program_evaluators()
  else if ext = 'GL_APPLE_aux_depth_stencil' then Result := Load_GL_APPLE_aux_depth_stencil()
  else if ext = 'GL_APPLE_object_purgeable' then Result := Load_GL_APPLE_object_purgeable()
  else if ext = 'GL_APPLE_row_bytes' then Result := Load_GL_APPLE_row_bytes()
  else if ext = 'GL_APPLE_rgb_422' then Result := Load_GL_APPLE_rgb_422()
  else if ext = 'GL_NV_video_capture' then Result := Load_GL_NV_video_capture()
  else if ext = 'GL_NV_copy_image' then Result := Load_GL_NV_copy_image()
  else if ext = 'GL_EXT_separate_shader_objects' then Result := Load_GL_EXT_separate_shader_objects()
  else if ext = 'GL_NV_parameter_buffer_object2' then Result := Load_GL_NV_parameter_buffer_object2()
  else if ext = 'GL_NV_shader_buffer_load' then Result := Load_GL_NV_shader_buffer_load()
  else if ext = 'GL_NV_vertex_buffer_unified_memory' then Result := Load_GL_NV_vertex_buffer_unified_memory()
  else if ext = 'GL_NV_texture_barrier' then Result := Load_GL_NV_texture_barrier()
  else if ext = 'GL_AMD_shader_stencil_export' then Result := Load_GL_AMD_shader_stencil_export()
  else if ext = 'GL_AMD_seamless_cubemap_per_texture' then Result := Load_GL_AMD_seamless_cubemap_per_texture()
  else if ext = 'GL_SGIX_texture_select' then Result := Load_GL_SGIX_texture_select()
  else if ext = 'GL_INGR_blend_func_separate' then Result := Load_GL_INGR_blend_func_separate()
  else if ext = 'GL_SGIX_depth_pass_instrument' then Result := Load_GL_SGIX_depth_pass_instrument()
  else if ext = 'GL_SGIX_igloo_interface' then Result := Load_GL_SGIX_igloo_interface()
end;

end.

