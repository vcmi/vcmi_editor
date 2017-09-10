{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

}
unit vcmi.glwin32wglcontext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, Windows, LCLProc, LCLType, gl, Forms, Controls, 
  Win32Int, WSLCLClasses, WSControls, Win32WSControls, Win32Proc, LCLMessageGlue,
  vcmi.wgl;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLReleaseContext(Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
                    WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
                    DoubleBuffered, RGBA, DebugContext: boolean;
                    const RedBits, GreenBits, BlueBits,
                    MajorVersion, MinorVersion,
                    MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
                    const AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);


//procedure InitOpenGLContextGLWindowClass;


type
  TWGLControlInfo = record
    Window: HWND;
    DC: HDC;
    PixelFormat: GLUInt;
    WGLContext: HGLRC;
  end;
  PWGLControlInfo = ^TWGLControlInfo;

var
  WGLControlInfoAtom: ATOM = 0;

function AllocWGLControlInfo(Window: HWND): PWGLControlInfo;
function DisposeWGLControlInfo(Window: HWND): boolean;
function GetWGLControlInfo(Window: HWND): PWGLControlInfo;

//var
//  OpenGLContextWindowClassInitialized: boolean = false;
//  OpenGLContextWindowClass: WNDCLASS;

const
  DefaultOpenGLContextInitAttrList: array [0..0] of LongInt = (
    0
    );

implementation
uses vcmi.glext;

function GLGetProcAddress(ProcName: PChar):Pointer;
begin
  Result := wglGetProcAddress(ProcName);
end;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
var
  Info: PWGLControlInfo;
begin
  Info:=GetWGLControlInfo(Handle);
  // don't use wglSwapLayerBuffers or wglSwapBuffers!
  SwapBuffers(Info^.DC);
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  Info: PWGLControlInfo;
begin
  Info:=GetWGLControlInfo(Handle);
  Result:=wglMakeCurrent(Info^.DC,Info^.WGLContext);
end;

function LOpenGLReleaseContext(Handle: HWND): boolean;
begin
  Result:=wglMakeCurrent(0,0);
end;

function GlWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  PaintMsg   : TLMPaint;
  winctrl    : TWinControl;
begin
  case Msg of 
    WM_ERASEBKGND: begin
      Result:=0;
    end;
    WM_PAINT: begin 
      winctrl := GetWin32WindowInfo(Window)^.WinControl;
      if Assigned(winctrl) then begin
        FillChar(PaintMsg, SizeOf(PaintMsg), 0);
        PaintMsg.Msg := LM_PAINT;
        PaintMsg.DC := WParam;
        DeliverMessage(winctrl, PaintMsg);
        Result:=PaintMsg.Result;
      end else 
        Result:=WindowProc(Window, Msg, WParam, LParam);
    end;
  else
    Result:=WindowProc(Window, Msg, WParam, LParam);
  end;
end;

function LGlMsCreateOpenGLContextAttrList(DoubleBuffered: boolean; RGBA: boolean;
  const RedBits, GreenBits, BlueBits, MultiSampling, AlphaBits, DepthBits,
  StencilBits, AUXBuffers: Cardinal): PInteger;
var
  p: integer;

  procedure Add(i: integer);
  begin
    if Result<>nil then
      Result[p]:=i;
    inc(p);
  end;

  procedure CreateList;
  begin
    Add(WGL_DRAW_TO_WINDOW_ARB); Add(GL_TRUE);
    Add(WGL_SUPPORT_OPENGL_ARB); Add(GL_TRUE);
    Add(WGL_ACCELERATION_ARB); Add(WGL_FULL_ACCELERATION_ARB);
    if DoubleBuffered then
      begin Add(WGL_DOUBLE_BUFFER_ARB); Add(GL_TRUE); end;
    Add(WGL_PIXEL_TYPE_ARB);
    if RGBA then
      Add(WGL_TYPE_RGBA_ARB)
    else
      Add(WGL_TYPE_COLORINDEX_ARB);

    Add(WGL_RED_BITS_ARB);  Add(RedBits);
    Add(WGL_GREEN_BITS_ARB);  Add(GreenBits);
    Add(WGL_BLUE_BITS_ARB);  Add(BlueBits);
    Add(WGL_COLOR_BITS_ARB);  Add(RedBits+GreenBits+BlueBits);
    Add(WGL_ALPHA_BITS_ARB);  Add(AlphaBits);
    Add(WGL_DEPTH_BITS_ARB);  Add(DepthBits);
    Add(WGL_STENCIL_BITS_ARB);  Add(StencilBits);
    Add(WGL_AUX_BUFFERS_ARB);  Add(AUXBuffers);
    if MultiSampling > 1 then
    begin
      Add(WGL_SAMPLE_BUFFERS_ARB); Add(1);
      Add(WGL_SAMPLES_ARB);        Add(MultiSampling);
    end;
    Add(0); Add(0);
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  p:=0;
  CreateList;
end;

function LOpenGLCreateContext(AWinControl: TWinControl; WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, RGBA, DebugContext: boolean; const RedBits, GreenBits, BlueBits, MajorVersion, MinorVersion,
  MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal; const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  pfd: PIXELFORMATDESCRIPTOR;
  Info, SharedInfo: PWGLControlInfo;

  ReturnedFormats: UINT;
  VisualAttrList: PInteger;
  VisualAttrFloat: array [0..1] of Single;
  MsInitSuccess: WINBOOL;
  FailReason : string;
  attribList : array [0..6] of GLint;

  ContextFlags: Cardinal;
begin
  InitWGL(true);
  //InitOpenGLContextGLWindowClass;
  
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do begin
    pClassName := @ClsName;
    WindowTitle := StrCaption;
    SubClassWndProc := @GlWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  
  // create info
  Info:=AllocWGLControlInfo(Result);

  // create device context
  Info^.DC := GetDC(Result);
  if Info^.DC=0 then
    raise Exception.Create('LOpenGLCreateContext GetDC failed');

  // get pixelformat
  FillChar(pfd,SizeOf(pfd),0);
  with pfd do begin
    nSize:=sizeOf(pfd);
    nVersion:=1;
    dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;
    if DoubleBuffered then
      dwFlags:=dwFlags or PFD_DOUBLEBUFFER;
    if RGBA then
      iPixelType:=PFD_TYPE_RGBA
    else
      iPixelType:=PFD_TYPE_COLORINDEX;
    cColorBits:=RedBits+GreenBits+BlueBits; // color depth
    cRedBits:=RedBits;
    cGreenBits:=GreenBits;
    cBlueBits:=BlueBits;
    cAlphaBits:=AlphaBits;
    cDepthBits:=DepthBits; // Z-Buffer
    cStencilBits:=StencilBits;
    cAuxBuffers:=AUXBuffers;
    iLayerType:=PFD_MAIN_PLANE;
  end;

  MsInitSuccess := false;
  if (MultiSampling > 1) and WGL_ARB_multisample and WGL_ARB_pixel_format
    and Assigned(wglChoosePixelFormatARB) then
  begin
    VisualAttrList := LGlMsCreateOpenGLContextAttrList(DoubleBuffered, RGBA,
      RedBits, GreenBits, BlueBits, MultiSampling, AlphaBits, DepthBits,
      StencilBits, AUXBuffers);
    try
      FillChar(VisualAttrFloat, SizeOf(VisualAttrFloat), 0);
      MsInitSuccess := wglChoosePixelFormatARB(Info^.DC, PGLint(VisualAttrList),
                         @VisualAttrFloat[0], 1, @Info^.PixelFormat, @ReturnedFormats);
    finally FreeMem(VisualAttrList) end;

    if MsInitSuccess and (ReturnedFormats >= 1) then
       SetPixelFormat(Info^.DC, Info^.PixelFormat, nil)
    else
       MsInitSuccess := false;
  end;

  if not MsInitSuccess then
  begin
    Info^.PixelFormat:=ChoosePixelFormat(Info^.DC,@pfd);
    if Info^.PixelFormat=0 then
      raise Exception.Create('LOpenGLCreateContext ChoosePixelFormat failed');

    // set pixel format in device context
    if not SetPixelFormat(Info^.DC,Info^.PixelFormat,@pfd) then
      raise Exception.Create('LOpenGLCreateContext SetPixelFormat failed');
  end;

  ContextFlags := WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB; //TODO: use WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB of not?

  if DebugContext then
  begin
    ContextFlags := ContextFlags or WGL_CONTEXT_DEBUG_BIT_ARB;
  end;

  // create WGL context
  Info^.WGLContext:=0;
  if wglCreateContextAttribsARB = nil then
  begin
    FailReason:='wglCreateContextAttribsARB not supported';
  end
  else
  begin
    // try to create debug context
    attribList[0]:=WGL_CONTEXT_FLAGS_ARB;
    attribList[1]:=ContextFlags;
    attribList[2]:=WGL_CONTEXT_MAJOR_VERSION_ARB;
    if MajorVersion = 0 then
    begin
      attribList[3]:=1;
    end
    else
    begin
      attribList[3]:= MajorVersion;
    end;
    attribList[4]:=WGL_CONTEXT_MINOR_VERSION_ARB;
    attribList[5]:=MinorVersion;
    attribList[6]:=0;
    Info^.WGLContext:=wglCreateContextAttribsARB(Info^.DC, 0, @attribList);
    FailReason:='wglCreateContextAttribsARB failed';
  end;

  if Info^.WGLContext=0 then
    raise Exception.CreateFmt('LOpenGLCreateContext: %s', [FailReason]);

  // share context objects
  if Assigned(SharedControl) then begin
    SharedInfo:=GetWGLControlInfo(SharedControl.Handle);
    if Assigned(SharedInfo) then wglShareLists(SharedInfo^.WGLContext, Info^.WGLContext);
  end;
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
var
  Info: PWGLControlInfo;
begin
  if not AWinControl.HandleAllocated then exit;
  Info:=GetWGLControlInfo(AWinControl.Handle);
  if Info=nil then exit;
  if wglMakeCurrent(Info^.DC,Info^.WGLContext) then begin
    wglDeleteContext(Info^.WGLContext);
    Info^.WGLContext:=0;
  end;
  if (Info^.DC<>0) then begin
    ReleaseDC(Info^.Window,Info^.DC);
  end;
  DisposeWGLControlInfo(Info^.Window);
end;


//procedure InitOpenGLContextGLWindowClass;
//begin
//  if OpenGLContextWindowClassInitialized then exit;
//  with OpenGLContextWindowClass do begin
//    style:=CS_HREDRAW or CS_VREDRAW or CS_OWNDC;// Redraw On Move, And Own DC For Window
//    lpfnWndProc  := @WindowProc;                // WndProc Handles Messages
//    cbClsExtra   := 0;                          // No Extra Window Data
//    cbWndExtra   := 0;                          // No Extra Window Data
//    hInstance    := System.HInstance;           // Set The Instance
//    hIcon        := LoadIcon(NULL, IDI_WINLOGO);// Load The Default Icon
//    hCursor      := LoadCursor(NULL, IDC_ARROW);// Load The Arrow Pointer
//    hbrBackground:= NULL;                       // No Background Required For GL
//    lpszMenuName := nil;                       // We Don't Want A Menu
//    lpszClassName:= 'LazOpenGLContext';         // Set The Class Name
//  end;
//  if RegisterClass(@OpenGLContextWindowClass)=0 then
//    raise Exception.Create('registering OpenGLContextWindowClass failed');
//
//  OpenGLContextWindowClassInitialized:=true;
//end;

function AllocWGLControlInfo(Window: HWND): PWGLControlInfo;
begin
  New(Result);
  FillChar(Result^, sizeof(Result^), 0);
  Result^.Window := Window;
  if WGLControlInfoAtom=0 then
    WGLControlInfoAtom := Windows.GlobalAddAtom('WGLControlInfo');
  Windows.SetProp(Window, PChar(PtrUInt(WGLControlInfoAtom)), PtrUInt(Result));
end;

function DisposeWGLControlInfo(Window: HWND): boolean;
var
  Info: PWGLControlInfo;
begin
  Info := PWGLControlInfo(Windows.GetProp(Window,
                                          PChar(PtrUInt(WGLControlInfoAtom))));
  Result := Windows.RemoveProp(Window, PChar(PtrUInt(WGLControlInfoAtom)))<>0;
  if Result then begin
    Dispose(Info);
  end;
end;

function GetWGLControlInfo(Window: HWND): PWGLControlInfo;
begin
  Result:=PWGLControlInfo(Windows.GetProp(Window,
                                          PChar(PtrUInt(WGLControlInfoAtom))));
end;

end.

