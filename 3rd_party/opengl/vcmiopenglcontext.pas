{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vcmiOpenGLContext;

interface

uses
  vcmi.openglcontext, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('vcmi.openglcontext', @vcmi.openglcontext.Register);
end;

initialization
  RegisterPackage('vcmiOpenGLContext', @Register);
end.
