unit vcmi.image_loaders;

{ This file is a part of Map editor for VCMI project.

  Copyright (C) 2016 Alexander Shishkin alexvins@users.sourceforge.net

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

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils,
  FPimage,
  FPReadBMP,FPReadJPEG,FPReadPNG,FPReadTGA,FPReadTiff,
  Graphics, IntfGraphics, GraphType, LazLoggerBase, filesystem_base, vcmi.image_formats.h3pcx;

type

  { TImageResource }

  TImageResource = class(TBaseResource, IResource)
  private
    FData: TPicture;
  public
    constructor Create(APath: AnsiString);
    destructor Destroy; override;

    procedure LoadFromStream(AFileName: AnsiString; AStream: TStream); override;

    property Data: TPicture read FData;
  end;

  { TIntfImageResource }

  //RGBA intf bitmap, temprory storage for load into video memory

  TIntfImageResource = class(TBaseResource, IResource)
  private
    FData: TLazIntfImage;
    FRawImage: TRawImage;
  public
    constructor Create(APath: AnsiString);
    destructor Destroy; override;

    procedure LoadFromStream(AFileName: AnsiString; AStream: TStream); override;

    property Data: TLazIntfImage read FData;
  end;

implementation

{ TIntfImageResource }

constructor TIntfImageResource.Create(APath: AnsiString);
begin
  inherited Create(TResourceType.Image, APath);
  FData := TLazIntfImage.Create(0,0);

  FRawImage.Init;
  FRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(0,0);
  FRawImage.CreateData(false);

  FData.SetRawImage(FRawImage);
end;

destructor TIntfImageResource.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TIntfImageResource.LoadFromStream(AFileName: AnsiString; AStream: TStream);
var
  ext: AnsiString;
  r: TFPCustomImageReader;
begin
  ext:=UpperCase(ExtractFileExt(AFileName));

  if ext = '.PNG' then
  begin
    r := TFPReaderPNG.create;
    FData.LoadFromStream(AStream, r);

    r.Free;
  end
  else if ext = '.PCX' then
  begin
    //todo:8|24 bit h3 pcx -> 32 bit texture
    assert(false, 'not implemented')
  end
  else
  begin
    //TODO:
    assert(false, 'not implemented')
  end;
end;

{ TImageResource }

constructor TImageResource.Create(APath: AnsiString);
begin
  inherited Create(TResourceType.Image, APath);

  FData := TPicture.Create;
end;

destructor TImageResource.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TImageResource.LoadFromStream(AFileName: AnsiString; AStream: TStream);
var
  ext: AnsiString;
begin
  ext:=UpperCase(ExtractFileExt(AFileName));

  if ext <> '.PCX' then
  begin
    //assume anything else is supported out of the box
    FData.LoadFromStreamWithFileExt(AStream, ext);
  end
  else
  begin
    LoadH3Pcx(AStream, FData);
  end;
end;

end.

