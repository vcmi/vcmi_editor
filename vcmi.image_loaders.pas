unit vcmi.image_loaders;

{ This file is a part of Map editor for VCMI project.

  Copyright (C) 2016-2017 Alexander Shishkin alexvins@users.sourceforge.net

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
  Classes, SysUtils, fgl,
  FPimage,
  FPReadBMP,FPReadJPEG,FPReadPNG,FPReadTGA,FPReadTiff,

  Graphics, IntfGraphics, GraphType, LazLoggerBase,

  editor_classes, filesystem_base, vcmi.image_formats.h3pcx;

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

  TReadersMap = specialize TObjectMap<AnsiString, TFPCustomImageReader>;

  { TImageReaders }

  TImageReaders = class sealed
  private
    FUpdated: Boolean;
    FMap:TReadersMap;
    procedure CheckUpdated;
  public
    constructor Create;
    destructor Destroy; override;

    function GetLoader(AExt: string): TFPCustomImageReader;
  end;

implementation

var
  GImageLoaders: TImageReaders;

{ TImageReaders }

procedure TImageReaders.CheckUpdated;
var
  i: Integer;
  typename, ext: String;
  c: TFPCustomImageReaderClass;
begin
  if not FUpdated then
  begin
    FMap.Clear;

    for i := 0 to ImageHandlers.Count - 1 do
    begin
      typename := ImageHandlers.TypeNames[i];
      c := ImageHandlers.ImageReader[typename];
      ext := '.'+Trim(UpperCase(ImageHandlers.Extensions[typename]));
      if Assigned(c) then
      begin
        FMap.Add(ext,c.Create());
      end;
    end;
  end;
end;

constructor TImageReaders.Create;
begin
  FMap := TReadersMap.Create;
end;

destructor TImageReaders.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

function TImageReaders.GetLoader(AExt: string): TFPCustomImageReader;
var
  idx: LongInt;
begin
  Result := nil;
  idx := FMap.IndexOf(AExt);
  if idx >= 0 then
  begin
    Result := FMap.Data[idx];
  end;
end;

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

  if ext = '.PCX' then
  begin
    //(!)untested
    LoadH3Pcx(AStream, FData);
  end
  else
  begin
    r := GImageLoaders.GetLoader(ext);

    if Assigned(r) then
      FData.LoadFromStream(AStream, r)
    else
      raise Exception.CreateFmt('Unknown image file extension for ',[AFileName]);
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

initialization
  GImageLoaders := TImageReaders.Create;
  GImageLoaders.CheckUpdated();
finalization;
  FreeAndNil(GImageLoaders);
end.

