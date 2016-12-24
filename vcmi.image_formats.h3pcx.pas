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

unit vcmi.image_formats.h3pcx;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FPImage;

type

  { TReaderH3PCX }

  TReaderH3PCX = class (TFPCustomImageReader)
  protected
    procedure InternalRead  (Str:TStream; Img:TFPCustomImage); override;
    function  InternalCheck (Str:TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TReaderH3PCX }

procedure TReaderH3PCX.InternalRead(Str: TStream; Img: TFPCustomImage);
begin

end;

function TReaderH3PCX.InternalCheck(Str: TStream): boolean;
begin

end;

constructor TReaderH3PCX.Create;
begin
  inherited Create;
end;

destructor TReaderH3PCX.Destroy;
begin
  inherited Destroy;
end;

initialization
  ImageHandlers.RegisterImageReader ('H3 PCX Format', 'pcx', TReaderH3PCX);

end.

