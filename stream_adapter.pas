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
unit stream_adapter;

{$I compilersetup.inc}


interface

uses
  Classes, SysUtils, FileUtil, editor_types;

type

  { TStreamWriteAdapter }

  { TStreamAdapter }

  TStreamAdapter = object
  strict protected
    FStm: TStream;
  public
    constructor Create(dest: TStream);
  end;

  { TStreamReadAdapter }

  TStreamReadAdapter = object (TStreamAdapter)
  public
    constructor Create(dest: TStream);

    procedure ReadBuffer(var Buffer; Count: Longint);

    function ReadLocalizedString: TLocalizedString;
    function ReadString: AnsiString;

    function ReadDWord: DWord;inline;
    function ReadInt32: int32;inline;
    function ReadWord: Word;inline;

    function ReadByte: Byte; inline;
    function ReadBoolean: Boolean; inline;

    procedure Skip(Count: Integer);
    procedure SkipString;

    function ReadIDByte:TCustomID;

    function GetPos: Int64;
  end;

  TStreamWriteAdapter = object (TStreamAdapter)
  public
    constructor Create(dest: TStream);

    procedure WriteString(str: ansistring); inline;
    procedure WriteZero(size: integer);inline;
    procedure WriteByte(V: uint8; rep:integer = 1); inline;
    procedure WriteWord(V: Word);inline;
    procedure WriteDword(V: DWord);inline;
    procedure WriteBuffer(const Buffer; Count: Longint);{inline;}


  end;



implementation

{ TStreamReadAdapter }

constructor TStreamReadAdapter.Create(dest: TStream);
begin
   inherited Create(dest);
end;

function TStreamReadAdapter.ReadBoolean: Boolean;
begin
  Result := ReadByte <> 0;
end;

procedure TStreamReadAdapter.ReadBuffer(var Buffer; Count: Longint);
begin
  FStm.Read(Buffer,Count);
end;

function TStreamReadAdapter.ReadByte: Byte;
begin
  Result := FStm.ReadByte;
end;

function TStreamReadAdapter.ReadDWord: DWord;
begin
  Result := LEtoN(FStm.ReadDWord);
end;

function TStreamReadAdapter.ReadInt32: int32;
begin
  Result := Int32(ReadDWord);
end;

function TStreamReadAdapter.ReadIDByte: TCustomID;
var
  b: Byte;
begin
  b := ReadByte;
  if b = 255 then
    Result := ID_RANDOM
  else
    Result := b;
end;

function TStreamReadAdapter.GetPos: Int64;
begin
  Result := FStm.Position;
end;

function TStreamReadAdapter.ReadLocalizedString: TLocalizedString;
begin
  Result := AnsiToUtf8(ReadString); //TODO: select localization
end;

function TStreamReadAdapter.ReadString: AnsiString;
var
  L: int32;
begin
  L := ReadDWord;
  SetLength(Result,L);
  if L > 0 then ReadBuffer(Result[1],L);
end;

function TStreamReadAdapter.ReadWord: Word;
begin
  Result := LEtoN(FStm.ReadWord);
end;

procedure TStreamReadAdapter.Skip(Count: Integer);
begin
  FStm.Seek(Count,soCurrent);
end;

procedure TStreamReadAdapter.SkipString;
var
  L: int32;
begin
  L := ReadDWord;
  Skip(L);
end;

{ TStreamAdapter }

constructor TStreamAdapter.Create(dest: TStream);
begin
  FStm := dest;
end;

{ TStreamWriteAdapter }

constructor TStreamWriteAdapter.Create(dest: TStream);
begin
  inherited Create(dest);
end;

procedure TStreamWriteAdapter.WriteBuffer(const Buffer; Count: Longint);
begin
  FStm.WriteBuffer(Buffer,Count);
end;

procedure TStreamWriteAdapter.WriteByte(V: uint8; rep: integer);
var
  i: Integer;
begin
  for i:=1 to rep do
  begin
    FStm.WriteByte(V);
  end;
end;

procedure TStreamWriteAdapter.WriteDword(V: DWord);
begin
  FStm.WriteDWord(NtoLE(V));
end;

procedure TStreamWriteAdapter.WriteString(str: ansistring);
var
  L: int32;
begin
  L:=Length(str); //assume string cant be too large
  WriteDWord(L);
  FStm.WriteBuffer(Pointer(str)^,L);
end;

procedure TStreamWriteAdapter.WriteWord(V: Word);
begin
  FStm.WriteWord(NtoLE(V));
end;

procedure TStreamWriteAdapter.WriteZero(size: integer);
begin
  WriteByte(0, size);
end;

end.

