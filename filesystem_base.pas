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
unit filesystem_base;

{$I compilersetup.inc}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils;

type

  TResourceType = (Text, Json, Animation, Mask);

  TResourceTypes = set of TResourceType;

  IResource = interface
    procedure LoadFromStream(AStream: TStream);
  end;

  { TResourceLoader }

  IResourceLoader = interface ['{6BBEC2EA-75F4-4836-B25B-2F68B25091F2}']
    (*
       AResource - resource object to load
       AResType - type of resourse
       AName - relative path + filename .w\o ext
    *)
    procedure LoadResource(AResource: IResource; AResType: TResourceType; AName: string);

    function ExistsResource(AResType: TResourceType; AName: string): boolean;

    function TryLoadResource(AResource: IResource; AResType: TResourceType; AName: string):boolean;
  end;

  { TBaseResource }

  TBaseResource = class abstract (TObject, IResource)
  private
    FPath: AnsiString;
    FTyp: TResourceType;
  public
    constructor Create(AType: TResourceType; APath: AnsiString);
    property Typ: TResourceType read FTyp;
    property Path: AnsiString read FPath;

     procedure LoadFromStream(AStream: TStream); virtual; abstract;

    procedure Load(ALoader: IResourceLoader);

  end;

  { TFSConsumer }

  TFSConsumer = class abstract (TComponent)
  private
    FResourceLoader: IResourceLoader;
    procedure SetResourceLoader(AValue: IResourceLoader);
  public
    constructor Create(AOwner: TComponent); override;
    property ResourceLoader:IResourceLoader read FResourceLoader write SetResourceLoader;
  end;

implementation

{ TBaseResource }

constructor TBaseResource.Create(AType: TResourceType; APath: AnsiString);
begin
  FTyp:=AType;
  FPath:=APath;
end;

procedure TBaseResource.Load(ALoader: IResourceLoader);
begin
  ALoader.LoadResource(Self, Typ, Path);
end;


{ TFSConsumer }

constructor TFSConsumer.Create(AOwner: TComponent);
var
  rl: IResourceLoader;
begin
  inherited Create(AOwner);

  if AOwner.GetInterface(IResourceLoader,rl) then
  begin
    ResourceLoader := rl;
  end;

  if AOwner is TFSConsumer then
  begin
    ResourceLoader := (AOwner as TFSConsumer).ResourceLoader;
  end;
end;

procedure TFSConsumer.SetResourceLoader(AValue: IResourceLoader);
begin
  if FResourceLoader = AValue then Exit;
  FResourceLoader := AValue;
end;

end.

