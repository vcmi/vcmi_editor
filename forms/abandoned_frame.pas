{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge,net

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

unit abandoned_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, base_object_options_frame, editor_types, object_options,
  editor_consts;

type

  { TAbandonedFrame }

  TAbandonedFrame = class(TBaseObjectOptionsFrame)
    edPossibleResources: TCheckListBox;
    Label1: TLabel;
  private
    FOptions: TAbandonedOptions;
  public
    procedure Commit; override;
    procedure VisitAbandonedMine(AOptions: TAbandonedOptions); override;
  end;

implementation

{$R *.lfm}

{ TAbandonedFrame }

procedure TAbandonedFrame.Commit;
var
  res_type: TResType;
begin
  FOptions.PossibleResources.Clear;

  for res_type in TResType do
  begin
    if edPossibleResources.Checked[Integer(res_type)] then
    begin
      FOptions.PossibleResources.Add(RESOURCE_NAMES[res_type]);
    end;
  end;
end;

procedure TAbandonedFrame.VisitAbandonedMine(AOptions: TAbandonedOptions);
var
  res_type: TResType;
begin
  FOptions:=AOptions;

  for res_type in TResType do
  begin
    edPossibleResources.AddItem(RESOURCE_NAMES[res_type], nil);
  end;

  for res_type in TResType do
  begin
    edPossibleResources.Checked[Integer(res_type)] := (FOptions.PossibleResources.IndexOf(RESOURCE_NAMES[res_type]) >=0);
  end;

end;

end.

