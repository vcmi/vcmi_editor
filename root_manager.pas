{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge.net

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
unit root_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazLogger,
  forms, Controls,

  progress_form, filesystem_base,
  filesystem;

type

  { TRootManager }

  TRootManager = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    FProgressForm: TProgressForm;
  public
    procedure InitComplete;

    property ProgressForm:TProgressForm read FProgressForm;
  end;

var
  RootManager: TRootManager;

implementation

{$R *.lfm}

{ TRootManager }

procedure TRootManager.DataModuleCreate(Sender: TObject);
var
  log_name: String;
begin
  log_name := ExtractFilePath(ParamStr(0)) + 'editor.log';

  if FileExistsUTF8(log_name) then
    DeleteFileUTF8(log_name);

  DebugLogger.LogName := log_name;

  FProgressForm := TProgressForm.Create(Self);
  FProgressForm.Visible := True;
  Application.ProcessMessages;
end;

procedure TRootManager.InitComplete;
begin
  if FProgressForm.Visible then
  begin
    FProgressForm.Close;
  end;

end;

end.

