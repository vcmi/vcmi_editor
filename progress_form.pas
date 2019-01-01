{ This file is a part of Map editor for VCMI project

  Copyright (C) 2017 Alexander Shishkin alexvins@users.sourceforge.net

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
unit progress_form;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, LazLoggerBase, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, editor_classes;

type

  { TProgressForm }

  TProgressForm = class(TForm,IProgressCallback)
    act: TActionList;
    actClose: TAction;
    CloseButton: TButton;
    lbDetail: TLabel;
    Errors: TMemo;
    pbDetail: TProgressBar;
    procedure actCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FCompleted: Boolean;
    FHasErrors: boolean;
    FMax: Integer;
    FCurrent: integer;
    procedure SetCompleted(AValue: Boolean);
    { private declarations }
  public
    property HasErrors: boolean read FHasErrors;
    property Completed: Boolean read FCompleted write SetCompleted;
    procedure Reset;
  public
    //IProgressCallback

    function GetMax: Integer;
    procedure SetMax(AValue: Integer);
    procedure Advance(ADelta: integer);
    procedure NextStage(const AStageLabel:string);
    procedure AddError(const ADescription: string);
    procedure AddMessage(const ADescription: string);
  end;

implementation

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.Advance(ADelta: integer);
var
  FOld, FOldNormalized, FCurrentNormalized: Integer;
begin
  FOld := FCurrent;
  inc(FCurrent, ADelta);

  FOldNormalized := FOld * pbDetail.Max div FMax;
  FCurrentNormalized := FCurrent * pbDetail.Max div FMax;

  if FOldNormalized < FCurrentNormalized then
  begin
    pbDetail.StepBy(FCurrentNormalized - FOldNormalized);
    Application.ProcessMessages;
  end;
end;

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  Reset;
  Height:=75;
  Errors.Visible:=false;
end;

procedure TProgressForm.SetCompleted(AValue: Boolean);
begin
  FCompleted:=AValue;

  CloseButton.Enabled := FCompleted;
end;

procedure TProgressForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide; //may be reused, owned by root_manager
end;

procedure TProgressForm.actCloseExecute(Sender: TObject);
begin
  Close;
end;

function TProgressForm.GetMax: Integer;
begin
  Result := FMax;
end;

procedure TProgressForm.NextStage(const AStageLabel: string);
begin
  pbDetail.Position := pbDetail.Min;
  lbDetail.Caption := AStageLabel;

  Application.ProcessMessages;
end;

procedure TProgressForm.AddError(const ADescription: string);
begin
  Height:=320;
  Errors.Visible := true;

  Errors.Append(ADescription);

  DebugLn(ADescription);

  FHasErrors:=True;
end;

procedure TProgressForm.AddMessage(const ADescription: string);
begin
  //todo: use different format
  AddError(ADescription);
end;

procedure TProgressForm.Reset;
begin
  pbDetail.Position := pbDetail.Min;
  FCurrent:=0;
  lbDetail.Caption := '';
  FCompleted:=false;
end;

procedure TProgressForm.SetMax(AValue: Integer);
begin
  FMax := AValue;
  FCurrent:=0;
  pbDetail.Position := pbDetail.Min;
  Application.ProcessMessages;
end;

end.

