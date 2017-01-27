{ This file is a part of Map editor for VCMI project

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

unit edit_triggered_events;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, typinfo, SynHighlighterAny, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, ComCtrls, Buttons, ExtCtrls, Map, fpjson, vcmi_json,
  logical_event_condition, field_editors, editor_classes, triggered_event_frame, vcmi_fpjsonrtti;

type

  { TTriggeredEventsForm }

  TTriggeredEventsForm = class(TForm)
    act: TActionList;
    actDontSave: TAction;
    actAdd: TAction;
    actRename: TAction;
    actRemove: TAction;
    actSave: TAction;
    btCancel: TButton;
    btOk: TButton;
    DefeatText: TEdit;
    DefeatTextLabel: TLabel;
    pcEvents: TPageControl;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    VictoryText: TEdit;
    JsonSyn: TSynAnySyn;
    ErrorLabel: TLabel;
    VictoryTextLabel: TLabel;
    lbEvents: TLabel;
    procedure actAddExecute(Sender: TObject);
    procedure actDontSaveExecute(Sender: TObject);
    procedure actRemoveExecute(Sender: TObject);
    procedure actRemoveUpdate(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure pcEventsChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pcEventsCloseTabClicked(Sender: TObject);
  private
    FMap: TVCMIMap;
    FEditors: TFieldEditors;
    FPages:TTriggeredEventFrameList;

    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Map: TVCMIMap read FMap write SetMap;

  end;


implementation

{$R *.lfm}

{ TTriggeredEventsForm }

procedure TTriggeredEventsForm.actSaveExecute(Sender: TObject);
var
  error_page: TTabSheet;
  error_message: AnsiString;
begin
  //todo: validate more

  if FPages.Stash(error_page, error_message) then
  begin
    ErrorLabel.Visible:=False;
    FEditors.Commit;
    FPages.Commit(FMap.TriggeredEvents);
    ModalResult:=mrOK;
  end
  else
  begin
    ErrorLabel.Visible:=True;
    ErrorLabel.Caption := error_message;
    pcEvents.ActivePage := error_page;
  end;
end;

procedure TTriggeredEventsForm.actSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(FMap);
end;

procedure TTriggeredEventsForm.pcEventsChanging(Sender: TObject; var AllowChange: Boolean);
begin
  //
end;

procedure TTriggeredEventsForm.pcEventsCloseTabClicked(Sender: TObject);
begin
  //
end;

procedure TTriggeredEventsForm.actDontSaveExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTriggeredEventsForm.actRemoveExecute(Sender: TObject);
begin
  pcEvents.ActivePage.Free;
end;

procedure TTriggeredEventsForm.actRemoveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:= pcEvents.PageIndex <> -1;
end;

procedure TTriggeredEventsForm.actAddExecute(Sender: TObject);
begin
  FPages.AddFrame(nil, pcEvents);
end;

procedure TTriggeredEventsForm.SetMap(AValue: TVCMIMap);
begin
  FMap:=AValue;
  if Assigned(FMap) then ReadData;
end;

procedure TTriggeredEventsForm.ReadData;
var
  i: Integer;
  event: TTriggeredEvents.TItemType;
begin
  FEditors.Clear;

  FEditors.Add(TStringFieldEditor.Create(FMap,'VictoryString', VictoryText));
  FEditors.Add(TStringFieldEditor.Create(FMap,'DefeatString', DefeatText));

  FEditors.Load;

  FPages.Clear;
  for i := 0 to FMap.TriggeredEvents.Count - 1 do
  begin
    event := FMap.TriggeredEvents.Items[i];
    FPages.AddFrame(event, pcEvents);
  end;
end;

constructor TTriggeredEventsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditors := TFieldEditors.Create;
  FPages := TTriggeredEventFrameList.Create(Self);
end;

destructor TTriggeredEventsForm.Destroy;
begin
  FEditors.Free;
  inherited Destroy;
end;

end.

