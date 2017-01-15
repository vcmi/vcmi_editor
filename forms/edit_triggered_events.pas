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
  Graphics, Dialogs, ActnList, StdCtrls, ComCtrls, Buttons, Map, fpjson, vcmi_json,
  logical_event_condition, field_editors, editor_classes, vcmi_fpjsonrtti;

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
    tcEvents: TTabControl;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    VictoryText: TEdit;
    JsonEditor: TSynEdit;
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
    procedure tcEventsChange(Sender: TObject);
    procedure tcEventsChanging(Sender: TObject; var AllowChange: Boolean);
  private
    FMap: TVCMIMap;
    FIgnoreEvents: Boolean;

    FBuffer: TTriggeredEvents;

    FStreamer: TVCMIJSONStreamer;
    FDeStreamer: TVCMIJSONDestreamer;

    FEditors: TFieldEditors;

    procedure SetMap(AValue: TVCMIMap);
    procedure RecreateTabs;
    procedure ReadEvents;
    procedure ReadData;

    procedure LoadEvent(Idx: Integer);
    function StashEvent(Idx: Integer): boolean;
    function StashSelectedEvent: boolean;

    Procedure OnPropertyError(Sender : TObject; AObject : TObject; Info : PPropInfo; AValue : TJSONData;
      Error : Exception; Var Continue : Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Map: TVCMIMap read FMap write SetMap;
  end;


implementation

{$R *.lfm}

{ TTriggeredEventsForm }

procedure TTriggeredEventsForm.actSaveExecute(Sender: TObject);
begin
  ErrorLabel.Visible:=False;
  if not StashSelectedEvent then
  begin
    exit;
  end;

  //todo: validate more

  FMap.TriggeredEvents.Assign(FBuffer);

  FEditors.Commit;

  ModalResult:=mrOK;
end;

procedure TTriggeredEventsForm.actSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(FMap);
end;

procedure TTriggeredEventsForm.tcEventsChange(Sender: TObject);
begin
  if (csLoading in ComponentState) or FIgnoreEvents then
  begin
    Exit;
  end;

  LoadEvent(tcEvents.TabIndex);
end;

procedure TTriggeredEventsForm.tcEventsChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if (csLoading in ComponentState) or FIgnoreEvents then
  begin
    Exit;
  end;

  AllowChange:=StashEvent(tcEvents.TabIndex);
end;

procedure TTriggeredEventsForm.actDontSaveExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTriggeredEventsForm.actRemoveExecute(Sender: TObject);
var
  old_index: Integer;
begin
  old_index := tcEvents.TabIndex;
  FBuffer.Delete(tcEvents.TabIndex);
  RecreateTabs;
  //try to switch to previous tab
  Dec(old_index);
  if old_index < 0 then
    old_index:=0;
  if tcEvents.PageCount > 0 then
  begin
    tcEvents.TabIndex := old_index;
  end
  else
  begin
    LoadEvent(-1);
  end;
end;

procedure TTriggeredEventsForm.actRemoveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FBuffer.Count > 0) and (tcEvents.TabIndex <> -1);
end;

procedure TTriggeredEventsForm.actAddExecute(Sender: TObject);
var
  event_name: AnsiString;
  event: TTriggeredEvent;
  idx : integer;
  stop: Boolean;
begin
  if not StashSelectedEvent then
  begin
    Exit;
  end;

  stop := false;
  idx := 1;
  event_name := '';
  while not stop do
  begin
    event_name:='event'+IntToStr(idx);
    stop := FBuffer.IndexOfName(event_name) < 0;
    Inc(Idx);
  end;

  if event_name = '' then
  begin
    Exit;
  end;

  event := FBuffer.Add;
  event.Identifier:=event_name;

  RecreateTabs;

  if tcEvents.TabIndex <> event.Index then
  begin
    tcEvents.TabIndex := event.Index;
  end
  else
  begin
    LoadEvent(event.Index);
  end;
end;

procedure TTriggeredEventsForm.SetMap(AValue: TVCMIMap);
begin
  FMap:=AValue;
  if Assigned(FMap) then ReadData;
end;

procedure TTriggeredEventsForm.ReadEvents;
begin
  RecreateTabs;

  if FBuffer.Count > 0 then
    LoadEvent(0)
  else
    LoadEvent(-1);
end;

procedure TTriggeredEventsForm.ReadData;
begin
  FBuffer.Assign(FMap.TriggeredEvents);
  ReadEvents;
  FEditors.Add(TStringFieldEditor.Create(FMap,'VictoryString', VictoryText));
  FEditors.Add(TStringFieldEditor.Create(FMap,'DefeatString', DefeatText));

  FEditors.Load;
end;

procedure TTriggeredEventsForm.RecreateTabs;
var
  i: Integer;
  event: TTriggeredEvent;
begin
  FIgnoreEvents:=true;
  tcEvents.BeginUpdate;
  try
    tcEvents.Tabs.Clear;
    for i := 0 to FBuffer.Count - 1 do
    begin
      event := FBuffer.Items[i];
      tcEvents.Tabs.Add(event.Identifier);
    end;
  finally
    tcEvents.EndUpdate;
    FIgnoreEvents:=false;
  end;
end;

procedure TTriggeredEventsForm.LoadEvent(Idx: Integer);
var
  event: TTriggeredEvent;
  FRawData: TJSONData;
begin
  if Idx < 0 then
  begin
    JsonEditor.Clear;
    JsonEditor.Enabled:=false;
  end
  else
  begin
    event := FBuffer.Items[Idx];

    FRawData := FStreamer.ObjectToJsonEx(event);
    JsonEditor.Text := FRawData.FormatJSON([foUseTabchar], 1);
    FRawData.Free;
    JsonEditor.Enabled:=True;
  end;
end;

function TTriggeredEventsForm.StashEvent(Idx: Integer): boolean;
var
  event: TTriggeredEvent;
  id: AnsiString;
begin
  Result := true;

  if Idx < 0 then
  begin
    Exit;
  end;

  event := TTriggeredEvent.Create(nil);

  try
    FDeStreamer.JSONToObject(JsonEditor.Text, event);
    id := FBuffer.Items[Idx].Identifier;
    FBuffer.Items[Idx].Assign(event);
    FBuffer.Items[Idx].Identifier := id;
  except
    on e: Exception do
    begin
      ErrorLabel.Visible:=True;
      ErrorLabel.Caption := e.Message;
      Result := false;
    end;
  end;
  event.Free;
end;

function TTriggeredEventsForm.StashSelectedEvent: boolean;
begin
  Result := StashEvent(tcEvents.TabIndex);
end;

procedure TTriggeredEventsForm.OnPropertyError(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData;
  Error: Exception; var Continue: Boolean);
begin
  Continue := false;
end;

constructor TTriggeredEventsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStreamer := TVCMIJSONStreamer.Create(Self);
  FDeStreamer := TVCMIJSONDestreamer.Create(Self);
  FDeStreamer.OnPropertyError := @OnPropertyError;
  FBuffer:=TTriggeredEvents.Create;
  FEditors := TFieldEditors.Create;
  FIgnoreEvents := false;
end;

destructor TTriggeredEventsForm.Destroy;
begin
  FEditors.Free;
  FBuffer.Free;
  inherited Destroy;
end;

end.

