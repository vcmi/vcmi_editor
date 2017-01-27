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

unit triggered_event_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, typinfo, fpjson,
  FileUtil, SynEdit, Forms, Controls, StdCtrls, ComCtrls,
  logical_event_condition, field_editors, editor_classes, vcmi_fpjsonrtti, vcmi_json;

type

  { TTriggeredEventFrame }

  TTriggeredEventFrame = class(TFrame)
    edEffect: TComboBox;
    edMessage: TEdit;
    edMessageToSend: TEdit;
    JsonEditor: TSynEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    FStreamer: TVCMIJSONStreamer;
    FDeStreamer: TVCMIJSONDestreamer;
    FEditors: TFieldEditors;
    FBuffer: TTriggeredEvent;

    Procedure OnPropertyError(Sender : TObject; AObject : TObject; Info : PPropInfo; AValue : TJSONData;
      Error : Exception; Var Continue : Boolean);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFrom(AObject: TTriggeredEvent);
    function Stash(): AnsiString;
  end;

  TTriggeredEventFrameVector = specialize TFPGList<TTriggeredEventFrame>;

  { TTriggeredEventFrameList }

  TTriggeredEventFrameList = class (TComponent)
  private
    FData: TTriggeredEventFrameVector;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddFrame(AObject: TTriggeredEvent; AParent: TPageControl);
    procedure Clear;
    function Stash(out APage: TTabSheet; out AError: AnsiString): Boolean;

    procedure Commit(ATarget: TTriggeredEvents);
  end;

implementation

{$R *.lfm}

{ TTriggeredEventFrameList }

constructor TTriggeredEventFrameList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TTriggeredEventFrameVector.Create;
end;

destructor TTriggeredEventFrameList.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TTriggeredEventFrameList.AddFrame(AObject: TTriggeredEvent; AParent: TPageControl);
var
  ts: TTabSheet;
  f: TTriggeredEventFrame;

  event_name: AnsiString;
  event: TTriggeredEvent;
  idx, idx_max, i: integer;
begin
  if Assigned(AObject) and (AObject.Identifier <> '') then
  begin
    event_name := AObject.Identifier;
  end
  else
  begin
    idx := 0;
    idx_max:=0;
    event_name := '';

    for i := 0 to FData.Count - 1 do
    begin
      event := FData.Items[i].FBuffer;

      if TryStrToInt(Copy(event.Identifier, 6, MaxInt), idx) then
      begin
        if idx > idx_max then
        begin
          idx_max:=idx;
        end;
      end;
    end;
    event_name:='event'+IntToStr(idx+1);
  end;

  ts := TTabSheet.Create(AParent);
  ts.PageControl := AParent;
  ts.TabVisible:=true;
  ts.Caption:=event_name;

  f := TTriggeredEventFrame.Create(ts);
  f.Parent := ts;

  f.Align := alClient;

  f.LoadFrom(AObject);
  f.FBuffer.Identifier:=event_name;

  FData.Add(f);
  f.FreeNotification(Self);
end;

procedure TTriggeredEventFrameList.Clear;
begin
  FData.Clear;
end;

function TTriggeredEventFrameList.Stash(out APage: TTabSheet; out AError: AnsiString): Boolean;
var
  i: Integer;
  frame: TTriggeredEventFrame;
begin
  Result := true;
  APage := nil;
  AError:='';
  i := 0;

  while Result and (i < FData.Count) do
  begin
    frame := FData.Items[i];
    AError:=frame.Stash();
    if AError <> '' then
    begin
      APage := frame.Parent as TTabSheet;
      Result := False;
    end;
    inc(i);
  end;
end;

procedure TTriggeredEventFrameList.Commit(ATarget: TTriggeredEvents);
var
  i: Integer;
  frame: TTriggeredEventFrame;
  event: TTriggeredEvent;
begin
  ATarget.Clear;

  for i := 0 to FData.Count - 1 do
  begin
    frame := FData.Items[i];
    event := frame.FBuffer;
    frame.FBuffer := nil;
    event.Collection := ATarget;
  end;
end;

procedure TTriggeredEventFrameList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TTriggeredEventFrame then
  begin
    FData.Remove(TTriggeredEventFrame(AComponent));
  end;
end;

{ TTriggeredEventFrame }

procedure TTriggeredEventFrame.OnPropertyError(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData;
  Error: Exception; var Continue: Boolean);
begin
  Continue := false;
end;

constructor TTriggeredEventFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStreamer := TVCMIJSONStreamer.Create(Self);
  FDeStreamer := TVCMIJSONDestreamer.Create(Self);
  FDeStreamer.OnPropertyError := @OnPropertyError;
  FEditors := TFieldEditors.Create;
  FBuffer := TTriggeredEvent.Create(nil);
end;

destructor TTriggeredEventFrame.Destroy;
begin
  FreeAndNil(FBuffer);
  FEditors.Free;
  inherited Destroy;
end;

procedure TTriggeredEventFrame.LoadFrom(AObject: TTriggeredEvent);
var
  FRawData: TJSONData;
begin
  FEditors.Clear;
  FBuffer.Clear;
  if Assigned(AObject) then
  begin
    FBuffer.Assign(AObject);
  end;

  FEditors.Add(TStringFieldEditor.Create(FBuffer, 'Message', edMessage));
  FEditors.Add(TStringFieldEditor.Create(FBuffer.Effect, 'MessageToSend', edMessageToSend));
  FEditors.Add(TEnumEditor.Create(FBuffer.Effect, 'type', edEffect));

  FRawData := FStreamer.ObjectToJsonEx(FBuffer.Condition);
  JsonEditor.Text := FRawData.FormatJSON([foUseTabchar], 1);
  FRawData.Free;

  FEditors.Load;
end;

function TTriggeredEventFrame.Stash: AnsiString;
begin
  Result := '';

  try
    FBuffer.Clear;
    FEditors.Commit;
    FDeStreamer.JSONToObject(JsonEditor.Text, FBuffer.Condition);
  except
    on e: Exception do
    begin
      Result := e.Message;
    end;
  end;
end;

end.

