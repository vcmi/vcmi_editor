{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016 Alexander Shishkin alexvins@users.sourceforge.net

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

unit edit_triggered_events;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, Map, fpjson, vcmi_json,
  logical_event_condition, vcmi_fpjsonrtti;

type

  { TTriggeredEventsForm }

  TTriggeredEventsForm = class(TForm)
    act: TActionList;
    actDontSave: TAction;
    actSave: TAction;
    btCancel: TButton;
    btOk: TButton;
    JsonEditor: TSynEdit;
    JsonSyn: TSynAnySyn;
    ErrorLabel: TLabel;
    procedure actDontSaveExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
  private
    FMap: TVCMIMap;

    FRawData: TJSONData;

    FBuffer: TTriggeredEvents;

    FStreamer: TVCMIJSONStreamer;
    FDeStreamer: TVCMIJSONDestreamer;

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
begin
  ErrorLabel.Visible:=False;
  //todo: validate more
  try
    FBuffer.Clear;
    FDeStreamer.JSONToObject(JsonEditor.Text, FBuffer);
  except
    on e: Exception do
    begin
      ErrorLabel.Visible:=True;
      ErrorLabel.Caption := e.Message;
      Exit;
    end;
  end;

  FMap.TriggeredEvents.Assign(FBuffer);
  ModalResult:=mrOK;
end;

procedure TTriggeredEventsForm.actSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(FMap);
end;

procedure TTriggeredEventsForm.actDontSaveExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTriggeredEventsForm.SetMap(AValue: TVCMIMap);
begin
  FMap:=AValue;
  if Assigned(FMap) then ReadData;
end;

procedure TTriggeredEventsForm.ReadData;
begin
  FBuffer.Assign(FMap.TriggeredEvents);

  FRawData := FStreamer.StreamCollection(FBuffer);

  JsonEditor.Text := FRawData.FormatJSON([foUseTabchar], 1);
end;

constructor TTriggeredEventsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStreamer := TVCMIJSONStreamer.Create(Self);
  FDeStreamer := TVCMIJSONDestreamer.Create(Self);
  FRawData := nil;
  FBuffer:=TTriggeredEvents.Create;
end;

destructor TTriggeredEventsForm.Destroy;
begin
  FBuffer.Free;
  FRawData.Free;
  inherited Destroy;
end;

end.

