{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2016 Alexander Shishkin alexvins@users.sourceforge.net

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

unit logical_event_condition;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fpjson, editor_classes, logical_expression, editor_types,
  vcmi_json, root_manager, position;

type

  { TLogicalEventConditionItem }

  TLogicalEventConditionItem = class(TLogicalExpressionItem, ISerializeSpecial)
  private
    FSubtype: AnsiString;
    Ftype: AnsiString;
    FConditionType: TWinLossCondition;
    FObjectLink: string;
    FPosition: TPosition;
    FValue: Int32;
    function IsPositionStored: Boolean;
    procedure SetConditionType(AValue: TWinLossCondition);
    procedure SetObjectLink(AValue: string);
    procedure SetSubtype(AValue: AnsiString);
    procedure Settype(AValue: AnsiString);
    procedure SetValue(AValue: Int32);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    //ISerializeSpecial
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
  public
    property ConditionType: TWinLossCondition read FConditionType write SetConditionType;

    function AddSubCondition:TLogicalEventConditionItem;
  published
    property Value:Int32 read FValue write SetValue default 0;

    property &Object: string read FObjectLink write SetObjectLink;

    property Position: TPosition read FPosition stored IsPositionStored;
    property &type: AnsiString read FType write SetType;
    property Subtype: AnsiString read FSubtype write SetSubtype;
  end;

  { TLogicalEventCondition }

  TLogicalEventCondition = class(TLogicalExpression, IReferenceNotify)
  public //IReferenceNotify
    procedure NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
  end;

  { TTriggeredEventEffect }

  TTriggeredEventEffect = class(TPersistent)
  private
    Ftype: AnsiString;
    FMessageToSend: TLocalizedString;
    procedure settype(AValue: AnsiString);
    procedure SetMessageToSend(AValue: TLocalizedString);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property MessageToSend: TLocalizedString read FMessageToSend write SetMessageToSend;
    property &type: AnsiString read Ftype write Settype;
  end;

  { TTriggeredEvent }

  TTriggeredEvent = class(TNamedCollectionItem)
  private
    FCondition: TLogicalEventCondition;
    FEffect: TTriggeredEventEffect;
    FMessage: TLocalizedString;
    procedure SetMessage(AValue: TLocalizedString);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    function AddCondition:TLogicalEventConditionItem;
  published
    property Message: TLocalizedString read FMessage write SetMessage;
    property Condition:TLogicalEventCondition read FCondition;
    property Effect: TTriggeredEventEffect read FEffect;
  end;



  { TTriggeredEvents }
  TTriggeredEventsCollection = specialize TGNamedCollection<TTriggeredEvent>;

  TTriggeredEvents = class(TTriggeredEventsCollection)
  public
    procedure AddStandardVictory();
    procedure AddStandardDefeat();
  end;

implementation

uses typinfo;

{ TTriggeredEvents }

procedure TTriggeredEvents.AddStandardVictory;
var
  standard_victory: TTriggeredEvent;
  condition:TLogicalEventConditionItem;
begin
  standard_victory := Add;
  standard_victory.Effect.&type := 'victory';
  standard_victory.Effect.MessageToSend := RootManager.LocaleManager.GeneralTexts[0,5];
  standard_victory.Identifier:='standardVictory';
  standard_victory.Message:=RootManager.LocaleManager.GeneralTexts[0,659];

  condition := standard_victory.AddCondition;
  condition.ConditionType:=TWinLossCondition.standardWin;
end;

procedure TTriggeredEvents.AddStandardDefeat;
var
  standard_defeat: TTriggeredEvent;
  condition:TLogicalEventConditionItem;
begin
  standard_defeat := Add;
  standard_defeat.Effect.&type := 'defeat';
  standard_defeat.Effect.MessageToSend := RootManager.LocaleManager.GeneralTexts[0,8];
  standard_defeat.Identifier:='standardDefeat';
  standard_defeat.Message:=RootManager.LocaleManager.GeneralTexts[0,7];

  condition := standard_defeat.AddCondition;
  condition.ConditionType:=TWinLossCondition.daysWithoutTown;
  condition.Value := 7;
end;

{ TLogicalEventCondition }

procedure TLogicalEventCondition.NotifyReferenced(AOldIdentifier,
  ANewIdentifier: AnsiString);
begin

end;

{ TLogicalEventConditionItem }

procedure TLogicalEventConditionItem.SetConditionType(AValue: TWinLossCondition);
begin
  if FConditionType=AValue then Exit;
  FConditionType:=AValue;
end;

function TLogicalEventConditionItem.IsPositionStored: Boolean;
begin
  Result := not FPosition.IsEmpty;
end;

procedure TLogicalEventConditionItem.SetObjectLink(AValue: string);
begin
  if FObjectLink=AValue then Exit;
  FObjectLink:=AValue;
end;

procedure TLogicalEventConditionItem.SetSubtype(AValue: AnsiString);
begin
  if FSubtype=AValue then Exit;
  FSubtype:=AValue;
end;

procedure TLogicalEventConditionItem.Settype(AValue: AnsiString);
begin
  if Ftype=AValue then Exit;
  Ftype:=AValue;
end;

procedure TLogicalEventConditionItem.SetValue(AValue: Int32);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

procedure TLogicalEventConditionItem.AssignTo(Dest: TPersistent);
var
  dest_typed:  TLogicalEventConditionItem;
begin
  if Dest is TLogicalEventConditionItem then
  begin
    dest_typed := TLogicalEventConditionItem(Dest);
    dest_typed.ConditionType := ConditionType;
    dest_typed.Value:=Value;
    dest_typed.&Object:=&Object;
    dest_typed.Position.X:=Position.X;
    dest_typed.Position.Y:=Position.Y;
    dest_typed.Position.L:=Position.L;
    dest_typed.&type:=&type;
    dest_typed.SubType:=SubType;
  end;

  inherited AssignTo(Dest);
end;

function TLogicalEventConditionItem.Serialize(AHandler: TVCMIJSONStreamer
  ): TJSONData;
var
  item: TCollectionItem;

  o: TJSONObject;
begin
  Result := CreateJSONArray([]);

  if SubExpressions.Count > 0 then
  begin
     TJSONArray(Result).Add(GetEnumName( TypeInfo(TLogicalOperator), Integer(LogicalOperator)));

     for item in SubExpressions do
     begin
       TJSONArray(Result).Add((item as TLogicalEventConditionItem).Serialize(AHandler));
     end;
  end
  else
  begin
    TJSONArray(Result).Add(GetEnumName(TypeInfo(TWinLossCondition), Integer(ConditionType)));

    o := AHandler.ObjectToJSON(Self);
    TJSONArray(Result).Add(o);
  end;
end;

procedure TLogicalEventConditionItem.Deserialize(AHandler: TVCMIJSONDestreamer;
  ASrc: TJSONData);
var
  ASrcArray: TJSONArray;
  instruction_name: TJSONStringType;
  raw_instruction: Integer;
  i: Integer;
  SubExpression: TLogicalEventConditionItem;
  o: TJSONObject;
begin
  if ASrc.JSONType <> jtArray then
  begin
    raise Exception.Create('invalid format for event condition, array required');
  end;

  ASrcArray :=  TJSONArray(ASrc);

  instruction_name :=  ASrcArray.Strings[0];

  raw_instruction := GetEnumValue(TypeInfo(TLogicalOperator), instruction_name);

  if raw_instruction >=0 then
  begin
    LogicalOperator:=TLogicalOperator(raw_instruction);

    for i := 1 to ASrcArray.Count - 1 do
    begin
      SubExpression := TLogicalEventConditionItem (SubExpressions.Add);

      SubExpression.Deserialize(AHandler, ASrcArray.Items[i]);
    end;
  end
  else
  begin
    raw_instruction := GetEnumValue(TypeInfo(TWinLossCondition), instruction_name);

    if raw_instruction <0 then
    begin
      raise Exception.Create('invalid instruction for event condition: '+instruction_name);
    end;

    ConditionType := TWinLossCondition(raw_instruction);
    if(ASrcArray.Count > 1) and (ASrcArray.Types[1] = jtObject) then
    begin
      o := ASrcArray.Objects[1];
      AHandler.JSONToObject(o,Self);
    end;
  end;
end;

function TLogicalEventConditionItem.AddSubCondition: TLogicalEventConditionItem;
begin
  Result := SubExpressions.Add as TLogicalEventConditionItem;
end;

constructor TLogicalEventConditionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FPosition := TPosition.Create;

end;

destructor TLogicalEventConditionItem.Destroy;
begin
  FPosition.Free;
  inherited Destroy;
end;

{ TTriggeredEventEffect }

procedure TTriggeredEventEffect.SetMessageToSend(AValue: TLocalizedString);
begin
  if FMessageToSend=AValue then Exit;
  FMessageToSend:=AValue;
end;

procedure TTriggeredEventEffect.AssignTo(Dest: TPersistent);
var
  dest_typed:  TTriggeredEventEffect;
begin
  if Dest is TTriggeredEventEffect then
  begin
    dest_typed := TTriggeredEventEffect(Dest);
    dest_typed.MessageToSend := MessageToSend;
    dest_typed.&type := &type;
  end
  else
  begin
     inherited AssignTo(Dest);
  end;
end;

procedure TTriggeredEventEffect.settype(AValue: AnsiString);
begin
  if Ftype=AValue then Exit;
  Ftype:=AValue;
end;

{ TTriggeredEvent }

procedure TTriggeredEvent.SetMessage(AValue: TLocalizedString);
begin
  if FMessage=AValue then Exit;
  FMessage:=AValue;
end;

procedure TTriggeredEvent.AssignTo(Dest: TPersistent);
var
  dest_typed:  TTriggeredEvent;
begin
  if Dest is TTriggeredEvent then
  begin
    dest_typed := TTriggeredEvent(Dest);
    dest_typed.Condition.Assign(Condition);
    dest_typed.Effect.Assign(Effect);
    dest_typed.Message:=Message;
  end;
  inherited AssignTo(Dest);
end;

constructor TTriggeredEvent.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCondition := TLogicalEventCondition.CreateRoot(TLogicalEventConditionItem);
  FEffect := TTriggeredEventEffect.Create;
end;

destructor TTriggeredEvent.Destroy;
begin
  FEffect.Free;
  FCondition.Free;
  inherited Destroy;
end;

function TTriggeredEvent.AddCondition: TLogicalEventConditionItem;
begin
  Result := Condition.Add as TLogicalEventConditionItem;
end;

end.

