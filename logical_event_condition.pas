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

unit logical_event_condition;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fpjson, editor_classes, logical_expression, editor_types,
  vcmi_json, object_link;

type

  { TLogicalEventConditionItem }

  TLogicalEventConditionItem = class(TLogicalExpressionItem, ISerializeSpecial)
  private
    FEventType: TWinLossCondition;
    FObjectLink: TObjectLink;
    Ftype: AnsiString;
    FValue: Int32;
    procedure SetEventType(AValue: TWinLossCondition);
    procedure Settype(AValue: AnsiString);
    procedure SetValue(AValue: Int32);
  public
    //ISerializeSpecial
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property ConditionType: TWinLossCondition read FEventType write SetEventType;

    property Value:Int32 read FValue write SetValue;

    property ObjectLink: TObjectLink read FObjectLink;
  end;

  { TLogicalEventCondition }

  TLogicalEventCondition = class(TLogicalExpression)
  public
    constructor Create();
  end;

  { TTriggeredEventEffect }

  TTriggeredEventEffect = class(TPersistent)
  private
    Ftype: AnsiString;
    FMessageToSend: TLocalizedString;
    procedure settype(AValue: AnsiString);
    procedure SetMessageToSend(AValue: TLocalizedString);
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
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Message: TLocalizedString read FMessage write SetMessage;

    property Condition:TLogicalEventCondition read FCondition;
    property Effect: TTriggeredEventEffect read FEffect;
  end;



  { TTriggeredEvents }

  TTriggeredEvents = class(specialize TGNamedCollection<TTriggeredEvent>)
  public
    procedure AddStandardVictory();
    procedure AddStandardDefeat();
  end;

implementation

uses typinfo;

{ TTriggeredEvents }

procedure TTriggeredEvents.AddStandardVictory;
begin
  //todo:TTriggeredEvents.AddStandardVictory
end;

procedure TTriggeredEvents.AddStandardVictory;
begin
  //todo:TTriggeredEvents.AddStandardVictory
end;

{ TLogicalEventCondition }

constructor TLogicalEventCondition.Create;
begin
  inherited Create(TLogicalEventConditionItem);
end;

{ TLogicalEventConditionItem }

procedure TLogicalEventConditionItem.SetEventType(AValue: TWinLossCondition);
begin
  if FEventType=AValue then Exit;
  FEventType:=AValue;
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

    o := (ObjectLink.Serialize(AHandler)) as TJSONObject;

    if Value <> 0 then
    begin
      o.Add('value', Value);
    end;

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

    o := ASrcArray.Objects[1];

    ObjectLink.Deserialize(AHandler, o);

    if o.IndexOfName('value') >=0 then
    begin
      Value:=o.Integers['value'];
    end;
  end;

end;

constructor TLogicalEventConditionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FObjectLink := TObjectLink.Create;
end;

destructor TLogicalEventConditionItem.Destroy;
begin
  FObjectLink.Free;
  inherited Destroy;
end;

{ TTriggeredEventEffect }

procedure TTriggeredEventEffect.SetMessageToSend(AValue: TLocalizedString);
begin
  if FMessageToSend=AValue then Exit;
  FMessageToSend:=AValue;
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

constructor TTriggeredEvent.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCondition := TLogicalEventCondition.Create();
  FEffect := TTriggeredEventEffect.Create;
end;

destructor TTriggeredEvent.Destroy;
begin
  FEffect.Free;
  FCondition.Free;
  inherited Destroy;
end;

end.

