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
  Classes, SysUtils, editor_classes, logical_expression, editor_types;

type

  { TLogicalEventData }

  TLogicalEventData  = class(TPersistent)
  private
    Ftype: AnsiString;
    FValue: Int32;
    procedure Settype(AValue: AnsiString);
    procedure SetValue(AValue: Int32);
  published
    property &type: AnsiString read Ftype write Settype;
    property Value:Int32 read FValue write SetValue;
  end;

  { TLogicalEventConditionItem }

  TLogicalEventConditionItem = class(TLogicalExpressionItem)
  private
    FData:TLogicalEventData;
  protected
    function GetAsObject: TObject; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
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
  end;

implementation

{ TLogicalEventCondition }

constructor TLogicalEventCondition.Create;
begin
  inherited Create(TLogicalEventConditionItem);
end;

{ TLogicalEventConditionItem }

function TLogicalEventConditionItem.GetAsObject: TObject;
begin
  Result := FData;
end;

constructor TLogicalEventConditionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FData := TLogicalEventData.Create;
end;

destructor TLogicalEventConditionItem.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

{ TLogicalEventData }

procedure TLogicalEventData.Settype(AValue: AnsiString);
begin
  if Ftype=AValue then Exit;
  Ftype:=AValue;
end;

procedure TLogicalEventData.SetValue(AValue: Int32);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
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

