unit event_condition_item_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, typinfo, LMessages, Forms, Controls, StdCtrls, VirtualTrees,
  editor_types,
  logical_expression, logical_event_condition;

type

  { TConditionItemData }

  TConditionItemData = class
  private
    FConditionItem: TLogicalEventConditionItem;

  public
    constructor Create(AConditionItem: TLogicalEventConditionItem);

    function getDisplayName(): AnsiString;

    property Item: TLogicalEventConditionItem read FConditionItem;

  end;

  { TEventConditionItemFrame }

  TEventConditionItemFrame = class(TFrame, IVTEditLink)
    OperatorEdit: TComboBox;
    OperatorLabel: TLabel;
  private

  public
    Tree: TBaseVirtualTree;
    CurrentNode: PVirtualNode;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(ATree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TLMessage); stdcall;
    procedure SetBounds(R: TRect); overload; stdcall;

  end;

implementation

{$R *.lfm}

{ TConditionItemData }

constructor TConditionItemData.Create(AConditionItem: TLogicalEventConditionItem);
begin
  FConditionItem := AConditionItem;
end;

function TConditionItemData.getDisplayName(): AnsiString;
begin
  if FConditionItem.SubExpressions.Count > 0 then
  begin
    Result := GetEnumName( TypeInfo(TLogicalOperator), Integer(FConditionItem.LogicalOperator));
  end
  else
  begin
    Result := GetEnumName(TypeInfo(TWinLossCondition), Integer(FConditionItem.ConditionType));
  end;
end;

{ TEventConditionItemFrame }

function TEventConditionItemFrame.BeginEdit: Boolean; stdcall;
begin
  Result := true;
  Show;
end;

function TEventConditionItemFrame.CancelEdit: Boolean; stdcall;
begin
  Result := true;
  Visible:=False;
  CurrentNode := nil;
end;

function TEventConditionItemFrame.EndEdit: Boolean; stdcall;
begin
  Result := true;
  Visible:=False;
  CurrentNode := nil;
end;

function TEventConditionItemFrame.GetBounds: TRect; stdcall;
begin
  Result := BoundsRect;
end;

function TEventConditionItemFrame.PrepareEdit(ATree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
begin
  Result := true;
  Self.Tree := ATree;
  Parent := ATree;
  Visible:=False;
  BorderStyle:=bsSingle;
  CurrentNode := Node;
  HandleNeeded;
  AdjustSize;
end;

procedure TEventConditionItemFrame.ProcessMessage(var Message: TLMessage); stdcall;
begin
  //WindowProc(Message);
end;

procedure TEventConditionItemFrame.SetBounds(R: TRect); stdcall;
begin
  BoundsRect := r;
end;

end.

