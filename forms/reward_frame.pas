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
unit reward_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, typinfo, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ActnList, Spin, Buttons,
  contnrs, base_options_frame, gui_helpers, object_options, base_info, editor_classes, editor_types, editor_utils,
  lists_manager;

type


  { TRewardInfo }

  TRewardInfo = class
  public
    constructor Create(AMetaClassList: TMetaclassInfos; AReward: TReward);

    constructor Create(AMetaClass: TMetaclassInfo; AScope: AnsiString; AIdentifier: AnsiString; AValue: Int64);

    function ToString: ansistring; override;

    procedure SaveTo(AReward: TReward);
  public
    metaclass: TMetaclassInfo;
    scope: AnsiString;
    identifier: AnsiString;
    value: Int64;
  end;


  { TRewardFrame }

  TRewardFrame = class(TBaseOptionsFrame)
    act: TActionList;
    actAdd: TAction;
    actDelete: TAction;
    TypeEdit: TComboBox;
    MetaTypeEdit: TComboBox;
    iml: TImageList;
    RewardsEdit: TListBox;
    AmountEdit: TSpinEdit;
    ButtonAdd: TSpeedButton;
    ButtonRemove: TSpeedButton;
    procedure actAddExecute(Sender: TObject);
    procedure actAddUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure MetaTypeEditSelect(Sender: TObject);
    procedure RewardsEditSelectionChange(Sender: TObject; User: boolean);
  private
    FFreeList: TObjectList;
    FObject: TRewards;

    procedure SetupMetaTypes;

    procedure LoadValue(AMetaClass: TMetaclassInfo; AValue: Int64);
  protected
    procedure Load; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;
    procedure VisitLocalEvent(AOptions: TLocalEventOptions); override;
    procedure VisitPandorasBox(AOptions: TPandorasOptions); override;
    procedure VisitSeerHut(AOptions: TSeerHutOptions); override;
  end;

implementation

{$R *.lfm}

{ TRewardInfo }

constructor TRewardInfo.Create(AMetaClassList: TMetaclassInfos; AReward: TReward);
var
  m: TMetaclass;
begin
  m := TMetaclass.invalid;

  DecodeFullIdentifier(AReward.Identifier, m, scope, identifier);

  metaclass := AMetaClassList.FindItem(GetEnumName(TypeInfo(TMetaclass),Integer(m)));

  value := AReward.Value;
end;

constructor TRewardInfo.Create(AMetaClass: TMetaclassInfo; AScope: AnsiString; AIdentifier: AnsiString; AValue: Int64);
begin
  metaclass := AMetaClass;
  scope := AScope;
  Identifier:=AIdentifier;
  value:=AValue;
end;

function TRewardInfo.ToString: ansistring;
begin
  Result:=Format('%d %s %s %s',[value, metaclass.Name, scope, identifier]);
end;

procedure TRewardInfo.SaveTo(AReward: TReward);
begin
  AReward.Value:=value;
  AReward.Identifier:=EncodeFullIdentifier(metaclass.Identifier, scope, identifier);
end;

{ TRewardFrame }

procedure TRewardFrame.RewardsEditSelectionChange(Sender: TObject; User: boolean);
var
  idx, i: Integer;
  o: TRewardInfo;
begin
  idx := RewardsEdit.ItemIndex;

  if idx < 0 then
  begin
    MetaTypeEdit.ItemIndex:=-1;
    LoadValue(nil, 0);
    exit;
  end;

  o := TRewardInfo(RewardsEdit.Items.Objects[idx]);

  idx := -1;

  for i := 0 to MetaTypeEdit.Items.Count - 1 do
  begin
    if o.metaclass = TMetaclassInfo(MetaTypeEdit.Items.Objects[i]) then
    begin
      idx := i;
      Break;
    end;
  end;

  MetaTypeEdit.ItemIndex:=idx;

  TypeEdit.Items.Clear;
  TypeEdit.Enabled := o.metaclass.IsEntity;

  if TypeEdit.Enabled then
  begin
    TypeEdit.FillFromList(o.metaclass.List, o.identifier);
  end;

  LoadValue(o.metaclass, o.value);
end;

procedure TRewardFrame.SetupMetaTypes;
var
  tp: TMetaclass;
  identifier: AnsiString;
  info: TMetaclassInfo;
begin
  MetaTypeEdit.Clear;

  for tp in FObject.AllowedReward do
  begin
    identifier :=  GetEnumName(TypeInfo(TMetaclass), Integer(tp));
    info := ListsManager.Metaclasses.FindItem(identifier);
    MetaTypeEdit.AddItem(info.Name, info);
  end;
end;

procedure TRewardFrame.LoadValue(AMetaClass: TMetaclassInfo; AValue: Int64);
begin
  AmountEdit.Value := AValue;

  if Assigned(AMetaClass) then
  begin
    AmountEdit.MinValue:=AMetaClass.MinValue;
    AmountEdit.MaxValue:=AMetaClass.MaxValue;
  end
  else
  begin
    AmountEdit.MinValue:=0;
    AmountEdit.MaxValue:=0;
  end;
end;

procedure TRewardFrame.actAddExecute(Sender: TObject);
var
  i, matched: Integer;
  o, reward: TRewardInfo;
  meta_class: TMetaclassInfo;
  tp_info: TBaseInfo;
  identifier, scope: AnsiString;
begin
  //update value if metaclass and type match or add new item

  matched := -1;

  meta_class := TMetaclassInfo(MetaTypeEdit.SelectedInfo);

  if not Assigned(meta_class) then
  begin
    ShowMessage('No metaclass selected!');
    Exit;
  end;

  if TypeEdit.Enabled then
  begin
    tp_info := TypeEdit.SelectedInfo;

    if not Assigned(tp_info) then
    begin
      ShowMessage('No object selected!');
      Exit;
    end;
  end
  else
  begin
    tp_info := nil;
  end;

  if Assigned(tp_info) then
  begin
    identifier := tp_info.Identifier;
    scope:=ExtractModID2(identifier);
  end
  else
  begin
    identifier := '';
    scope := '';
  end;

  for i := 0 to RewardsEdit.Items.Count - 1 do
  begin
    o := TRewardInfo(RewardsEdit.Items.Objects[i]);

    if (o.metaclass = meta_class) and (o.identifier = identifier) and (o.scope = scope) then
    begin
      matched := i;
      Break;
    end;
  end;

  if matched < 0 then
  begin
    //add
    reward := TRewardInfo.Create(meta_class, scope, identifier, AmountEdit.Value);
    RewardsEdit.AddItem(reward.ToString(), reward);
    FFreeList.Add(reward);
  end
  else
  begin
    //update
    reward := TRewardInfo(RewardsEdit.Items.Objects[matched]);

    reward.value := AmountEdit.Value;

    RewardsEdit.Items[i] := reward.ToString();
    RewardsEdit.Items.Objects[i] := reward;
  end;
end;

procedure TRewardFrame.actAddUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (MetaTypeEdit.ItemIndex >= 0) and ((not TypeEdit.Enabled) or (TypeEdit.ItemIndex >= 0)) and (AmountEdit.Value <> 0);
end;

procedure TRewardFrame.actDeleteExecute(Sender: TObject);
var
  idx: Integer;
begin
  idx := RewardsEdit.ItemIndex;

  if idx < 0 then
    exit;
  RewardsEdit.Items.Delete(idx);
end;

procedure TRewardFrame.actDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=RewardsEdit.ItemIndex >= 0;
end;

procedure TRewardFrame.MetaTypeEditSelect(Sender: TObject);
var
  o: TMetaclassInfo;
begin
  TypeEdit.Clear;

  o := TMetaclassInfo(MetaTypeEdit.SelectedInfo);

  if not Assigned(o) then
    exit;

  TypeEdit.Enabled := o.IsEntity;

  if TypeEdit.Enabled then
  begin
    TypeEdit.FillFromList(o.List, '');
  end;

  LoadValue(o, 1);
end;

procedure TRewardFrame.Load;
var
  i: Integer;
  info: TRewardInfo;
  reward: TReward;
begin
  inherited Load;

  SetupMetaTypes;

  RewardsEdit.Clear;

  for i := 0 to FObject.Count - 1 do
  begin
    reward := FObject.Items[i];

    info := TRewardInfo.Create(ListsManager.Metaclasses, reward);

    RewardsEdit.AddItem(info.ToString(), info);
    FFreeList.Add(info);
  end;
end;

constructor TRewardFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFreeList := TObjectList.Create(true);
end;

destructor TRewardFrame.Destroy;
begin
  FFreeList.Free;
  inherited Destroy;
end;

procedure TRewardFrame.Commit;
var
  i: Integer;
  info: TRewardInfo;
  reward: TReward;
begin
  FObject.Clear;

  for i := 0 to RewardsEdit.Items.Count - 1 do
  begin
    info := RewardsEdit.Items.Objects[i] as TRewardInfo;
    reward := FObject.Add;
    info.SaveTo(reward);
  end;

  inherited Commit;
end;

procedure TRewardFrame.VisitLocalEvent(AOptions: TLocalEventOptions);
begin
  inherited VisitLocalEvent(AOptions);
end;

procedure TRewardFrame.VisitPandorasBox(AOptions: TPandorasOptions);
begin
  inherited VisitPandorasBox(AOptions);
end;

procedure TRewardFrame.VisitSeerHut(AOptions: TSeerHutOptions);
begin
  inherited VisitSeerHut(AOptions);
  FObject := AOptions.Reward;
  Load;
end;

end.

