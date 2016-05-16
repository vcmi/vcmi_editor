{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit base_options_frame;

{$I compilersetup.inc}

{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, gvector, FileUtil, LCLType, Forms, Controls, ComCtrls,
  Spin, Grids, ExtCtrls, StdCtrls, editor_types, object_options, map,
  lists_manager, editor_consts, field_editors;

type

  IObjectOptionsNotify = interface
    procedure InstanceTypeChanged(ANewValue: AnsiString);
  end;

   //todo: FIXME reload defaults of all frames (f.e. on type change)

  { TBaseOptionsFrame }

  TBaseOptionsFrame = class(TFrame, IObjectOptionsVisitor, IObjectOptionsNotify)
  private
    FListsManager: TListsManager;
    FMainIdentifier: AnsiString;
    FMap: TVCMIMap;
    FFieldEditors: TFieldEditors;
    procedure SetMap(AValue: TVCMIMap);
  protected
    FUseMapDefaults: Boolean;

    procedure ReadResourceSet(AParentControl: TWinControl; ASrc: TResourceSet);
    procedure SaveResourceSet(AParentControl: TWinControl; ADest: TResourceSet);

    procedure VisitNormalTown({%H-}AOptions: TTownOptions);virtual;
    procedure VisitRandomTown({%H-}AOptions: TTownOptions);virtual;

    procedure HandleStringGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleStringGridResize(Sender: TObject);

    procedure ReadOwner(AOptions: TObjectOptions; AEditor: TCustomRadioGroup);
    procedure WriteOwner(AOptions: TObjectOptions; AEditor: TCustomRadioGroup);

    property InstanceType: AnsiString read FMainIdentifier;

    procedure Load; virtual;

    procedure ApplyDefaults(); virtual;
    procedure ReloadDefaults(); virtual;

    procedure UpdateControls(); virtual;

    procedure DoUpdateText(AControl: TCustomEdit; AFlag: TCustomCheckBox; ACustom: TLocalizedString; ADefault: TLocalizedString);

    procedure AddStrEditor(ATarget: TObject; const APropName: string; AWidget: TCustomEdit; ACheck: TCustomCheckBox);
    procedure AddStrEditor(ATarget: TObject; const APropName: string; AWidget: TCustomEdit; ACheck: TCustomCheckBox; ACallback: TOnGetString);

    procedure AddIntEditor(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit);
    procedure AddIntEditor(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit; ACheck: TCustomCheckBox);

    procedure NotifyInstanceTypeChange(ANewValue: AnsiString);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; virtual;

  public
    procedure InstanceTypeChanged(ANewValue: AnsiString); virtual;

  public //IObjectOptionsVisitor
    procedure VisitLocalEvent({%H-}AOptions: TLocalEventOptions); virtual;
    procedure VisitSignBottle({%H-}AOptions: TSignBottleOptions);virtual;
    procedure VisitNormalHero({%H-}AOptions: TNormalHeroOptions);virtual;
    procedure VisitRandomHero({%H-}AOptions: TRandomHeroOptions);virtual;
    procedure VisitPrison({%H-}AOptions: TPrisonOptions);virtual;
    procedure VisitMonster({%H-}AOptions: TCreatureOptions);virtual;
    procedure VisitSeerHut({%H-}AOptions: TSeerHutOptions);virtual;
    procedure VisitWitchHut({%H-}AOptions: TWitchHutOptions);virtual;
    procedure VisitScholar({%H-}AOptions: TScholarOptions);virtual;
    procedure VisitGarrison({%H-}AOptions: TGarrisonOptions);virtual;
    procedure VisitArtifact({%H-}AOptions: TArtifactOptions);virtual;
    procedure VisitSpellScroll({%H-}AOptions: TSpellScrollOptions);virtual;
    procedure VisitResource({%H-}AOptions: TResourceOptions);virtual;
    procedure VisitTown(AOptions: TTownOptions);virtual;
    procedure VisitAbandonedMine({%H-}AOptions: TAbandonedOptions); virtual;
    procedure VisitMine({%H-}AOptions: TMineOptions); virtual;
    procedure VisitShrine({%H-}AOptions: TShrineOptions);virtual;
    procedure VisitPandorasBox({%H-}AOptions: TPandorasOptions);virtual;
    procedure VisitGrail({%H-}AOptions: TGrailOptions);virtual;
    procedure VisitRandomDwelling({%H-}AOptions: TRandomDwellingOptions);virtual;
    procedure VisitRandomDwellingLVL({%H-}AOptions: TRandomDwellingLVLOptions);virtual;
    procedure VisitRandomDwellingTown({%H-}AOptions: TRandomDwellingTownOptions);virtual;
    procedure VisitQuestGuard({%H-}AOptions:TQuestGuardOptions);virtual;
    procedure VisitHeroPlaceholder({%H-}AOptions: THeroPlaceholderOptions);virtual;

    procedure VisitOwnedObject({%H-}AOptions: TOwnedObjectOptions);virtual;

    procedure VisitHero({%H-}AOptions: THeroOptions);virtual;
  public //map options

    procedure VisitHeroDefinition({%H-}AOptions: THeroDefinition); virtual;

  public
    property ListsManager: TListsManager read FListsManager;
    property Map: TVCMIMap read FMap write SetMap;

    function IsDirty: Boolean; virtual;

    function Validate: String; virtual;
  end;

  TBaseOptionsFrameClass = class of TBaseOptionsFrame;

  { TBaseOptionsFrameList }

  TBaseOptionsFrameVector = specialize TVector<TBaseOptionsFrame>;

  TBaseOptionsFrameList = class(TComponent, IObjectOptionsNotify)
  private
    FListsManager: TListsManager;
    FMap: TVCMIMap;

    FData:  TBaseOptionsFrameVector;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InstanceTypeChanged(ANewValue: AnsiString);

    procedure AddFrame(AClass: TBaseOptionsFrameClass; AOptions: TObjectOptions; AParent:TTabSheet);
    procedure AddFrame(AClass: TBaseOptionsFrameClass; AOptions: THeroDefinition; AParent:TTabSheet);
    procedure Clear;

    procedure Commit;

    property ListsManager: TListsManager read FListsManager write FListsManager;
    property Map: TVCMIMap read FMap write FMap;

    function IsDirty: Boolean;
  end;

implementation

{ TBaseOptionsFrameList }

constructor TBaseOptionsFrameList.Create(AOwner: TComponent);
begin
  FData := TBaseOptionsFrameVector.Create;
  inherited Create(AOwner);

end;

destructor TBaseOptionsFrameList.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TBaseOptionsFrameList.InstanceTypeChanged(ANewValue: AnsiString);
var
  i: SizeInt;
begin
  for i := 0 to FData.Size - 1 do
  begin
    FData[i].InstanceTypeChanged(ANewValue);
  end;
end;

procedure TBaseOptionsFrameList.AddFrame(AClass: TBaseOptionsFrameClass;
  AOptions: TObjectOptions; AParent: TTabSheet);
var
  F: TBaseOptionsFrame;
begin
  Assert(Assigned(FMap));
  Assert(Assigned(FListsManager));

  F := AClass.Create(Self);
  F.Parent := AParent;
  F.Align := alClient;
  F.Map := Map;
  AOptions.ApplyVisitor(F); //do AFTER assign properties
  FData.PushBack(F);
  AParent.TabVisible := true;
end;

procedure TBaseOptionsFrameList.AddFrame(AClass: TBaseOptionsFrameClass;
  AOptions: THeroDefinition; AParent: TTabSheet);
var
  F: TBaseOptionsFrame;
begin
  Assert(Assigned(FMap));
  Assert(Assigned(FListsManager));

  F := AClass.Create(Self);
  F.Parent := AParent;
  F.Align := alClient;
  F.Map := Map;
  F.VisitHeroDefinition(AOptions); //do AFTER assign properties
  FData.PushBack(F);
  AParent.TabVisible := true;

end;

procedure TBaseOptionsFrameList.Clear;
begin
  FData.Clear;
end;

procedure TBaseOptionsFrameList.Commit;
var
  i: SizeInt;
begin
  for i := 0 to FData.Size - 1 do
  begin
    FData[i].Commit;
  end;
end;

function TBaseOptionsFrameList.IsDirty: Boolean;
var
  i: SizeInt;
begin
  Result := false;
  for i := 0 to FData.Size - 1 do
  begin
    if FData[i].IsDirty then
     Exit(True);
  end;
end;

{$R *.lfm}

{ TBaseOptionsFrame }

procedure TBaseOptionsFrame.Commit;
begin
  FFieldEditors.Commit;
end;

procedure TBaseOptionsFrame.InstanceTypeChanged(ANewValue: AnsiString);
begin
  FMainIdentifier:=ANewValue;

  ReloadDefaults();
  ApplyDefaults();
end;

constructor TBaseOptionsFrame.Create(TheOwner: TComponent);
begin
  if TheOwner is TBaseOptionsFrameList  then
  begin
    FListsManager := TBaseOptionsFrameList(TheOwner).ListsManager;
  end
  else if TheOwner is TBaseOptionsFrame then
  begin
    FListsManager := TBaseOptionsFrame(TheOwner).ListsManager;
  end
  else
  begin
    raise Exception.Create('Invalid owner');
  end;

  inherited Create(TheOwner);

  FFieldEditors := TFieldEditors.Create;
end;

destructor TBaseOptionsFrame.Destroy;
begin
  FFieldEditors.Free;
  inherited Destroy;
end;

procedure TBaseOptionsFrame.SetMap(AValue: TVCMIMap);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
end;

procedure TBaseOptionsFrame.ReadResourceSet(AParentControl: TWinControl;
  ASrc: TResourceSet);
var
  res_type: TResType;
  c: TControl;
  res_name: String;
  editor: TCustomSpinEdit;
begin
  for res_type in TResType do
  begin
    res_name := RESOURCE_NAMES[res_type];

    c := AParentControl.FindChildControl('ed'+res_name);

    if not Assigned(c) then
    begin
      Assert(false, 'no editor control for '+res_name);
    end;

    if not (c is TCustomSpinEdit) then
    begin
      Assert(false, 'wrong conrol class for '+res_name+ ' '+c.ClassName);
    end;

    editor := TCustomSpinEdit(c);

    editor.Value := ASrc.Amount[res_type];
  end;
end;

procedure TBaseOptionsFrame.SaveResourceSet(AParentControl: TWinControl;
  ADest: TResourceSet);
var
  res_type: TResType;
  c: TControl;
  res_name: String;
  editor: TCustomSpinEdit;
begin
  for res_type in TResType do
  begin
    res_name := RESOURCE_NAMES[res_type];

    c := AParentControl.FindChildControl('ed'+res_name);

    if not Assigned(c) then
    begin
      Assert(false, 'no editor control for '+res_name);
    end;

    if not (c is TCustomSpinEdit) then
    begin
       Assert(false, 'wrong conrol class for '+res_name+ ' '+c.ClassName);
    end;

    editor := TCustomSpinEdit(c);

    ADest.Amount[res_type] := editor.Value;
  end;

end;

procedure TBaseOptionsFrame.VisitNormalHero(AOptions: TNormalHeroOptions);
begin
  VisitHero(AOptions);
end;

procedure TBaseOptionsFrame.VisitRandomHero(AOptions: TRandomHeroOptions);
begin
  VisitHero(AOptions);
end;

procedure TBaseOptionsFrame.VisitPrison(AOptions: TPrisonOptions);
begin
  VisitHero(AOptions);
end;

procedure TBaseOptionsFrame.VisitNormalTown(AOptions: TTownOptions);
begin
 //do nothing
end;

procedure TBaseOptionsFrame.VisitRandomTown(AOptions: TTownOptions);
begin
 //do nothing
end;

procedure TBaseOptionsFrame.HandleStringGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  grid : TCustomStringGrid;
begin
  grid := Sender as TCustomStringGrid;
  if (key = VK_DELETE) and (Shift=[]) then
  begin
    if (grid.RowCount > grid.FixedRows) then
    begin
      grid.DeleteRow(grid.Row);
    end;
    Exit;
  end;

  if (key = VK_INSERT) and (Shift=[]) then
  begin
    grid.InsertColRow(false, grid.Row+1);
    Exit;
  end;
end;

procedure TBaseOptionsFrame.HandleStringGridResize(Sender: TObject);
var
  grid: TCustomStringGrid;
begin
  grid := Sender as TCustomStringGrid;

  if grid.EditorMode then
    grid.Editor.BoundsRect := grid.CellRect(grid.Col,grid.Row);
end;

procedure TBaseOptionsFrame.ReadOwner(AOptions: TObjectOptions;
  AEditor: TCustomRadioGroup);
begin
  if AOptions.Owner = TPlayer.NONE then
  begin
    AEditor.ItemIndex := 0; //no player = 255 -> index 0
  end
  else begin
    AEditor.ItemIndex := Integer(AOptions.Owner)+1;
  end;
end;

procedure TBaseOptionsFrame.WriteOwner(AOptions: TObjectOptions;
  AEditor: TCustomRadioGroup);
begin
  if AEditor.ItemIndex = 0 then
  begin
    AOptions.Owner := TPlayer.NONE;
  end
  else begin
    AOptions.Owner := TPlayer(AEditor.ItemIndex-1);
  end;
end;

procedure TBaseOptionsFrame.Load;
begin
  FFieldEditors.Load;
end;

procedure TBaseOptionsFrame.ApplyDefaults;
begin

end;

procedure TBaseOptionsFrame.ReloadDefaults;
begin
  FFieldEditors.ReloadDefaults;
end;

procedure TBaseOptionsFrame.UpdateControls;
begin
  //do nothing
end;

procedure TBaseOptionsFrame.DoUpdateText(AControl: TCustomEdit;
  AFlag: TCustomCheckBox; ACustom: TLocalizedString; ADefault: TLocalizedString
  );
begin
  if AFlag.State = cbChecked then
  begin
    if ACustom = '' then
    begin
      AControl.Text := ADefault;
    end
    else
    begin
      AControl.Text := ACustom;
    end;
  end
  else
  begin
    AControl.Text := ADefault;
  end;
end;

procedure TBaseOptionsFrame.AddStrEditor(ATarget: TObject; const APropName: string; AWidget: TCustomEdit;
  ACheck: TCustomCheckBox);
begin
  FFieldEditors.Add(TOptStringFieldEditor.Create(ATarget, APropName, AWidget, ACheck));
end;

procedure TBaseOptionsFrame.AddStrEditor(ATarget: TObject; const APropName: string; AWidget: TCustomEdit;
  ACheck: TCustomCheckBox; ACallback: TOnGetString);
begin
  FFieldEditors.Add(TOptStringFieldEditor.Create(ATarget, APropName, AWidget, ACheck, ACallback));
end;

procedure TBaseOptionsFrame.AddIntEditor(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit);
begin
  FFieldEditors.Add(TIntEditor.Create(ATarget, APropName, AWidget));
end;

procedure TBaseOptionsFrame.AddIntEditor(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit;
  ACheck: TCustomCheckBox);
begin
  FFieldEditors.Add(TOptIntEditor.Create(ATarget, APropName, AWidget, ACheck));
end;

procedure TBaseOptionsFrame.NotifyInstanceTypeChange(ANewValue: AnsiString);
begin
  if Owner is TBaseOptionsFrameList then
  begin
    TBaseOptionsFrameList(Owner).InstanceTypeChanged(ANewValue);
  end
  else if Owner is TBaseOptionsFrame then
  begin
    TBaseOptionsFrame(Owner).InstanceTypeChanged(ANewValue);
  end
end;

procedure TBaseOptionsFrame.VisitAbandonedMine(AOptions: TAbandonedOptions
  );
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitMine(AOptions: TMineOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitArtifact(AOptions: TArtifactOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitGarrison(AOptions: TGarrisonOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitGrail(AOptions: TGrailOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitHeroPlaceholder(
  AOptions: THeroPlaceholderOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitLocalEvent(AOptions: TLocalEventOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitMonster(AOptions: TCreatureOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitOwnedObject(AOptions: TOwnedObjectOptions
  );
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitHero(AOptions: THeroOptions);
begin
   //do nothing
end;

procedure TBaseOptionsFrame.VisitHeroDefinition(AOptions: THeroDefinition);
begin
  //do nothing
end;

function TBaseOptionsFrame.IsDirty: Boolean;
begin
  Result := True;
  //TODO: TBaseOptionsFrame.IsDirty
end;

function TBaseOptionsFrame.Validate: String;
begin
  Result := '';
end;

procedure TBaseOptionsFrame.VisitPandorasBox(AOptions: TPandorasOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitQuestGuard(AOptions: TQuestGuardOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitRandomDwelling(
  AOptions: TRandomDwellingOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitResource(AOptions: TResourceOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitScholar(AOptions: TScholarOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitSeerHut(AOptions: TSeerHutOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitShrine(AOptions: TShrineOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitSignBottle(AOptions: TSignBottleOptions);
begin
  //do nothing
end;

procedure TBaseOptionsFrame.VisitSpellScroll(AOptions: TSpellScrollOptions);
begin
  //do nothig
end;

procedure TBaseOptionsFrame.VisitTown(AOptions: TTownOptions);
begin
  case AOptions.MapObject.GetID of
    TYPE_TOWN: VisitNormalTown(AOptions);
    TYPE_RANDOMTOWN: VisitRandomTown(AOptions);
  end;
end;

procedure TBaseOptionsFrame.VisitWitchHut(AOptions: TWitchHutOptions);
begin
  //do nothing
end;

end.

