unit town_spells_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ActnList,
  base_options_frame, object_options, editor_str_consts, logical_id_condition, editor_classes, base_info, lists_manager;

type

  { TTownSpellsFrame }

  TTownSpellsFrame = class(TBaseOptionsFrame)
    act: TActionList;
    RemoveRequired: TAction;
    AddRequired: TAction;
    RemoveAllowed: TAction;
    AddAllowed: TAction;
    cbLevel: TComboBox;
    lbPossible: TLabel;
    lbPossible1: TLabel;
    Possible: TListBox;
    Obligatory: TListBox;
    AllSpells: TListBox;
    pnObligatory: TPanel;
    pnPossible: TPanel;
    pnRight: TPanel;
    pnLeft: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure AddAllowedExecute(Sender: TObject);
    procedure AddAllowedUpdate(Sender: TObject);
    procedure AddRequiredExecute(Sender: TObject);
    procedure AddRequiredUpdate(Sender: TObject);
    procedure cbLevelChange(Sender: TObject);
    procedure RemoveAllowedExecute(Sender: TObject);
    procedure RemoveAllowedUpdate(Sender: TObject);
    procedure RemoveRequiredExecute(Sender: TObject);
    procedure RemoveRequiredUpdate(Sender: TObject);
  private
    FMaxLevel: integer;
    FAllSpells: array [0..5] of TIdentifierSet;
    FCache, FObject: TLogicalIDCondition;

    procedure FillSpellLevels;
    procedure FillAllSpells;
    procedure FillSpells(Alevel: Integer);

    function SpellsSelected: Boolean;

    procedure Load;

    procedure FillList(Alist: TListBox; AFullList: TStrings; ACache: TStrings);

    procedure AddToList(ASource, ADest: TListBox; ACache: TStrings);
    procedure RemoveFromList(ADest: TListBox; ACache: TStrings);
  protected
    procedure VisitNormalTown(AOptions: TTownOptions); override;
    procedure VisitRandomTown(AOptions: TTownOptions); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ TTownSpellsFrame }

procedure TTownSpellsFrame.cbLevelChange(Sender: TObject);
begin
  if cbLevel.ItemIndex < 0 then
  begin
    cbLevel.ItemIndex := 0;
  end;

  FillSpells(cbLevel.ItemIndex);

  FillList(Possible, FAllSpells[cbLevel.ItemIndex], FCache.AnyOf);
  FillList(Obligatory, FAllSpells[cbLevel.ItemIndex], FCache.AllOf);
end;

procedure TTownSpellsFrame.RemoveAllowedExecute(Sender: TObject);
begin
  RemoveFromList(Possible, FCache.AnyOf);
end;

procedure TTownSpellsFrame.RemoveAllowedUpdate(Sender: TObject);
begin
 (Sender as TAction).Enabled:=Possible.SelCount > 0;
end;

procedure TTownSpellsFrame.RemoveRequiredExecute(Sender: TObject);
begin
  RemoveFromList(Obligatory, FCache.AllOf);
end;

procedure TTownSpellsFrame.RemoveRequiredUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Obligatory.SelCount > 0;
end;

procedure TTownSpellsFrame.AddAllowedUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=SpellsSelected;
end;

procedure TTownSpellsFrame.AddRequiredExecute(Sender: TObject);
begin
  AddToList(AllSpells, Obligatory, FCache.AllOf);
end;

procedure TTownSpellsFrame.AddAllowedExecute(Sender: TObject);
begin
  AddToList(AllSpells, Possible, FCache.AnyOf);
end;

procedure TTownSpellsFrame.AddRequiredUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=SpellsSelected;
end;

procedure TTownSpellsFrame.FillSpellLevels;
var
  lvl: Integer;
begin
  cbLevel.Items.Clear;

  cbLevel.Items.Add(rsAllSpells);

  for lvl := 1 to FMaxLevel do
  begin
    cbLevel.Items.Add(Format(rsSpellsOfLevel,[lvl]));
  end;

  cbLevel.ItemIndex:=0;//all spells
end;

procedure TTownSpellsFrame.FillAllSpells;
var
  i: Integer;
begin
  FAllSpells[0].Clear;
  for i := 1 to FMaxLevel do
  begin
    ListsManager.SpellInfos.FillWithAllIds(FAllSpells[i], i);
    FAllSpells[0].AddStrings(FAllSpells[i]);
  end;
end;

procedure TTownSpellsFrame.FillSpells(Alevel: Integer);
begin
  if Alevel < 0 then
  begin
    Alevel:=0;
  end;

  AllSpells.Items.Assign(FAllSpells[Alevel]);
end;

function TTownSpellsFrame.SpellsSelected: Boolean;
begin
  Result := AllSpells.SelCount > 0;
end;

procedure TTownSpellsFrame.Load;
begin
  FillSpellLevels;
  FillAllSpells;
  FillSpells(0);

  FCache.Assign(FObject);
  FCache.SetPermissive(FAllSpells[0], false);

  FillList(Possible, FAllSpells[0], FCache.AnyOf);
  FillList(Obligatory, FAllSpells[0], FCache.AllOf);
end;

procedure TTownSpellsFrame.FillList(Alist: TListBox; AFullList: TStrings; ACache: TStrings);
var
  i: Integer;
  info: TBaseInfo;
  item_name: TCaption;
begin
  Alist.Items.Clear;

  for i := 0 to AFullList.Count - 1 do
  begin
    item_name := AFullList[i];
    info := AFullList.Objects[i] as TBaseInfo;

    if ACache.IndexOf(info.Identifier) >=0 then
    begin
      Alist.Items.AddObject(item_name, info);
    end;
  end;
end;

procedure TTownSpellsFrame.AddToList(ASource, ADest: TListBox; ACache: TStrings);
var
  i: Integer;
begin
  for i := 0 to ASource.Items.Count - 1 do
  begin
    if ASource.Selected[i] then
    begin
      ACache.Add((ASource.Items.Objects[i] as TBaseInfo).Identifier);
      ASource.Selected[i] := false;
    end;
  end;

  FillList(ADest, FAllSpells[cbLevel.ItemIndex], ACache);
end;

procedure TTownSpellsFrame.RemoveFromList(ADest: TListBox; ACache: TStrings);
var
  i: Integer;
begin
  for i := 0 to ADest.Items.Count - 1 do
  begin
    if ADest.Selected[i] then
    begin
      ACache.Delete(ACache.IndexOf((ADest.Items.Objects[i] as TBaseInfo).Identifier));
    end;
  end;
  FillList(ADest, FAllSpells[cbLevel.ItemIndex], ACache);
end;

procedure TTownSpellsFrame.VisitNormalTown(AOptions: TTownOptions);
var
  factionId: String;
  faction: TFactionInfo;
begin
  factionId := AOptions.MapObject.GetSubId;

  faction := ListsManager.GetFaction(factionId);

  FMaxLevel:=faction.Town.MageGuild;

  inherited VisitNormalTown(AOptions);
  FObject := AOptions.Spells;
  Load;
end;

procedure TTownSpellsFrame.VisitRandomTown(AOptions: TTownOptions);
begin
  FMaxLevel := 5;
  inherited VisitRandomTown(AOptions);
  FObject := AOptions.Spells;
  Load;
end;

constructor TTownSpellsFrame.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(TheOwner);
  FCache := TLogicalIDCondition.Create(nil);

  for i := Low(FAllSpells) to High(FAllSpells) do
    FAllSpells[i] := TIdentifierSet.Create(nil);

end;

destructor TTownSpellsFrame.Destroy;
var
  i: Integer;
begin
  for i := Low(FAllSpells) to High(FAllSpells) do
    FAllSpells[i].Free;

  FCache.Free;
  inherited Destroy;
end;

procedure TTownSpellsFrame.Commit;
begin
  inherited Commit;
  FCache.SetPermissive(FAllSpells[0], true);
  FObject.Assign(FCache);
end;

end.

