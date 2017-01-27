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

unit field_editors;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, typinfo, Controls, StdCtrls, Spin, editor_rtti, editor_types;

type
  TOnGetString = function(): TLocalizedString of object;

  { TAbstractFieldEditor }

  TAbstractFieldEditor = class abstract
  public
    procedure Commit; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure ReloadDefaults; virtual;
    function IsDirty: Boolean; virtual; abstract;
  end;

  { TBaseFieldEditor }

  TBaseFieldEditor = class abstract(TAbstractFieldEditor)
  protected
    FTarget: TObject;
    FPropName: string;
    FPropInfo: PPropInfo;

    FEditorControl: TWinControl;

    function IsCustomized: Boolean; virtual;
  public
    constructor Create(ATarget: TObject; const APropName: string; AEditorControl: TWinControl);
  end;

  { TBaseOptFieldEditor }

  TBaseOptFieldEditor = class abstract(TBaseFieldEditor)
  strict private
    FCheck: TCustomCheckBox;
    procedure CheckChange(Sender: TObject);
  protected
    procedure LoadCustom; virtual; abstract;
    procedure LoadDefault; virtual; abstract;

    procedure StashCustom; virtual; abstract;
    procedure RestoreCustom; virtual; abstract;

    function IsCustomized: Boolean; override;
    function IsInitialDefault: Boolean;

    procedure SaveCustom; virtual; abstract;
    procedure SaveDefault; virtual; abstract;

    procedure UpdateControls;
  public
    constructor Create(ATarget: TObject; const APropName: string; AEditorControl: TWinControl; ACheck: TCustomCheckBox);

    procedure Commit; override;
    procedure Load; override;
  end;

  { TStringFieldEditor }

  TStringFieldEditor = class(TBaseFieldEditor)
  strict private
    FOldValue: string;
    FWidget: TCustomEdit;
  public
    constructor Create(ATarget: TObject; const APropName: string; AWidget: TCustomEdit);

    procedure Commit; override;
    function IsDirty: Boolean; override;
    procedure Load; override;
  end;


  { TOptStringFieldEditor }

  TOptStringFieldEditor = class(TBaseOptFieldEditor)
  strict private
    FOldValue: string;
    FWidget: TCustomEdit;
    FCustomText: TLocalizedString;
    FOnGetDefaultText:TOnGetString;

    function DoGetDefaultText: TLocalizedString;
  protected
    procedure LoadCustom; override;
    procedure LoadDefault; override;

    procedure StashCustom; override;
    procedure RestoreCustom; override;

    procedure SaveCustom; override;
    procedure SaveDefault; override;
  public
    constructor Create(ATarget: TObject; const APropName: string; AWidget: TCustomEdit; ACheck: TCustomCheckBox);
    constructor Create(ATarget: TObject; const APropName: string; AWidget: TCustomEdit; ACheck: TCustomCheckBox; AGetDefaultValue: TOnGetString);

    procedure Load; override;
    function IsDirty: Boolean; override;
    procedure ReloadDefaults; override;
  end;

  { TIntEditor }

  TIntEditor = class(TBaseFieldEditor)
  private
    FOldValue: Int64;
    FWidget: TCustomSpinEdit;
  public
    constructor Create(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit);

    procedure Commit; override;
    function IsDirty: Boolean; override;
    procedure Load; override;
  end;

  { TOptIntEditor }

  TOptIntEditor = class(TBaseOptFieldEditor)
  private
    FOldValue: Int64;
    FWidget: TCustomSpinEdit;
    FDefaultValue: Int64;
    FCustomValue: Int64;
  protected
    procedure LoadCustom; override;
    procedure LoadDefault; override;

    procedure StashCustom; override;
    procedure RestoreCustom; override;

    procedure SaveCustom; override;
    procedure SaveDefault; override;
  public
    constructor Create(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit; ACheck: TCustomCheckBox);
    function IsDirty: Boolean; override;
    procedure Load; override;
  end;

  { TEnumEditor }

  TEnumEditor = class(TBaseFieldEditor)
  private
    FOldValue: Int64;
    FDefaultValue: Int64;
    FCustomValue: Int64;

    FWidget: TCustomComboBox;
  public
    constructor Create(ATarget: TObject; const APropName: string; AWidget: TCustomComboBox);
    procedure Commit; override;
    function IsDirty: Boolean; override;
    procedure Load; override;
  end;

  { TFieldEditors }

  TFieldEditors = class(specialize TFPGObjectList<TAbstractFieldEditor>)
  public
    constructor Create;
    procedure Commit;
    procedure Load;
    procedure ReloadDefaults;

    function IsDirty: Boolean;
  end;

implementation

{ TStringFieldEditor }

constructor TStringFieldEditor.Create(ATarget: TObject; const APropName: string; AWidget: TCustomEdit);
begin
  FWidget := AWidget;
  inherited Create(ATarget, APropName, AWidget);
end;

procedure TStringFieldEditor.Commit;
begin
  SetStrProp(FTarget, FPropInfo, FWidget.Text);
end;

function TStringFieldEditor.IsDirty: Boolean;
begin
  Result:=FOldValue <> FWidget.Text;
end;

procedure TStringFieldEditor.Load;
begin
  FOldValue:= GetStrProp(FTarget, FPropInfo);
  FWidget.Text := FOldValue;
end;

{ TAbstractFieldEditor }

procedure TAbstractFieldEditor.ReloadDefaults;
begin
  //do nothing
end;

{ TBaseOptFieldEditor }

procedure TBaseOptFieldEditor.CheckChange(Sender: TObject);
begin
  UpdateControls;

  if IsCustomized then
  begin
    RestoreCustom;
  end
  else
  begin
    StashCustom;
  end;
end;

function TBaseOptFieldEditor.IsCustomized: Boolean;
begin
  Result := FCheck.State in [cbChecked];
end;

constructor TBaseOptFieldEditor.Create(ATarget: TObject; const APropName: string; AEditorControl: TWinControl;
  ACheck: TCustomCheckBox);
begin
  inherited Create(ATarget, APropName, AEditorControl);
  FCheck := ACheck;

  FCheck.OnChange := @CheckChange;
end;

procedure TBaseOptFieldEditor.Commit;
begin
  if IsCustomized then
  begin
    SaveCustom;
  end
  else
  begin
    SaveDefault;
  end;
end;

procedure TBaseOptFieldEditor.Load;
begin
  if IsInitialDefault then
  begin
    LoadDefault;
    FCheck.State:=cbUnchecked;
  end
  else
  begin
    LoadCustom;
    FCheck.State:=cbChecked;
  end;

  UpdateControls;
end;

function TBaseOptFieldEditor.IsInitialDefault: Boolean;
begin
  Result := IsDefaultValue(FTarget, FPropInfo);
end;

procedure TBaseOptFieldEditor.UpdateControls;
begin
  FEditorControl.Enabled := IsCustomized();
end;

{ TOptIntEditor }

procedure TOptIntEditor.LoadCustom;
begin
  FWidget.Value := GetOrdProp(FTarget, FPropInfo);
end;

procedure TOptIntEditor.LoadDefault;
begin
  FWidget.Value:=FDefaultValue;
end;

procedure TOptIntEditor.StashCustom;
begin
  FCustomValue:=FWidget.Value;
  FWidget.Value := FDefaultValue;
end;

procedure TOptIntEditor.RestoreCustom;
begin
  FWidget.Value := FCustomValue;
end;

procedure TOptIntEditor.SaveCustom;
begin
  SetOrdProp(FTarget, FPropInfo, FWidget.Value);
end;

procedure TOptIntEditor.SaveDefault;
begin
  SetOrdProp(FTarget, FPropName, FDefaultValue);
end;

constructor TOptIntEditor.Create(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit;
  ACheck: TCustomCheckBox);
begin
  inherited Create(ATarget, APropName, AWidget, ACheck);
  FWidget := AWidget;

  FDefaultValue:=FPropInfo^.Default;

  if FDefaultValue = longint($80000000) then
  begin
    raise Exception.CreateFmt('TOptIntEditor: property %s has no default value',[APropName]);
  end;
end;

function TOptIntEditor.IsDirty: Boolean;
begin
  if IsCustomized then
  begin
    Result := FOldValue = FWidget.Value;
  end
  else
  begin
    Result := FOldValue = FDefaultValue;
  end;
end;

procedure TOptIntEditor.Load;
begin
  FOldValue := GetOrdProp(FTarget, FPropInfo);
  inherited Load;
end;

{ TIntEditor }

constructor TIntEditor.Create(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit);
begin
  FWidget := AWidget;
  inherited Create(ATarget, APropName, AWidget);
end;

procedure TIntEditor.Commit;
begin
  SetOrdProp(FTarget, FPropInfo, FWidget.Value);
end;

function TIntEditor.IsDirty: Boolean;
begin
  Result:=FWidget.Value <> FOldValue;
end;

procedure TIntEditor.Load;
begin
  FOldValue:=GetOrdProp(FTarget, FPropInfo);
  FWidget.Value := FOldValue;
end;

{ TOptStringFieldEditor }

function TOptStringFieldEditor.DoGetDefaultText: TLocalizedString;
begin
  if Assigned(FOnGetDefaultText) then
  begin
    Result := FOnGetDefaultText();
  end
  else
  begin
    Result := '';
  end;
end;

procedure TOptStringFieldEditor.LoadCustom;
begin
  FWidget.Text := GetStrProp(FTarget, FPropInfo);
end;

procedure TOptStringFieldEditor.LoadDefault;
begin
  FWidget.Text := DoGetDefaultText();
end;

procedure TOptStringFieldEditor.StashCustom;
begin
  FCustomText := FWidget.Text;
  FWidget.Text := DoGetDefaultText();
end;

procedure TOptStringFieldEditor.RestoreCustom;
begin
  FWidget.Text := FCustomText;
end;

procedure TOptStringFieldEditor.SaveCustom;
begin
  SetStrProp(FTarget, FPropInfo, FWidget.Text);
end;

procedure TOptStringFieldEditor.SaveDefault;
begin
  SetStrProp(FTarget, FPropInfo, '');
end;

constructor TOptStringFieldEditor.Create(ATarget: TObject; const APropName: string; AWidget: TCustomEdit;
  ACheck: TCustomCheckBox);
begin
  FWidget := AWidget;
  Inherited Create(ATarget, APropName, AWidget, ACheck);
end;

constructor TOptStringFieldEditor.Create(ATarget: TObject; const APropName: string; AWidget: TCustomEdit;
  ACheck: TCustomCheckBox; AGetDefaultValue: TOnGetString);
begin
  Create(ATarget, APropName, AWidget, ACheck);
  FOnGetDefaultText := AGetDefaultValue;
end;

procedure TOptStringFieldEditor.Load;
begin
  FCustomText := GetStrProp(FTarget, FPropInfo);
  FOldValue := FCustomText;
  inherited Load;
end;

function TOptStringFieldEditor.IsDirty: Boolean;
begin
  if IsCustomized then
  begin
    Result := FOldValue = FCustomText;
  end
  else
  begin
    Result := FOldValue = '';
  end;
end;

procedure TOptStringFieldEditor.ReloadDefaults;
begin
  inherited ReloadDefaults;
  if not IsCustomized then
  begin
    FWidget.Text := DoGetDefaultText();
  end;
end;

{ TBaseFieldEditor }

function TBaseFieldEditor.IsCustomized: Boolean;
begin
  Result := true;
end;

constructor TBaseFieldEditor.Create(ATarget: TObject; const APropName: string; AEditorControl: TWinControl);
begin
  FTarget := ATarget;
  FPropName := APropName;

  FPropInfo := GetPropInfo(ATarget, APropName);
  FEditorControl := AEditorControl;
end;

{ TEnumEditor }

constructor TEnumEditor.Create(ATarget: TObject; const APropName: string; AWidget: TCustomComboBox);
var
  i, cnt: SizeInt;
  value_name: AnsiString;
begin
  FWidget := AWidget;
  inherited Create(ATarget, APropName, AWidget);

  AWidget.Items.Clear;

  cnt := GetEnumNameCount(FPropInfo^.PropType);

  for i := 0 to cnt - 1 do
  begin
    value_name := GetEnumName(FPropInfo^.PropType, i);
    AWidget.Items.Add(value_name);
  end;
end;

procedure TEnumEditor.Commit;
begin
  SetOrdProp(FTarget, FPropInfo, FWidget.ItemIndex);
end;

function TEnumEditor.IsDirty: Boolean;
begin
  Result := FWidget.ItemIndex <> FOldValue;
end;

procedure TEnumEditor.Load;
begin
  FOldValue:=GetOrdProp(FTarget, FPropInfo);
  FWidget.ItemIndex := FOldValue;
end;

{ TFieldEditors }

constructor TFieldEditors.Create;
begin
  inherited Create(True);
end;

procedure TFieldEditors.Commit;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Commit;
  end;
end;

procedure TFieldEditors.Load;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].load;
  end;
end;

procedure TFieldEditors.ReloadDefaults;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ReloadDefaults;
  end;
end;

function TFieldEditors.IsDirty: Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].IsDirty then
      exit(True);
  end;
  Result := False;
end;

end.

