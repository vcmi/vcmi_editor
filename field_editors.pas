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

unit field_editors;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, typinfo, StdCtrls, Spin;

type

  { TAbstractFieldEditor }

  TAbstractFieldEditor = class abstract
  public
    procedure Commit; virtual; abstract;
    procedure Load; virtual; abstract;
  end;

  { TBaseFieldEditor }

  TBaseFieldEditor = class abstract(TAbstractFieldEditor)
  protected
    FTarget: TObject;
    FPropName: string;
    FPropInfo: PPropInfo;
  public
    constructor Create(ATarget: TObject; const APropName: string);
  end;

  { TOptStringFieldEditor }

  TOptStringFieldEditor = class(TBaseFieldEditor)
  private
    FWidget: TCustomEdit;
    FCheck: TCustomCheckBox;

    procedure CheckChange(Sender: TObject);
  public
    constructor Create(ATarget: TObject; const APropName: string; AWidget: TCustomEdit; ACheck: TCustomCheckBox);

    procedure Commit; override;
    procedure Load; override;
  end;

  { TIntEditor }

  TIntEditor = class(TBaseFieldEditor)
  private
    FWidget: TCustomSpinEdit;
  public
    constructor Create(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit);

    procedure Commit; override;
    procedure Load; override;

  end;


  { TFieldEditors }

  TFieldEditors = class(specialize TFPGObjectList<TAbstractFieldEditor>)
  public
    constructor Create;
    procedure Commit;
    procedure Load;
  end;

implementation

{ TIntEditor }

constructor TIntEditor.Create(ATarget: TObject; const APropName: string; AWidget: TCustomSpinEdit);
begin
  FWidget := AWidget;
  inherited Create(ATarget, APropName);

end;

procedure TIntEditor.Commit;
var
  value: Int64;
begin
  value:=FWidget.Value;
  SetOrdProp(FTarget, FPropInfo, value);
end;

procedure TIntEditor.Load;
var
  value: Int64;
begin
  value := GetOrdProp(FTarget, FPropInfo);
end;

{ TOptStringFieldEditor }

procedure TOptStringFieldEditor.CheckChange(Sender: TObject);
begin
  FWidget.Enabled := FCheck.State = cbChecked;
end;

constructor TOptStringFieldEditor.Create(ATarget: TObject; const APropName: string; AWidget: TCustomEdit;
  ACheck: TCustomCheckBox);
begin
  FWidget := AWidget;
  FCheck := ACheck;
  Inherited Create(ATarget, APropName);

  FCheck.OnChange := @CheckChange;
end;

procedure TOptStringFieldEditor.Commit;
var
  value: string;
begin
  if FCheck.State in [cbChecked] then
  begin
    value := FWidget.Text;
  end
  else
  begin
    value := '';
  end;

  SetStrProp(FTarget, FPropInfo, value);
end;

procedure TOptStringFieldEditor.Load;
var
  value: string;
begin
  value := GetStrProp(FTarget, FPropInfo);

  if value = '' then
  begin
    FCheck.State:=cbUnchecked;
    FWidget.Text:='';
  end
  else
  begin
    FCheck.State:=cbChecked;
    FWidget.Text:=value;
  end;

  CheckChange(FCheck);
end;

{ TBaseFieldEditor }

constructor TBaseFieldEditor.Create(ATarget: TObject; const APropName: string);
begin
  FTarget := ATarget;
  FPropName := APropName;

  FPropInfo := GetPropInfo(ATarget, APropName);
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

end.

