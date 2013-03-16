{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge,net

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
unit edit_object_options;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, object_options, map, base_object_options_frame;

type

  { TEditObjectOptions }

  TEditObjectOptions = class(TForm, IObjectOptionsVisitor)
    btOk: TButton;
    btCancel: TButton;
    lbNothingToEdit: TLabel;
    pcMain: TPageControl;
    tsWog: TTabSheet;
    tsCommon: TTabSheet;
    procedure btOkClick(Sender: TObject);
  strict private
    //FCurrentObject: TMapObject;

    FActiveEditors: TObjectOptionsFrameList;

    procedure HideAllTabs;

    procedure CreateFrame (AClass: TBaseObjectOptionsFrameClass;
      AOptions: TObjectOptions; AParent:TWinControl);

    procedure SaveChanges;
  strict private //IObjectOptionsVisitor
    procedure VisitLocalEvent(AOptions: TLocalEvenOptions);
    procedure VisitSignBottle(AOptions: TSignBottleOptions); //+
    procedure VisitHero(AOptions: THeroOptions);
    procedure VisitMonster(AOptions: TMonsterOptions);
    procedure VisitSeerHut(AOptions: TSeerHutOptions);
    procedure VisitWitchHut(AOptions: TWitchHutOptions);//+
    procedure VisitScholar(AOptions: TScholarOptions);
    procedure VisitGarrison(AOptions: TGarrisonOptions);
    procedure VisitArtifact(AOptions: TArtifactOptions);
    procedure VisitSpellScroll(AOptions: TSpellScrollOptions);
    procedure VisitResource(AOptions: TResourceOptions);
    procedure VisitTown(AOptions: TTownOptions);
    procedure VisitShrine(AOptions: TShrineOptions);
    procedure VisitPandorasBox(AOptions: TPandorasOptions);
    procedure VisitGrail(AOptions: TGrailOptions); //+
    procedure VisitRandomDwelling(AOptions: TRandomDwellingOptions);
    procedure VisitRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions);
    procedure VisitRandomDwellingTown(AOptions: TRandomDwellingTownOptions);
    procedure VisitQuestGuard(AOptions:TQuestGuardOptions);
    procedure VisitHeroPlaseholder(AOptions: THeroPlaceholderOptions);

    procedure VisitOwnedObject(AOptions: TOwnedObjectOptions);//+

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditObject(Obj: TMapObject);

  end;


implementation

uses
  signbottle_frame, grail_frame, flaggable_object_frame, witchhut_frame,
  root_manager;

{$R *.lfm}

{ TEditObjectOptions }

procedure TEditObjectOptions.btOkClick(Sender: TObject);
begin
  SaveChanges;
end;

constructor TEditObjectOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FActiveEditors := TObjectOptionsFrameList.Create;
end;

procedure TEditObjectOptions.CreateFrame(AClass: TBaseObjectOptionsFrameClass;
  AOptions: TObjectOptions; AParent: TWinControl);
var
  frame: TBaseObjectOptionsFrame;
begin
  frame := AClass.Create(Self);
  frame.Parent := AParent;
  frame.Align := alClient;
  frame.ListsManager := RootManager.ListsManager;
  AOptions.ApplyVisitor(frame); //do AFTER assign properties
  FActiveEditors.PushBack(frame);
end;

destructor TEditObjectOptions.Destroy;
begin
  FActiveEditors.Free;
  inherited Destroy;
end;

procedure TEditObjectOptions.EditObject(Obj: TMapObject);
begin
  FActiveEditors.Clear;
  HideAllTabs;
  Obj.Options.ApplyVisitor(Self);
  ShowModal;
end;

procedure TEditObjectOptions.HideAllTabs;
var
  i: Integer;
begin
  for i := 0 to pcMain.PageCount - 1 do
  begin
    pcMain.Pages[i].TabVisible := False;
  end;

  tsCommon.TabVisible := True;
  pcMain.ActivePage := tsCommon;
end;

procedure TEditObjectOptions.SaveChanges;
begin
  FActiveEditors.Commit;
  //TODO: free all editors and use cashed form
end;

procedure TEditObjectOptions.VisitArtifact(AOptions: TArtifactOptions);
begin

end;

procedure TEditObjectOptions.VisitGarrison(AOptions: TGarrisonOptions);
begin

end;

procedure TEditObjectOptions.VisitGrail(AOptions: TGrailOptions);
begin
  CreateFrame(TGrailFrame,AOptions,tsCommon);
end;

procedure TEditObjectOptions.VisitHero(AOptions: THeroOptions);
begin

end;

procedure TEditObjectOptions.VisitHeroPlaseholder(
  AOptions: THeroPlaceholderOptions);
begin

end;

procedure TEditObjectOptions.VisitLocalEvent(AOptions: TLocalEvenOptions);
begin

end;

procedure TEditObjectOptions.VisitMonster(AOptions: TMonsterOptions);
begin

end;

procedure TEditObjectOptions.VisitOwnedObject(AOptions: TOwnedObjectOptions);
begin
  CreateFrame(TFlaggableFrame,AOptions,tsCommon);
end;

procedure TEditObjectOptions.VisitPandorasBox(AOptions: TPandorasOptions);
begin

end;

procedure TEditObjectOptions.VisitQuestGuard(AOptions: TQuestGuardOptions);
begin

end;

procedure TEditObjectOptions.VisitRandomDwelling(
  AOptions: TRandomDwellingOptions);
begin

end;

procedure TEditObjectOptions.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin

end;

procedure TEditObjectOptions.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin

end;

procedure TEditObjectOptions.VisitResource(AOptions: TResourceOptions);
begin

end;

procedure TEditObjectOptions.VisitScholar(AOptions: TScholarOptions);
begin

end;

procedure TEditObjectOptions.VisitSeerHut(AOptions: TSeerHutOptions);
begin

end;

procedure TEditObjectOptions.VisitShrine(AOptions: TShrineOptions);
begin

end;

procedure TEditObjectOptions.VisitSignBottle(AOptions: TSignBottleOptions);
begin
  CreateFrame(TSignBottleFrame,AOptions,tsCommon);
end;

procedure TEditObjectOptions.VisitSpellScroll(AOptions: TSpellScrollOptions);
begin

end;

procedure TEditObjectOptions.VisitTown(AOptions: TTownOptions);
begin

end;

procedure TEditObjectOptions.VisitWitchHut(AOptions: TWitchHutOptions);
begin
  CreateFrame(TWitchHutFrame,AOptions,tsCommon);
end;

end.

