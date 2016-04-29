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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ActnList, Spin, Buttons,
  base_options_frame, gui_helpers, object_options, base_info, editor_classes, editor_types;

type

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
    procedure RewardsEditSelectionChange(Sender: TObject; User: boolean);
  private
    FObject: TRewards;
  protected
    procedure Load; override;
  public
    procedure Commit; override;
    procedure VisitLocalEvent(AOptions: TLocalEventOptions); override;
    procedure VisitPandorasBox(AOptions: TPandorasOptions); override;
    procedure VisitSeerHut(AOptions: TSeerHutOptions); override;
  end;

implementation

{$R *.lfm}

{ TRewardFrame }

procedure TRewardFrame.RewardsEditSelectionChange(Sender: TObject; User: boolean);
begin
  //
end;

procedure TRewardFrame.actAddExecute(Sender: TObject);
begin
  //
end;

procedure TRewardFrame.actAddUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (MetaTypeEdit.ItemIndex >= 0) and (TypeEdit.ItemIndex >= 0) and (AmountEdit.Value <> 0);
end;

procedure TRewardFrame.actDeleteExecute(Sender: TObject);
begin
  //
end;

procedure TRewardFrame.actDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=RewardsEdit.ItemIndex >= 0;
end;

procedure TRewardFrame.Load;
begin
  inherited Load;
end;

procedure TRewardFrame.Commit;
begin
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
end;

end.

