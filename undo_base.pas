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
unit undo_base;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils;

type

  TUndoItemState = (Idle, UnDone, ReDone);

  { TAbstractUndoItem }

  TAbstractUndoItem = class abstract
  private
    FState: TUndoItemState;
    procedure SetState(AValue: TUndoItemState);
  protected
    property State:TUndoItemState read FState write SetState;
    function GetDescription: string; virtual; abstract;
  public
    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;

    procedure Execute; virtual; abstract;
    property Description: string read GetDescription;
  end;

  { TAbstractUndoManager }

  TAbstractUndoManager = class abstract
  protected
    class procedure SetItemState(AItem: TAbstractUndoItem; AState: TUndoItemState); static;
  public
    //Last executed item. Will be undone first
    function PeekCurrent: TAbstractUndoItem; virtual; abstract;
    //Last undone item. Will be redone next if any
    function PeekNext: TAbstractUndoItem; virtual; abstract;

    //current present
    function CanUndo: boolean; virtual; abstract;
    //next present
    function CanRedo: boolean; virtual; abstract;

    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;

    //redo action (= first execute), push to stack, acquire ownership
    procedure ExecuteItem(AItem: TAbstractUndoItem); virtual; abstract;

    procedure Clear; virtual; abstract;
  end;

implementation

{ TAbstractUndoManager }

class procedure TAbstractUndoManager.SetItemState(AItem: TAbstractUndoItem;
  AState: TUndoItemState);
begin
  AItem.State:=AState;
end;


{ TAbstractUndoItem }

procedure TAbstractUndoItem.SetState(AValue: TUndoItemState);
begin
  if FState=AValue then Exit;
  FState:=AValue;
end;

end.

