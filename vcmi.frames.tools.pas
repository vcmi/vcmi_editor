unit vcmi.frames.tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ActnList;

type

  { TToolsFrame }

  TToolsFrame = class(TFrame)
    act: TActionList;
    ToolsPages: TPageControl;
    tsRivers: TTabSheet;
    tsRoads: TTabSheet;
    tsTerrain: TTabSheet;
    tsObjects: TTabSheet;
    procedure ToolsPagesChange(Sender: TObject);
    procedure ToolsPagesChanging(Sender: TObject; var AllowChange: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TToolsFrame }

procedure TToolsFrame.ToolsPagesChange(Sender: TObject);
begin
  //todo: select active brush
end;

procedure TToolsFrame.ToolsPagesChanging(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange := true;
  //TODO: clear active brush
end;

end.

