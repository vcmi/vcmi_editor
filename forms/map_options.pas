unit map_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Spin, Map;

type

  { TMapOptionsForm }

  TMapOptionsForm = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    cbEnableLevelLimit: TCheckBox;
    edName: TEdit;
    lMapName: TLabel;
    lMapDescription: TLabel;
    edDescription: TMemo;
    PageControl1: TPageControl;
    edDifficulty: TRadioGroup;
    edLevelLimit: TSpinEdit;
    tsMain: TTabSheet;
  private
    FMap: TVCMIMap;
    procedure SetMap(AValue: TVCMIMap);
    { private declarations }
  public
    { public declarations }
    property Map: TVCMIMap read FMap write SetMap;
  end;


implementation

{$R *.lfm}

{ TMapOptionsForm }

procedure TMapOptionsForm.SetMap(AValue: TVCMIMap);
begin
  if FMap = AValue then Exit;
  FMap := AValue;

end;



end.

