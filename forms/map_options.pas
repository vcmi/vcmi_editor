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
    procedure btOkClick(Sender: TObject);
  private
    FMap: TVCMIMap;
    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;
  public
    { public declarations }
    property Map: TVCMIMap read FMap write SetMap;
  end;


implementation

{$R *.lfm}

{ TMapOptionsForm }

procedure TMapOptionsForm.btOkClick(Sender: TObject);
begin
  //todo: validate
  //todo: save

  FMap.Name := edName.Text;
  FMap.Description := edDescription.Text;

  ModalResult := mrOK;
  Close;
end;

procedure TMapOptionsForm.ReadData;
begin
  edName.Text := FMap.Name;
  edDescription.Text := FMap.Description;
end;

procedure TMapOptionsForm.SetMap(AValue: TVCMIMap);
begin
  if FMap = AValue then Exit;
  FMap := AValue;
  if Assigned( FMap) then ReadData;
end;



end.

