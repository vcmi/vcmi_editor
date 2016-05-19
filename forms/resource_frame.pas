unit resource_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, base_options_frame, object_options;

type

  { TResourceFrame }

  TResourceFrame = class(TBaseOptionsFrame)
    cbRandom: TCheckBox;
    GroupAmount: TGroupBox;
    edAmount: TSpinEdit;
    GroupMessage: TGroupBox;
    edMessage: TMemo;
  private
    FOptions: TResourceOptions;
  protected
  public
    procedure Commit; override;
    procedure VisitResource(AOptions: TResourceOptions); override;
  end;

implementation

{$R *.lfm}

{ TResourceFrame }


procedure TResourceFrame.Commit;
begin
  inherited Commit;
  FOptions.GuardMessage := edMessage.Text;
end;

procedure TResourceFrame.VisitResource(AOptions: TResourceOptions);
begin
  FOptions := AOptions;

  AddIntEditor(AOptions, 'Amount', edAmount, cbRandom);

  inherited VisitResource(AOptions);

  edMessage.Text:=AOptions.GuardMessage;

  Load;

  UpdateControls;
end;

end.

