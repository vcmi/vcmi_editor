unit grail_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, object_options, base_object_options_frame;

type

  { TGrailFrame }

  TGrailFrame = class(TBaseObjectOptionsFrame)
    lbRadius: TLabel;
    edRadius: TSpinEdit;
  strict private
    FObject: TGrailOptions;
  public
    procedure Commit; override;
    procedure VisitGrail(AOptions: TGrailOptions); override;
  end;

implementation

{$R *.lfm}

{ TGrailFrame }

procedure TGrailFrame.Commit;
begin
  inherited Commit;
  FObject.Radius := edRadius.Value;
end;

procedure TGrailFrame.VisitGrail(AOptions: TGrailOptions);
begin
  inherited VisitGrail(AOptions);
  FObject := AOptions;
  edRadius.Value := AOptions.Radius;
end;

end.

