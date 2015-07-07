unit artifact_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, 
    base_object_options_frame, object_options;

type

  { TArtifactFrame }

  TArtifactFrame = class(TBaseObjectOptionsFrame)
    edMessage: TMemo;
    Label1: TLabel;
  private
    FOptions:TArtifactOptions;
  public
    procedure Commit; override;
    procedure VisitArtifact(AOptions: TArtifactOptions); override;
  end;

implementation

{$R *.lfm}

{ TArtifactFrame }

procedure TArtifactFrame.Commit;
begin
  inherited Commit;
  FOptions.GuardMessage:=edMessage.Text;
end;

procedure TArtifactFrame.VisitArtifact(AOptions: TArtifactOptions);
begin
  inherited VisitArtifact(AOptions);

  FOptions := AOptions;

  edMessage.Text:=AOptions.GuardMessage;
end;

end.

