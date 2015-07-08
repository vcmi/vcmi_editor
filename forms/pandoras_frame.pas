unit pandoras_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, 
    base_object_options_frame, object_options;

type

  { TPandorasFrame }

  TPandorasFrame = class(TBaseObjectOptionsFrame)
    edMessage: TMemo;
    Label1: TLabel;
  private
    FOptions: TPandorasOptions;
  public
    procedure VisitPandorasBox(AOptions: TPandorasOptions); override;
    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ TPandorasFrame }

procedure TPandorasFrame.VisitPandorasBox(AOptions: TPandorasOptions);
begin
  inherited VisitPandorasBox(AOptions);

  FOptions := AOptions;

  edMessage.Text:=AOptions.GuardMessage;
end;

procedure TPandorasFrame.Commit;
begin
  inherited Commit;
  FOptions.GuardMessage:=edMessage.Text;
end;

end.

