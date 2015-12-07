unit progress_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, editor_classes;

type

  { TProgressForm }

  TProgressForm = class(TForm,IProgressCallback)
    lbDetail: TLabel;
    pbDetail: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public

    procedure Reset;
    procedure NextStage(const AStageLabel:string);
  public
    //IProgressCallback

    function GetMax: Integer;
    procedure SetMax(AValue: Integer);
    procedure Advance(ADelta: integer);

  end;

implementation

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.Advance(ADelta: integer);
begin
  pbDetail.Position := pbDetail.Position+ADelta;
  Application.ProcessMessages;
end;

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  Reset;
end;

function TProgressForm.GetMax: Integer;
begin
  Result := pbDetail.Max;
end;

procedure TProgressForm.NextStage(const AStageLabel: string);
begin
  pbDetail.Position := pbDetail.Min;
  lbDetail.Caption := AStageLabel;

  Application.ProcessMessages;
end;

procedure TProgressForm.Reset;
begin
  pbDetail.Position := pbDetail.Min;
  lbDetail.Caption := '';
end;

procedure TProgressForm.SetMax(AValue: Integer);
begin
  pbDetail.Max := AValue;
end;

end.

