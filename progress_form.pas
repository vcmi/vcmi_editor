unit progress_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, editor_classes;

type

  { TProgressForm }

  TProgressForm = class(TForm,IProgressCallback)
    lbOverall: TLabel;
    lbDetail: TLabel;
    pbDetail: TProgressBar;
    pbOverall: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private
    FStageCount: Integer;
    procedure SetStageCount(AValue: Integer);
    { private declarations }
  public
    property StageCount: Integer read FStageCount write SetStageCount;

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
  pbOverall.Position := pbOverall.Position+1;
  lbOverall.Caption := AStageLabel;

  pbDetail.Position := pbDetail.Min;
  lbDetail.Caption := '';

  Application.ProcessMessages;
end;

procedure TProgressForm.Reset;
begin
  pbOverall.Min := 0;

  pbOverall.Position := pbOverall.Min;
  lbOverall.Caption := '';
  pbDetail.Position := pbDetail.Min;
  lbDetail.Caption := '';
end;

procedure TProgressForm.SetMax(AValue: Integer);
begin
  pbDetail.Max := AValue;
end;

procedure TProgressForm.SetStageCount(AValue: Integer);
begin
  if FStageCount = AValue then Exit;
  FStageCount := AValue;
  pbOverall.Max := FStageCount;
end;

end.

