unit progress_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, editor_classes;

type

  { TProgressForm }

  TProgressForm = class(TForm,IProgressCallback)
    CloseButton: TButton;
    lbDetail: TLabel;
    Errors: TMemo;
    pbDetail: TProgressBar;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FCompleted: Boolean;
    FHasErrors: boolean;
    procedure SetCompleted(AValue: Boolean);
    { private declarations }
  public
    property HasErrors: boolean read FHasErrors;
    property Completed: Boolean read FCompleted write SetCompleted;
    procedure Reset;
  public
    //IProgressCallback

    function GetMax: Integer;
    procedure SetMax(AValue: Integer);
    procedure Advance(ADelta: integer);
    procedure NextStage(const AStageLabel:string);
    procedure AddError(const ADescription: string);
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
  Height:=75;
  Errors.Visible:=false;
end;

procedure TProgressForm.SetCompleted(AValue: Boolean);
begin
  FCompleted:=AValue;

  CloseButton.Enabled := FCompleted;
end;

procedure TProgressForm.CloseButtonClick(Sender: TObject);
begin
  Close
end;

procedure TProgressForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide; //???
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

procedure TProgressForm.AddError(const ADescription: string);
begin
  Height:=320;
  Errors.Visible := true;

  Errors.Append(ADescription);
  FHasErrors:=True;
end;

procedure TProgressForm.Reset;
begin
  pbDetail.Position := pbDetail.Min;
  lbDetail.Caption := '';
  FCompleted:=false;
end;

procedure TProgressForm.SetMax(AValue: Integer);
begin
  pbDetail.Max := AValue;
  pbDetail.Position := pbDetail.Min;
  Application.ProcessMessages;
end;

end.

