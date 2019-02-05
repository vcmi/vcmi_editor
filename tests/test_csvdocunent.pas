unit test_CSVDocunent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, vcmi_csvdocument, vcmi_csvreadwrite;

type

  { TTestCSVParser }

  TTestCSVParser = class(TTestCase)
  private
    FParser: TCSVParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure AssertNextCell(ret: Boolean; col, row: Integer; value: String);
  published
    procedure TestLineBreak;
    procedure TestLineBreakEmptyCells;
    procedure TestEmptyCells;
  end;

  { TTestCSVDocunent }

  TTestCSVDocunent = class(TTestCase)
  private
    FDoc: TCSVDocument;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLineBreak;
    procedure TestLineBreak2;
    procedure TestLineBreak3;
    procedure TestLineBreak4;
  end;

implementation

{ TTestCSVParser }

procedure TTestCSVParser.SetUp;
begin
  FParser := TCSVParser.Create;
  FParser.EqualColCountPerRow := False;
  FParser.IgnoreOuterWhitespace := False;
  FParser.QuoteOuterWhitespace := False;
  FParser.LineEnding := #13#10;
  FParser.QuoteChar := '"';
  FParser.Delimiter:=#9;
end;

procedure TTestCSVParser.TearDown;
begin
  FParser.Free;
end;

procedure TTestCSVParser.AssertNextCell(ret: Boolean; col, row: Integer; value: String);
begin
  AssertEquals('ParseNextCell', ret, FParser.ParseNextCell);
  AssertEquals('col', col, FParser.CurrentCol);
  AssertEquals('row', row, FParser.CurrentRow);
  AssertEquals('cell value', value, FParser.CurrentCellText);
end;

procedure TTestCSVParser.TestLineBreak;
const
  data = '1'#9'100'#13#10'2'#9'101'#13#10;
begin
  FParser.SetSource(data);
  AssertNextCell(true, 0,0,'1');
  AssertNextCell(true, 1,0,'100');
  AssertNextCell(true, 0,1,'2');
  AssertNextCell(true, 1,1,'101');
  AssertEquals('eof', false, FParser.ParseNextCell);
end;

procedure TTestCSVParser.TestLineBreakEmptyCells;
const
  data = #9#13#10#9;
begin
  FParser.SetSource(data);
  AssertNextCell(true, 0,0,'');
  AssertNextCell(true, 1,0,'');

  AssertNextCell(true, 0,1,'');
  AssertNextCell(true, 1,1,'');
end;

procedure TTestCSVParser.TestEmptyCells;
const
  data = #9#9#9;
begin
  FParser.SetSource(data);
  AssertNextCell(true, 0,0,'');
  AssertNextCell(true, 1,0,'');
  AssertNextCell(true, 2,0,'');
  AssertNextCell(true, 3,0,'');
  AssertEquals('eof', false, FParser.ParseNextCell);
end;

{ TTestCSVDocunent }

procedure TTestCSVDocunent.TestLineBreak;
const
  data = '1'#9'100'#13#10'2'#9'101'#13#10;
begin
  FDoc.CSVText:=data;

  AssertEquals('Row count', 2, FDoc.RowCount);
  AssertEquals('Col count 0', 2, FDoc.ColCount[0]);
  AssertEquals('Col count 1', 2, FDoc.ColCount[1]);
  AssertEquals('[0,0]', '1', FDoc.Cells[0,0]);
  AssertEquals('[0,1]', '100', FDoc.Cells[1,0]);
  AssertEquals('[1,0]', '2', FDoc.Cells[0,1]);
  AssertEquals('[1,1]', '101', FDoc.Cells[1,1]);
end;

procedure TTestCSVDocunent.TestLineBreak2;
const
  data = '1'#9'"10'#13#10'0"'#9#9#9#13#10'2'#9'101'#9;
begin
  FDoc.CSVText:=data;

  AssertEquals('Row count', 2, FDoc.RowCount);
  AssertEquals('Col count 0', 5, FDoc.ColCount[0]);
  AssertEquals('Col count 1', 3, FDoc.ColCount[1]);
  AssertEquals('[0,0]', '1', FDoc.Cells[0,0]);
  AssertEquals('[0,1]', '10'#13#10'0', FDoc.Cells[1,0]);
  AssertEquals('[0,2]', '', FDoc.Cells[2,0]);
  AssertEquals('[1,0]', '2', FDoc.Cells[0,1]);
  AssertEquals('[1,1]', '101', FDoc.Cells[1,1]);
  AssertEquals('[1,2]', '', FDoc.Cells[2,1]);
end;

procedure TTestCSVDocunent.TestLineBreak3;
const
  data = '"123"'#9#9#13#10'"321"';
begin
  FDoc.CSVText:=data;
  AssertEquals('Row count', 2, FDoc.RowCount);
  AssertEquals('Col count 0', 3, FDoc.ColCount[0]);
  AssertEquals('Col count 1', 1, FDoc.ColCount[1]);
  AssertEquals('[0,0]', '123', FDoc.Cells[0,0]);
  AssertEquals('[1,0]', '321', FDoc.Cells[0,1]);
end;

procedure TTestCSVDocunent.TestLineBreak4;
const
  data = '"123"'#9#9#13#10'321';
begin
  FDoc.CSVText:=data;
  AssertEquals('Row count', 2, FDoc.RowCount);
  AssertEquals('Col count 0', 3, FDoc.ColCount[0]);
  AssertEquals('Col count 1', 1, FDoc.ColCount[1]);
  AssertEquals('[0,0]', '123', FDoc.Cells[0,0]);
  AssertEquals('[1,0]', '321', FDoc.Cells[0,1]);
end;

procedure TTestCSVDocunent.SetUp;
begin
  FDoc := TCSVDocument.Create;
  FDoc.EqualColCountPerRow := False;
  FDoc.IgnoreOuterWhitespace := False;
  FDoc.QuoteOuterWhitespace := False;
  FDoc.LineEnding := #13#10;
  FDoc.QuoteChar := '"';
  FDoc.Delimiter:=#9;
end;

procedure TTestCSVDocunent.TearDown;
begin
  FDoc.Free;
end;

initialization

  RegisterTest(TTestCSVParser);
  RegisterTest(TTestCSVDocunent);
end.

