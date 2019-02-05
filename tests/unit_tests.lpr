program unit_tests;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
uses
  Classes,
  fpcunit, testregistry, testutils, fpcunitreport,
  consoletestrunner, test_CSVDocunent;

type

  { TErrorCounter }

  TErrorCounter = class(TNoRefCountObject, ITestListener)
  private
    FTotalErrors: Integer;
  public
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);

    property TotalErrors: Integer read FTotalErrors;
  end;

  { TMyProgressWriter }

  TMyProgressWriter = class(TNoRefCountObject, ITestListener)
  private
    FSuccess: boolean;
    procedure WriteChar(c: char);
  public
    destructor Destroy; override;

    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  end;

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  strict private
    FErrorCounter: TErrorCounter;
  protected
    procedure DoRun; override;
    procedure DoTestRun(ATest: TTest); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Application: TMyTestRunner;

procedure TErrorCounter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  Inc(FTotalErrors);
end;

procedure TErrorCounter.AddError(ATest: TTest; AError: TTestFailure);
begin
  Inc(FTotalErrors);
end;

procedure TErrorCounter.StartTest(ATest: TTest);
begin
  // do nothing
end;

procedure TErrorCounter.EndTest(ATest: TTest);
begin
  // do nothing
end;

procedure TErrorCounter.StartTestSuite(ATestSuite: TTestSuite);
begin
   // do nothing
end;

procedure TErrorCounter.EndTestSuite(ATestSuite: TTestSuite);
begin
   // do nothing
end;

{ TMyProgressWriter }

procedure TMyProgressWriter.WriteChar(c: char);
begin
  write(c);
  // flush output, so that we see the char immediately, even it is written to file
  Flush(output);
end;

destructor TMyProgressWriter.Destroy;
begin
  // on descruction, just write the missing line ending
  writeln;
  inherited Destroy;
end;

procedure TMyProgressWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  FSuccess := false;
  writechar('F');
end;

procedure TMyProgressWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  FSuccess := false;
  writechar('E');
end;

procedure TMyProgressWriter.StartTest(ATest: TTest);
begin
  FSuccess := true; // assume success, until proven otherwise
end;

procedure TMyProgressWriter.EndTest(ATest: TTest);
begin
  if FSuccess then
    writechar('.');
end;

procedure TMyProgressWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
end;

procedure TMyProgressWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
end;

{ TMyTestRunner }

procedure TMyTestRunner.DoRun;
begin
  inherited DoRun;
  ExitCode:=FErrorCounter.TotalErrors;
end;

procedure TMyTestRunner.DoTestRun(ATest: TTest);
var
  ResultsWriter: TCustomResultsWriter;
  ProgressWriter: TMyProgressWriter;
  TestResult: TTestResult;
begin
  ResultsWriter := GetResultsWriter;
  ResultsWriter.Filename := FileName;
  TestResult := TTestResult.Create;
  try
    if ShowProgress then
    begin
      ProgressWriter := TMyProgressWriter.Create;
      TestResult.AddListener(ProgressWriter);
    end
    else
      ProgressWriter := nil;
    TestResult.AddListener(ResultsWriter);
    TestResult.AddListener(FErrorCounter);
    ATest.Run(TestResult);
    ResultsWriter.WriteResult(TestResult);
  finally
    TestResult.Free;
    ResultsWriter.Free;
    ProgressWriter.Free;
  end;
end;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FErrorCounter := TErrorCounter.Create;
end;

destructor TMyTestRunner.Destroy;
begin
  FErrorCounter.Free;
  inherited Destroy;
end;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
