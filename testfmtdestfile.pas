unit testfmtdestfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Convert;

type

  TTestFormatDestFileName= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestFormatDestFileName.TestHookUp;
var
  RawFileName: String;
  FormattedFileName: String;
begin
  RawFileName := 'Experiment1 MOI1 D0_F02_s1_w113B355CE-FCBD-4A99-8C21-207064C44CFD.tif';
  FormattedFileName := FormatDestImageName(RawFileName);

  // Test 1
  AssertEquals('File name not formatted correctly!', FormattedFileName, 'Experiment1 MOI1 D0_F02_s1_w1.tif');

  // Test 2
  if FormattedFileName = 'Experiment1 MOI1 D0_F02_s1_w1.tif' then
     Fail('Not correctly separating between well number and hash ID!');
end;

procedure TTestFormatDestFileName.SetUp;
begin

end;

procedure TTestFormatDestFileName.TearDown;
begin

end;

initialization

  RegisterTest(TTestFormatDestFileName);
end.

