program TestMDCFileExport;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testfmtdestfile;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

