unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, FileUtil, RegExpr, SysUtils;

procedure ConvertFolderStructure(srcDir: String; destDir: String);

implementation

procedure ConvertFolderStructure(srcDir: String; destDir: String);
var
  exprExperimentDir: TRegExpr;
  dirs: TStringList;
  dir: String;
begin
  // TODO: Implement processing here.
  exprExperimentDir := TRegExpr.Create;
  exprExperimentDir.Expression := 'Experiment\ .*';
  dirs := FileUtil.FindAllDirectories(srcDir, False);
  for dir in dirs do
  begin
    if exprExperimentDir.Exec(dir) then
      ShowMessage('Match: ' + dir);
  end;

  // Parse all the info needed to create destination folder structure

  // Create destination folder structure

  // Copy files from source to destination

  ShowMessage('Processing finished!');

  exprExperimentDir.Free;
end;

end.

