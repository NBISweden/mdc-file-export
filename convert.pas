unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, FileUtil, RegExpr, SysUtils, StringObject;

//procedure ConvertFolderStructure(srcDir: String; destDir: String);
procedure ConvertFolderStructure(plateDirs: TStringList; destDir: String);

implementation

//procedure ConvertFolderStructure(srcDir: String; destDir: String);
procedure ConvertFolderStructure(plateDirs: TStringList; destDir: String);
var
  //plateDirs: TStringList;
  plateDir: String;
  i: integer;
  plateDirObj: TString;
  plateDirPath: String;
begin
  for i := 0 to plateDirs.Count-1 do
  begin
    plateDir := plateDirs.Strings[i];
    plateDirObj := TString(plateDirs.Objects[i]);
    plateDirPath := plateDirObj.Text;
    ShowMessage('Plate dir: ' + plateDir + '\nWith directory:\n' + plateDirPath);
  end;

  //for experimentDir in experimentDirs do
  //begin
  //  if exprExperimentDir.Exec(experimentDir) then
  //    ShowMessage('Match: ' + experimentDir);
  //end;

  // STEP: Parse all the info needed to create destination folder structure

  // STEP: Create destination folder structure

  // STEP: Copy files from source to destination

  ShowMessage('Processing finished!');
  plateDirs.Free;
end;

end.

