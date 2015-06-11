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
  plateDirName: String;
  i: integer;
  j: integer;
  plateDirObj: TString;
  plateDirPath: String;
  imgFileNameExpr: TRegExpr;
  imgFilePaths: TStringList;
  imgFilePath: String;
  imgFileName: String;

begin
  imgFileNameExpr := TRegExpr.Create;
  imgFileNameExpr.Expression := '(.+)_(.+)_.*';
  imgFileNameExpr.Compile;

  for i := 0 to plateDirs.Count-1 do
  begin
    plateDirName := plateDirs.Strings[i];
    plateDirObj := TString(plateDirs.Objects[i]);
    plateDirPath := plateDirObj.Text;
    ShowMessage('Plate dir: ' + plateDirName + LineEnding + 'With directory:' + LineEnding + plateDirPath);
    // Loop over image files in plate directory
    // TODO: Remember to check for possible "timepoint" folders here
    imgFilePaths := TStringList.Create;
    imgFilePaths := FileUtil.FindAllFiles(plateDirPath);
    for imgFilePath in imgFilePaths do
    begin
      imgFileName := FileUtil.ExtractFileNameOnly(imgFilePath);
      imgFileNameExpr.Exec(imgFileName);
      ShowMessage('Image file: ' + LineEnding + imgFilePath);
      for j := 0 to imgFileNameExpr.SubExprMatchCount-1 do
      begin
        ShowMessage('Match ' + IntToStr(j) + ': ' + imgFileNameExpr.Match[j]);
      end;
    end;
    imgFilePaths.Free;
  end;

  //for experimentDir in experimentDirs do
  //begin
  //  if exprExperimentDir.Exec(experimentDir) then
  //    ShowMessage('Match: ' + experimentDir);
  //end;

  // STEP: Parse image file names for additional info not avilable already


  // STEP: Create destination folder structure

  // STEP: Copy files from source to destination

  ShowMessage('Processing finished!');
  plateDirs.Free;
end;

end.

