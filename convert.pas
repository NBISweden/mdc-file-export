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

  imgInfoBaseName: String;
  imgInfoWell: String;
  imgInfoWellItem: String;
  imgInfoWaveLength: String;
  imgInfoIsThumb: Boolean;

begin
  imgFileNameExpr := TRegExpr.Create;
  imgFileNameExpr.Expression := '([^_]+)_([^_]+)_(s[^_]+)_((w[^_]+))?_(thumb)?.*';
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
      imgInfoBaseName := imgFileNameExpr.Match[1];
      imgInfoWell := imgFileNameExpr.Match[2];
      imgInfoWellItem := imgFileNameExpr.Match[3];
      imgInfoWaveLength := imgFileNameExpr.Match[4];
      // imgInfoIsThumb := imgFileNameExpr[1];
      ShowMessage('BaseName: '   + imgInfoBaseName + LineEnding +
                  'Well: '       + imgInfoWell + LineEnding +
                  'WellItem: '   + imgInfoWellItem + LineEnding +
                  'WaveLength: ' + imgInfoWaveLength);
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

