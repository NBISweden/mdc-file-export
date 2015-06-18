unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, FileUtil, RegExpr, SysUtils, StringObject, Strings;

//procedure ConvertFolderStructure(srcDir: String; destDir: String);
procedure ConvertFolderStructure(plateDirs: TStringList; destDir: String; logStringList: TStrings);

implementation

//procedure ConvertFolderStructure(srcDir: String; destDir: String);
procedure ConvertFolderStructure(plateDirs: TStringList; destDir: String; logStringList: TStrings);
var
  //plateDirs: TStringList;
  plateDirName: String;
  i: integer;
  j: integer;
  plateDirObj: TString;
  plateDirPath: String;
  plateNo: String;
  plateDirDateDirPath: String;
  plateDirDate: String;
  plateDirExperimentDirPath: String;
  plateDirExperiment: String;

  imgFileNameExpr: TRegExpr;
  imgFilePaths: TStringList;
  imgFilePath: String;
  imgFileName: String;

  imgInfoBaseName: String;
  imgInfoWell: String;
  imgInfoSite: String;
  imgInfoWaveLength: String;
  imgInfoIsThumb: Boolean;

  // Destination stuff
  destPlateFolderName: String;
  destPlateFolderPath: String;
  imgDestPath: String;

begin
  imgFileNameExpr := TRegExpr.Create;
  imgFileNameExpr.Expression := '([^_]+)_([^_]+)_(s[^_]+)_((w[^_]+))?_(thumb)?.*';
  imgFileNameExpr.Compile;

  for i := 0 to plateDirs.Count-1 do
  begin
    // ------------------------------------------------------------------------
    // Extract info
    // ------------------------------------------------------------------------
    plateDirObj := TString(plateDirs.Objects[i]);
    plateDirPath := plateDirObj.Text;
    plateDirName := FileUtil.ExtractFileNameOnly(plateDirPath);
    // Extract Date from the Plate Dir's parent directory
    plateDirDateDirPath := SysUtils.ExtractFilePath(plateDirPath);
    plateNo := FileUtil.ExtractFileNameOnly(plateDirPath);
    plateDirDate := FileUtil.ExtractFileNameOnly(
                      FileUtil.ChompPathDelim(plateDirDateDirPath));
    plateDirExperimentDirPath := SysUtils.ExtractFilePath(
                                   FileUtil.ChompPathDelim(plateDirDateDirPath));
    plateDirExperiment := FileUtil.ExtractFileNameOnly(
                            FileUtil.ChompPathDelim(plateDirExperimentDirPath));

    logStringList.Add('--------------------------------------------------');
    logStringList.Add('Now processing:');
    logStringList.Add('Experiment: ' + plateDirExperiment);
    logStringList.Add('Plate dir: ' + plateDirName);
    logStringList.Add('Date: ' + plateDirDate);

    // ------------------------------------------------------------------------
    // Create folder
    // ------------------------------------------------------------------------

    destPlateFolderName := StringReplace(plateDirExperiment, ' ', '_', [rfReplaceAll]) +
                           '_plate_' + plateNo + '_' + plateDirDate;

    destPlateFolderPath := destDir + DirectorySeparator + destPlateFolderName;

    if not SysUtils.DirectoryExists(destPlateFolderPath) then
    begin
      // -----------------------------------------------------------------------
      // Create destination folder structure
      // -----------------------------------------------------------------------
      logStringList.Add('--------------------------------------------------');
      logStringList.Add('Trying to create folder:');
      logStringList.Add(destPlateFolderPath);
      SysUtils.CreateDir(destPlateFolderPath);
    end;

    // -------------------------------------------------------------------------
    // Loop over image files, and copy
    // -------------------------------------------------------------------------
    imgFilePaths := TStringList.Create;
    imgFilePaths := FileUtil.FindAllFiles(plateDirPath);

    // Loop over image files in plate directory
    // TODO: Remember to check for possible "timepoint" folders here
    for imgFilePath in imgFilePaths do
    begin
      imgFileName := FileUtil.ExtractFileNameOnly(imgFilePath);
      imgFileNameExpr.Exec(imgFileName);
      imgInfoBaseName := imgFileNameExpr.Match[1];
      imgInfoWell := imgFileNameExpr.Match[2];
      imgInfoSite := imgFileNameExpr.Match[3];
      imgInfoWaveLength := imgFileNameExpr.Match[4];
      // imgInfoIsThumb := imgFileNameExpr[1]; TODO: Implement
      //ShowMessage('Image info' + LineEnding +
      //            '------------------------------' + LineEnding +
      //            'BaseName: '   + imgInfoBaseName + LineEnding +
      //            'Well: '       + imgInfoWell + LineEnding +
      //            'WellItem: '   + imgInfoSite + LineEnding +
      //            'WaveLength: ' + imgInfoWaveLength);

      // -----------------------------------------------------------------------
      // Construct destination path
      // -----------------------------------------------------------------------
      imgDestPath := '';
      logStringList.Add('----------------------------------------------------------------------');
      logStringList.Add('Trying to copy file:' + LineEnding +
                  'from: ' + imgFilePath + LineEnding +
                  '->to: ' + imgDestPath);
    end;
    imgFilePaths.Free;
  end;

  // TODO: Provide better assertions that things are following the correct
  //       structure

  logStringList.Add('--------------------------------------------------');
  logStringList.Add('Processing finished!');
  plateDirs.Free;
end;

end.

