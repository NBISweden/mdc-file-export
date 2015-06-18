unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, FileUtil, Forms, RegExpr, SysUtils, StringObject;

//procedure ConvertFolderStructure(srcDir: String; destDir: String);
procedure ConvertFolderStructure(plateDirs: TStringList; destDir: String; logStringList: TStrings; app: TApplication);
function FormatDestImageName(imgFilePath: String): String;
procedure CopyImage(imgSrcPath: String; imgDestPath: String; logStringList: TStrings; app: TApplication);

implementation

// =============================================================================

procedure ConvertFolderStructure(plateDirs: TStringList; destDir: String; logStringList: TStrings; app: TApplication);
var
  //plateDirs: TStringList;
  plateDirName: String;
  i: integer;
  plateDirObj: TString;
  plateDirPath: String;
  plateNo: String;
  plateDirDateDirPath: String;
  plateDirDate: String;
  plateDirExperimentDirPath: String;
  plateDirExperiment: String;

  imgFilePaths: TStringList;
  imgFilePath: String;

  // Time point folder stuff
  timeptDirName: String;
  timeptDirPath: String;
  timeptDirPaths: TStringList;

  // Destination stuff
  destPlateFolderName: String;
  destPlateFolderPath: String;
  imgDestName: String;
  imgDestPath: String;

  destTimeptDirPath: String;

const
  ImagePathPatterns = '*.tif; *.TIF; *.tiff; *.TIFF';

begin
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
    logStringList.Add('Plate dir:  ' + plateDirName);
    logStringList.Add('Date:       ' + plateDirDate);

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


    // Loop over image files in plate directory
    imgFilePaths := TStringList.Create;
    imgFilePaths := FileUtil.FindAllFiles(plateDirPath, ImagePathPatterns, false);

    // TODO: Remember to check for possible "timepoint" folders here
    if not (imgFilePaths.Count = 0) then
    begin
      // ----------------------------------------------------------------------
      // In case TimePoint folders do NOT exist
      // ----------------------------------------------------------------------
      for imgFilePath in imgFilePaths do
      begin
        imgDestName := FormatDestImageName(imgFilePath);
        imgDestPath := destPlateFolderPath + PathDelim + imgDestName;
        try
          CopyImage(imgFilePath, imgDestPath, logStringList, app);
        except
          on E: EFCreateError do
            ShowMessage('An error occured:' + LineEnding + E.Message);
        end;
      end;
    end
    else
    begin
      // ----------------------------------------------------------------------
      // In case TimePoint folders DO exist
      // ----------------------------------------------------------------------
      logStringList.Add('No *.tif images in plate dir, so assuming to contain timepoints: ' + LineEnding +
                        plateDirPath);

      timeptDirPaths := TStringList.Create;

      timeptDirPaths := FileUtil.FindAllDirectories(plateDirPath, false);
      for timeptDirPath in timeptDirPaths do
      begin
        imgFilePaths.Clear;

        timeptDirName := FileUtil.ExtractFileNameOnly(timeptDirPath);
        logStringList.Add('--------------------------------------------------');
        logStringList.Add('Now processing: ' + timeptDirName);

        // Create timepoint folder in dest folder ...
        destTimeptDirPath := destPlateFolderPath + PathDelim + timeptDirName;
        try
          SysUtils.CreateDir(destTimeptDirPath);
        //except ...
        finally
        end;

        imgFilePaths := FileUtil.FindAllFiles(timeptDirPath, ImagePathPatterns, false);
        for imgFilePath in imgFilePaths do
        begin
          imgDestName := FormatDestImageName(imgFilePath);
          imgDestPath := destTimeptDirPath + PathDelim + imgDestName;
          CopyImage(imgFilePath, imgDestPath, logStringList, app);
        end;
      end;

      timeptDirPaths.Free;
      // ----------------------------------------------------------------------
    end;

    imgFilePaths.Free;
  end;

  // TODO: Provide better assertions that things are following the correct
  //       structure

  logStringList.Add('--------------------------------------------------');
  logStringList.Add('Processing finished!');
  plateDirs.Free;
end;

// =============================================================================

function FormatDestImageName(imgFilePath: String): String;
var
  imgFileNameExpr: TRegExpr;
  imgFileName: String;

  imgInfoBaseName: String;
  imgInfoWell: String;
  imgInfoSite: String;
  imgInfoWaveLength: String;

  imgDestName: String;

begin
  imgFileNameExpr := TRegExpr.Create;
  imgFileNameExpr.Expression := '([^_]+)_([^_]+)_(s[^_]+)_((w[0-9]))?.*';
  imgFileNameExpr.Compile;

  imgFileName := FileUtil.ExtractFileNameOnly(imgFilePath);
  imgFileNameExpr.Exec(imgFileName);
  imgInfoBaseName := imgFileNameExpr.Match[1];
  imgInfoWell := imgFileNameExpr.Match[2];
  imgInfoSite := imgFileNameExpr.Match[3];
  imgInfoWaveLength := imgFileNameExpr.Match[4];

  // Construct destination path
  imgDestName := imgInfoBaseName + '_' +
                 imginfoWell + '_' +
                 imgInfoSite + '_' +
                 imgInfoWaveLength + '.tif';
  Result := imgDestName;
end;

// =============================================================================

procedure CopyImage(imgSrcPath: String; imgDestPath: String; logStringList: TStrings; app: TApplication);
begin
  // Log paths to be copied
  logStringList.BeginUpdate;
  logStringList.Add('----------------------------------------------------------------------');
  logStringList.Add('Trying to copy file:' + LineEnding +
              'from: ' + imgSrcPath + LineEnding +
              '->to: ' + imgDestPath);
  app.ProcessMessages;

  // Do the actual copy
  if FileUtil.CopyFile(imgSrcPath, imgDestPath) then
     logStringList.Add('Copy successful!');
  logStringList.EndUpdate;

  // Make sure the UI doesn't freeze
  app.ProcessMessages;
end;

end.
