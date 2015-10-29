unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, FileUtil, Forms, RegExpr, SysUtils, StringObject;

//procedure ConvertFolderStructure(srcDir: String; destDir: String);
procedure ConvertFolderStructure(plateDirs: TStringList; destDir: String; logStringList: TStrings; app: TApplication);
function FormatDestImageName(imgFilePath: String): String;
procedure CopyImage(imgSrcPath: String; imgDestPath: String; logStringList: TStrings; app: TApplication);
procedure Log(LogMsg: String; logStrings: TStrings);

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

  Msg: String;

const
  ImagePathPatterns = '*.tif; *.TIF; *.tiff; *.TIFF';
  LogSep = '------------------------------------------------------------------------';

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

    Msg := 'Now processing:' + LineEnding +
           LogSep + LineEnding +
           'Experiment: ' + plateDirExperiment + LineEnding +
           'Plate dir:  ' + plateDirName + LineEnding +
           'Date:       ' + plateDirDate + LineEnding +
           LogSep;
    Log(Msg, logStringList);

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
      Log('Trying to create folder:', logStringList);
      Log(destPlateFolderPath, logStringList);
      try
        SysUtils.CreateDir(destPlateFolderPath);
      except
        on E: Exception do
        begin
          Log('Error on trying to create folder ' + destPlateFolderPath, logStringList);
          Log(E.Message, logStringList);
          ShowMessage(E.Message);
        end;
      end;
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
          on E: Exception do
            ShowMessage('An error occured:' + LineEnding + E.Message);
        end;
      end;
    end
    else
    begin
      // ----------------------------------------------------------------------
      // In case TimePoint folders DO exist
      // ----------------------------------------------------------------------
      Log('No *.tif images in plate dir, so assuming to contain timepoints: ' + LineEnding +
                        plateDirPath, logStringList);

      timeptDirPaths := TStringList.Create;

      timeptDirPaths := FileUtil.FindAllDirectories(plateDirPath, false);
      if timeptDirPaths.Count = 0 then
      begin
        Msg := 'Directory contains nether *.tif files, nor time point ' +
               'directories, so skipping!';
        Log(Msg, logStringList);
        ShowMessage(Msg);
        Exit;
      end;

      for timeptDirPath in timeptDirPaths do
      begin
        imgFilePaths.Clear;

        timeptDirName := FileUtil.ExtractFileNameOnly(timeptDirPath);
        Log('Now processing: ' + timeptDirName, logStringList);

        // Create timepoint folder in dest folder ...
        destTimeptDirPath := destPlateFolderPath + PathDelim + timeptDirName;
        try
          SysUtils.CreateDir(destTimeptDirPath);
        except
          on E: Exception do
          begin
            Log('Error on trying to create folder ' + destTimeptDirPath, logStringList);
            Log(E.Message, logStringList);
            ShowMessage(E.Message);
            Exit;
          end;
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

  Log('Processing finished!', logStringList);
  plateDirs.Free;
end;

// =============================================================================

function FormatDestImageName(imgFilePath: String): String;
var
  imgFileNameExpr: TRegExpr;
  imgFileName: String;

begin
  imgFileNameExpr := TRegExpr.Create;
  imgFileNameExpr.Expression := '(_)?[0-9A-Z]{7,8}-[0-9A-Z]{4}-[0-9A-Z]{4}-([0-9A-Z]{4}-)?[0-9A-Z]{10,12}';
  imgFileNameExpr.Compile;
  imgFileName := ExtractFileName(imgFilePath);
  imgFileName := imgFileNameExpr.Replace(imgFileName, '', true);
  Result := imgFileName;
end;

// =============================================================================

procedure CopyImage(imgSrcPath: String; imgDestPath: String; logStringList: TStrings; app: TApplication);
begin
  // Log paths to be copied
  logStringList.BeginUpdate;
  Log('Trying to copy file:' + LineEnding +
      'from: ' + imgSrcPath + LineEnding +
      '  to: ' + imgDestPath, logStringList);
  app.ProcessMessages;

  // Do the actual copy
  if FileUtil.CopyFile(imgSrcPath, imgDestPath) then
    Log('Copy successful!', logStringList);
  logStringList.EndUpdate;

  // Make sure the UI doesn't freeze
  app.ProcessMessages;
end;

// =============================================================================

procedure Log(LogMsg: String; logStrings: TStrings);
var
  DateStr: String;
begin
  DateStr := '[' + FormatDateTime('YYYY-MM-DD, hh:mm:ss', Now) + ']';
  logStrings.Add(DateStr + ' ' + LogMsg);
  Application.ProcessMessages;
end;

end.
