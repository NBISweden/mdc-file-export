unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Dialogs, FileUtil, Forms, RegExpr, StrUtils,
  StdCtrls, SysUtils, StringObject;

//procedure ConvertFolderStructure(srcDir: String; destDir: String);
procedure ConvertFolderStructure(experimentDirPath: string;
  plateDirs: TStringList; destDir: string; logStringList: TStrings;
  progressBar: TProgressBar; chkAborted: TCheckBox; app: TApplication);
function FormatDestImageName(imgFilePath: string): string;
procedure CopyImage(imgSrcPath: string; imgDestPath: string;
  logStringList: TStrings; app: TApplication);
procedure Log(LogMsg: string; logStrings: TStrings);

implementation

// =============================================================================

type
  TFromToPathPair = record
    fromPath: string;
    toPath: string;
  end;

// =============================================================================

procedure ConvertFolderStructure(experimentDirPath: string;
  plateDirs: TStringList; destDir: string; logStringList: TStrings;
  progressBar: TProgressBar; chkAborted: TCheckBox; app: TApplication);
var
  //plateDirs: TStringList;
  plateDirName: string;
  plateDirIdx: integer;
  plateDirObj: TString;
  plateDirPath: string;
  plateNo: string;
  plateDirDateDirPath: string;
  plateDirDate: string;
  plateDirParentDirPath: string;
  plateDirBarCode: string;
  plateDirExperiment: string;
  imgFilePaths: TStringList;
  imgFilePath: string;
  // Time point folder stuff
  timeptDirName: string;
  timeptDirPath: string;
  timeptDirPaths: TStringList;
  // Destination stuff
  destPlateFolderName: string;
  destPlateFolderPath: string;
  imgDestName: string;
  imgDestPath: string;
  destTimeptDirPath: string;
  Msg: string;

  pathPairPtr: ^TFromToPathPair;
  imagesToCopy: TList;

  imgFromPath: string;
  imgToPath: string;

const
  ImagePathPatterns = '*.tif; *.TIF; *.tiff; *.TIFF';
  LogSep = '------------------------------------------------------------------------';

begin
  imagesToCopy := TList.Create;

  for plateDirIdx := 0 to plateDirs.Count - 1 do
  begin
    // Extract plate directory info
    plateDirObj := TString(plateDirs.Objects[plateDirIdx]);
    plateDirPath := plateDirObj.Text;
    plateDirName := FileUtil.ExtractFileNameOnly(plateDirPath);

    // Extract Date from the Plate Dir's parent directory
    plateDirDateDirPath := SysUtils.ExtractFilePath(plateDirPath);
    plateNo := FileUtil.ExtractFileNameOnly(plateDirPath);
    plateDirDate := FileUtil.ExtractFileNameOnly(
      FileUtil.ChompPathDelim(plateDirDateDirPath));
    plateDirParentDirPath := SysUtils.ExtractFilePath(
      FileUtil.ChompPathDelim(plateDirDateDirPath));

    // *** BEGIN: DEBUG CODE ***
    //Msg := LogSep + LineEnding +
    //       'plateDirParentDirPath: ' + plateDirParentDirPath + LineEnding +
    //       'experimentDirPath: ' + experimentDirPath + LineEnding +
    //       LogSep + LineEnding;
    //Log(Msg, logStringList);
    // *** END: DEBUG CODE ***

    // Check if the parent folder of the plate dir is the experiment folder
    if (FileUtil.ChompPathDelim(plateDirParentDirPath) =
      FileUtil.ChompPathDelim(experimentDirPath)) then
    begin
      plateDirExperiment := FileUtil.ExtractFileNameOnly(
        FileUtil.ChompPathDelim(plateDirParentDirPath));
    end
    else
    begin
      plateDirBarCode := FileUtil.ExtractFileNameOnly(
        FileUtil.ChompPathDelim(plateDirParentDirPath));
      plateDirExperiment := FileUtil.ExtractFileNameOnly(
        FileUtil.ChompPathDelim(SysUtils.ExtractFilePath(
        FileUtil.ChompPathDelim(plateDirParentDirPath))));
    end;


    if (plateDirBarCode <> '') then
    begin
      Msg := 'Now processing:' + LineEnding + LogSep + LineEnding +
        'Experiment: ' + plateDirExperiment + LineEnding + 'Bar code:   ' +
        plateDirBarCode + LineEnding + 'Plate dir:  ' + plateDirName +
        LineEnding + 'Date:       ' + plateDirDate + LineEnding + LogSep;
    end
    else
    begin
      Msg := 'Now processing:' + LineEnding + LogSep + LineEnding +
        'Experiment: ' + plateDirExperiment + LineEnding + 'Plate dir:  ' +
        plateDirName + LineEnding + 'Date:       ' + plateDirDate + LineEnding + LogSep;
    end;
    Log(Msg, logStringList);

    // ------------------------------------------------------------------------
    // Create folder
    // ------------------------------------------------------------------------

    if (plateDirBarCode <> '') then
    begin
      destPlateFolderName := StringReplace(plateDirExperiment, ' ',
        '_', [rfReplaceAll]) + '.barcode_' + StringReplace(
        plateDirBarCode, ' ', '_', [rfReplaceAll]) + '.plate_' +
        plateNo + '.' + plateDirDate;
    end
    else
    begin
      destPlateFolderName := StringReplace(plateDirExperiment, ' ',
        '_', [rfReplaceAll]) + '.plate_' + plateNo + '.' + plateDirDate;

    end;

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
    imgFilePaths := FileUtil.FindAllFiles(plateDirPath, ImagePathPatterns, False);

    // Handle case when plate folder contains timepoint folders
    if (imgFilePaths.Count > 0) then
    begin
      // ----------------------------------------------------------------------
      // In case TimePoint folders do NOT exist
      // ----------------------------------------------------------------------
      for imgFilePath in imgFilePaths do
      begin
        imgDestName := FormatDestImageName(imgFilePath);
        imgDestPath := destPlateFolderPath + PathDelim + imgDestName;

        // TODO: Make the below into a function
        new(pathPairPtr);
        pathPairPtr^.fromPath := imgFilePath;
        pathPairPtr^.toPath := imgDestPath;
        imagesToCopy.Add(pathPairPtr);
      end;
    end
    else
    begin
      // ----------------------------------------------------------------------
      // In case TimePoint folders DO exist
      // ----------------------------------------------------------------------
      Log('No *.tif images in plate dir, so assuming to contain timepoints: ' +
        LineEnding + plateDirPath, logStringList);

      timeptDirPaths := TStringList.Create;

      timeptDirPaths := FileUtil.FindAllDirectories(plateDirPath, False);
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

        // Add image paths from time point dir
        imgFilePaths.AddStrings(FileUtil.FindAllFiles(timeptDirPath,
          ImagePathPatterns, False));

        for imgFilePath in imgFilePaths do
        begin
          imgDestName := FormatDestImageName(imgFilePath);
          imgDestPath := destTimeptDirPath + PathDelim + imgDestName;

          // TODO: Make the below into a function
          new(pathPairPtr);
          pathPairPtr^.fromPath := imgFilePath;
          pathPairPtr^.toPath := imgDestPath;
          imagesToCopy.Add(pathPairPtr);
        end;
      end;
      timeptDirPaths.Free;
    end;

    imgFilePaths.Free;
  end;

  // Initialize progressbar
  progressBar.Min := 0;
  progressBar.Max := imagesToCopy.Count;
  progressBar.Step := 1;
  progressBar.Position := 0;

  // Copy images
  for pathPairPtr in imagesToCopy do
  begin
    imgFromPath := pathPairPtr^.fromPath;
    imgToPath := pathPairPtr^.toPath;

    if (AnsiContainsStr(imgFromPath, 'thumb')) then // Don't include thumbnails
    begin
      Log('Skipping thumbnail: ' + imgFromPath, logStringList);
    end
    else
    begin
      try
        CopyImage(imgFromPath, imgToPath, logStringList, app);
        app.ProcessMessages; // Update UI, so it doesn't freeze
        if (chkAborted.Checked) then // Abort, if abort button is pressed
        begin
          Log('*** !!! WARNING: EXPORT ABORTED !!! ***', logStringList);
          ShowMessage('Warning: Export aborted!');
          Exit;
        end;
      except
        on E: Exception do
          ShowMessage('An error occured:' + LineEnding + E.Message);
      end;
    end;
    progressBar.StepBy(1); // We need to step up both for thumbnails and normal images, to reach 100%
  end;

  // TODO: Provide better assertions that things are following the correct
  //       structure
  Log('Processing finished!', logStringList);
  plateDirs.Free;
  imagesToCopy.Free;
end;

// =============================================================================

function FormatDestImageName(imgFilePath: string): string;
var
  imgFileNameExpr: TRegExpr;
  imgFileName: string;

begin
  imgFileNameExpr := TRegExpr.Create;
  imgFileNameExpr.Expression :=
    '(_)?[0-9A-Z]{7,8}-[0-9A-Z]{4}-[0-9A-Z]{4}-([0-9A-Z]{4}-)?[0-9A-Z]{10,12}';
  imgFileNameExpr.Compile;
  imgFileName := ExtractFileName(imgFilePath);
  imgFileName := imgFileNameExpr.Replace(imgFileName, '', True);
  Result := imgFileName;
end;

// =============================================================================

procedure CopyImage(imgSrcPath: string; imgDestPath: string;
  logStringList: TStrings; app: TApplication);
begin
  // Log paths to be copied
  logStringList.BeginUpdate;
  Log('Trying to copy file:' + LineEnding + 'from: ' + imgSrcPath +
    LineEnding + '  to: ' + imgDestPath, logStringList);

  // Do the actual copy
  if FileUtil.CopyFile(imgSrcPath, imgDestPath) then
    Log('Copy successful!', logStringList);
  logStringList.EndUpdate;

  // Make sure the UI doesn't freeze
end;

// =============================================================================

procedure Log(LogMsg: string; logStrings: TStrings);
var
  DateStr: string;
begin
  DateStr := '[' + FormatDateTime('YYYY-MM-DD, hh:mm:ss', Now) + ']';
  logStrings.Add(DateStr + ' ' + LogMsg);
end;

end.
