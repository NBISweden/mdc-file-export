unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, FileUtil, Forms, RegExpr, StrUtils, StdCtrls, SysUtils, StringObject;

//procedure ConvertFolderStructure(srcDir: String; destDir: String);
procedure ConvertFolderStructure(experimentDirPath: string;
  plateDirs: TStringList; destDir: string; logStringList: TStrings;
  chkAborted: TCheckBox; app: TApplication);
function FormatDestImageName(imgFilePath: string): string;
procedure CopyImage(imgSrcPath: string; imgDestPath: string;
  logStringList: TStrings; app: TApplication);
procedure Log(LogMsg: string; logStrings: TStrings);

implementation

// =============================================================================

procedure ConvertFolderStructure(experimentDirPath: string;
  plateDirs: TStringList; destDir: string; logStringList: TStrings;
  chkAborted: TCheckBox; app: TApplication);
var
  //plateDirs: TStringList;
  plateDirName: string;
  i: integer;
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

const
  ImagePathPatterns = '*.tif; *.TIF; *.tiff; *.TIFF';
  LogSep = '------------------------------------------------------------------------';

begin
  for i := 0 to plateDirs.Count - 1 do
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
      Msg := 'Now processing:' + LineEnding + LogSep +
        LineEnding + 'Experiment: ' + plateDirExperiment +
        LineEnding + 'Bar code:   ' + plateDirBarCode + LineEnding +
        'Plate dir:  ' + plateDirName + LineEnding +
        'Date:       ' + plateDirDate + LineEnding + LogSep;
    end
    else
    begin
      Msg := 'Now processing:' + LineEnding + LogSep +
        LineEnding + 'Experiment: ' + plateDirExperiment +
        LineEnding + 'Plate dir:  ' + plateDirName + LineEnding +
        'Date:       ' + plateDirDate + LineEnding + LogSep;
    end;
    Log(Msg, logStringList);

    // ------------------------------------------------------------------------
    // Create folder
    // ------------------------------------------------------------------------

    if (plateDirBarCode <> '') then
    begin
      destPlateFolderName := StringReplace(plateDirExperiment, ' ',
        '_', [rfReplaceAll]) + '.barcode_' +
        StringReplace(plateDirBarCode, ' ', '_', [rfReplaceAll]) +
        '.plate_' + plateNo + '.' + plateDirDate;
    end
    else
    begin
      destPlateFolderName := StringReplace(plateDirExperiment, ' ',
        '_', [rfReplaceAll]) + '.plate_' + plateNo +
        '.' + plateDirDate;
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

    // TODO: Remember to check for possible "timepoint" folders here
    if not (imgFilePaths.Count = 0) then
    begin
      // ----------------------------------------------------------------------
      // In case TimePoint folders do NOT exist
      // ----------------------------------------------------------------------
      for imgFilePath in imgFilePaths do
      begin
        if not (AnsiContainsStr(imgFilePath, 'thumb')) then
        begin
          imgDestName := FormatDestImageName(imgFilePath);
          imgDestPath := destPlateFolderPath + PathDelim + imgDestName;
          try
            CopyImage(imgFilePath, imgDestPath, logStringList, app);
            app.ProcessMessages;
            //Sleep(1); // Debug code to test the abort functionality
            if (chkAborted.Checked) then
            begin
              Log('*** !!! EXPORT ABORTED !!! ***', logStringList);
              ShowMessage('Warning: Export aborted!');
              Exit;
            end;
          except
            on E: Exception do
              ShowMessage('An error occured:' + LineEnding + E.Message);
          end;
        end
        else
        begin
          Log('Skipping thumbnail: ' + imgFilePath, logStringList);
        end;
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

        imgFilePaths := FileUtil.FindAllFiles(timeptDirPath, ImagePathPatterns, False);
        for imgFilePath in imgFilePaths do
        begin
          imgDestName := FormatDestImageName(imgFilePath);
          imgDestPath := destTimeptDirPath + PathDelim + imgDestName;
          CopyImage(imgFilePath, imgDestPath, logStringList, app);
          app.ProcessMessages;
          Sleep(1);
          if (chkAborted.Checked) then
          begin
            Log('*** !!! EXPORT ABORTED !!! ***', logStringList);
            ShowMessage('Warning: Export aborted!');
            Exit;
          end;

        end;
      end;

      timeptDirPaths.Free;
      // ----------------------------------------------------------------------
    end;

    imgFilePaths.Free;
  end;

  // TODO: Provide better assertions that things are following the correct
  //       structure

  if (chkAborted.Checked) then
  begin
    Log('Warning: Processing stopped, after abort!', logStringList);
  end
  else
  begin
    Log('Processing finished!', logStringList);
  end;
  plateDirs.Free;
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
