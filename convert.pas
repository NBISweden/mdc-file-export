unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, FileUtil, RegExpr, SysUtils, StringObject, Strings;

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

begin
  imgFileNameExpr := TRegExpr.Create;
  imgFileNameExpr.Expression := '([^_]+)_([^_]+)_(s[^_]+)_((w[^_]+))?_(thumb)?.*';
  imgFileNameExpr.Compile;

  for i := 0 to plateDirs.Count-1 do
  begin
    // ------------------------------------------------------------------------
    // Extract info
    // ------------------------------------------------------------------------
    plateDirName := plateDirs.Strings[i];
    plateDirObj := TString(plateDirs.Objects[i]);
    plateDirPath := plateDirObj.Text;
    // Extract Date from the Plate Dir's parent directory
    plateDirDateDirPath := SysUtils.ExtractFilePath(plateDirPath);
    plateNo := FileUtil.ExtractFileNameOnly(plateDirPath);
    plateDirDate := FileUtil.ExtractFileNameOnly(
                      FileUtil.ChompPathDelim(plateDirDateDirPath));
    plateDirExperimentDirPath := SysUtils.ExtractFilePath(
                                   FileUtil.ChompPathDelim(plateDirDateDirPath));
    plateDirExperiment := FileUtil.ExtractFileNameOnly(
                            FileUtil.ChompPathDelim(plateDirExperimentDirPath));

    ShowMessage('Plate dir: ' + plateDirName + LineEnding +
                'Date: ' + plateDirDate + LineEnding +
                'Experiment: ' + plateDirExperiment);

    // ------------------------------------------------------------------------
    // Create folder
    // ------------------------------------------------------------------------

    destPlateFolderName := StringReplace(plateDirExperiment, ' ', '_', [rfReplaceAll]) +
                           '_plate_' + plateNo + '_' + plateDirDate;

    destPlateFolderPath := destDir + DirectorySeparator + destPlateFolderName;

    if not SysUtils.DirectoryExists(destPlateFolderPath) then
    begin
      ShowMessage('Trying to create folder ' + destPlateFolderPath);
      // SysUtils.CreateDir(destPlateFolder);
    end;

    // ------------------------------------------------------------------------
    // Loop over image files, and copy
    // ------------------------------------------------------------------------

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
    end;
    imgFilePaths.Free;
  end;

  // NOTE: Ok, what info do we need to create the destination file/folder
  // structure? ... and which info do we have (see checkboxes below)
  // --------------------------------------------------------------------
  // [x] 1. Basename (from image)
  // [x] 2. Plate number (from plate folder)
  // [x] 3. Date (possibly)
  // [x] 4. Well (from image)
  // [x] 5. Site (from image)
  // [x] 6. Wavelength (from image)

  // TODO: Remaining step: Create destination folder structure
  // So, now we need to construct what?
  // --------------------------------------------------------------------
  // 1. Experiment folders
  //    a. Base name
  //    b. Plate no
  //    c. Date(?)
  // 2. Optional time point folders
  // 3. Image files
  //    a. (Experiment) base name
  //    b. Well
  //    c. Site
  //    d. Wavelength

  // TODO: Remaining step: Copy files from source to destination
  //       Can we do everything inside the loop? Maybe inside each experiment
  //       folder loop iteration?

  // TODO: Provide better assertions that things are following the correct
  //       structure

  ShowMessage('Processing finished!');
  plateDirs.Free;
end;

end.

