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
  plateDirDate: String;

  imgFileNameExpr: TRegExpr;
  imgFilePaths: TStringList;
  imgFilePath: String;
  imgFileName: String;

  imgInfoBaseName: String;
  imgInfoWell: String;
  imgInfoSite: String;
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
    // Extract Date from the Plate Dir's parent directory
    plateDirDate := FileUtil.ExtractFileNameOnly(
                      FileUtil.ChompPathDelim(
                        SysUtils.ExtractFilePath(plateDirPath)
                      )
                    );

    ShowMessage('Plate dir: ' + plateDirName + LineEnding +
                'With directory:' + LineEnding +
                plateDirPath + LineEnding +
                'Date: ' + plateDirDate);

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
      // imgInfoIsThumb := imgFileNameExpr[1];
      ShowMessage('Image info' + LineEnding +
                  '------------------------------' + LineEnding +
                  'BaseName: '   + imgInfoBaseName + LineEnding +
                  'Well: '       + imgInfoWell + LineEnding +
                  'WellItem: '   + imgInfoSite + LineEnding +
                  'WaveLength: ' + imgInfoWaveLength);
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

