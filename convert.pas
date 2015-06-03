unit Convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, FileUtil, RegExpr, SysUtils;

procedure ConvertFolderStructure(srcDir: String; destDir: String);

implementation

procedure ConvertFolderStructure(srcDir: String; destDir: String);
var
  exprExperimentDir: TRegExpr;
  exprDateDir: TRegExpr;
  exprPlateDir: TRegExpr;
  experimentDirs: TStringList;
  experimentDir: String;
  dateDirs: TStringList;
  dateDir: String;
  plateDirs: TStringList;
  plateDir: String;
begin
  experimentDirs := TStringList.Create;
  // TODO: Implement processing here.

  exprExperimentDir := TRegExpr.Create;
  exprExperimentDir.Expression := 'Experiment\ .*';
  exprExperimentDir.Compile;

  experimentDirs := FileUtil.FindAllDirectories(srcDir, False);

  for experimentDir in experimentDirs do
  begin
    exprDateDir := TRegExpr.Create;
    exprDateDir.Expression := '[0-9]{4}\-[0-9]{2}\-[0-9]{2}';
    exprDateDir.Compile;

    dateDirs := FileUtil.FindAllDirectories(experimentDir, False);

    for dateDir in dateDirs do
    begin
      exprPlateDir := TRegExpr.Create;
      exprPlateDir.Expression := '[0-9]{4}';
      exprPlateDir.Compile;

      plateDirs := FileUtil.FindAllDirectories(dateDir, False);
      for plateDir in plateDirs do
      begin
        if exprPlateDir.Exec(SysUtils.ExtractFileName(plateDir)) then
          ShowMessage('Plate dir: ' + SysUtils.ExtractFileName(plateDir));
      end;
    end;

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

  exprPlateDir.Free;
  plateDirs.Free;
end;

end.

