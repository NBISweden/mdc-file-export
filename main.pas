unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList;

type

  { TMainForm }

  TMainForm = class(TForm)
    cmdStartConversion: TButton;
    chkSelectWells: TCheckBox;
    txtSourceDir: TEdit;
    txtDestDir: TEdit;
    groupBoxSelectWells: TGroupBox;
    cmdSelectSrcDir: TButton;
    cmdSelectDestDir: TButton;
    Label1SrcDir: TLabel;
    Label1SrcDir1: TLabel;
    lstWells: TListBox;
    lstExperiments: TListBox;
    dlgSelectDestDir: TSelectDirectoryDialog;
    progressConversion: TProgressBar;
    SelectExperimenLabel: TLabel;
    dlgSelectSrcDir: TSelectDirectoryDialog;
    procedure chkSelectWellsChange(Sender: TObject);
    procedure txtSourceDirChange(Sender: TObject);
    procedure cmdSelectSrcDirClick(Sender: TObject);
    procedure Label1SrcDirClick(Sender: TObject);
    procedure lstExperimentsSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.cmdSelectSrcDirClick(Sender: TObject);
begin
  if dlgSelectSrcDir.Execute then
  begin
    txtSourceDir.Text:=dlgSelectSrcDir.FileName;
  end;
end;

procedure TMainForm.Label1SrcDirClick(Sender: TObject);
begin

end;

procedure TMainForm.lstExperimentsSelectionChange(Sender: TObject;
  User: boolean);
var
  ExperimentDirs: TStringList;
  ExperimentDir: String;
  Experiment: String;
  DateDirs: TStringList;
  DateDir: String;
  Dates: TStringList;
  Date: String;
  Well: String;
  Wells: TStringList;
  i: Integer;
begin
  lstWells.Clear;
  ExperimentDirs := TStringList.Create;
  for i:=0 to lstExperiments.Items.Count-1 do
      // Process only the selected items in the experiments listbox
      if lstExperiments.Selected[i] then
         Experiment := lstExperiments.Items[i];
         // Construct the full path to experiment folder
         ExperimentDir := txtSourceDir.Text + SysUtils.PathDelim + Experiment;
         ExperimentDirs.Add(ExperimentDir);
  DateDirs := TStringList.Create;
  for ExperimentDir in ExperimentDirs do
      DateDirs.AddStrings(FileUtil.FindAllDirectories(ExperimentDir, False));
  // Populate the wells listbox
  Wells := TStringList.Create;
  for DateDir in DateDirs do
      Wells.AddStrings(FileUtil.FindAllDirectories(DateDir, False));
  for Well in Wells do
      lstWells.AddItem(SysUtils.ExtractFileName(Well), Sender);
end;

procedure TMainForm.txtSourceDirChange(Sender: TObject);
var
  Exp: String;
  Experiments: TStringList;
begin
  lstExperiments.Clear;
  if SysUtils.DirectoryExists(txtSourceDir.Text) then
  begin
    Experiments := FileUtil.FindAllDirectories(txtSourceDir.Text, False);
    lstExperiments.Clear;
    for Exp in Experiments do
        lstExperiments.AddItem(SysUtils.ExtractFileName(Exp),Sender);
  end;
end;

procedure TMainForm.chkSelectWellsChange(Sender: TObject);
begin

end;

end.

