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
    lblSrcDir: TLabel;
    lblDestDir: TLabel;
    lstWells: TListBox;
    lstExperiments: TListBox;
    dlgSelectDestDir: TSelectDirectoryDialog;
    progressConversion: TProgressBar;
    lblSelectExperimen: TLabel;
    dlgSelectSrcDir: TSelectDirectoryDialog;
    procedure chkSelectWellsChange(Sender: TObject);
    procedure cmdSelectDestDirClick(Sender: TObject);
    procedure txtDestDirChange(Sender: TObject);
    procedure txtSourceDirectoryChange(Sender: TObject);
    procedure cmdSelectSrcDirClick(Sender: TObject);
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
  // Loop down into the date folders, to retrieve the well folders
  for DateDir in DateDirs do // TODO: Add validation of date pattern
    Wells.AddStrings(FileUtil.FindAllDirectories(DateDir, False));
  // Populate wells listbox with the wells (TODO: Use more than well no as ID?)
  for Well in Wells do
    lstWells.AddItem(SysUtils.ExtractFileName(Well), Sender);
end;

procedure TMainForm.txtSourceDirectoryChange(Sender: TObject);
var
  Exp: String;
  Experiments: TStringList;
begin
  lstExperiments.Clear;
  if SysUtils.DirectoryExists(txtSourceDir.Text) then
  begin
    // Enable the edit box
    txtSourceDir.Enabled := True;
    // Populate experiments listbox based on selected directory
    Experiments := FileUtil.FindAllDirectories(txtSourceDir.Text, False);
    lstExperiments.Clear;
    for Exp in Experiments do
      lstExperiments.AddItem(SysUtils.ExtractFileName(Exp),Sender);
  end;
end;

procedure TMainForm.cmdSelectSrcDirClick(Sender: TObject);
begin
  if dlgSelectSrcDir.Execute then
  begin
    txtSourceDir.Text := dlgSelectSrcDir.FileName;
  end;
end;

procedure TMainForm.cmdSelectDestDirClick(Sender: TObject);
begin
    if dlgSelectDestDir.Execute then
    begin
      txtDestDir.Text := dlgSelectDestDir.FileName;
    end;
end;

procedure TMainForm.chkSelectWellsChange(Sender: TObject);
begin
  if groupBoxSelectWells.Enabled then
    groupBoxSelectWells.Enabled := False
  else
    groupBoxSelectWells.Enabled := True
end;

procedure TMainForm.txtDestDirChange(Sender: TObject);
begin
  txtDestDir.Enabled := True;
end;

end.

