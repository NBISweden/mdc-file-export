unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Convert, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Strings, ActnList, StringObject;

type

  { TMainForm }

  TMainForm = class(TForm)
    cmdStartConversion: TButton;
    chkSelectPlates: TCheckBox;
    lblLog: TLabel;
    memoLog: TMemo;
    txtSourceDir: TEdit;
    txtDestDir: TEdit;
    groupBoxSelectPlates: TGroupBox;
    cmdSelectSrcDir: TButton;
    cmdSelectDestDir: TButton;
    lblSrcDir: TLabel;
    lblDestDir: TLabel;
    lstPlates: TListBox;
    lstExperiments: TListBox;
    dlgSelectDestDir: TSelectDirectoryDialog;
    progressConversion: TProgressBar;
    lblSelectExperimen: TLabel;
    dlgSelectSrcDir: TSelectDirectoryDialog;
    procedure chkSelectPlatesChange(Sender: TObject);
    procedure cmdSelectDestDirClick(Sender: TObject);
    procedure cmdStartConversionClick(Sender: TObject);
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
  PlateDir: String;
  objPlateDir: TString;
  i: Integer;
begin
  lstPlates.Clear;

  ExperimentDirs := TStringList.Create;
  for i:=0 to lstExperiments.Items.Count-1 do
  begin
    // Process only the selected items in the experiments listbox
    if lstExperiments.Selected[i] then
    begin
      Experiment := lstExperiments.Items[i];
      // Construct the full path to experiment folder
      ExperimentDir := txtSourceDir.Text + SysUtils.PathDelim + Experiment;
      ExperimentDirs.Append(ExperimentDir);
    end;
  end;

  DateDirs := TStringList.Create;
  for ExperimentDir in ExperimentDirs do
  begin
    Experiment := SysUtils.ExtractFileName(ExperimentDir);
    DateDirs.Clear;
    DateDirs.AddStrings(FileUtil.FindAllDirectories(ExperimentDir, False));
    // Populate the plates listbox
    // Loop down into the date folders, to retrieve the PlateDir folders
    for DateDir in DateDirs do // TODO: Add validation of date pattern
    begin
      // Populate plates listbox with the plates (TODO: Use more than PlateDir no as ID?)
      for PlateDir in FileUtil.FindAllDirectories(DateDir, False) do
      begin
        objPlateDir := TString.Create;
        objPlateDir.Text := PlateDir;
        lstPlates.AddItem(SysUtils.ExtractFileName(PlateDir) + ' (' + Experiment + ')', objPlateDir);
      end;
    end;
  end;
  ExperimentDirs.Free;
  DateDirs.Free;
end;

procedure TMainForm.txtSourceDirectoryChange(Sender: TObject);
var
  Exp: String;
  Experiments: TStringList;
begin
  lstExperiments.Clear;
  if SysUtils.DirectoryExists(txtSourceDir.Text) then
  begin
    Experiments := TStringList.Create;
    // Enable the edit box
    txtSourceDir.Enabled := True;
    // Populate experiments listbox based on selected directory
    Experiments := FileUtil.FindAllDirectories(txtSourceDir.Text, False);
    lstExperiments.Clear;
    for Exp in Experiments do
      lstExperiments.AddItem(SysUtils.ExtractFileName(Exp),Sender);
    Experiments.Free;
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

procedure TMainForm.chkSelectPlatesChange(Sender: TObject);
begin
  if groupBoxSelectPlates.Enabled then
    groupBoxSelectPlates.Enabled := False
  else
    groupBoxSelectPlates.Enabled := True
end;

procedure TMainForm.txtDestDirChange(Sender: TObject);
begin
  txtDestDir.Enabled := True;
end;

procedure TMainForm.cmdStartConversionClick(Sender: TObject);
var
  plateDirs: TStringList;
  plateDir: String;
  i: integer;
begin
  // Retrieve the plate dirs
  plateDirs := TStringList.Create;
  for i := 0 to lstPlates.Items.Count-1 do
  begin
    if lstPlates.Selected[i] then
    begin
      plateDirs.AddObject(lstPlates.Items[i], lstPlates.Items.Objects[i]);
    end;
  end;

  // ----------------
  // Bunch of checks
  // ----------------

  // Check if source and destination paths are the same
  if Pos(txtSourceDir.Text, txtDestDir.Text) > 0 then
  begin
    ShowMessage('The destination directory must not be placed under the ' +
                'Source directory!');
    exit;
  end;

  // Source directory must be set
  if Length(txtSourceDir.Text) = 0 then
  begin
    ShowMessage('Source directory is not set!');
    exit;
  end;

  // Destination directory must be set
  if Length(txtDestDir.Text) = 0 then
  begin
    ShowMessage('Destination directory is empty!');
    exit;
  end;

  // At least one experiment has to be selected
  if (lstExperiments.SelCount = 0) then
  begin
    ShowMessage('At least one Experiment has to be selected!');
    exit;
  end;

  // Plates have to be selected, if enabled
  if chkSelectPlates.Checked and (lstPlates.SelCount = 0) then
  begin
    ShowMessage('At least one plate has to be selected, when the ' +
                '"Select specific plates" checkbox is selected.');
    exit;
  end;

  // Destination directory has to be writeable
  if not FileUtil.DirectoryIsWritable(txtDestDir.Text) then
  begin
    ShowMessage('Destination directory is not writable!');
    exit;
  end;

  // Destination directory must be empty
  if not ((FileUtil.FindAllDirectories(txtDestDir.Text).Count = 0) and
         (FileUtil.FindAllFiles(txtDestDir.Text).Count = 0)) then
  begin
    ShowMessage('Destination directory is not empty!');
    exit;
  end;

  // TODO: Add checks for folder structure in source folder!

  //ConvertFolderStructure(txtSourceDir.Text, txtDestDir.Text);
  ConvertFolderStructure(plateDirs, txtDestDir.Text, memoLog.Lines);
end;

end.
