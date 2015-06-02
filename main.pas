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
  Well: String;
  i: Integer;
begin
  lstWells.Clear;

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
    // Populate the wells listbox
    // Loop down into the date folders, to retrieve the well folders
    for DateDir in DateDirs do // TODO: Add validation of date pattern
    begin
      // Populate wells listbox with the wells (TODO: Use more than well no as ID?)
      for Well in FileUtil.FindAllDirectories(DateDir, False) do
      begin
        lstWells.AddItem(SysUtils.ExtractFileName(Well) + ' (' + Experiment + ')', Sender);
      end;
    end;
  end;
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

procedure TMainForm.cmdStartConversionClick(Sender: TObject);
begin
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
    ShowMessage('Source directory is empty!');
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

  // Wells have to be selected, if enabled
  if chkSelectWells.Checked and (lstWells.SelCount = 0) then
  begin
    ShowMessage('At least one well has to be selected, when the ' +
                '"Select specific wells" checkbox is selected.');
    exit;
  end;

  ShowMessage('Starting to process...'); // TODO: Implement processing here ...
end;

end.
