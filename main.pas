unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Convert, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, StringObject, RegExpr, LCLIntf;

type

  { TMainForm }

  TMainForm = class(TForm)
    chkAborted: TCheckBox;
    cmdOpenDestDir: TButton;
    cmdStartConversion: TButton;
    chkSelectPlates: TCheckBox;
    cmdAbortExport: TButton;
    lblLog: TLabel;
    memoLog: TMemo;
    prgbarMain: TProgressBar;
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
    procedure cmdAbortExportClick(Sender: TObject);
    procedure cmdOpenDestDirClick(Sender: TObject);
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

procedure TMainForm.lstExperimentsSelectionChange(Sender: TObject; User: boolean);
var
  ExperimentDirs: TStringList;
  ExperimentDir: string;
  Experiment: string;

  ExperimentSubDirs: TStringList;
  ExperimentSubDir: string;

  BarCodeDirs: TStringList;
  BarCodeDir: string;
  BarCode: string;
  BarCodeExists: boolean;

  DateDirs: TStringList;
  DateDir: string;
  Date: string;
  DatePtrn: TRegExpr;

  PlateDir: string;
  objPlateDir: TString;
  i: integer;
begin
  lstPlates.Clear;
  BarCodeExists := False;

  ExperimentDirs := TStringList.Create;
  for i := 0 to lstExperiments.Items.Count - 1 do
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

  ExperimentSubDirs := TStringList.Create;
  DatePtrn := TRegExpr.Create;
  DatePtrn.Expression := '\d{4}-\d{2}-\d{2}';
  DatePtrn.Compile;

  for ExperimentDir in ExperimentDirs do
  begin
    Experiment := SysUtils.ExtractFileName(ExperimentDir);
    ExperimentSubDirs.Clear;
    ExperimentSubDirs.AddStrings(FileUtil.FindAllDirectories(ExperimentDir, False));
    // Populate the plates listbox by looping down into the barcode and date
    // folders, to retrieve the PlateDir folders
    for ExperimentSubDir in ExperimentSubDirs do
    begin
      if DatePtrn.Exec(ExperimentSubDir) then
        // -----------------------------------------------------------------------
        // CASE: DATE FOLDER
        // -----------------------------------------------------------------------
      begin
        Date := FileUtil.ExtractFileNameOnly(ExperimentSubDir);
        // Populate plates listbox with the plates (TODO: Use more than PlateDir no as ID?)
        for PlateDir in FileUtil.FindAllDirectories(ExperimentSubDir, False) do
        begin
          objPlateDir := TString.Create;
          objPlateDir.Text := PlateDir;
          lstPlates.AddItem('Plate: ' + SysUtils.ExtractFileName(PlateDir) +
            ' | Experiment: ' + Experiment + ' | Date: ' + Date, objPlateDir);
        end;
      end
      else
      begin
        // -----------------------------------------------------------------------
        // CASE: NOT DATE FOLDER (IMPLICITLY ASSUME BARCODE FOLDER?)
        // -----------------------------------------------------------------------
        BarCodeExists := True;
        BarCode := FileUtil.ExtractFileNameOnly(ExperimentSubDir);
        for DateDir in FileUtil.FindAllDirectories(ExperimentSubDir, False) do
        begin
          Date := FileUtil.ExtractFileNameOnly(DateDir);
          if DatePtrn.Exec(Date) then
          begin
            for PlateDir in FileUtil.FindAllDirectories(DateDir, False) do
            begin
              objPlateDir := TString.Create;
              objPlateDir.Text := PlateDir;
              lstPlates.AddItem('Plate: ' + SysUtils.ExtractFileName(PlateDir) +
                ' | Experiment: ' + Experiment + ' | Barcode: ' +
                BarCode + ' | Date: ' + Date, objPlateDir);
            end;
          end
          else
          begin
            ShowMessage('Warning: Subfolder of barcode dir is not a date: ' +
              DateDir + '!');
          end;
        end;
        // ShowMessage('Folder is not a date: ' + ExperimentSubDir);
      end;
    end;
  end;
  ExperimentDirs.Free;
  ExperimentSubDirs.Free;
end;

procedure TMainForm.txtSourceDirectoryChange(Sender: TObject);
var
  Exp: string;
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
      lstExperiments.AddItem(SysUtils.ExtractFileName(Exp), Sender);
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
    groupBoxSelectPlates.Enabled := True;
end;

procedure TMainForm.cmdAbortExportClick(Sender: TObject);
begin
  chkAborted.Checked := True;
  cmdAbortExport.Enabled := False;
  cmdAbortExport.Caption := 'Aborting ...';
end;

procedure TMainForm.cmdOpenDestDirClick(Sender: TObject);
begin
  OpenDocument(txtDestDir.Text);
end;

procedure TMainForm.txtDestDirChange(Sender: TObject);
begin
  txtDestDir.Enabled := True;
  if (txtDestDir.Text <> '') then
  begin
    cmdOpenDestDir.Enabled := True;
  end;
end;

procedure TMainForm.cmdStartConversionClick(Sender: TObject);
var
  plateDirs: TStringList;
  i: integer;
begin
  // Restore abortion button and (hidden checkbox) states:
  chkAborted.Checked := False;
  cmdAbortExport.Enabled := True;
  cmdAbortExport.Caption := 'Abort Image Export';

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
  try
    if not FileUtil.DirectoryIsWritable(txtDestDir.Text) then
    begin
      ShowMessage('Destination directory is not writable!');
      exit;
    end;
  except
    on E: Exception do
      ShowMessage('Error on checking if directory is writable:' +
        LineEnding + txtDestDir.Text + LineEnding + 'Error message: ' +
        E.Message);


  end;

  // Destination directory must be empty
  if not ((FileUtil.FindAllDirectories(txtDestDir.Text).Count = 0) and
    (FileUtil.FindAllFiles(txtDestDir.Text).Count = 0)) then
  begin
    ShowMessage('Destination directory is not empty!');
    exit;
  end;

  // TODO: Add checks for folder structure in source folder!

  // Retrieve the plate dirs
  plateDirs := TStringList.Create;
  for i := 0 to lstPlates.Items.Count - 1 do
  begin
    if lstPlates.Selected[i] or not chkSelectPlates.Checked then
      // If we don't select on plates, take all the plates
    begin
      plateDirs.AddObject(lstPlates.Items[i], lstPlates.Items.Objects[i]);
    end;
  end;

  memoLog.Clear; // In case of a restart, so we don't mix up multiple things...

  //prgbarMain.Style := pbstMarquee;
  ConvertFolderStructure(txtSourceDir.Text, plateDirs, txtDestDir.Text,
    memoLog.Lines, prgbarMain, chkAborted, Application);

  if (chkAborted.Checked) then
  begin
    ShowMessage('Warning: Processing stopped, after abort!');
  end
  else
  begin
    ShowMessage('Conversion Finished!');
  end;

end;

end.
