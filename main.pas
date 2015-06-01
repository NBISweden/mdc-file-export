unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    EditSourceDir: TEdit;
    EditSourceDir1: TEdit;
    GroupBox1: TGroupBox;
    InputDir: TButton;
    InputDir1: TButton;
    Label1SrcDir: TLabel;
    Label1SrcDir1: TLabel;
    ListBoxWells: TListBox;
    ListBoxExperiments: TListBox;
    dlgSelectDestDir: TSelectDirectoryDialog;
    ProgressBar1: TProgressBar;
    SelectExperimenLabel: TLabel;
    dlgSelectSrcDir: TSelectDirectoryDialog;
    procedure CheckBox1Change(Sender: TObject);
    procedure EditSourceDirChange(Sender: TObject);
    procedure InputDirClick(Sender: TObject);
    procedure Label1SrcDirClick(Sender: TObject);
    procedure ListBoxExperimentsSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.InputDirClick(Sender: TObject);
begin
  if dlgSelectSrcDir.Execute then
  begin
    EditSourceDir.Text:=dlgSelectSrcDir.FileName;
  end;
end;

procedure TForm1.Label1SrcDirClick(Sender: TObject);
begin

end;

procedure TForm1.ListBoxExperimentsSelectionChange(Sender: TObject;
  User: boolean);
var
  ExperimentDirs: TStringList;
  Wells: TStringList;
  Well: String;
  Experiment: String;
  ExperimentDir: String;
  i: Integer;
begin
  ListBoxWells.Clear;
  ExperimentDirs := TStringList.Create;
  for i:=0 to ListBoxExperiments.Items.Count-1 do
      if ListBoxExperiments.Selected[i] then
         Experiment := ListBoxExperiments.Items[i];
         ExperimentDir := EditSourceDir.Text + SysUtils.PathDelim + Experiment;
         ExperimentDirs.Add(ExperimentDir);
  Wells := TStringList.Create;
  For ExperimentDir in ExperimentDirs do
      Wells.AddStrings(FileUtil.FindAllDirectories(ExperimentDir, False));
  For Well in Wells do
      ListBoxWells.AddItem(SysUtils.ExtractFileName(Well), Sender);
end;

procedure TForm1.EditSourceDirChange(Sender: TObject);
var
  Exp: String;
  Experiments: TStringList;
begin
  ListBoxExperiments.Clear;
  if SysUtils.DirectoryExists(EditSourceDir.Text) then
  begin
    Experiments := FileUtil.FindAllDirectories(EditSourceDir.Text, False);
    ListBoxExperiments.Clear;
    For Exp in Experiments do
        ListBoxExperiments.AddItem(SysUtils.ExtractFileName(Exp),Sender);
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin

end;

end.

