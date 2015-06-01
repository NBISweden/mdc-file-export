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
    Experiments: TStringList;
    GroupBox1: TGroupBox;
    InputDir: TButton;
    InputDir1: TButton;
    Label1SrcDir: TLabel;
    Label1SrcDir1: TLabel;
    ListBox1: TListBox;
    ListBoxExperiments: TListBox;
    dlgSelectDestDir: TSelectDirectoryDialog;
    ProgressBar1: TProgressBar;
    SelectExperimenLabel: TLabel;
    dlgSelectSrcDir: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    procedure CheckBox1Change(Sender: TObject);
    procedure EditSourceDirChange(Sender: TObject);
    procedure InputDirClick(Sender: TObject);
    procedure Label1SrcDirClick(Sender: TObject);
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

procedure TForm1.EditSourceDirChange(Sender: TObject);
var
  Exp: String;
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

