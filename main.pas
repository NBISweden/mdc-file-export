unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList;

type

  { TForm1 }

  TForm1 = class(TForm)
    EditSourceDir: TEdit;
    Experiments: TStringList;
    InputDir: TButton;
    Label1SrcDir: TLabel;
    ListBoxExperiments: TListBox;
    SelectExperimenLabel: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
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
  if SelectDirectoryDialog1.Execute then
  begin
    EditSourceDir.Text:=SelectDirectoryDialog1.FileName;
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

end.

