unit StringObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TString = class(TObject)
    TheText: String;
    constructor Create;
    function GetText: String;
    procedure SetText(NewText: String);
    property Text: String Read GetText Write SetText;
  end;

implementation
  var
    TheText: String;

  { TString }

  constructor TString.Create;
  begin
    Self.TheText := '';
  end;

  procedure TString.SetText(NewText: String);
  begin
    TheText := NewText;
  end;

  function TString.GetText(): String;
  begin
    Result := TheText;
  end;

end.

