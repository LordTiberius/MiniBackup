unit ApplicationSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, LazFileUtils;

type

  { TApplicationSettings }

  TApplicationSettings = class
  protected
    FConfigurationFile: String;

    function DetermineStandardConfigurationPath: String;
  public
    property ConfigurationFile: String read FConfigurationFile
      write FConfigurationFile;
    constructor Create;
    destructor Destroy; reintroduce;

    procedure SetSettingsToDefaultValues;
    procedure LoadFromFile;
    procedure SaveToFile;
  end;

implementation

{ TApplicationSettings }

function TApplicationSettings.DetermineStandardConfigurationPath: String;
begin
  Result := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'config.xml';
end;

constructor TApplicationSettings.Create;
begin
  FConfigurationFile := DetermineStandardConfigurationPath;
  SetSettingsToDefaultValues;
end;

destructor TApplicationSettings.Destroy;
begin
end;

procedure TApplicationSettings.SetSettingsToDefaultValues;
begin

end;

procedure TApplicationSettings.LoadFromFile;
var
  XMLDocument: TXMLDocument;
begin
  if not FileExistsUTF8(FConfigurationFile) then
    raise Exception.CreateFmt('Configuration file "%s" not found',
      [FConfigurationFile]);

  ReadXMLFile(XMLDocument, FConfigurationFile);

  XMLDocument.Free;
end;

procedure TApplicationSettings.SaveToFile;
var
  XMLDocument: TXMLDocument;
begin
  XMLDocument := TXMLDocument.Create;

  try
    WriteXMLFile(XMLDocument, FConfigurationFile);
  finally
    XMLDocument.Free;
  end;
end;

end.

