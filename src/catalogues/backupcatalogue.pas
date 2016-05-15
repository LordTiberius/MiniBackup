unit BackupCatalogue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BackupSet, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, LazUTF8;

type

  { TBackupCatalogue }

  TBackupCatalogue = class
  private
    function GetBackupSetCount: Integer;
  protected
    FName: String;
    FBackupSets: TFPList;

    procedure LoadBackupCatalogueFromStream(Filename: String);
    procedure LoadBackupCatalogueFromXMLDocument(Filename: String);
    procedure SaveBackupCatalogueToStream(Filename: String);
    procedure SaveBackupCatalogueToXMLDocument(Filename: String);


    procedure LoadNameFromDOMNode(Node: TDOMNode);
    procedure LoadNameFromStream(Stream: TStream);
    procedure SaveNameToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
    procedure SaveNameToStream(Stream: TStream);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function AddBackupSet: TBackupSet;

    procedure LoadFromFile(Filename: String);
    procedure SaveToFile(Filename: String);

    property BackupSetCount: Integer read GetBackupSetCount;
    property Name: String read FName write FName;
  end;

implementation

{ TBackupCatalogue }

function TBackupCatalogue.GetBackupSetCount: Integer;
begin
  Result := FBackupSets.Count;
end;

procedure TBackupCatalogue.LoadBackupCatalogueFromStream(Filename: String);
var
  Stream: TFileStream;
  BSCount: Word;
  Loop: Integer;
begin
  Stream := TFileStream.Create(UTF8ToSys(Filename), fmOpenRead);

  LoadNameFromStream(Stream);

  Stream.ReadBuffer(BSCount, SizeOf(BSCount));;

  for Loop := 0 to BSCount-1 do
    AddBackupSet.ReadFromStream(Stream);

  Stream.Free;
end;

procedure TBackupCatalogue.LoadBackupCatalogueFromXMLDocument(Filename: String);
var
  XMLDoc: TXMLDocument;
  RootNode: TDOMNode;
  Loop: Integer;
begin
  ReadXMLFile(XMLDoc, Filename);

  RootNode := XMLDoc.GetElementsByTagName('BackupCatalogue')[0];

  LoadNameFromDOMNode(RootNode);

  for Loop := 0 to TDOMElement(RootNode).GetElementsByTagName('BackupSet').
    Count-1 do
  begin
    AddBackupSet.ReadFromDOMNode(TDOMElement(RootNode).
      GetElementsByTagName('BackupSet')[Loop]);
  end;

  XMLDoc.Free;
end;

procedure TBackupCatalogue.SaveBackupCatalogueToStream(Filename: String);
var
  Stream: TFileStream;
  BSCount: Word;
  Loop: Integer;
begin
  Stream := TFileStream.Create(UTF8ToSys(Filename), fmCreate);

  SaveNameToStream(Stream);
  if FBackupSets.Count > High(BSCount) then
    raise Exception.Create('Number of BackupSets is too high (value overflow)');

  BSCount := FBackupSets.Count;
  Stream.WriteBuffer(BSCount, SizeOf(BSCount));;

  for Loop := 0 to FBackupSets.Count-1 do
    TBackupSet(FBackupSets[Loop]).SaveToStream(Stream);

  Stream.Free;
end;

procedure TBackupCatalogue.SaveBackupCatalogueToXMLDocument(Filename: String);
var
  XMLDoc: TXMLDocument;
  RootNode: TDOMNode;
  BackupSetNode: TDOMNode;
  Loop: Integer;
begin
  XMLDoc := TXMLDocument.Create;
  RootNode := XMLDoc.CreateElement('BackupCatalogue');

  SaveNameToDOMNode(XMLDoc, RootNode);

  for Loop := 0 to FBackupSets.Count-1 do
  begin
    BackupSetNode := XMLDoc.CreateElement('BackupSet');
    TBackupSet(FBackupSets[Loop]).SaveToDOMNode(XMLDoc, BackupSetNode);
    RootNode.AppendChild(BackupSetNode);
  end;

  XMLDoc.AppendChild(XMLDoc);
  WriteXMLFile(XMLDoc, Filename);
  XMLDoc.Free;
end;

procedure TBackupCatalogue.LoadNameFromDOMNode(Node: TDOMNode);
begin
  if TDOMElement(Node).GetElementsByTagName('CatalogueName').Count > 0 then
    FName := TDOMElement(Node).GetElementsByTagName('CatalogueName')[0].
      NodeValue;
end;

procedure TBackupCatalogue.LoadNameFromStream(Stream: TStream);
var
  Len: Word;
begin
  Stream.ReadBuffer(Len, SizeOf(Len));
  SetLength(FName, Len);
  Stream.ReadBuffer(FName[1], Len);
end;

procedure TBackupCatalogue.SaveNameToDOMNode(XMLDoc: TXMLDocument;
  Node: TDOMNode);
var
  ParentNode, TextNode: TDOMNode;
begin
  ParentNode := XMLDoc.CreateElement('CatalogueName');
  TextNode := XMLDoc.CreateTextNode(FName);

  ParentNode.AppendChild(TextNode);
  Node.AppendChild(ParentNode);
end;

procedure TBackupCatalogue.SaveNameToStream(Stream: TStream);
var
  Len: Word;
begin
  if Length(FName) > High(Len) then
    raise Exception.Create('Name is too long (value overflow)');

  Len := Length(FName);
  Stream.WriteBuffer(Len, SizeOf(Len));
  Stream.WriteBuffer(FName[1], Len);
end;

constructor TBackupCatalogue.Create;
begin
  FBackupSets := TFPList.Create;
end;

destructor TBackupCatalogue.Destroy;
begin
  FBackupSets.Free;
  inherited Destroy;
end;

function TBackupCatalogue.AddBackupSet: TBackupSet;
var
  NewBackupSet: TBackupSet;
begin
  NewBackupSet := TBackupSet.Create;
  FBackupSets.Add(NewBackupSet);
  Result := NewBackupSet;
end;

procedure TBackupCatalogue.LoadFromFile(Filename: String);
begin
  case LowerCase(ExtractFileExt(Filename)) of
    '.xml': LoadBackupCatalogueFromXMLDocument(Filename);
    '.buc': LoadBackupCatalogueFromStream(Filename);
  end;
end;

procedure TBackupCatalogue.SaveToFile(Filename: String);
begin
  case LowerCase(ExtractFileExt(Filename)) of
    '.xml': SaveBackupCatalogueToXMLDocument(Filename);
    '.buc': SaveBackupCatalogueToStream(Filename);
  end;
end;

end.

