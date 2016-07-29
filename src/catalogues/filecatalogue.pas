unit FileCatalogue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM;

type
  TSHA512Hash = array[0..63] of Byte;

  TFileVersion = record
    BackupFilename: String;
    Size, SizeCompressed: Int64;
    LastChangedTimestamp: Int64;
    SHA512, SHA512Compressed: TSHA512Hash;
    BackupSet: Word;
  end;

  TEntryType = (etUnspecified, etFile, etDirectory);

  TEntry = class;

  { TEntries }

  TEntries = class
  private
    function GetItem(Index: Integer): TEntry;
    function GetItemCount: Integer;
    procedure SetItem(Index: Integer; AValue: TEntry);
  protected
    FItems: TFPList;
    FParent: TEntry;
  public
    constructor Create(AParent: TEntry); reintroduce;
    destructor Destroy; override;

    procedure SaveToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromDOMNode(Node: TDOMNode);
    procedure LoadFromStream(Stream: TStream);

    function Add: TEntry;
    function AggergateFileCount: Integer;
    function AggregateSize: Int64;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TEntry read GetItem write SetItem; default;
  end;

  { TEntry }

  TEntry = class
  private
    function GetCurrentFileVersion: TFileVersion;
    function GetEntries: TEntries;
    function GetFileVersionByIndex(Index: Integer): TFileVersion;
    function GetFileVersionCount: Integer;
    function GethasChilds: Boolean;
    function GetIsDirectory: Boolean;
    function GetName: String;
    function GetParent: TEntry;
    procedure SetIsDirectory(AValue: Boolean);
    procedure SetName(AValue: String);

    procedure SaveNameToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
    procedure SaveNameToStream(Stream: TStream);
    procedure LoadNameFromDOMNode(Node: TDOMNode);
    procedure LoadNameFromStream(Stream: TStream);

    procedure SaveFlagsToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
    procedure SaveFlagsToStream(Stream: TStream);
    procedure LoadFlagsFromDOMNode(Node: TDOMNode);
    procedure LoadFlagsFromStream(Stream: TStream);

    procedure SaveVersionsToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
    procedure SaveVersionsToStream(Stream: TStream);
      procedure SaveVersionToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode;
        FileVersion: TFileVersion);
    procedure LoadVersionsFromDOMNode(Node: TDOMNode);
      function LoadVersionFromDOMNode(Node: TDOMNode): TFileVersion;
    procedure LoadVersionsFromStream(Stream: TStream);
  protected
    FEntries: TEntries;
    FName: String;
    FParent: TEntry;
    FEntryType: TEntryType;
    FVersions: array of TFileVersion;

    procedure RemoveChild(Child: TEntry);
  public
    constructor Create(AParent: TEntry); reintroduce;
    destructor Destroy; override;

    function AddChild: TEntry;
    function AddNewVersion(BackupFilename: String; Size, CompressedSize: Int64;
      LastChangedTimestamp: Int64; SHA512, CompressedSHA512: TSHA512Hash;
      BackupSet: Word): Integer;
    function AggregateFileCount: Integer;
    function AggregateFilename: String;
    function AggregateSize: Int64;

    function FileExists(Filename: String): Boolean;
    procedure Remove;

    procedure SetCurrentVersionSHA512(Hash: TSHA512Hash);

    procedure SaveToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromDOMNode(Node: TDOMNode);
    procedure LoadFromStream(Stream: TStream);

    property Childs: TEntries read GetEntries;
    property CurrentVersion: TFileVersion read GetCurrentFileVersion;
    property hasChilds: Boolean read GethasChilds;
    property isDirectory: Boolean read GetIsDirectory write SetIsDirectory;
    property Parent: TEntry read GetParent;
    property Name: String read GetName write SetName;
    property VersionCount: Integer read GetFileVersionCount;
    property Versions[Index: Integer]: TFileVersion read GetFileVersionByIndex;
  end;

  operator = (h1, h2 : TSHA512Hash) b : boolean;

implementation

operator=(h1, h2: TSHA512Hash)b: boolean;
begin
  Result := CompareMem(@h1, @h2, 64)
end;

{ TEntries }

function TEntries.GetItem(Index: Integer): TEntry;
begin
  if (Index < 0) or (Index > FItems.Count-1) then
    raise Exception.CreateFmt('Item index (%d) out of bounds (must be ' +
      'between 0 and %d)', [Index, FItems.Count-1]);

  Result := TEntry(FItems[Index]);
end;

function TEntries.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TEntries.SetItem(Index: Integer; AValue: TEntry);
begin
  if (Index < 0) or (Index > FItems.Count-1) then
    raise Exception.CreateFmt('Item index (%d) out of bounds (must be ' +
      'between 0 and %d)', [Index, FItems.Count-1]);

  FItems[Index] := AValue;
end;

constructor TEntries.Create(AParent: TEntry);
begin
  FParent := AParent;
  FItems := TFPList.Create;
end;

destructor TEntries.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TEntries.SaveToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
var
  Loop: Integer;
  NewNode, Child: TDOMNode;
begin
  NewNode := XMLDoc.CreateElement('Entries');
  Node.AppendChild(NewNode);
  for Loop := 0 to FItems.Count-1 do
  begin
    Child := XMLDoc.CreateElement('Entry');
    NewNode.AppendChild(Child);

    TEntry(FItems[Loop]).SaveToDOMNode(XMLDoc, Child);
  end;
end;

procedure TEntries.SaveToStream(Stream: TStream);
var
  ICount: Word;
  Loop: Integer;
begin
  if FItems.Count > High(ICount) then
    raise Exception.Create('Value overflow');

  ICount := FItems.Count;
  Stream.WriteBuffer(ICount, SizeOf(ICount));
  for Loop := 0 to FItems.Count-1 do
    TEntry(FItems[Loop]).SaveToStream(Stream);
end;

procedure TEntries.LoadFromDOMNode(Node: TDOMNode);
var
  NewEntry: TEntry;
  Loop: Integer;
  ParentNode: TDOMNode;
begin
  if TDOMElement(Node).GetElementsByTagName('Entries').Count > 0 then
  begin
    ParentNode := TDOMElement(Node).GetElementsByTagName('Entries')[0];

    for Loop := 0 to ParentNode.ChildNodes.Count-1 do
    begin
      NewEntry := TEntry.Create(FParent);
      FItems.Add(NewEntry);
      NewEntry.LoadFromDOMNode(ParentNode.ChildNodes[Loop]);
    end;
  end;
end;

procedure TEntries.LoadFromStream(Stream: TStream);
var
  ICount: Word;
  Loop: Integer;
  NE: TEntry;
begin
  Stream.ReadBuffer(ICount, SizeOf(ICount));
  for Loop := 0 to ICount-1 do
  begin
    NE := TEntry.Create(FParent);
    FItems.Add(NE);
    NE.LoadFromStream(Stream);
  end;
end;

function TEntries.Add: TEntry;
var
  NewEntry: TEntry;
begin
  NewEntry := TEntry.Create(FParent);
  FItems.Add(NewEntry);
  Result := NewEntry;
end;

function TEntries.AggergateFileCount: Integer;
var
  Loop: Integer;
begin
  Result := 0;
  for Loop := 0 to FItems.Count-1 do
    Result := Result + TEntry(FItems[Loop]).AggregateFileCount;
end;

function TEntries.AggregateSize: Int64;
var
  Loop: Integer;
begin
  Result := 0;
  for Loop := 0 to FItems.Count-1 do
    if TEntry(FItems[Loop]).isDirectory then
      Result := Result + TEntry(FItems[Loop]).AggregateSize
    else
      if TEntry(FItems[Loop]).VersionCount > 0 then
        Result := Result + TEntry(FItems[Loop]).CurrentVersion.Size;
end;

{ TEntry }

function TEntry.GetCurrentFileVersion: TFileVersion;
begin
  if Length(FVersions) = 0 then
    raise Exception.Create('No versions available');

  Result := FVersions[High(FVersions)];
end;

function TEntry.GetEntries: TEntries;
begin
  Result := FEntries;
end;

function TEntry.GetFileVersionByIndex(Index: Integer): TFileVersion;
begin
  if (Index < Low(FVersions)) or (Index > High(FVersions)) then
    raise Exception.CreateFmt('Version index (%d) out of bounds (must be ' +
      'between %d and %d)', [Index, Low(FVersions), High(FVersions)]);

  Result := FVersions[Index];
end;

function TEntry.GetFileVersionCount: Integer;
begin
  Result := Length(FVersions);
end;

function TEntry.GethasChilds: Boolean;
begin
  Result := (FEntries.ItemCount > 0);
end;

function TEntry.GetIsDirectory: Boolean;
begin
  Result := (FEntryType = etDirectory);
end;

function TEntry.GetName: String;
begin
  Result := FName;
end;

function TEntry.GetParent: TEntry;
begin
  Result := FParent;
end;

procedure TEntry.SetIsDirectory(AValue: Boolean);
begin
  if FEntryType <> etUnspecified then
    raise Exception.Create('Entry type was already specified');

  if AValue then
    FEntryType := etDirectory
  else
    FEntryType := etFile;
end;

procedure TEntry.SetName(AValue: String);
begin
  if (FParent <> nil) and (FParent.FileExists(AValue)) then
    raise Exception.Create('Filename already exists');
  FName := AValue;
end;

procedure TEntry.SaveNameToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
var
  NewNode, TextNode: TDOMNode;
begin
  NewNode := XMLDoc.CreateElement('Name');
  Node.AppendChild(NewNode);

  TextNode := XMLDoc.CreateTextNode(FName);
  NewNode.AppendChild(TextNode);
end;

procedure TEntry.SaveNameToStream(Stream: TStream);
var
  Len: Word;
begin
  Len := Length(FName);
  Stream.WriteBuffer(Len, SizeOf(Len));
  Stream.WriteBuffer(FName[1], Len);
end;

procedure TEntry.LoadNameFromDOMNode(Node: TDOMNode);
begin
  if TDOMElement(Node).GetElementsByTagName('Name').Count > 0 then
    FName := TDOMElement(Node).GetElementsByTagName('Name')[0].FirstChild.NodeValue;
end;

procedure TEntry.LoadNameFromStream(Stream: TStream);
var
  Len: Word;
begin
  Stream.ReadBuffer(Len, SizeOf(Len));
  SetLength(FName, Len);
  Stream.ReadBuffer(FName[1], Len);
end;

procedure TEntry.SaveFlagsToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
var
  FlagsNode, NewNode, TextNode: TDOMNode;
begin
  FlagsNode := XMLDoc.CreateElement('Flags');
  Node.AppendChild(FlagsNode);

  NewNode := XMLDoc.CreateElement('entryType');
  TextNode := XMLDoc.CreateTextNode(IntToStr(Ord(FEntryType)));

  NewNode.AppendChild(TextNode);
  FlagsNode.AppendChild(NewNode);
end;

procedure TEntry.SaveFlagsToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FEntryType, SizeOf(TEntryType));
end;

procedure TEntry.LoadFlagsFromDOMNode(Node: TDOMNode);
var
  ParentNode: TDOMNode;
begin
  if TDOMElement(Node).GetElementsByTagName('Flags').Count > 0 then
  begin
    ParentNode := TDOMElement(Node).GetElementsByTagName('Flags')[0];

    if TDOMElement(ParentNode).GetElementsByTagName('entryType').Count > 0 then
    begin
      FEntryType := TEntryType(StrToInt(TDOMElement(ParentNode).
        GetElementsByTagName('entryType')[0].FirstChild.NodeValue));
    end;
  end;
end;

procedure TEntry.LoadFlagsFromStream(Stream: TStream);
begin
  Stream.ReadBuffer(FEntryType, SizeOf(TEntryType));
end;

procedure TEntry.SaveVersionsToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
var
  ParentNode: TDOMNode;
  Loop: Integer;
begin
  ParentNode := XMLDoc.CreateElement('Versions');
  Node.AppendChild(ParentNode);

  for Loop := 0 to High(FVersions) do
    SaveVersionToDOMNode(XMLDoc, ParentNode, FVersions[Loop]);
end;

procedure TEntry.SaveVersionsToStream(Stream: TStream);
var
  Count: Word;
  Loop: Integer;
begin
  Count := Length(FVersions);
  Stream.WriteBuffer(Count, SizeOf(Count));
  for Loop := 0 to High(FVersions) do
  begin
    Count := Length(FVersions[Loop].BackupFilename);
    Stream.WriteBuffer(Count, SizeOf(Count));
    Stream.WriteBuffer(FVersions[Loop].BackupFilename[1], Count);
    Stream.WriteBuffer(FVersions[Loop].Size, SizeOf(FVersions[Loop].Size));
    Stream.WriteBuffer(FVersions[Loop].SizeCompressed, SizeOf(FVersions[Loop].
      SizeCompressed));
    Stream.WriteBuffer(FVersions[Loop].SHA512, SizeOf(FVersions[Loop].SHA512));
    Stream.WriteBuffer(FVersions[Loop].SHA512Compressed, SizeOf(FVersions[Loop].
      SHA512Compressed));
    Stream.WriteBuffer(FVersions[Loop].LastChangedTimestamp,
      SizeOf(FVersions[Loop].LastChangedTimestamp));
    Stream.WriteBuffer(FVersions[Loop].BackupSet, SizeOf(FVersions[Loop].
      BackupSet));
  end;
end;

procedure TEntry.SaveVersionToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode;
  FileVersion: TFileVersion);
var
  ParentNode, NewNode, TextNode: TDOMNode;
  Hex: String;
begin
  ParentNode := XMLDoc.CreateElement('Version');
  Node.AppendChild(ParentNode);

  NewNode := XMLDoc.CreateElement('BackupFilename');
  TextNode := XMLDoc.CreateTextNode(FileVersion.BackupFilename);
  NewNode.AppendChild(TextNode);
  ParentNode.AppendChild(NewNode);

  NewNode := XMLDoc.CreateElement('Size');
  TextNode := XMLDoc.CreateTextNode(IntToStr(FileVersion.Size));
  NewNode.AppendChild(TextNode);
  ParentNode.AppendChild(NewNode);

  NewNode := XMLDoc.CreateElement('SizeCompressed');
  TextNode := XMLDoc.CreateTextNode(IntToStr(FileVersion.SizeCompressed));
  NewNode.AppendChild(TextNode);
  ParentNode.AppendChild(NewNode);

  SetLength(Hex, Length(FileVersion.SHA512)*2);
  BinToHex(@FileVersion.SHA512, @Hex[1], Length(FileVersion.SHA512));
  NewNode := XMLDoc.CreateElement('SHA512');
  TextNode := XMLDoc.CreateTextNode(LowerCase(Hex));
  NewNode.AppendChild(TextNode);
  ParentNode.AppendChild(NewNode);

  SetLength(Hex, Length(FileVersion.SHA512Compressed)*2);
  BinToHex(@FileVersion.SHA512Compressed, @Hex[1], Length(FileVersion.SHA512));
  NewNode := XMLDoc.CreateElement('SHA512Compressed');
  TextNode := XMLDoc.CreateTextNode(LowerCase(Hex));
  NewNode.AppendChild(TextNode);
  ParentNode.AppendChild(NewNode);

  NewNode := XMLDoc.CreateElement('LastChangedTimestamp');
  TextNode := XMLDoc.CreateTextNode(IntToStr(FileVersion.LastChangedTimestamp));
  NewNode.AppendChild(TextNode);
  ParentNode.AppendChild(NewNode);

  NewNode := XMLDoc.CreateElement('BackupSet');
  TextNode := XMLDoc.CreateTextNode(IntToStr(FileVersion.BackupSet));
  NewNode.AppendChild(TextNode);
  ParentNode.AppendChild(NewNode);
end;

procedure TEntry.LoadVersionsFromDOMNode(Node: TDOMNode);
var
  ParentNode: TDOMNode;
  Loop: Integer;
begin
  if TDOMElement(Node).GetElementsByTagName('Versions').Count > 0 then
  begin
    ParentNode := TDOMElement(Node).GetElementsByTagName('Versions')[0];

    SetLength(FVersions, ParentNode.ChildNodes.Count);
    for Loop := 0 to High(FVersions) do
      FVersions[Loop] := LoadVersionFromDOMNode(ParentNode.ChildNodes[Loop]);
  end;
end;

function TEntry.LoadVersionFromDOMNode(Node: TDOMNode): TFileVersion;
var
  ChildNode: TDOMNode;
  NodeLoop: Integer;
  Hex: String;
begin
  for NodeLoop := 0 to Node.ChildNodes.Count-1 do
  begin
    ChildNode := Node.ChildNodes[NodeLoop];

    case ChildNode.NodeName of
      'BackupFilename':
        Result.BackupFilename := ChildNode.FirstChild.NodeValue;
      'Size':
        Result.Size := StrToInt(ChildNode.FirstChild.NodeValue);
      'SizeCompressed':
        Result.SizeCompressed := StrToInt(ChildNode.FirstChild.NodeValue);
      'SHA512':
        begin
          Hex := ChildNode.FirstChild.NodeValue;
          HexToBin(@Hex[1], @Result.SHA512, Length(Result.SHA512));
        end;
      'SHA512Compressed':
        begin
          Hex := ChildNode.FirstChild.NodeValue;
          HexToBin(@Hex[1], @Result.SHA512Compressed, Length(Result.
            SHA512Compressed));
        end;
      'LastChangedTimestamp':
        Result.LastChangedTimestamp := StrToInt(ChildNode.FirstChild.NodeValue);
      'BackupSet':
        Result.BackupSet := StrToInt(ChildNode.FirstChild.NodeValue);
    end;
  end;
end;

procedure TEntry.LoadVersionsFromStream(Stream: TStream);
var
  Count: Word;
  Loop: Integer;
begin
  Stream.ReadBuffer(Count, SizeOf(Count));
  SetLength(FVersions, Count);
  for Loop := 0 to High(FVersions) do
  begin
    Stream.ReadBuffer(Count, SizeOf(Count));
    SetLength(FVersions[Loop].BackupFilename, Count);
    Stream.ReadBuffer(FVersions[Loop].BackupFilename[1], Count);
    Stream.ReadBuffer(FVersions[Loop].Size, SizeOf(FVersions[Loop].Size));
    Stream.ReadBuffer(FVersions[Loop].SizeCompressed, SizeOf(FVersions[Loop].
      SizeCompressed));
    Stream.ReadBuffer(FVersions[Loop].SHA512, SizeOf(FVersions[Loop].SHA512));
    Stream.ReadBuffer(FVersions[Loop].SHA512Compressed, SizeOf(FVersions[Loop].
      SHA512Compressed));
    Stream.ReadBuffer(FVersions[Loop].LastChangedTimestamp,
      SizeOf(FVersions[Loop].LastChangedTimestamp));
    Stream.ReadBuffer(FVersions[Loop].BackupSet, SizeOf(FVersions[Loop].
      BackupSet));
  end;
end;

procedure TEntry.RemoveChild(Child: TEntry);
var
  Idx: Integer;
begin
  Idx := FEntries.FItems.IndexOf(Child);
  if Idx <> -1 then
    FEntries.FItems.Remove(Child);
end;

constructor TEntry.Create(AParent: TEntry);
begin
  FParent := AParent;
  FEntries := TEntries.Create(Self);
  FName := '';
  FEntryType := etUnspecified;
end;

destructor TEntry.Destroy;
begin
  FEntries.Free;
  inherited Destroy;
end;

function TEntry.AddChild: TEntry;
begin
  Result := FEntries.Add;
end;

function TEntry.AddNewVersion(BackupFilename: String; Size,
  CompressedSize: Int64; LastChangedTimestamp: Int64; SHA512,
  CompressedSHA512: TSHA512Hash; BackupSet: Word): Integer;
var
  Index: Integer;
begin
  SetLength(FVersions, Length(FVersions)+1);
  Index := High(FVersions);
  FVersions[Index].BackupFilename := BackupFilename;
  FVersions[Index].Size := Size;
  FVersions[Index].SizeCompressed := CompressedSize;
  FVersions[Index].LastChangedTimestamp := LastChangedTimestamp;
  FVersions[Index].SHA512 := SHA512;
  FVersions[Index].SHA512Compressed := CompressedSHA512;
  FVersions[Index].BackupSet := BackupSet;
  Result := Index;
end;

function TEntry.AggregateFileCount: Integer;
begin
  case FEntryType of
    etDirectory: Result := FEntries.AggergateFileCount;
    etFile: Result := 1;
    etUnspecified: Result := 0;
  end;
end;

function TEntry.AggregateFilename: String;
begin
  if FParent <> nil then
    Result := IncludeTrailingPathDelimiter(FParent.AggregateFilename) + FName
  else
    Result := FName;
end;

function TEntry.AggregateSize: Int64;
begin
  case FEntryType of
    etDirectory: Result := FEntries.AggregateSize;
    else
      Result := 0;
  end;
end;

function TEntry.FileExists(Filename: String): Boolean;
var
  FileLoop: Integer;
begin
  if FEntryType <> etDirectory then
    raise Exception.Create('FileExists method is only allowed for directories');

  Result := False;
  for FileLoop := 0 to FEntries.ItemCount-1 do
    if FEntries[FileLoop].FName = Filename then
    begin
      Result := True;
      Break;
    end;
end;

procedure TEntry.Remove;
begin
  if FParent <> nil then
    FParent.RemoveChild(Self);
end;

procedure TEntry.SetCurrentVersionSHA512(Hash: TSHA512Hash);
begin
  FVersions[High(FVersions)].SHA512 := Hash;
end;

procedure TEntry.SaveToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
begin
  SaveNameToDOMNode(XMLDoc, Node);
  SaveFlagsToDOMNode(XMLDoc, Node);
  SaveVersionsToDOMNode(XMLDoc, Node);
  FEntries.SaveToDOMNode(XMLDoc, Node);
end;

procedure TEntry.SaveToStream(Stream: TStream);
begin
  SaveNameToStream(Stream);
  SaveFlagsToStream(Stream);
  SaveVersionsToStream(Stream);
  FEntries.SaveToStream(Stream);
end;

procedure TEntry.LoadFromDOMNode(Node: TDOMNode);
begin
  LoadNameFromDOMNode(Node);
  LoadFlagsFromDOMNode(Node);
  LoadVersionsFromDOMNode(Node);
  FEntries.LoadFromDOMNode(Node);
end;

procedure TEntry.LoadFromStream(Stream: TStream);
begin
  LoadNameFromStream(Stream);
  LoadFlagsFromStream(Stream);
  LoadVersionsFromStream(Stream);
  FEntries.LoadFromStream(Stream);
end;

end.

