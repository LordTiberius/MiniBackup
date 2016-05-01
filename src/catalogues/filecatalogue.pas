unit FileCatalogue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPsha512, DCPmd5;

type
  TSHA512Hash = array[0..63] of Byte;

  TFileVersion = record
    BackupFilename: String;
    Size, SizeCompressed: Int64;
    LastChangedTimestamp: Int64;
    SHA512, SHA512Compressed: TSHA512Hash;
    BackupSet: Word;
  end;

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

    procedure SaveToStream(Stream: TStream);
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

    procedure SaveName(Stream: TStream);
    procedure LoadName(Stream: TStream);

    procedure SaveFlags(Stream: TStream);
    procedure LoadFlags(Stream: TStream);

    procedure SaveVersions(Stream: TStream);
    procedure LoadVersions(Stream: TStream);
  protected
    FEntries: TEntries;
    FName: String;
    FParent: TEntry;
    FisDirectory: Boolean;
    FVersions: array of TFileVersion;
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

    procedure SetCurrentVersionSHA512(Hash: TSHA512Hash);

    procedure SaveToStream(Stream: TStream);
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

implementation

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
  Result := FisDirectory;
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
  FisDirectory := AValue;
end;

procedure TEntry.SetName(AValue: String);
begin
  FName := AValue;
end;

procedure TEntry.SaveName(Stream: TStream);
var
  Len: Word;
begin
  Len := Length(FName);
  Stream.WriteBuffer(Len, SizeOf(Len));
  Stream.WriteBuffer(FName[1], Len);
end;

procedure TEntry.LoadName(Stream: TStream);
var
  Len: Word;
begin
  Stream.ReadBuffer(Len, SizeOf(Len));
  SetLength(FName, Len);
  Stream.ReadBuffer(FName[1], Len);
end;

procedure TEntry.SaveFlags(Stream: TStream);
begin
  Stream.WriteBuffer(FisDirectory, SizeOf(FisDirectory));
end;

procedure TEntry.LoadFlags(Stream: TStream);
begin
  Stream.ReadBuffer(FisDirectory, SizeOf(FisDirectory));
end;

procedure TEntry.SaveVersions(Stream: TStream);
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

procedure TEntry.LoadVersions(Stream: TStream);
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

constructor TEntry.Create(AParent: TEntry);
begin
  FParent := AParent;
  FEntries := TEntries.Create(Self);
  FName := '';
  FisDirectory := False;
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
  if FisDirectory then
    Result := FEntries.AggergateFileCount
  else
    Result := 1;
end;

function TEntry.AggregateFilename: String;
begin
  if FParent <> nil then
    Result := FParent.AggregateFilename + DirectorySeparator +  FName
  else
    Result := FName;
end;

function TEntry.AggregateSize: Int64;
var
  Loop: Integer;
begin
  Result := 0;
  if FisDirectory then
  begin
    Result := FEntries.AggregateSize;
  end;
end;

procedure TEntry.SetCurrentVersionSHA512(Hash: TSHA512Hash);
begin
  FVersions[High(FVersions)].SHA512 := Hash;
end;

procedure TEntry.SaveToStream(Stream: TStream);
begin
  SaveName(Stream);
  SaveFlags(Stream);
  SaveVersions(Stream);
  FEntries.SaveToStream(Stream);
end;

procedure TEntry.LoadFromStream(Stream: TStream);
begin
  LoadName(Stream);
  LoadFlags(Stream);
  LoadVersions(Stream);
  FEntries.LoadFromStream(Stream);
end;

end.

