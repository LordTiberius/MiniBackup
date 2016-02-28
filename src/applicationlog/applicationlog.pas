unit ApplicationLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, DateUtils, SysUtils, FileUtil;

type
  TEntryCount = Word;

  TLogSeverity = (lsDebug, lsInformation, lsWarning, lsError, lsException);
  TLogEntry = record
    Timestamp: Int64;
    Severity: TLogSeverity;
    LogMessage: String;
  end;

  { TApplicationLog }

  TApplicationLog = class
  private
    function AddEntry(Severity: TLogSeverity;
      LogMessage: String; Timestamp: Int64 = -1): Integer;
    function GetEntryCount: Integer;
    function GetLastEntry: TLogEntry;
    function GetLogEntry(Index: Integer): TLogEntry;
    procedure LoadEntryFromStream(var Entry: TLogEntry; Stream: TStream);
    procedure SaveEntryToStream(Entry: TLogEntry; Stream: TStream);
  protected
    FEntries: array of TLogEntry;
    FVerbosity: TLogSeverity;
    FOnNewEntry: TNotifyEvent;
  public
    constructor Create;
    procedure Debug(DebugMessage: String);
    procedure Error(ErrorMessage: String);
    procedure Exception(ExceptionMessage: String);
    procedure Information(InformationMessage: String);
    procedure LoadFromFile(Filename: String);
    procedure SaveToFile(Filename: String);
    procedure SaveToTextFile(Filename: String);
    procedure Warning(WarningMessage: String);

    property EntryCount: Integer read GetEntryCount;
    property Entries[Index: Integer]: TLogEntry read GetLogEntry;
    property LastEntry: TLogEntry read GetLastEntry;
    property Verbosity: TLogSeverity read FVerbosity write FVerbosity;
    property OnNewEntry: TNotifyEvent read FOnNewEntry write FOnNewEntry;
  end;

function SeverityToStr(LogSeverity: TLogSeverity): String;

implementation

function SeverityToStr(LogSeverity: TLogSeverity): String;
begin
  case LogSeverity of
    lsDebug: Result := 'Debug';
    lsInformation: Result := 'Information';
    lsWarning: Result := 'Warning';
    lsError: Result := 'Error';
    lsException: Result := 'Exception';
  end;
end;

{ TApplicationLog }

function TApplicationLog.GetEntryCount: Integer;
begin
  Result := Length(FEntries);
end;

function TApplicationLog.GetLastEntry: TLogEntry;
begin
  if Length(FEntries) = 0 then
    raise SysUtils.Exception.Create('Last entry not available');

  Result := FEntries[High(FEntries)];
end;

function TApplicationLog.GetLogEntry(Index: Integer): TLogEntry;
begin
  if (Index < Low(FEntries))
    or (Index > High(FEntries)) then
    raise SysUtils.Exception.Create('Entry index out of bounds');

  Result  := FEntries[Index];
end;

procedure TApplicationLog.LoadEntryFromStream(var Entry: TLogEntry;
  Stream: TStream);
var
  Count: Word;
begin
  Stream.ReadBuffer(Entry.Timestamp, SizeOf(Entry.Timestamp));
  Stream.ReadBuffer(Entry.Severity, SizeOf(Entry.Severity));
  Stream.ReadBuffer(Count, SizeOf(Count));
  SetLength(Entry.LogMessage, Count);
  Stream.ReadBuffer(Entry.LogMessage[1], Count);
end;

procedure TApplicationLog.SaveEntryToStream(Entry: TLogEntry; Stream: TStream);
var
  Count: Word;
begin
  Stream.WriteBuffer(Entry.Timestamp, SizeOf(Entry.Timestamp));
  Stream.WriteBuffer(Entry.Severity, SizeOf(Entry.Severity));
  Count := Length(Entry.LogMessage);
  Stream.WriteBuffer(Count, SizeOf(Count));
  Stream.WriteBuffer(Entry.LogMessage[1], Count);
end;

constructor TApplicationLog.Create;
begin
  FVerbosity := lsInformation;
end;

function TApplicationLog.AddEntry(Severity: TLogSeverity; LogMessage: String;
  Timestamp: Int64): Integer;
begin
  SetLength(FEntries, Length(FEntries)+1);

  if Timestamp = -1 then
    FEntries[High(FEntries)].Timestamp := DateTimeToUnix(TDateTime(now))
  else
    FEntries[High(FEntries)].Timestamp := Timestamp;

  FEntries[High(FEntries)].Severity := Severity;
  FEntries[High(FEntries)].LogMessage := LogMessage;

  Result := High(FEntries);

  if Assigned(FOnNewEntry) then
    FOnNewEntry(Self);
end;

procedure TApplicationLog.Debug(DebugMessage: String);
begin
  AddEntry(lsDebug, DebugMessage);
end;

procedure TApplicationLog.Error(ErrorMessage: String);
begin
  AddEntry(lsError, ErrorMessage);
end;

procedure TApplicationLog.Exception(ExceptionMessage: String);
begin
  AddEntry(lsException, ExceptionMessage);
end;

procedure TApplicationLog.Information(InformationMessage: String);
begin
  AddEntry(lsInformation, InformationMessage);
end;

procedure TApplicationLog.LoadFromFile(Filename: String);
var
  EC: TEntryCount;
  FileStream: TFileStream;
  Loop: Integer;
begin
  FileStream := TFileStream.Create(UTF8ToSys(Filename), fmCreate);
  FileStream.ReadBuffer(EC, SizeOf(EC));
  SetLength(FEntries, EC);

  for Loop := 0 to EC-1 do
  begin
    LoadEntryFromStream(FEntries[Loop], FileStream);
  end;

  FileStream.Free;
end;

procedure TApplicationLog.SaveToFile(Filename: String);
var
  EC: TEntryCount;
  FileStream: TFileStream;
  Loop: Integer;
begin
  if Length(FEntries) > High(EntryCount) then
    raise SysUtils.Exception.Create('Value overflow: More entries than ' +
      'counter can hold');

  FileStream := TFileStream.Create(UTF8ToSys(Filename), fmCreate);
  EC := Length(FEntries);
  FileStream.WriteBuffer(EC, SizeOf(EC));

  for Loop := 0 to High(FEntries) do
  begin
    SaveEntryToStream(FEntries[Loop], FileStream);
  end;

  FileStream.Free;
end;

procedure TApplicationLog.SaveToTextFile(Filename: String);
var
  Strings: TStringList;
  Loop: Integer;
begin
  Strings := TStringList.Create;
  for Loop := 0 to High(FEntries) do
  begin
    if Ord(FEntries[Loop].Severity) >= Ord(FVerbosity) then
    begin
      Strings.Add(Format('[%s] %s: %s', [DateTimeToStr(UnixToDateTime(
        FEntries[Loop].Timestamp)), SeverityToStr(FEntries[Loop].Severity),
        FEntries[Loop].LogMessage]));
    end;
  end;
  Strings.Free;
end;

procedure TApplicationLog.Warning(WarningMessage: String);
begin
  AddEntry(lsWarning, WarningMessage);
end;

end.

