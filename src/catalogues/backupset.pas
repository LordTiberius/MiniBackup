unit BackupSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM;

type

  { TBackupSet }

  TBackupSet = class
  private
    function GetExclusionRule(Index: Integer): String;
    function GetExclusionRuleCount: Integer;
    function GetInclusionRule(Index: Integer): String;
    function GetInclusionRuleCount: Integer;
    procedure SetExclusionRule(Index: Integer; AValue: String);
    procedure SetInclusionRule(Index: Integer; AValue: String);
  protected
    FSourcePath, FDestinationPath: String;
    FExclusionRules, FInclusionRules: TStringList;

    procedure CheckExclusionRulesIndexBounds(Index: Integer);
    procedure CheckInclusionRulesIndexBounds(Index: Integer);

    procedure ReadRulesFromDOMNode(Node: TDOMNode; var Rules: TStringList);
    procedure ReadRulesFromStream(Stream: TStream; var Rules: TStringList);
    procedure WriteRulesToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode;
      Rules: TStrings);
    procedure WriteRulesToStream(Stream: TStream; Rules: TStrings);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function AddExclusionRule(Rule: String): Integer;
    function AddInclusionRule(Rule: String): Integer;
    property DestinationPath: String read FDestinationPath
      write FDestinationPath;
    function DeleteExclusionRule(Rule: String): Boolean;
    function DeleteExclusionRule(RuleIndex: Integer): Boolean;
    function DeleteInclusionRule(Rule: String): Boolean;
    function DeleteInclusionRule(RuleIndex: Integer): Boolean;
    function ExclusionRuleExists(Rule: String): Boolean;
    function ExclusionRuleIndex(Rule: String): Integer;
    property ExclusionRuleCount: Integer read GetExclusionRuleCount;
    property ExclusionRules[Index: Integer]: String read GetExclusionRule
      write SetExclusionRule;
    property InclusionRuleCount: Integer read GetInclusionRuleCount;
    function InclusionRuleExists(Rule: String): Boolean;
    function InclusionRuleIndex(Rule: String): Integer;
    property InclusionRules[Index: Integer]: String read GetInclusionRule
      write SetInclusionRule;
    procedure ReadFromDOMNode(Node: TDOMNode);
    procedure ReadFromStream(Stream: TStream);
    procedure SaveToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
    procedure SaveToStream(Stream: TStream);
    property SourcePath: String read FSourcePath write FSourcePath;
  end;

implementation

{ TBackupSet }

function TBackupSet.GetExclusionRule(Index: Integer): String;
begin
  CheckExclusionRulesIndexBounds(Index);
  Result := FExclusionRules[Index];
end;

function TBackupSet.GetExclusionRuleCount: Integer;
begin
  Result := FExclusionRules.Count;
end;

function TBackupSet.GetInclusionRule(Index: Integer): String;
begin
  CheckInclusionRulesIndexBounds(Index);
  Result := FInclusionRules[Index];
end;

function TBackupSet.GetInclusionRuleCount: Integer;
begin
  Result := FInclusionRules.Count;
end;

procedure TBackupSet.SetExclusionRule(Index: Integer; AValue: String);
begin
  CheckExclusionRulesIndexBounds(Index);
  FExclusionRules[Index] := AValue;
end;

procedure TBackupSet.SetInclusionRule(Index: Integer; AValue: String);
begin
  CheckInclusionRulesIndexBounds(Index);
  FInclusionRules[Index] := AValue;
end;

procedure TBackupSet.CheckExclusionRulesIndexBounds(Index: Integer);
begin
  if (Index < 0) or (Index > FExclusionRules.Count-1) then
    raise Exception.CreateFmt('ExclusionRules: Index (%d) out of bounds',
      [Index]);
end;

procedure TBackupSet.CheckInclusionRulesIndexBounds(Index: Integer);
begin
  if (Index < 0) or (Index > FInclusionRules.Count-1) then
    raise Exception.CreateFmt('InclusionRules: Index (%d) out of bounds',
      [Index]);
end;

procedure TBackupSet.ReadRulesFromDOMNode(Node: TDOMNode; var Rules: TStringList
  );
var
  ChildLoop: Integer;
  ChildNode: TDOMNode;
begin
  for ChildLoop := 0 to Node.ChildNodes.Count-1 do
  begin
    ChildNode := Node.ChildNodes.Item[ChildLoop];
    case ChildNode.NodeName of
      'Rule':
        Rules.Add(ChildNode.NodeValue);
    end;
  end;
end;

procedure TBackupSet.ReadRulesFromStream(Stream: TStream; var Rules: TStringList
  );
var
  RuleLoop: Integer;
  RuleCount, RuleLen: Word;
  Rule: String;
begin
  Stream.ReadBuffer(RuleCount, SizeOf(RuleCount));
  for RuleLoop := 0 to RuleCount-1 do
  begin
    Stream.ReadBuffer(RuleLen, SizeOf(RuleLen));
    SetLength(Rule, RuleLen);
    Stream.ReadBuffer(Rule[1], RuleLen);
    Rules.Add(Rule);
  end;
end;

procedure TBackupSet.WriteRulesToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode;
  Rules: TStrings);
var
  RuleLoop: Integer;
  NewNode, TextNode: TDOMNode;
begin
  for RuleLoop := 0 to Rules.Count-1 do
  begin
    NewNode := XMLDoc.CreateElement('Rule');
    TextNode := XMLDoc.CreateTextNode(Rules[RuleLoop]);

    NewNode.AppendChild(TextNode);
    Node.AppendChild(NewNode);
  end;
end;

procedure TBackupSet.WriteRulesToStream(Stream: TStream; Rules: TStrings);
var
  RuleLoop: Integer;
  RuleCount, RuleLen: Word;
begin
  if Rules.Count > High(RuleCount) then
    raise Exception.CreateFmt('Rule count out of range', [Rules.Count]);

  RuleCount := Rules.Count;
  Stream.WriteBuffer(RuleCount, SizeOf(RuleCount));
  for RuleLoop := 0 to RuleCount-1 do
  begin
    RuleLen := Length(Rules[RuleLoop]);
    Stream.WriteBuffer(RuleLen, SizeOf(RuleLen));
    Stream.WriteBuffer(Rules[RuleLoop][1], RuleLen);
  end;
end;

constructor TBackupSet.Create;
begin
  FInclusionRules := TStringList.Create;
  FExclusionRules := TStringList.Create;
end;

destructor TBackupSet.Destroy;
begin
  FInclusionRules.Free;
  FExclusionRules.Free;
  inherited Destroy;
end;

function TBackupSet.AddExclusionRule(Rule: String): Integer;
begin
  if not ExclusionRuleExists(Rule) then
    Result := FExclusionRules.Add(Rule)
  else
    Result := -1;
end;

function TBackupSet.AddInclusionRule(Rule: String): Integer;
begin
  if not InclusionRuleExists(Rule) then
    Result := FInclusionRules.Add(Rule)
  else
    Result := -1;
end;

function TBackupSet.ExclusionRuleExists(Rule: String): Boolean;
begin
  Result := (ExclusionRuleIndex(Rule) > -1);
end;

function TBackupSet.ExclusionRuleIndex(Rule: String): Integer;
begin
  Result := FExclusionRules.IndexOf(Rule);
end;

function TBackupSet.InclusionRuleExists(Rule: String): Boolean;
begin
  Result := (InclusionRuleIndex(Rule) > -1);
end;

function TBackupSet.InclusionRuleIndex(Rule: String): Integer;
begin
  Result := FInclusionRules.IndexOf(Rule);
end;

procedure TBackupSet.ReadFromDOMNode(Node: TDOMNode);
var
  ChildLoop: Integer;
  ChildNode: TDOMNode;
begin
  for ChildLoop := 0 to Node.ChildNodes.Count-1 do
  begin
    ChildNode := Node.ChildNodes.Item[ChildLoop];
    case ChildNode.NodeName of
      'DestinationPath':
        FDestinationPath := ChildNode.NodeValue;
      'SourcePath':
        FSourcePath := ChildNode.NodeValue;
      'ExclusionRules':
        ReadRulesFromDOMNode(Node, FExclusionRules);
      'InclusionRules':
        ReadRulesFromDOMNode(Node, FInclusionRules);
    end;
  end;
end;

procedure TBackupSet.ReadFromStream(Stream: TStream);
var
  StringLen: Word;
begin
  Stream.ReadBuffer(StringLen, SizeOf(StringLen));
  SetLength(FDestinationPath, StringLen);
  Stream.ReadBuffer(FDestinationPath[1], StringLen);

  Stream.ReadBuffer(StringLen, SizeOf(StringLen));
  SetLength(FSourcePath, StringLen);
  Stream.ReadBuffer(FSourcePath[1], StringLen);

  ReadRulesFromStream(Stream, FExclusionRules);
  ReadRulesFromStream(Stream, FInclusionRules);
end;

procedure TBackupSet.SaveToDOMNode(XMLDoc: TXMLDocument; Node: TDOMNode);
var
  NewNode, TextNode: TDOMNode;
begin
  NewNode := XMLDoc.CreateElement('DestinationPath');
  TextNode := XMLDoc.CreateTextNode(FDestinationPath);
  NewNode.AppendChild(TextNode);
  Node.AppendChild(NewNode);

  NewNode := XMLDoc.CreateElement('SourcePath');
  TextNode := XMLDoc.CreateTextNode(FSourcePath);
  NewNode.AppendChild(TextNode);
  Node.AppendChild(NewNode);

  NewNode := XMLDoc.CreateElement('ExclusionRules');
  WriteRulesToDOMNode(XMLDoc, NewNode, FExclusionRules);
  Node.AppendChild(NewNode);

  NewNode := XMLDoc.CreateElement('InclusionRules');
  WriteRulesToDOMNode(XMLDoc, NewNode, FInclusionRules);
  Node.AppendChild(NewNode);
end;

procedure TBackupSet.SaveToStream(Stream: TStream);
var
  StringLen: Word;
begin
  StringLen := Length(FDestinationPath);
  Stream.WriteBuffer(StringLen, SizeOf(StringLen));
  Stream.WriteBuffer(FDestinationPath[1], StringLen);

  StringLen := Length(FSourcePath);
  Stream.WriteBuffer(StringLen, SizeOf(StringLen));
  Stream.WriteBuffer(FSourcePath[1], StringLen);

  WriteRulesToStream(Stream, FExclusionRules);
  WriteRulesToStream(Stream, FInclusionRules);
end;

function TBackupSet.DeleteExclusionRule(Rule: String): Boolean;
var
  RuleIndex: Integer;
begin
  RuleIndex := ExclusionRuleIndex(Rule);
  if RuleIndex > -1 then
  begin
    FExclusionRules.Delete(RuleIndex);
    Result := True;
  end
  else
    Result := False;
end;

function TBackupSet.DeleteExclusionRule(RuleIndex: Integer): Boolean;
begin
  CheckExclusionRulesIndexBounds(RuleIndex);
  FExclusionRules.Delete(RuleIndex);
  Result := True;
end;

function TBackupSet.DeleteInclusionRule(Rule: String): Boolean;
var
  RuleIndex: Integer;
begin
  RuleIndex := InclusionRuleIndex(Rule);
  if RuleIndex > -1 then
  begin
    FInclusionRules.Delete(RuleIndex);
    Result := True;
  end
  else
    Result := False;
end;

function TBackupSet.DeleteInclusionRule(RuleIndex: Integer): Boolean;
begin
  CheckInclusionRulesIndexBounds(RuleIndex);
  FInclusionRules.Delete(RuleIndex);
  Result := True;
end;

end.

