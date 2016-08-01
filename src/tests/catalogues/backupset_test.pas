unit BackupSet_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, BackupSet, laz2_DOM, laz2_XMLWrite;

type

  { TBackupSetTest }

  TBackupSetTest = class(TTestCase)
  protected
    FBackupSet: TBackupSet;
    FMemoryStream: TMemoryStream;
    FXMLDoc: TXMLDocument;
    FRootDOMNode: TDOMNode;

    procedure SetUpOnce; override;
    procedure TearDownOnce; override;

    procedure TestInclusionRuleBoundsException;
    procedure TestExclusionRuleBoundsException;
  published
    procedure CheckInitialValuesAfterSetup;
    procedure CheckSourceDestPaths;
    procedure CheckInclusionRules;
    procedure CheckExclusionRules;

    procedure SaveSetToStreamAndFreeRoot;
    procedure LoadSetFromStream;
    procedure CheckRulesAfterLoadingFromStream;

    procedure SaveSetToDOMNodeAndFreeRoot;
    procedure LoadSetFromDOMNode;
    procedure CheckRulesAfterLoadingFromDOMNode;
  end;

implementation

{ TBackupSetTest }

procedure TBackupSetTest.SetUpOnce;
begin
  FBackupSet := TBackupSet.Create;
  FMemoryStream := TMemoryStream.Create;
  FXMLDoc := TXMLDocument.Create;
  FRootDOMNode := FXMLDoc.CreateElement('root');
  FXMLDoc.AppendChild(FRootDOMNode);
end;

procedure TBackupSetTest.TearDownOnce;
begin
  FBackupSet.Free;
  FMemoryStream.Free;
  FRootDOMNode.Free;
  FXMLDoc.Free;
end;

procedure TBackupSetTest.TestInclusionRuleBoundsException;
begin
  FBackupSet.InclusionRules[0];
end;

procedure TBackupSetTest.TestExclusionRuleBoundsException;
begin
  FBackupSet.ExclusionRules[0];
end;

procedure TBackupSetTest.CheckInitialValuesAfterSetup;
begin
  CheckEquals('', FBackupSet.SourcePath, 'Wrong initial source path');
  CheckEquals('', FBackupSet.DestinationPath, 'Wrong initial destination path');
  CheckEquals(0, FBackupSet.InclusionRuleCount, 'Inclusion rules not empty');
  CheckEquals(0, FBackupSet.ExclusionRuleCount, 'Exclusion rules not empty');
  CheckException(@TestInclusionRuleBoundsException, Exception,
    'Exception for inclusion rule boundary violation expected');
  CheckException(@TestExclusionRuleBoundsException, Exception,
    'Exception for exclusion rule boundary violation expected');
end;

procedure TBackupSetTest.CheckSourceDestPaths;
begin
  FBackupSet.SourcePath := '/Test for/Source/Path';
  CheckEquals('/Test for/Source/Path', FBackupSet.SourcePath,
    'Wrong source path');

  FBackupSet.DestinationPath :=
    '/and\ a\ test\ for/Destination\ Path/with\ äüß';
  CheckEquals('/and\ a\ test\ for/Destination\ Path/with\ äüß',
    FBackupSet.DestinationPath, 'Wrong destination path');
end;

procedure TBackupSetTest.CheckInclusionRules;
var
  intRuleIndex: Integer;
begin
  // Test for adding a rule
  intRuleIndex := FBackupSet.AddInclusionRule('Rule1');
  CheckEquals(0, intRuleIndex, 'Wrong expected rule index');
  CheckEquals('Rule1', FBackupSet.InclusionRules[0], 'Wrong inclusion rule');
  CheckEquals(True, FBackupSet.InclusionRuleExists('Rule1'));
  CheckEquals(0, FBackupSet.InclusionRuleIndex('Rule1'));
  CheckEquals(1, FBackupSet.InclusionRuleCount);

  // Test for checking a non-existing rule
  CheckEquals(-1, FBackupSet.InclusionRuleIndex('NotExisting'),
    'Wrong index for non-existing inclusion rule');
  CheckEquals(False, FBackupSet.InclusionRuleExists('NotExisting'),
    'Wrong result for inclusion rule existance check');
  CheckEquals(1, FBackupSet.InclusionRuleCount);

  // Test for adding an existing rule
  CheckEquals(-1, FBackupSet.AddInclusionRule('Rule1'),
    'Wrong expected rule index for adding an existing rule');
  CheckEquals(1, FBackupSet.InclusionRuleCount);
end;

procedure TBackupSetTest.CheckExclusionRules;
var
  intRuleIndex: Integer;
begin
  // Test for adding a rule
  intRuleIndex := FBackupSet.AddExclusionRule('ExRule1');
  CheckEquals(0, intRuleIndex, 'Wrong expected rule index');
  CheckEquals('ExRule1', FBackupSet.ExclusionRules[0], 'Wrong exclusion rule');
  CheckEquals(True, FBackupSet.ExclusionRuleExists('ExRule1'));
  CheckEquals(0, FBackupSet.ExclusionRuleIndex('ExRule1'));
  CheckEquals(1, FBackupSet.ExclusionRuleCount);

  // Test for checking a non-existing rule
  CheckEquals(-1, FBackupSet.ExclusionRuleIndex('NotExisting'),
    'Wrong index for non-existing inclusion rule');
  CheckEquals(False, FBackupSet.ExclusionRuleExists('NotExisting'),
    'Wrong result for inclusion rule exishttp://enobysoft.de.vutance check');
  CheckEquals(1, FBackupSet.ExclusionRuleCount);

  // Test for adding an existing rule
  CheckEquals(-1, FBackupSet.AddExclusionRule('ExRule1'),
    'Wrong expected rule index for adding an existing rule');
  CheckEquals(1, FBackupSet.ExclusionRuleCount);
end;

procedure TBackupSetTest.SaveSetToStreamAndFreeRoot;
begin
  FMemoryStream.Clear;
  FBackupSet.SaveToStream(FMemoryStream);
  FBackupSet.Free;
  FBackupSet := nil;
  CheckNotEquals(0, FMemoryStream.Size, 'Unexpected stream size');
end;

procedure TBackupSetTest.LoadSetFromStream;
begin
  Check(not Assigned(FBackupSet), 'Unexpected assigned backup set');
  FBackupSet := TBackupSet.Create;
  CheckEquals(0, FBackupSet.InclusionRuleCount);
  CheckEquals(0, FBackupSet.ExclusionRuleCount);
  FMemoryStream.Position := 0;
  FBackupSet.ReadFromStream(FMemoryStream);

  CheckEquals('/Test for/Source/Path', FBackupSet.SourcePath,
    'Wrong source path');
  CheckEquals('/and\ a\ test\ for/Destination\ Path/with\ äüß',
    FBackupSet.DestinationPath, 'Wrong destination path');
  CheckEquals(1, FBackupSet.InclusionRuleCount);
  CheckEquals(1, FBackupSet.ExclusionRuleCount);
end;

procedure TBackupSetTest.CheckRulesAfterLoadingFromStream;
begin
  CheckEquals('Rule1', FBackupSet.InclusionRules[0], 'Wrong inclusion rule');
  CheckEquals(True, FBackupSet.InclusionRuleExists('Rule1'));
  CheckEquals(0, FBackupSet.InclusionRuleIndex('Rule1'));

  CheckEquals('ExRule1', FBackupSet.ExclusionRules[0], 'Wrong exclusion rule');
  CheckEquals(True, FBackupSet.ExclusionRuleExists('ExRule1'));
  CheckEquals(0, FBackupSet.ExclusionRuleIndex('ExRule1'));
end;

procedure TBackupSetTest.SaveSetToDOMNodeAndFreeRoot;
begin
  FBackupSet.SaveToDOMNode(FXMLDoc, FRootDOMNode);
  FBackupSet.Free;
  FBackupSet := nil;
  Check(FXMLDoc.FirstChild <> nil, 'XML root node has no child after saving');
end;

procedure TBackupSetTest.LoadSetFromDOMNode;
begin
  Check(not Assigned(FBackupSet), 'Unexpected assigned backup set');
  FBackupSet := TBackupSet.Create;
  CheckEquals(0, FBackupSet.InclusionRuleCount);
  CheckEquals(0, FBackupSet.ExclusionRuleCount);
  FBackupSet.ReadFromDOMNode(FRootDOMNode);

  CheckEquals('/Test for/Source/Path', FBackupSet.SourcePath,
    'Wrong source path');
  CheckEquals('/and\ a\ test\ for/Destination\ Path/with\ äüß',
    FBackupSet.DestinationPath, 'Wrong destination path');
  CheckEquals(1, FBackupSet.InclusionRuleCount);
  CheckEquals(1, FBackupSet.ExclusionRuleCount);
end;

procedure TBackupSetTest.CheckRulesAfterLoadingFromDOMNode;
begin
  CheckEquals('Rule1', FBackupSet.InclusionRules[0], 'Wrong inclusion rule');
  CheckEquals(True, FBackupSet.InclusionRuleExists('Rule1'));
  CheckEquals(0, FBackupSet.InclusionRuleIndex('Rule1'));

  CheckEquals('ExRule1', FBackupSet.ExclusionRules[0], 'Wrong exclusion rule');
  CheckEquals(True, FBackupSet.ExclusionRuleExists('ExRule1'));
  CheckEquals(0, FBackupSet.ExclusionRuleIndex('ExRule1'));
end;

end.


