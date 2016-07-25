unit filecatalogue_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, filecatalogue;

type

  { TFileCatalogueTest }

  TFileCatalogueTest = class(TTestCase)
  protected
    FRootEntry: TEntry;
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;

    procedure TryGetCurrentVersion;
    procedure AddFakeFile2Twice;
  published
    procedure ChecksAfterSetup;
    procedure InitializeRootDirectory;
    procedure AddFakeFiles;
  end;

implementation

{ TFileCatalogueTest }


procedure TFileCatalogueTest.SetUpOnce;
begin
  FRootEntry := TEntry.Create(nil);
end;

procedure TFileCatalogueTest.TearDownOnce;
begin
  FRootEntry.Free;
end;

procedure TFileCatalogueTest.TryGetCurrentVersion;
begin
  FRootEntry.CurrentVersion;
end;

procedure TFileCatalogueTest.AddFakeFile2Twice;
var
  FakeFile: TEntry;
begin
  FakeFile := FRootEntry.AddChild;
  FakeFile.Name := 'FakeFile2';
  FakeFile.isDirectory := False;
end;

procedure TFileCatalogueTest.ChecksAfterSetup;
begin
  Check(not FRootEntry.hasChilds, 'hasChilds after setup');
  Check(not FRootEntry.isDirectory, 'isDirectory after setup');
  CheckEquals(0, FRootEntry.VersionCount, 'Not empty VersionCount');
  CheckException(@TryGetCurrentVersion, Exception, 'Expected exception while' +
    'obtaining current version');
  CheckEquals(0, FRootEntry.AggregateSize, 'Unexpected aggregate size');
  CheckEquals(0, FRootEntry.AggregateFileCount, 'Unexpected file count is');
end;

procedure TFileCatalogueTest.InitializeRootDirectory;
begin
  FRootEntry.Name := '/';
  FRootEntry.isDirectory := True;
  CheckEquals('/', FRootEntry.Name, 'Unexpected root entry name');
  CheckEquals(True, FRootEntry.isDirectory, 'Root entry is not a directory');
end;

procedure TFileCatalogueTest.AddFakeFiles;
var
  FakeFile: TEntry;
begin
  FakeFile := FRootEntry.AddChild;
  FakeFile.Name := 'FakeFile1';
  FakeFile.isDirectory := False;

  CheckEquals(1, FRootEntry.AggregateFileCount, 'Unexpected file count');
  CheckEquals('/FakeFile1', FakeFile.AggregateFilename,
    'Unexpected aggregate filename');


  FakeFile := FRootEntry.AddChild;
  FakeFile.Name := 'FakeFile2';
  FakeFile.isDirectory := False;

  CheckEquals(2, FRootEntry.AggregateFileCount, 'Unexpected file count');
  CheckEquals('/FakeFile2', FakeFile.AggregateFilename,
    'Unexpected aggregate filename');

  CheckException(@AddFakeFile2Twice, Exception, 'Expected exception while ' +
    'adding a file twice');
end;

end.

