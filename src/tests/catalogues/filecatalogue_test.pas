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
    procedure AddFileAndCheckFileExistsEception;
  published
    procedure ChecksAfterSetup;
    procedure InitializeRootDirectory;
    procedure AddFakeFiles;
    procedure AddFileVersionsToFile1;
    procedure CheckFirstVersionOfFile1;
  end;

implementation

const
  SHA512_EMPTY: TSHA512Hash = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  SHA512_Ver1: TSHA512Hash = (255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255);
  SHA512_Ver2: TSHA512Hash = (127, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255);
  SHA512_Ver3: TSHA512Hash = (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255);

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

procedure TFileCatalogueTest.AddFileAndCheckFileExistsEception;
var
  FakeFile: TEntry;
begin
  FakeFile := FRootEntry.AddChild;
  FakeFile.Name := 'FakeFile3';
  FakeFile.isDirectory := False;

  FakeFile.FileExists('Test');
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
  CheckEquals(0, FRootEntry.AggregateSize, 'Unexpected aggregate size');
  CheckEquals('/FakeFile2', FakeFile.AggregateFilename,
    'Unexpected aggregate filename');

  CheckException(@AddFakeFile2Twice, Exception, 'Expected exception while ' +
    'adding a file twice');

  CheckException(@AddFileAndCheckFileExistsEception, Exception,
    'Expected exception while calling FileExists on a file');

end;

procedure TFileCatalogueTest.AddFileVersionsToFile1;
var
  File1: TEntry;
begin
  File1 := FRootEntry.Childs[0];

  CheckEquals('FakeFile1', File1.Name, 'Unexpected filename');
  CheckEquals(0, File1.VersionCount, 'Unexpected version count');

  File1.AddNewVersion('BackupFake1', 2, 1, 4, SHA512_EMPTY, SHA512_Ver1, 3);

  CheckEquals(1, File1.VersionCount, 'Unexpected version count');
  CheckEquals('BackupFake1', File1.CurrentVersion.BackupFilename,
    'Unexpected backup file name in current version');
  CheckEquals(2, File1.CurrentVersion.Size,
    'Unexpected file size in current version');
  CheckEquals(1, File1.CurrentVersion.SizeCompressed,
    'Unexpected compressed file size in current version');
  CheckEquals(4, File1.CurrentVersion.LastChangedTimestamp,
    'Unexpected last changed timestamp');
  Check(File1.CurrentVersion.SHA512 = SHA512_EMPTY, 'Unexpected SHA hash');
  Check(File1.CurrentVersion.SHA512Compressed = SHA512_Ver1,
    'Unexpected compressed SHA hash');
  CheckEquals(3, File1.CurrentVersion.BackupSet, 'Unexpected backup set');

  CheckEquals(2, FRootEntry.AggregateSize, 'Unexpected aggregate size');

  File1.AddNewVersion('BackupFake1', 6, 5, 8, SHA512_Ver2, SHA512_Ver3, 7);

  CheckEquals(2, File1.VersionCount, 'Unexpected version count');
  CheckEquals('BackupFake1', File1.CurrentVersion.BackupFilename,
    'Unexpected backup file name in current version');
  CheckEquals(6, File1.CurrentVersion.Size,
    'Unexpected file size in current version');
  CheckEquals(5, File1.CurrentVersion.SizeCompressed,
    'Unexpected compressed file size in current version');
  CheckEquals(8, File1.CurrentVersion.LastChangedTimestamp,
    'Unexpected last changed timestamp');
  Check(File1.CurrentVersion.SHA512 = SHA512_Ver2, 'Unexpected SHA hash');
  Check(File1.CurrentVersion.SHA512Compressed = SHA512_Ver3,
    'Unexpected compressed SHA hash');
  CheckEquals(7, File1.CurrentVersion.BackupSet, 'Unexpected backup set');
end;

procedure TFileCatalogueTest.CheckFirstVersionOfFile1;
var
  File1: TEntry;
  Version1: TFileVersion;
begin
  File1 := FRootEntry.Childs[0];

  CheckEquals('FakeFile1', File1.Name, 'Unexpected filename');
  CheckEquals(2, File1.VersionCount, 'Unexpected version count');

  Version1 := File1.Versions[0];

  CheckEquals('BackupFake1', Version1.BackupFilename,
    'Unexpected backup file name in current version');
  CheckEquals(2, Version1.Size, 'Unexpected file size in current version');
  CheckEquals(1, Version1.SizeCompressed,
    'Unexpected compressed file size in current version');
  CheckEquals(4, Version1.LastChangedTimestamp,
    'Unexpected last changed timestamp');
  Check(Version1.SHA512 = SHA512_EMPTY, 'Unexpected SHA hash');
  Check(Version1.SHA512Compressed = SHA512_Ver1,
    'Unexpected compressed SHA hash');
  CheckEquals(3, Version1.BackupSet, 'Unexpected backup set');
end;

end.

