unit filecatalogue_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, filecatalogue, laz2_DOM;

type

  { TFileCatalogueTest }

  TFileCatalogueTest = class(TTestCase)
  protected
    FRootEntry: TEntry;
    FMemoryStream: TMemoryStream;
    FXMLDoc: TXMLDocument;
    FRootDOMNode: TDOMNode;
    FakeFileException: TEntry;

    procedure SetUpOnce; override;
    procedure TearDownOnce; override;

    procedure TryGetCurrentVersion;
    procedure AddFakeFile2Twice;
    procedure RemoveExceptionEntry;
    procedure AddFileAndCheckFileExistsEception;
  published
    procedure ChecksAfterSetup;
    procedure InitializeRootDirectory;
    procedure AddFakeFiles;
    procedure AddFileVersionsToFile1;
    procedure CheckFirstVersionOfFile1;

    procedure SaveRootToStreamAndFreeRoot;
    procedure LoadRootFromStream;
    procedure CheckFilesAfterLoadingFromStream;

    procedure SaveRootToDOMNodeAndFreeRoot;
    procedure LoadRootFromDOMNode;
    procedure CheckFilesAfterLoadingFromDOMNode;
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
  FMemoryStream := TMemoryStream.Create;
  FXMLDoc := TXMLDocument.Create;
  FRootDOMNode := FXMLDoc.CreateElement('root');
  FXMLDoc.AppendChild(FRootDOMNode);
end;

procedure TFileCatalogueTest.TearDownOnce;
begin
  FRootEntry.Free;
  FMemoryStream.Free;
  FRootDOMNode.Free;
  FXMLDoc.Free;
end;

procedure TFileCatalogueTest.TryGetCurrentVersion;
begin
  FRootEntry.CurrentVersion;
end;

procedure TFileCatalogueTest.AddFakeFile2Twice;
begin
  FakeFileException := FRootEntry.AddChild;
  FakeFileException.Name := 'FakeFile2';
  FakeFileException.isDirectory := False;
end;

procedure TFileCatalogueTest.RemoveExceptionEntry;
begin
  FakeFileException.Remove;
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

  CheckEquals(3, FRootEntry.Childs.ItemCount, 'Unexpected child item count');
  RemoveExceptionEntry;
  CheckEquals(2, FRootEntry.Childs.ItemCount, 'Unexpected child item count');

  CheckException(@AddFileAndCheckFileExistsEception, Exception,
    'Expected exception while calling FileExists on a file');
  CheckEquals(3, FRootEntry.AggregateFileCount, 'Unexpected file count');
  CheckEquals(0, FRootEntry.AggregateSize, 'Unexpected aggregate size');
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
  CheckEquals(3, FRootEntry.AggregateFileCount, 'Unexpected file count');

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

  CheckEquals(6, FRootEntry.AggregateSize, 'Unexpected aggregate size');
  CheckEquals(3, FRootEntry.AggregateFileCount, 'Unexpected file count');
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

procedure TFileCatalogueTest.SaveRootToStreamAndFreeRoot;
begin
  FMemoryStream.Clear;
  FRootEntry.SaveToStream(FMemoryStream);
  FRootEntry.Free;
  FRootEntry := nil;
  CheckNotEquals(0, FMemoryStream.Size, 'Unexpected stream size');
end;

procedure TFileCatalogueTest.LoadRootFromStream;
begin
  Check(not Assigned(FRootEntry), 'Unexpected assigned root entry');
  FRootEntry := TEntry.Create(nil);
  CheckEquals(0, FRootEntry.AggregateFileCount);
  FMemoryStream.Position := 0;
  FRootEntry.LoadFromStream(FMemoryStream);

  CheckEquals('/', FRootEntry.Name, 'Unexpected root entry name');
  CheckEquals(3, FRootEntry.AggregateFileCount,
    'Unexpected aggregate file count');
  CheckEquals(6, FRootEntry.AggregateSize, 'Unexpected aggregate size');
end;

procedure TFileCatalogueTest.CheckFilesAfterLoadingFromStream;
var
  File1, File2: TEntry;
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


  File2 := FRootEntry.Childs[1];

  CheckEquals('FakeFile2', File2.Name, 'Unexpected filename');
  CheckEquals(0, File2.VersionCount, 'Unexpected version count');
end;

procedure TFileCatalogueTest.SaveRootToDOMNodeAndFreeRoot;
begin
  FRootEntry.SaveToDOMNode(FXMLDoc, FRootDOMNode);
  FRootEntry.Free;
  FRootEntry := nil;
  Check(FXMLDoc.FirstChild <> nil, 'XML root node has no child after saving');
end;

procedure TFileCatalogueTest.LoadRootFromDOMNode;
begin
  Check(not Assigned(FRootEntry), 'Unexpected assigned root entry');
  FRootEntry := TEntry.Create(nil);
  CheckEquals(0, FRootEntry.AggregateFileCount);
  FRootEntry.LoadFromDOMNode(FRootDOMNode);

  CheckEquals('/', FRootEntry.Name, 'Unexpected root entry name');
  CheckEquals(3, FRootEntry.AggregateFileCount,
    'Unexpected aggregate file count');
  CheckEquals(6, FRootEntry.AggregateSize, 'Unexpected aggregate size');
end;

procedure TFileCatalogueTest.CheckFilesAfterLoadingFromDOMNode;
var
  File1, File2: TEntry;
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


  File2 := FRootEntry.Childs[1];

  CheckEquals('FakeFile2', File2.Name, 'Unexpected filename');
  CheckEquals(0, File2.VersionCount, 'Unexpected version count');
end;

end.

