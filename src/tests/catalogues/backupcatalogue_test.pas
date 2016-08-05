unit BackupCatalogue_Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, BackupCatalogue, BackupSet, lazFileUtils;

type

  { TBackupCatalogueTest }

  TBackupCatalogueTest = class(TTestCase)
  protected
    FBackupCatalogue: TBackupCatalogue;

    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
    procedure LoadFromUnknownFile;
  published
    procedure ChecksAfterInitialization;
    procedure CheckAddingBackupSets;
    procedure CheckSavingToBUCFile;
    procedure CheckLoadingFromBUCFile;
    procedure CheckSavingToXMLFile;
    procedure CheckLoadingFromXMLFile;
    procedure CheckSavingToUnknownFile;
  end;

implementation

{ TBackupCatalogueTest }

procedure TBackupCatalogueTest.SetUpOnce;
begin
  FBackupCatalogue := TBackupCatalogue.Create;
end;

procedure TBackupCatalogueTest.TearDownOnce;
begin
  FBackupCatalogue.Free;
  DeleteFileUTF8('CheckBUC.buc');
  DeleteFileUTF8('CheckXML.xml');
end;

procedure TBackupCatalogueTest.LoadFromUnknownFile;
begin
  FBackupCatalogue.LoadFromFile('CheckUnknown.ukn');
end;

procedure TBackupCatalogueTest.ChecksAfterInitialization;
begin
  CheckEquals('', FBackupCatalogue.Name);
  CheckEquals(0, FBackupCatalogue.BackupSetCount);

  FBackupCatalogue.Name := 'Test Backup Catalogue';
  CheckEquals('Test Backup Catalogue', FBackupCatalogue.Name);
end;

procedure TBackupCatalogueTest.CheckAddingBackupSets;
begin
  FBackupCatalogue.AddBackupSet.SourcePath := 'SP1';
  CheckEquals(1, FBackupCatalogue.BackupSetCount);
  FBackupCatalogue.AddBackupSet.SourcePath := 'SP2';
  CheckEquals(2, FBackupCatalogue.BackupSetCount);
end;

procedure TBackupCatalogueTest.CheckSavingToBUCFile;
begin
  CheckEquals(False, FileExistsUTF8('CheckBUC.buc'));
  FBackupCatalogue.SaveToFile('CheckBUC.buc');
  FBackupCatalogue.Free;
  FBackupCatalogue := nil;
  CheckEquals(True, FileExistsUTF8('CheckBUC.buc'));
end;

procedure TBackupCatalogueTest.CheckLoadingFromBUCFile;
begin
  Check(FBackupCatalogue = nil);
  FBackupCatalogue := TBackupCatalogue.Create;
  FBackupCatalogue.LoadFromFile('CheckBUC.buc');
  CheckEquals(2, FBackupCatalogue.BackupSetCount);
  CheckEquals('SP1', FBackupCatalogue.BackupSets[0].SourcePath);
  CheckEquals('SP2', FBackupCatalogue.BackupSets[1].SourcePath);
end;

procedure TBackupCatalogueTest.CheckSavingToXMLFile;
begin
  CheckEquals(False, FileExistsUTF8('CheckXML.xml'));
  FBackupCatalogue.SaveToFile('CheckXML.xml');
  FBackupCatalogue.Free;
  FBackupCatalogue := nil;
  CheckEquals(True, FileExistsUTF8('CheckXML.xml'));
end;

procedure TBackupCatalogueTest.CheckLoadingFromXMLFile;
begin
  Check(FBackupCatalogue = nil);
  FBackupCatalogue := TBackupCatalogue.Create;
  FBackupCatalogue.LoadFromFile('CheckXML.xml');
  CheckEquals(2, FBackupCatalogue.BackupSetCount);
  CheckEquals('SP1', FBackupCatalogue.BackupSets[0].SourcePath);
  CheckEquals('SP2', FBackupCatalogue.BackupSets[1].SourcePath);
end;

procedure TBackupCatalogueTest.CheckSavingToUnknownFile;
begin
  FBackupCatalogue.Free;
  FBackupCatalogue := TBackupCatalogue.Create;
  CheckException(@LoadFromUnknownFile, Exception);
end;

end.

