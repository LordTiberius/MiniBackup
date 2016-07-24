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
  published
    procedure ChecksAfterSetup;
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

procedure TFileCatalogueTest.ChecksAfterSetup;
begin
  Check(not FRootEntry.hasChilds, 'hasChilds after setup');
  Check(not FRootEntry.isDirectory, 'isDirectory after setup');
  CheckEquals(0, FRootEntry.VersionCount, 'Not empty VersionCount');
  CheckException(@TryGetCurrentVersion, Exception, '');
  CheckEquals(0, FRootEntry.AggregateSize, 'Size is not 0');
  CheckEquals(0, FRootEntry.AggregateFileCount, 'File count is not 0');
end;

end.

