program catalogues_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, TestFramework, filecatalogue_test, TextTestRunner, BackupSet_test,
  BackupCatalogue_Test
  { you can add units after this };

begin
  TestFramework.RegisterTest(TFileCatalogueTest.Suite);
  TestFramework.RegisterTest(TBackupSetTest.Suite);
  TestFramework.RegisterTest(TBackupCatalogueTest.Suite);
  RunRegisteredTests;
  Readln;
end.

