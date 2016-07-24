program catalogues_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, TestFramework, filecatalogue_test, TextTestRunner
  { you can add units after this };

begin
  TestFramework.RegisterTest(TFileCatalogueTest.Suite);
  RunRegisteredTests;
  Readln;
end.

