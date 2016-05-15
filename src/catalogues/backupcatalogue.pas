unit BackupCatalogue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BackupSet;

type

  { TBackupCatalogue }

  TBackupCatalogue = class
  private
    function GetBackupSetCount: Integer;
  protected
    FName: String;
    FBackupSets: TFPList;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function AddBackupSet: TBackupSet;
    property BackupSetCount: Integer read GetBackupSetCount;
    property Name: String read FName write FName;
  end;

implementation

{ TBackupCatalogue }

function TBackupCatalogue.GetBackupSetCount: Integer;
begin
  Result := FBackupSets.Count;
end;

constructor TBackupCatalogue.Create;
begin
  FBackupSets := TFPList.Create;
end;

destructor TBackupCatalogue.Destroy;
begin
  FBackupSets.Free;
  inherited Destroy;
end;

function TBackupCatalogue.AddBackupSet: TBackupSet;
var
  NewBackupSet: TBackupSet;
begin
  NewBackupSet := TBackupSet.Create;
  FBackupSets.Add(NewBackupSet);
  Result := NewBackupSet;
end;

end.

