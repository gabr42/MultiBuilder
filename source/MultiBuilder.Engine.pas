unit MultiBuilder.Engine;

interface

uses
  MultiBuilder.Engine.Intf;

function CreateMultiBuilderEngine: IMultiBuilderEngine;

implementation

uses
  System.SysUtils, System.Classes, System.IniFiles;

type
  TMultiBuilderEngine = class(TInterfacedObject, IMultiBuilderEngine)
  strict private
  const
    CGlobalSectionName = 'Global';
  var
    FEnvironments: TArray<string>;
    FSections: TStringList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Environments: TArray<string>;
    function LoadFrom(const iniFile: string): boolean;
    function LoadProject(const projFile: string): boolean;
  end;

{ exports }

function CreateMultiBuilderEngine: IMultiBuilderEngine;
begin
  Result := TMultiBuilderEngine.Create;
end;

procedure TMultiBuilderEngine.AfterConstruction;
begin
  inherited;
  FSections := TStringList.Create;
end;

procedure TMultiBuilderEngine.BeforeDestruction;
begin
  FreeAndNil(FSections);
  inherited;
end;

function TMultiBuilderEngine.Environments: TArray<string>;
begin
  Result := FEnvironments;
end;

function TMultiBuilderEngine.LoadFrom(const iniFile: string): boolean;
var
  iEnv  : integer;
  memIni: TMemIniFile;
  sEnv  : string;
begin
  FSections.Clear;
  SetLength(FEnvironments, 0);
  if not FileExists(iniFile) then
    Exit(true);

  try
    memIni := TMemIniFile.Create(iniFile);
  except
    Result := false;
    Exit;
  end;

  try
    memIni.ReadSections(FSections);
    SetLength(FEnvironments, FSections.Count);
    iEnv := 0;
    for sEnv in FSections do
      if not SameText(sEnv, CGlobalSectionName) then begin
        FEnvironments[iEnv] := sEnv;
        Inc(iEnv);
      end;
    SetLength(FEnvironments, iEnv);
  finally FreeAndNil(memIni); end;
  Result := true;
end;

function TMultiBuilderEngine.LoadProject(const projFile: string): boolean;
begin
  Result := true;
end;

end.
