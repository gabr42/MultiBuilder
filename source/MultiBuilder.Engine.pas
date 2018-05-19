unit MultiBuilder.Engine;

interface

uses
  MultiBuilder.Engine.Intf;

function CreateMultiBuilderEngine: IMultiBuilderEngine;

implementation

uses
Windows,
  System.SysUtils, System.StrUtils, System.Classes, System.IniFiles,
  System.Generics.Defaults, System.Generics.Collections;

type
  TMultiBuilderEngine = class(TInterfacedObject, IMultiBuilderEngine)
  strict private
  const
    CGlobalSectionName    = 'Global';
    CWorkingFolderKeyName = 'Folder';
    CCommandKeyName       = 'Cmd';
    CEnvironmentMacro     = 'EnvironmentName';
  type
    TEnvVarTable   = TDictionary<string, string>;
    TProjectConfig = record
      WorkDir : string;
      Commands: TArray<string>;
      procedure Append(const values: TStringList);
    end;
  var
    FEnvironments: TArray<string>;
    FProject     : TMemIniFile;
    FSections    : TStringList;
    FVariables   : TEnvVarTable;
    procedure Run;
  strict protected
    function EvaluateMacro(const environment, name: string): string;
    procedure LoadFromIni(memIni: TMemIniFile);
    procedure PrepareProjectConfig(const environment: string; var projectConfig:
      TProjectConfig);
    procedure ReplaceMacros(const environment: string; values: TStringList); overload;
    function  ReplaceMacros(const environment, value: string): string; overload;
    procedure StartRunner(const projectConfig: TProjectConfig);
  protected
    class function Split(const kv: string; var key, value: string): boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ClearProject;
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
  FVariables := TEnvVarTable.Create(TIStringComparer.Ordinal);
end;

procedure TMultiBuilderEngine.BeforeDestruction;
begin
  ClearProject;
  FreeAndNil(FSections);
  FreeAndNil(FVariables);
  inherited;
end;

procedure TMultiBuilderEngine.ClearProject;
begin
  FreeAndNil(FProject);
end;

function TMultiBuilderEngine.Environments: TArray<string>;
begin
  Result := FEnvironments;
end;

function TMultiBuilderEngine.EvaluateMacro(const environment, name: string): string;
begin
  if SameText(name, CEnvironmentMacro) then
    Result := environment
  else if (not FVariables.TryGetValue(environment + '/' + name, Result))
           and (not FVariables.TryGetValue(CGlobalSectionName + '/' + name, Result))
  then
    Result := '';
end;

function TMultiBuilderEngine.LoadFrom(const iniFile: string): boolean;
var
  memIni: TMemIniFile;
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
    LoadFromIni(memIni);
  finally FreeAndNil(memIni); end;
  Result := true;
end;

procedure TMultiBuilderEngine.LoadFromIni(memIni: TMemIniFile);
var
  iEnv    : integer;
  p       : integer;
  sEnv    : string;
  sName   : string;
  sNameVar: string;
  sVar    : string;
  values  : TStringList;
  kv: TPair<string, string>;
begin
  memIni.ReadSections(FSections);
  SetLength(FEnvironments, FSections.Count);
  FVariables.Clear;
  iEnv := 0;
  values := TStringList.Create;
  try
    for sEnv in FSections do begin
      if not SameText(sEnv, CGlobalSectionName) then begin
        FEnvironments[iEnv] := sEnv;
        Inc(iEnv);
      end;
      memIni.ReadSectionValues(sEnv, values);
      for sNameVar in values do begin
        if Split(sNameVar, sName, sVar) then
          FVariables.Add(sEnv + '/' + sName, sVar);
      end;
    end;
  finally FreeAndNil(values); end;
  SetLength(FEnvironments, iEnv);

  for kv in FVariables do
    OutputDebugString(PChar(Format('%s=%s', [kv.Key, kv.Value])));
end;

function TMultiBuilderEngine.LoadProject(const projFile: string): boolean;
begin
  Result := true;
  ClearProject;

  if not FileExists(projFile) then
    Exit(true);

  try
    FProject := TMemIniFile.Create(projFile);
  except
    ClearProject;
    Result := false;
  end;
end;

procedure TMultiBuilderEngine.PrepareProjectConfig(const environment: string;
  var projectConfig: TProjectConfig);
var
  values: TStringList;
begin
  values := TStringList.Create;
  try
    projectConfig := Default(TProjectConfig);
    FProject.ReadSectionValues(CGlobalSectionName, values);
    ReplaceMacros(environment, values);
    projectConfig.Append(values);
    FProject.ReadSectionValues(environment, values);
    ReplaceMacros(environment, values);
    projectConfig.Append(values);
  finally FreeAndNil(values); end;
end;

procedure TMultiBuilderEngine.ReplaceMacros(const environment: string;
  values: TStringList);
var
  i: integer;
begin
  for i := 0 to values.Count - 1 do
    values[i] := ReplaceMacros(environment, values[i]);
end;

function TMultiBuilderEngine.ReplaceMacros(const environment, value: string): string;
var
  name: string;
  p1  : integer;
  p2  : integer;
begin
  Result := value;
  p1 := 1;
  repeat
    p1 := PosEx('$(', Result, p1);
    if p1 <= 0 then
      break; //repeat
    p2 := PosEx(')', Result, p1);
    if p2 <= 0 then
      break; //repeat
    name := Copy(Result, p1 + 2, p2 - p1 - 2);
    Delete(Result, p1, p2 - p1 + 1);
    Insert(EvaluateMacro(environment, name), Result, p1);
  until false;
end;

procedure TMultiBuilderEngine.Run;
var
  projConfig: TProjectConfig;
  sEnv      : string;
begin
  if not assigned(FProject) then
    Exit;

  for sEnv in FEnvironments do begin
    PrepareProjectConfig(sEnv, projConfig);
    StartRunner(projConfig);
  end;
end;

class function TMultiBuilderEngine.Split(const kv: string; var key, value: string):
  boolean;
var
  p: integer;
begin
  p := Pos('=', kv);
  Result := (p > 0);
  if Result then begin
    key := Copy(kv, 1, p-1);
    value := kv;
    Delete(value, 1, p);
  end;
end;

procedure TMultiBuilderEngine.StartRunner(const projectConfig: TProjectConfig);
var
  threadConfig: TProjectConfig;
begin
  // TODO -cMM: TMultiBuilderEngine.StartRunner default body inserted
  threadConfig := projectConfig;
end;

procedure TMultiBuilderEngine.TProjectConfig.Append(const values: TStringList);
var
  kv   : string;
  name : string;
  value: string;
begin
  for kv in values do begin
    if not TMultiBuilderEngine.Split(kv, name, value) then
      continue; //for

    if SameText(name, CWorkingFolderKeyName) then
      WorkDir := value
    else if SameText(name, CCommandKeyName) then begin
      SetLength(Commands, Length(Commands) + 1);
      Commands[High(Commands)] := value;
    end;
  end;
end;

end.
