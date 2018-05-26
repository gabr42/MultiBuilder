unit MultiBuilder.Engine;

interface

uses
  MultiBuilder.Engine.Intf;

function CreateMultiBuilderEngine: IMultiBuilderEngine;

implementation

uses
  System.SysUtils, System.StrUtils, System.Classes, System.IniFiles,
  System.Generics.Defaults, System.Generics.Collections,
  System.Threading,
  MultiBuilder.Platform,
  MultiBuilder.Engine.Variables.Intf,
  MultiBuilder.Engine.Variables,
  MultiBuilder.Engine.Filters.Intf;

type
  TMultiBuilderEngine = class(TInterfacedObject, IMultiBuilderEngine)
  strict private
  const
    FS = #28;
    CGlobalSectionName  = 'Global';  // used always
    CDefaultSectionName = 'Default'; // used only if environment-specific section doesn't exist
    CFiltersSectionName = 'Filters';
    CEnvironmentMacro   = 'EnvironmentName';
  strict private
  type
    TFilterMap = TDictionary<string, string>;
    TProjectConfig = record
    private
    const
      CWorkingFolderKeyName = 'WorkingDir';
      CCommandKeyName       = 'Execute';
      CForceDirKeyName      = 'ForceDir';
      procedure CreateFolders(const folderList: string);
      function Filter(const filterMap: TFilterMap; const cmdRes: TExecuteResult):
        TExecuteResult;
      procedure GetFilterNameAndParams(filterMap: TFilterMap; const exeName: string;
        var name, params: string);
    public
      Commands: TArray<string>;
      procedure Append(const values: TStringList);
      function  Execute_Asy(const environment: string; filterMap: TFilterMap;
        const onCommandDoneProc: TProc<string, TExecuteResult>): TExecuteResult;
    end;
  var
    FCountRunners  : integer;
    FEnvironments  : TArray<string>;
    FFilterMap     : TStringList;
    FOnCommandDone : TCommandDoneEvent;
    FOnJobDone     : TJobDoneEvent;
    FOnRunCompleted: TRunCompletedEvent;
    FProject       : TMemIniFile;
    FSections      : TStringList;
    FVariables     : IMultiBuilderVariables;
  strict protected
    procedure Asy_OnCommandDone(environment: string; result: TExecuteResult);
    procedure Asy_SignalJobDone(const environment: string; const future: IFuture<TExecuteResult>);
    function  CreateFilterMap(const environment: string): TFilterMap;
    function  GetNumRunningProjects: integer;
    function  GetOnCommandDone: TCommandDoneEvent;
    function  GetOnJobDone: TJobDoneEvent;
    function  GetOnRunCompleted: TRunCompletedEvent;
    procedure LoadFromIni(memIni: TMemIniFile);
    procedure PrepareProjectConfig(const environment: string; var projectConfig:
      TProjectConfig);
    procedure ReplaceMacros(const environment: string; values: TStringList); overload;
    function  ReplaceMacros(const environment, value: string): string; overload;
    procedure JobDone(const environment: string; const result: TExecuteResult);
    procedure SetOnCommandDone(const value: TCommandDoneEvent);
    procedure SetOnJobDone(const value: TJobDoneEvent);
    procedure SetOnRunCompleted(const value: TRunCompletedEvent);
    procedure StartRunner(const environment: string; const projectConfig: TProjectConfig);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ClearProject;
    function  Environments: TArray<string>;
    function  LoadFrom(const iniFile: string): boolean;
    function  LoadProject(const projFile: string): boolean;
    procedure Run;
    procedure RunSelected(const environment: string);
    property NumRunningProjects: integer read GetNumRunningProjects;
    property OnCommandDone: TCommandDoneEvent read GetOnCommandDone write SetOnCommandDone;
    property OnJobDone: TJobDoneEvent read GetOnJobDone write SetOnJobDone;
    property OnRunCompleted: TRunCompletedEvent read GetOnRunCompleted write SetOnRunCompleted;
  end;

{ exports }

function CreateMultiBuilderEngine: IMultiBuilderEngine;
begin
  Result := TMultiBuilderEngine.Create;
end;

{ globals }

function Split(const setting: string; delimiter: char; var key, value: string): boolean;
var
  p: integer;
begin
  p := Pos(delimiter, setting);
  Result := (p > 0);
  if Result then begin
    key := Copy(setting, 1, p-1);
    value := setting;
    Delete(value, 1, p);
  end;
end;

{ TMultiBuilderEngine }

procedure TMultiBuilderEngine.AfterConstruction;
begin
  inherited;
  FSections := TStringList.Create;
  FVariables := CreateMBVariables(CGlobalSectionName, CDefaultSectionName, CEnvironmentMacro);
  FFilterMap := TStringList.Create;
end;

procedure TMultiBuilderEngine.Asy_OnCommandDone(environment: string; result:
  TExecuteResult);
var
  cmdResult: TExecuteResult;
begin
  cmdResult := result;
  TThread.Queue(nil,
    procedure
    begin
      if assigned(OnCommandDone) then
        OnCommandDone(environment, cmdResult);
    end);
end;

procedure TMultiBuilderEngine.Asy_SignalJobDone(const environment: string; const future:
  IFuture<TExecuteResult>);
begin
  TThread.Queue(nil,
    procedure
    begin
      JobDone(environment, future.Value);
    end);
end;

procedure TMultiBuilderEngine.BeforeDestruction;
begin
  ClearProject;
  FreeAndNil(FFilterMap);
  FreeAndNil(FSections);
  inherited;
end;

procedure TMultiBuilderEngine.ClearProject;
begin
  FreeAndNil(FProject);
  FFilterMap.Clear;
end;

function TMultiBuilderEngine.CreateFilterMap(const environment: string): TFilterMap;
var
  key  : string;
  kv   : string;
  value: string;
begin
  Result := TFilterMap.Create(TIStringComparer.Ordinal);
  for kv in FFilterMap do begin
    if Split(ReplaceMacros(environment, kv), '=', key, value) then
      Result.AddOrSetValue(key, value);
  end;
end;

function TMultiBuilderEngine.Environments: TArray<string>;
begin
  Result := FEnvironments;
end;

function TMultiBuilderEngine.GetNumRunningProjects: integer;
begin
  Result := FCountRunners;
end;

function TMultiBuilderEngine.GetOnCommandDone: TCommandDoneEvent;
begin
  Result := FOnCommandDone;
end;

function TMultiBuilderEngine.GetOnJobDone: TJobDoneEvent;
begin
  Result := FOnJobDone;
end;

function TMultiBuilderEngine.GetOnRunCompleted: TRunCompletedEvent;
begin
  Result := FOnRunCompleted;
end;

procedure TMultiBuilderEngine.JobDone(const environment: string;
  const result: TExecuteResult);
var
  jobResult: TExecuteResult;
begin
  // make sure this is processed after last OnCommandDone event
  jobResult := result;
  TThread.ForceQueue(nil,
    procedure
    begin
      if assigned(OnJobDone) then
        OnJobDone(environment, jobResult);
      Dec(FCountRunners);
      if (FCountRunners = 0) and assigned(OnRunCompleted) then
        OnRunCompleted();
    end);
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
  sEnv    : string;
  sName   : string;
  sNameVar: string;
  sVar    : string;
  values  : TStringList;
begin
  memIni.ReadSections(FSections);
  FVariables.BeginUpdate;
  try
    FVariables.Clear;
    SetLength(FEnvironments, FSections.Count);
    iEnv := 0;
    values := TStringList.Create;
    try
      for sEnv in FSections do begin
        if not (SameText(sEnv, CGlobalSectionName) or SameText(sEnv, CDefaultSectionName)) then begin
          FEnvironments[iEnv] := sEnv;
          Inc(iEnv);
        end;
        memIni.ReadSectionValues(sEnv, values);
        for sNameVar in values do begin
          if Split(sNameVar, '=', sName, sVar) then
            FVariables.Add(sEnv, sName, sVar);
        end;
      end;
    finally FreeAndNil(values); end;
    SetLength(FEnvironments, iEnv);
  finally FVariables.EndUpdate; end;
end;

function TMultiBuilderEngine.LoadProject(const projFile: string): boolean;
begin
  Result := true;
  ClearProject;

  if not FileExists(projFile) then
    Exit(true);

  try
    FProject := TMemIniFile.Create(projFile);
    FProject.ReadSectionValues(CFiltersSectionName, FFilterMap);
  except
    ClearProject;
    Result := false;
  end;
end;

procedure TMultiBuilderEngine.PrepareProjectConfig(const environment: string;
  var projectConfig: TProjectConfig);
var
  section: string;
  values : TStringList;
begin
  values := TStringList.Create;
  try
    projectConfig := Default(TProjectConfig);
    FProject.ReadSectionValues(CGlobalSectionName, values);
    ReplaceMacros(environment, values);
    projectConfig.Append(values);

    if FProject.SectionExists(environment) then
      section := environment
    else
      section := CDefaultSectionName;
    FProject.ReadSectionValues(section, values);
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
  //Used from multiple threads
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
    Insert(FVariables.Evaluate(environment, name), Result, p1);
  until false;
end;

procedure TMultiBuilderEngine.Run;
var
  sEnv: string;
begin
  if (not assigned(FProject)) or (Length(FEnvironments) = 0) then begin
    if assigned(OnRunCompleted) then
      OnRunCompleted();
    Exit;
  end;

  for sEnv in FEnvironments do
    RunSelected(sEnv);
end;

procedure TMultiBuilderEngine.RunSelected(const environment: string);
var
  projConfig: TProjectConfig;
begin
  PrepareProjectConfig(environment, projConfig);
  try
    Inc(FCountRunners);
    StartRunner(environment, projConfig);
  except
    on E: Exception do
      JobDone(environment, TExecuteResult.Create('*** Internal exception', 255, E.Message));
  end;
end;

procedure TMultiBuilderEngine.SetOnCommandDone(const value: TCommandDoneEvent);
begin
  FOnCommandDone := value;
end;

procedure TMultiBuilderEngine.SetOnJobDone(const value: TJobDoneEvent);
begin
  FOnJobDone := value;
end;

procedure TMultiBuilderEngine.SetOnRunCompleted(const value: TRunCompletedEvent);
begin
  FOnRunCompleted := value;
end;

procedure TMultiBuilderEngine.StartRunner(const environment: string; const projectConfig: TProjectConfig);
var
  future      : IFuture<TExecuteResult>;
  threadConfig: TProjectConfig;
begin
  threadConfig := projectConfig;
  future := TFuture<TExecuteResult>.Create(TObject(nil), TFunctionEvent<TExecuteResult>(nil),
    function: TExecuteResult
    begin
      Result := threadConfig.Execute_Asy(environment, CreateFilterMap(environment), Asy_OnCommandDone);
      Asy_SignalJobDone(environment, future);
    end,
    TThreadPool.Default);
  future.Start;
end;

{ TMultiBuilderEngine.TProject }

procedure TMultiBuilderEngine.TProjectConfig.Append(const values: TStringList);
var
  kv   : string;
  name : string;
  value: string;
begin
  for kv in values do begin
    if not Split(kv, '=', name, value) then
      continue; //for

    SetLength(Commands, Length(Commands) + 1);
    Commands[High(Commands)] := name + FS + value;
  end;
end;

procedure TMultiBuilderEngine.TProjectConfig.CreateFolders(const folderList: string);
var
  folder: string;
begin
  for folder in folderList.Split([';'], '"', '"') do
    ForceDirectories(folder);
end;

function TMultiBuilderEngine.TProjectConfig.Execute_Asy(const environment: string;
  filterMap: TFilterMap; const onCommandDoneProc: TProc<string, TExecuteResult>): TExecuteResult;
var
  cmd     : string;
  cmdRes  : TExecuteResult;
  exitCode: integer;
  name    : string;
  output  : string;
  value   : string;
  workDir : string;
begin
  try
    workDir := '';
    for cmd in Commands do begin
      if not Split(cmd, FS, name, value) then
        continue; //for cmd

      if SameText(name, CWorkingFolderKeyName) then
        workDir := value
      else if SameText(name, CForceDirKeyName) then
        CreateFolders(value)
      else if SameText(name, CCommandKeyName) then begin
        if workDir = '' then
          cmdRes := TExecuteResult.Create(cmd, 255, 'Working directory is not set (missing WorkingDir directive)')
        else begin
          MBPlatform.Execute(WorkDir, value, exitCode, output);
          cmdRes := Filter(filterMap, TExecuteResult.Create(value, exitCode, output));
        end;
        onCommandDoneProc(environment, cmdRes);
        if cmdRes.exitCode <> 0 then
          Exit(cmdRes);
      end;
    end;
    Result := TExecuteResult.Create('', 0, '');
  finally FreeAndNil(filterMap); end;
end;

function TMultiBuilderEngine.TProjectConfig.Filter(const filterMap: TFilterMap;
  const cmdRes: TExecuteResult): TExecuteResult;
var
  filter: IMultiBuilderFilter;
  name  : string;
  params: string;
begin
  Result := cmdRes;
  GetFilterNameAndParams(filterMap,
    ExtractFileName((cmdRes.Command + ' !').Split([' '], '"', '"')[0]),
    name, params);
  filter := FilterManager.CreateNewInstance(name, params);
  if assigned(filter) then
    Result := filter.Process(Result);
end;

procedure TMultiBuilderEngine.TProjectConfig.GetFilterNameAndParams(filterMap: TFilterMap;
  const exeName: string; var name, params: string);
var
  parts: TArray<string>;
begin
  if (not filterMap.TryGetValue(exeName, name)) or (Trim(name) = '') then begin
    name := '';
    params := '';
  end
  else begin
    parts := name.Split([' '], '"', '"');
    name := parts[0];
    Delete(parts, 0, 1);
    params := string.Join(' ', parts);
  end;
end;

end.
